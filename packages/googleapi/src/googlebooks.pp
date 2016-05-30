unit googlebooks;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAnnotation = Class;
  TAnnotationdata = Class;
  TAnnotations = Class;
  TAnnotationsSummary = Class;
  TAnnotationsdata = Class;
  TBooksAnnotationsRange = Class;
  TBooksCloudloadingResource = Class;
  TBooksVolumesRecommendedRateResponse = Class;
  TBookshelf = Class;
  TBookshelves = Class;
  TCategory = Class;
  TConcurrentAccessRestriction = Class;
  TDictlayerdata = Class;
  TDiscoveryclusters = Class;
  TDownloadAccessRestriction = Class;
  TDownloadAccesses = Class;
  TGeolayerdata = Class;
  TLayersummaries = Class;
  TLayersummary = Class;
  TMetadata = Class;
  TNotification = Class;
  TOffers = Class;
  TReadingPosition = Class;
  TRequestAccess = Class;
  TReview = Class;
  TSeries = Class;
  TSeriesmembership = Class;
  TUsersettings = Class;
  TVolume = Class;
  TVolume2 = Class;
  TVolumeannotation = Class;
  TVolumeannotations = Class;
  TVolumes = Class;
  TVolumeseriesinfo = Class;
  TAnnotationArray = Array of TAnnotation;
  TAnnotationdataArray = Array of TAnnotationdata;
  TAnnotationsArray = Array of TAnnotations;
  TAnnotationsSummaryArray = Array of TAnnotationsSummary;
  TAnnotationsdataArray = Array of TAnnotationsdata;
  TBooksAnnotationsRangeArray = Array of TBooksAnnotationsRange;
  TBooksCloudloadingResourceArray = Array of TBooksCloudloadingResource;
  TBooksVolumesRecommendedRateResponseArray = Array of TBooksVolumesRecommendedRateResponse;
  TBookshelfArray = Array of TBookshelf;
  TBookshelvesArray = Array of TBookshelves;
  TCategoryArray = Array of TCategory;
  TConcurrentAccessRestrictionArray = Array of TConcurrentAccessRestriction;
  TDictlayerdataArray = Array of TDictlayerdata;
  TDiscoveryclustersArray = Array of TDiscoveryclusters;
  TDownloadAccessRestrictionArray = Array of TDownloadAccessRestriction;
  TDownloadAccessesArray = Array of TDownloadAccesses;
  TGeolayerdataArray = Array of TGeolayerdata;
  TLayersummariesArray = Array of TLayersummaries;
  TLayersummaryArray = Array of TLayersummary;
  TMetadataArray = Array of TMetadata;
  TNotificationArray = Array of TNotification;
  TOffersArray = Array of TOffers;
  TReadingPositionArray = Array of TReadingPosition;
  TRequestAccessArray = Array of TRequestAccess;
  TReviewArray = Array of TReview;
  TSeriesArray = Array of TSeries;
  TSeriesmembershipArray = Array of TSeriesmembership;
  TUsersettingsArray = Array of TUsersettings;
  TVolumeArray = Array of TVolume;
  TVolume2Array = Array of TVolume2;
  TVolumeannotationArray = Array of TVolumeannotation;
  TVolumeannotationsArray = Array of TVolumeannotations;
  TVolumesArray = Array of TVolumes;
  TVolumeseriesinfoArray = Array of TVolumeseriesinfo;
  //Anonymous types, using auto-generated names
  TAnnotationTypeclientVersionRanges = Class;
  TAnnotationTypecurrentVersionRanges = Class;
  TAnnotationTypelayerSummary = Class;
  TAnnotationsSummaryTypelayersItem = Class;
  TCategoryTypeitemsItem = Class;
  TDictlayerdataTypecommon = Class;
  TDictlayerdataTypedictTypesource = Class;
  TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource = Class;
  TDictlayerdataTypedictTypewordsItemTypederivativesItem = Class;
  TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource = Class;
  TDictlayerdataTypedictTypewordsItemTypeexamplesItem = Class;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem = Class;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource = Class;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem = Class;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem = Class;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource = Class;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource = Class;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem = Class;
  TDictlayerdataTypedictTypewordsItemTypesensesItem = Class;
  TDictlayerdataTypedictTypewordsItemTypesource = Class;
  TDictlayerdataTypedictTypewordsItem = Class;
  TDictlayerdataTypedict = Class;
  TDiscoveryclustersTypeclustersItemTypebanner_with_content_container = Class;
  TDiscoveryclustersTypeclustersItem = Class;
  TGeolayerdataTypecommon = Class;
  TGeolayerdataTypegeoTypeboundaryItemItem = Class;
  TGeolayerdataTypegeoTypeviewportTypehi = Class;
  TGeolayerdataTypegeoTypeviewportTypelo = Class;
  TGeolayerdataTypegeoTypeviewport = Class;
  TGeolayerdataTypegeo = Class;
  TMetadataTypeitemsItem = Class;
  TOffersTypeitemsItemTypeitemsItem = Class;
  TOffersTypeitemsItem = Class;
  TReviewTypeauthor = Class;
  TReviewTypesource = Class;
  TSeriesTypeseriesItem = Class;
  TUsersettingsTypenotesExport = Class;
  TUsersettingsTypenotificationTypemoreFromAuthors = Class;
  TUsersettingsTypenotification = Class;
  TVolumeTypeaccessInfoTypeepub = Class;
  TVolumeTypeaccessInfoTypepdf = Class;
  TVolumeTypeaccessInfo = Class;
  TVolumeTypelayerInfoTypelayersItem = Class;
  TVolumeTypelayerInfo = Class;
  TVolumeTyperecommendedInfo = Class;
  TVolumeTypesaleInfoTypelistPrice = Class;
  TVolumeTypesaleInfoTypeoffersItemTypelistPrice = Class;
  TVolumeTypesaleInfoTypeoffersItemTyperentalDuration = Class;
  TVolumeTypesaleInfoTypeoffersItemTyperetailPrice = Class;
  TVolumeTypesaleInfoTypeoffersItem = Class;
  TVolumeTypesaleInfoTyperetailPrice = Class;
  TVolumeTypesaleInfo = Class;
  TVolumeTypesearchInfo = Class;
  TVolumeTypeuserInfoTypecopy = Class;
  TVolumeTypeuserInfoTypefamilySharing = Class;
  TVolumeTypeuserInfoTyperentalPeriod = Class;
  TVolumeTypeuserInfoTypeuserUploadedVolumeInfo = Class;
  TVolumeTypeuserInfo = Class;
  TVolumeTypevolumeInfoTypedimensions = Class;
  TVolumeTypevolumeInfoTypeimageLinks = Class;
  TVolumeTypevolumeInfoTypeindustryIdentifiersItem = Class;
  TVolumeTypevolumeInfo = Class;
  TVolumeannotationTypecontentRanges = Class;
  TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem = Class;
  TVolumeseriesinfoTypevolumeSeriesItem = Class;
  TAnnotationsTypeitemsArray = Array of TAnnotation;
  TAnnotationsSummaryTypelayersArray = Array of TAnnotationsSummaryTypelayersItem;
  TAnnotationsdataTypeitemsArray = Array of TAnnotationdata;
  TBookshelvesTypeitemsArray = Array of TBookshelf;
  TCategoryTypeitemsArray = Array of TCategoryTypeitemsItem;
  TDictlayerdataTypedictTypewordsItemTypederivativesArray = Array of TDictlayerdataTypedictTypewordsItemTypederivativesItem;
  TDictlayerdataTypedictTypewordsItemTypeexamplesArray = Array of TDictlayerdataTypedictTypewordsItemTypeexamplesItem;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsArray = Array of TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesArray = Array of TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsArray = Array of TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsArray = Array of TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem;
  TDictlayerdataTypedictTypewordsItemTypesensesArray = Array of TDictlayerdataTypedictTypewordsItemTypesensesItem;
  TDictlayerdataTypedictTypewordsArray = Array of TDictlayerdataTypedictTypewordsItem;
  TDiscoveryclustersTypeclustersItemTypevolumesArray = Array of TVolume;
  TDiscoveryclustersTypeclustersArray = Array of TDiscoveryclustersTypeclustersItem;
  TDownloadAccessesTypedownloadAccessListArray = Array of TDownloadAccessRestriction;
  TGeolayerdataTypegeoTypeboundaryItemArray = Array of TGeolayerdataTypegeoTypeboundaryItemItem;
  TGeolayerdataTypegeoTypeboundaryArray = Array of TGeolayerdataTypegeoTypeboundaryItemArray;
  TLayersummariesTypeitemsArray = Array of TLayersummary;
  TMetadataTypeitemsArray = Array of TMetadataTypeitemsItem;
  TOffersTypeitemsItemTypeitemsArray = Array of TOffersTypeitemsItemTypeitemsItem;
  TOffersTypeitemsArray = Array of TOffersTypeitemsItem;
  TSeriesTypeseriesArray = Array of TSeriesTypeseriesItem;
  TSeriesmembershipTypememberArray = Array of TVolume;
  TVolumeTypelayerInfoTypelayersArray = Array of TVolumeTypelayerInfoTypelayersItem;
  TVolumeTypesaleInfoTypeoffersArray = Array of TVolumeTypesaleInfoTypeoffersItem;
  TVolumeTypevolumeInfoTypeindustryIdentifiersArray = Array of TVolumeTypevolumeInfoTypeindustryIdentifiersItem;
  TVolume2TypeitemsArray = Array of TVolume;
  TVolumeannotationsTypeitemsArray = Array of TVolumeannotation;
  TVolumesTypeitemsArray = Array of TVolume;
  TVolumeseriesinfoTypevolumeSeriesItemTypeissueArray = Array of TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem;
  TVolumeseriesinfoTypevolumeSeriesArray = Array of TVolumeseriesinfoTypevolumeSeriesItem;
  
  { --------------------------------------------------------------------
    TAnnotationTypeclientVersionRanges
    --------------------------------------------------------------------}
  
  TAnnotationTypeclientVersionRanges = Class(TGoogleBaseObject)
  Private
    FcfiRange : TBooksAnnotationsRange;
    FcontentVersion : String;
    FgbImageRange : TBooksAnnotationsRange;
    FgbTextRange : TBooksAnnotationsRange;
    FimageCfiRange : TBooksAnnotationsRange;
  Protected
    //Property setters
    Procedure SetcfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
    Procedure SetcontentVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgbImageRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
    Procedure SetgbTextRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
    Procedure SetimageCfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
  Public
  Published
    Property cfiRange : TBooksAnnotationsRange Index 0 Read FcfiRange Write SetcfiRange;
    Property contentVersion : String Index 8 Read FcontentVersion Write SetcontentVersion;
    Property gbImageRange : TBooksAnnotationsRange Index 16 Read FgbImageRange Write SetgbImageRange;
    Property gbTextRange : TBooksAnnotationsRange Index 24 Read FgbTextRange Write SetgbTextRange;
    Property imageCfiRange : TBooksAnnotationsRange Index 32 Read FimageCfiRange Write SetimageCfiRange;
  end;
  TAnnotationTypeclientVersionRangesClass = Class of TAnnotationTypeclientVersionRanges;
  
  { --------------------------------------------------------------------
    TAnnotationTypecurrentVersionRanges
    --------------------------------------------------------------------}
  
  TAnnotationTypecurrentVersionRanges = Class(TGoogleBaseObject)
  Private
    FcfiRange : TBooksAnnotationsRange;
    FcontentVersion : String;
    FgbImageRange : TBooksAnnotationsRange;
    FgbTextRange : TBooksAnnotationsRange;
    FimageCfiRange : TBooksAnnotationsRange;
  Protected
    //Property setters
    Procedure SetcfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
    Procedure SetcontentVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgbImageRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
    Procedure SetgbTextRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
    Procedure SetimageCfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
  Public
  Published
    Property cfiRange : TBooksAnnotationsRange Index 0 Read FcfiRange Write SetcfiRange;
    Property contentVersion : String Index 8 Read FcontentVersion Write SetcontentVersion;
    Property gbImageRange : TBooksAnnotationsRange Index 16 Read FgbImageRange Write SetgbImageRange;
    Property gbTextRange : TBooksAnnotationsRange Index 24 Read FgbTextRange Write SetgbTextRange;
    Property imageCfiRange : TBooksAnnotationsRange Index 32 Read FimageCfiRange Write SetimageCfiRange;
  end;
  TAnnotationTypecurrentVersionRangesClass = Class of TAnnotationTypecurrentVersionRanges;
  
  { --------------------------------------------------------------------
    TAnnotationTypelayerSummary
    --------------------------------------------------------------------}
  
  TAnnotationTypelayerSummary = Class(TGoogleBaseObject)
  Private
    FallowedCharacterCount : integer;
    FlimitType : String;
    FremainingCharacterCount : integer;
  Protected
    //Property setters
    Procedure SetallowedCharacterCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetlimitType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetremainingCharacterCount(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property allowedCharacterCount : integer Index 0 Read FallowedCharacterCount Write SetallowedCharacterCount;
    Property limitType : String Index 8 Read FlimitType Write SetlimitType;
    Property remainingCharacterCount : integer Index 16 Read FremainingCharacterCount Write SetremainingCharacterCount;
  end;
  TAnnotationTypelayerSummaryClass = Class of TAnnotationTypelayerSummary;
  
  { --------------------------------------------------------------------
    TAnnotation
    --------------------------------------------------------------------}
  
  TAnnotation = Class(TGoogleBaseObject)
  Private
    FafterSelectedText : String;
    FbeforeSelectedText : String;
    FclientVersionRanges : TAnnotationTypeclientVersionRanges;
    Fcreated : TDatetime;
    FcurrentVersionRanges : TAnnotationTypecurrentVersionRanges;
    Fdata : String;
    Fdeleted : boolean;
    FhighlightStyle : String;
    Fid : String;
    Fkind : String;
    FlayerId : String;
    FlayerSummary : TAnnotationTypelayerSummary;
    FpageIds : TStringArray;
    FselectedText : String;
    FselfLink : String;
    Fupdated : TDatetime;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure SetafterSelectedText(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbeforeSelectedText(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientVersionRanges(AIndex : Integer; const AValue : TAnnotationTypeclientVersionRanges); virtual;
    Procedure Setcreated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetcurrentVersionRanges(AIndex : Integer; const AValue : TAnnotationTypecurrentVersionRanges); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdeleted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethighlightStyle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlayerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlayerSummary(AIndex : Integer; const AValue : TAnnotationTypelayerSummary); virtual;
    Procedure SetpageIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetselectedText(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property afterSelectedText : String Index 0 Read FafterSelectedText Write SetafterSelectedText;
    Property beforeSelectedText : String Index 8 Read FbeforeSelectedText Write SetbeforeSelectedText;
    Property clientVersionRanges : TAnnotationTypeclientVersionRanges Index 16 Read FclientVersionRanges Write SetclientVersionRanges;
    Property created : TDatetime Index 24 Read Fcreated Write Setcreated;
    Property currentVersionRanges : TAnnotationTypecurrentVersionRanges Index 32 Read FcurrentVersionRanges Write SetcurrentVersionRanges;
    Property data : String Index 40 Read Fdata Write Setdata;
    Property deleted : boolean Index 48 Read Fdeleted Write Setdeleted;
    Property highlightStyle : String Index 56 Read FhighlightStyle Write SethighlightStyle;
    Property id : String Index 64 Read Fid Write Setid;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property layerId : String Index 80 Read FlayerId Write SetlayerId;
    Property layerSummary : TAnnotationTypelayerSummary Index 88 Read FlayerSummary Write SetlayerSummary;
    Property pageIds : TStringArray Index 96 Read FpageIds Write SetpageIds;
    Property selectedText : String Index 104 Read FselectedText Write SetselectedText;
    Property selfLink : String Index 112 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 120 Read Fupdated Write Setupdated;
    Property volumeId : String Index 128 Read FvolumeId Write SetvolumeId;
  end;
  TAnnotationClass = Class of TAnnotation;
  
  { --------------------------------------------------------------------
    TAnnotationdata
    --------------------------------------------------------------------}
  
  TAnnotationdata = Class(TGoogleBaseObject)
  Private
    FannotationType : String;
    Fdata : TJSONSchema;
    Fencoded_data : String;
    Fid : String;
    Fkind : String;
    FlayerId : String;
    FselfLink : String;
    Fupdated : TDatetime;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure SetannotationType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure Setencoded_data(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlayerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property annotationType : String Index 0 Read FannotationType Write SetannotationType;
    Property data : TJSONSchema Index 8 Read Fdata Write Setdata;
    Property encoded_data : String Index 16 Read Fencoded_data Write Setencoded_data;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property layerId : String Index 40 Read FlayerId Write SetlayerId;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
    Property volumeId : String Index 64 Read FvolumeId Write SetvolumeId;
  end;
  TAnnotationdataClass = Class of TAnnotationdata;
  
  { --------------------------------------------------------------------
    TAnnotations
    --------------------------------------------------------------------}
  
  TAnnotations = Class(TGoogleBaseObject)
  Private
    Fitems : TAnnotationsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TAnnotationsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TAnnotationsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TAnnotationsClass = Class of TAnnotations;
  
  { --------------------------------------------------------------------
    TAnnotationsSummaryTypelayersItem
    --------------------------------------------------------------------}
  
  TAnnotationsSummaryTypelayersItem = Class(TGoogleBaseObject)
  Private
    FallowedCharacterCount : integer;
    FlayerId : String;
    FlimitType : String;
    FremainingCharacterCount : integer;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure SetallowedCharacterCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetlayerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlimitType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetremainingCharacterCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
  Public
  Published
    Property allowedCharacterCount : integer Index 0 Read FallowedCharacterCount Write SetallowedCharacterCount;
    Property layerId : String Index 8 Read FlayerId Write SetlayerId;
    Property limitType : String Index 16 Read FlimitType Write SetlimitType;
    Property remainingCharacterCount : integer Index 24 Read FremainingCharacterCount Write SetremainingCharacterCount;
    Property updated : TDatetime Index 32 Read Fupdated Write Setupdated;
  end;
  TAnnotationsSummaryTypelayersItemClass = Class of TAnnotationsSummaryTypelayersItem;
  
  { --------------------------------------------------------------------
    TAnnotationsSummary
    --------------------------------------------------------------------}
  
  TAnnotationsSummary = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Flayers : TAnnotationsSummaryTypelayersArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlayers(AIndex : Integer; const AValue : TAnnotationsSummaryTypelayersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property layers : TAnnotationsSummaryTypelayersArray Index 8 Read Flayers Write Setlayers;
  end;
  TAnnotationsSummaryClass = Class of TAnnotationsSummary;
  
  { --------------------------------------------------------------------
    TAnnotationsdata
    --------------------------------------------------------------------}
  
  TAnnotationsdata = Class(TGoogleBaseObject)
  Private
    Fitems : TAnnotationsdataTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TAnnotationsdataTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TAnnotationsdataTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TAnnotationsdataClass = Class of TAnnotationsdata;
  
  { --------------------------------------------------------------------
    TBooksAnnotationsRange
    --------------------------------------------------------------------}
  
  TBooksAnnotationsRange = Class(TGoogleBaseObject)
  Private
    FendOffset : String;
    FendPosition : String;
    FstartOffset : String;
    FstartPosition : String;
  Protected
    //Property setters
    Procedure SetendOffset(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendPosition(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartOffset(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartPosition(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property endOffset : String Index 0 Read FendOffset Write SetendOffset;
    Property endPosition : String Index 8 Read FendPosition Write SetendPosition;
    Property startOffset : String Index 16 Read FstartOffset Write SetstartOffset;
    Property startPosition : String Index 24 Read FstartPosition Write SetstartPosition;
  end;
  TBooksAnnotationsRangeClass = Class of TBooksAnnotationsRange;
  
  { --------------------------------------------------------------------
    TBooksCloudloadingResource
    --------------------------------------------------------------------}
  
  TBooksCloudloadingResource = Class(TGoogleBaseObject)
  Private
    Fauthor : String;
    FprocessingState : String;
    Ftitle : String;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure Setauthor(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprocessingState(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property author : String Index 0 Read Fauthor Write Setauthor;
    Property processingState : String Index 8 Read FprocessingState Write SetprocessingState;
    Property title : String Index 16 Read Ftitle Write Settitle;
    Property volumeId : String Index 24 Read FvolumeId Write SetvolumeId;
  end;
  TBooksCloudloadingResourceClass = Class of TBooksCloudloadingResource;
  
  { --------------------------------------------------------------------
    TBooksVolumesRecommendedRateResponse
    --------------------------------------------------------------------}
  
  TBooksVolumesRecommendedRateResponse = Class(TGoogleBaseObject)
  Private
    Fconsistency_token : String;
  Protected
    //Property setters
    Procedure Setconsistency_token(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property consistency_token : String Index 0 Read Fconsistency_token Write Setconsistency_token;
  end;
  TBooksVolumesRecommendedRateResponseClass = Class of TBooksVolumesRecommendedRateResponse;
  
  { --------------------------------------------------------------------
    TBookshelf
    --------------------------------------------------------------------}
  
  TBookshelf = Class(TGoogleBaseObject)
  Private
    Faccess : String;
    Fcreated : TDatetime;
    Fdescription : String;
    Fid : integer;
    Fkind : String;
    FselfLink : String;
    Ftitle : String;
    Fupdated : TDatetime;
    FvolumeCount : integer;
    FvolumesLastUpdated : TDatetime;
  Protected
    //Property setters
    Procedure Setaccess(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcreated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetvolumeCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetvolumesLastUpdated(AIndex : Integer; const AValue : TDatetime); virtual;
  Public
  Published
    Property access : String Index 0 Read Faccess Write Setaccess;
    Property created : TDatetime Index 8 Read Fcreated Write Setcreated;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property id : integer Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property selfLink : String Index 40 Read FselfLink Write SetselfLink;
    Property title : String Index 48 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
    Property volumeCount : integer Index 64 Read FvolumeCount Write SetvolumeCount;
    Property volumesLastUpdated : TDatetime Index 72 Read FvolumesLastUpdated Write SetvolumesLastUpdated;
  end;
  TBookshelfClass = Class of TBookshelf;
  
  { --------------------------------------------------------------------
    TBookshelves
    --------------------------------------------------------------------}
  
  TBookshelves = Class(TGoogleBaseObject)
  Private
    Fitems : TBookshelvesTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TBookshelvesTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TBookshelvesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TBookshelvesClass = Class of TBookshelves;
  
  { --------------------------------------------------------------------
    TCategoryTypeitemsItem
    --------------------------------------------------------------------}
  
  TCategoryTypeitemsItem = Class(TGoogleBaseObject)
  Private
    FbadgeUrl : String;
    FcategoryId : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetbadgeUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcategoryId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property badgeUrl : String Index 0 Read FbadgeUrl Write SetbadgeUrl;
    Property categoryId : String Index 8 Read FcategoryId Write SetcategoryId;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TCategoryTypeitemsItemClass = Class of TCategoryTypeitemsItem;
  
  { --------------------------------------------------------------------
    TCategory
    --------------------------------------------------------------------}
  
  TCategory = Class(TGoogleBaseObject)
  Private
    Fitems : TCategoryTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TCategoryTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TCategoryTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCategoryClass = Class of TCategory;
  
  { --------------------------------------------------------------------
    TConcurrentAccessRestriction
    --------------------------------------------------------------------}
  
  TConcurrentAccessRestriction = Class(TGoogleBaseObject)
  Private
    FdeviceAllowed : boolean;
    Fkind : String;
    FmaxConcurrentDevices : integer;
    Fmessage : String;
    Fnonce : String;
    FreasonCode : String;
    Frestricted : boolean;
    Fsignature : String;
    Fsource : String;
    FtimeWindowSeconds : integer;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure SetdeviceAllowed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxConcurrentDevices(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnonce(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreasonCode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrestricted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setsignature(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : String); virtual;
    Procedure SettimeWindowSeconds(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property deviceAllowed : boolean Index 0 Read FdeviceAllowed Write SetdeviceAllowed;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property maxConcurrentDevices : integer Index 16 Read FmaxConcurrentDevices Write SetmaxConcurrentDevices;
    Property message : String Index 24 Read Fmessage Write Setmessage;
    Property nonce : String Index 32 Read Fnonce Write Setnonce;
    Property reasonCode : String Index 40 Read FreasonCode Write SetreasonCode;
    Property restricted : boolean Index 48 Read Frestricted Write Setrestricted;
    Property signature : String Index 56 Read Fsignature Write Setsignature;
    Property source : String Index 64 Read Fsource Write Setsource;
    Property timeWindowSeconds : integer Index 72 Read FtimeWindowSeconds Write SettimeWindowSeconds;
    Property volumeId : String Index 80 Read FvolumeId Write SetvolumeId;
  end;
  TConcurrentAccessRestrictionClass = Class of TConcurrentAccessRestriction;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypecommon
    --------------------------------------------------------------------}
  
  TDictlayerdataTypecommon = Class(TGoogleBaseObject)
  Private
    Ftitle : String;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property title : String Index 0 Read Ftitle Write Settitle;
  end;
  TDictlayerdataTypecommonClass = Class of TDictlayerdataTypecommon;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypesource
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypesource = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdataTypedictTypesourceClass = Class of TDictlayerdataTypedictTypesource;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesourceClass = Class of TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypederivativesItem
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypederivativesItem = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource;
    Ftext : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource); virtual;
    Procedure Settext(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource Index 0 Read Fsource Write Setsource;
    Property text : String Index 8 Read Ftext Write Settext;
  end;
  TDictlayerdataTypedictTypewordsItemTypederivativesItemClass = Class of TDictlayerdataTypedictTypewordsItemTypederivativesItem;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesourceClass = Class of TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypeexamplesItem
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypeexamplesItem = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource;
    Ftext : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource); virtual;
    Procedure Settext(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource Index 0 Read Fsource Write Setsource;
    Property text : String Index 8 Read Ftext Write Settext;
  end;
  TDictlayerdataTypedictTypewordsItemTypeexamplesItemClass = Class of TDictlayerdataTypedictTypewordsItemTypeexamplesItem;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItemClass = Class of TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesourceClass = Class of TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource;
    Ftext : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource); virtual;
    Procedure Settext(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource Index 0 Read Fsource Write Setsource;
    Property text : String Index 8 Read Ftext Write Settext;
  end;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemClass = Class of TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem = Class(TGoogleBaseObject)
  Private
    Fdefinition : String;
    Fexamples : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesArray;
  Protected
    //Property setters
    Procedure Setdefinition(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexamples(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property definition : String Index 0 Read Fdefinition Write Setdefinition;
    Property examples : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesArray Index 8 Read Fexamples Write Setexamples;
  end;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemClass = Class of TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesourceClass = Class of TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesourceClass = Class of TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource;
    Ftext : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource); virtual;
    Procedure Settext(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource Index 0 Read Fsource Write Setsource;
    Property text : String Index 8 Read Ftext Write Settext;
  end;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemClass = Class of TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesensesItem
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesensesItem = Class(TGoogleBaseObject)
  Private
    Fconjugations : TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsArray;
    Fdefinitions : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsArray;
    FpartOfSpeech : String;
    Fpronunciation : String;
    FpronunciationUrl : String;
    Fsource : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource;
    Fsyllabification : String;
    Fsynonyms : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsArray;
  Protected
    //Property setters
    Procedure Setconjugations(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsArray); virtual;
    Procedure Setdefinitions(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsArray); virtual;
    Procedure SetpartOfSpeech(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpronunciation(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpronunciationUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource); virtual;
    Procedure Setsyllabification(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsynonyms(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property conjugations : TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsArray Index 0 Read Fconjugations Write Setconjugations;
    Property definitions : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsArray Index 8 Read Fdefinitions Write Setdefinitions;
    Property partOfSpeech : String Index 16 Read FpartOfSpeech Write SetpartOfSpeech;
    Property pronunciation : String Index 24 Read Fpronunciation Write Setpronunciation;
    Property pronunciationUrl : String Index 32 Read FpronunciationUrl Write SetpronunciationUrl;
    Property source : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource Index 40 Read Fsource Write Setsource;
    Property syllabification : String Index 48 Read Fsyllabification Write Setsyllabification;
    Property synonyms : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsArray Index 56 Read Fsynonyms Write Setsynonyms;
  end;
  TDictlayerdataTypedictTypewordsItemTypesensesItemClass = Class of TDictlayerdataTypedictTypewordsItemTypesensesItem;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItemTypesource
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItemTypesource = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdataTypedictTypewordsItemTypesourceClass = Class of TDictlayerdataTypedictTypewordsItemTypesource;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedictTypewordsItem
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedictTypewordsItem = Class(TGoogleBaseObject)
  Private
    Fderivatives : TDictlayerdataTypedictTypewordsItemTypederivativesArray;
    Fexamples : TDictlayerdataTypedictTypewordsItemTypeexamplesArray;
    Fsenses : TDictlayerdataTypedictTypewordsItemTypesensesArray;
    Fsource : TDictlayerdataTypedictTypewordsItemTypesource;
  Protected
    //Property setters
    Procedure Setderivatives(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypederivativesArray); virtual;
    Procedure Setexamples(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypeexamplesArray); virtual;
    Procedure Setsenses(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesArray); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesource); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property derivatives : TDictlayerdataTypedictTypewordsItemTypederivativesArray Index 0 Read Fderivatives Write Setderivatives;
    Property examples : TDictlayerdataTypedictTypewordsItemTypeexamplesArray Index 8 Read Fexamples Write Setexamples;
    Property senses : TDictlayerdataTypedictTypewordsItemTypesensesArray Index 16 Read Fsenses Write Setsenses;
    Property source : TDictlayerdataTypedictTypewordsItemTypesource Index 24 Read Fsource Write Setsource;
  end;
  TDictlayerdataTypedictTypewordsItemClass = Class of TDictlayerdataTypedictTypewordsItem;
  
  { --------------------------------------------------------------------
    TDictlayerdataTypedict
    --------------------------------------------------------------------}
  
  TDictlayerdataTypedict = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdataTypedictTypesource;
    Fwords : TDictlayerdataTypedictTypewordsArray;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypesource); virtual;
    Procedure Setwords(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property source : TDictlayerdataTypedictTypesource Index 0 Read Fsource Write Setsource;
    Property words : TDictlayerdataTypedictTypewordsArray Index 8 Read Fwords Write Setwords;
  end;
  TDictlayerdataTypedictClass = Class of TDictlayerdataTypedict;
  
  { --------------------------------------------------------------------
    TDictlayerdata
    --------------------------------------------------------------------}
  
  TDictlayerdata = Class(TGoogleBaseObject)
  Private
    Fcommon : TDictlayerdataTypecommon;
    Fdict : TDictlayerdataTypedict;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setcommon(AIndex : Integer; const AValue : TDictlayerdataTypecommon); virtual;
    Procedure Setdict(AIndex : Integer; const AValue : TDictlayerdataTypedict); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property common : TDictlayerdataTypecommon Index 0 Read Fcommon Write Setcommon;
    Property dict : TDictlayerdataTypedict Index 8 Read Fdict Write Setdict;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TDictlayerdataClass = Class of TDictlayerdata;
  
  { --------------------------------------------------------------------
    TDiscoveryclustersTypeclustersItemTypebanner_with_content_container
    --------------------------------------------------------------------}
  
  TDiscoveryclustersTypeclustersItemTypebanner_with_content_container = Class(TGoogleBaseObject)
  Private
    FfillColorArgb : String;
    FimageUrl : String;
    FmaskColorArgb : String;
    FmoreButtonText : String;
    FmoreButtonUrl : String;
    FtextColorArgb : String;
  Protected
    //Property setters
    Procedure SetfillColorArgb(AIndex : Integer; const AValue : String); virtual;
    Procedure SetimageUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaskColorArgb(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmoreButtonText(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmoreButtonUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SettextColorArgb(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property fillColorArgb : String Index 0 Read FfillColorArgb Write SetfillColorArgb;
    Property imageUrl : String Index 8 Read FimageUrl Write SetimageUrl;
    Property maskColorArgb : String Index 16 Read FmaskColorArgb Write SetmaskColorArgb;
    Property moreButtonText : String Index 24 Read FmoreButtonText Write SetmoreButtonText;
    Property moreButtonUrl : String Index 32 Read FmoreButtonUrl Write SetmoreButtonUrl;
    Property textColorArgb : String Index 40 Read FtextColorArgb Write SettextColorArgb;
  end;
  TDiscoveryclustersTypeclustersItemTypebanner_with_content_containerClass = Class of TDiscoveryclustersTypeclustersItemTypebanner_with_content_container;
  
  { --------------------------------------------------------------------
    TDiscoveryclustersTypeclustersItem
    --------------------------------------------------------------------}
  
  TDiscoveryclustersTypeclustersItem = Class(TGoogleBaseObject)
  Private
    Fbanner_with_content_container : TDiscoveryclustersTypeclustersItemTypebanner_with_content_container;
    FsubTitle : String;
    Ftitle : String;
    FtotalVolumes : integer;
    Fuid : String;
    Fvolumes : TDiscoveryclustersTypeclustersItemTypevolumesArray;
  Protected
    //Property setters
    Procedure Setbanner_with_content_container(AIndex : Integer; const AValue : TDiscoveryclustersTypeclustersItemTypebanner_with_content_container); virtual;
    Procedure SetsubTitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalVolumes(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setuid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvolumes(AIndex : Integer; const AValue : TDiscoveryclustersTypeclustersItemTypevolumesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property banner_with_content_container : TDiscoveryclustersTypeclustersItemTypebanner_with_content_container Index 0 Read Fbanner_with_content_container Write Setbanner_with_content_container;
    Property subTitle : String Index 8 Read FsubTitle Write SetsubTitle;
    Property title : String Index 16 Read Ftitle Write Settitle;
    Property totalVolumes : integer Index 24 Read FtotalVolumes Write SettotalVolumes;
    Property uid : String Index 32 Read Fuid Write Setuid;
    Property volumes : TDiscoveryclustersTypeclustersItemTypevolumesArray Index 40 Read Fvolumes Write Setvolumes;
  end;
  TDiscoveryclustersTypeclustersItemClass = Class of TDiscoveryclustersTypeclustersItem;
  
  { --------------------------------------------------------------------
    TDiscoveryclusters
    --------------------------------------------------------------------}
  
  TDiscoveryclusters = Class(TGoogleBaseObject)
  Private
    Fclusters : TDiscoveryclustersTypeclustersArray;
    Fkind : String;
    FtotalClusters : integer;
  Protected
    //Property setters
    Procedure Setclusters(AIndex : Integer; const AValue : TDiscoveryclustersTypeclustersArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalClusters(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clusters : TDiscoveryclustersTypeclustersArray Index 0 Read Fclusters Write Setclusters;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property totalClusters : integer Index 16 Read FtotalClusters Write SettotalClusters;
  end;
  TDiscoveryclustersClass = Class of TDiscoveryclusters;
  
  { --------------------------------------------------------------------
    TDownloadAccessRestriction
    --------------------------------------------------------------------}
  
  TDownloadAccessRestriction = Class(TGoogleBaseObject)
  Private
    FdeviceAllowed : boolean;
    FdownloadsAcquired : integer;
    FjustAcquired : boolean;
    Fkind : String;
    FmaxDownloadDevices : integer;
    Fmessage : String;
    Fnonce : String;
    FreasonCode : String;
    Frestricted : boolean;
    Fsignature : String;
    Fsource : String;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure SetdeviceAllowed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdownloadsAcquired(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetjustAcquired(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxDownloadDevices(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnonce(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreasonCode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrestricted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setsignature(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property deviceAllowed : boolean Index 0 Read FdeviceAllowed Write SetdeviceAllowed;
    Property downloadsAcquired : integer Index 8 Read FdownloadsAcquired Write SetdownloadsAcquired;
    Property justAcquired : boolean Index 16 Read FjustAcquired Write SetjustAcquired;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property maxDownloadDevices : integer Index 32 Read FmaxDownloadDevices Write SetmaxDownloadDevices;
    Property message : String Index 40 Read Fmessage Write Setmessage;
    Property nonce : String Index 48 Read Fnonce Write Setnonce;
    Property reasonCode : String Index 56 Read FreasonCode Write SetreasonCode;
    Property restricted : boolean Index 64 Read Frestricted Write Setrestricted;
    Property signature : String Index 72 Read Fsignature Write Setsignature;
    Property source : String Index 80 Read Fsource Write Setsource;
    Property volumeId : String Index 88 Read FvolumeId Write SetvolumeId;
  end;
  TDownloadAccessRestrictionClass = Class of TDownloadAccessRestriction;
  
  { --------------------------------------------------------------------
    TDownloadAccesses
    --------------------------------------------------------------------}
  
  TDownloadAccesses = Class(TGoogleBaseObject)
  Private
    FdownloadAccessList : TDownloadAccessesTypedownloadAccessListArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetdownloadAccessList(AIndex : Integer; const AValue : TDownloadAccessesTypedownloadAccessListArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property downloadAccessList : TDownloadAccessesTypedownloadAccessListArray Index 0 Read FdownloadAccessList Write SetdownloadAccessList;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TDownloadAccessesClass = Class of TDownloadAccesses;
  
  { --------------------------------------------------------------------
    TGeolayerdataTypecommon
    --------------------------------------------------------------------}
  
  TGeolayerdataTypecommon = Class(TGoogleBaseObject)
  Private
    Flang : String;
    FpreviewImageUrl : String;
    Fsnippet : String;
    FsnippetUrl : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setlang(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpreviewImageUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsnippetUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property lang : String Index 0 Read Flang Write Setlang;
    Property previewImageUrl : String Index 8 Read FpreviewImageUrl Write SetpreviewImageUrl;
    Property snippet : String Index 16 Read Fsnippet Write Setsnippet;
    Property snippetUrl : String Index 24 Read FsnippetUrl Write SetsnippetUrl;
    Property title : String Index 32 Read Ftitle Write Settitle;
  end;
  TGeolayerdataTypecommonClass = Class of TGeolayerdataTypecommon;
  
  { --------------------------------------------------------------------
    TGeolayerdataTypegeoTypeboundaryItemItem
    --------------------------------------------------------------------}
  
  TGeolayerdataTypegeoTypeboundaryItemItem = Class(TGoogleBaseObject)
  Private
    Flatitude : integer;
    Flongitude : integer;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property latitude : integer Index 0 Read Flatitude Write Setlatitude;
    Property longitude : integer Index 8 Read Flongitude Write Setlongitude;
  end;
  TGeolayerdataTypegeoTypeboundaryItemItemClass = Class of TGeolayerdataTypegeoTypeboundaryItemItem;
  
  { --------------------------------------------------------------------
    TGeolayerdataTypegeoTypeviewportTypehi
    --------------------------------------------------------------------}
  
  TGeolayerdataTypegeoTypeviewportTypehi = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TGeolayerdataTypegeoTypeviewportTypehiClass = Class of TGeolayerdataTypegeoTypeviewportTypehi;
  
  { --------------------------------------------------------------------
    TGeolayerdataTypegeoTypeviewportTypelo
    --------------------------------------------------------------------}
  
  TGeolayerdataTypegeoTypeviewportTypelo = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TGeolayerdataTypegeoTypeviewportTypeloClass = Class of TGeolayerdataTypegeoTypeviewportTypelo;
  
  { --------------------------------------------------------------------
    TGeolayerdataTypegeoTypeviewport
    --------------------------------------------------------------------}
  
  TGeolayerdataTypegeoTypeviewport = Class(TGoogleBaseObject)
  Private
    Fhi : TGeolayerdataTypegeoTypeviewportTypehi;
    Flo : TGeolayerdataTypegeoTypeviewportTypelo;
  Protected
    //Property setters
    Procedure Sethi(AIndex : Integer; const AValue : TGeolayerdataTypegeoTypeviewportTypehi); virtual;
    Procedure Setlo(AIndex : Integer; const AValue : TGeolayerdataTypegeoTypeviewportTypelo); virtual;
  Public
  Published
    Property hi : TGeolayerdataTypegeoTypeviewportTypehi Index 0 Read Fhi Write Sethi;
    Property lo : TGeolayerdataTypegeoTypeviewportTypelo Index 8 Read Flo Write Setlo;
  end;
  TGeolayerdataTypegeoTypeviewportClass = Class of TGeolayerdataTypegeoTypeviewport;
  
  { --------------------------------------------------------------------
    TGeolayerdataTypegeo
    --------------------------------------------------------------------}
  
  TGeolayerdataTypegeo = Class(TGoogleBaseObject)
  Private
    Fboundary : TGeolayerdataTypegeoTypeboundaryArray;
    FcachePolicy : String;
    FcountryCode : String;
    Flatitude : double;
    Flongitude : double;
    FmapType : String;
    Fviewport : TGeolayerdataTypegeoTypeviewport;
    Fzoom : integer;
  Protected
    //Property setters
    Procedure Setboundary(AIndex : Integer; const AValue : TGeolayerdataTypegeoTypeboundaryArray); virtual;
    Procedure SetcachePolicy(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcountryCode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
    Procedure SetmapType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setviewport(AIndex : Integer; const AValue : TGeolayerdataTypegeoTypeviewport); virtual;
    Procedure Setzoom(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property boundary : TGeolayerdataTypegeoTypeboundaryArray Index 0 Read Fboundary Write Setboundary;
    Property cachePolicy : String Index 8 Read FcachePolicy Write SetcachePolicy;
    Property countryCode : String Index 16 Read FcountryCode Write SetcountryCode;
    Property latitude : double Index 24 Read Flatitude Write Setlatitude;
    Property longitude : double Index 32 Read Flongitude Write Setlongitude;
    Property mapType : String Index 40 Read FmapType Write SetmapType;
    Property viewport : TGeolayerdataTypegeoTypeviewport Index 48 Read Fviewport Write Setviewport;
    Property zoom : integer Index 56 Read Fzoom Write Setzoom;
  end;
  TGeolayerdataTypegeoClass = Class of TGeolayerdataTypegeo;
  
  { --------------------------------------------------------------------
    TGeolayerdata
    --------------------------------------------------------------------}
  
  TGeolayerdata = Class(TGoogleBaseObject)
  Private
    Fcommon : TGeolayerdataTypecommon;
    Fgeo : TGeolayerdataTypegeo;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setcommon(AIndex : Integer; const AValue : TGeolayerdataTypecommon); virtual;
    Procedure Setgeo(AIndex : Integer; const AValue : TGeolayerdataTypegeo); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property common : TGeolayerdataTypecommon Index 0 Read Fcommon Write Setcommon;
    Property geo : TGeolayerdataTypegeo Index 8 Read Fgeo Write Setgeo;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TGeolayerdataClass = Class of TGeolayerdata;
  
  { --------------------------------------------------------------------
    TLayersummaries
    --------------------------------------------------------------------}
  
  TLayersummaries = Class(TGoogleBaseObject)
  Private
    Fitems : TLayersummariesTypeitemsArray;
    Fkind : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TLayersummariesTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TLayersummariesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property totalItems : integer Index 16 Read FtotalItems Write SettotalItems;
  end;
  TLayersummariesClass = Class of TLayersummaries;
  
  { --------------------------------------------------------------------
    TLayersummary
    --------------------------------------------------------------------}
  
  TLayersummary = Class(TGoogleBaseObject)
  Private
    FannotationCount : integer;
    FannotationTypes : TStringArray;
    FannotationsDataLink : String;
    FannotationsLink : String;
    FcontentVersion : String;
    FdataCount : integer;
    Fid : String;
    Fkind : String;
    FlayerId : String;
    FselfLink : String;
    Fupdated : TDatetime;
    FvolumeAnnotationsVersion : String;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure SetannotationCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetannotationTypes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetannotationsDataLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetannotationsLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontentVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlayerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetvolumeAnnotationsVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property annotationCount : integer Index 0 Read FannotationCount Write SetannotationCount;
    Property annotationTypes : TStringArray Index 8 Read FannotationTypes Write SetannotationTypes;
    Property annotationsDataLink : String Index 16 Read FannotationsDataLink Write SetannotationsDataLink;
    Property annotationsLink : String Index 24 Read FannotationsLink Write SetannotationsLink;
    Property contentVersion : String Index 32 Read FcontentVersion Write SetcontentVersion;
    Property dataCount : integer Index 40 Read FdataCount Write SetdataCount;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property layerId : String Index 64 Read FlayerId Write SetlayerId;
    Property selfLink : String Index 72 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
    Property volumeAnnotationsVersion : String Index 88 Read FvolumeAnnotationsVersion Write SetvolumeAnnotationsVersion;
    Property volumeId : String Index 96 Read FvolumeId Write SetvolumeId;
  end;
  TLayersummaryClass = Class of TLayersummary;
  
  { --------------------------------------------------------------------
    TMetadataTypeitemsItem
    --------------------------------------------------------------------}
  
  TMetadataTypeitemsItem = Class(TGoogleBaseObject)
  Private
    Fdownload_url : String;
    Fencrypted_key : String;
    Flanguage : String;
    Fsize : String;
    Fversion : String;
  Protected
    //Property setters
    Procedure Setdownload_url(AIndex : Integer; const AValue : String); virtual;
    Procedure Setencrypted_key(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; const AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property download_url : String Index 0 Read Fdownload_url Write Setdownload_url;
    Property encrypted_key : String Index 8 Read Fencrypted_key Write Setencrypted_key;
    Property language : String Index 16 Read Flanguage Write Setlanguage;
    Property size : String Index 24 Read Fsize Write Setsize;
    Property version : String Index 32 Read Fversion Write Setversion;
  end;
  TMetadataTypeitemsItemClass = Class of TMetadataTypeitemsItem;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    Fitems : TMetadataTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TMetadataTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TMetadataTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TNotification
    --------------------------------------------------------------------}
  
  TNotification = Class(TGoogleBaseObject)
  Private
    Fbody : String;
    FcrmExperimentIds : TStringArray;
    Fdoc_id : String;
    Fdoc_type : String;
    Fdont_show_notification : boolean;
    FiconUrl : String;
    Fkind : String;
    Fnotification_type : String;
    Fpcampaign_id : String;
    Freason : String;
    Fshow_notification_settings_action : boolean;
    FtargetUrl : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setbody(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcrmExperimentIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setdoc_id(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdoc_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdont_show_notification(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SeticonUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnotification_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpcampaign_id(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreason(AIndex : Integer; const AValue : String); virtual;
    Procedure Setshow_notification_settings_action(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettargetUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property body : String Index 0 Read Fbody Write Setbody;
    Property crmExperimentIds : TStringArray Index 8 Read FcrmExperimentIds Write SetcrmExperimentIds;
    Property doc_id : String Index 16 Read Fdoc_id Write Setdoc_id;
    Property doc_type : String Index 24 Read Fdoc_type Write Setdoc_type;
    Property dont_show_notification : boolean Index 32 Read Fdont_show_notification Write Setdont_show_notification;
    Property iconUrl : String Index 40 Read FiconUrl Write SeticonUrl;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property notification_type : String Index 56 Read Fnotification_type Write Setnotification_type;
    Property pcampaign_id : String Index 64 Read Fpcampaign_id Write Setpcampaign_id;
    Property reason : String Index 72 Read Freason Write Setreason;
    Property show_notification_settings_action : boolean Index 80 Read Fshow_notification_settings_action Write Setshow_notification_settings_action;
    Property targetUrl : String Index 88 Read FtargetUrl Write SettargetUrl;
    Property title : String Index 96 Read Ftitle Write Settitle;
  end;
  TNotificationClass = Class of TNotification;
  
  { --------------------------------------------------------------------
    TOffersTypeitemsItemTypeitemsItem
    --------------------------------------------------------------------}
  
  TOffersTypeitemsItemTypeitemsItem = Class(TGoogleBaseObject)
  Private
    Fauthor : String;
    FcanonicalVolumeLink : String;
    FcoverUrl : String;
    Fdescription : String;
    Ftitle : String;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure Setauthor(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcanonicalVolumeLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcoverUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property author : String Index 0 Read Fauthor Write Setauthor;
    Property canonicalVolumeLink : String Index 8 Read FcanonicalVolumeLink Write SetcanonicalVolumeLink;
    Property coverUrl : String Index 16 Read FcoverUrl Write SetcoverUrl;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property title : String Index 32 Read Ftitle Write Settitle;
    Property volumeId : String Index 40 Read FvolumeId Write SetvolumeId;
  end;
  TOffersTypeitemsItemTypeitemsItemClass = Class of TOffersTypeitemsItemTypeitemsItem;
  
  { --------------------------------------------------------------------
    TOffersTypeitemsItem
    --------------------------------------------------------------------}
  
  TOffersTypeitemsItem = Class(TGoogleBaseObject)
  Private
    FartUrl : String;
    FgservicesKey : String;
    Fid : String;
    Fitems : TOffersTypeitemsItemTypeitemsArray;
  Protected
    //Property setters
    Procedure SetartUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgservicesKey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; const AValue : TOffersTypeitemsItemTypeitemsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property artUrl : String Index 0 Read FartUrl Write SetartUrl;
    Property gservicesKey : String Index 8 Read FgservicesKey Write SetgservicesKey;
    Property id : String Index 16 Read Fid Write Setid;
    Property items : TOffersTypeitemsItemTypeitemsArray Index 24 Read Fitems Write Setitems;
  end;
  TOffersTypeitemsItemClass = Class of TOffersTypeitemsItem;
  
  { --------------------------------------------------------------------
    TOffers
    --------------------------------------------------------------------}
  
  TOffers = Class(TGoogleBaseObject)
  Private
    Fitems : TOffersTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TOffersTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TOffersTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TOffersClass = Class of TOffers;
  
  { --------------------------------------------------------------------
    TReadingPosition
    --------------------------------------------------------------------}
  
  TReadingPosition = Class(TGoogleBaseObject)
  Private
    FepubCfiPosition : String;
    FgbImagePosition : String;
    FgbTextPosition : String;
    Fkind : String;
    FpdfPosition : String;
    Fupdated : TDatetime;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure SetepubCfiPosition(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgbImagePosition(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgbTextPosition(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpdfPosition(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property epubCfiPosition : String Index 0 Read FepubCfiPosition Write SetepubCfiPosition;
    Property gbImagePosition : String Index 8 Read FgbImagePosition Write SetgbImagePosition;
    Property gbTextPosition : String Index 16 Read FgbTextPosition Write SetgbTextPosition;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property pdfPosition : String Index 32 Read FpdfPosition Write SetpdfPosition;
    Property updated : TDatetime Index 40 Read Fupdated Write Setupdated;
    Property volumeId : String Index 48 Read FvolumeId Write SetvolumeId;
  end;
  TReadingPositionClass = Class of TReadingPosition;
  
  { --------------------------------------------------------------------
    TRequestAccess
    --------------------------------------------------------------------}
  
  TRequestAccess = Class(TGoogleBaseObject)
  Private
    FconcurrentAccess : TConcurrentAccessRestriction;
    FdownloadAccess : TDownloadAccessRestriction;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetconcurrentAccess(AIndex : Integer; const AValue : TConcurrentAccessRestriction); virtual;
    Procedure SetdownloadAccess(AIndex : Integer; const AValue : TDownloadAccessRestriction); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property concurrentAccess : TConcurrentAccessRestriction Index 0 Read FconcurrentAccess Write SetconcurrentAccess;
    Property downloadAccess : TDownloadAccessRestriction Index 8 Read FdownloadAccess Write SetdownloadAccess;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TRequestAccessClass = Class of TRequestAccess;
  
  { --------------------------------------------------------------------
    TReviewTypeauthor
    --------------------------------------------------------------------}
  
  TReviewTypeauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
  end;
  TReviewTypeauthorClass = Class of TReviewTypeauthor;
  
  { --------------------------------------------------------------------
    TReviewTypesource
    --------------------------------------------------------------------}
  
  TReviewTypesource = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    FextraDescription : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetextraDescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property extraDescription : String Index 8 Read FextraDescription Write SetextraDescription;
    Property url : String Index 16 Read Furl Write Seturl;
  end;
  TReviewTypesourceClass = Class of TReviewTypesource;
  
  { --------------------------------------------------------------------
    TReview
    --------------------------------------------------------------------}
  
  TReview = Class(TGoogleBaseObject)
  Private
    Fauthor : TReviewTypeauthor;
    Fcontent : String;
    Fdate : String;
    FfullTextUrl : String;
    Fkind : String;
    Frating : String;
    Fsource : TReviewTypesource;
    Ftitle : String;
    F_type : String;
    FvolumeId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; const AValue : TReviewTypeauthor); virtual;
    Procedure Setcontent(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfullTextUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrating(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : TReviewTypesource); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property author : TReviewTypeauthor Index 0 Read Fauthor Write Setauthor;
    Property content : String Index 8 Read Fcontent Write Setcontent;
    Property date : String Index 16 Read Fdate Write Setdate;
    Property fullTextUrl : String Index 24 Read FfullTextUrl Write SetfullTextUrl;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property rating : String Index 40 Read Frating Write Setrating;
    Property source : TReviewTypesource Index 48 Read Fsource Write Setsource;
    Property title : String Index 56 Read Ftitle Write Settitle;
    Property _type : String Index 64 Read F_type Write Set_type;
    Property volumeId : String Index 72 Read FvolumeId Write SetvolumeId;
  end;
  TReviewClass = Class of TReview;
  
  { --------------------------------------------------------------------
    TSeriesTypeseriesItem
    --------------------------------------------------------------------}
  
  TSeriesTypeseriesItem = Class(TGoogleBaseObject)
  Private
    FbannerImageUrl : String;
    FimageUrl : String;
    FseriesId : String;
    FseriesType : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetbannerImageUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetimageUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseriesId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseriesType(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property bannerImageUrl : String Index 0 Read FbannerImageUrl Write SetbannerImageUrl;
    Property imageUrl : String Index 8 Read FimageUrl Write SetimageUrl;
    Property seriesId : String Index 16 Read FseriesId Write SetseriesId;
    Property seriesType : String Index 24 Read FseriesType Write SetseriesType;
    Property title : String Index 32 Read Ftitle Write Settitle;
  end;
  TSeriesTypeseriesItemClass = Class of TSeriesTypeseriesItem;
  
  { --------------------------------------------------------------------
    TSeries
    --------------------------------------------------------------------}
  
  TSeries = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fseries : TSeriesTypeseriesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setseries(AIndex : Integer; const AValue : TSeriesTypeseriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property series : TSeriesTypeseriesArray Index 8 Read Fseries Write Setseries;
  end;
  TSeriesClass = Class of TSeries;
  
  { --------------------------------------------------------------------
    TSeriesmembership
    --------------------------------------------------------------------}
  
  TSeriesmembership = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fmember : TSeriesmembershipTypememberArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmember(AIndex : Integer; const AValue : TSeriesmembershipTypememberArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property member : TSeriesmembershipTypememberArray Index 8 Read Fmember Write Setmember;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TSeriesmembershipClass = Class of TSeriesmembership;
  
  { --------------------------------------------------------------------
    TUsersettingsTypenotesExport
    --------------------------------------------------------------------}
  
  TUsersettingsTypenotesExport = Class(TGoogleBaseObject)
  Private
    FfolderName : String;
    FisEnabled : boolean;
  Protected
    //Property setters
    Procedure SetfolderName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisEnabled(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property folderName : String Index 0 Read FfolderName Write SetfolderName;
    Property isEnabled : boolean Index 8 Read FisEnabled Write SetisEnabled;
  end;
  TUsersettingsTypenotesExportClass = Class of TUsersettingsTypenotesExport;
  
  { --------------------------------------------------------------------
    TUsersettingsTypenotificationTypemoreFromAuthors
    --------------------------------------------------------------------}
  
  TUsersettingsTypenotificationTypemoreFromAuthors = Class(TGoogleBaseObject)
  Private
    Fopted_state : String;
  Protected
    //Property setters
    Procedure Setopted_state(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property opted_state : String Index 0 Read Fopted_state Write Setopted_state;
  end;
  TUsersettingsTypenotificationTypemoreFromAuthorsClass = Class of TUsersettingsTypenotificationTypemoreFromAuthors;
  
  { --------------------------------------------------------------------
    TUsersettingsTypenotification
    --------------------------------------------------------------------}
  
  TUsersettingsTypenotification = Class(TGoogleBaseObject)
  Private
    FmoreFromAuthors : TUsersettingsTypenotificationTypemoreFromAuthors;
  Protected
    //Property setters
    Procedure SetmoreFromAuthors(AIndex : Integer; const AValue : TUsersettingsTypenotificationTypemoreFromAuthors); virtual;
  Public
  Published
    Property moreFromAuthors : TUsersettingsTypenotificationTypemoreFromAuthors Index 0 Read FmoreFromAuthors Write SetmoreFromAuthors;
  end;
  TUsersettingsTypenotificationClass = Class of TUsersettingsTypenotification;
  
  { --------------------------------------------------------------------
    TUsersettings
    --------------------------------------------------------------------}
  
  TUsersettings = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnotesExport : TUsersettingsTypenotesExport;
    Fnotification : TUsersettingsTypenotification;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnotesExport(AIndex : Integer; const AValue : TUsersettingsTypenotesExport); virtual;
    Procedure Setnotification(AIndex : Integer; const AValue : TUsersettingsTypenotification); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property notesExport : TUsersettingsTypenotesExport Index 8 Read FnotesExport Write SetnotesExport;
    Property notification : TUsersettingsTypenotification Index 16 Read Fnotification Write Setnotification;
  end;
  TUsersettingsClass = Class of TUsersettings;
  
  { --------------------------------------------------------------------
    TVolumeTypeaccessInfoTypeepub
    --------------------------------------------------------------------}
  
  TVolumeTypeaccessInfoTypeepub = Class(TGoogleBaseObject)
  Private
    FacsTokenLink : String;
    FdownloadLink : String;
    FisAvailable : boolean;
  Protected
    //Property setters
    Procedure SetacsTokenLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdownloadLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisAvailable(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property acsTokenLink : String Index 0 Read FacsTokenLink Write SetacsTokenLink;
    Property downloadLink : String Index 8 Read FdownloadLink Write SetdownloadLink;
    Property isAvailable : boolean Index 16 Read FisAvailable Write SetisAvailable;
  end;
  TVolumeTypeaccessInfoTypeepubClass = Class of TVolumeTypeaccessInfoTypeepub;
  
  { --------------------------------------------------------------------
    TVolumeTypeaccessInfoTypepdf
    --------------------------------------------------------------------}
  
  TVolumeTypeaccessInfoTypepdf = Class(TGoogleBaseObject)
  Private
    FacsTokenLink : String;
    FdownloadLink : String;
    FisAvailable : boolean;
  Protected
    //Property setters
    Procedure SetacsTokenLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdownloadLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisAvailable(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property acsTokenLink : String Index 0 Read FacsTokenLink Write SetacsTokenLink;
    Property downloadLink : String Index 8 Read FdownloadLink Write SetdownloadLink;
    Property isAvailable : boolean Index 16 Read FisAvailable Write SetisAvailable;
  end;
  TVolumeTypeaccessInfoTypepdfClass = Class of TVolumeTypeaccessInfoTypepdf;
  
  { --------------------------------------------------------------------
    TVolumeTypeaccessInfo
    --------------------------------------------------------------------}
  
  TVolumeTypeaccessInfo = Class(TGoogleBaseObject)
  Private
    FaccessViewStatus : String;
    Fcountry : String;
    FdownloadAccess : TDownloadAccessRestriction;
    FdriveImportedContentLink : String;
    Fembeddable : boolean;
    Fepub : TVolumeTypeaccessInfoTypeepub;
    FexplicitOfflineLicenseManagement : boolean;
    Fpdf : TVolumeTypeaccessInfoTypepdf;
    FpublicDomain : boolean;
    FquoteSharingAllowed : boolean;
    FtextToSpeechPermission : String;
    FviewOrderUrl : String;
    Fviewability : String;
    FwebReaderLink : String;
  Protected
    //Property setters
    Procedure SetaccessViewStatus(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcountry(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdownloadAccess(AIndex : Integer; const AValue : TDownloadAccessRestriction); virtual;
    Procedure SetdriveImportedContentLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setembeddable(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setepub(AIndex : Integer; const AValue : TVolumeTypeaccessInfoTypeepub); virtual;
    Procedure SetexplicitOfflineLicenseManagement(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setpdf(AIndex : Integer; const AValue : TVolumeTypeaccessInfoTypepdf); virtual;
    Procedure SetpublicDomain(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetquoteSharingAllowed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettextToSpeechPermission(AIndex : Integer; const AValue : String); virtual;
    Procedure SetviewOrderUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setviewability(AIndex : Integer; const AValue : String); virtual;
    Procedure SetwebReaderLink(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accessViewStatus : String Index 0 Read FaccessViewStatus Write SetaccessViewStatus;
    Property country : String Index 8 Read Fcountry Write Setcountry;
    Property downloadAccess : TDownloadAccessRestriction Index 16 Read FdownloadAccess Write SetdownloadAccess;
    Property driveImportedContentLink : String Index 24 Read FdriveImportedContentLink Write SetdriveImportedContentLink;
    Property embeddable : boolean Index 32 Read Fembeddable Write Setembeddable;
    Property epub : TVolumeTypeaccessInfoTypeepub Index 40 Read Fepub Write Setepub;
    Property explicitOfflineLicenseManagement : boolean Index 48 Read FexplicitOfflineLicenseManagement Write SetexplicitOfflineLicenseManagement;
    Property pdf : TVolumeTypeaccessInfoTypepdf Index 56 Read Fpdf Write Setpdf;
    Property publicDomain : boolean Index 64 Read FpublicDomain Write SetpublicDomain;
    Property quoteSharingAllowed : boolean Index 72 Read FquoteSharingAllowed Write SetquoteSharingAllowed;
    Property textToSpeechPermission : String Index 80 Read FtextToSpeechPermission Write SettextToSpeechPermission;
    Property viewOrderUrl : String Index 88 Read FviewOrderUrl Write SetviewOrderUrl;
    Property viewability : String Index 96 Read Fviewability Write Setviewability;
    Property webReaderLink : String Index 104 Read FwebReaderLink Write SetwebReaderLink;
  end;
  TVolumeTypeaccessInfoClass = Class of TVolumeTypeaccessInfo;
  
  { --------------------------------------------------------------------
    TVolumeTypelayerInfoTypelayersItem
    --------------------------------------------------------------------}
  
  TVolumeTypelayerInfoTypelayersItem = Class(TGoogleBaseObject)
  Private
    FlayerId : String;
    FvolumeAnnotationsVersion : String;
  Protected
    //Property setters
    Procedure SetlayerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvolumeAnnotationsVersion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property layerId : String Index 0 Read FlayerId Write SetlayerId;
    Property volumeAnnotationsVersion : String Index 8 Read FvolumeAnnotationsVersion Write SetvolumeAnnotationsVersion;
  end;
  TVolumeTypelayerInfoTypelayersItemClass = Class of TVolumeTypelayerInfoTypelayersItem;
  
  { --------------------------------------------------------------------
    TVolumeTypelayerInfo
    --------------------------------------------------------------------}
  
  TVolumeTypelayerInfo = Class(TGoogleBaseObject)
  Private
    Flayers : TVolumeTypelayerInfoTypelayersArray;
  Protected
    //Property setters
    Procedure Setlayers(AIndex : Integer; const AValue : TVolumeTypelayerInfoTypelayersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property layers : TVolumeTypelayerInfoTypelayersArray Index 0 Read Flayers Write Setlayers;
  end;
  TVolumeTypelayerInfoClass = Class of TVolumeTypelayerInfo;
  
  { --------------------------------------------------------------------
    TVolumeTyperecommendedInfo
    --------------------------------------------------------------------}
  
  TVolumeTyperecommendedInfo = Class(TGoogleBaseObject)
  Private
    Fexplanation : String;
  Protected
    //Property setters
    Procedure Setexplanation(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property explanation : String Index 0 Read Fexplanation Write Setexplanation;
  end;
  TVolumeTyperecommendedInfoClass = Class of TVolumeTyperecommendedInfo;
  
  { --------------------------------------------------------------------
    TVolumeTypesaleInfoTypelistPrice
    --------------------------------------------------------------------}
  
  TVolumeTypesaleInfoTypelistPrice = Class(TGoogleBaseObject)
  Private
    Famount : double;
    FcurrencyCode : String;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property amount : double Index 0 Read Famount Write Setamount;
    Property currencyCode : String Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TVolumeTypesaleInfoTypelistPriceClass = Class of TVolumeTypesaleInfoTypelistPrice;
  
  { --------------------------------------------------------------------
    TVolumeTypesaleInfoTypeoffersItemTypelistPrice
    --------------------------------------------------------------------}
  
  TVolumeTypesaleInfoTypeoffersItemTypelistPrice = Class(TGoogleBaseObject)
  Private
    FamountInMicros : double;
    FcurrencyCode : String;
  Protected
    //Property setters
    Procedure SetamountInMicros(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property amountInMicros : double Index 0 Read FamountInMicros Write SetamountInMicros;
    Property currencyCode : String Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TVolumeTypesaleInfoTypeoffersItemTypelistPriceClass = Class of TVolumeTypesaleInfoTypeoffersItemTypelistPrice;
  
  { --------------------------------------------------------------------
    TVolumeTypesaleInfoTypeoffersItemTyperentalDuration
    --------------------------------------------------------------------}
  
  TVolumeTypesaleInfoTypeoffersItemTyperentalDuration = Class(TGoogleBaseObject)
  Private
    Fcount : double;
    F_unit : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcount(AIndex : Integer; const AValue : double); virtual;
    Procedure Set_unit(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property count : double Index 0 Read Fcount Write Setcount;
    Property _unit : String Index 8 Read F_unit Write Set_unit;
  end;
  TVolumeTypesaleInfoTypeoffersItemTyperentalDurationClass = Class of TVolumeTypesaleInfoTypeoffersItemTyperentalDuration;
  
  { --------------------------------------------------------------------
    TVolumeTypesaleInfoTypeoffersItemTyperetailPrice
    --------------------------------------------------------------------}
  
  TVolumeTypesaleInfoTypeoffersItemTyperetailPrice = Class(TGoogleBaseObject)
  Private
    FamountInMicros : double;
    FcurrencyCode : String;
  Protected
    //Property setters
    Procedure SetamountInMicros(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property amountInMicros : double Index 0 Read FamountInMicros Write SetamountInMicros;
    Property currencyCode : String Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TVolumeTypesaleInfoTypeoffersItemTyperetailPriceClass = Class of TVolumeTypesaleInfoTypeoffersItemTyperetailPrice;
  
  { --------------------------------------------------------------------
    TVolumeTypesaleInfoTypeoffersItem
    --------------------------------------------------------------------}
  
  TVolumeTypesaleInfoTypeoffersItem = Class(TGoogleBaseObject)
  Private
    FfinskyOfferType : integer;
    FlistPrice : TVolumeTypesaleInfoTypeoffersItemTypelistPrice;
    FrentalDuration : TVolumeTypesaleInfoTypeoffersItemTyperentalDuration;
    FretailPrice : TVolumeTypesaleInfoTypeoffersItemTyperetailPrice;
  Protected
    //Property setters
    Procedure SetfinskyOfferType(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetlistPrice(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypeoffersItemTypelistPrice); virtual;
    Procedure SetrentalDuration(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypeoffersItemTyperentalDuration); virtual;
    Procedure SetretailPrice(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypeoffersItemTyperetailPrice); virtual;
  Public
  Published
    Property finskyOfferType : integer Index 0 Read FfinskyOfferType Write SetfinskyOfferType;
    Property listPrice : TVolumeTypesaleInfoTypeoffersItemTypelistPrice Index 8 Read FlistPrice Write SetlistPrice;
    Property rentalDuration : TVolumeTypesaleInfoTypeoffersItemTyperentalDuration Index 16 Read FrentalDuration Write SetrentalDuration;
    Property retailPrice : TVolumeTypesaleInfoTypeoffersItemTyperetailPrice Index 24 Read FretailPrice Write SetretailPrice;
  end;
  TVolumeTypesaleInfoTypeoffersItemClass = Class of TVolumeTypesaleInfoTypeoffersItem;
  
  { --------------------------------------------------------------------
    TVolumeTypesaleInfoTyperetailPrice
    --------------------------------------------------------------------}
  
  TVolumeTypesaleInfoTyperetailPrice = Class(TGoogleBaseObject)
  Private
    Famount : double;
    FcurrencyCode : String;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property amount : double Index 0 Read Famount Write Setamount;
    Property currencyCode : String Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TVolumeTypesaleInfoTyperetailPriceClass = Class of TVolumeTypesaleInfoTyperetailPrice;
  
  { --------------------------------------------------------------------
    TVolumeTypesaleInfo
    --------------------------------------------------------------------}
  
  TVolumeTypesaleInfo = Class(TGoogleBaseObject)
  Private
    FbuyLink : String;
    Fcountry : String;
    FisEbook : boolean;
    FlistPrice : TVolumeTypesaleInfoTypelistPrice;
    Foffers : TVolumeTypesaleInfoTypeoffersArray;
    FonSaleDate : TDatetime;
    FretailPrice : TVolumeTypesaleInfoTyperetailPrice;
    Fsaleability : String;
  Protected
    //Property setters
    Procedure SetbuyLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcountry(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisEbook(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetlistPrice(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypelistPrice); virtual;
    Procedure Setoffers(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypeoffersArray); virtual;
    Procedure SetonSaleDate(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetretailPrice(AIndex : Integer; const AValue : TVolumeTypesaleInfoTyperetailPrice); virtual;
    Procedure Setsaleability(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property buyLink : String Index 0 Read FbuyLink Write SetbuyLink;
    Property country : String Index 8 Read Fcountry Write Setcountry;
    Property isEbook : boolean Index 16 Read FisEbook Write SetisEbook;
    Property listPrice : TVolumeTypesaleInfoTypelistPrice Index 24 Read FlistPrice Write SetlistPrice;
    Property offers : TVolumeTypesaleInfoTypeoffersArray Index 32 Read Foffers Write Setoffers;
    Property onSaleDate : TDatetime Index 40 Read FonSaleDate Write SetonSaleDate;
    Property retailPrice : TVolumeTypesaleInfoTyperetailPrice Index 48 Read FretailPrice Write SetretailPrice;
    Property saleability : String Index 56 Read Fsaleability Write Setsaleability;
  end;
  TVolumeTypesaleInfoClass = Class of TVolumeTypesaleInfo;
  
  { --------------------------------------------------------------------
    TVolumeTypesearchInfo
    --------------------------------------------------------------------}
  
  TVolumeTypesearchInfo = Class(TGoogleBaseObject)
  Private
    FtextSnippet : String;
  Protected
    //Property setters
    Procedure SettextSnippet(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property textSnippet : String Index 0 Read FtextSnippet Write SettextSnippet;
  end;
  TVolumeTypesearchInfoClass = Class of TVolumeTypesearchInfo;
  
  { --------------------------------------------------------------------
    TVolumeTypeuserInfoTypecopy
    --------------------------------------------------------------------}
  
  TVolumeTypeuserInfoTypecopy = Class(TGoogleBaseObject)
  Private
    FallowedCharacterCount : integer;
    FlimitType : String;
    FremainingCharacterCount : integer;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure SetallowedCharacterCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetlimitType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetremainingCharacterCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
  Public
  Published
    Property allowedCharacterCount : integer Index 0 Read FallowedCharacterCount Write SetallowedCharacterCount;
    Property limitType : String Index 8 Read FlimitType Write SetlimitType;
    Property remainingCharacterCount : integer Index 16 Read FremainingCharacterCount Write SetremainingCharacterCount;
    Property updated : TDatetime Index 24 Read Fupdated Write Setupdated;
  end;
  TVolumeTypeuserInfoTypecopyClass = Class of TVolumeTypeuserInfoTypecopy;
  
  { --------------------------------------------------------------------
    TVolumeTypeuserInfoTypefamilySharing
    --------------------------------------------------------------------}
  
  TVolumeTypeuserInfoTypefamilySharing = Class(TGoogleBaseObject)
  Private
    FfamilyRole : String;
    FisSharingAllowed : boolean;
    FisSharingDisabledByFop : boolean;
  Protected
    //Property setters
    Procedure SetfamilyRole(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisSharingAllowed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisSharingDisabledByFop(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property familyRole : String Index 0 Read FfamilyRole Write SetfamilyRole;
    Property isSharingAllowed : boolean Index 8 Read FisSharingAllowed Write SetisSharingAllowed;
    Property isSharingDisabledByFop : boolean Index 16 Read FisSharingDisabledByFop Write SetisSharingDisabledByFop;
  end;
  TVolumeTypeuserInfoTypefamilySharingClass = Class of TVolumeTypeuserInfoTypefamilySharing;
  
  { --------------------------------------------------------------------
    TVolumeTypeuserInfoTyperentalPeriod
    --------------------------------------------------------------------}
  
  TVolumeTypeuserInfoTyperentalPeriod = Class(TGoogleBaseObject)
  Private
    FendUtcSec : String;
    FstartUtcSec : String;
  Protected
    //Property setters
    Procedure SetendUtcSec(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartUtcSec(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property endUtcSec : String Index 0 Read FendUtcSec Write SetendUtcSec;
    Property startUtcSec : String Index 8 Read FstartUtcSec Write SetstartUtcSec;
  end;
  TVolumeTypeuserInfoTyperentalPeriodClass = Class of TVolumeTypeuserInfoTyperentalPeriod;
  
  { --------------------------------------------------------------------
    TVolumeTypeuserInfoTypeuserUploadedVolumeInfo
    --------------------------------------------------------------------}
  
  TVolumeTypeuserInfoTypeuserUploadedVolumeInfo = Class(TGoogleBaseObject)
  Private
    FprocessingState : String;
  Protected
    //Property setters
    Procedure SetprocessingState(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property processingState : String Index 0 Read FprocessingState Write SetprocessingState;
  end;
  TVolumeTypeuserInfoTypeuserUploadedVolumeInfoClass = Class of TVolumeTypeuserInfoTypeuserUploadedVolumeInfo;
  
  { --------------------------------------------------------------------
    TVolumeTypeuserInfo
    --------------------------------------------------------------------}
  
  TVolumeTypeuserInfo = Class(TGoogleBaseObject)
  Private
    FacquiredTime : TDatetime;
    FacquisitionType : integer;
    Fcopy : TVolumeTypeuserInfoTypecopy;
    FentitlementType : integer;
    FfamilySharing : TVolumeTypeuserInfoTypefamilySharing;
    FisFamilySharedFromUser : boolean;
    FisFamilySharedToUser : boolean;
    FisFamilySharingAllowed : boolean;
    FisFamilySharingDisabledByFop : boolean;
    FisInMyBooks : boolean;
    FisPreordered : boolean;
    FisPurchased : boolean;
    FisUploaded : boolean;
    FreadingPosition : TReadingPosition;
    FrentalPeriod : TVolumeTypeuserInfoTyperentalPeriod;
    FrentalState : String;
    Freview : TReview;
    Fupdated : TDatetime;
    FuserUploadedVolumeInfo : TVolumeTypeuserInfoTypeuserUploadedVolumeInfo;
  Protected
    //Property setters
    Procedure SetacquiredTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetacquisitionType(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setcopy(AIndex : Integer; const AValue : TVolumeTypeuserInfoTypecopy); virtual;
    Procedure SetentitlementType(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetfamilySharing(AIndex : Integer; const AValue : TVolumeTypeuserInfoTypefamilySharing); virtual;
    Procedure SetisFamilySharedFromUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisFamilySharedToUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisFamilySharingAllowed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisFamilySharingDisabledByFop(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisInMyBooks(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisPreordered(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisPurchased(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisUploaded(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetreadingPosition(AIndex : Integer; const AValue : TReadingPosition); virtual;
    Procedure SetrentalPeriod(AIndex : Integer; const AValue : TVolumeTypeuserInfoTyperentalPeriod); virtual;
    Procedure SetrentalState(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreview(AIndex : Integer; const AValue : TReview); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetuserUploadedVolumeInfo(AIndex : Integer; const AValue : TVolumeTypeuserInfoTypeuserUploadedVolumeInfo); virtual;
  Public
  Published
    Property acquiredTime : TDatetime Index 0 Read FacquiredTime Write SetacquiredTime;
    Property acquisitionType : integer Index 8 Read FacquisitionType Write SetacquisitionType;
    Property copy : TVolumeTypeuserInfoTypecopy Index 16 Read Fcopy Write Setcopy;
    Property entitlementType : integer Index 24 Read FentitlementType Write SetentitlementType;
    Property familySharing : TVolumeTypeuserInfoTypefamilySharing Index 32 Read FfamilySharing Write SetfamilySharing;
    Property isFamilySharedFromUser : boolean Index 40 Read FisFamilySharedFromUser Write SetisFamilySharedFromUser;
    Property isFamilySharedToUser : boolean Index 48 Read FisFamilySharedToUser Write SetisFamilySharedToUser;
    Property isFamilySharingAllowed : boolean Index 56 Read FisFamilySharingAllowed Write SetisFamilySharingAllowed;
    Property isFamilySharingDisabledByFop : boolean Index 64 Read FisFamilySharingDisabledByFop Write SetisFamilySharingDisabledByFop;
    Property isInMyBooks : boolean Index 72 Read FisInMyBooks Write SetisInMyBooks;
    Property isPreordered : boolean Index 80 Read FisPreordered Write SetisPreordered;
    Property isPurchased : boolean Index 88 Read FisPurchased Write SetisPurchased;
    Property isUploaded : boolean Index 96 Read FisUploaded Write SetisUploaded;
    Property readingPosition : TReadingPosition Index 104 Read FreadingPosition Write SetreadingPosition;
    Property rentalPeriod : TVolumeTypeuserInfoTyperentalPeriod Index 112 Read FrentalPeriod Write SetrentalPeriod;
    Property rentalState : String Index 120 Read FrentalState Write SetrentalState;
    Property review : TReview Index 128 Read Freview Write Setreview;
    Property updated : TDatetime Index 136 Read Fupdated Write Setupdated;
    Property userUploadedVolumeInfo : TVolumeTypeuserInfoTypeuserUploadedVolumeInfo Index 144 Read FuserUploadedVolumeInfo Write SetuserUploadedVolumeInfo;
  end;
  TVolumeTypeuserInfoClass = Class of TVolumeTypeuserInfo;
  
  { --------------------------------------------------------------------
    TVolumeTypevolumeInfoTypedimensions
    --------------------------------------------------------------------}
  
  TVolumeTypevolumeInfoTypedimensions = Class(TGoogleBaseObject)
  Private
    Fheight : String;
    Fthickness : String;
    Fwidth : String;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : String); virtual;
    Procedure Setthickness(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property height : String Index 0 Read Fheight Write Setheight;
    Property thickness : String Index 8 Read Fthickness Write Setthickness;
    Property width : String Index 16 Read Fwidth Write Setwidth;
  end;
  TVolumeTypevolumeInfoTypedimensionsClass = Class of TVolumeTypevolumeInfoTypedimensions;
  
  { --------------------------------------------------------------------
    TVolumeTypevolumeInfoTypeimageLinks
    --------------------------------------------------------------------}
  
  TVolumeTypevolumeInfoTypeimageLinks = Class(TGoogleBaseObject)
  Private
    FextraLarge : String;
    Flarge : String;
    Fmedium : String;
    Fsmall : String;
    FsmallThumbnail : String;
    Fthumbnail : String;
  Protected
    //Property setters
    Procedure SetextraLarge(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlarge(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmedium(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsmall(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsmallThumbnail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setthumbnail(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property extraLarge : String Index 0 Read FextraLarge Write SetextraLarge;
    Property large : String Index 8 Read Flarge Write Setlarge;
    Property medium : String Index 16 Read Fmedium Write Setmedium;
    Property small : String Index 24 Read Fsmall Write Setsmall;
    Property smallThumbnail : String Index 32 Read FsmallThumbnail Write SetsmallThumbnail;
    Property thumbnail : String Index 40 Read Fthumbnail Write Setthumbnail;
  end;
  TVolumeTypevolumeInfoTypeimageLinksClass = Class of TVolumeTypevolumeInfoTypeimageLinks;
  
  { --------------------------------------------------------------------
    TVolumeTypevolumeInfoTypeindustryIdentifiersItem
    --------------------------------------------------------------------}
  
  TVolumeTypevolumeInfoTypeindustryIdentifiersItem = Class(TGoogleBaseObject)
  Private
    Fidentifier : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setidentifier(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property identifier : String Index 0 Read Fidentifier Write Setidentifier;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TVolumeTypevolumeInfoTypeindustryIdentifiersItemClass = Class of TVolumeTypevolumeInfoTypeindustryIdentifiersItem;
  
  { --------------------------------------------------------------------
    TVolumeTypevolumeInfo
    --------------------------------------------------------------------}
  
  TVolumeTypevolumeInfo = Class(TGoogleBaseObject)
  Private
    FallowAnonLogging : boolean;
    Fauthors : TStringArray;
    FaverageRating : double;
    FcanonicalVolumeLink : String;
    Fcategories : TStringArray;
    FcontentVersion : String;
    Fdescription : String;
    Fdimensions : TVolumeTypevolumeInfoTypedimensions;
    FimageLinks : TVolumeTypevolumeInfoTypeimageLinks;
    FindustryIdentifiers : TVolumeTypevolumeInfoTypeindustryIdentifiersArray;
    FinfoLink : String;
    Flanguage : String;
    FmainCategory : String;
    FmaturityRating : String;
    FpageCount : integer;
    FpreviewLink : String;
    FprintType : String;
    FprintedPageCount : integer;
    FpublishedDate : String;
    Fpublisher : String;
    FratingsCount : integer;
    FreadingModes : TJSONSchema;
    FsamplePageCount : integer;
    FseriesInfo : TVolumeseriesinfo;
    Fsubtitle : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetallowAnonLogging(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setauthors(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetaverageRating(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcanonicalVolumeLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcategories(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetcontentVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdimensions(AIndex : Integer; const AValue : TVolumeTypevolumeInfoTypedimensions); virtual;
    Procedure SetimageLinks(AIndex : Integer; const AValue : TVolumeTypevolumeInfoTypeimageLinks); virtual;
    Procedure SetindustryIdentifiers(AIndex : Integer; const AValue : TVolumeTypevolumeInfoTypeindustryIdentifiersArray); virtual;
    Procedure SetinfoLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmainCategory(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaturityRating(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetpreviewLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprintType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprintedPageCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetpublishedDate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpublisher(AIndex : Integer; const AValue : String); virtual;
    Procedure SetratingsCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetreadingModes(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure SetsamplePageCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetseriesInfo(AIndex : Integer; const AValue : TVolumeseriesinfo); virtual;
    Procedure Setsubtitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property allowAnonLogging : boolean Index 0 Read FallowAnonLogging Write SetallowAnonLogging;
    Property authors : TStringArray Index 8 Read Fauthors Write Setauthors;
    Property averageRating : double Index 16 Read FaverageRating Write SetaverageRating;
    Property canonicalVolumeLink : String Index 24 Read FcanonicalVolumeLink Write SetcanonicalVolumeLink;
    Property categories : TStringArray Index 32 Read Fcategories Write Setcategories;
    Property contentVersion : String Index 40 Read FcontentVersion Write SetcontentVersion;
    Property description : String Index 48 Read Fdescription Write Setdescription;
    Property dimensions : TVolumeTypevolumeInfoTypedimensions Index 56 Read Fdimensions Write Setdimensions;
    Property imageLinks : TVolumeTypevolumeInfoTypeimageLinks Index 64 Read FimageLinks Write SetimageLinks;
    Property industryIdentifiers : TVolumeTypevolumeInfoTypeindustryIdentifiersArray Index 72 Read FindustryIdentifiers Write SetindustryIdentifiers;
    Property infoLink : String Index 80 Read FinfoLink Write SetinfoLink;
    Property language : String Index 88 Read Flanguage Write Setlanguage;
    Property mainCategory : String Index 96 Read FmainCategory Write SetmainCategory;
    Property maturityRating : String Index 104 Read FmaturityRating Write SetmaturityRating;
    Property pageCount : integer Index 112 Read FpageCount Write SetpageCount;
    Property previewLink : String Index 120 Read FpreviewLink Write SetpreviewLink;
    Property printType : String Index 128 Read FprintType Write SetprintType;
    Property printedPageCount : integer Index 136 Read FprintedPageCount Write SetprintedPageCount;
    Property publishedDate : String Index 144 Read FpublishedDate Write SetpublishedDate;
    Property publisher : String Index 152 Read Fpublisher Write Setpublisher;
    Property ratingsCount : integer Index 160 Read FratingsCount Write SetratingsCount;
    Property readingModes : TJSONSchema Index 168 Read FreadingModes Write SetreadingModes;
    Property samplePageCount : integer Index 176 Read FsamplePageCount Write SetsamplePageCount;
    Property seriesInfo : TVolumeseriesinfo Index 184 Read FseriesInfo Write SetseriesInfo;
    Property subtitle : String Index 192 Read Fsubtitle Write Setsubtitle;
    Property title : String Index 200 Read Ftitle Write Settitle;
  end;
  TVolumeTypevolumeInfoClass = Class of TVolumeTypevolumeInfo;
  
  { --------------------------------------------------------------------
    TVolume
    --------------------------------------------------------------------}
  
  TVolume = Class(TGoogleBaseObject)
  Private
    FaccessInfo : TVolumeTypeaccessInfo;
    Fetag : String;
    Fid : String;
    Fkind : String;
    FlayerInfo : TVolumeTypelayerInfo;
    FrecommendedInfo : TVolumeTyperecommendedInfo;
    FsaleInfo : TVolumeTypesaleInfo;
    FsearchInfo : TVolumeTypesearchInfo;
    FselfLink : String;
    FuserInfo : TVolumeTypeuserInfo;
    FvolumeInfo : TVolumeTypevolumeInfo;
  Protected
    //Property setters
    Procedure SetaccessInfo(AIndex : Integer; const AValue : TVolumeTypeaccessInfo); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlayerInfo(AIndex : Integer; const AValue : TVolumeTypelayerInfo); virtual;
    Procedure SetrecommendedInfo(AIndex : Integer; const AValue : TVolumeTyperecommendedInfo); virtual;
    Procedure SetsaleInfo(AIndex : Integer; const AValue : TVolumeTypesaleInfo); virtual;
    Procedure SetsearchInfo(AIndex : Integer; const AValue : TVolumeTypesearchInfo); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserInfo(AIndex : Integer; const AValue : TVolumeTypeuserInfo); virtual;
    Procedure SetvolumeInfo(AIndex : Integer; const AValue : TVolumeTypevolumeInfo); virtual;
  Public
  Published
    Property accessInfo : TVolumeTypeaccessInfo Index 0 Read FaccessInfo Write SetaccessInfo;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property layerInfo : TVolumeTypelayerInfo Index 32 Read FlayerInfo Write SetlayerInfo;
    Property recommendedInfo : TVolumeTyperecommendedInfo Index 40 Read FrecommendedInfo Write SetrecommendedInfo;
    Property saleInfo : TVolumeTypesaleInfo Index 48 Read FsaleInfo Write SetsaleInfo;
    Property searchInfo : TVolumeTypesearchInfo Index 56 Read FsearchInfo Write SetsearchInfo;
    Property selfLink : String Index 64 Read FselfLink Write SetselfLink;
    Property userInfo : TVolumeTypeuserInfo Index 72 Read FuserInfo Write SetuserInfo;
    Property volumeInfo : TVolumeTypevolumeInfo Index 80 Read FvolumeInfo Write SetvolumeInfo;
  end;
  TVolumeClass = Class of TVolume;
  
  { --------------------------------------------------------------------
    TVolume2
    --------------------------------------------------------------------}
  
  TVolume2 = Class(TGoogleBaseObject)
  Private
    Fitems : TVolume2TypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TVolume2TypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TVolume2TypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TVolume2Class = Class of TVolume2;
  
  { --------------------------------------------------------------------
    TVolumeannotationTypecontentRanges
    --------------------------------------------------------------------}
  
  TVolumeannotationTypecontentRanges = Class(TGoogleBaseObject)
  Private
    FcfiRange : TBooksAnnotationsRange;
    FcontentVersion : String;
    FgbImageRange : TBooksAnnotationsRange;
    FgbTextRange : TBooksAnnotationsRange;
  Protected
    //Property setters
    Procedure SetcfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
    Procedure SetcontentVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgbImageRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
    Procedure SetgbTextRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); virtual;
  Public
  Published
    Property cfiRange : TBooksAnnotationsRange Index 0 Read FcfiRange Write SetcfiRange;
    Property contentVersion : String Index 8 Read FcontentVersion Write SetcontentVersion;
    Property gbImageRange : TBooksAnnotationsRange Index 16 Read FgbImageRange Write SetgbImageRange;
    Property gbTextRange : TBooksAnnotationsRange Index 24 Read FgbTextRange Write SetgbTextRange;
  end;
  TVolumeannotationTypecontentRangesClass = Class of TVolumeannotationTypecontentRanges;
  
  { --------------------------------------------------------------------
    TVolumeannotation
    --------------------------------------------------------------------}
  
  TVolumeannotation = Class(TGoogleBaseObject)
  Private
    FannotationDataId : String;
    FannotationDataLink : String;
    FannotationType : String;
    FcontentRanges : TVolumeannotationTypecontentRanges;
    Fdata : String;
    Fdeleted : boolean;
    Fid : String;
    Fkind : String;
    FlayerId : String;
    FpageIds : TStringArray;
    FselectedText : String;
    FselfLink : String;
    Fupdated : TDatetime;
    FvolumeId : String;
  Protected
    //Property setters
    Procedure SetannotationDataId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetannotationDataLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetannotationType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontentRanges(AIndex : Integer; const AValue : TVolumeannotationTypecontentRanges); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdeleted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlayerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetselectedText(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetvolumeId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property annotationDataId : String Index 0 Read FannotationDataId Write SetannotationDataId;
    Property annotationDataLink : String Index 8 Read FannotationDataLink Write SetannotationDataLink;
    Property annotationType : String Index 16 Read FannotationType Write SetannotationType;
    Property contentRanges : TVolumeannotationTypecontentRanges Index 24 Read FcontentRanges Write SetcontentRanges;
    Property data : String Index 32 Read Fdata Write Setdata;
    Property deleted : boolean Index 40 Read Fdeleted Write Setdeleted;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property layerId : String Index 64 Read FlayerId Write SetlayerId;
    Property pageIds : TStringArray Index 72 Read FpageIds Write SetpageIds;
    Property selectedText : String Index 80 Read FselectedText Write SetselectedText;
    Property selfLink : String Index 88 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 96 Read Fupdated Write Setupdated;
    Property volumeId : String Index 104 Read FvolumeId Write SetvolumeId;
  end;
  TVolumeannotationClass = Class of TVolumeannotation;
  
  { --------------------------------------------------------------------
    TVolumeannotations
    --------------------------------------------------------------------}
  
  TVolumeannotations = Class(TGoogleBaseObject)
  Private
    Fitems : TVolumeannotationsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FtotalItems : integer;
    Fversion : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TVolumeannotationsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TVolumeannotationsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
    Property version : String Index 32 Read Fversion Write Setversion;
  end;
  TVolumeannotationsClass = Class of TVolumeannotations;
  
  { --------------------------------------------------------------------
    TVolumes
    --------------------------------------------------------------------}
  
  TVolumes = Class(TGoogleBaseObject)
  Private
    Fitems : TVolumesTypeitemsArray;
    Fkind : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; const AValue : TVolumesTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TVolumesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property totalItems : integer Index 16 Read FtotalItems Write SettotalItems;
  end;
  TVolumesClass = Class of TVolumes;
  
  { --------------------------------------------------------------------
    TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem
    --------------------------------------------------------------------}
  
  TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem = Class(TGoogleBaseObject)
  Private
    FissueDisplayNumber : String;
    FissueOrderNumber : integer;
  Protected
    //Property setters
    Procedure SetissueDisplayNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetissueOrderNumber(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property issueDisplayNumber : String Index 0 Read FissueDisplayNumber Write SetissueDisplayNumber;
    Property issueOrderNumber : integer Index 8 Read FissueOrderNumber Write SetissueOrderNumber;
  end;
  TVolumeseriesinfoTypevolumeSeriesItemTypeissueItemClass = Class of TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem;
  
  { --------------------------------------------------------------------
    TVolumeseriesinfoTypevolumeSeriesItem
    --------------------------------------------------------------------}
  
  TVolumeseriesinfoTypevolumeSeriesItem = Class(TGoogleBaseObject)
  Private
    Fissue : TVolumeseriesinfoTypevolumeSeriesItemTypeissueArray;
    ForderNumber : integer;
    FseriesBookType : String;
    FseriesId : String;
  Protected
    //Property setters
    Procedure Setissue(AIndex : Integer; const AValue : TVolumeseriesinfoTypevolumeSeriesItemTypeissueArray); virtual;
    Procedure SetorderNumber(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetseriesBookType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseriesId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property issue : TVolumeseriesinfoTypevolumeSeriesItemTypeissueArray Index 0 Read Fissue Write Setissue;
    Property orderNumber : integer Index 8 Read ForderNumber Write SetorderNumber;
    Property seriesBookType : String Index 16 Read FseriesBookType Write SetseriesBookType;
    Property seriesId : String Index 24 Read FseriesId Write SetseriesId;
  end;
  TVolumeseriesinfoTypevolumeSeriesItemClass = Class of TVolumeseriesinfoTypevolumeSeriesItem;
  
  { --------------------------------------------------------------------
    TVolumeseriesinfo
    --------------------------------------------------------------------}
  
  TVolumeseriesinfo = Class(TGoogleBaseObject)
  Private
    FbookDisplayNumber : String;
    Fkind : String;
    FshortSeriesBookTitle : String;
    FvolumeSeries : TVolumeseriesinfoTypevolumeSeriesArray;
  Protected
    //Property setters
    Procedure SetbookDisplayNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetshortSeriesBookTitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvolumeSeries(AIndex : Integer; const AValue : TVolumeseriesinfoTypevolumeSeriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bookDisplayNumber : String Index 0 Read FbookDisplayNumber Write SetbookDisplayNumber;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property shortSeriesBookTitle : String Index 16 Read FshortSeriesBookTitle Write SetshortSeriesBookTitle;
    Property volumeSeries : TVolumeseriesinfoTypevolumeSeriesArray Index 24 Read FvolumeSeries Write SetvolumeSeries;
  end;
  TVolumeseriesinfoClass = Class of TVolumeseriesinfo;
  
  { --------------------------------------------------------------------
    TBookshelvesVolumesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBookshelvesVolumesResource, method List
  
  TBookshelvesVolumesListOptions = Record
    maxResults : integer;
    showPreorders : boolean;
    source : String;
    startIndex : integer;
  end;
  
  TBookshelvesVolumesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(shelf: string; userId: string; AQuery : string  = '') : TVolumes;
    Function List(shelf: string; userId: string; AQuery : TBookshelvesVolumeslistOptions) : TVolumes;
  end;
  
  
  { --------------------------------------------------------------------
    TBookshelvesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBookshelvesResource, method Get
  
  TBookshelvesGetOptions = Record
    source : String;
  end;
  
  
  //Optional query Options for TBookshelvesResource, method List
  
  TBookshelvesListOptions = Record
    source : String;
  end;
  
  TBookshelvesResource = Class(TGoogleResource)
  Private
    FVolumesInstance : TBookshelvesVolumesResource;
    Function GetVolumesInstance : TBookshelvesVolumesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(shelf: string; userId: string; AQuery : string  = '') : TBookshelf;
    Function Get(shelf: string; userId: string; AQuery : TBookshelvesgetOptions) : TBookshelf;
    Function List(userId: string; AQuery : string  = '') : TBookshelves;
    Function List(userId: string; AQuery : TBookshelveslistOptions) : TBookshelves;
    Function CreateVolumesResource(AOwner : TComponent) : TBookshelvesVolumesResource;virtual;overload;
    Function CreateVolumesResource : TBookshelvesVolumesResource;virtual;overload;
    Property VolumesResource : TBookshelvesVolumesResource Read GetVolumesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TCloudloadingResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCloudloadingResource, method AddBook
  
  TCloudloadingAddBookOptions = Record
    drive_document_id : String;
    mime_type : String;
    _name : String;
    upload_client_token : String;
  end;
  
  
  //Optional query Options for TCloudloadingResource, method DeleteBook
  
  TCloudloadingDeleteBookOptions = Record
    volumeId : String;
  end;
  
  TCloudloadingResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AddBook(AQuery : string  = '') : TBooksCloudloadingResource;
    Function AddBook(AQuery : TCloudloadingaddBookOptions) : TBooksCloudloadingResource;
    Procedure DeleteBook(AQuery : string  = '');
    Procedure DeleteBook(AQuery : TCloudloadingdeleteBookOptions);
    Function UpdateBook(aBooksCloudloadingResource : TBooksCloudloadingResource) : TBooksCloudloadingResource;
  end;
  
  
  { --------------------------------------------------------------------
    TDictionaryResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDictionaryResource, method ListOfflineMetadata
  
  TDictionaryListOfflineMetadataOptions = Record
    cpksver : String;
  end;
  
  TDictionaryResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function ListOfflineMetadata(AQuery : string  = '') : TMetadata;
    Function ListOfflineMetadata(AQuery : TDictionarylistOfflineMetadataOptions) : TMetadata;
  end;
  
  
  { --------------------------------------------------------------------
    TLayersAnnotationDataResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLayersAnnotationDataResource, method Get
  
  TLayersAnnotationDataGetOptions = Record
    allowWebDefinitions : boolean;
    contentVersion : String;
    h : integer;
    locale : String;
    scale : integer;
    source : String;
    w : integer;
  end;
  
  
  //Optional query Options for TLayersAnnotationDataResource, method List
  
  TLayersAnnotationDataListOptions = Record
    annotationDataId : String;
    contentVersion : String;
    h : integer;
    locale : String;
    maxResults : integer;
    pageToken : String;
    scale : integer;
    source : String;
    updatedMax : String;
    updatedMin : String;
    w : integer;
  end;
  
  TLayersAnnotationDataResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(annotationDataId: string; layerId: string; volumeId: string; AQuery : string  = '') : TAnnotationdata;
    Function Get(annotationDataId: string; layerId: string; volumeId: string; AQuery : TLayersAnnotationDatagetOptions) : TAnnotationdata;
    Function List(layerId: string; volumeId: string; AQuery : string  = '') : TAnnotationsdata;
    Function List(layerId: string; volumeId: string; AQuery : TLayersAnnotationDatalistOptions) : TAnnotationsdata;
  end;
  
  
  { --------------------------------------------------------------------
    TLayersVolumeAnnotationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLayersVolumeAnnotationsResource, method Get
  
  TLayersVolumeAnnotationsGetOptions = Record
    locale : String;
    source : String;
  end;
  
  
  //Optional query Options for TLayersVolumeAnnotationsResource, method List
  
  TLayersVolumeAnnotationsListOptions = Record
    contentVersion : String;
    endOffset : String;
    endPosition : String;
    locale : String;
    maxResults : integer;
    pageToken : String;
    showDeleted : boolean;
    source : String;
    startOffset : String;
    startPosition : String;
    updatedMax : String;
    updatedMin : String;
    volumeAnnotationsVersion : String;
  end;
  
  TLayersVolumeAnnotationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(annotationId: string; layerId: string; volumeId: string; AQuery : string  = '') : TVolumeannotation;
    Function Get(annotationId: string; layerId: string; volumeId: string; AQuery : TLayersVolumeAnnotationsgetOptions) : TVolumeannotation;
    Function List(layerId: string; volumeId: string; AQuery : string  = '') : TVolumeannotations;
    Function List(layerId: string; volumeId: string; AQuery : TLayersVolumeAnnotationslistOptions) : TVolumeannotations;
  end;
  
  
  { --------------------------------------------------------------------
    TLayersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLayersResource, method Get
  
  TLayersGetOptions = Record
    contentVersion : String;
    source : String;
  end;
  
  
  //Optional query Options for TLayersResource, method List
  
  TLayersListOptions = Record
    contentVersion : String;
    maxResults : integer;
    pageToken : String;
    source : String;
  end;
  
  TLayersResource = Class(TGoogleResource)
  Private
    FAnnotationDataInstance : TLayersAnnotationDataResource;
    FVolumeAnnotationsInstance : TLayersVolumeAnnotationsResource;
    Function GetAnnotationDataInstance : TLayersAnnotationDataResource;virtual;
    Function GetVolumeAnnotationsInstance : TLayersVolumeAnnotationsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(summaryId: string; volumeId: string; AQuery : string  = '') : TLayersummary;
    Function Get(summaryId: string; volumeId: string; AQuery : TLayersgetOptions) : TLayersummary;
    Function List(volumeId: string; AQuery : string  = '') : TLayersummaries;
    Function List(volumeId: string; AQuery : TLayerslistOptions) : TLayersummaries;
    Function CreateAnnotationDataResource(AOwner : TComponent) : TLayersAnnotationDataResource;virtual;overload;
    Function CreateAnnotationDataResource : TLayersAnnotationDataResource;virtual;overload;
    Function CreateVolumeAnnotationsResource(AOwner : TComponent) : TLayersVolumeAnnotationsResource;virtual;overload;
    Function CreateVolumeAnnotationsResource : TLayersVolumeAnnotationsResource;virtual;overload;
    Property AnnotationDataResource : TLayersAnnotationDataResource Read GetAnnotationDataInstance;
    Property VolumeAnnotationsResource : TLayersVolumeAnnotationsResource Read GetVolumeAnnotationsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TMyconfigResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMyconfigResource, method ReleaseDownloadAccess
  
  TMyconfigReleaseDownloadAccessOptions = Record
    cpksver : String;
    locale : String;
    source : String;
    volumeIds : String;
  end;
  
  
  //Optional query Options for TMyconfigResource, method RequestAccess
  
  TMyconfigRequestAccessOptions = Record
    cpksver : String;
    licenseTypes : String;
    locale : String;
    nonce : String;
    source : String;
    volumeId : String;
  end;
  
  
  //Optional query Options for TMyconfigResource, method SyncVolumeLicenses
  
  TMyconfigSyncVolumeLicensesOptions = Record
    cpksver : String;
    features : String;
    includeNonComicsSeries : boolean;
    locale : String;
    nonce : String;
    showPreorders : boolean;
    source : String;
    volumeIds : String;
  end;
  
  TMyconfigResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetUserSettings : TUsersettings;
    Function ReleaseDownloadAccess(AQuery : string  = '') : TDownloadAccesses;
    Function ReleaseDownloadAccess(AQuery : TMyconfigreleaseDownloadAccessOptions) : TDownloadAccesses;
    Function RequestAccess(AQuery : string  = '') : TRequestAccess;
    Function RequestAccess(AQuery : TMyconfigrequestAccessOptions) : TRequestAccess;
    Function SyncVolumeLicenses(AQuery : string  = '') : TVolumes;
    Function SyncVolumeLicenses(AQuery : TMyconfigsyncVolumeLicensesOptions) : TVolumes;
    Function UpdateUserSettings(aUsersettings : TUsersettings) : TUsersettings;
  end;
  
  
  { --------------------------------------------------------------------
    TMylibraryAnnotationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMylibraryAnnotationsResource, method Delete
  
  TMylibraryAnnotationsDeleteOptions = Record
    source : String;
  end;
  
  
  //Optional query Options for TMylibraryAnnotationsResource, method Insert
  
  TMylibraryAnnotationsInsertOptions = Record
    country : String;
    showOnlySummaryInResponse : boolean;
    source : String;
  end;
  
  
  //Optional query Options for TMylibraryAnnotationsResource, method List
  
  TMylibraryAnnotationsListOptions = Record
    contentVersion : String;
    layerId : String;
    layerIds : String;
    maxResults : integer;
    pageToken : String;
    showDeleted : boolean;
    source : String;
    updatedMax : String;
    updatedMin : String;
    volumeId : String;
  end;
  
  
  //Optional query Options for TMylibraryAnnotationsResource, method Summary
  
  TMylibraryAnnotationsSummaryOptions = Record
    layerIds : String;
    volumeId : String;
  end;
  
  
  //Optional query Options for TMylibraryAnnotationsResource, method Update
  
  TMylibraryAnnotationsUpdateOptions = Record
    source : String;
  end;
  
  TMylibraryAnnotationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(annotationId: string; AQuery : string  = '');
    Procedure Delete(annotationId: string; AQuery : TMylibraryAnnotationsdeleteOptions);
    Function Insert(aAnnotation : TAnnotation; AQuery : string  = '') : TAnnotation;
    Function Insert(aAnnotation : TAnnotation; AQuery : TMylibraryAnnotationsinsertOptions) : TAnnotation;
    Function List(AQuery : string  = '') : TAnnotations;
    Function List(AQuery : TMylibraryAnnotationslistOptions) : TAnnotations;
    Function Summary(AQuery : string  = '') : TAnnotationsSummary;
    Function Summary(AQuery : TMylibraryAnnotationssummaryOptions) : TAnnotationsSummary;
    Function Update(annotationId: string; aAnnotation : TAnnotation; AQuery : string  = '') : TAnnotation;
    Function Update(annotationId: string; aAnnotation : TAnnotation; AQuery : TMylibraryAnnotationsupdateOptions) : TAnnotation;
  end;
  
  
  { --------------------------------------------------------------------
    TMylibraryBookshelvesVolumesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMylibraryBookshelvesVolumesResource, method List
  
  TMylibraryBookshelvesVolumesListOptions = Record
    country : String;
    maxResults : integer;
    projection : String;
    q : String;
    showPreorders : boolean;
    source : String;
    startIndex : integer;
  end;
  
  TMylibraryBookshelvesVolumesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(shelf: string; AQuery : string  = '') : TVolumes;
    Function List(shelf: string; AQuery : TMylibraryBookshelvesVolumeslistOptions) : TVolumes;
  end;
  
  
  { --------------------------------------------------------------------
    TMylibraryBookshelvesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMylibraryBookshelvesResource, method AddVolume
  
  TMylibraryBookshelvesAddVolumeOptions = Record
    reason : String;
    source : String;
    volumeId : String;
  end;
  
  
  //Optional query Options for TMylibraryBookshelvesResource, method ClearVolumes
  
  TMylibraryBookshelvesClearVolumesOptions = Record
    source : String;
  end;
  
  
  //Optional query Options for TMylibraryBookshelvesResource, method Get
  
  TMylibraryBookshelvesGetOptions = Record
    source : String;
  end;
  
  
  //Optional query Options for TMylibraryBookshelvesResource, method List
  
  TMylibraryBookshelvesListOptions = Record
    source : String;
  end;
  
  
  //Optional query Options for TMylibraryBookshelvesResource, method MoveVolume
  
  TMylibraryBookshelvesMoveVolumeOptions = Record
    source : String;
    volumeId : String;
    volumePosition : integer;
  end;
  
  
  //Optional query Options for TMylibraryBookshelvesResource, method RemoveVolume
  
  TMylibraryBookshelvesRemoveVolumeOptions = Record
    reason : String;
    source : String;
    volumeId : String;
  end;
  
  TMylibraryBookshelvesResource = Class(TGoogleResource)
  Private
    FVolumesInstance : TMylibraryBookshelvesVolumesResource;
    Function GetVolumesInstance : TMylibraryBookshelvesVolumesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure AddVolume(shelf: string; AQuery : string  = '');
    Procedure AddVolume(shelf: string; AQuery : TMylibraryBookshelvesaddVolumeOptions);
    Procedure ClearVolumes(shelf: string; AQuery : string  = '');
    Procedure ClearVolumes(shelf: string; AQuery : TMylibraryBookshelvesclearVolumesOptions);
    Function Get(shelf: string; AQuery : string  = '') : TBookshelf;
    Function Get(shelf: string; AQuery : TMylibraryBookshelvesgetOptions) : TBookshelf;
    Function List(AQuery : string  = '') : TBookshelves;
    Function List(AQuery : TMylibraryBookshelveslistOptions) : TBookshelves;
    Procedure MoveVolume(shelf: string; AQuery : string  = '');
    Procedure MoveVolume(shelf: string; AQuery : TMylibraryBookshelvesmoveVolumeOptions);
    Procedure RemoveVolume(shelf: string; AQuery : string  = '');
    Procedure RemoveVolume(shelf: string; AQuery : TMylibraryBookshelvesremoveVolumeOptions);
    Function CreateVolumesResource(AOwner : TComponent) : TMylibraryBookshelvesVolumesResource;virtual;overload;
    Function CreateVolumesResource : TMylibraryBookshelvesVolumesResource;virtual;overload;
    Property VolumesResource : TMylibraryBookshelvesVolumesResource Read GetVolumesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TMylibraryReadingpositionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMylibraryReadingpositionsResource, method Get
  
  TMylibraryReadingpositionsGetOptions = Record
    contentVersion : String;
    source : String;
  end;
  
  
  //Optional query Options for TMylibraryReadingpositionsResource, method SetPosition
  
  TMylibraryReadingpositionsSetPositionOptions = Record
    action : String;
    contentVersion : String;
    deviceCookie : String;
    position : String;
    source : String;
    timestamp : String;
  end;
  
  TMylibraryReadingpositionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(volumeId: string; AQuery : string  = '') : TReadingPosition;
    Function Get(volumeId: string; AQuery : TMylibraryReadingpositionsgetOptions) : TReadingPosition;
    Procedure SetPosition(volumeId: string; AQuery : string  = '');
    Procedure SetPosition(volumeId: string; AQuery : TMylibraryReadingpositionssetPositionOptions);
  end;
  
  
  { --------------------------------------------------------------------
    TMylibraryResource
    --------------------------------------------------------------------}
  
  TMylibraryResource = Class(TGoogleResource)
  Private
    FAnnotationsInstance : TMylibraryAnnotationsResource;
    FBookshelvesVolumesInstance : TMylibraryBookshelvesVolumesResource;
    FBookshelvesInstance : TMylibraryBookshelvesResource;
    FReadingpositionsInstance : TMylibraryReadingpositionsResource;
    Function GetAnnotationsInstance : TMylibraryAnnotationsResource;virtual;
    Function GetBookshelvesVolumesInstance : TMylibraryBookshelvesVolumesResource;virtual;
    Function GetBookshelvesInstance : TMylibraryBookshelvesResource;virtual;
    Function GetReadingpositionsInstance : TMylibraryReadingpositionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateAnnotationsResource(AOwner : TComponent) : TMylibraryAnnotationsResource;virtual;overload;
    Function CreateAnnotationsResource : TMylibraryAnnotationsResource;virtual;overload;
    Function CreateBookshelvesVolumesResource(AOwner : TComponent) : TMylibraryBookshelvesVolumesResource;virtual;overload;
    Function CreateBookshelvesVolumesResource : TMylibraryBookshelvesVolumesResource;virtual;overload;
    Function CreateBookshelvesResource(AOwner : TComponent) : TMylibraryBookshelvesResource;virtual;overload;
    Function CreateBookshelvesResource : TMylibraryBookshelvesResource;virtual;overload;
    Function CreateReadingpositionsResource(AOwner : TComponent) : TMylibraryReadingpositionsResource;virtual;overload;
    Function CreateReadingpositionsResource : TMylibraryReadingpositionsResource;virtual;overload;
    Property AnnotationsResource : TMylibraryAnnotationsResource Read GetAnnotationsInstance;
    Property BookshelvesVolumesResource : TMylibraryBookshelvesVolumesResource Read GetBookshelvesVolumesInstance;
    Property BookshelvesResource : TMylibraryBookshelvesResource Read GetBookshelvesInstance;
    Property ReadingpositionsResource : TMylibraryReadingpositionsResource Read GetReadingpositionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TNotificationResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TNotificationResource, method Get
  
  TNotificationGetOptions = Record
    locale : String;
    notification_id : String;
    source : String;
  end;
  
  TNotificationResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TNotification;
    Function Get(AQuery : TNotificationgetOptions) : TNotification;
  end;
  
  
  { --------------------------------------------------------------------
    TOnboardingResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOnboardingResource, method ListCategories
  
  TOnboardingListCategoriesOptions = Record
    locale : String;
  end;
  
  
  //Optional query Options for TOnboardingResource, method ListCategoryVolumes
  
  TOnboardingListCategoryVolumesOptions = Record
    categoryId : String;
    locale : String;
    maxAllowedMaturityRating : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TOnboardingResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function ListCategories(AQuery : string  = '') : TCategory;
    Function ListCategories(AQuery : TOnboardinglistCategoriesOptions) : TCategory;
    Function ListCategoryVolumes(AQuery : string  = '') : TVolume2;
    Function ListCategoryVolumes(AQuery : TOnboardinglistCategoryVolumesOptions) : TVolume2;
  end;
  
  
  { --------------------------------------------------------------------
    TPersonalizedstreamResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPersonalizedstreamResource, method Get
  
  TPersonalizedstreamGetOptions = Record
    locale : String;
    maxAllowedMaturityRating : String;
    source : String;
  end;
  
  TPersonalizedstreamResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TDiscoveryclusters;
    Function Get(AQuery : TPersonalizedstreamgetOptions) : TDiscoveryclusters;
  end;
  
  
  { --------------------------------------------------------------------
    TPromoofferResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPromoofferResource, method Accept
  
  TPromoofferAcceptOptions = Record
    androidId : String;
    device : String;
    manufacturer : String;
    model : String;
    offerId : String;
    product : String;
    serial : String;
    volumeId : String;
  end;
  
  
  //Optional query Options for TPromoofferResource, method Dismiss
  
  TPromoofferDismissOptions = Record
    androidId : String;
    device : String;
    manufacturer : String;
    model : String;
    offerId : String;
    product : String;
    serial : String;
  end;
  
  
  //Optional query Options for TPromoofferResource, method Get
  
  TPromoofferGetOptions = Record
    androidId : String;
    device : String;
    manufacturer : String;
    model : String;
    product : String;
    serial : String;
  end;
  
  TPromoofferResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Accept(AQuery : string  = '');
    Procedure Accept(AQuery : TPromoofferacceptOptions);
    Procedure Dismiss(AQuery : string  = '');
    Procedure Dismiss(AQuery : TPromoofferdismissOptions);
    Function Get(AQuery : string  = '') : TOffers;
    Function Get(AQuery : TPromooffergetOptions) : TOffers;
  end;
  
  
  { --------------------------------------------------------------------
    TSeriesMembershipResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSeriesMembershipResource, method Get
  
  TSeriesMembershipGetOptions = Record
    page_size : integer;
    page_token : String;
    series_id : String;
  end;
  
  TSeriesMembershipResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TSeriesmembership;
    Function Get(AQuery : TSeriesMembershipgetOptions) : TSeriesmembership;
  end;
  
  
  { --------------------------------------------------------------------
    TSeriesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSeriesResource, method Get
  
  TSeriesGetOptions = Record
    series_id : String;
  end;
  
  TSeriesResource = Class(TGoogleResource)
  Private
    FMembershipInstance : TSeriesMembershipResource;
    Function GetMembershipInstance : TSeriesMembershipResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TSeries;
    Function Get(AQuery : TSeriesgetOptions) : TSeries;
    Function CreateMembershipResource(AOwner : TComponent) : TSeriesMembershipResource;virtual;overload;
    Function CreateMembershipResource : TSeriesMembershipResource;virtual;overload;
    Property MembershipResource : TSeriesMembershipResource Read GetMembershipInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TVolumesAssociatedResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVolumesAssociatedResource, method List
  
  TVolumesAssociatedListOptions = Record
    association : String;
    locale : String;
    maxAllowedMaturityRating : String;
    source : String;
  end;
  
  TVolumesAssociatedResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(volumeId: string; AQuery : string  = '') : TVolumes;
    Function List(volumeId: string; AQuery : TVolumesAssociatedlistOptions) : TVolumes;
  end;
  
  
  { --------------------------------------------------------------------
    TVolumesMybooksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVolumesMybooksResource, method List
  
  TVolumesMybooksListOptions = Record
    acquireMethod : String;
    country : String;
    locale : String;
    maxResults : integer;
    processingState : String;
    source : String;
    startIndex : integer;
  end;
  
  TVolumesMybooksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TVolumes;
    Function List(AQuery : TVolumesMybookslistOptions) : TVolumes;
  end;
  
  
  { --------------------------------------------------------------------
    TVolumesRecommendedResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVolumesRecommendedResource, method List
  
  TVolumesRecommendedListOptions = Record
    locale : String;
    maxAllowedMaturityRating : String;
    source : String;
  end;
  
  
  //Optional query Options for TVolumesRecommendedResource, method Rate
  
  TVolumesRecommendedRateOptions = Record
    locale : String;
    rating : String;
    source : String;
    volumeId : String;
  end;
  
  TVolumesRecommendedResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TVolumes;
    Function List(AQuery : TVolumesRecommendedlistOptions) : TVolumes;
    Function Rate(AQuery : string  = '') : TBooksVolumesRecommendedRateResponse;
    Function Rate(AQuery : TVolumesRecommendedrateOptions) : TBooksVolumesRecommendedRateResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TVolumesUseruploadedResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVolumesUseruploadedResource, method List
  
  TVolumesUseruploadedListOptions = Record
    locale : String;
    maxResults : integer;
    processingState : String;
    source : String;
    startIndex : integer;
    volumeId : String;
  end;
  
  TVolumesUseruploadedResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TVolumes;
    Function List(AQuery : TVolumesUseruploadedlistOptions) : TVolumes;
  end;
  
  
  { --------------------------------------------------------------------
    TVolumesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVolumesResource, method Get
  
  TVolumesGetOptions = Record
    country : String;
    includeNonComicsSeries : boolean;
    partner : String;
    projection : String;
    source : String;
    user_library_consistent_read : boolean;
  end;
  
  
  //Optional query Options for TVolumesResource, method List
  
  TVolumesListOptions = Record
    download : String;
    filter : String;
    langRestrict : String;
    libraryRestrict : String;
    maxResults : integer;
    orderBy : String;
    partner : String;
    printType : String;
    projection : String;
    q : String;
    showPreorders : boolean;
    source : String;
    startIndex : integer;
  end;
  
  TVolumesResource = Class(TGoogleResource)
  Private
    FAssociatedInstance : TVolumesAssociatedResource;
    FMybooksInstance : TVolumesMybooksResource;
    FRecommendedInstance : TVolumesRecommendedResource;
    FUseruploadedInstance : TVolumesUseruploadedResource;
    Function GetAssociatedInstance : TVolumesAssociatedResource;virtual;
    Function GetMybooksInstance : TVolumesMybooksResource;virtual;
    Function GetRecommendedInstance : TVolumesRecommendedResource;virtual;
    Function GetUseruploadedInstance : TVolumesUseruploadedResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(volumeId: string; AQuery : string  = '') : TVolume;
    Function Get(volumeId: string; AQuery : TVolumesgetOptions) : TVolume;
    Function List(AQuery : string  = '') : TVolumes;
    Function List(AQuery : TVolumeslistOptions) : TVolumes;
    Function CreateAssociatedResource(AOwner : TComponent) : TVolumesAssociatedResource;virtual;overload;
    Function CreateAssociatedResource : TVolumesAssociatedResource;virtual;overload;
    Function CreateMybooksResource(AOwner : TComponent) : TVolumesMybooksResource;virtual;overload;
    Function CreateMybooksResource : TVolumesMybooksResource;virtual;overload;
    Function CreateRecommendedResource(AOwner : TComponent) : TVolumesRecommendedResource;virtual;overload;
    Function CreateRecommendedResource : TVolumesRecommendedResource;virtual;overload;
    Function CreateUseruploadedResource(AOwner : TComponent) : TVolumesUseruploadedResource;virtual;overload;
    Function CreateUseruploadedResource : TVolumesUseruploadedResource;virtual;overload;
    Property AssociatedResource : TVolumesAssociatedResource Read GetAssociatedInstance;
    Property MybooksResource : TVolumesMybooksResource Read GetMybooksInstance;
    Property RecommendedResource : TVolumesRecommendedResource Read GetRecommendedInstance;
    Property UseruploadedResource : TVolumesUseruploadedResource Read GetUseruploadedInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TBooksAPI
    --------------------------------------------------------------------}
  
  TBooksAPI = Class(TGoogleAPI)
  Private
    FBookshelvesVolumesInstance : TBookshelvesVolumesResource;
    FBookshelvesInstance : TBookshelvesResource;
    FCloudloadingInstance : TCloudloadingResource;
    FDictionaryInstance : TDictionaryResource;
    FLayersAnnotationDataInstance : TLayersAnnotationDataResource;
    FLayersVolumeAnnotationsInstance : TLayersVolumeAnnotationsResource;
    FLayersInstance : TLayersResource;
    FMyconfigInstance : TMyconfigResource;
    FMylibraryAnnotationsInstance : TMylibraryAnnotationsResource;
    FMylibraryBookshelvesVolumesInstance : TMylibraryBookshelvesVolumesResource;
    FMylibraryBookshelvesInstance : TMylibraryBookshelvesResource;
    FMylibraryReadingpositionsInstance : TMylibraryReadingpositionsResource;
    FMylibraryInstance : TMylibraryResource;
    FNotificationInstance : TNotificationResource;
    FOnboardingInstance : TOnboardingResource;
    FPersonalizedstreamInstance : TPersonalizedstreamResource;
    FPromoofferInstance : TPromoofferResource;
    FSeriesMembershipInstance : TSeriesMembershipResource;
    FSeriesInstance : TSeriesResource;
    FVolumesAssociatedInstance : TVolumesAssociatedResource;
    FVolumesMybooksInstance : TVolumesMybooksResource;
    FVolumesRecommendedInstance : TVolumesRecommendedResource;
    FVolumesUseruploadedInstance : TVolumesUseruploadedResource;
    FVolumesInstance : TVolumesResource;
    Function GetBookshelvesVolumesInstance : TBookshelvesVolumesResource;virtual;
    Function GetBookshelvesInstance : TBookshelvesResource;virtual;
    Function GetCloudloadingInstance : TCloudloadingResource;virtual;
    Function GetDictionaryInstance : TDictionaryResource;virtual;
    Function GetLayersAnnotationDataInstance : TLayersAnnotationDataResource;virtual;
    Function GetLayersVolumeAnnotationsInstance : TLayersVolumeAnnotationsResource;virtual;
    Function GetLayersInstance : TLayersResource;virtual;
    Function GetMyconfigInstance : TMyconfigResource;virtual;
    Function GetMylibraryAnnotationsInstance : TMylibraryAnnotationsResource;virtual;
    Function GetMylibraryBookshelvesVolumesInstance : TMylibraryBookshelvesVolumesResource;virtual;
    Function GetMylibraryBookshelvesInstance : TMylibraryBookshelvesResource;virtual;
    Function GetMylibraryReadingpositionsInstance : TMylibraryReadingpositionsResource;virtual;
    Function GetMylibraryInstance : TMylibraryResource;virtual;
    Function GetNotificationInstance : TNotificationResource;virtual;
    Function GetOnboardingInstance : TOnboardingResource;virtual;
    Function GetPersonalizedstreamInstance : TPersonalizedstreamResource;virtual;
    Function GetPromoofferInstance : TPromoofferResource;virtual;
    Function GetSeriesMembershipInstance : TSeriesMembershipResource;virtual;
    Function GetSeriesInstance : TSeriesResource;virtual;
    Function GetVolumesAssociatedInstance : TVolumesAssociatedResource;virtual;
    Function GetVolumesMybooksInstance : TVolumesMybooksResource;virtual;
    Function GetVolumesRecommendedInstance : TVolumesRecommendedResource;virtual;
    Function GetVolumesUseruploadedInstance : TVolumesUseruploadedResource;virtual;
    Function GetVolumesInstance : TVolumesResource;virtual;
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
    Function CreateBookshelvesVolumesResource(AOwner : TComponent) : TBookshelvesVolumesResource;virtual;overload;
    Function CreateBookshelvesVolumesResource : TBookshelvesVolumesResource;virtual;overload;
    Function CreateBookshelvesResource(AOwner : TComponent) : TBookshelvesResource;virtual;overload;
    Function CreateBookshelvesResource : TBookshelvesResource;virtual;overload;
    Function CreateCloudloadingResource(AOwner : TComponent) : TCloudloadingResource;virtual;overload;
    Function CreateCloudloadingResource : TCloudloadingResource;virtual;overload;
    Function CreateDictionaryResource(AOwner : TComponent) : TDictionaryResource;virtual;overload;
    Function CreateDictionaryResource : TDictionaryResource;virtual;overload;
    Function CreateLayersAnnotationDataResource(AOwner : TComponent) : TLayersAnnotationDataResource;virtual;overload;
    Function CreateLayersAnnotationDataResource : TLayersAnnotationDataResource;virtual;overload;
    Function CreateLayersVolumeAnnotationsResource(AOwner : TComponent) : TLayersVolumeAnnotationsResource;virtual;overload;
    Function CreateLayersVolumeAnnotationsResource : TLayersVolumeAnnotationsResource;virtual;overload;
    Function CreateLayersResource(AOwner : TComponent) : TLayersResource;virtual;overload;
    Function CreateLayersResource : TLayersResource;virtual;overload;
    Function CreateMyconfigResource(AOwner : TComponent) : TMyconfigResource;virtual;overload;
    Function CreateMyconfigResource : TMyconfigResource;virtual;overload;
    Function CreateMylibraryAnnotationsResource(AOwner : TComponent) : TMylibraryAnnotationsResource;virtual;overload;
    Function CreateMylibraryAnnotationsResource : TMylibraryAnnotationsResource;virtual;overload;
    Function CreateMylibraryBookshelvesVolumesResource(AOwner : TComponent) : TMylibraryBookshelvesVolumesResource;virtual;overload;
    Function CreateMylibraryBookshelvesVolumesResource : TMylibraryBookshelvesVolumesResource;virtual;overload;
    Function CreateMylibraryBookshelvesResource(AOwner : TComponent) : TMylibraryBookshelvesResource;virtual;overload;
    Function CreateMylibraryBookshelvesResource : TMylibraryBookshelvesResource;virtual;overload;
    Function CreateMylibraryReadingpositionsResource(AOwner : TComponent) : TMylibraryReadingpositionsResource;virtual;overload;
    Function CreateMylibraryReadingpositionsResource : TMylibraryReadingpositionsResource;virtual;overload;
    Function CreateMylibraryResource(AOwner : TComponent) : TMylibraryResource;virtual;overload;
    Function CreateMylibraryResource : TMylibraryResource;virtual;overload;
    Function CreateNotificationResource(AOwner : TComponent) : TNotificationResource;virtual;overload;
    Function CreateNotificationResource : TNotificationResource;virtual;overload;
    Function CreateOnboardingResource(AOwner : TComponent) : TOnboardingResource;virtual;overload;
    Function CreateOnboardingResource : TOnboardingResource;virtual;overload;
    Function CreatePersonalizedstreamResource(AOwner : TComponent) : TPersonalizedstreamResource;virtual;overload;
    Function CreatePersonalizedstreamResource : TPersonalizedstreamResource;virtual;overload;
    Function CreatePromoofferResource(AOwner : TComponent) : TPromoofferResource;virtual;overload;
    Function CreatePromoofferResource : TPromoofferResource;virtual;overload;
    Function CreateSeriesMembershipResource(AOwner : TComponent) : TSeriesMembershipResource;virtual;overload;
    Function CreateSeriesMembershipResource : TSeriesMembershipResource;virtual;overload;
    Function CreateSeriesResource(AOwner : TComponent) : TSeriesResource;virtual;overload;
    Function CreateSeriesResource : TSeriesResource;virtual;overload;
    Function CreateVolumesAssociatedResource(AOwner : TComponent) : TVolumesAssociatedResource;virtual;overload;
    Function CreateVolumesAssociatedResource : TVolumesAssociatedResource;virtual;overload;
    Function CreateVolumesMybooksResource(AOwner : TComponent) : TVolumesMybooksResource;virtual;overload;
    Function CreateVolumesMybooksResource : TVolumesMybooksResource;virtual;overload;
    Function CreateVolumesRecommendedResource(AOwner : TComponent) : TVolumesRecommendedResource;virtual;overload;
    Function CreateVolumesRecommendedResource : TVolumesRecommendedResource;virtual;overload;
    Function CreateVolumesUseruploadedResource(AOwner : TComponent) : TVolumesUseruploadedResource;virtual;overload;
    Function CreateVolumesUseruploadedResource : TVolumesUseruploadedResource;virtual;overload;
    Function CreateVolumesResource(AOwner : TComponent) : TVolumesResource;virtual;overload;
    Function CreateVolumesResource : TVolumesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property BookshelvesVolumesResource : TBookshelvesVolumesResource Read GetBookshelvesVolumesInstance;
    Property BookshelvesResource : TBookshelvesResource Read GetBookshelvesInstance;
    Property CloudloadingResource : TCloudloadingResource Read GetCloudloadingInstance;
    Property DictionaryResource : TDictionaryResource Read GetDictionaryInstance;
    Property LayersAnnotationDataResource : TLayersAnnotationDataResource Read GetLayersAnnotationDataInstance;
    Property LayersVolumeAnnotationsResource : TLayersVolumeAnnotationsResource Read GetLayersVolumeAnnotationsInstance;
    Property LayersResource : TLayersResource Read GetLayersInstance;
    Property MyconfigResource : TMyconfigResource Read GetMyconfigInstance;
    Property MylibraryAnnotationsResource : TMylibraryAnnotationsResource Read GetMylibraryAnnotationsInstance;
    Property MylibraryBookshelvesVolumesResource : TMylibraryBookshelvesVolumesResource Read GetMylibraryBookshelvesVolumesInstance;
    Property MylibraryBookshelvesResource : TMylibraryBookshelvesResource Read GetMylibraryBookshelvesInstance;
    Property MylibraryReadingpositionsResource : TMylibraryReadingpositionsResource Read GetMylibraryReadingpositionsInstance;
    Property MylibraryResource : TMylibraryResource Read GetMylibraryInstance;
    Property NotificationResource : TNotificationResource Read GetNotificationInstance;
    Property OnboardingResource : TOnboardingResource Read GetOnboardingInstance;
    Property PersonalizedstreamResource : TPersonalizedstreamResource Read GetPersonalizedstreamInstance;
    Property PromoofferResource : TPromoofferResource Read GetPromoofferInstance;
    Property SeriesMembershipResource : TSeriesMembershipResource Read GetSeriesMembershipInstance;
    Property SeriesResource : TSeriesResource Read GetSeriesInstance;
    Property VolumesAssociatedResource : TVolumesAssociatedResource Read GetVolumesAssociatedInstance;
    Property VolumesMybooksResource : TVolumesMybooksResource Read GetVolumesMybooksInstance;
    Property VolumesRecommendedResource : TVolumesRecommendedResource Read GetVolumesRecommendedInstance;
    Property VolumesUseruploadedResource : TVolumesUseruploadedResource Read GetVolumesUseruploadedInstance;
    Property VolumesResource : TVolumesResource Read GetVolumesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAnnotationTypeclientVersionRanges
  --------------------------------------------------------------------}


Procedure TAnnotationTypeclientVersionRanges.SetcfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FcfiRange=AValue) then exit;
  FcfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypeclientVersionRanges.SetcontentVersion(AIndex : Integer; const AValue : String); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypeclientVersionRanges.SetgbImageRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FgbImageRange=AValue) then exit;
  FgbImageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypeclientVersionRanges.SetgbTextRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FgbTextRange=AValue) then exit;
  FgbTextRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypeclientVersionRanges.SetimageCfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FimageCfiRange=AValue) then exit;
  FimageCfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationTypecurrentVersionRanges
  --------------------------------------------------------------------}


Procedure TAnnotationTypecurrentVersionRanges.SetcfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FcfiRange=AValue) then exit;
  FcfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypecurrentVersionRanges.SetcontentVersion(AIndex : Integer; const AValue : String); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypecurrentVersionRanges.SetgbImageRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FgbImageRange=AValue) then exit;
  FgbImageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypecurrentVersionRanges.SetgbTextRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FgbTextRange=AValue) then exit;
  FgbTextRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypecurrentVersionRanges.SetimageCfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FimageCfiRange=AValue) then exit;
  FimageCfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationTypelayerSummary
  --------------------------------------------------------------------}


Procedure TAnnotationTypelayerSummary.SetallowedCharacterCount(AIndex : Integer; const AValue : integer); 

begin
  If (FallowedCharacterCount=AValue) then exit;
  FallowedCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypelayerSummary.SetlimitType(AIndex : Integer; const AValue : String); 

begin
  If (FlimitType=AValue) then exit;
  FlimitType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationTypelayerSummary.SetremainingCharacterCount(AIndex : Integer; const AValue : integer); 

begin
  If (FremainingCharacterCount=AValue) then exit;
  FremainingCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotation
  --------------------------------------------------------------------}


Procedure TAnnotation.SetafterSelectedText(AIndex : Integer; const AValue : String); 

begin
  If (FafterSelectedText=AValue) then exit;
  FafterSelectedText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetbeforeSelectedText(AIndex : Integer; const AValue : String); 

begin
  If (FbeforeSelectedText=AValue) then exit;
  FbeforeSelectedText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetclientVersionRanges(AIndex : Integer; const AValue : TAnnotationTypeclientVersionRanges); 

begin
  If (FclientVersionRanges=AValue) then exit;
  FclientVersionRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setcreated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetcurrentVersionRanges(AIndex : Integer; const AValue : TAnnotationTypecurrentVersionRanges); 

begin
  If (FcurrentVersionRanges=AValue) then exit;
  FcurrentVersionRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setdata(AIndex : Integer; const AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setdeleted(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SethighlightStyle(AIndex : Integer; const AValue : String); 

begin
  If (FhighlightStyle=AValue) then exit;
  FhighlightStyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetlayerId(AIndex : Integer; const AValue : String); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetlayerSummary(AIndex : Integer; const AValue : TAnnotationTypelayerSummary); 

begin
  If (FlayerSummary=AValue) then exit;
  FlayerSummary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetpageIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FpageIds=AValue) then exit;
  FpageIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetselectedText(AIndex : Integer; const AValue : String); 

begin
  If (FselectedText=AValue) then exit;
  FselectedText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnnotation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'pageids' : SetLength(FpageIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnnotationdata
  --------------------------------------------------------------------}


Procedure TAnnotationdata.SetannotationType(AIndex : Integer; const AValue : String); 

begin
  If (FannotationType=AValue) then exit;
  FannotationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setdata(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setencoded_data(AIndex : Integer; const AValue : String); 

begin
  If (Fencoded_data=AValue) then exit;
  Fencoded_data:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.SetlayerId(AIndex : Integer; const AValue : String); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotations
  --------------------------------------------------------------------}


Procedure TAnnotations.Setitems(AIndex : Integer; const AValue : TAnnotationsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotations.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotations.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotations.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnnotations.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnnotationsSummaryTypelayersItem
  --------------------------------------------------------------------}


Procedure TAnnotationsSummaryTypelayersItem.SetallowedCharacterCount(AIndex : Integer; const AValue : integer); 

begin
  If (FallowedCharacterCount=AValue) then exit;
  FallowedCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummaryTypelayersItem.SetlayerId(AIndex : Integer; const AValue : String); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummaryTypelayersItem.SetlimitType(AIndex : Integer; const AValue : String); 

begin
  If (FlimitType=AValue) then exit;
  FlimitType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummaryTypelayersItem.SetremainingCharacterCount(AIndex : Integer; const AValue : integer); 

begin
  If (FremainingCharacterCount=AValue) then exit;
  FremainingCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummaryTypelayersItem.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationsSummary
  --------------------------------------------------------------------}


Procedure TAnnotationsSummary.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummary.Setlayers(AIndex : Integer; const AValue : TAnnotationsSummaryTypelayersArray); 

begin
  If (Flayers=AValue) then exit;
  Flayers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnnotationsSummary.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'layers' : SetLength(Flayers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnnotationsdata
  --------------------------------------------------------------------}


Procedure TAnnotationsdata.Setitems(AIndex : Integer; const AValue : TAnnotationsdataTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsdata.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsdata.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsdata.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnnotationsdata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBooksAnnotationsRange
  --------------------------------------------------------------------}


Procedure TBooksAnnotationsRange.SetendOffset(AIndex : Integer; const AValue : String); 

begin
  If (FendOffset=AValue) then exit;
  FendOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksAnnotationsRange.SetendPosition(AIndex : Integer; const AValue : String); 

begin
  If (FendPosition=AValue) then exit;
  FendPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksAnnotationsRange.SetstartOffset(AIndex : Integer; const AValue : String); 

begin
  If (FstartOffset=AValue) then exit;
  FstartOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksAnnotationsRange.SetstartPosition(AIndex : Integer; const AValue : String); 

begin
  If (FstartPosition=AValue) then exit;
  FstartPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBooksCloudloadingResource
  --------------------------------------------------------------------}


Procedure TBooksCloudloadingResource.Setauthor(AIndex : Integer; const AValue : String); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksCloudloadingResource.SetprocessingState(AIndex : Integer; const AValue : String); 

begin
  If (FprocessingState=AValue) then exit;
  FprocessingState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksCloudloadingResource.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksCloudloadingResource.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBooksVolumesRecommendedRateResponse
  --------------------------------------------------------------------}


Procedure TBooksVolumesRecommendedRateResponse.Setconsistency_token(AIndex : Integer; const AValue : String); 

begin
  If (Fconsistency_token=AValue) then exit;
  Fconsistency_token:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBookshelf
  --------------------------------------------------------------------}


Procedure TBookshelf.Setaccess(AIndex : Integer; const AValue : String); 

begin
  If (Faccess=AValue) then exit;
  Faccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setcreated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setid(AIndex : Integer; const AValue : integer); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.SetvolumeCount(AIndex : Integer; const AValue : integer); 

begin
  If (FvolumeCount=AValue) then exit;
  FvolumeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.SetvolumesLastUpdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FvolumesLastUpdated=AValue) then exit;
  FvolumesLastUpdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBookshelves
  --------------------------------------------------------------------}


Procedure TBookshelves.Setitems(AIndex : Integer; const AValue : TBookshelvesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelves.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBookshelves.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCategoryTypeitemsItem
  --------------------------------------------------------------------}


Procedure TCategoryTypeitemsItem.SetbadgeUrl(AIndex : Integer; const AValue : String); 

begin
  If (FbadgeUrl=AValue) then exit;
  FbadgeUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategoryTypeitemsItem.SetcategoryId(AIndex : Integer; const AValue : String); 

begin
  If (FcategoryId=AValue) then exit;
  FcategoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategoryTypeitemsItem.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCategory
  --------------------------------------------------------------------}


Procedure TCategory.Setitems(AIndex : Integer; const AValue : TCategoryTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategory.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCategory.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TConcurrentAccessRestriction
  --------------------------------------------------------------------}


Procedure TConcurrentAccessRestriction.SetdeviceAllowed(AIndex : Integer; const AValue : boolean); 

begin
  If (FdeviceAllowed=AValue) then exit;
  FdeviceAllowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.SetmaxConcurrentDevices(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxConcurrentDevices=AValue) then exit;
  FmaxConcurrentDevices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setnonce(AIndex : Integer; const AValue : String); 

begin
  If (Fnonce=AValue) then exit;
  Fnonce:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.SetreasonCode(AIndex : Integer; const AValue : String); 

begin
  If (FreasonCode=AValue) then exit;
  FreasonCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setrestricted(AIndex : Integer; const AValue : boolean); 

begin
  If (Frestricted=AValue) then exit;
  Frestricted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setsignature(AIndex : Integer; const AValue : String); 

begin
  If (Fsignature=AValue) then exit;
  Fsignature:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setsource(AIndex : Integer; const AValue : String); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.SettimeWindowSeconds(AIndex : Integer; const AValue : integer); 

begin
  If (FtimeWindowSeconds=AValue) then exit;
  FtimeWindowSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypecommon
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypecommon.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypesource
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypesource.Setattribution(AIndex : Integer; const AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypesource.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource.Setattribution(AIndex : Integer; const AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypederivativesItem
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypederivativesItem.Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypederivativesItem.Settext(AIndex : Integer; const AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource.Setattribution(AIndex : Integer; const AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypeexamplesItem
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypeexamplesItem.Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypeexamplesItem.Settext(AIndex : Integer; const AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource.Setattribution(AIndex : Integer; const AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem.Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem.Settext(AIndex : Integer; const AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem.Setdefinition(AIndex : Integer; const AValue : String); 

begin
  If (Fdefinition=AValue) then exit;
  Fdefinition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem.Setexamples(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesArray); 

begin
  If (Fexamples=AValue) then exit;
  Fexamples:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'examples' : SetLength(Fexamples,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource.Setattribution(AIndex : Integer; const AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource.Setattribution(AIndex : Integer; const AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem.Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem.Settext(AIndex : Integer; const AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesensesItem
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.Setconjugations(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsArray); 

begin
  If (Fconjugations=AValue) then exit;
  Fconjugations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.Setdefinitions(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsArray); 

begin
  If (Fdefinitions=AValue) then exit;
  Fdefinitions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.SetpartOfSpeech(AIndex : Integer; const AValue : String); 

begin
  If (FpartOfSpeech=AValue) then exit;
  FpartOfSpeech:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.Setpronunciation(AIndex : Integer; const AValue : String); 

begin
  If (Fpronunciation=AValue) then exit;
  Fpronunciation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.SetpronunciationUrl(AIndex : Integer; const AValue : String); 

begin
  If (FpronunciationUrl=AValue) then exit;
  FpronunciationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.Setsyllabification(AIndex : Integer; const AValue : String); 

begin
  If (Fsyllabification=AValue) then exit;
  Fsyllabification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.Setsynonyms(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsArray); 

begin
  If (Fsynonyms=AValue) then exit;
  Fsynonyms:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDictlayerdataTypedictTypewordsItemTypesensesItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'conjugations' : SetLength(Fconjugations,ALength);
  'definitions' : SetLength(Fdefinitions,ALength);
  'synonyms' : SetLength(Fsynonyms,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItemTypesource
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItemTypesource.Setattribution(AIndex : Integer; const AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItemTypesource.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdataTypedictTypewordsItem
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedictTypewordsItem.Setderivatives(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypederivativesArray); 

begin
  If (Fderivatives=AValue) then exit;
  Fderivatives:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItem.Setexamples(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypeexamplesArray); 

begin
  If (Fexamples=AValue) then exit;
  Fexamples:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItem.Setsenses(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesensesArray); 

begin
  If (Fsenses=AValue) then exit;
  Fsenses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedictTypewordsItem.Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsItemTypesource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDictlayerdataTypedictTypewordsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'derivatives' : SetLength(Fderivatives,ALength);
  'examples' : SetLength(Fexamples,ALength);
  'senses' : SetLength(Fsenses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDictlayerdataTypedict
  --------------------------------------------------------------------}


Procedure TDictlayerdataTypedict.Setsource(AIndex : Integer; const AValue : TDictlayerdataTypedictTypesource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdataTypedict.Setwords(AIndex : Integer; const AValue : TDictlayerdataTypedictTypewordsArray); 

begin
  If (Fwords=AValue) then exit;
  Fwords:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDictlayerdataTypedict.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'words' : SetLength(Fwords,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDictlayerdata
  --------------------------------------------------------------------}


Procedure TDictlayerdata.Setcommon(AIndex : Integer; const AValue : TDictlayerdataTypecommon); 

begin
  If (Fcommon=AValue) then exit;
  Fcommon:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdata.Setdict(AIndex : Integer; const AValue : TDictlayerdataTypedict); 

begin
  If (Fdict=AValue) then exit;
  Fdict:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdata.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiscoveryclustersTypeclustersItemTypebanner_with_content_container
  --------------------------------------------------------------------}


Procedure TDiscoveryclustersTypeclustersItemTypebanner_with_content_container.SetfillColorArgb(AIndex : Integer; const AValue : String); 

begin
  If (FfillColorArgb=AValue) then exit;
  FfillColorArgb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItemTypebanner_with_content_container.SetimageUrl(AIndex : Integer; const AValue : String); 

begin
  If (FimageUrl=AValue) then exit;
  FimageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItemTypebanner_with_content_container.SetmaskColorArgb(AIndex : Integer; const AValue : String); 

begin
  If (FmaskColorArgb=AValue) then exit;
  FmaskColorArgb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItemTypebanner_with_content_container.SetmoreButtonText(AIndex : Integer; const AValue : String); 

begin
  If (FmoreButtonText=AValue) then exit;
  FmoreButtonText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItemTypebanner_with_content_container.SetmoreButtonUrl(AIndex : Integer; const AValue : String); 

begin
  If (FmoreButtonUrl=AValue) then exit;
  FmoreButtonUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItemTypebanner_with_content_container.SettextColorArgb(AIndex : Integer; const AValue : String); 

begin
  If (FtextColorArgb=AValue) then exit;
  FtextColorArgb:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiscoveryclustersTypeclustersItem
  --------------------------------------------------------------------}


Procedure TDiscoveryclustersTypeclustersItem.Setbanner_with_content_container(AIndex : Integer; const AValue : TDiscoveryclustersTypeclustersItemTypebanner_with_content_container); 

begin
  If (Fbanner_with_content_container=AValue) then exit;
  Fbanner_with_content_container:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItem.SetsubTitle(AIndex : Integer; const AValue : String); 

begin
  If (FsubTitle=AValue) then exit;
  FsubTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItem.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItem.SettotalVolumes(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalVolumes=AValue) then exit;
  FtotalVolumes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItem.Setuid(AIndex : Integer; const AValue : String); 

begin
  If (Fuid=AValue) then exit;
  Fuid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclustersTypeclustersItem.Setvolumes(AIndex : Integer; const AValue : TDiscoveryclustersTypeclustersItemTypevolumesArray); 

begin
  If (Fvolumes=AValue) then exit;
  Fvolumes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDiscoveryclustersTypeclustersItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'volumes' : SetLength(Fvolumes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDiscoveryclusters
  --------------------------------------------------------------------}


Procedure TDiscoveryclusters.Setclusters(AIndex : Integer; const AValue : TDiscoveryclustersTypeclustersArray); 

begin
  If (Fclusters=AValue) then exit;
  Fclusters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclusters.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiscoveryclusters.SettotalClusters(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalClusters=AValue) then exit;
  FtotalClusters:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDiscoveryclusters.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'clusters' : SetLength(Fclusters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDownloadAccessRestriction
  --------------------------------------------------------------------}


Procedure TDownloadAccessRestriction.SetdeviceAllowed(AIndex : Integer; const AValue : boolean); 

begin
  If (FdeviceAllowed=AValue) then exit;
  FdeviceAllowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetdownloadsAcquired(AIndex : Integer; const AValue : integer); 

begin
  If (FdownloadsAcquired=AValue) then exit;
  FdownloadsAcquired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetjustAcquired(AIndex : Integer; const AValue : boolean); 

begin
  If (FjustAcquired=AValue) then exit;
  FjustAcquired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetmaxDownloadDevices(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxDownloadDevices=AValue) then exit;
  FmaxDownloadDevices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setnonce(AIndex : Integer; const AValue : String); 

begin
  If (Fnonce=AValue) then exit;
  Fnonce:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetreasonCode(AIndex : Integer; const AValue : String); 

begin
  If (FreasonCode=AValue) then exit;
  FreasonCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setrestricted(AIndex : Integer; const AValue : boolean); 

begin
  If (Frestricted=AValue) then exit;
  Frestricted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setsignature(AIndex : Integer; const AValue : String); 

begin
  If (Fsignature=AValue) then exit;
  Fsignature:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setsource(AIndex : Integer; const AValue : String); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadAccesses
  --------------------------------------------------------------------}


Procedure TDownloadAccesses.SetdownloadAccessList(AIndex : Integer; const AValue : TDownloadAccessesTypedownloadAccessListArray); 

begin
  If (FdownloadAccessList=AValue) then exit;
  FdownloadAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccesses.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDownloadAccesses.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'downloadaccesslist' : SetLength(FdownloadAccessList,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGeolayerdataTypecommon
  --------------------------------------------------------------------}


Procedure TGeolayerdataTypecommon.Setlang(AIndex : Integer; const AValue : String); 

begin
  If (Flang=AValue) then exit;
  Flang:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypecommon.SetpreviewImageUrl(AIndex : Integer; const AValue : String); 

begin
  If (FpreviewImageUrl=AValue) then exit;
  FpreviewImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypecommon.Setsnippet(AIndex : Integer; const AValue : String); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypecommon.SetsnippetUrl(AIndex : Integer; const AValue : String); 

begin
  If (FsnippetUrl=AValue) then exit;
  FsnippetUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypecommon.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdataTypegeoTypeboundaryItemItem
  --------------------------------------------------------------------}


Procedure TGeolayerdataTypegeoTypeboundaryItemItem.Setlatitude(AIndex : Integer; const AValue : integer); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeoTypeboundaryItemItem.Setlongitude(AIndex : Integer; const AValue : integer); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdataTypegeoTypeviewportTypehi
  --------------------------------------------------------------------}


Procedure TGeolayerdataTypegeoTypeviewportTypehi.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeoTypeviewportTypehi.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdataTypegeoTypeviewportTypelo
  --------------------------------------------------------------------}


Procedure TGeolayerdataTypegeoTypeviewportTypelo.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeoTypeviewportTypelo.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdataTypegeoTypeviewport
  --------------------------------------------------------------------}


Procedure TGeolayerdataTypegeoTypeviewport.Sethi(AIndex : Integer; const AValue : TGeolayerdataTypegeoTypeviewportTypehi); 

begin
  If (Fhi=AValue) then exit;
  Fhi:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeoTypeviewport.Setlo(AIndex : Integer; const AValue : TGeolayerdataTypegeoTypeviewportTypelo); 

begin
  If (Flo=AValue) then exit;
  Flo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdataTypegeo
  --------------------------------------------------------------------}


Procedure TGeolayerdataTypegeo.Setboundary(AIndex : Integer; const AValue : TGeolayerdataTypegeoTypeboundaryArray); 

begin
  If (Fboundary=AValue) then exit;
  Fboundary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeo.SetcachePolicy(AIndex : Integer; const AValue : String); 

begin
  If (FcachePolicy=AValue) then exit;
  FcachePolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeo.SetcountryCode(AIndex : Integer; const AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeo.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeo.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeo.SetmapType(AIndex : Integer; const AValue : String); 

begin
  If (FmapType=AValue) then exit;
  FmapType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeo.Setviewport(AIndex : Integer; const AValue : TGeolayerdataTypegeoTypeviewport); 

begin
  If (Fviewport=AValue) then exit;
  Fviewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdataTypegeo.Setzoom(AIndex : Integer; const AValue : integer); 

begin
  If (Fzoom=AValue) then exit;
  Fzoom:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGeolayerdataTypegeo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'boundary' : SetLength(Fboundary,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGeolayerdata
  --------------------------------------------------------------------}


Procedure TGeolayerdata.Setcommon(AIndex : Integer; const AValue : TGeolayerdataTypecommon); 

begin
  If (Fcommon=AValue) then exit;
  Fcommon:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdata.Setgeo(AIndex : Integer; const AValue : TGeolayerdataTypegeo); 

begin
  If (Fgeo=AValue) then exit;
  Fgeo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdata.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLayersummaries
  --------------------------------------------------------------------}


Procedure TLayersummaries.Setitems(AIndex : Integer; const AValue : TLayersummariesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummaries.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummaries.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLayersummaries.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLayersummary
  --------------------------------------------------------------------}


Procedure TLayersummary.SetannotationCount(AIndex : Integer; const AValue : integer); 

begin
  If (FannotationCount=AValue) then exit;
  FannotationCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetannotationTypes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FannotationTypes=AValue) then exit;
  FannotationTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetannotationsDataLink(AIndex : Integer; const AValue : String); 

begin
  If (FannotationsDataLink=AValue) then exit;
  FannotationsDataLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetannotationsLink(AIndex : Integer; const AValue : String); 

begin
  If (FannotationsLink=AValue) then exit;
  FannotationsLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetcontentVersion(AIndex : Integer; const AValue : String); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetdataCount(AIndex : Integer; const AValue : integer); 

begin
  If (FdataCount=AValue) then exit;
  FdataCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetlayerId(AIndex : Integer; const AValue : String); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetvolumeAnnotationsVersion(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeAnnotationsVersion=AValue) then exit;
  FvolumeAnnotationsVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLayersummary.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'annotationtypes' : SetLength(FannotationTypes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetadataTypeitemsItem
  --------------------------------------------------------------------}


Procedure TMetadataTypeitemsItem.Setdownload_url(AIndex : Integer; const AValue : String); 

begin
  If (Fdownload_url=AValue) then exit;
  Fdownload_url:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataTypeitemsItem.Setencrypted_key(AIndex : Integer; const AValue : String); 

begin
  If (Fencrypted_key=AValue) then exit;
  Fencrypted_key:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataTypeitemsItem.Setlanguage(AIndex : Integer; const AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataTypeitemsItem.Setsize(AIndex : Integer; const AValue : String); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataTypeitemsItem.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.Setitems(AIndex : Integer; const AValue : TMetadataTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TNotification
  --------------------------------------------------------------------}


Procedure TNotification.Setbody(AIndex : Integer; const AValue : String); 

begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetcrmExperimentIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcrmExperimentIds=AValue) then exit;
  FcrmExperimentIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setdoc_id(AIndex : Integer; const AValue : String); 

begin
  If (Fdoc_id=AValue) then exit;
  Fdoc_id:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setdoc_type(AIndex : Integer; const AValue : String); 

begin
  If (Fdoc_type=AValue) then exit;
  Fdoc_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setdont_show_notification(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdont_show_notification=AValue) then exit;
  Fdont_show_notification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SeticonUrl(AIndex : Integer; const AValue : String); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setnotification_type(AIndex : Integer; const AValue : String); 

begin
  If (Fnotification_type=AValue) then exit;
  Fnotification_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setpcampaign_id(AIndex : Integer; const AValue : String); 

begin
  If (Fpcampaign_id=AValue) then exit;
  Fpcampaign_id:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setreason(AIndex : Integer; const AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setshow_notification_settings_action(AIndex : Integer; const AValue : boolean); 

begin
  If (Fshow_notification_settings_action=AValue) then exit;
  Fshow_notification_settings_action:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SettargetUrl(AIndex : Integer; const AValue : String); 

begin
  If (FtargetUrl=AValue) then exit;
  FtargetUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TNotification.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'crmexperimentids' : SetLength(FcrmExperimentIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOffersTypeitemsItemTypeitemsItem
  --------------------------------------------------------------------}


Procedure TOffersTypeitemsItemTypeitemsItem.Setauthor(AIndex : Integer; const AValue : String); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersTypeitemsItemTypeitemsItem.SetcanonicalVolumeLink(AIndex : Integer; const AValue : String); 

begin
  If (FcanonicalVolumeLink=AValue) then exit;
  FcanonicalVolumeLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersTypeitemsItemTypeitemsItem.SetcoverUrl(AIndex : Integer; const AValue : String); 

begin
  If (FcoverUrl=AValue) then exit;
  FcoverUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersTypeitemsItemTypeitemsItem.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersTypeitemsItemTypeitemsItem.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersTypeitemsItemTypeitemsItem.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOffersTypeitemsItem
  --------------------------------------------------------------------}


Procedure TOffersTypeitemsItem.SetartUrl(AIndex : Integer; const AValue : String); 

begin
  If (FartUrl=AValue) then exit;
  FartUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersTypeitemsItem.SetgservicesKey(AIndex : Integer; const AValue : String); 

begin
  If (FgservicesKey=AValue) then exit;
  FgservicesKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersTypeitemsItem.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersTypeitemsItem.Setitems(AIndex : Integer; const AValue : TOffersTypeitemsItemTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOffersTypeitemsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOffers
  --------------------------------------------------------------------}


Procedure TOffers.Setitems(AIndex : Integer; const AValue : TOffersTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffers.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOffers.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReadingPosition
  --------------------------------------------------------------------}


Procedure TReadingPosition.SetepubCfiPosition(AIndex : Integer; const AValue : String); 

begin
  If (FepubCfiPosition=AValue) then exit;
  FepubCfiPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.SetgbImagePosition(AIndex : Integer; const AValue : String); 

begin
  If (FgbImagePosition=AValue) then exit;
  FgbImagePosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.SetgbTextPosition(AIndex : Integer; const AValue : String); 

begin
  If (FgbTextPosition=AValue) then exit;
  FgbTextPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.SetpdfPosition(AIndex : Integer; const AValue : String); 

begin
  If (FpdfPosition=AValue) then exit;
  FpdfPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRequestAccess
  --------------------------------------------------------------------}


Procedure TRequestAccess.SetconcurrentAccess(AIndex : Integer; const AValue : TConcurrentAccessRestriction); 

begin
  If (FconcurrentAccess=AValue) then exit;
  FconcurrentAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestAccess.SetdownloadAccess(AIndex : Integer; const AValue : TDownloadAccessRestriction); 

begin
  If (FdownloadAccess=AValue) then exit;
  FdownloadAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestAccess.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReviewTypeauthor
  --------------------------------------------------------------------}


Procedure TReviewTypeauthor.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReviewTypesource
  --------------------------------------------------------------------}


Procedure TReviewTypesource.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReviewTypesource.SetextraDescription(AIndex : Integer; const AValue : String); 

begin
  If (FextraDescription=AValue) then exit;
  FextraDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReviewTypesource.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReview
  --------------------------------------------------------------------}


Procedure TReview.Setauthor(AIndex : Integer; const AValue : TReviewTypeauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setcontent(AIndex : Integer; const AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setdate(AIndex : Integer; const AValue : String); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.SetfullTextUrl(AIndex : Integer; const AValue : String); 

begin
  If (FfullTextUrl=AValue) then exit;
  FfullTextUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setrating(AIndex : Integer; const AValue : String); 

begin
  If (Frating=AValue) then exit;
  Frating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setsource(AIndex : Integer; const AValue : TReviewTypesource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TReview.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSeriesTypeseriesItem
  --------------------------------------------------------------------}


Procedure TSeriesTypeseriesItem.SetbannerImageUrl(AIndex : Integer; const AValue : String); 

begin
  If (FbannerImageUrl=AValue) then exit;
  FbannerImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeriesTypeseriesItem.SetimageUrl(AIndex : Integer; const AValue : String); 

begin
  If (FimageUrl=AValue) then exit;
  FimageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeriesTypeseriesItem.SetseriesId(AIndex : Integer; const AValue : String); 

begin
  If (FseriesId=AValue) then exit;
  FseriesId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeriesTypeseriesItem.SetseriesType(AIndex : Integer; const AValue : String); 

begin
  If (FseriesType=AValue) then exit;
  FseriesType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeriesTypeseriesItem.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSeries
  --------------------------------------------------------------------}


Procedure TSeries.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeries.Setseries(AIndex : Integer; const AValue : TSeriesTypeseriesArray); 

begin
  If (Fseries=AValue) then exit;
  Fseries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSeries.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'series' : SetLength(Fseries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSeriesmembership
  --------------------------------------------------------------------}


Procedure TSeriesmembership.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeriesmembership.Setmember(AIndex : Integer; const AValue : TSeriesmembershipTypememberArray); 

begin
  If (Fmember=AValue) then exit;
  Fmember:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeriesmembership.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSeriesmembership.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'member' : SetLength(Fmember,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUsersettingsTypenotesExport
  --------------------------------------------------------------------}


Procedure TUsersettingsTypenotesExport.SetfolderName(AIndex : Integer; const AValue : String); 

begin
  If (FfolderName=AValue) then exit;
  FfolderName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsersettingsTypenotesExport.SetisEnabled(AIndex : Integer; const AValue : boolean); 

begin
  If (FisEnabled=AValue) then exit;
  FisEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersettingsTypenotificationTypemoreFromAuthors
  --------------------------------------------------------------------}


Procedure TUsersettingsTypenotificationTypemoreFromAuthors.Setopted_state(AIndex : Integer; const AValue : String); 

begin
  If (Fopted_state=AValue) then exit;
  Fopted_state:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersettingsTypenotification
  --------------------------------------------------------------------}


Procedure TUsersettingsTypenotification.SetmoreFromAuthors(AIndex : Integer; const AValue : TUsersettingsTypenotificationTypemoreFromAuthors); 

begin
  If (FmoreFromAuthors=AValue) then exit;
  FmoreFromAuthors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersettings
  --------------------------------------------------------------------}


Procedure TUsersettings.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsersettings.SetnotesExport(AIndex : Integer; const AValue : TUsersettingsTypenotesExport); 

begin
  If (FnotesExport=AValue) then exit;
  FnotesExport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsersettings.Setnotification(AIndex : Integer; const AValue : TUsersettingsTypenotification); 

begin
  If (Fnotification=AValue) then exit;
  Fnotification:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypeaccessInfoTypeepub
  --------------------------------------------------------------------}


Procedure TVolumeTypeaccessInfoTypeepub.SetacsTokenLink(AIndex : Integer; const AValue : String); 

begin
  If (FacsTokenLink=AValue) then exit;
  FacsTokenLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfoTypeepub.SetdownloadLink(AIndex : Integer; const AValue : String); 

begin
  If (FdownloadLink=AValue) then exit;
  FdownloadLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfoTypeepub.SetisAvailable(AIndex : Integer; const AValue : boolean); 

begin
  If (FisAvailable=AValue) then exit;
  FisAvailable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypeaccessInfoTypepdf
  --------------------------------------------------------------------}


Procedure TVolumeTypeaccessInfoTypepdf.SetacsTokenLink(AIndex : Integer; const AValue : String); 

begin
  If (FacsTokenLink=AValue) then exit;
  FacsTokenLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfoTypepdf.SetdownloadLink(AIndex : Integer; const AValue : String); 

begin
  If (FdownloadLink=AValue) then exit;
  FdownloadLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfoTypepdf.SetisAvailable(AIndex : Integer; const AValue : boolean); 

begin
  If (FisAvailable=AValue) then exit;
  FisAvailable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypeaccessInfo
  --------------------------------------------------------------------}


Procedure TVolumeTypeaccessInfo.SetaccessViewStatus(AIndex : Integer; const AValue : String); 

begin
  If (FaccessViewStatus=AValue) then exit;
  FaccessViewStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.Setcountry(AIndex : Integer; const AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.SetdownloadAccess(AIndex : Integer; const AValue : TDownloadAccessRestriction); 

begin
  If (FdownloadAccess=AValue) then exit;
  FdownloadAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.SetdriveImportedContentLink(AIndex : Integer; const AValue : String); 

begin
  If (FdriveImportedContentLink=AValue) then exit;
  FdriveImportedContentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.Setembeddable(AIndex : Integer; const AValue : boolean); 

begin
  If (Fembeddable=AValue) then exit;
  Fembeddable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.Setepub(AIndex : Integer; const AValue : TVolumeTypeaccessInfoTypeepub); 

begin
  If (Fepub=AValue) then exit;
  Fepub:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.SetexplicitOfflineLicenseManagement(AIndex : Integer; const AValue : boolean); 

begin
  If (FexplicitOfflineLicenseManagement=AValue) then exit;
  FexplicitOfflineLicenseManagement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.Setpdf(AIndex : Integer; const AValue : TVolumeTypeaccessInfoTypepdf); 

begin
  If (Fpdf=AValue) then exit;
  Fpdf:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.SetpublicDomain(AIndex : Integer; const AValue : boolean); 

begin
  If (FpublicDomain=AValue) then exit;
  FpublicDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.SetquoteSharingAllowed(AIndex : Integer; const AValue : boolean); 

begin
  If (FquoteSharingAllowed=AValue) then exit;
  FquoteSharingAllowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.SettextToSpeechPermission(AIndex : Integer; const AValue : String); 

begin
  If (FtextToSpeechPermission=AValue) then exit;
  FtextToSpeechPermission:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.SetviewOrderUrl(AIndex : Integer; const AValue : String); 

begin
  If (FviewOrderUrl=AValue) then exit;
  FviewOrderUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.Setviewability(AIndex : Integer; const AValue : String); 

begin
  If (Fviewability=AValue) then exit;
  Fviewability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeaccessInfo.SetwebReaderLink(AIndex : Integer; const AValue : String); 

begin
  If (FwebReaderLink=AValue) then exit;
  FwebReaderLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypelayerInfoTypelayersItem
  --------------------------------------------------------------------}


Procedure TVolumeTypelayerInfoTypelayersItem.SetlayerId(AIndex : Integer; const AValue : String); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypelayerInfoTypelayersItem.SetvolumeAnnotationsVersion(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeAnnotationsVersion=AValue) then exit;
  FvolumeAnnotationsVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypelayerInfo
  --------------------------------------------------------------------}


Procedure TVolumeTypelayerInfo.Setlayers(AIndex : Integer; const AValue : TVolumeTypelayerInfoTypelayersArray); 

begin
  If (Flayers=AValue) then exit;
  Flayers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolumeTypelayerInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'layers' : SetLength(Flayers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVolumeTyperecommendedInfo
  --------------------------------------------------------------------}


Procedure TVolumeTyperecommendedInfo.Setexplanation(AIndex : Integer; const AValue : String); 

begin
  If (Fexplanation=AValue) then exit;
  Fexplanation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypesaleInfoTypelistPrice
  --------------------------------------------------------------------}


Procedure TVolumeTypesaleInfoTypelistPrice.Setamount(AIndex : Integer; const AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfoTypelistPrice.SetcurrencyCode(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypesaleInfoTypeoffersItemTypelistPrice
  --------------------------------------------------------------------}


Procedure TVolumeTypesaleInfoTypeoffersItemTypelistPrice.SetamountInMicros(AIndex : Integer; const AValue : double); 

begin
  If (FamountInMicros=AValue) then exit;
  FamountInMicros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfoTypeoffersItemTypelistPrice.SetcurrencyCode(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypesaleInfoTypeoffersItemTyperentalDuration
  --------------------------------------------------------------------}


Procedure TVolumeTypesaleInfoTypeoffersItemTyperentalDuration.Setcount(AIndex : Integer; const AValue : double); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfoTypeoffersItemTyperentalDuration.Set_unit(AIndex : Integer; const AValue : String); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVolumeTypesaleInfoTypeoffersItemTyperentalDuration.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVolumeTypesaleInfoTypeoffersItemTyperetailPrice
  --------------------------------------------------------------------}


Procedure TVolumeTypesaleInfoTypeoffersItemTyperetailPrice.SetamountInMicros(AIndex : Integer; const AValue : double); 

begin
  If (FamountInMicros=AValue) then exit;
  FamountInMicros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfoTypeoffersItemTyperetailPrice.SetcurrencyCode(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypesaleInfoTypeoffersItem
  --------------------------------------------------------------------}


Procedure TVolumeTypesaleInfoTypeoffersItem.SetfinskyOfferType(AIndex : Integer; const AValue : integer); 

begin
  If (FfinskyOfferType=AValue) then exit;
  FfinskyOfferType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfoTypeoffersItem.SetlistPrice(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypeoffersItemTypelistPrice); 

begin
  If (FlistPrice=AValue) then exit;
  FlistPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfoTypeoffersItem.SetrentalDuration(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypeoffersItemTyperentalDuration); 

begin
  If (FrentalDuration=AValue) then exit;
  FrentalDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfoTypeoffersItem.SetretailPrice(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypeoffersItemTyperetailPrice); 

begin
  If (FretailPrice=AValue) then exit;
  FretailPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypesaleInfoTyperetailPrice
  --------------------------------------------------------------------}


Procedure TVolumeTypesaleInfoTyperetailPrice.Setamount(AIndex : Integer; const AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfoTyperetailPrice.SetcurrencyCode(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypesaleInfo
  --------------------------------------------------------------------}


Procedure TVolumeTypesaleInfo.SetbuyLink(AIndex : Integer; const AValue : String); 

begin
  If (FbuyLink=AValue) then exit;
  FbuyLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfo.Setcountry(AIndex : Integer; const AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfo.SetisEbook(AIndex : Integer; const AValue : boolean); 

begin
  If (FisEbook=AValue) then exit;
  FisEbook:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfo.SetlistPrice(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypelistPrice); 

begin
  If (FlistPrice=AValue) then exit;
  FlistPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfo.Setoffers(AIndex : Integer; const AValue : TVolumeTypesaleInfoTypeoffersArray); 

begin
  If (Foffers=AValue) then exit;
  Foffers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfo.SetonSaleDate(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FonSaleDate=AValue) then exit;
  FonSaleDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfo.SetretailPrice(AIndex : Integer; const AValue : TVolumeTypesaleInfoTyperetailPrice); 

begin
  If (FretailPrice=AValue) then exit;
  FretailPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypesaleInfo.Setsaleability(AIndex : Integer; const AValue : String); 

begin
  If (Fsaleability=AValue) then exit;
  Fsaleability:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolumeTypesaleInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'offers' : SetLength(Foffers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVolumeTypesearchInfo
  --------------------------------------------------------------------}


Procedure TVolumeTypesearchInfo.SettextSnippet(AIndex : Integer; const AValue : String); 

begin
  If (FtextSnippet=AValue) then exit;
  FtextSnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypeuserInfoTypecopy
  --------------------------------------------------------------------}


Procedure TVolumeTypeuserInfoTypecopy.SetallowedCharacterCount(AIndex : Integer; const AValue : integer); 

begin
  If (FallowedCharacterCount=AValue) then exit;
  FallowedCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfoTypecopy.SetlimitType(AIndex : Integer; const AValue : String); 

begin
  If (FlimitType=AValue) then exit;
  FlimitType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfoTypecopy.SetremainingCharacterCount(AIndex : Integer; const AValue : integer); 

begin
  If (FremainingCharacterCount=AValue) then exit;
  FremainingCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfoTypecopy.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypeuserInfoTypefamilySharing
  --------------------------------------------------------------------}


Procedure TVolumeTypeuserInfoTypefamilySharing.SetfamilyRole(AIndex : Integer; const AValue : String); 

begin
  If (FfamilyRole=AValue) then exit;
  FfamilyRole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfoTypefamilySharing.SetisSharingAllowed(AIndex : Integer; const AValue : boolean); 

begin
  If (FisSharingAllowed=AValue) then exit;
  FisSharingAllowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfoTypefamilySharing.SetisSharingDisabledByFop(AIndex : Integer; const AValue : boolean); 

begin
  If (FisSharingDisabledByFop=AValue) then exit;
  FisSharingDisabledByFop:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypeuserInfoTyperentalPeriod
  --------------------------------------------------------------------}


Procedure TVolumeTypeuserInfoTyperentalPeriod.SetendUtcSec(AIndex : Integer; const AValue : String); 

begin
  If (FendUtcSec=AValue) then exit;
  FendUtcSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfoTyperentalPeriod.SetstartUtcSec(AIndex : Integer; const AValue : String); 

begin
  If (FstartUtcSec=AValue) then exit;
  FstartUtcSec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypeuserInfoTypeuserUploadedVolumeInfo
  --------------------------------------------------------------------}


Procedure TVolumeTypeuserInfoTypeuserUploadedVolumeInfo.SetprocessingState(AIndex : Integer; const AValue : String); 

begin
  If (FprocessingState=AValue) then exit;
  FprocessingState:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypeuserInfo
  --------------------------------------------------------------------}


Procedure TVolumeTypeuserInfo.SetacquiredTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FacquiredTime=AValue) then exit;
  FacquiredTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetacquisitionType(AIndex : Integer; const AValue : integer); 

begin
  If (FacquisitionType=AValue) then exit;
  FacquisitionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.Setcopy(AIndex : Integer; const AValue : TVolumeTypeuserInfoTypecopy); 

begin
  If (Fcopy=AValue) then exit;
  Fcopy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetentitlementType(AIndex : Integer; const AValue : integer); 

begin
  If (FentitlementType=AValue) then exit;
  FentitlementType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetfamilySharing(AIndex : Integer; const AValue : TVolumeTypeuserInfoTypefamilySharing); 

begin
  If (FfamilySharing=AValue) then exit;
  FfamilySharing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetisFamilySharedFromUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FisFamilySharedFromUser=AValue) then exit;
  FisFamilySharedFromUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetisFamilySharedToUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FisFamilySharedToUser=AValue) then exit;
  FisFamilySharedToUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetisFamilySharingAllowed(AIndex : Integer; const AValue : boolean); 

begin
  If (FisFamilySharingAllowed=AValue) then exit;
  FisFamilySharingAllowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetisFamilySharingDisabledByFop(AIndex : Integer; const AValue : boolean); 

begin
  If (FisFamilySharingDisabledByFop=AValue) then exit;
  FisFamilySharingDisabledByFop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetisInMyBooks(AIndex : Integer; const AValue : boolean); 

begin
  If (FisInMyBooks=AValue) then exit;
  FisInMyBooks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetisPreordered(AIndex : Integer; const AValue : boolean); 

begin
  If (FisPreordered=AValue) then exit;
  FisPreordered:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetisPurchased(AIndex : Integer; const AValue : boolean); 

begin
  If (FisPurchased=AValue) then exit;
  FisPurchased:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetisUploaded(AIndex : Integer; const AValue : boolean); 

begin
  If (FisUploaded=AValue) then exit;
  FisUploaded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetreadingPosition(AIndex : Integer; const AValue : TReadingPosition); 

begin
  If (FreadingPosition=AValue) then exit;
  FreadingPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetrentalPeriod(AIndex : Integer; const AValue : TVolumeTypeuserInfoTyperentalPeriod); 

begin
  If (FrentalPeriod=AValue) then exit;
  FrentalPeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetrentalState(AIndex : Integer; const AValue : String); 

begin
  If (FrentalState=AValue) then exit;
  FrentalState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.Setreview(AIndex : Integer; const AValue : TReview); 

begin
  If (Freview=AValue) then exit;
  Freview:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypeuserInfo.SetuserUploadedVolumeInfo(AIndex : Integer; const AValue : TVolumeTypeuserInfoTypeuserUploadedVolumeInfo); 

begin
  If (FuserUploadedVolumeInfo=AValue) then exit;
  FuserUploadedVolumeInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypevolumeInfoTypedimensions
  --------------------------------------------------------------------}


Procedure TVolumeTypevolumeInfoTypedimensions.Setheight(AIndex : Integer; const AValue : String); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfoTypedimensions.Setthickness(AIndex : Integer; const AValue : String); 

begin
  If (Fthickness=AValue) then exit;
  Fthickness:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfoTypedimensions.Setwidth(AIndex : Integer; const AValue : String); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypevolumeInfoTypeimageLinks
  --------------------------------------------------------------------}


Procedure TVolumeTypevolumeInfoTypeimageLinks.SetextraLarge(AIndex : Integer; const AValue : String); 

begin
  If (FextraLarge=AValue) then exit;
  FextraLarge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfoTypeimageLinks.Setlarge(AIndex : Integer; const AValue : String); 

begin
  If (Flarge=AValue) then exit;
  Flarge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfoTypeimageLinks.Setmedium(AIndex : Integer; const AValue : String); 

begin
  If (Fmedium=AValue) then exit;
  Fmedium:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfoTypeimageLinks.Setsmall(AIndex : Integer; const AValue : String); 

begin
  If (Fsmall=AValue) then exit;
  Fsmall:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfoTypeimageLinks.SetsmallThumbnail(AIndex : Integer; const AValue : String); 

begin
  If (FsmallThumbnail=AValue) then exit;
  FsmallThumbnail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfoTypeimageLinks.Setthumbnail(AIndex : Integer; const AValue : String); 

begin
  If (Fthumbnail=AValue) then exit;
  Fthumbnail:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeTypevolumeInfoTypeindustryIdentifiersItem
  --------------------------------------------------------------------}


Procedure TVolumeTypevolumeInfoTypeindustryIdentifiersItem.Setidentifier(AIndex : Integer; const AValue : String); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfoTypeindustryIdentifiersItem.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVolumeTypevolumeInfoTypeindustryIdentifiersItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVolumeTypevolumeInfo
  --------------------------------------------------------------------}


Procedure TVolumeTypevolumeInfo.SetallowAnonLogging(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowAnonLogging=AValue) then exit;
  FallowAnonLogging:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.Setauthors(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fauthors=AValue) then exit;
  Fauthors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetaverageRating(AIndex : Integer; const AValue : double); 

begin
  If (FaverageRating=AValue) then exit;
  FaverageRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetcanonicalVolumeLink(AIndex : Integer; const AValue : String); 

begin
  If (FcanonicalVolumeLink=AValue) then exit;
  FcanonicalVolumeLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.Setcategories(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fcategories=AValue) then exit;
  Fcategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetcontentVersion(AIndex : Integer; const AValue : String); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.Setdimensions(AIndex : Integer; const AValue : TVolumeTypevolumeInfoTypedimensions); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetimageLinks(AIndex : Integer; const AValue : TVolumeTypevolumeInfoTypeimageLinks); 

begin
  If (FimageLinks=AValue) then exit;
  FimageLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetindustryIdentifiers(AIndex : Integer; const AValue : TVolumeTypevolumeInfoTypeindustryIdentifiersArray); 

begin
  If (FindustryIdentifiers=AValue) then exit;
  FindustryIdentifiers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetinfoLink(AIndex : Integer; const AValue : String); 

begin
  If (FinfoLink=AValue) then exit;
  FinfoLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.Setlanguage(AIndex : Integer; const AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetmainCategory(AIndex : Integer; const AValue : String); 

begin
  If (FmainCategory=AValue) then exit;
  FmainCategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetmaturityRating(AIndex : Integer; const AValue : String); 

begin
  If (FmaturityRating=AValue) then exit;
  FmaturityRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetpageCount(AIndex : Integer; const AValue : integer); 

begin
  If (FpageCount=AValue) then exit;
  FpageCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetpreviewLink(AIndex : Integer; const AValue : String); 

begin
  If (FpreviewLink=AValue) then exit;
  FpreviewLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetprintType(AIndex : Integer; const AValue : String); 

begin
  If (FprintType=AValue) then exit;
  FprintType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetprintedPageCount(AIndex : Integer; const AValue : integer); 

begin
  If (FprintedPageCount=AValue) then exit;
  FprintedPageCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetpublishedDate(AIndex : Integer; const AValue : String); 

begin
  If (FpublishedDate=AValue) then exit;
  FpublishedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.Setpublisher(AIndex : Integer; const AValue : String); 

begin
  If (Fpublisher=AValue) then exit;
  Fpublisher:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetratingsCount(AIndex : Integer; const AValue : integer); 

begin
  If (FratingsCount=AValue) then exit;
  FratingsCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetreadingModes(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (FreadingModes=AValue) then exit;
  FreadingModes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetsamplePageCount(AIndex : Integer; const AValue : integer); 

begin
  If (FsamplePageCount=AValue) then exit;
  FsamplePageCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.SetseriesInfo(AIndex : Integer; const AValue : TVolumeseriesinfo); 

begin
  If (FseriesInfo=AValue) then exit;
  FseriesInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.Setsubtitle(AIndex : Integer; const AValue : String); 

begin
  If (Fsubtitle=AValue) then exit;
  Fsubtitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeTypevolumeInfo.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolumeTypevolumeInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'authors' : SetLength(Fauthors,ALength);
  'categories' : SetLength(Fcategories,ALength);
  'industryidentifiers' : SetLength(FindustryIdentifiers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVolume
  --------------------------------------------------------------------}


Procedure TVolume.SetaccessInfo(AIndex : Integer; const AValue : TVolumeTypeaccessInfo); 

begin
  If (FaccessInfo=AValue) then exit;
  FaccessInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetlayerInfo(AIndex : Integer; const AValue : TVolumeTypelayerInfo); 

begin
  If (FlayerInfo=AValue) then exit;
  FlayerInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetrecommendedInfo(AIndex : Integer; const AValue : TVolumeTyperecommendedInfo); 

begin
  If (FrecommendedInfo=AValue) then exit;
  FrecommendedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetsaleInfo(AIndex : Integer; const AValue : TVolumeTypesaleInfo); 

begin
  If (FsaleInfo=AValue) then exit;
  FsaleInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetsearchInfo(AIndex : Integer; const AValue : TVolumeTypesearchInfo); 

begin
  If (FsearchInfo=AValue) then exit;
  FsearchInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetuserInfo(AIndex : Integer; const AValue : TVolumeTypeuserInfo); 

begin
  If (FuserInfo=AValue) then exit;
  FuserInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetvolumeInfo(AIndex : Integer; const AValue : TVolumeTypevolumeInfo); 

begin
  If (FvolumeInfo=AValue) then exit;
  FvolumeInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolume2
  --------------------------------------------------------------------}


Procedure TVolume2.Setitems(AIndex : Integer; const AValue : TVolume2TypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume2.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume2.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolume2.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVolumeannotationTypecontentRanges
  --------------------------------------------------------------------}


Procedure TVolumeannotationTypecontentRanges.SetcfiRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FcfiRange=AValue) then exit;
  FcfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotationTypecontentRanges.SetcontentVersion(AIndex : Integer; const AValue : String); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotationTypecontentRanges.SetgbImageRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FgbImageRange=AValue) then exit;
  FgbImageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotationTypecontentRanges.SetgbTextRange(AIndex : Integer; const AValue : TBooksAnnotationsRange); 

begin
  If (FgbTextRange=AValue) then exit;
  FgbTextRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeannotation
  --------------------------------------------------------------------}


Procedure TVolumeannotation.SetannotationDataId(AIndex : Integer; const AValue : String); 

begin
  If (FannotationDataId=AValue) then exit;
  FannotationDataId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetannotationDataLink(AIndex : Integer; const AValue : String); 

begin
  If (FannotationDataLink=AValue) then exit;
  FannotationDataLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetannotationType(AIndex : Integer; const AValue : String); 

begin
  If (FannotationType=AValue) then exit;
  FannotationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetcontentRanges(AIndex : Integer; const AValue : TVolumeannotationTypecontentRanges); 

begin
  If (FcontentRanges=AValue) then exit;
  FcontentRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setdata(AIndex : Integer; const AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setdeleted(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetlayerId(AIndex : Integer; const AValue : String); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetpageIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FpageIds=AValue) then exit;
  FpageIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetselectedText(AIndex : Integer; const AValue : String); 

begin
  If (FselectedText=AValue) then exit;
  FselectedText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetvolumeId(AIndex : Integer; const AValue : String); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolumeannotation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'pageids' : SetLength(FpageIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVolumeannotations
  --------------------------------------------------------------------}


Procedure TVolumeannotations.Setitems(AIndex : Integer; const AValue : TVolumeannotationsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotations.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotations.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotations.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotations.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolumeannotations.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVolumes
  --------------------------------------------------------------------}


Procedure TVolumes.Setitems(AIndex : Integer; const AValue : TVolumesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumes.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumes.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolumes.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem
  --------------------------------------------------------------------}


Procedure TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem.SetissueDisplayNumber(AIndex : Integer; const AValue : String); 

begin
  If (FissueDisplayNumber=AValue) then exit;
  FissueDisplayNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem.SetissueOrderNumber(AIndex : Integer; const AValue : integer); 

begin
  If (FissueOrderNumber=AValue) then exit;
  FissueOrderNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeseriesinfoTypevolumeSeriesItem
  --------------------------------------------------------------------}


Procedure TVolumeseriesinfoTypevolumeSeriesItem.Setissue(AIndex : Integer; const AValue : TVolumeseriesinfoTypevolumeSeriesItemTypeissueArray); 

begin
  If (Fissue=AValue) then exit;
  Fissue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeseriesinfoTypevolumeSeriesItem.SetorderNumber(AIndex : Integer; const AValue : integer); 

begin
  If (ForderNumber=AValue) then exit;
  ForderNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeseriesinfoTypevolumeSeriesItem.SetseriesBookType(AIndex : Integer; const AValue : String); 

begin
  If (FseriesBookType=AValue) then exit;
  FseriesBookType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeseriesinfoTypevolumeSeriesItem.SetseriesId(AIndex : Integer; const AValue : String); 

begin
  If (FseriesId=AValue) then exit;
  FseriesId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolumeseriesinfoTypevolumeSeriesItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'issue' : SetLength(Fissue,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVolumeseriesinfo
  --------------------------------------------------------------------}


Procedure TVolumeseriesinfo.SetbookDisplayNumber(AIndex : Integer; const AValue : String); 

begin
  If (FbookDisplayNumber=AValue) then exit;
  FbookDisplayNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeseriesinfo.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeseriesinfo.SetshortSeriesBookTitle(AIndex : Integer; const AValue : String); 

begin
  If (FshortSeriesBookTitle=AValue) then exit;
  FshortSeriesBookTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeseriesinfo.SetvolumeSeries(AIndex : Integer; const AValue : TVolumeseriesinfoTypevolumeSeriesArray); 

begin
  If (FvolumeSeries=AValue) then exit;
  FvolumeSeries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVolumeseriesinfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'volumeseries' : SetLength(FvolumeSeries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBookshelvesVolumesResource
  --------------------------------------------------------------------}


Class Function TBookshelvesVolumesResource.ResourceName : String;

begin
  Result:='volumes';
end;

Class Function TBookshelvesVolumesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TBookshelvesVolumesResource.List(shelf: string; userId: string; AQuery : string = '') : TVolumes;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/{userId}/bookshelves/{shelf}/volumes';
  _Methodid   = 'books.bookshelves.volumes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['shelf',shelf,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVolumes) as TVolumes;
end;


Function TBookshelvesVolumesResource.List(shelf: string; userId: string; AQuery : TBookshelvesVolumeslistOptions) : TVolumes;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'showPreorders',AQuery.showPreorders);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=List(shelf,userId,_Q);
end;



{ --------------------------------------------------------------------
  TBookshelvesResource
  --------------------------------------------------------------------}


Class Function TBookshelvesResource.ResourceName : String;

begin
  Result:='bookshelves';
end;

Class Function TBookshelvesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TBookshelvesResource.Get(shelf: string; userId: string; AQuery : string = '') : TBookshelf;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/{userId}/bookshelves/{shelf}';
  _Methodid   = 'books.bookshelves.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['shelf',shelf,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBookshelf) as TBookshelf;
end;


Function TBookshelvesResource.Get(shelf: string; userId: string; AQuery : TBookshelvesgetOptions) : TBookshelf;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Get(shelf,userId,_Q);
end;

Function TBookshelvesResource.List(userId: string; AQuery : string = '') : TBookshelves;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/{userId}/bookshelves';
  _Methodid   = 'books.bookshelves.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBookshelves) as TBookshelves;
end;


Function TBookshelvesResource.List(userId: string; AQuery : TBookshelveslistOptions) : TBookshelves;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'source',AQuery.source);
  Result:=List(userId,_Q);
end;



Function TBookshelvesResource.GetVolumesInstance : TBookshelvesVolumesResource;

begin
  if (FVolumesInstance=Nil) then
    FVolumesInstance:=CreateVolumesResource;
  Result:=FVolumesInstance;
end;

Function TBookshelvesResource.CreateVolumesResource : TBookshelvesVolumesResource;

begin
  Result:=CreateVolumesResource(Self);
end;


Function TBookshelvesResource.CreateVolumesResource(AOwner : TComponent) : TBookshelvesVolumesResource;

begin
  Result:=TBookshelvesVolumesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TCloudloadingResource
  --------------------------------------------------------------------}


Class Function TCloudloadingResource.ResourceName : String;

begin
  Result:='cloudloading';
end;

Class Function TCloudloadingResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TCloudloadingResource.AddBook(AQuery : string = '') : TBooksCloudloadingResource;

Const
  _HTTPMethod = 'POST';
  _Path       = 'cloudloading/addBook';
  _Methodid   = 'books.cloudloading.addBook';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TBooksCloudloadingResource) as TBooksCloudloadingResource;
end;


Function TCloudloadingResource.AddBook(AQuery : TCloudloadingaddBookOptions) : TBooksCloudloadingResource;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'drive_document_id',AQuery.drive_document_id);
  AddToQuery(_Q,'mime_type',AQuery.mime_type);
  AddToQuery(_Q,'name',AQuery._name);
  AddToQuery(_Q,'upload_client_token',AQuery.upload_client_token);
  Result:=AddBook(_Q);
end;

Procedure TCloudloadingResource.DeleteBook(AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'cloudloading/deleteBook';
  _Methodid   = 'books.cloudloading.deleteBook';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TCloudloadingResource.DeleteBook(AQuery : TCloudloadingdeleteBookOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  DeleteBook(_Q);
end;

Function TCloudloadingResource.UpdateBook(aBooksCloudloadingResource : TBooksCloudloadingResource) : TBooksCloudloadingResource;

Const
  _HTTPMethod = 'POST';
  _Path       = 'cloudloading/updateBook';
  _Methodid   = 'books.cloudloading.updateBook';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aBooksCloudloadingResource,TBooksCloudloadingResource) as TBooksCloudloadingResource;
end;



{ --------------------------------------------------------------------
  TDictionaryResource
  --------------------------------------------------------------------}


Class Function TDictionaryResource.ResourceName : String;

begin
  Result:='dictionary';
end;

Class Function TDictionaryResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TDictionaryResource.ListOfflineMetadata(AQuery : string = '') : TMetadata;

Const
  _HTTPMethod = 'GET';
  _Path       = 'dictionary/listOfflineMetadata';
  _Methodid   = 'books.dictionary.listOfflineMetadata';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TMetadata) as TMetadata;
end;


Function TDictionaryResource.ListOfflineMetadata(AQuery : TDictionarylistOfflineMetadataOptions) : TMetadata;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'cpksver',AQuery.cpksver);
  Result:=ListOfflineMetadata(_Q);
end;



{ --------------------------------------------------------------------
  TLayersAnnotationDataResource
  --------------------------------------------------------------------}


Class Function TLayersAnnotationDataResource.ResourceName : String;

begin
  Result:='annotationData';
end;

Class Function TLayersAnnotationDataResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TLayersAnnotationDataResource.Get(annotationDataId: string; layerId: string; volumeId: string; AQuery : string = '') : TAnnotationdata;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/{volumeId}/layers/{layerId}/data/{annotationDataId}';
  _Methodid   = 'books.layers.annotationData.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationDataId',annotationDataId,'layerId',layerId,'volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAnnotationdata) as TAnnotationdata;
end;


Function TLayersAnnotationDataResource.Get(annotationDataId: string; layerId: string; volumeId: string; AQuery : TLayersAnnotationDatagetOptions) : TAnnotationdata;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'allowWebDefinitions',AQuery.allowWebDefinitions);
  AddToQuery(_Q,'contentVersion',AQuery.contentVersion);
  AddToQuery(_Q,'h',AQuery.h);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'scale',AQuery.scale);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'w',AQuery.w);
  Result:=Get(annotationDataId,layerId,volumeId,_Q);
end;

Function TLayersAnnotationDataResource.List(layerId: string; volumeId: string; AQuery : string = '') : TAnnotationsdata;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/{volumeId}/layers/{layerId}/data';
  _Methodid   = 'books.layers.annotationData.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['layerId',layerId,'volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAnnotationsdata) as TAnnotationsdata;
end;


Function TLayersAnnotationDataResource.List(layerId: string; volumeId: string; AQuery : TLayersAnnotationDatalistOptions) : TAnnotationsdata;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'annotationDataId',AQuery.annotationDataId);
  AddToQuery(_Q,'contentVersion',AQuery.contentVersion);
  AddToQuery(_Q,'h',AQuery.h);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'scale',AQuery.scale);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'updatedMax',AQuery.updatedMax);
  AddToQuery(_Q,'updatedMin',AQuery.updatedMin);
  AddToQuery(_Q,'w',AQuery.w);
  Result:=List(layerId,volumeId,_Q);
end;



{ --------------------------------------------------------------------
  TLayersVolumeAnnotationsResource
  --------------------------------------------------------------------}


Class Function TLayersVolumeAnnotationsResource.ResourceName : String;

begin
  Result:='volumeAnnotations';
end;

Class Function TLayersVolumeAnnotationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TLayersVolumeAnnotationsResource.Get(annotationId: string; layerId: string; volumeId: string; AQuery : string = '') : TVolumeannotation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/{volumeId}/layers/{layerId}/annotations/{annotationId}';
  _Methodid   = 'books.layers.volumeAnnotations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId,'layerId',layerId,'volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVolumeannotation) as TVolumeannotation;
end;


Function TLayersVolumeAnnotationsResource.Get(annotationId: string; layerId: string; volumeId: string; AQuery : TLayersVolumeAnnotationsgetOptions) : TVolumeannotation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Get(annotationId,layerId,volumeId,_Q);
end;

Function TLayersVolumeAnnotationsResource.List(layerId: string; volumeId: string; AQuery : string = '') : TVolumeannotations;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/{volumeId}/layers/{layerId}';
  _Methodid   = 'books.layers.volumeAnnotations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['layerId',layerId,'volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVolumeannotations) as TVolumeannotations;
end;


Function TLayersVolumeAnnotationsResource.List(layerId: string; volumeId: string; AQuery : TLayersVolumeAnnotationslistOptions) : TVolumeannotations;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'contentVersion',AQuery.contentVersion);
  AddToQuery(_Q,'endOffset',AQuery.endOffset);
  AddToQuery(_Q,'endPosition',AQuery.endPosition);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'startOffset',AQuery.startOffset);
  AddToQuery(_Q,'startPosition',AQuery.startPosition);
  AddToQuery(_Q,'updatedMax',AQuery.updatedMax);
  AddToQuery(_Q,'updatedMin',AQuery.updatedMin);
  AddToQuery(_Q,'volumeAnnotationsVersion',AQuery.volumeAnnotationsVersion);
  Result:=List(layerId,volumeId,_Q);
end;



{ --------------------------------------------------------------------
  TLayersResource
  --------------------------------------------------------------------}


Class Function TLayersResource.ResourceName : String;

begin
  Result:='layers';
end;

Class Function TLayersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TLayersResource.Get(summaryId: string; volumeId: string; AQuery : string = '') : TLayersummary;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/{volumeId}/layersummary/{summaryId}';
  _Methodid   = 'books.layers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['summaryId',summaryId,'volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLayersummary) as TLayersummary;
end;


Function TLayersResource.Get(summaryId: string; volumeId: string; AQuery : TLayersgetOptions) : TLayersummary;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'contentVersion',AQuery.contentVersion);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Get(summaryId,volumeId,_Q);
end;

Function TLayersResource.List(volumeId: string; AQuery : string = '') : TLayersummaries;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/{volumeId}/layersummary';
  _Methodid   = 'books.layers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLayersummaries) as TLayersummaries;
end;


Function TLayersResource.List(volumeId: string; AQuery : TLayerslistOptions) : TLayersummaries;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'contentVersion',AQuery.contentVersion);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=List(volumeId,_Q);
end;



Function TLayersResource.GetAnnotationDataInstance : TLayersAnnotationDataResource;

begin
  if (FAnnotationDataInstance=Nil) then
    FAnnotationDataInstance:=CreateAnnotationDataResource;
  Result:=FAnnotationDataInstance;
end;

Function TLayersResource.CreateAnnotationDataResource : TLayersAnnotationDataResource;

begin
  Result:=CreateAnnotationDataResource(Self);
end;


Function TLayersResource.CreateAnnotationDataResource(AOwner : TComponent) : TLayersAnnotationDataResource;

begin
  Result:=TLayersAnnotationDataResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TLayersResource.GetVolumeAnnotationsInstance : TLayersVolumeAnnotationsResource;

begin
  if (FVolumeAnnotationsInstance=Nil) then
    FVolumeAnnotationsInstance:=CreateVolumeAnnotationsResource;
  Result:=FVolumeAnnotationsInstance;
end;

Function TLayersResource.CreateVolumeAnnotationsResource : TLayersVolumeAnnotationsResource;

begin
  Result:=CreateVolumeAnnotationsResource(Self);
end;


Function TLayersResource.CreateVolumeAnnotationsResource(AOwner : TComponent) : TLayersVolumeAnnotationsResource;

begin
  Result:=TLayersVolumeAnnotationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TMyconfigResource
  --------------------------------------------------------------------}


Class Function TMyconfigResource.ResourceName : String;

begin
  Result:='myconfig';
end;

Class Function TMyconfigResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TMyconfigResource.GetUserSettings : TUsersettings;

Const
  _HTTPMethod = 'GET';
  _Path       = 'myconfig/getUserSettings';
  _Methodid   = 'books.myconfig.getUserSettings';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TUsersettings) as TUsersettings;
end;

Function TMyconfigResource.ReleaseDownloadAccess(AQuery : string = '') : TDownloadAccesses;

Const
  _HTTPMethod = 'POST';
  _Path       = 'myconfig/releaseDownloadAccess';
  _Methodid   = 'books.myconfig.releaseDownloadAccess';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TDownloadAccesses) as TDownloadAccesses;
end;


Function TMyconfigResource.ReleaseDownloadAccess(AQuery : TMyconfigreleaseDownloadAccessOptions) : TDownloadAccesses;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'cpksver',AQuery.cpksver);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'volumeIds',AQuery.volumeIds);
  Result:=ReleaseDownloadAccess(_Q);
end;

Function TMyconfigResource.RequestAccess(AQuery : string = '') : TRequestAccess;

Const
  _HTTPMethod = 'POST';
  _Path       = 'myconfig/requestAccess';
  _Methodid   = 'books.myconfig.requestAccess';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TRequestAccess) as TRequestAccess;
end;


Function TMyconfigResource.RequestAccess(AQuery : TMyconfigrequestAccessOptions) : TRequestAccess;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'cpksver',AQuery.cpksver);
  AddToQuery(_Q,'licenseTypes',AQuery.licenseTypes);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'nonce',AQuery.nonce);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  Result:=RequestAccess(_Q);
end;

Function TMyconfigResource.SyncVolumeLicenses(AQuery : string = '') : TVolumes;

Const
  _HTTPMethod = 'POST';
  _Path       = 'myconfig/syncVolumeLicenses';
  _Methodid   = 'books.myconfig.syncVolumeLicenses';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVolumes) as TVolumes;
end;


Function TMyconfigResource.SyncVolumeLicenses(AQuery : TMyconfigsyncVolumeLicensesOptions) : TVolumes;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'cpksver',AQuery.cpksver);
  AddToQuery(_Q,'features',AQuery.features);
  AddToQuery(_Q,'includeNonComicsSeries',AQuery.includeNonComicsSeries);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'nonce',AQuery.nonce);
  AddToQuery(_Q,'showPreorders',AQuery.showPreorders);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'volumeIds',AQuery.volumeIds);
  Result:=SyncVolumeLicenses(_Q);
end;

Function TMyconfigResource.UpdateUserSettings(aUsersettings : TUsersettings) : TUsersettings;

Const
  _HTTPMethod = 'POST';
  _Path       = 'myconfig/updateUserSettings';
  _Methodid   = 'books.myconfig.updateUserSettings';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aUsersettings,TUsersettings) as TUsersettings;
end;



{ --------------------------------------------------------------------
  TMylibraryAnnotationsResource
  --------------------------------------------------------------------}


Class Function TMylibraryAnnotationsResource.ResourceName : String;

begin
  Result:='annotations';
end;

Class Function TMylibraryAnnotationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Procedure TMylibraryAnnotationsResource.Delete(annotationId: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'mylibrary/annotations/{annotationId}';
  _Methodid   = 'books.mylibrary.annotations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TMylibraryAnnotationsResource.Delete(annotationId: string; AQuery : TMylibraryAnnotationsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'source',AQuery.source);
  Delete(annotationId,_Q);
end;

Function TMylibraryAnnotationsResource.Insert(aAnnotation : TAnnotation; AQuery : string = '') : TAnnotation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'mylibrary/annotations';
  _Methodid   = 'books.mylibrary.annotations.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aAnnotation,TAnnotation) as TAnnotation;
end;


Function TMylibraryAnnotationsResource.Insert(aAnnotation : TAnnotation; AQuery : TMylibraryAnnotationsinsertOptions) : TAnnotation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'country',AQuery.country);
  AddToQuery(_Q,'showOnlySummaryInResponse',AQuery.showOnlySummaryInResponse);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Insert(aAnnotation,_Q);
end;

Function TMylibraryAnnotationsResource.List(AQuery : string = '') : TAnnotations;

Const
  _HTTPMethod = 'GET';
  _Path       = 'mylibrary/annotations';
  _Methodid   = 'books.mylibrary.annotations.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAnnotations) as TAnnotations;
end;


Function TMylibraryAnnotationsResource.List(AQuery : TMylibraryAnnotationslistOptions) : TAnnotations;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'contentVersion',AQuery.contentVersion);
  AddToQuery(_Q,'layerId',AQuery.layerId);
  AddToQuery(_Q,'layerIds',AQuery.layerIds);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'updatedMax',AQuery.updatedMax);
  AddToQuery(_Q,'updatedMin',AQuery.updatedMin);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  Result:=List(_Q);
end;

Function TMylibraryAnnotationsResource.Summary(AQuery : string = '') : TAnnotationsSummary;

Const
  _HTTPMethod = 'POST';
  _Path       = 'mylibrary/annotations/summary';
  _Methodid   = 'books.mylibrary.annotations.summary';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAnnotationsSummary) as TAnnotationsSummary;
end;


Function TMylibraryAnnotationsResource.Summary(AQuery : TMylibraryAnnotationssummaryOptions) : TAnnotationsSummary;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'layerIds',AQuery.layerIds);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  Result:=Summary(_Q);
end;

Function TMylibraryAnnotationsResource.Update(annotationId: string; aAnnotation : TAnnotation; AQuery : string = '') : TAnnotation;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'mylibrary/annotations/{annotationId}';
  _Methodid   = 'books.mylibrary.annotations.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAnnotation,TAnnotation) as TAnnotation;
end;


Function TMylibraryAnnotationsResource.Update(annotationId: string; aAnnotation : TAnnotation; AQuery : TMylibraryAnnotationsupdateOptions) : TAnnotation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Update(annotationId,aAnnotation,_Q);
end;



{ --------------------------------------------------------------------
  TMylibraryBookshelvesVolumesResource
  --------------------------------------------------------------------}


Class Function TMylibraryBookshelvesVolumesResource.ResourceName : String;

begin
  Result:='volumes';
end;

Class Function TMylibraryBookshelvesVolumesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TMylibraryBookshelvesVolumesResource.List(shelf: string; AQuery : string = '') : TVolumes;

Const
  _HTTPMethod = 'GET';
  _Path       = 'mylibrary/bookshelves/{shelf}/volumes';
  _Methodid   = 'books.mylibrary.bookshelves.volumes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['shelf',shelf]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVolumes) as TVolumes;
end;


Function TMylibraryBookshelvesVolumesResource.List(shelf: string; AQuery : TMylibraryBookshelvesVolumeslistOptions) : TVolumes;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'country',AQuery.country);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'showPreorders',AQuery.showPreorders);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=List(shelf,_Q);
end;



{ --------------------------------------------------------------------
  TMylibraryBookshelvesResource
  --------------------------------------------------------------------}


Class Function TMylibraryBookshelvesResource.ResourceName : String;

begin
  Result:='bookshelves';
end;

Class Function TMylibraryBookshelvesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Procedure TMylibraryBookshelvesResource.AddVolume(shelf: string; AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'mylibrary/bookshelves/{shelf}/addVolume';
  _Methodid   = 'books.mylibrary.bookshelves.addVolume';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['shelf',shelf]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TMylibraryBookshelvesResource.AddVolume(shelf: string; AQuery : TMylibraryBookshelvesaddVolumeOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'reason',AQuery.reason);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  AddVolume(shelf,_Q);
end;

Procedure TMylibraryBookshelvesResource.ClearVolumes(shelf: string; AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'mylibrary/bookshelves/{shelf}/clearVolumes';
  _Methodid   = 'books.mylibrary.bookshelves.clearVolumes';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['shelf',shelf]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TMylibraryBookshelvesResource.ClearVolumes(shelf: string; AQuery : TMylibraryBookshelvesclearVolumesOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'source',AQuery.source);
  ClearVolumes(shelf,_Q);
end;

Function TMylibraryBookshelvesResource.Get(shelf: string; AQuery : string = '') : TBookshelf;

Const
  _HTTPMethod = 'GET';
  _Path       = 'mylibrary/bookshelves/{shelf}';
  _Methodid   = 'books.mylibrary.bookshelves.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['shelf',shelf]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBookshelf) as TBookshelf;
end;


Function TMylibraryBookshelvesResource.Get(shelf: string; AQuery : TMylibraryBookshelvesgetOptions) : TBookshelf;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Get(shelf,_Q);
end;

Function TMylibraryBookshelvesResource.List(AQuery : string = '') : TBookshelves;

Const
  _HTTPMethod = 'GET';
  _Path       = 'mylibrary/bookshelves';
  _Methodid   = 'books.mylibrary.bookshelves.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TBookshelves) as TBookshelves;
end;


Function TMylibraryBookshelvesResource.List(AQuery : TMylibraryBookshelveslistOptions) : TBookshelves;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'source',AQuery.source);
  Result:=List(_Q);
end;

Procedure TMylibraryBookshelvesResource.MoveVolume(shelf: string; AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'mylibrary/bookshelves/{shelf}/moveVolume';
  _Methodid   = 'books.mylibrary.bookshelves.moveVolume';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['shelf',shelf]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TMylibraryBookshelvesResource.MoveVolume(shelf: string; AQuery : TMylibraryBookshelvesmoveVolumeOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  AddToQuery(_Q,'volumePosition',AQuery.volumePosition);
  MoveVolume(shelf,_Q);
end;

Procedure TMylibraryBookshelvesResource.RemoveVolume(shelf: string; AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'mylibrary/bookshelves/{shelf}/removeVolume';
  _Methodid   = 'books.mylibrary.bookshelves.removeVolume';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['shelf',shelf]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TMylibraryBookshelvesResource.RemoveVolume(shelf: string; AQuery : TMylibraryBookshelvesremoveVolumeOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'reason',AQuery.reason);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  RemoveVolume(shelf,_Q);
end;



Function TMylibraryBookshelvesResource.GetVolumesInstance : TMylibraryBookshelvesVolumesResource;

begin
  if (FVolumesInstance=Nil) then
    FVolumesInstance:=CreateVolumesResource;
  Result:=FVolumesInstance;
end;

Function TMylibraryBookshelvesResource.CreateVolumesResource : TMylibraryBookshelvesVolumesResource;

begin
  Result:=CreateVolumesResource(Self);
end;


Function TMylibraryBookshelvesResource.CreateVolumesResource(AOwner : TComponent) : TMylibraryBookshelvesVolumesResource;

begin
  Result:=TMylibraryBookshelvesVolumesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TMylibraryReadingpositionsResource
  --------------------------------------------------------------------}


Class Function TMylibraryReadingpositionsResource.ResourceName : String;

begin
  Result:='readingpositions';
end;

Class Function TMylibraryReadingpositionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TMylibraryReadingpositionsResource.Get(volumeId: string; AQuery : string = '') : TReadingPosition;

Const
  _HTTPMethod = 'GET';
  _Path       = 'mylibrary/readingpositions/{volumeId}';
  _Methodid   = 'books.mylibrary.readingpositions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TReadingPosition) as TReadingPosition;
end;


Function TMylibraryReadingpositionsResource.Get(volumeId: string; AQuery : TMylibraryReadingpositionsgetOptions) : TReadingPosition;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'contentVersion',AQuery.contentVersion);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Get(volumeId,_Q);
end;

Procedure TMylibraryReadingpositionsResource.SetPosition(volumeId: string; AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'mylibrary/readingpositions/{volumeId}/setPosition';
  _Methodid   = 'books.mylibrary.readingpositions.setPosition';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['volumeId',volumeId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TMylibraryReadingpositionsResource.SetPosition(volumeId: string; AQuery : TMylibraryReadingpositionssetPositionOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'action',AQuery.action);
  AddToQuery(_Q,'contentVersion',AQuery.contentVersion);
  AddToQuery(_Q,'deviceCookie',AQuery.deviceCookie);
  AddToQuery(_Q,'position',AQuery.position);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'timestamp',AQuery.timestamp);
  SetPosition(volumeId,_Q);
end;



{ --------------------------------------------------------------------
  TMylibraryResource
  --------------------------------------------------------------------}


Class Function TMylibraryResource.ResourceName : String;

begin
  Result:='mylibrary';
end;

Class Function TMylibraryResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;



Function TMylibraryResource.GetAnnotationsInstance : TMylibraryAnnotationsResource;

begin
  if (FAnnotationsInstance=Nil) then
    FAnnotationsInstance:=CreateAnnotationsResource;
  Result:=FAnnotationsInstance;
end;

Function TMylibraryResource.CreateAnnotationsResource : TMylibraryAnnotationsResource;

begin
  Result:=CreateAnnotationsResource(Self);
end;


Function TMylibraryResource.CreateAnnotationsResource(AOwner : TComponent) : TMylibraryAnnotationsResource;

begin
  Result:=TMylibraryAnnotationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMylibraryResource.GetBookshelvesVolumesInstance : TMylibraryBookshelvesVolumesResource;

begin
  if (FBookshelvesVolumesInstance=Nil) then
    FBookshelvesVolumesInstance:=CreateBookshelvesVolumesResource;
  Result:=FBookshelvesVolumesInstance;
end;

Function TMylibraryResource.CreateBookshelvesVolumesResource : TMylibraryBookshelvesVolumesResource;

begin
  Result:=CreateBookshelvesVolumesResource(Self);
end;


Function TMylibraryResource.CreateBookshelvesVolumesResource(AOwner : TComponent) : TMylibraryBookshelvesVolumesResource;

begin
  Result:=TMylibraryBookshelvesVolumesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMylibraryResource.GetBookshelvesInstance : TMylibraryBookshelvesResource;

begin
  if (FBookshelvesInstance=Nil) then
    FBookshelvesInstance:=CreateBookshelvesResource;
  Result:=FBookshelvesInstance;
end;

Function TMylibraryResource.CreateBookshelvesResource : TMylibraryBookshelvesResource;

begin
  Result:=CreateBookshelvesResource(Self);
end;


Function TMylibraryResource.CreateBookshelvesResource(AOwner : TComponent) : TMylibraryBookshelvesResource;

begin
  Result:=TMylibraryBookshelvesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMylibraryResource.GetReadingpositionsInstance : TMylibraryReadingpositionsResource;

begin
  if (FReadingpositionsInstance=Nil) then
    FReadingpositionsInstance:=CreateReadingpositionsResource;
  Result:=FReadingpositionsInstance;
end;

Function TMylibraryResource.CreateReadingpositionsResource : TMylibraryReadingpositionsResource;

begin
  Result:=CreateReadingpositionsResource(Self);
end;


Function TMylibraryResource.CreateReadingpositionsResource(AOwner : TComponent) : TMylibraryReadingpositionsResource;

begin
  Result:=TMylibraryReadingpositionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TNotificationResource
  --------------------------------------------------------------------}


Class Function TNotificationResource.ResourceName : String;

begin
  Result:='notification';
end;

Class Function TNotificationResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TNotificationResource.Get(AQuery : string = '') : TNotification;

Const
  _HTTPMethod = 'GET';
  _Path       = 'notification/get';
  _Methodid   = 'books.notification.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TNotification) as TNotification;
end;


Function TNotificationResource.Get(AQuery : TNotificationgetOptions) : TNotification;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'notification_id',AQuery.notification_id);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Get(_Q);
end;



{ --------------------------------------------------------------------
  TOnboardingResource
  --------------------------------------------------------------------}


Class Function TOnboardingResource.ResourceName : String;

begin
  Result:='onboarding';
end;

Class Function TOnboardingResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TOnboardingResource.ListCategories(AQuery : string = '') : TCategory;

Const
  _HTTPMethod = 'GET';
  _Path       = 'onboarding/listCategories';
  _Methodid   = 'books.onboarding.listCategories';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TCategory) as TCategory;
end;


Function TOnboardingResource.ListCategories(AQuery : TOnboardinglistCategoriesOptions) : TCategory;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  Result:=ListCategories(_Q);
end;

Function TOnboardingResource.ListCategoryVolumes(AQuery : string = '') : TVolume2;

Const
  _HTTPMethod = 'GET';
  _Path       = 'onboarding/listCategoryVolumes';
  _Methodid   = 'books.onboarding.listCategoryVolumes';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVolume2) as TVolume2;
end;


Function TOnboardingResource.ListCategoryVolumes(AQuery : TOnboardinglistCategoryVolumesOptions) : TVolume2;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'categoryId',AQuery.categoryId);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxAllowedMaturityRating',AQuery.maxAllowedMaturityRating);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListCategoryVolumes(_Q);
end;



{ --------------------------------------------------------------------
  TPersonalizedstreamResource
  --------------------------------------------------------------------}


Class Function TPersonalizedstreamResource.ResourceName : String;

begin
  Result:='personalizedstream';
end;

Class Function TPersonalizedstreamResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TPersonalizedstreamResource.Get(AQuery : string = '') : TDiscoveryclusters;

Const
  _HTTPMethod = 'GET';
  _Path       = 'personalizedstream/get';
  _Methodid   = 'books.personalizedstream.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TDiscoveryclusters) as TDiscoveryclusters;
end;


Function TPersonalizedstreamResource.Get(AQuery : TPersonalizedstreamgetOptions) : TDiscoveryclusters;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxAllowedMaturityRating',AQuery.maxAllowedMaturityRating);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=Get(_Q);
end;



{ --------------------------------------------------------------------
  TPromoofferResource
  --------------------------------------------------------------------}


Class Function TPromoofferResource.ResourceName : String;

begin
  Result:='promooffer';
end;

Class Function TPromoofferResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Procedure TPromoofferResource.Accept(AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'promooffer/accept';
  _Methodid   = 'books.promooffer.accept';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TPromoofferResource.Accept(AQuery : TPromoofferacceptOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'androidId',AQuery.androidId);
  AddToQuery(_Q,'device',AQuery.device);
  AddToQuery(_Q,'manufacturer',AQuery.manufacturer);
  AddToQuery(_Q,'model',AQuery.model);
  AddToQuery(_Q,'offerId',AQuery.offerId);
  AddToQuery(_Q,'product',AQuery.product);
  AddToQuery(_Q,'serial',AQuery.serial);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  Accept(_Q);
end;

Procedure TPromoofferResource.Dismiss(AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'promooffer/dismiss';
  _Methodid   = 'books.promooffer.dismiss';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TPromoofferResource.Dismiss(AQuery : TPromoofferdismissOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'androidId',AQuery.androidId);
  AddToQuery(_Q,'device',AQuery.device);
  AddToQuery(_Q,'manufacturer',AQuery.manufacturer);
  AddToQuery(_Q,'model',AQuery.model);
  AddToQuery(_Q,'offerId',AQuery.offerId);
  AddToQuery(_Q,'product',AQuery.product);
  AddToQuery(_Q,'serial',AQuery.serial);
  Dismiss(_Q);
end;

Function TPromoofferResource.Get(AQuery : string = '') : TOffers;

Const
  _HTTPMethod = 'GET';
  _Path       = 'promooffer/get';
  _Methodid   = 'books.promooffer.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TOffers) as TOffers;
end;


Function TPromoofferResource.Get(AQuery : TPromooffergetOptions) : TOffers;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'androidId',AQuery.androidId);
  AddToQuery(_Q,'device',AQuery.device);
  AddToQuery(_Q,'manufacturer',AQuery.manufacturer);
  AddToQuery(_Q,'model',AQuery.model);
  AddToQuery(_Q,'product',AQuery.product);
  AddToQuery(_Q,'serial',AQuery.serial);
  Result:=Get(_Q);
end;



{ --------------------------------------------------------------------
  TSeriesMembershipResource
  --------------------------------------------------------------------}


Class Function TSeriesMembershipResource.ResourceName : String;

begin
  Result:='membership';
end;

Class Function TSeriesMembershipResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TSeriesMembershipResource.Get(AQuery : string = '') : TSeriesmembership;

Const
  _HTTPMethod = 'GET';
  _Path       = 'series/membership/get';
  _Methodid   = 'books.series.membership.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSeriesmembership) as TSeriesmembership;
end;


Function TSeriesMembershipResource.Get(AQuery : TSeriesMembershipgetOptions) : TSeriesmembership;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'page_size',AQuery.page_size);
  AddToQuery(_Q,'page_token',AQuery.page_token);
  AddToQuery(_Q,'series_id',AQuery.series_id);
  Result:=Get(_Q);
end;



{ --------------------------------------------------------------------
  TSeriesResource
  --------------------------------------------------------------------}


Class Function TSeriesResource.ResourceName : String;

begin
  Result:='series';
end;

Class Function TSeriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TSeriesResource.Get(AQuery : string = '') : TSeries;

Const
  _HTTPMethod = 'GET';
  _Path       = 'series/get';
  _Methodid   = 'books.series.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSeries) as TSeries;
end;


Function TSeriesResource.Get(AQuery : TSeriesgetOptions) : TSeries;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'series_id',AQuery.series_id);
  Result:=Get(_Q);
end;



Function TSeriesResource.GetMembershipInstance : TSeriesMembershipResource;

begin
  if (FMembershipInstance=Nil) then
    FMembershipInstance:=CreateMembershipResource;
  Result:=FMembershipInstance;
end;

Function TSeriesResource.CreateMembershipResource : TSeriesMembershipResource;

begin
  Result:=CreateMembershipResource(Self);
end;


Function TSeriesResource.CreateMembershipResource(AOwner : TComponent) : TSeriesMembershipResource;

begin
  Result:=TSeriesMembershipResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TVolumesAssociatedResource
  --------------------------------------------------------------------}


Class Function TVolumesAssociatedResource.ResourceName : String;

begin
  Result:='associated';
end;

Class Function TVolumesAssociatedResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TVolumesAssociatedResource.List(volumeId: string; AQuery : string = '') : TVolumes;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/{volumeId}/associated';
  _Methodid   = 'books.volumes.associated.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVolumes) as TVolumes;
end;


Function TVolumesAssociatedResource.List(volumeId: string; AQuery : TVolumesAssociatedlistOptions) : TVolumes;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'association',AQuery.association);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxAllowedMaturityRating',AQuery.maxAllowedMaturityRating);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=List(volumeId,_Q);
end;



{ --------------------------------------------------------------------
  TVolumesMybooksResource
  --------------------------------------------------------------------}


Class Function TVolumesMybooksResource.ResourceName : String;

begin
  Result:='mybooks';
end;

Class Function TVolumesMybooksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TVolumesMybooksResource.List(AQuery : string = '') : TVolumes;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/mybooks';
  _Methodid   = 'books.volumes.mybooks.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVolumes) as TVolumes;
end;


Function TVolumesMybooksResource.List(AQuery : TVolumesMybookslistOptions) : TVolumes;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'acquireMethod',AQuery.acquireMethod);
  AddToQuery(_Q,'country',AQuery.country);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'processingState',AQuery.processingState);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TVolumesRecommendedResource
  --------------------------------------------------------------------}


Class Function TVolumesRecommendedResource.ResourceName : String;

begin
  Result:='recommended';
end;

Class Function TVolumesRecommendedResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TVolumesRecommendedResource.List(AQuery : string = '') : TVolumes;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/recommended';
  _Methodid   = 'books.volumes.recommended.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVolumes) as TVolumes;
end;


Function TVolumesRecommendedResource.List(AQuery : TVolumesRecommendedlistOptions) : TVolumes;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxAllowedMaturityRating',AQuery.maxAllowedMaturityRating);
  AddToQuery(_Q,'source',AQuery.source);
  Result:=List(_Q);
end;

Function TVolumesRecommendedResource.Rate(AQuery : string = '') : TBooksVolumesRecommendedRateResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'volumes/recommended/rate';
  _Methodid   = 'books.volumes.recommended.rate';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TBooksVolumesRecommendedRateResponse) as TBooksVolumesRecommendedRateResponse;
end;


Function TVolumesRecommendedResource.Rate(AQuery : TVolumesRecommendedrateOptions) : TBooksVolumesRecommendedRateResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'rating',AQuery.rating);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  Result:=Rate(_Q);
end;



{ --------------------------------------------------------------------
  TVolumesUseruploadedResource
  --------------------------------------------------------------------}


Class Function TVolumesUseruploadedResource.ResourceName : String;

begin
  Result:='useruploaded';
end;

Class Function TVolumesUseruploadedResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TVolumesUseruploadedResource.List(AQuery : string = '') : TVolumes;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/useruploaded';
  _Methodid   = 'books.volumes.useruploaded.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVolumes) as TVolumes;
end;


Function TVolumesUseruploadedResource.List(AQuery : TVolumesUseruploadedlistOptions) : TVolumes;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'processingState',AQuery.processingState);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  AddToQuery(_Q,'volumeId',AQuery.volumeId);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TVolumesResource
  --------------------------------------------------------------------}


Class Function TVolumesResource.ResourceName : String;

begin
  Result:='volumes';
end;

Class Function TVolumesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbooksAPI;
end;

Function TVolumesResource.Get(volumeId: string; AQuery : string = '') : TVolume;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes/{volumeId}';
  _Methodid   = 'books.volumes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['volumeId',volumeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVolume) as TVolume;
end;


Function TVolumesResource.Get(volumeId: string; AQuery : TVolumesgetOptions) : TVolume;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'country',AQuery.country);
  AddToQuery(_Q,'includeNonComicsSeries',AQuery.includeNonComicsSeries);
  AddToQuery(_Q,'partner',AQuery.partner);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'user_library_consistent_read',AQuery.user_library_consistent_read);
  Result:=Get(volumeId,_Q);
end;

Function TVolumesResource.List(AQuery : string = '') : TVolumes;

Const
  _HTTPMethod = 'GET';
  _Path       = 'volumes';
  _Methodid   = 'books.volumes.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVolumes) as TVolumes;
end;


Function TVolumesResource.List(AQuery : TVolumeslistOptions) : TVolumes;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'download',AQuery.download);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'langRestrict',AQuery.langRestrict);
  AddToQuery(_Q,'libraryRestrict',AQuery.libraryRestrict);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'partner',AQuery.partner);
  AddToQuery(_Q,'printType',AQuery.printType);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'showPreorders',AQuery.showPreorders);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=List(_Q);
end;



Function TVolumesResource.GetAssociatedInstance : TVolumesAssociatedResource;

begin
  if (FAssociatedInstance=Nil) then
    FAssociatedInstance:=CreateAssociatedResource;
  Result:=FAssociatedInstance;
end;

Function TVolumesResource.CreateAssociatedResource : TVolumesAssociatedResource;

begin
  Result:=CreateAssociatedResource(Self);
end;


Function TVolumesResource.CreateAssociatedResource(AOwner : TComponent) : TVolumesAssociatedResource;

begin
  Result:=TVolumesAssociatedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TVolumesResource.GetMybooksInstance : TVolumesMybooksResource;

begin
  if (FMybooksInstance=Nil) then
    FMybooksInstance:=CreateMybooksResource;
  Result:=FMybooksInstance;
end;

Function TVolumesResource.CreateMybooksResource : TVolumesMybooksResource;

begin
  Result:=CreateMybooksResource(Self);
end;


Function TVolumesResource.CreateMybooksResource(AOwner : TComponent) : TVolumesMybooksResource;

begin
  Result:=TVolumesMybooksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TVolumesResource.GetRecommendedInstance : TVolumesRecommendedResource;

begin
  if (FRecommendedInstance=Nil) then
    FRecommendedInstance:=CreateRecommendedResource;
  Result:=FRecommendedInstance;
end;

Function TVolumesResource.CreateRecommendedResource : TVolumesRecommendedResource;

begin
  Result:=CreateRecommendedResource(Self);
end;


Function TVolumesResource.CreateRecommendedResource(AOwner : TComponent) : TVolumesRecommendedResource;

begin
  Result:=TVolumesRecommendedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TVolumesResource.GetUseruploadedInstance : TVolumesUseruploadedResource;

begin
  if (FUseruploadedInstance=Nil) then
    FUseruploadedInstance:=CreateUseruploadedResource;
  Result:=FUseruploadedInstance;
end;

Function TVolumesResource.CreateUseruploadedResource : TVolumesUseruploadedResource;

begin
  Result:=CreateUseruploadedResource(Self);
end;


Function TVolumesResource.CreateUseruploadedResource(AOwner : TComponent) : TVolumesUseruploadedResource;

begin
  Result:=TVolumesUseruploadedResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TBooksAPI
  --------------------------------------------------------------------}

Class Function TBooksAPI.APIName : String;

begin
  Result:='books';
end;

Class Function TBooksAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TBooksAPI.APIRevision : String;

begin
  Result:='20160411';
end;

Class Function TBooksAPI.APIID : String;

begin
  Result:='books:v1';
end;

Class Function TBooksAPI.APITitle : String;

begin
  Result:='Books API';
end;

Class Function TBooksAPI.APIDescription : String;

begin
  Result:='Lets you search for books and manage your Google Books library.';
end;

Class Function TBooksAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TBooksAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TBooksAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/ebooks-16.png';
end;

Class Function TBooksAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/ebooks-32.png';
end;

Class Function TBooksAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/books/docs/v1/getting_started';
end;

Class Function TBooksAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TBooksAPI.APIbasePath : string;

begin
  Result:='/books/v1/';
end;

Class Function TBooksAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/books/v1/';
end;

Class Function TBooksAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TBooksAPI.APIservicePath : string;

begin
  Result:='books/v1/';
end;

Class Function TBooksAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TBooksAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/books';
  Result[0].Description:='Manage your books';
  
end;

Class Function TBooksAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TBooksAPI.RegisterAPIResources;

begin
  TAnnotationTypeclientVersionRanges.RegisterObject;
  TAnnotationTypecurrentVersionRanges.RegisterObject;
  TAnnotationTypelayerSummary.RegisterObject;
  TAnnotation.RegisterObject;
  TAnnotationdata.RegisterObject;
  TAnnotations.RegisterObject;
  TAnnotationsSummaryTypelayersItem.RegisterObject;
  TAnnotationsSummary.RegisterObject;
  TAnnotationsdata.RegisterObject;
  TBooksAnnotationsRange.RegisterObject;
  TBooksCloudloadingResource.RegisterObject;
  TBooksVolumesRecommendedRateResponse.RegisterObject;
  TBookshelf.RegisterObject;
  TBookshelves.RegisterObject;
  TCategoryTypeitemsItem.RegisterObject;
  TCategory.RegisterObject;
  TConcurrentAccessRestriction.RegisterObject;
  TDictlayerdataTypecommon.RegisterObject;
  TDictlayerdataTypedictTypesource.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypederivativesItemTypesource.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypederivativesItem.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypeexamplesItemTypesource.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypeexamplesItem.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypeconjugationsItem.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItemTypesource.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItemTypeexamplesItem.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypedefinitionsItem.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesource.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItemTypesource.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesensesItemTypesynonymsItem.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesensesItem.RegisterObject;
  TDictlayerdataTypedictTypewordsItemTypesource.RegisterObject;
  TDictlayerdataTypedictTypewordsItem.RegisterObject;
  TDictlayerdataTypedict.RegisterObject;
  TDictlayerdata.RegisterObject;
  TDiscoveryclustersTypeclustersItemTypebanner_with_content_container.RegisterObject;
  TDiscoveryclustersTypeclustersItem.RegisterObject;
  TDiscoveryclusters.RegisterObject;
  TDownloadAccessRestriction.RegisterObject;
  TDownloadAccesses.RegisterObject;
  TGeolayerdataTypecommon.RegisterObject;
  TGeolayerdataTypegeoTypeboundaryItemItem.RegisterObject;
  TGeolayerdataTypegeoTypeviewportTypehi.RegisterObject;
  TGeolayerdataTypegeoTypeviewportTypelo.RegisterObject;
  TGeolayerdataTypegeoTypeviewport.RegisterObject;
  TGeolayerdataTypegeo.RegisterObject;
  TGeolayerdata.RegisterObject;
  TLayersummaries.RegisterObject;
  TLayersummary.RegisterObject;
  TMetadataTypeitemsItem.RegisterObject;
  TMetadata.RegisterObject;
  TNotification.RegisterObject;
  TOffersTypeitemsItemTypeitemsItem.RegisterObject;
  TOffersTypeitemsItem.RegisterObject;
  TOffers.RegisterObject;
  TReadingPosition.RegisterObject;
  TRequestAccess.RegisterObject;
  TReviewTypeauthor.RegisterObject;
  TReviewTypesource.RegisterObject;
  TReview.RegisterObject;
  TSeriesTypeseriesItem.RegisterObject;
  TSeries.RegisterObject;
  TSeriesmembership.RegisterObject;
  TUsersettingsTypenotesExport.RegisterObject;
  TUsersettingsTypenotificationTypemoreFromAuthors.RegisterObject;
  TUsersettingsTypenotification.RegisterObject;
  TUsersettings.RegisterObject;
  TVolumeTypeaccessInfoTypeepub.RegisterObject;
  TVolumeTypeaccessInfoTypepdf.RegisterObject;
  TVolumeTypeaccessInfo.RegisterObject;
  TVolumeTypelayerInfoTypelayersItem.RegisterObject;
  TVolumeTypelayerInfo.RegisterObject;
  TVolumeTyperecommendedInfo.RegisterObject;
  TVolumeTypesaleInfoTypelistPrice.RegisterObject;
  TVolumeTypesaleInfoTypeoffersItemTypelistPrice.RegisterObject;
  TVolumeTypesaleInfoTypeoffersItemTyperentalDuration.RegisterObject;
  TVolumeTypesaleInfoTypeoffersItemTyperetailPrice.RegisterObject;
  TVolumeTypesaleInfoTypeoffersItem.RegisterObject;
  TVolumeTypesaleInfoTyperetailPrice.RegisterObject;
  TVolumeTypesaleInfo.RegisterObject;
  TVolumeTypesearchInfo.RegisterObject;
  TVolumeTypeuserInfoTypecopy.RegisterObject;
  TVolumeTypeuserInfoTypefamilySharing.RegisterObject;
  TVolumeTypeuserInfoTyperentalPeriod.RegisterObject;
  TVolumeTypeuserInfoTypeuserUploadedVolumeInfo.RegisterObject;
  TVolumeTypeuserInfo.RegisterObject;
  TVolumeTypevolumeInfoTypedimensions.RegisterObject;
  TVolumeTypevolumeInfoTypeimageLinks.RegisterObject;
  TVolumeTypevolumeInfoTypeindustryIdentifiersItem.RegisterObject;
  TVolumeTypevolumeInfo.RegisterObject;
  TVolume.RegisterObject;
  TVolume2.RegisterObject;
  TVolumeannotationTypecontentRanges.RegisterObject;
  TVolumeannotation.RegisterObject;
  TVolumeannotations.RegisterObject;
  TVolumes.RegisterObject;
  TVolumeseriesinfoTypevolumeSeriesItemTypeissueItem.RegisterObject;
  TVolumeseriesinfoTypevolumeSeriesItem.RegisterObject;
  TVolumeseriesinfo.RegisterObject;
end;


Function TBooksAPI.GetBookshelvesVolumesInstance : TBookshelvesVolumesResource;

begin
  if (FBookshelvesVolumesInstance=Nil) then
    FBookshelvesVolumesInstance:=CreateBookshelvesVolumesResource;
  Result:=FBookshelvesVolumesInstance;
end;

Function TBooksAPI.CreateBookshelvesVolumesResource : TBookshelvesVolumesResource;

begin
  Result:=CreateBookshelvesVolumesResource(Self);
end;


Function TBooksAPI.CreateBookshelvesVolumesResource(AOwner : TComponent) : TBookshelvesVolumesResource;

begin
  Result:=TBookshelvesVolumesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetBookshelvesInstance : TBookshelvesResource;

begin
  if (FBookshelvesInstance=Nil) then
    FBookshelvesInstance:=CreateBookshelvesResource;
  Result:=FBookshelvesInstance;
end;

Function TBooksAPI.CreateBookshelvesResource : TBookshelvesResource;

begin
  Result:=CreateBookshelvesResource(Self);
end;


Function TBooksAPI.CreateBookshelvesResource(AOwner : TComponent) : TBookshelvesResource;

begin
  Result:=TBookshelvesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetCloudloadingInstance : TCloudloadingResource;

begin
  if (FCloudloadingInstance=Nil) then
    FCloudloadingInstance:=CreateCloudloadingResource;
  Result:=FCloudloadingInstance;
end;

Function TBooksAPI.CreateCloudloadingResource : TCloudloadingResource;

begin
  Result:=CreateCloudloadingResource(Self);
end;


Function TBooksAPI.CreateCloudloadingResource(AOwner : TComponent) : TCloudloadingResource;

begin
  Result:=TCloudloadingResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetDictionaryInstance : TDictionaryResource;

begin
  if (FDictionaryInstance=Nil) then
    FDictionaryInstance:=CreateDictionaryResource;
  Result:=FDictionaryInstance;
end;

Function TBooksAPI.CreateDictionaryResource : TDictionaryResource;

begin
  Result:=CreateDictionaryResource(Self);
end;


Function TBooksAPI.CreateDictionaryResource(AOwner : TComponent) : TDictionaryResource;

begin
  Result:=TDictionaryResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetLayersAnnotationDataInstance : TLayersAnnotationDataResource;

begin
  if (FLayersAnnotationDataInstance=Nil) then
    FLayersAnnotationDataInstance:=CreateLayersAnnotationDataResource;
  Result:=FLayersAnnotationDataInstance;
end;

Function TBooksAPI.CreateLayersAnnotationDataResource : TLayersAnnotationDataResource;

begin
  Result:=CreateLayersAnnotationDataResource(Self);
end;


Function TBooksAPI.CreateLayersAnnotationDataResource(AOwner : TComponent) : TLayersAnnotationDataResource;

begin
  Result:=TLayersAnnotationDataResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetLayersVolumeAnnotationsInstance : TLayersVolumeAnnotationsResource;

begin
  if (FLayersVolumeAnnotationsInstance=Nil) then
    FLayersVolumeAnnotationsInstance:=CreateLayersVolumeAnnotationsResource;
  Result:=FLayersVolumeAnnotationsInstance;
end;

Function TBooksAPI.CreateLayersVolumeAnnotationsResource : TLayersVolumeAnnotationsResource;

begin
  Result:=CreateLayersVolumeAnnotationsResource(Self);
end;


Function TBooksAPI.CreateLayersVolumeAnnotationsResource(AOwner : TComponent) : TLayersVolumeAnnotationsResource;

begin
  Result:=TLayersVolumeAnnotationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetLayersInstance : TLayersResource;

begin
  if (FLayersInstance=Nil) then
    FLayersInstance:=CreateLayersResource;
  Result:=FLayersInstance;
end;

Function TBooksAPI.CreateLayersResource : TLayersResource;

begin
  Result:=CreateLayersResource(Self);
end;


Function TBooksAPI.CreateLayersResource(AOwner : TComponent) : TLayersResource;

begin
  Result:=TLayersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetMyconfigInstance : TMyconfigResource;

begin
  if (FMyconfigInstance=Nil) then
    FMyconfigInstance:=CreateMyconfigResource;
  Result:=FMyconfigInstance;
end;

Function TBooksAPI.CreateMyconfigResource : TMyconfigResource;

begin
  Result:=CreateMyconfigResource(Self);
end;


Function TBooksAPI.CreateMyconfigResource(AOwner : TComponent) : TMyconfigResource;

begin
  Result:=TMyconfigResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetMylibraryAnnotationsInstance : TMylibraryAnnotationsResource;

begin
  if (FMylibraryAnnotationsInstance=Nil) then
    FMylibraryAnnotationsInstance:=CreateMylibraryAnnotationsResource;
  Result:=FMylibraryAnnotationsInstance;
end;

Function TBooksAPI.CreateMylibraryAnnotationsResource : TMylibraryAnnotationsResource;

begin
  Result:=CreateMylibraryAnnotationsResource(Self);
end;


Function TBooksAPI.CreateMylibraryAnnotationsResource(AOwner : TComponent) : TMylibraryAnnotationsResource;

begin
  Result:=TMylibraryAnnotationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetMylibraryBookshelvesVolumesInstance : TMylibraryBookshelvesVolumesResource;

begin
  if (FMylibraryBookshelvesVolumesInstance=Nil) then
    FMylibraryBookshelvesVolumesInstance:=CreateMylibraryBookshelvesVolumesResource;
  Result:=FMylibraryBookshelvesVolumesInstance;
end;

Function TBooksAPI.CreateMylibraryBookshelvesVolumesResource : TMylibraryBookshelvesVolumesResource;

begin
  Result:=CreateMylibraryBookshelvesVolumesResource(Self);
end;


Function TBooksAPI.CreateMylibraryBookshelvesVolumesResource(AOwner : TComponent) : TMylibraryBookshelvesVolumesResource;

begin
  Result:=TMylibraryBookshelvesVolumesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetMylibraryBookshelvesInstance : TMylibraryBookshelvesResource;

begin
  if (FMylibraryBookshelvesInstance=Nil) then
    FMylibraryBookshelvesInstance:=CreateMylibraryBookshelvesResource;
  Result:=FMylibraryBookshelvesInstance;
end;

Function TBooksAPI.CreateMylibraryBookshelvesResource : TMylibraryBookshelvesResource;

begin
  Result:=CreateMylibraryBookshelvesResource(Self);
end;


Function TBooksAPI.CreateMylibraryBookshelvesResource(AOwner : TComponent) : TMylibraryBookshelvesResource;

begin
  Result:=TMylibraryBookshelvesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetMylibraryReadingpositionsInstance : TMylibraryReadingpositionsResource;

begin
  if (FMylibraryReadingpositionsInstance=Nil) then
    FMylibraryReadingpositionsInstance:=CreateMylibraryReadingpositionsResource;
  Result:=FMylibraryReadingpositionsInstance;
end;

Function TBooksAPI.CreateMylibraryReadingpositionsResource : TMylibraryReadingpositionsResource;

begin
  Result:=CreateMylibraryReadingpositionsResource(Self);
end;


Function TBooksAPI.CreateMylibraryReadingpositionsResource(AOwner : TComponent) : TMylibraryReadingpositionsResource;

begin
  Result:=TMylibraryReadingpositionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetMylibraryInstance : TMylibraryResource;

begin
  if (FMylibraryInstance=Nil) then
    FMylibraryInstance:=CreateMylibraryResource;
  Result:=FMylibraryInstance;
end;

Function TBooksAPI.CreateMylibraryResource : TMylibraryResource;

begin
  Result:=CreateMylibraryResource(Self);
end;


Function TBooksAPI.CreateMylibraryResource(AOwner : TComponent) : TMylibraryResource;

begin
  Result:=TMylibraryResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetNotificationInstance : TNotificationResource;

begin
  if (FNotificationInstance=Nil) then
    FNotificationInstance:=CreateNotificationResource;
  Result:=FNotificationInstance;
end;

Function TBooksAPI.CreateNotificationResource : TNotificationResource;

begin
  Result:=CreateNotificationResource(Self);
end;


Function TBooksAPI.CreateNotificationResource(AOwner : TComponent) : TNotificationResource;

begin
  Result:=TNotificationResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetOnboardingInstance : TOnboardingResource;

begin
  if (FOnboardingInstance=Nil) then
    FOnboardingInstance:=CreateOnboardingResource;
  Result:=FOnboardingInstance;
end;

Function TBooksAPI.CreateOnboardingResource : TOnboardingResource;

begin
  Result:=CreateOnboardingResource(Self);
end;


Function TBooksAPI.CreateOnboardingResource(AOwner : TComponent) : TOnboardingResource;

begin
  Result:=TOnboardingResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetPersonalizedstreamInstance : TPersonalizedstreamResource;

begin
  if (FPersonalizedstreamInstance=Nil) then
    FPersonalizedstreamInstance:=CreatePersonalizedstreamResource;
  Result:=FPersonalizedstreamInstance;
end;

Function TBooksAPI.CreatePersonalizedstreamResource : TPersonalizedstreamResource;

begin
  Result:=CreatePersonalizedstreamResource(Self);
end;


Function TBooksAPI.CreatePersonalizedstreamResource(AOwner : TComponent) : TPersonalizedstreamResource;

begin
  Result:=TPersonalizedstreamResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetPromoofferInstance : TPromoofferResource;

begin
  if (FPromoofferInstance=Nil) then
    FPromoofferInstance:=CreatePromoofferResource;
  Result:=FPromoofferInstance;
end;

Function TBooksAPI.CreatePromoofferResource : TPromoofferResource;

begin
  Result:=CreatePromoofferResource(Self);
end;


Function TBooksAPI.CreatePromoofferResource(AOwner : TComponent) : TPromoofferResource;

begin
  Result:=TPromoofferResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetSeriesMembershipInstance : TSeriesMembershipResource;

begin
  if (FSeriesMembershipInstance=Nil) then
    FSeriesMembershipInstance:=CreateSeriesMembershipResource;
  Result:=FSeriesMembershipInstance;
end;

Function TBooksAPI.CreateSeriesMembershipResource : TSeriesMembershipResource;

begin
  Result:=CreateSeriesMembershipResource(Self);
end;


Function TBooksAPI.CreateSeriesMembershipResource(AOwner : TComponent) : TSeriesMembershipResource;

begin
  Result:=TSeriesMembershipResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetSeriesInstance : TSeriesResource;

begin
  if (FSeriesInstance=Nil) then
    FSeriesInstance:=CreateSeriesResource;
  Result:=FSeriesInstance;
end;

Function TBooksAPI.CreateSeriesResource : TSeriesResource;

begin
  Result:=CreateSeriesResource(Self);
end;


Function TBooksAPI.CreateSeriesResource(AOwner : TComponent) : TSeriesResource;

begin
  Result:=TSeriesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetVolumesAssociatedInstance : TVolumesAssociatedResource;

begin
  if (FVolumesAssociatedInstance=Nil) then
    FVolumesAssociatedInstance:=CreateVolumesAssociatedResource;
  Result:=FVolumesAssociatedInstance;
end;

Function TBooksAPI.CreateVolumesAssociatedResource : TVolumesAssociatedResource;

begin
  Result:=CreateVolumesAssociatedResource(Self);
end;


Function TBooksAPI.CreateVolumesAssociatedResource(AOwner : TComponent) : TVolumesAssociatedResource;

begin
  Result:=TVolumesAssociatedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetVolumesMybooksInstance : TVolumesMybooksResource;

begin
  if (FVolumesMybooksInstance=Nil) then
    FVolumesMybooksInstance:=CreateVolumesMybooksResource;
  Result:=FVolumesMybooksInstance;
end;

Function TBooksAPI.CreateVolumesMybooksResource : TVolumesMybooksResource;

begin
  Result:=CreateVolumesMybooksResource(Self);
end;


Function TBooksAPI.CreateVolumesMybooksResource(AOwner : TComponent) : TVolumesMybooksResource;

begin
  Result:=TVolumesMybooksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetVolumesRecommendedInstance : TVolumesRecommendedResource;

begin
  if (FVolumesRecommendedInstance=Nil) then
    FVolumesRecommendedInstance:=CreateVolumesRecommendedResource;
  Result:=FVolumesRecommendedInstance;
end;

Function TBooksAPI.CreateVolumesRecommendedResource : TVolumesRecommendedResource;

begin
  Result:=CreateVolumesRecommendedResource(Self);
end;


Function TBooksAPI.CreateVolumesRecommendedResource(AOwner : TComponent) : TVolumesRecommendedResource;

begin
  Result:=TVolumesRecommendedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetVolumesUseruploadedInstance : TVolumesUseruploadedResource;

begin
  if (FVolumesUseruploadedInstance=Nil) then
    FVolumesUseruploadedInstance:=CreateVolumesUseruploadedResource;
  Result:=FVolumesUseruploadedInstance;
end;

Function TBooksAPI.CreateVolumesUseruploadedResource : TVolumesUseruploadedResource;

begin
  Result:=CreateVolumesUseruploadedResource(Self);
end;


Function TBooksAPI.CreateVolumesUseruploadedResource(AOwner : TComponent) : TVolumesUseruploadedResource;

begin
  Result:=TVolumesUseruploadedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBooksAPI.GetVolumesInstance : TVolumesResource;

begin
  if (FVolumesInstance=Nil) then
    FVolumesInstance:=CreateVolumesResource;
  Result:=FVolumesInstance;
end;

Function TBooksAPI.CreateVolumesResource : TVolumesResource;

begin
  Result:=CreateVolumesResource(Self);
end;


Function TBooksAPI.CreateVolumesResource(AOwner : TComponent) : TVolumesResource;

begin
  Result:=TVolumesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TBooksAPI.RegisterAPI;
end.
