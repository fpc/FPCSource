unit googlebooks;
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
  TAnnotation = class;
  TAnnotationArray = Array of TAnnotation;
  TAnnotationclientVersionRanges = class;
  TAnnotationclientVersionRangesArray = Array of TAnnotationclientVersionRanges;
  TAnnotationcurrentVersionRanges = class;
  TAnnotationcurrentVersionRangesArray = Array of TAnnotationcurrentVersionRanges;
  TAnnotationlayerSummary = class;
  TAnnotationlayerSummaryArray = Array of TAnnotationlayerSummary;
  TAnnotationpageIds = class;
  TAnnotationpageIdsArray = Array of TAnnotationpageIds;
  TAnnotationdata = class;
  TAnnotationdataArray = Array of TAnnotationdata;
  TAnnotations = class;
  TAnnotationsArray = Array of TAnnotations;
  TAnnotationsitems = class;
  TAnnotationsitemsArray = Array of TAnnotationsitems;
  TAnnotationsSummary = class;
  TAnnotationsSummaryArray = Array of TAnnotationsSummary;
  TAnnotationsSummarylayers = class;
  TAnnotationsSummarylayersArray = Array of TAnnotationsSummarylayers;
  TAnnotationsdata = class;
  TAnnotationsdataArray = Array of TAnnotationsdata;
  TAnnotationsdataitems = class;
  TAnnotationsdataitemsArray = Array of TAnnotationsdataitems;
  TBooksAnnotationsRange = class;
  TBooksAnnotationsRangeArray = Array of TBooksAnnotationsRange;
  TBooksCloudloadingResource = class;
  TBooksCloudloadingResourceArray = Array of TBooksCloudloadingResource;
  TBooksVolumesRecommendedRateResponse = class;
  TBooksVolumesRecommendedRateResponseArray = Array of TBooksVolumesRecommendedRateResponse;
  TBookshelf = class;
  TBookshelfArray = Array of TBookshelf;
  TBookshelves = class;
  TBookshelvesArray = Array of TBookshelves;
  TBookshelvesitems = class;
  TBookshelvesitemsArray = Array of TBookshelvesitems;
  TCategory = class;
  TCategoryArray = Array of TCategory;
  TCategoryitems = class;
  TCategoryitemsArray = Array of TCategoryitems;
  TConcurrentAccessRestriction = class;
  TConcurrentAccessRestrictionArray = Array of TConcurrentAccessRestriction;
  TDictlayerdata = class;
  TDictlayerdataArray = Array of TDictlayerdata;
  TDictlayerdatacommon = class;
  TDictlayerdatacommonArray = Array of TDictlayerdatacommon;
  TDictlayerdatadict = class;
  TDictlayerdatadictArray = Array of TDictlayerdatadict;
  TDictlayerdatadictsource = class;
  TDictlayerdatadictsourceArray = Array of TDictlayerdatadictsource;
  TDictlayerdatadictwords = class;
  TDictlayerdatadictwordsArray = Array of TDictlayerdatadictwords;
  TDictlayerdatadictwordsderivatives = class;
  TDictlayerdatadictwordsderivativesArray = Array of TDictlayerdatadictwordsderivatives;
  TDictlayerdatadictwordsderivativessource = class;
  TDictlayerdatadictwordsderivativessourceArray = Array of TDictlayerdatadictwordsderivativessource;
  TDictlayerdatadictwordsexamples = class;
  TDictlayerdatadictwordsexamplesArray = Array of TDictlayerdatadictwordsexamples;
  TDictlayerdatadictwordsexamplessource = class;
  TDictlayerdatadictwordsexamplessourceArray = Array of TDictlayerdatadictwordsexamplessource;
  TDictlayerdatadictwordssenses = class;
  TDictlayerdatadictwordssensesArray = Array of TDictlayerdatadictwordssenses;
  TDictlayerdatadictwordssensesconjugations = class;
  TDictlayerdatadictwordssensesconjugationsArray = Array of TDictlayerdatadictwordssensesconjugations;
  TDictlayerdatadictwordssensesdefinitions = class;
  TDictlayerdatadictwordssensesdefinitionsArray = Array of TDictlayerdatadictwordssensesdefinitions;
  TDictlayerdatadictwordssensesdefinitionsexamples = class;
  TDictlayerdatadictwordssensesdefinitionsexamplesArray = Array of TDictlayerdatadictwordssensesdefinitionsexamples;
  TDictlayerdatadictwordssensesdefinitionsexamplessource = class;
  TDictlayerdatadictwordssensesdefinitionsexamplessourceArray = Array of TDictlayerdatadictwordssensesdefinitionsexamplessource;
  TDictlayerdatadictwordssensessource = class;
  TDictlayerdatadictwordssensessourceArray = Array of TDictlayerdatadictwordssensessource;
  TDictlayerdatadictwordssensessynonyms = class;
  TDictlayerdatadictwordssensessynonymsArray = Array of TDictlayerdatadictwordssensessynonyms;
  TDictlayerdatadictwordssensessynonymssource = class;
  TDictlayerdatadictwordssensessynonymssourceArray = Array of TDictlayerdatadictwordssensessynonymssource;
  TDictlayerdatadictwordssource = class;
  TDictlayerdatadictwordssourceArray = Array of TDictlayerdatadictwordssource;
  TDownloadAccessRestriction = class;
  TDownloadAccessRestrictionArray = Array of TDownloadAccessRestriction;
  TDownloadAccesses = class;
  TDownloadAccessesArray = Array of TDownloadAccesses;
  TDownloadAccessesdownloadAccessList = class;
  TDownloadAccessesdownloadAccessListArray = Array of TDownloadAccessesdownloadAccessList;
  TGeolayerdata = class;
  TGeolayerdataArray = Array of TGeolayerdata;
  TGeolayerdatacommon = class;
  TGeolayerdatacommonArray = Array of TGeolayerdatacommon;
  TGeolayerdatageo = class;
  TGeolayerdatageoArray = Array of TGeolayerdatageo;
  TGeolayerdatageoboundary = class;
  TGeolayerdatageoboundaryArray = Array of TGeolayerdatageoboundary;
  TGeolayerdatageoviewport = class;
  TGeolayerdatageoviewportArray = Array of TGeolayerdatageoviewport;
  TGeolayerdatageoviewporthi = class;
  TGeolayerdatageoviewporthiArray = Array of TGeolayerdatageoviewporthi;
  TGeolayerdatageoviewportlo = class;
  TGeolayerdatageoviewportloArray = Array of TGeolayerdatageoviewportlo;
  TLayersummaries = class;
  TLayersummariesArray = Array of TLayersummaries;
  TLayersummariesitems = class;
  TLayersummariesitemsArray = Array of TLayersummariesitems;
  TLayersummary = class;
  TLayersummaryArray = Array of TLayersummary;
  TLayersummaryannotationTypes = class;
  TLayersummaryannotationTypesArray = Array of TLayersummaryannotationTypes;
  TMetadata = class;
  TMetadataArray = Array of TMetadata;
  TMetadataitems = class;
  TMetadataitemsArray = Array of TMetadataitems;
  TOffers = class;
  TOffersArray = Array of TOffers;
  TOffersitems = class;
  TOffersitemsArray = Array of TOffersitems;
  TOffersitemsitems = class;
  TOffersitemsitemsArray = Array of TOffersitemsitems;
  TReadingPosition = class;
  TReadingPositionArray = Array of TReadingPosition;
  TRequestAccess = class;
  TRequestAccessArray = Array of TRequestAccess;
  TReview = class;
  TReviewArray = Array of TReview;
  TReviewauthor = class;
  TReviewauthorArray = Array of TReviewauthor;
  TReviewsource = class;
  TReviewsourceArray = Array of TReviewsource;
  TUsersettings = class;
  TUsersettingsArray = Array of TUsersettings;
  TUsersettingsnotesExport = class;
  TUsersettingsnotesExportArray = Array of TUsersettingsnotesExport;
  TVolume = class;
  TVolumeArray = Array of TVolume;
  TVolumeaccessInfo = class;
  TVolumeaccessInfoArray = Array of TVolumeaccessInfo;
  TVolumeaccessInfoepub = class;
  TVolumeaccessInfoepubArray = Array of TVolumeaccessInfoepub;
  TVolumeaccessInfopdf = class;
  TVolumeaccessInfopdfArray = Array of TVolumeaccessInfopdf;
  TVolumelayerInfo = class;
  TVolumelayerInfoArray = Array of TVolumelayerInfo;
  TVolumelayerInfolayers = class;
  TVolumelayerInfolayersArray = Array of TVolumelayerInfolayers;
  TVolumerecommendedInfo = class;
  TVolumerecommendedInfoArray = Array of TVolumerecommendedInfo;
  TVolumesaleInfo = class;
  TVolumesaleInfoArray = Array of TVolumesaleInfo;
  TVolumesaleInfolistPrice = class;
  TVolumesaleInfolistPriceArray = Array of TVolumesaleInfolistPrice;
  TVolumesaleInfooffers = class;
  TVolumesaleInfooffersArray = Array of TVolumesaleInfooffers;
  TVolumesaleInfoofferslistPrice = class;
  TVolumesaleInfoofferslistPriceArray = Array of TVolumesaleInfoofferslistPrice;
  TVolumesaleInfooffersrentalDuration = class;
  TVolumesaleInfooffersrentalDurationArray = Array of TVolumesaleInfooffersrentalDuration;
  TVolumesaleInfooffersretailPrice = class;
  TVolumesaleInfooffersretailPriceArray = Array of TVolumesaleInfooffersretailPrice;
  TVolumesaleInforetailPrice = class;
  TVolumesaleInforetailPriceArray = Array of TVolumesaleInforetailPrice;
  TVolumesearchInfo = class;
  TVolumesearchInfoArray = Array of TVolumesearchInfo;
  TVolumeuserInfo = class;
  TVolumeuserInfoArray = Array of TVolumeuserInfo;
  TVolumeuserInfocopy = class;
  TVolumeuserInfocopyArray = Array of TVolumeuserInfocopy;
  TVolumeuserInforentalPeriod = class;
  TVolumeuserInforentalPeriodArray = Array of TVolumeuserInforentalPeriod;
  TVolumeuserInfouserUploadedVolumeInfo = class;
  TVolumeuserInfouserUploadedVolumeInfoArray = Array of TVolumeuserInfouserUploadedVolumeInfo;
  TVolumevolumeInfo = class;
  TVolumevolumeInfoArray = Array of TVolumevolumeInfo;
  TVolumevolumeInfoauthors = class;
  TVolumevolumeInfoauthorsArray = Array of TVolumevolumeInfoauthors;
  TVolumevolumeInfocategories = class;
  TVolumevolumeInfocategoriesArray = Array of TVolumevolumeInfocategories;
  TVolumevolumeInfodimensions = class;
  TVolumevolumeInfodimensionsArray = Array of TVolumevolumeInfodimensions;
  TVolumevolumeInfoimageLinks = class;
  TVolumevolumeInfoimageLinksArray = Array of TVolumevolumeInfoimageLinks;
  TVolumevolumeInfoindustryIdentifiers = class;
  TVolumevolumeInfoindustryIdentifiersArray = Array of TVolumevolumeInfoindustryIdentifiers;
  TVolume2 = class;
  TVolume2Array = Array of TVolume2;
  TVolume2items = class;
  TVolume2itemsArray = Array of TVolume2items;
  TVolumeannotation = class;
  TVolumeannotationArray = Array of TVolumeannotation;
  TVolumeannotationcontentRanges = class;
  TVolumeannotationcontentRangesArray = Array of TVolumeannotationcontentRanges;
  TVolumeannotationpageIds = class;
  TVolumeannotationpageIdsArray = Array of TVolumeannotationpageIds;
  TVolumeannotations = class;
  TVolumeannotationsArray = Array of TVolumeannotations;
  TVolumeannotationsitems = class;
  TVolumeannotationsitemsArray = Array of TVolumeannotationsitems;
  TVolumes = class;
  TVolumesArray = Array of TVolumes;
  TVolumesitems = class;
  TVolumesitemsArray = Array of TVolumesitems;
  
  { --------------------------------------------------------------------
    TAnnotation
    --------------------------------------------------------------------}
  
  TAnnotation = Class(TGoogleBaseObject)
  Private
    FafterSelectedText : string;
    FbeforeSelectedText : string;
    FclientVersionRanges : TAnnotationclientVersionRanges;
    Fcreated : TDatetime;
    FcurrentVersionRanges : TAnnotationcurrentVersionRanges;
    Fdata : string;
    Fdeleted : boolean;
    FhighlightStyle : string;
    Fid : string;
    Fkind : string;
    FlayerId : string;
    FlayerSummary : TAnnotationlayerSummary;
    FpageIds : TAnnotationpageIds;
    FselectedText : string;
    FselfLink : string;
    Fupdated : TDatetime;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure SetafterSelectedText(AIndex : Integer; AValue : string); virtual;
    Procedure SetbeforeSelectedText(AIndex : Integer; AValue : string); virtual;
    Procedure SetclientVersionRanges(AIndex : Integer; AValue : TAnnotationclientVersionRanges); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcurrentVersionRanges(AIndex : Integer; AValue : TAnnotationcurrentVersionRanges); virtual;
    Procedure Setdata(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SethighlightStyle(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlayerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetlayerSummary(AIndex : Integer; AValue : TAnnotationlayerSummary); virtual;
    Procedure SetpageIds(AIndex : Integer; AValue : TAnnotationpageIds); virtual;
    Procedure SetselectedText(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property afterSelectedText : string Index 0 Read FafterSelectedText Write SetafterSelectedText;
    Property beforeSelectedText : string Index 8 Read FbeforeSelectedText Write SetbeforeSelectedText;
    Property clientVersionRanges : TAnnotationclientVersionRanges Index 16 Read FclientVersionRanges Write SetclientVersionRanges;
    Property created : TDatetime Index 24 Read Fcreated Write Setcreated;
    Property currentVersionRanges : TAnnotationcurrentVersionRanges Index 32 Read FcurrentVersionRanges Write SetcurrentVersionRanges;
    Property data : string Index 40 Read Fdata Write Setdata;
    Property deleted : boolean Index 48 Read Fdeleted Write Setdeleted;
    Property highlightStyle : string Index 56 Read FhighlightStyle Write SethighlightStyle;
    Property id : string Index 64 Read Fid Write Setid;
    Property kind : string Index 72 Read Fkind Write Setkind;
    Property layerId : string Index 80 Read FlayerId Write SetlayerId;
    Property layerSummary : TAnnotationlayerSummary Index 88 Read FlayerSummary Write SetlayerSummary;
    Property pageIds : TAnnotationpageIds Index 96 Read FpageIds Write SetpageIds;
    Property selectedText : string Index 104 Read FselectedText Write SetselectedText;
    Property selfLink : string Index 112 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 120 Read Fupdated Write Setupdated;
    Property volumeId : string Index 128 Read FvolumeId Write SetvolumeId;
  end;
  TAnnotationClass = Class of TAnnotation;
  
  { --------------------------------------------------------------------
    TAnnotationclientVersionRanges
    --------------------------------------------------------------------}
  
  TAnnotationclientVersionRanges = Class(TGoogleBaseObject)
  Private
    FcfiRange : TBooksAnnotationsRange;
    FcontentVersion : string;
    FgbImageRange : TBooksAnnotationsRange;
    FgbTextRange : TBooksAnnotationsRange;
    FimageCfiRange : TBooksAnnotationsRange;
  Protected
    //Property setters
    Procedure SetcfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
    Procedure SetcontentVersion(AIndex : Integer; AValue : string); virtual;
    Procedure SetgbImageRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
    Procedure SetgbTextRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
    Procedure SetimageCfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
  Public
  Published
    Property cfiRange : TBooksAnnotationsRange Index 0 Read FcfiRange Write SetcfiRange;
    Property contentVersion : string Index 8 Read FcontentVersion Write SetcontentVersion;
    Property gbImageRange : TBooksAnnotationsRange Index 16 Read FgbImageRange Write SetgbImageRange;
    Property gbTextRange : TBooksAnnotationsRange Index 24 Read FgbTextRange Write SetgbTextRange;
    Property imageCfiRange : TBooksAnnotationsRange Index 32 Read FimageCfiRange Write SetimageCfiRange;
  end;
  TAnnotationclientVersionRangesClass = Class of TAnnotationclientVersionRanges;
  
  { --------------------------------------------------------------------
    TAnnotationcurrentVersionRanges
    --------------------------------------------------------------------}
  
  TAnnotationcurrentVersionRanges = Class(TGoogleBaseObject)
  Private
    FcfiRange : TBooksAnnotationsRange;
    FcontentVersion : string;
    FgbImageRange : TBooksAnnotationsRange;
    FgbTextRange : TBooksAnnotationsRange;
    FimageCfiRange : TBooksAnnotationsRange;
  Protected
    //Property setters
    Procedure SetcfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
    Procedure SetcontentVersion(AIndex : Integer; AValue : string); virtual;
    Procedure SetgbImageRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
    Procedure SetgbTextRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
    Procedure SetimageCfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
  Public
  Published
    Property cfiRange : TBooksAnnotationsRange Index 0 Read FcfiRange Write SetcfiRange;
    Property contentVersion : string Index 8 Read FcontentVersion Write SetcontentVersion;
    Property gbImageRange : TBooksAnnotationsRange Index 16 Read FgbImageRange Write SetgbImageRange;
    Property gbTextRange : TBooksAnnotationsRange Index 24 Read FgbTextRange Write SetgbTextRange;
    Property imageCfiRange : TBooksAnnotationsRange Index 32 Read FimageCfiRange Write SetimageCfiRange;
  end;
  TAnnotationcurrentVersionRangesClass = Class of TAnnotationcurrentVersionRanges;
  
  { --------------------------------------------------------------------
    TAnnotationlayerSummary
    --------------------------------------------------------------------}
  
  TAnnotationlayerSummary = Class(TGoogleBaseObject)
  Private
    FallowedCharacterCount : integer;
    FlimitType : string;
    FremainingCharacterCount : integer;
  Protected
    //Property setters
    Procedure SetallowedCharacterCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetlimitType(AIndex : Integer; AValue : string); virtual;
    Procedure SetremainingCharacterCount(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property allowedCharacterCount : integer Index 0 Read FallowedCharacterCount Write SetallowedCharacterCount;
    Property limitType : string Index 8 Read FlimitType Write SetlimitType;
    Property remainingCharacterCount : integer Index 16 Read FremainingCharacterCount Write SetremainingCharacterCount;
  end;
  TAnnotationlayerSummaryClass = Class of TAnnotationlayerSummary;
  
  { --------------------------------------------------------------------
    TAnnotationpageIds
    --------------------------------------------------------------------}
  
  TAnnotationpageIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnnotationpageIdsClass = Class of TAnnotationpageIds;
  
  { --------------------------------------------------------------------
    TAnnotationdata
    --------------------------------------------------------------------}
  
  TAnnotationdata = Class(TGoogleBaseObject)
  Private
    FannotationType : string;
    Fdata : TJSONSchema;
    Fencoded_data : string;
    Fid : string;
    Fkind : string;
    FlayerId : string;
    FselfLink : string;
    Fupdated : TDatetime;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure SetannotationType(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TJSONSchema); virtual;
    Procedure Setencoded_data(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlayerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property annotationType : string Index 0 Read FannotationType Write SetannotationType;
    Property data : TJSONSchema Index 8 Read Fdata Write Setdata;
    Property encoded_data : string Index 16 Read Fencoded_data Write Setencoded_data;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property layerId : string Index 40 Read FlayerId Write SetlayerId;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
    Property volumeId : string Index 64 Read FvolumeId Write SetvolumeId;
  end;
  TAnnotationdataClass = Class of TAnnotationdata;
  
  { --------------------------------------------------------------------
    TAnnotations
    --------------------------------------------------------------------}
  
  TAnnotations = Class(TGoogleBaseObject)
  Private
    Fitems : TAnnotationsitems;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAnnotationsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TAnnotationsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TAnnotationsClass = Class of TAnnotations;
  
  { --------------------------------------------------------------------
    TAnnotationsitems
    --------------------------------------------------------------------}
  
  TAnnotationsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnnotationsitemsClass = Class of TAnnotationsitems;
  
  { --------------------------------------------------------------------
    TAnnotationsSummary
    --------------------------------------------------------------------}
  
  TAnnotationsSummary = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Flayers : TAnnotationsSummarylayers;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlayers(AIndex : Integer; AValue : TAnnotationsSummarylayers); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property layers : TAnnotationsSummarylayers Index 8 Read Flayers Write Setlayers;
  end;
  TAnnotationsSummaryClass = Class of TAnnotationsSummary;
  
  { --------------------------------------------------------------------
    TAnnotationsSummarylayers
    --------------------------------------------------------------------}
  
  TAnnotationsSummarylayers = Class(TGoogleBaseObject)
  Private
    FallowedCharacterCount : integer;
    FlayerId : string;
    FlimitType : string;
    FremainingCharacterCount : integer;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure SetallowedCharacterCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetlayerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetlimitType(AIndex : Integer; AValue : string); virtual;
    Procedure SetremainingCharacterCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property allowedCharacterCount : integer Index 0 Read FallowedCharacterCount Write SetallowedCharacterCount;
    Property layerId : string Index 8 Read FlayerId Write SetlayerId;
    Property limitType : string Index 16 Read FlimitType Write SetlimitType;
    Property remainingCharacterCount : integer Index 24 Read FremainingCharacterCount Write SetremainingCharacterCount;
    Property updated : TDatetime Index 32 Read Fupdated Write Setupdated;
  end;
  TAnnotationsSummarylayersClass = Class of TAnnotationsSummarylayers;
  
  { --------------------------------------------------------------------
    TAnnotationsdata
    --------------------------------------------------------------------}
  
  TAnnotationsdata = Class(TGoogleBaseObject)
  Private
    Fitems : TAnnotationsdataitems;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAnnotationsdataitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TAnnotationsdataitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TAnnotationsdataClass = Class of TAnnotationsdata;
  
  { --------------------------------------------------------------------
    TAnnotationsdataitems
    --------------------------------------------------------------------}
  
  TAnnotationsdataitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnnotationsdataitemsClass = Class of TAnnotationsdataitems;
  
  { --------------------------------------------------------------------
    TBooksAnnotationsRange
    --------------------------------------------------------------------}
  
  TBooksAnnotationsRange = Class(TGoogleBaseObject)
  Private
    FendOffset : string;
    FendPosition : string;
    FstartOffset : string;
    FstartPosition : string;
  Protected
    //Property setters
    Procedure SetendOffset(AIndex : Integer; AValue : string); virtual;
    Procedure SetendPosition(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartOffset(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartPosition(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property endOffset : string Index 0 Read FendOffset Write SetendOffset;
    Property endPosition : string Index 8 Read FendPosition Write SetendPosition;
    Property startOffset : string Index 16 Read FstartOffset Write SetstartOffset;
    Property startPosition : string Index 24 Read FstartPosition Write SetstartPosition;
  end;
  TBooksAnnotationsRangeClass = Class of TBooksAnnotationsRange;
  
  { --------------------------------------------------------------------
    TBooksCloudloadingResource
    --------------------------------------------------------------------}
  
  TBooksCloudloadingResource = Class(TGoogleBaseObject)
  Private
    Fauthor : string;
    FprocessingState : string;
    Ftitle : string;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : string); virtual;
    Procedure SetprocessingState(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property author : string Index 0 Read Fauthor Write Setauthor;
    Property processingState : string Index 8 Read FprocessingState Write SetprocessingState;
    Property title : string Index 16 Read Ftitle Write Settitle;
    Property volumeId : string Index 24 Read FvolumeId Write SetvolumeId;
  end;
  TBooksCloudloadingResourceClass = Class of TBooksCloudloadingResource;
  
  { --------------------------------------------------------------------
    TBooksVolumesRecommendedRateResponse
    --------------------------------------------------------------------}
  
  TBooksVolumesRecommendedRateResponse = Class(TGoogleBaseObject)
  Private
    Fconsistency_token : string;
  Protected
    //Property setters
    Procedure Setconsistency_token(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property consistency_token : string Index 0 Read Fconsistency_token Write Setconsistency_token;
  end;
  TBooksVolumesRecommendedRateResponseClass = Class of TBooksVolumesRecommendedRateResponse;
  
  { --------------------------------------------------------------------
    TBookshelf
    --------------------------------------------------------------------}
  
  TBookshelf = Class(TGoogleBaseObject)
  Private
    Faccess : string;
    Fcreated : TDatetime;
    Fdescription : string;
    Fid : integer;
    Fkind : string;
    FselfLink : string;
    Ftitle : string;
    Fupdated : TDatetime;
    FvolumeCount : integer;
    FvolumesLastUpdated : TDatetime;
  Protected
    //Property setters
    Procedure Setaccess(AIndex : Integer; AValue : string); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetvolumeCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetvolumesLastUpdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property access : string Index 0 Read Faccess Write Setaccess;
    Property created : TDatetime Index 8 Read Fcreated Write Setcreated;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : integer Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
    Property title : string Index 48 Read Ftitle Write Settitle;
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
    Fitems : TBookshelvesitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBookshelvesitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TBookshelvesitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TBookshelvesClass = Class of TBookshelves;
  
  { --------------------------------------------------------------------
    TBookshelvesitems
    --------------------------------------------------------------------}
  
  TBookshelvesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBookshelvesitemsClass = Class of TBookshelvesitems;
  
  { --------------------------------------------------------------------
    TCategory
    --------------------------------------------------------------------}
  
  TCategory = Class(TGoogleBaseObject)
  Private
    Fitems : TCategoryitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCategoryitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCategoryitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TCategoryClass = Class of TCategory;
  
  { --------------------------------------------------------------------
    TCategoryitems
    --------------------------------------------------------------------}
  
  TCategoryitems = Class(TGoogleBaseObject)
  Private
    FbadgeUrl : string;
    FcategoryId : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetbadgeUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetcategoryId(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property badgeUrl : string Index 0 Read FbadgeUrl Write SetbadgeUrl;
    Property categoryId : string Index 8 Read FcategoryId Write SetcategoryId;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TCategoryitemsClass = Class of TCategoryitems;
  
  { --------------------------------------------------------------------
    TConcurrentAccessRestriction
    --------------------------------------------------------------------}
  
  TConcurrentAccessRestriction = Class(TGoogleBaseObject)
  Private
    FdeviceAllowed : boolean;
    Fkind : string;
    FmaxConcurrentDevices : integer;
    Fmessage : string;
    Fnonce : string;
    FreasonCode : string;
    Frestricted : boolean;
    Fsignature : string;
    Fsource : string;
    FtimeWindowSeconds : integer;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure SetdeviceAllowed(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxConcurrentDevices(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setnonce(AIndex : Integer; AValue : string); virtual;
    Procedure SetreasonCode(AIndex : Integer; AValue : string); virtual;
    Procedure Setrestricted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setsignature(AIndex : Integer; AValue : string); virtual;
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeWindowSeconds(AIndex : Integer; AValue : integer); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property deviceAllowed : boolean Index 0 Read FdeviceAllowed Write SetdeviceAllowed;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property maxConcurrentDevices : integer Index 16 Read FmaxConcurrentDevices Write SetmaxConcurrentDevices;
    Property message : string Index 24 Read Fmessage Write Setmessage;
    Property nonce : string Index 32 Read Fnonce Write Setnonce;
    Property reasonCode : string Index 40 Read FreasonCode Write SetreasonCode;
    Property restricted : boolean Index 48 Read Frestricted Write Setrestricted;
    Property signature : string Index 56 Read Fsignature Write Setsignature;
    Property source : string Index 64 Read Fsource Write Setsource;
    Property timeWindowSeconds : integer Index 72 Read FtimeWindowSeconds Write SettimeWindowSeconds;
    Property volumeId : string Index 80 Read FvolumeId Write SetvolumeId;
  end;
  TConcurrentAccessRestrictionClass = Class of TConcurrentAccessRestriction;
  
  { --------------------------------------------------------------------
    TDictlayerdata
    --------------------------------------------------------------------}
  
  TDictlayerdata = Class(TGoogleBaseObject)
  Private
    Fcommon : TDictlayerdatacommon;
    Fdict : TDictlayerdatadict;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setcommon(AIndex : Integer; AValue : TDictlayerdatacommon); virtual;
    Procedure Setdict(AIndex : Integer; AValue : TDictlayerdatadict); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property common : TDictlayerdatacommon Index 0 Read Fcommon Write Setcommon;
    Property dict : TDictlayerdatadict Index 8 Read Fdict Write Setdict;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TDictlayerdataClass = Class of TDictlayerdata;
  
  { --------------------------------------------------------------------
    TDictlayerdatacommon
    --------------------------------------------------------------------}
  
  TDictlayerdatacommon = Class(TGoogleBaseObject)
  Private
    Ftitle : string;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property title : string Index 0 Read Ftitle Write Settitle;
  end;
  TDictlayerdatacommonClass = Class of TDictlayerdatacommon;
  
  { --------------------------------------------------------------------
    TDictlayerdatadict
    --------------------------------------------------------------------}
  
  TDictlayerdatadict = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdatadictsource;
    Fwords : TDictlayerdatadictwords;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : TDictlayerdatadictsource); virtual;
    Procedure Setwords(AIndex : Integer; AValue : TDictlayerdatadictwords); virtual;
  Public
  Published
    Property source : TDictlayerdatadictsource Index 0 Read Fsource Write Setsource;
    Property words : TDictlayerdatadictwords Index 8 Read Fwords Write Setwords;
  end;
  TDictlayerdatadictClass = Class of TDictlayerdatadict;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictsource
    --------------------------------------------------------------------}
  
  TDictlayerdatadictsource = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdatadictsourceClass = Class of TDictlayerdatadictsource;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwords
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwords = Class(TGoogleBaseObject)
  Private
    Fderivatives : TDictlayerdatadictwordsderivatives;
    Fexamples : TDictlayerdatadictwordsexamples;
    Fsenses : TDictlayerdatadictwordssenses;
    Fsource : TDictlayerdatadictwordssource;
  Protected
    //Property setters
    Procedure Setderivatives(AIndex : Integer; AValue : TDictlayerdatadictwordsderivatives); virtual;
    Procedure Setexamples(AIndex : Integer; AValue : TDictlayerdatadictwordsexamples); virtual;
    Procedure Setsenses(AIndex : Integer; AValue : TDictlayerdatadictwordssenses); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordssource); virtual;
  Public
  Published
    Property derivatives : TDictlayerdatadictwordsderivatives Index 0 Read Fderivatives Write Setderivatives;
    Property examples : TDictlayerdatadictwordsexamples Index 8 Read Fexamples Write Setexamples;
    Property senses : TDictlayerdatadictwordssenses Index 16 Read Fsenses Write Setsenses;
    Property source : TDictlayerdatadictwordssource Index 24 Read Fsource Write Setsource;
  end;
  TDictlayerdatadictwordsClass = Class of TDictlayerdatadictwords;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordsderivatives
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordsderivatives = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdatadictwordsderivativessource;
    Ftext : string;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordsderivativessource); virtual;
    Procedure Settext(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property source : TDictlayerdatadictwordsderivativessource Index 0 Read Fsource Write Setsource;
    Property text : string Index 8 Read Ftext Write Settext;
  end;
  TDictlayerdatadictwordsderivativesClass = Class of TDictlayerdatadictwordsderivatives;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordsderivativessource
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordsderivativessource = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdatadictwordsderivativessourceClass = Class of TDictlayerdatadictwordsderivativessource;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordsexamples
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordsexamples = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdatadictwordsexamplessource;
    Ftext : string;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordsexamplessource); virtual;
    Procedure Settext(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property source : TDictlayerdatadictwordsexamplessource Index 0 Read Fsource Write Setsource;
    Property text : string Index 8 Read Ftext Write Settext;
  end;
  TDictlayerdatadictwordsexamplesClass = Class of TDictlayerdatadictwordsexamples;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordsexamplessource
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordsexamplessource = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdatadictwordsexamplessourceClass = Class of TDictlayerdatadictwordsexamplessource;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssenses
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssenses = Class(TGoogleBaseObject)
  Private
    Fconjugations : TDictlayerdatadictwordssensesconjugations;
    Fdefinitions : TDictlayerdatadictwordssensesdefinitions;
    FpartOfSpeech : string;
    Fpronunciation : string;
    FpronunciationUrl : string;
    Fsource : TDictlayerdatadictwordssensessource;
    Fsyllabification : string;
    Fsynonyms : TDictlayerdatadictwordssensessynonyms;
  Protected
    //Property setters
    Procedure Setconjugations(AIndex : Integer; AValue : TDictlayerdatadictwordssensesconjugations); virtual;
    Procedure Setdefinitions(AIndex : Integer; AValue : TDictlayerdatadictwordssensesdefinitions); virtual;
    Procedure SetpartOfSpeech(AIndex : Integer; AValue : string); virtual;
    Procedure Setpronunciation(AIndex : Integer; AValue : string); virtual;
    Procedure SetpronunciationUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordssensessource); virtual;
    Procedure Setsyllabification(AIndex : Integer; AValue : string); virtual;
    Procedure Setsynonyms(AIndex : Integer; AValue : TDictlayerdatadictwordssensessynonyms); virtual;
  Public
  Published
    Property conjugations : TDictlayerdatadictwordssensesconjugations Index 0 Read Fconjugations Write Setconjugations;
    Property definitions : TDictlayerdatadictwordssensesdefinitions Index 8 Read Fdefinitions Write Setdefinitions;
    Property partOfSpeech : string Index 16 Read FpartOfSpeech Write SetpartOfSpeech;
    Property pronunciation : string Index 24 Read Fpronunciation Write Setpronunciation;
    Property pronunciationUrl : string Index 32 Read FpronunciationUrl Write SetpronunciationUrl;
    Property source : TDictlayerdatadictwordssensessource Index 40 Read Fsource Write Setsource;
    Property syllabification : string Index 48 Read Fsyllabification Write Setsyllabification;
    Property synonyms : TDictlayerdatadictwordssensessynonyms Index 56 Read Fsynonyms Write Setsynonyms;
  end;
  TDictlayerdatadictwordssensesClass = Class of TDictlayerdatadictwordssenses;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssensesconjugations
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssensesconjugations = Class(TGoogleBaseObject)
  Private
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _type : string Index 0 Read F_type Write Set_type;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TDictlayerdatadictwordssensesconjugationsClass = Class of TDictlayerdatadictwordssensesconjugations;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssensesdefinitions
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssensesdefinitions = Class(TGoogleBaseObject)
  Private
    Fdefinition : string;
    Fexamples : TDictlayerdatadictwordssensesdefinitionsexamples;
  Protected
    //Property setters
    Procedure Setdefinition(AIndex : Integer; AValue : string); virtual;
    Procedure Setexamples(AIndex : Integer; AValue : TDictlayerdatadictwordssensesdefinitionsexamples); virtual;
  Public
  Published
    Property definition : string Index 0 Read Fdefinition Write Setdefinition;
    Property examples : TDictlayerdatadictwordssensesdefinitionsexamples Index 8 Read Fexamples Write Setexamples;
  end;
  TDictlayerdatadictwordssensesdefinitionsClass = Class of TDictlayerdatadictwordssensesdefinitions;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssensesdefinitionsexamples
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssensesdefinitionsexamples = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdatadictwordssensesdefinitionsexamplessource;
    Ftext : string;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordssensesdefinitionsexamplessource); virtual;
    Procedure Settext(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property source : TDictlayerdatadictwordssensesdefinitionsexamplessource Index 0 Read Fsource Write Setsource;
    Property text : string Index 8 Read Ftext Write Settext;
  end;
  TDictlayerdatadictwordssensesdefinitionsexamplesClass = Class of TDictlayerdatadictwordssensesdefinitionsexamples;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssensesdefinitionsexamplessource
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssensesdefinitionsexamplessource = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdatadictwordssensesdefinitionsexamplessourceClass = Class of TDictlayerdatadictwordssensesdefinitionsexamplessource;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssensessource
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssensessource = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdatadictwordssensessourceClass = Class of TDictlayerdatadictwordssensessource;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssensessynonyms
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssensessynonyms = Class(TGoogleBaseObject)
  Private
    Fsource : TDictlayerdatadictwordssensessynonymssource;
    Ftext : string;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordssensessynonymssource); virtual;
    Procedure Settext(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property source : TDictlayerdatadictwordssensessynonymssource Index 0 Read Fsource Write Setsource;
    Property text : string Index 8 Read Ftext Write Settext;
  end;
  TDictlayerdatadictwordssensessynonymsClass = Class of TDictlayerdatadictwordssensessynonyms;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssensessynonymssource
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssensessynonymssource = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdatadictwordssensessynonymssourceClass = Class of TDictlayerdatadictwordssensessynonymssource;
  
  { --------------------------------------------------------------------
    TDictlayerdatadictwordssource
    --------------------------------------------------------------------}
  
  TDictlayerdatadictwordssource = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TDictlayerdatadictwordssourceClass = Class of TDictlayerdatadictwordssource;
  
  { --------------------------------------------------------------------
    TDownloadAccessRestriction
    --------------------------------------------------------------------}
  
  TDownloadAccessRestriction = Class(TGoogleBaseObject)
  Private
    FdeviceAllowed : boolean;
    FdownloadsAcquired : integer;
    FjustAcquired : boolean;
    Fkind : string;
    FmaxDownloadDevices : integer;
    Fmessage : string;
    Fnonce : string;
    FreasonCode : string;
    Frestricted : boolean;
    Fsignature : string;
    Fsource : string;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure SetdeviceAllowed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdownloadsAcquired(AIndex : Integer; AValue : integer); virtual;
    Procedure SetjustAcquired(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxDownloadDevices(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setnonce(AIndex : Integer; AValue : string); virtual;
    Procedure SetreasonCode(AIndex : Integer; AValue : string); virtual;
    Procedure Setrestricted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setsignature(AIndex : Integer; AValue : string); virtual;
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property deviceAllowed : boolean Index 0 Read FdeviceAllowed Write SetdeviceAllowed;
    Property downloadsAcquired : integer Index 8 Read FdownloadsAcquired Write SetdownloadsAcquired;
    Property justAcquired : boolean Index 16 Read FjustAcquired Write SetjustAcquired;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property maxDownloadDevices : integer Index 32 Read FmaxDownloadDevices Write SetmaxDownloadDevices;
    Property message : string Index 40 Read Fmessage Write Setmessage;
    Property nonce : string Index 48 Read Fnonce Write Setnonce;
    Property reasonCode : string Index 56 Read FreasonCode Write SetreasonCode;
    Property restricted : boolean Index 64 Read Frestricted Write Setrestricted;
    Property signature : string Index 72 Read Fsignature Write Setsignature;
    Property source : string Index 80 Read Fsource Write Setsource;
    Property volumeId : string Index 88 Read FvolumeId Write SetvolumeId;
  end;
  TDownloadAccessRestrictionClass = Class of TDownloadAccessRestriction;
  
  { --------------------------------------------------------------------
    TDownloadAccesses
    --------------------------------------------------------------------}
  
  TDownloadAccesses = Class(TGoogleBaseObject)
  Private
    FdownloadAccessList : TDownloadAccessesdownloadAccessList;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetdownloadAccessList(AIndex : Integer; AValue : TDownloadAccessesdownloadAccessList); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property downloadAccessList : TDownloadAccessesdownloadAccessList Index 0 Read FdownloadAccessList Write SetdownloadAccessList;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TDownloadAccessesClass = Class of TDownloadAccesses;
  
  { --------------------------------------------------------------------
    TDownloadAccessesdownloadAccessList
    --------------------------------------------------------------------}
  
  TDownloadAccessesdownloadAccessList = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDownloadAccessesdownloadAccessListClass = Class of TDownloadAccessesdownloadAccessList;
  
  { --------------------------------------------------------------------
    TGeolayerdata
    --------------------------------------------------------------------}
  
  TGeolayerdata = Class(TGoogleBaseObject)
  Private
    Fcommon : TGeolayerdatacommon;
    Fgeo : TGeolayerdatageo;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setcommon(AIndex : Integer; AValue : TGeolayerdatacommon); virtual;
    Procedure Setgeo(AIndex : Integer; AValue : TGeolayerdatageo); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property common : TGeolayerdatacommon Index 0 Read Fcommon Write Setcommon;
    Property geo : TGeolayerdatageo Index 8 Read Fgeo Write Setgeo;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TGeolayerdataClass = Class of TGeolayerdata;
  
  { --------------------------------------------------------------------
    TGeolayerdatacommon
    --------------------------------------------------------------------}
  
  TGeolayerdatacommon = Class(TGoogleBaseObject)
  Private
    Flang : string;
    FpreviewImageUrl : string;
    Fsnippet : string;
    FsnippetUrl : string;
    Ftitle : string;
  Protected
    //Property setters
    Procedure Setlang(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviewImageUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : string); virtual;
    Procedure SetsnippetUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property lang : string Index 0 Read Flang Write Setlang;
    Property previewImageUrl : string Index 8 Read FpreviewImageUrl Write SetpreviewImageUrl;
    Property snippet : string Index 16 Read Fsnippet Write Setsnippet;
    Property snippetUrl : string Index 24 Read FsnippetUrl Write SetsnippetUrl;
    Property title : string Index 32 Read Ftitle Write Settitle;
  end;
  TGeolayerdatacommonClass = Class of TGeolayerdatacommon;
  
  { --------------------------------------------------------------------
    TGeolayerdatageo
    --------------------------------------------------------------------}
  
  TGeolayerdatageo = Class(TGoogleBaseObject)
  Private
    Fboundary : TGeolayerdatageoboundary;
    FcachePolicy : string;
    FcountryCode : string;
    Flatitude : double;
    Flongitude : double;
    FmapType : string;
    Fviewport : TGeolayerdatageoviewport;
    Fzoom : integer;
  Protected
    //Property setters
    Procedure Setboundary(AIndex : Integer; AValue : TGeolayerdatageoboundary); virtual;
    Procedure SetcachePolicy(AIndex : Integer; AValue : string); virtual;
    Procedure SetcountryCode(AIndex : Integer; AValue : string); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
    Procedure SetmapType(AIndex : Integer; AValue : string); virtual;
    Procedure Setviewport(AIndex : Integer; AValue : TGeolayerdatageoviewport); virtual;
    Procedure Setzoom(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property boundary : TGeolayerdatageoboundary Index 0 Read Fboundary Write Setboundary;
    Property cachePolicy : string Index 8 Read FcachePolicy Write SetcachePolicy;
    Property countryCode : string Index 16 Read FcountryCode Write SetcountryCode;
    Property latitude : double Index 24 Read Flatitude Write Setlatitude;
    Property longitude : double Index 32 Read Flongitude Write Setlongitude;
    Property mapType : string Index 40 Read FmapType Write SetmapType;
    Property viewport : TGeolayerdatageoviewport Index 48 Read Fviewport Write Setviewport;
    Property zoom : integer Index 56 Read Fzoom Write Setzoom;
  end;
  TGeolayerdatageoClass = Class of TGeolayerdatageo;
  
  { --------------------------------------------------------------------
    TGeolayerdatageoboundary
    --------------------------------------------------------------------}
  
  TGeolayerdatageoboundary = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeolayerdatageoboundaryClass = Class of TGeolayerdatageoboundary;
  
  { --------------------------------------------------------------------
    TGeolayerdatageoviewport
    --------------------------------------------------------------------}
  
  TGeolayerdatageoviewport = Class(TGoogleBaseObject)
  Private
    Fhi : TGeolayerdatageoviewporthi;
    Flo : TGeolayerdatageoviewportlo;
  Protected
    //Property setters
    Procedure Sethi(AIndex : Integer; AValue : TGeolayerdatageoviewporthi); virtual;
    Procedure Setlo(AIndex : Integer; AValue : TGeolayerdatageoviewportlo); virtual;
  Public
  Published
    Property hi : TGeolayerdatageoviewporthi Index 0 Read Fhi Write Sethi;
    Property lo : TGeolayerdatageoviewportlo Index 8 Read Flo Write Setlo;
  end;
  TGeolayerdatageoviewportClass = Class of TGeolayerdatageoviewport;
  
  { --------------------------------------------------------------------
    TGeolayerdatageoviewporthi
    --------------------------------------------------------------------}
  
  TGeolayerdatageoviewporthi = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TGeolayerdatageoviewporthiClass = Class of TGeolayerdatageoviewporthi;
  
  { --------------------------------------------------------------------
    TGeolayerdatageoviewportlo
    --------------------------------------------------------------------}
  
  TGeolayerdatageoviewportlo = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TGeolayerdatageoviewportloClass = Class of TGeolayerdatageoviewportlo;
  
  { --------------------------------------------------------------------
    TLayersummaries
    --------------------------------------------------------------------}
  
  TLayersummaries = Class(TGoogleBaseObject)
  Private
    Fitems : TLayersummariesitems;
    Fkind : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLayersummariesitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TLayersummariesitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property totalItems : integer Index 16 Read FtotalItems Write SettotalItems;
  end;
  TLayersummariesClass = Class of TLayersummaries;
  
  { --------------------------------------------------------------------
    TLayersummariesitems
    --------------------------------------------------------------------}
  
  TLayersummariesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLayersummariesitemsClass = Class of TLayersummariesitems;
  
  { --------------------------------------------------------------------
    TLayersummary
    --------------------------------------------------------------------}
  
  TLayersummary = Class(TGoogleBaseObject)
  Private
    FannotationCount : integer;
    FannotationTypes : TLayersummaryannotationTypes;
    FannotationsDataLink : string;
    FannotationsLink : string;
    FcontentVersion : string;
    FdataCount : integer;
    Fid : string;
    Fkind : string;
    FlayerId : string;
    FselfLink : string;
    Fupdated : TDatetime;
    FvolumeAnnotationsVersion : string;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure SetannotationCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetannotationTypes(AIndex : Integer; AValue : TLayersummaryannotationTypes); virtual;
    Procedure SetannotationsDataLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetannotationsLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentVersion(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlayerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetvolumeAnnotationsVersion(AIndex : Integer; AValue : string); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property annotationCount : integer Index 0 Read FannotationCount Write SetannotationCount;
    Property annotationTypes : TLayersummaryannotationTypes Index 8 Read FannotationTypes Write SetannotationTypes;
    Property annotationsDataLink : string Index 16 Read FannotationsDataLink Write SetannotationsDataLink;
    Property annotationsLink : string Index 24 Read FannotationsLink Write SetannotationsLink;
    Property contentVersion : string Index 32 Read FcontentVersion Write SetcontentVersion;
    Property dataCount : integer Index 40 Read FdataCount Write SetdataCount;
    Property id : string Index 48 Read Fid Write Setid;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property layerId : string Index 64 Read FlayerId Write SetlayerId;
    Property selfLink : string Index 72 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
    Property volumeAnnotationsVersion : string Index 88 Read FvolumeAnnotationsVersion Write SetvolumeAnnotationsVersion;
    Property volumeId : string Index 96 Read FvolumeId Write SetvolumeId;
  end;
  TLayersummaryClass = Class of TLayersummary;
  
  { --------------------------------------------------------------------
    TLayersummaryannotationTypes
    --------------------------------------------------------------------}
  
  TLayersummaryannotationTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLayersummaryannotationTypesClass = Class of TLayersummaryannotationTypes;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    Fitems : TMetadataitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TMetadataitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TMetadataitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TMetadataitems
    --------------------------------------------------------------------}
  
  TMetadataitems = Class(TGoogleBaseObject)
  Private
    Fdownload_url : string;
    Fencrypted_key : string;
    Flanguage : string;
    Fsize : string;
    Fversion : string;
  Protected
    //Property setters
    Procedure Setdownload_url(AIndex : Integer; AValue : string); virtual;
    Procedure Setencrypted_key(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure Setsize(AIndex : Integer; AValue : string); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property download_url : string Index 0 Read Fdownload_url Write Setdownload_url;
    Property encrypted_key : string Index 8 Read Fencrypted_key Write Setencrypted_key;
    Property language : string Index 16 Read Flanguage Write Setlanguage;
    Property size : string Index 24 Read Fsize Write Setsize;
    Property version : string Index 32 Read Fversion Write Setversion;
  end;
  TMetadataitemsClass = Class of TMetadataitems;
  
  { --------------------------------------------------------------------
    TOffers
    --------------------------------------------------------------------}
  
  TOffers = Class(TGoogleBaseObject)
  Private
    Fitems : TOffersitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TOffersitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TOffersitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TOffersClass = Class of TOffers;
  
  { --------------------------------------------------------------------
    TOffersitems
    --------------------------------------------------------------------}
  
  TOffersitems = Class(TGoogleBaseObject)
  Private
    FartUrl : string;
    FgservicesKey : string;
    Fid : string;
    Fitems : TOffersitemsitems;
  Protected
    //Property setters
    Procedure SetartUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetgservicesKey(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOffersitemsitems); virtual;
  Public
  Published
    Property artUrl : string Index 0 Read FartUrl Write SetartUrl;
    Property gservicesKey : string Index 8 Read FgservicesKey Write SetgservicesKey;
    Property id : string Index 16 Read Fid Write Setid;
    Property items : TOffersitemsitems Index 24 Read Fitems Write Setitems;
  end;
  TOffersitemsClass = Class of TOffersitems;
  
  { --------------------------------------------------------------------
    TOffersitemsitems
    --------------------------------------------------------------------}
  
  TOffersitemsitems = Class(TGoogleBaseObject)
  Private
    Fauthor : string;
    FcanonicalVolumeLink : string;
    FcoverUrl : string;
    Fdescription : string;
    Ftitle : string;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : string); virtual;
    Procedure SetcanonicalVolumeLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetcoverUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property author : string Index 0 Read Fauthor Write Setauthor;
    Property canonicalVolumeLink : string Index 8 Read FcanonicalVolumeLink Write SetcanonicalVolumeLink;
    Property coverUrl : string Index 16 Read FcoverUrl Write SetcoverUrl;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property title : string Index 32 Read Ftitle Write Settitle;
    Property volumeId : string Index 40 Read FvolumeId Write SetvolumeId;
  end;
  TOffersitemsitemsClass = Class of TOffersitemsitems;
  
  { --------------------------------------------------------------------
    TReadingPosition
    --------------------------------------------------------------------}
  
  TReadingPosition = Class(TGoogleBaseObject)
  Private
    FepubCfiPosition : string;
    FgbImagePosition : string;
    FgbTextPosition : string;
    Fkind : string;
    FpdfPosition : string;
    Fupdated : TDatetime;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure SetepubCfiPosition(AIndex : Integer; AValue : string); virtual;
    Procedure SetgbImagePosition(AIndex : Integer; AValue : string); virtual;
    Procedure SetgbTextPosition(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetpdfPosition(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property epubCfiPosition : string Index 0 Read FepubCfiPosition Write SetepubCfiPosition;
    Property gbImagePosition : string Index 8 Read FgbImagePosition Write SetgbImagePosition;
    Property gbTextPosition : string Index 16 Read FgbTextPosition Write SetgbTextPosition;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property pdfPosition : string Index 32 Read FpdfPosition Write SetpdfPosition;
    Property updated : TDatetime Index 40 Read Fupdated Write Setupdated;
    Property volumeId : string Index 48 Read FvolumeId Write SetvolumeId;
  end;
  TReadingPositionClass = Class of TReadingPosition;
  
  { --------------------------------------------------------------------
    TRequestAccess
    --------------------------------------------------------------------}
  
  TRequestAccess = Class(TGoogleBaseObject)
  Private
    FconcurrentAccess : TConcurrentAccessRestriction;
    FdownloadAccess : TDownloadAccessRestriction;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetconcurrentAccess(AIndex : Integer; AValue : TConcurrentAccessRestriction); virtual;
    Procedure SetdownloadAccess(AIndex : Integer; AValue : TDownloadAccessRestriction); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property concurrentAccess : TConcurrentAccessRestriction Index 0 Read FconcurrentAccess Write SetconcurrentAccess;
    Property downloadAccess : TDownloadAccessRestriction Index 8 Read FdownloadAccess Write SetdownloadAccess;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TRequestAccessClass = Class of TRequestAccess;
  
  { --------------------------------------------------------------------
    TReview
    --------------------------------------------------------------------}
  
  TReview = Class(TGoogleBaseObject)
  Private
    Fauthor : TReviewauthor;
    Fcontent : string;
    Fdate : string;
    FfullTextUrl : string;
    Fkind : string;
    Frating : string;
    Fsource : TReviewsource;
    Ftitle : string;
    F_type : string;
    FvolumeId : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TReviewauthor); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure Setdate(AIndex : Integer; AValue : string); virtual;
    Procedure SetfullTextUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setrating(AIndex : Integer; AValue : string); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TReviewsource); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property author : TReviewauthor Index 0 Read Fauthor Write Setauthor;
    Property content : string Index 8 Read Fcontent Write Setcontent;
    Property date : string Index 16 Read Fdate Write Setdate;
    Property fullTextUrl : string Index 24 Read FfullTextUrl Write SetfullTextUrl;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property rating : string Index 40 Read Frating Write Setrating;
    Property source : TReviewsource Index 48 Read Fsource Write Setsource;
    Property title : string Index 56 Read Ftitle Write Settitle;
    Property _type : string Index 64 Read F_type Write Set_type;
    Property volumeId : string Index 72 Read FvolumeId Write SetvolumeId;
  end;
  TReviewClass = Class of TReview;
  
  { --------------------------------------------------------------------
    TReviewauthor
    --------------------------------------------------------------------}
  
  TReviewauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
  end;
  TReviewauthorClass = Class of TReviewauthor;
  
  { --------------------------------------------------------------------
    TReviewsource
    --------------------------------------------------------------------}
  
  TReviewsource = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    FextraDescription : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetextraDescription(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property extraDescription : string Index 8 Read FextraDescription Write SetextraDescription;
    Property url : string Index 16 Read Furl Write Seturl;
  end;
  TReviewsourceClass = Class of TReviewsource;
  
  { --------------------------------------------------------------------
    TUsersettings
    --------------------------------------------------------------------}
  
  TUsersettings = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnotesExport : TUsersettingsnotesExport;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnotesExport(AIndex : Integer; AValue : TUsersettingsnotesExport); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property notesExport : TUsersettingsnotesExport Index 8 Read FnotesExport Write SetnotesExport;
  end;
  TUsersettingsClass = Class of TUsersettings;
  
  { --------------------------------------------------------------------
    TUsersettingsnotesExport
    --------------------------------------------------------------------}
  
  TUsersettingsnotesExport = Class(TGoogleBaseObject)
  Private
    FfolderName : string;
    FisEnabled : boolean;
  Protected
    //Property setters
    Procedure SetfolderName(AIndex : Integer; AValue : string); virtual;
    Procedure SetisEnabled(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property folderName : string Index 0 Read FfolderName Write SetfolderName;
    Property isEnabled : boolean Index 8 Read FisEnabled Write SetisEnabled;
  end;
  TUsersettingsnotesExportClass = Class of TUsersettingsnotesExport;
  
  { --------------------------------------------------------------------
    TVolume
    --------------------------------------------------------------------}
  
  TVolume = Class(TGoogleBaseObject)
  Private
    FaccessInfo : TVolumeaccessInfo;
    Fetag : string;
    Fid : string;
    Fkind : string;
    FlayerInfo : TVolumelayerInfo;
    FrecommendedInfo : TVolumerecommendedInfo;
    FsaleInfo : TVolumesaleInfo;
    FsearchInfo : TVolumesearchInfo;
    FselfLink : string;
    FuserInfo : TVolumeuserInfo;
    FvolumeInfo : TVolumevolumeInfo;
  Protected
    //Property setters
    Procedure SetaccessInfo(AIndex : Integer; AValue : TVolumeaccessInfo); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlayerInfo(AIndex : Integer; AValue : TVolumelayerInfo); virtual;
    Procedure SetrecommendedInfo(AIndex : Integer; AValue : TVolumerecommendedInfo); virtual;
    Procedure SetsaleInfo(AIndex : Integer; AValue : TVolumesaleInfo); virtual;
    Procedure SetsearchInfo(AIndex : Integer; AValue : TVolumesearchInfo); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserInfo(AIndex : Integer; AValue : TVolumeuserInfo); virtual;
    Procedure SetvolumeInfo(AIndex : Integer; AValue : TVolumevolumeInfo); virtual;
  Public
  Published
    Property accessInfo : TVolumeaccessInfo Index 0 Read FaccessInfo Write SetaccessInfo;
    Property etag : string Index 8 Read Fetag Write Setetag;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property layerInfo : TVolumelayerInfo Index 32 Read FlayerInfo Write SetlayerInfo;
    Property recommendedInfo : TVolumerecommendedInfo Index 40 Read FrecommendedInfo Write SetrecommendedInfo;
    Property saleInfo : TVolumesaleInfo Index 48 Read FsaleInfo Write SetsaleInfo;
    Property searchInfo : TVolumesearchInfo Index 56 Read FsearchInfo Write SetsearchInfo;
    Property selfLink : string Index 64 Read FselfLink Write SetselfLink;
    Property userInfo : TVolumeuserInfo Index 72 Read FuserInfo Write SetuserInfo;
    Property volumeInfo : TVolumevolumeInfo Index 80 Read FvolumeInfo Write SetvolumeInfo;
  end;
  TVolumeClass = Class of TVolume;
  
  { --------------------------------------------------------------------
    TVolumeaccessInfo
    --------------------------------------------------------------------}
  
  TVolumeaccessInfo = Class(TGoogleBaseObject)
  Private
    FaccessViewStatus : string;
    Fcountry : string;
    FdownloadAccess : TDownloadAccessRestriction;
    FdriveImportedContentLink : string;
    Fembeddable : boolean;
    Fepub : TVolumeaccessInfoepub;
    FexplicitOfflineLicenseManagement : boolean;
    Fpdf : TVolumeaccessInfopdf;
    FpublicDomain : boolean;
    FquoteSharingAllowed : boolean;
    FtextToSpeechPermission : string;
    FviewOrderUrl : string;
    Fviewability : string;
    FwebReaderLink : string;
  Protected
    //Property setters
    Procedure SetaccessViewStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetdownloadAccess(AIndex : Integer; AValue : TDownloadAccessRestriction); virtual;
    Procedure SetdriveImportedContentLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setembeddable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setepub(AIndex : Integer; AValue : TVolumeaccessInfoepub); virtual;
    Procedure SetexplicitOfflineLicenseManagement(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setpdf(AIndex : Integer; AValue : TVolumeaccessInfopdf); virtual;
    Procedure SetpublicDomain(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetquoteSharingAllowed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SettextToSpeechPermission(AIndex : Integer; AValue : string); virtual;
    Procedure SetviewOrderUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setviewability(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebReaderLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accessViewStatus : string Index 0 Read FaccessViewStatus Write SetaccessViewStatus;
    Property country : string Index 8 Read Fcountry Write Setcountry;
    Property downloadAccess : TDownloadAccessRestriction Index 16 Read FdownloadAccess Write SetdownloadAccess;
    Property driveImportedContentLink : string Index 24 Read FdriveImportedContentLink Write SetdriveImportedContentLink;
    Property embeddable : boolean Index 32 Read Fembeddable Write Setembeddable;
    Property epub : TVolumeaccessInfoepub Index 40 Read Fepub Write Setepub;
    Property explicitOfflineLicenseManagement : boolean Index 48 Read FexplicitOfflineLicenseManagement Write SetexplicitOfflineLicenseManagement;
    Property pdf : TVolumeaccessInfopdf Index 56 Read Fpdf Write Setpdf;
    Property publicDomain : boolean Index 64 Read FpublicDomain Write SetpublicDomain;
    Property quoteSharingAllowed : boolean Index 72 Read FquoteSharingAllowed Write SetquoteSharingAllowed;
    Property textToSpeechPermission : string Index 80 Read FtextToSpeechPermission Write SettextToSpeechPermission;
    Property viewOrderUrl : string Index 88 Read FviewOrderUrl Write SetviewOrderUrl;
    Property viewability : string Index 96 Read Fviewability Write Setviewability;
    Property webReaderLink : string Index 104 Read FwebReaderLink Write SetwebReaderLink;
  end;
  TVolumeaccessInfoClass = Class of TVolumeaccessInfo;
  
  { --------------------------------------------------------------------
    TVolumeaccessInfoepub
    --------------------------------------------------------------------}
  
  TVolumeaccessInfoepub = Class(TGoogleBaseObject)
  Private
    FacsTokenLink : string;
    FdownloadLink : string;
    FisAvailable : boolean;
  Protected
    //Property setters
    Procedure SetacsTokenLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetdownloadLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetisAvailable(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property acsTokenLink : string Index 0 Read FacsTokenLink Write SetacsTokenLink;
    Property downloadLink : string Index 8 Read FdownloadLink Write SetdownloadLink;
    Property isAvailable : boolean Index 16 Read FisAvailable Write SetisAvailable;
  end;
  TVolumeaccessInfoepubClass = Class of TVolumeaccessInfoepub;
  
  { --------------------------------------------------------------------
    TVolumeaccessInfopdf
    --------------------------------------------------------------------}
  
  TVolumeaccessInfopdf = Class(TGoogleBaseObject)
  Private
    FacsTokenLink : string;
    FdownloadLink : string;
    FisAvailable : boolean;
  Protected
    //Property setters
    Procedure SetacsTokenLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetdownloadLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetisAvailable(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property acsTokenLink : string Index 0 Read FacsTokenLink Write SetacsTokenLink;
    Property downloadLink : string Index 8 Read FdownloadLink Write SetdownloadLink;
    Property isAvailable : boolean Index 16 Read FisAvailable Write SetisAvailable;
  end;
  TVolumeaccessInfopdfClass = Class of TVolumeaccessInfopdf;
  
  { --------------------------------------------------------------------
    TVolumelayerInfo
    --------------------------------------------------------------------}
  
  TVolumelayerInfo = Class(TGoogleBaseObject)
  Private
    Flayers : TVolumelayerInfolayers;
  Protected
    //Property setters
    Procedure Setlayers(AIndex : Integer; AValue : TVolumelayerInfolayers); virtual;
  Public
  Published
    Property layers : TVolumelayerInfolayers Index 0 Read Flayers Write Setlayers;
  end;
  TVolumelayerInfoClass = Class of TVolumelayerInfo;
  
  { --------------------------------------------------------------------
    TVolumelayerInfolayers
    --------------------------------------------------------------------}
  
  TVolumelayerInfolayers = Class(TGoogleBaseObject)
  Private
    FlayerId : string;
    FvolumeAnnotationsVersion : string;
  Protected
    //Property setters
    Procedure SetlayerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetvolumeAnnotationsVersion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property layerId : string Index 0 Read FlayerId Write SetlayerId;
    Property volumeAnnotationsVersion : string Index 8 Read FvolumeAnnotationsVersion Write SetvolumeAnnotationsVersion;
  end;
  TVolumelayerInfolayersClass = Class of TVolumelayerInfolayers;
  
  { --------------------------------------------------------------------
    TVolumerecommendedInfo
    --------------------------------------------------------------------}
  
  TVolumerecommendedInfo = Class(TGoogleBaseObject)
  Private
    Fexplanation : string;
  Protected
    //Property setters
    Procedure Setexplanation(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property explanation : string Index 0 Read Fexplanation Write Setexplanation;
  end;
  TVolumerecommendedInfoClass = Class of TVolumerecommendedInfo;
  
  { --------------------------------------------------------------------
    TVolumesaleInfo
    --------------------------------------------------------------------}
  
  TVolumesaleInfo = Class(TGoogleBaseObject)
  Private
    FbuyLink : string;
    Fcountry : string;
    FisEbook : boolean;
    FlistPrice : TVolumesaleInfolistPrice;
    Foffers : TVolumesaleInfooffers;
    FonSaleDate : TDatetime;
    FretailPrice : TVolumesaleInforetailPrice;
    Fsaleability : string;
  Protected
    //Property setters
    Procedure SetbuyLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetisEbook(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlistPrice(AIndex : Integer; AValue : TVolumesaleInfolistPrice); virtual;
    Procedure Setoffers(AIndex : Integer; AValue : TVolumesaleInfooffers); virtual;
    Procedure SetonSaleDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetretailPrice(AIndex : Integer; AValue : TVolumesaleInforetailPrice); virtual;
    Procedure Setsaleability(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property buyLink : string Index 0 Read FbuyLink Write SetbuyLink;
    Property country : string Index 8 Read Fcountry Write Setcountry;
    Property isEbook : boolean Index 16 Read FisEbook Write SetisEbook;
    Property listPrice : TVolumesaleInfolistPrice Index 24 Read FlistPrice Write SetlistPrice;
    Property offers : TVolumesaleInfooffers Index 32 Read Foffers Write Setoffers;
    Property onSaleDate : TDatetime Index 40 Read FonSaleDate Write SetonSaleDate;
    Property retailPrice : TVolumesaleInforetailPrice Index 48 Read FretailPrice Write SetretailPrice;
    Property saleability : string Index 56 Read Fsaleability Write Setsaleability;
  end;
  TVolumesaleInfoClass = Class of TVolumesaleInfo;
  
  { --------------------------------------------------------------------
    TVolumesaleInfolistPrice
    --------------------------------------------------------------------}
  
  TVolumesaleInfolistPrice = Class(TGoogleBaseObject)
  Private
    Famount : double;
    FcurrencyCode : string;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property amount : double Index 0 Read Famount Write Setamount;
    Property currencyCode : string Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TVolumesaleInfolistPriceClass = Class of TVolumesaleInfolistPrice;
  
  { --------------------------------------------------------------------
    TVolumesaleInfooffers
    --------------------------------------------------------------------}
  
  TVolumesaleInfooffers = Class(TGoogleBaseObject)
  Private
    FfinskyOfferType : integer;
    FlistPrice : TVolumesaleInfoofferslistPrice;
    FrentalDuration : TVolumesaleInfooffersrentalDuration;
    FretailPrice : TVolumesaleInfooffersretailPrice;
  Protected
    //Property setters
    Procedure SetfinskyOfferType(AIndex : Integer; AValue : integer); virtual;
    Procedure SetlistPrice(AIndex : Integer; AValue : TVolumesaleInfoofferslistPrice); virtual;
    Procedure SetrentalDuration(AIndex : Integer; AValue : TVolumesaleInfooffersrentalDuration); virtual;
    Procedure SetretailPrice(AIndex : Integer; AValue : TVolumesaleInfooffersretailPrice); virtual;
  Public
  Published
    Property finskyOfferType : integer Index 0 Read FfinskyOfferType Write SetfinskyOfferType;
    Property listPrice : TVolumesaleInfoofferslistPrice Index 8 Read FlistPrice Write SetlistPrice;
    Property rentalDuration : TVolumesaleInfooffersrentalDuration Index 16 Read FrentalDuration Write SetrentalDuration;
    Property retailPrice : TVolumesaleInfooffersretailPrice Index 24 Read FretailPrice Write SetretailPrice;
  end;
  TVolumesaleInfooffersClass = Class of TVolumesaleInfooffers;
  
  { --------------------------------------------------------------------
    TVolumesaleInfoofferslistPrice
    --------------------------------------------------------------------}
  
  TVolumesaleInfoofferslistPrice = Class(TGoogleBaseObject)
  Private
    FamountInMicros : double;
    FcurrencyCode : string;
  Protected
    //Property setters
    Procedure SetamountInMicros(AIndex : Integer; AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property amountInMicros : double Index 0 Read FamountInMicros Write SetamountInMicros;
    Property currencyCode : string Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TVolumesaleInfoofferslistPriceClass = Class of TVolumesaleInfoofferslistPrice;
  
  { --------------------------------------------------------------------
    TVolumesaleInfooffersrentalDuration
    --------------------------------------------------------------------}
  
  TVolumesaleInfooffersrentalDuration = Class(TGoogleBaseObject)
  Private
    Fcount : double;
    F_unit : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : double); virtual;
    Procedure Set_unit(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : double Index 0 Read Fcount Write Setcount;
    Property _unit : string Index 8 Read F_unit Write Set_unit;
  end;
  TVolumesaleInfooffersrentalDurationClass = Class of TVolumesaleInfooffersrentalDuration;
  
  { --------------------------------------------------------------------
    TVolumesaleInfooffersretailPrice
    --------------------------------------------------------------------}
  
  TVolumesaleInfooffersretailPrice = Class(TGoogleBaseObject)
  Private
    FamountInMicros : double;
    FcurrencyCode : string;
  Protected
    //Property setters
    Procedure SetamountInMicros(AIndex : Integer; AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property amountInMicros : double Index 0 Read FamountInMicros Write SetamountInMicros;
    Property currencyCode : string Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TVolumesaleInfooffersretailPriceClass = Class of TVolumesaleInfooffersretailPrice;
  
  { --------------------------------------------------------------------
    TVolumesaleInforetailPrice
    --------------------------------------------------------------------}
  
  TVolumesaleInforetailPrice = Class(TGoogleBaseObject)
  Private
    Famount : double;
    FcurrencyCode : string;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property amount : double Index 0 Read Famount Write Setamount;
    Property currencyCode : string Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TVolumesaleInforetailPriceClass = Class of TVolumesaleInforetailPrice;
  
  { --------------------------------------------------------------------
    TVolumesearchInfo
    --------------------------------------------------------------------}
  
  TVolumesearchInfo = Class(TGoogleBaseObject)
  Private
    FtextSnippet : string;
  Protected
    //Property setters
    Procedure SettextSnippet(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property textSnippet : string Index 0 Read FtextSnippet Write SettextSnippet;
  end;
  TVolumesearchInfoClass = Class of TVolumesearchInfo;
  
  { --------------------------------------------------------------------
    TVolumeuserInfo
    --------------------------------------------------------------------}
  
  TVolumeuserInfo = Class(TGoogleBaseObject)
  Private
    Fcopy : TVolumeuserInfocopy;
    FisInMyBooks : boolean;
    FisPreordered : boolean;
    FisPurchased : boolean;
    FisUploaded : boolean;
    FreadingPosition : TReadingPosition;
    FrentalPeriod : TVolumeuserInforentalPeriod;
    FrentalState : string;
    Freview : TReview;
    Fupdated : TDatetime;
    FuserUploadedVolumeInfo : TVolumeuserInfouserUploadedVolumeInfo;
  Protected
    //Property setters
    Procedure Setcopy(AIndex : Integer; AValue : TVolumeuserInfocopy); virtual;
    Procedure SetisInMyBooks(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisPreordered(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisPurchased(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisUploaded(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetreadingPosition(AIndex : Integer; AValue : TReadingPosition); virtual;
    Procedure SetrentalPeriod(AIndex : Integer; AValue : TVolumeuserInforentalPeriod); virtual;
    Procedure SetrentalState(AIndex : Integer; AValue : string); virtual;
    Procedure Setreview(AIndex : Integer; AValue : TReview); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuserUploadedVolumeInfo(AIndex : Integer; AValue : TVolumeuserInfouserUploadedVolumeInfo); virtual;
  Public
  Published
    Property copy : TVolumeuserInfocopy Index 0 Read Fcopy Write Setcopy;
    Property isInMyBooks : boolean Index 8 Read FisInMyBooks Write SetisInMyBooks;
    Property isPreordered : boolean Index 16 Read FisPreordered Write SetisPreordered;
    Property isPurchased : boolean Index 24 Read FisPurchased Write SetisPurchased;
    Property isUploaded : boolean Index 32 Read FisUploaded Write SetisUploaded;
    Property readingPosition : TReadingPosition Index 40 Read FreadingPosition Write SetreadingPosition;
    Property rentalPeriod : TVolumeuserInforentalPeriod Index 48 Read FrentalPeriod Write SetrentalPeriod;
    Property rentalState : string Index 56 Read FrentalState Write SetrentalState;
    Property review : TReview Index 64 Read Freview Write Setreview;
    Property updated : TDatetime Index 72 Read Fupdated Write Setupdated;
    Property userUploadedVolumeInfo : TVolumeuserInfouserUploadedVolumeInfo Index 80 Read FuserUploadedVolumeInfo Write SetuserUploadedVolumeInfo;
  end;
  TVolumeuserInfoClass = Class of TVolumeuserInfo;
  
  { --------------------------------------------------------------------
    TVolumeuserInfocopy
    --------------------------------------------------------------------}
  
  TVolumeuserInfocopy = Class(TGoogleBaseObject)
  Private
    FallowedCharacterCount : integer;
    FlimitType : string;
    FremainingCharacterCount : integer;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure SetallowedCharacterCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetlimitType(AIndex : Integer; AValue : string); virtual;
    Procedure SetremainingCharacterCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property allowedCharacterCount : integer Index 0 Read FallowedCharacterCount Write SetallowedCharacterCount;
    Property limitType : string Index 8 Read FlimitType Write SetlimitType;
    Property remainingCharacterCount : integer Index 16 Read FremainingCharacterCount Write SetremainingCharacterCount;
    Property updated : TDatetime Index 24 Read Fupdated Write Setupdated;
  end;
  TVolumeuserInfocopyClass = Class of TVolumeuserInfocopy;
  
  { --------------------------------------------------------------------
    TVolumeuserInforentalPeriod
    --------------------------------------------------------------------}
  
  TVolumeuserInforentalPeriod = Class(TGoogleBaseObject)
  Private
    FendUtcSec : string;
    FstartUtcSec : string;
  Protected
    //Property setters
    Procedure SetendUtcSec(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartUtcSec(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property endUtcSec : string Index 0 Read FendUtcSec Write SetendUtcSec;
    Property startUtcSec : string Index 8 Read FstartUtcSec Write SetstartUtcSec;
  end;
  TVolumeuserInforentalPeriodClass = Class of TVolumeuserInforentalPeriod;
  
  { --------------------------------------------------------------------
    TVolumeuserInfouserUploadedVolumeInfo
    --------------------------------------------------------------------}
  
  TVolumeuserInfouserUploadedVolumeInfo = Class(TGoogleBaseObject)
  Private
    FprocessingState : string;
  Protected
    //Property setters
    Procedure SetprocessingState(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property processingState : string Index 0 Read FprocessingState Write SetprocessingState;
  end;
  TVolumeuserInfouserUploadedVolumeInfoClass = Class of TVolumeuserInfouserUploadedVolumeInfo;
  
  { --------------------------------------------------------------------
    TVolumevolumeInfo
    --------------------------------------------------------------------}
  
  TVolumevolumeInfo = Class(TGoogleBaseObject)
  Private
    FallowAnonLogging : boolean;
    Fauthors : TVolumevolumeInfoauthors;
    FaverageRating : double;
    FcanonicalVolumeLink : string;
    Fcategories : TVolumevolumeInfocategories;
    FcontentVersion : string;
    Fdescription : string;
    Fdimensions : TVolumevolumeInfodimensions;
    FimageLinks : TVolumevolumeInfoimageLinks;
    FindustryIdentifiers : TVolumevolumeInfoindustryIdentifiers;
    FinfoLink : string;
    Flanguage : string;
    FmainCategory : string;
    FmaturityRating : string;
    FpageCount : integer;
    FpreviewLink : string;
    FprintType : string;
    FprintedPageCount : integer;
    FpublishedDate : string;
    Fpublisher : string;
    FratingsCount : integer;
    FreadingModes : TJSONSchema;
    FsamplePageCount : integer;
    Fsubtitle : string;
    Ftitle : string;
  Protected
    //Property setters
    Procedure SetallowAnonLogging(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setauthors(AIndex : Integer; AValue : TVolumevolumeInfoauthors); virtual;
    Procedure SetaverageRating(AIndex : Integer; AValue : double); virtual;
    Procedure SetcanonicalVolumeLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setcategories(AIndex : Integer; AValue : TVolumevolumeInfocategories); virtual;
    Procedure SetcontentVersion(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TVolumevolumeInfodimensions); virtual;
    Procedure SetimageLinks(AIndex : Integer; AValue : TVolumevolumeInfoimageLinks); virtual;
    Procedure SetindustryIdentifiers(AIndex : Integer; AValue : TVolumevolumeInfoindustryIdentifiers); virtual;
    Procedure SetinfoLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetmainCategory(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaturityRating(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpreviewLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetprintType(AIndex : Integer; AValue : string); virtual;
    Procedure SetprintedPageCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpublishedDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setpublisher(AIndex : Integer; AValue : string); virtual;
    Procedure SetratingsCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetreadingModes(AIndex : Integer; AValue : TJSONSchema); virtual;
    Procedure SetsamplePageCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setsubtitle(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property allowAnonLogging : boolean Index 0 Read FallowAnonLogging Write SetallowAnonLogging;
    Property authors : TVolumevolumeInfoauthors Index 8 Read Fauthors Write Setauthors;
    Property averageRating : double Index 16 Read FaverageRating Write SetaverageRating;
    Property canonicalVolumeLink : string Index 24 Read FcanonicalVolumeLink Write SetcanonicalVolumeLink;
    Property categories : TVolumevolumeInfocategories Index 32 Read Fcategories Write Setcategories;
    Property contentVersion : string Index 40 Read FcontentVersion Write SetcontentVersion;
    Property description : string Index 48 Read Fdescription Write Setdescription;
    Property dimensions : TVolumevolumeInfodimensions Index 56 Read Fdimensions Write Setdimensions;
    Property imageLinks : TVolumevolumeInfoimageLinks Index 64 Read FimageLinks Write SetimageLinks;
    Property industryIdentifiers : TVolumevolumeInfoindustryIdentifiers Index 72 Read FindustryIdentifiers Write SetindustryIdentifiers;
    Property infoLink : string Index 80 Read FinfoLink Write SetinfoLink;
    Property language : string Index 88 Read Flanguage Write Setlanguage;
    Property mainCategory : string Index 96 Read FmainCategory Write SetmainCategory;
    Property maturityRating : string Index 104 Read FmaturityRating Write SetmaturityRating;
    Property pageCount : integer Index 112 Read FpageCount Write SetpageCount;
    Property previewLink : string Index 120 Read FpreviewLink Write SetpreviewLink;
    Property printType : string Index 128 Read FprintType Write SetprintType;
    Property printedPageCount : integer Index 136 Read FprintedPageCount Write SetprintedPageCount;
    Property publishedDate : string Index 144 Read FpublishedDate Write SetpublishedDate;
    Property publisher : string Index 152 Read Fpublisher Write Setpublisher;
    Property ratingsCount : integer Index 160 Read FratingsCount Write SetratingsCount;
    Property readingModes : TJSONSchema Index 168 Read FreadingModes Write SetreadingModes;
    Property samplePageCount : integer Index 176 Read FsamplePageCount Write SetsamplePageCount;
    Property subtitle : string Index 184 Read Fsubtitle Write Setsubtitle;
    Property title : string Index 192 Read Ftitle Write Settitle;
  end;
  TVolumevolumeInfoClass = Class of TVolumevolumeInfo;
  
  { --------------------------------------------------------------------
    TVolumevolumeInfoauthors
    --------------------------------------------------------------------}
  
  TVolumevolumeInfoauthors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVolumevolumeInfoauthorsClass = Class of TVolumevolumeInfoauthors;
  
  { --------------------------------------------------------------------
    TVolumevolumeInfocategories
    --------------------------------------------------------------------}
  
  TVolumevolumeInfocategories = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVolumevolumeInfocategoriesClass = Class of TVolumevolumeInfocategories;
  
  { --------------------------------------------------------------------
    TVolumevolumeInfodimensions
    --------------------------------------------------------------------}
  
  TVolumevolumeInfodimensions = Class(TGoogleBaseObject)
  Private
    Fheight : string;
    Fthickness : string;
    Fwidth : string;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : string); virtual;
    Procedure Setthickness(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property height : string Index 0 Read Fheight Write Setheight;
    Property thickness : string Index 8 Read Fthickness Write Setthickness;
    Property width : string Index 16 Read Fwidth Write Setwidth;
  end;
  TVolumevolumeInfodimensionsClass = Class of TVolumevolumeInfodimensions;
  
  { --------------------------------------------------------------------
    TVolumevolumeInfoimageLinks
    --------------------------------------------------------------------}
  
  TVolumevolumeInfoimageLinks = Class(TGoogleBaseObject)
  Private
    FextraLarge : string;
    Flarge : string;
    Fmedium : string;
    Fsmall : string;
    FsmallThumbnail : string;
    Fthumbnail : string;
  Protected
    //Property setters
    Procedure SetextraLarge(AIndex : Integer; AValue : string); virtual;
    Procedure Setlarge(AIndex : Integer; AValue : string); virtual;
    Procedure Setmedium(AIndex : Integer; AValue : string); virtual;
    Procedure Setsmall(AIndex : Integer; AValue : string); virtual;
    Procedure SetsmallThumbnail(AIndex : Integer; AValue : string); virtual;
    Procedure Setthumbnail(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property extraLarge : string Index 0 Read FextraLarge Write SetextraLarge;
    Property large : string Index 8 Read Flarge Write Setlarge;
    Property medium : string Index 16 Read Fmedium Write Setmedium;
    Property small : string Index 24 Read Fsmall Write Setsmall;
    Property smallThumbnail : string Index 32 Read FsmallThumbnail Write SetsmallThumbnail;
    Property thumbnail : string Index 40 Read Fthumbnail Write Setthumbnail;
  end;
  TVolumevolumeInfoimageLinksClass = Class of TVolumevolumeInfoimageLinks;
  
  { --------------------------------------------------------------------
    TVolumevolumeInfoindustryIdentifiers
    --------------------------------------------------------------------}
  
  TVolumevolumeInfoindustryIdentifiers = Class(TGoogleBaseObject)
  Private
    Fidentifier : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setidentifier(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property identifier : string Index 0 Read Fidentifier Write Setidentifier;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TVolumevolumeInfoindustryIdentifiersClass = Class of TVolumevolumeInfoindustryIdentifiers;
  
  { --------------------------------------------------------------------
    TVolume2
    --------------------------------------------------------------------}
  
  TVolume2 = Class(TGoogleBaseObject)
  Private
    Fitems : TVolume2items;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TVolume2items); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TVolume2items Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TVolume2Class = Class of TVolume2;
  
  { --------------------------------------------------------------------
    TVolume2items
    --------------------------------------------------------------------}
  
  TVolume2items = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVolume2itemsClass = Class of TVolume2items;
  
  { --------------------------------------------------------------------
    TVolumeannotation
    --------------------------------------------------------------------}
  
  TVolumeannotation = Class(TGoogleBaseObject)
  Private
    FannotationDataId : string;
    FannotationDataLink : string;
    FannotationType : string;
    FcontentRanges : TVolumeannotationcontentRanges;
    Fdata : string;
    Fdeleted : boolean;
    Fid : string;
    Fkind : string;
    FlayerId : string;
    FpageIds : TVolumeannotationpageIds;
    FselectedText : string;
    FselfLink : string;
    Fupdated : TDatetime;
    FvolumeId : string;
  Protected
    //Property setters
    Procedure SetannotationDataId(AIndex : Integer; AValue : string); virtual;
    Procedure SetannotationDataLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetannotationType(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentRanges(AIndex : Integer; AValue : TVolumeannotationcontentRanges); virtual;
    Procedure Setdata(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlayerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageIds(AIndex : Integer; AValue : TVolumeannotationpageIds); virtual;
    Procedure SetselectedText(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetvolumeId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property annotationDataId : string Index 0 Read FannotationDataId Write SetannotationDataId;
    Property annotationDataLink : string Index 8 Read FannotationDataLink Write SetannotationDataLink;
    Property annotationType : string Index 16 Read FannotationType Write SetannotationType;
    Property contentRanges : TVolumeannotationcontentRanges Index 24 Read FcontentRanges Write SetcontentRanges;
    Property data : string Index 32 Read Fdata Write Setdata;
    Property deleted : boolean Index 40 Read Fdeleted Write Setdeleted;
    Property id : string Index 48 Read Fid Write Setid;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property layerId : string Index 64 Read FlayerId Write SetlayerId;
    Property pageIds : TVolumeannotationpageIds Index 72 Read FpageIds Write SetpageIds;
    Property selectedText : string Index 80 Read FselectedText Write SetselectedText;
    Property selfLink : string Index 88 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 96 Read Fupdated Write Setupdated;
    Property volumeId : string Index 104 Read FvolumeId Write SetvolumeId;
  end;
  TVolumeannotationClass = Class of TVolumeannotation;
  
  { --------------------------------------------------------------------
    TVolumeannotationcontentRanges
    --------------------------------------------------------------------}
  
  TVolumeannotationcontentRanges = Class(TGoogleBaseObject)
  Private
    FcfiRange : TBooksAnnotationsRange;
    FcontentVersion : string;
    FgbImageRange : TBooksAnnotationsRange;
    FgbTextRange : TBooksAnnotationsRange;
  Protected
    //Property setters
    Procedure SetcfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
    Procedure SetcontentVersion(AIndex : Integer; AValue : string); virtual;
    Procedure SetgbImageRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
    Procedure SetgbTextRange(AIndex : Integer; AValue : TBooksAnnotationsRange); virtual;
  Public
  Published
    Property cfiRange : TBooksAnnotationsRange Index 0 Read FcfiRange Write SetcfiRange;
    Property contentVersion : string Index 8 Read FcontentVersion Write SetcontentVersion;
    Property gbImageRange : TBooksAnnotationsRange Index 16 Read FgbImageRange Write SetgbImageRange;
    Property gbTextRange : TBooksAnnotationsRange Index 24 Read FgbTextRange Write SetgbTextRange;
  end;
  TVolumeannotationcontentRangesClass = Class of TVolumeannotationcontentRanges;
  
  { --------------------------------------------------------------------
    TVolumeannotationpageIds
    --------------------------------------------------------------------}
  
  TVolumeannotationpageIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVolumeannotationpageIdsClass = Class of TVolumeannotationpageIds;
  
  { --------------------------------------------------------------------
    TVolumeannotations
    --------------------------------------------------------------------}
  
  TVolumeannotations = Class(TGoogleBaseObject)
  Private
    Fitems : TVolumeannotationsitems;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
    Fversion : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TVolumeannotationsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TVolumeannotationsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
    Property version : string Index 32 Read Fversion Write Setversion;
  end;
  TVolumeannotationsClass = Class of TVolumeannotations;
  
  { --------------------------------------------------------------------
    TVolumeannotationsitems
    --------------------------------------------------------------------}
  
  TVolumeannotationsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVolumeannotationsitemsClass = Class of TVolumeannotationsitems;
  
  { --------------------------------------------------------------------
    TVolumes
    --------------------------------------------------------------------}
  
  TVolumes = Class(TGoogleBaseObject)
  Private
    Fitems : TVolumesitems;
    Fkind : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TVolumesitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TVolumesitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property totalItems : integer Index 16 Read FtotalItems Write SettotalItems;
  end;
  TVolumesClass = Class of TVolumes;
  
  { --------------------------------------------------------------------
    TVolumesitems
    --------------------------------------------------------------------}
  
  TVolumesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVolumesitemsClass = Class of TVolumesitems;
  
  { --------------------------------------------------------------------
    TBookshelvesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBookshelvesResource, method Get
  
  TBookshelvesGetOptions = Record
    source : string;
  end;
  
  
  //Optional query Options for TBookshelvesResource, method List
  
  TBookshelvesListOptions = Record
    source : string;
  end;
  
  TBookshelvesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(shelf: string; userId: string; AQuery : string  = '') : TBookshelf;
    Function Get(shelf: string; userId: string; AQuery : TBookshelvesgetOptions) : TBookshelf;
    Function List(userId: string; AQuery : string  = '') : TBookshelves;
    Function List(userId: string; AQuery : TBookshelveslistOptions) : TBookshelves;
  end;
  
  
  { --------------------------------------------------------------------
    TCloudloadingResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCloudloadingResource, method AddBook
  
  TCloudloadingAddBookOptions = Record
    drive_document_id : string;
    mime_type : string;
    _name : string;
    upload_client_token : string;
  end;
  
  
  //Optional query Options for TCloudloadingResource, method DeleteBook
  
  TCloudloadingDeleteBookOptions = Record
    volumeId : string;
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
    cpksver : string;
  end;
  
  TDictionaryResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function ListOfflineMetadata(AQuery : string  = '') : TMetadata;
    Function ListOfflineMetadata(AQuery : TDictionarylistOfflineMetadataOptions) : TMetadata;
  end;
  
  
  { --------------------------------------------------------------------
    TLayersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLayersResource, method Get
  
  TLayersGetOptions = Record
    contentVersion : string;
    source : string;
  end;
  
  
  //Optional query Options for TLayersResource, method List
  
  TLayersListOptions = Record
    contentVersion : string;
    maxResults : integer;
    pageToken : string;
    source : string;
  end;
  
  TLayersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(summaryId: string; volumeId: string; AQuery : string  = '') : TLayersummary;
    Function Get(summaryId: string; volumeId: string; AQuery : TLayersgetOptions) : TLayersummary;
    Function List(volumeId: string; AQuery : string  = '') : TLayersummaries;
    Function List(volumeId: string; AQuery : TLayerslistOptions) : TLayersummaries;
  end;
  
  
  { --------------------------------------------------------------------
    TMyconfigResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMyconfigResource, method ReleaseDownloadAccess
  
  TMyconfigReleaseDownloadAccessOptions = Record
    cpksver : string;
    locale : string;
    source : string;
    volumeIds : string;
  end;
  
  
  //Optional query Options for TMyconfigResource, method RequestAccess
  
  TMyconfigRequestAccessOptions = Record
    cpksver : string;
    licenseTypes : string;
    locale : string;
    nonce : string;
    source : string;
    volumeId : string;
  end;
  
  
  //Optional query Options for TMyconfigResource, method SyncVolumeLicenses
  
  TMyconfigSyncVolumeLicensesOptions = Record
    cpksver : string;
    features : string;
    locale : string;
    nonce : string;
    showPreorders : boolean;
    source : string;
    volumeIds : string;
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
    TMylibraryResource
    --------------------------------------------------------------------}
  
  TMylibraryResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TOnboardingResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOnboardingResource, method ListCategories
  
  TOnboardingListCategoriesOptions = Record
    locale : string;
  end;
  
  
  //Optional query Options for TOnboardingResource, method ListCategoryVolumes
  
  TOnboardingListCategoryVolumesOptions = Record
    categoryId : string;
    locale : string;
    maxAllowedMaturityRating : string;
    pageSize : integer;
    pageToken : string;
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
    TPromoofferResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPromoofferResource, method Accept
  
  TPromoofferAcceptOptions = Record
    androidId : string;
    device : string;
    manufacturer : string;
    model : string;
    offerId : string;
    product : string;
    serial : string;
    volumeId : string;
  end;
  
  
  //Optional query Options for TPromoofferResource, method Dismiss
  
  TPromoofferDismissOptions = Record
    androidId : string;
    device : string;
    manufacturer : string;
    model : string;
    offerId : string;
    product : string;
    serial : string;
  end;
  
  
  //Optional query Options for TPromoofferResource, method Get
  
  TPromoofferGetOptions = Record
    androidId : string;
    device : string;
    manufacturer : string;
    model : string;
    product : string;
    serial : string;
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
    TVolumesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVolumesResource, method Get
  
  TVolumesGetOptions = Record
    country : string;
    partner : string;
    projection : string;
    source : string;
    user_library_consistent_read : boolean;
  end;
  
  
  //Optional query Options for TVolumesResource, method List
  
  TVolumesListOptions = Record
    download : string;
    filter : string;
    langRestrict : string;
    libraryRestrict : string;
    maxResults : integer;
    orderBy : string;
    partner : string;
    printType : string;
    projection : string;
    q : string;
    showPreorders : boolean;
    source : string;
    startIndex : integer;
  end;
  
  TVolumesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(volumeId: string; AQuery : string  = '') : TVolume;
    Function Get(volumeId: string; AQuery : TVolumesgetOptions) : TVolume;
    Function List(AQuery : string  = '') : TVolumes;
    Function List(AQuery : TVolumeslistOptions) : TVolumes;
  end;
  
  
  { --------------------------------------------------------------------
    TBooksAPI
    --------------------------------------------------------------------}
  
  TBooksAPI = Class(TGoogleAPI)
  Private
    FBookshelvesInstance : TBookshelvesResource;
    FCloudloadingInstance : TCloudloadingResource;
    FDictionaryInstance : TDictionaryResource;
    FLayersInstance : TLayersResource;
    FMyconfigInstance : TMyconfigResource;
    FMylibraryInstance : TMylibraryResource;
    FOnboardingInstance : TOnboardingResource;
    FPromoofferInstance : TPromoofferResource;
    FVolumesInstance : TVolumesResource;
    Function GetBookshelvesInstance : TBookshelvesResource;virtual;
    Function GetCloudloadingInstance : TCloudloadingResource;virtual;
    Function GetDictionaryInstance : TDictionaryResource;virtual;
    Function GetLayersInstance : TLayersResource;virtual;
    Function GetMyconfigInstance : TMyconfigResource;virtual;
    Function GetMylibraryInstance : TMylibraryResource;virtual;
    Function GetOnboardingInstance : TOnboardingResource;virtual;
    Function GetPromoofferInstance : TPromoofferResource;virtual;
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
    Function CreateBookshelvesResource(AOwner : TComponent) : TBookshelvesResource;virtual;overload;
    Function CreateBookshelvesResource : TBookshelvesResource;virtual;overload;
    Function CreateCloudloadingResource(AOwner : TComponent) : TCloudloadingResource;virtual;overload;
    Function CreateCloudloadingResource : TCloudloadingResource;virtual;overload;
    Function CreateDictionaryResource(AOwner : TComponent) : TDictionaryResource;virtual;overload;
    Function CreateDictionaryResource : TDictionaryResource;virtual;overload;
    Function CreateLayersResource(AOwner : TComponent) : TLayersResource;virtual;overload;
    Function CreateLayersResource : TLayersResource;virtual;overload;
    Function CreateMyconfigResource(AOwner : TComponent) : TMyconfigResource;virtual;overload;
    Function CreateMyconfigResource : TMyconfigResource;virtual;overload;
    Function CreateMylibraryResource(AOwner : TComponent) : TMylibraryResource;virtual;overload;
    Function CreateMylibraryResource : TMylibraryResource;virtual;overload;
    Function CreateOnboardingResource(AOwner : TComponent) : TOnboardingResource;virtual;overload;
    Function CreateOnboardingResource : TOnboardingResource;virtual;overload;
    Function CreatePromoofferResource(AOwner : TComponent) : TPromoofferResource;virtual;overload;
    Function CreatePromoofferResource : TPromoofferResource;virtual;overload;
    Function CreateVolumesResource(AOwner : TComponent) : TVolumesResource;virtual;overload;
    Function CreateVolumesResource : TVolumesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property BookshelvesResource : TBookshelvesResource Read GetBookshelvesInstance;
    Property CloudloadingResource : TCloudloadingResource Read GetCloudloadingInstance;
    Property DictionaryResource : TDictionaryResource Read GetDictionaryInstance;
    Property LayersResource : TLayersResource Read GetLayersInstance;
    Property MyconfigResource : TMyconfigResource Read GetMyconfigInstance;
    Property MylibraryResource : TMylibraryResource Read GetMylibraryInstance;
    Property OnboardingResource : TOnboardingResource Read GetOnboardingInstance;
    Property PromoofferResource : TPromoofferResource Read GetPromoofferInstance;
    Property VolumesResource : TVolumesResource Read GetVolumesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAnnotation
  --------------------------------------------------------------------}


Procedure TAnnotation.SetafterSelectedText(AIndex : Integer; AValue : string); 

begin
  If (FafterSelectedText=AValue) then exit;
  FafterSelectedText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetbeforeSelectedText(AIndex : Integer; AValue : string); 

begin
  If (FbeforeSelectedText=AValue) then exit;
  FbeforeSelectedText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetclientVersionRanges(AIndex : Integer; AValue : TAnnotationclientVersionRanges); 

begin
  If (FclientVersionRanges=AValue) then exit;
  FclientVersionRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetcurrentVersionRanges(AIndex : Integer; AValue : TAnnotationcurrentVersionRanges); 

begin
  If (FcurrentVersionRanges=AValue) then exit;
  FcurrentVersionRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setdata(AIndex : Integer; AValue : string); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SethighlightStyle(AIndex : Integer; AValue : string); 

begin
  If (FhighlightStyle=AValue) then exit;
  FhighlightStyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetlayerId(AIndex : Integer; AValue : string); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetlayerSummary(AIndex : Integer; AValue : TAnnotationlayerSummary); 

begin
  If (FlayerSummary=AValue) then exit;
  FlayerSummary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetpageIds(AIndex : Integer; AValue : TAnnotationpageIds); 

begin
  If (FpageIds=AValue) then exit;
  FpageIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetselectedText(AIndex : Integer; AValue : string); 

begin
  If (FselectedText=AValue) then exit;
  FselectedText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationclientVersionRanges
  --------------------------------------------------------------------}


Procedure TAnnotationclientVersionRanges.SetcfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FcfiRange=AValue) then exit;
  FcfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationclientVersionRanges.SetcontentVersion(AIndex : Integer; AValue : string); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationclientVersionRanges.SetgbImageRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FgbImageRange=AValue) then exit;
  FgbImageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationclientVersionRanges.SetgbTextRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FgbTextRange=AValue) then exit;
  FgbTextRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationclientVersionRanges.SetimageCfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FimageCfiRange=AValue) then exit;
  FimageCfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationcurrentVersionRanges
  --------------------------------------------------------------------}


Procedure TAnnotationcurrentVersionRanges.SetcfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FcfiRange=AValue) then exit;
  FcfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationcurrentVersionRanges.SetcontentVersion(AIndex : Integer; AValue : string); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationcurrentVersionRanges.SetgbImageRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FgbImageRange=AValue) then exit;
  FgbImageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationcurrentVersionRanges.SetgbTextRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FgbTextRange=AValue) then exit;
  FgbTextRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationcurrentVersionRanges.SetimageCfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FimageCfiRange=AValue) then exit;
  FimageCfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationlayerSummary
  --------------------------------------------------------------------}


Procedure TAnnotationlayerSummary.SetallowedCharacterCount(AIndex : Integer; AValue : integer); 

begin
  If (FallowedCharacterCount=AValue) then exit;
  FallowedCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationlayerSummary.SetlimitType(AIndex : Integer; AValue : string); 

begin
  If (FlimitType=AValue) then exit;
  FlimitType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationlayerSummary.SetremainingCharacterCount(AIndex : Integer; AValue : integer); 

begin
  If (FremainingCharacterCount=AValue) then exit;
  FremainingCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationpageIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAnnotationdata
  --------------------------------------------------------------------}


Procedure TAnnotationdata.SetannotationType(AIndex : Integer; AValue : string); 

begin
  If (FannotationType=AValue) then exit;
  FannotationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setdata(AIndex : Integer; AValue : TJSONSchema); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setencoded_data(AIndex : Integer; AValue : string); 

begin
  If (Fencoded_data=AValue) then exit;
  Fencoded_data:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.SetlayerId(AIndex : Integer; AValue : string); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationdata.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotations
  --------------------------------------------------------------------}


Procedure TAnnotations.Setitems(AIndex : Integer; AValue : TAnnotationsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotations.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotations.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotations.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAnnotationsSummary
  --------------------------------------------------------------------}


Procedure TAnnotationsSummary.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummary.Setlayers(AIndex : Integer; AValue : TAnnotationsSummarylayers); 

begin
  If (Flayers=AValue) then exit;
  Flayers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationsSummarylayers
  --------------------------------------------------------------------}


Procedure TAnnotationsSummarylayers.SetallowedCharacterCount(AIndex : Integer; AValue : integer); 

begin
  If (FallowedCharacterCount=AValue) then exit;
  FallowedCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummarylayers.SetlayerId(AIndex : Integer; AValue : string); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummarylayers.SetlimitType(AIndex : Integer; AValue : string); 

begin
  If (FlimitType=AValue) then exit;
  FlimitType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummarylayers.SetremainingCharacterCount(AIndex : Integer; AValue : integer); 

begin
  If (FremainingCharacterCount=AValue) then exit;
  FremainingCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsSummarylayers.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationsdata
  --------------------------------------------------------------------}


Procedure TAnnotationsdata.Setitems(AIndex : Integer; AValue : TAnnotationsdataitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsdata.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsdata.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationsdata.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationsdataitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBooksAnnotationsRange
  --------------------------------------------------------------------}


Procedure TBooksAnnotationsRange.SetendOffset(AIndex : Integer; AValue : string); 

begin
  If (FendOffset=AValue) then exit;
  FendOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksAnnotationsRange.SetendPosition(AIndex : Integer; AValue : string); 

begin
  If (FendPosition=AValue) then exit;
  FendPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksAnnotationsRange.SetstartOffset(AIndex : Integer; AValue : string); 

begin
  If (FstartOffset=AValue) then exit;
  FstartOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksAnnotationsRange.SetstartPosition(AIndex : Integer; AValue : string); 

begin
  If (FstartPosition=AValue) then exit;
  FstartPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBooksCloudloadingResource
  --------------------------------------------------------------------}


Procedure TBooksCloudloadingResource.Setauthor(AIndex : Integer; AValue : string); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksCloudloadingResource.SetprocessingState(AIndex : Integer; AValue : string); 

begin
  If (FprocessingState=AValue) then exit;
  FprocessingState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksCloudloadingResource.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooksCloudloadingResource.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBooksVolumesRecommendedRateResponse
  --------------------------------------------------------------------}


Procedure TBooksVolumesRecommendedRateResponse.Setconsistency_token(AIndex : Integer; AValue : string); 

begin
  If (Fconsistency_token=AValue) then exit;
  Fconsistency_token:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBookshelf
  --------------------------------------------------------------------}


Procedure TBookshelf.Setaccess(AIndex : Integer; AValue : string); 

begin
  If (Faccess=AValue) then exit;
  Faccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setid(AIndex : Integer; AValue : integer); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.SetvolumeCount(AIndex : Integer; AValue : integer); 

begin
  If (FvolumeCount=AValue) then exit;
  FvolumeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelf.SetvolumesLastUpdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (FvolumesLastUpdated=AValue) then exit;
  FvolumesLastUpdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBookshelves
  --------------------------------------------------------------------}


Procedure TBookshelves.Setitems(AIndex : Integer; AValue : TBookshelvesitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBookshelves.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBookshelvesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCategory
  --------------------------------------------------------------------}


Procedure TCategory.Setitems(AIndex : Integer; AValue : TCategoryitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategory.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCategoryitems
  --------------------------------------------------------------------}


Procedure TCategoryitems.SetbadgeUrl(AIndex : Integer; AValue : string); 

begin
  If (FbadgeUrl=AValue) then exit;
  FbadgeUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategoryitems.SetcategoryId(AIndex : Integer; AValue : string); 

begin
  If (FcategoryId=AValue) then exit;
  FcategoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategoryitems.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConcurrentAccessRestriction
  --------------------------------------------------------------------}


Procedure TConcurrentAccessRestriction.SetdeviceAllowed(AIndex : Integer; AValue : boolean); 

begin
  If (FdeviceAllowed=AValue) then exit;
  FdeviceAllowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.SetmaxConcurrentDevices(AIndex : Integer; AValue : integer); 

begin
  If (FmaxConcurrentDevices=AValue) then exit;
  FmaxConcurrentDevices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setnonce(AIndex : Integer; AValue : string); 

begin
  If (Fnonce=AValue) then exit;
  Fnonce:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.SetreasonCode(AIndex : Integer; AValue : string); 

begin
  If (FreasonCode=AValue) then exit;
  FreasonCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setrestricted(AIndex : Integer; AValue : boolean); 

begin
  If (Frestricted=AValue) then exit;
  Frestricted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setsignature(AIndex : Integer; AValue : string); 

begin
  If (Fsignature=AValue) then exit;
  Fsignature:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.SettimeWindowSeconds(AIndex : Integer; AValue : integer); 

begin
  If (FtimeWindowSeconds=AValue) then exit;
  FtimeWindowSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcurrentAccessRestriction.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdata
  --------------------------------------------------------------------}


Procedure TDictlayerdata.Setcommon(AIndex : Integer; AValue : TDictlayerdatacommon); 

begin
  If (Fcommon=AValue) then exit;
  Fcommon:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdata.Setdict(AIndex : Integer; AValue : TDictlayerdatadict); 

begin
  If (Fdict=AValue) then exit;
  Fdict:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdata.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatacommon
  --------------------------------------------------------------------}


Procedure TDictlayerdatacommon.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadict
  --------------------------------------------------------------------}


Procedure TDictlayerdatadict.Setsource(AIndex : Integer; AValue : TDictlayerdatadictsource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadict.Setwords(AIndex : Integer; AValue : TDictlayerdatadictwords); 

begin
  If (Fwords=AValue) then exit;
  Fwords:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictsource
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictsource.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictsource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwords
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwords.Setderivatives(AIndex : Integer; AValue : TDictlayerdatadictwordsderivatives); 

begin
  If (Fderivatives=AValue) then exit;
  Fderivatives:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwords.Setexamples(AIndex : Integer; AValue : TDictlayerdatadictwordsexamples); 

begin
  If (Fexamples=AValue) then exit;
  Fexamples:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwords.Setsenses(AIndex : Integer; AValue : TDictlayerdatadictwordssenses); 

begin
  If (Fsenses=AValue) then exit;
  Fsenses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwords.Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordssource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordsderivatives
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordsderivatives.Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordsderivativessource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordsderivatives.Settext(AIndex : Integer; AValue : string); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordsderivativessource
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordsderivativessource.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordsderivativessource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordsexamples
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordsexamples.Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordsexamplessource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordsexamples.Settext(AIndex : Integer; AValue : string); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordsexamplessource
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordsexamplessource.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordsexamplessource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordssenses
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssenses.Setconjugations(AIndex : Integer; AValue : TDictlayerdatadictwordssensesconjugations); 

begin
  If (Fconjugations=AValue) then exit;
  Fconjugations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssenses.Setdefinitions(AIndex : Integer; AValue : TDictlayerdatadictwordssensesdefinitions); 

begin
  If (Fdefinitions=AValue) then exit;
  Fdefinitions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssenses.SetpartOfSpeech(AIndex : Integer; AValue : string); 

begin
  If (FpartOfSpeech=AValue) then exit;
  FpartOfSpeech:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssenses.Setpronunciation(AIndex : Integer; AValue : string); 

begin
  If (Fpronunciation=AValue) then exit;
  Fpronunciation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssenses.SetpronunciationUrl(AIndex : Integer; AValue : string); 

begin
  If (FpronunciationUrl=AValue) then exit;
  FpronunciationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssenses.Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordssensessource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssenses.Setsyllabification(AIndex : Integer; AValue : string); 

begin
  If (Fsyllabification=AValue) then exit;
  Fsyllabification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssenses.Setsynonyms(AIndex : Integer; AValue : TDictlayerdatadictwordssensessynonyms); 

begin
  If (Fsynonyms=AValue) then exit;
  Fsynonyms:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordssensesconjugations
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssensesconjugations.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssensesconjugations.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDictlayerdatadictwordssensesconjugations.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDictlayerdatadictwordssensesdefinitions
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssensesdefinitions.Setdefinition(AIndex : Integer; AValue : string); 

begin
  If (Fdefinition=AValue) then exit;
  Fdefinition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssensesdefinitions.Setexamples(AIndex : Integer; AValue : TDictlayerdatadictwordssensesdefinitionsexamples); 

begin
  If (Fexamples=AValue) then exit;
  Fexamples:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordssensesdefinitionsexamples
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssensesdefinitionsexamples.Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordssensesdefinitionsexamplessource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssensesdefinitionsexamples.Settext(AIndex : Integer; AValue : string); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordssensesdefinitionsexamplessource
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssensesdefinitionsexamplessource.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssensesdefinitionsexamplessource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordssensessource
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssensessource.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssensessource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordssensessynonyms
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssensessynonyms.Setsource(AIndex : Integer; AValue : TDictlayerdatadictwordssensessynonymssource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssensessynonyms.Settext(AIndex : Integer; AValue : string); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordssensessynonymssource
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssensessynonymssource.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssensessynonymssource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDictlayerdatadictwordssource
  --------------------------------------------------------------------}


Procedure TDictlayerdatadictwordssource.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDictlayerdatadictwordssource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadAccessRestriction
  --------------------------------------------------------------------}


Procedure TDownloadAccessRestriction.SetdeviceAllowed(AIndex : Integer; AValue : boolean); 

begin
  If (FdeviceAllowed=AValue) then exit;
  FdeviceAllowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetdownloadsAcquired(AIndex : Integer; AValue : integer); 

begin
  If (FdownloadsAcquired=AValue) then exit;
  FdownloadsAcquired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetjustAcquired(AIndex : Integer; AValue : boolean); 

begin
  If (FjustAcquired=AValue) then exit;
  FjustAcquired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetmaxDownloadDevices(AIndex : Integer; AValue : integer); 

begin
  If (FmaxDownloadDevices=AValue) then exit;
  FmaxDownloadDevices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setnonce(AIndex : Integer; AValue : string); 

begin
  If (Fnonce=AValue) then exit;
  Fnonce:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetreasonCode(AIndex : Integer; AValue : string); 

begin
  If (FreasonCode=AValue) then exit;
  FreasonCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setrestricted(AIndex : Integer; AValue : boolean); 

begin
  If (Frestricted=AValue) then exit;
  Frestricted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setsignature(AIndex : Integer; AValue : string); 

begin
  If (Fsignature=AValue) then exit;
  Fsignature:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccessRestriction.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadAccesses
  --------------------------------------------------------------------}


Procedure TDownloadAccesses.SetdownloadAccessList(AIndex : Integer; AValue : TDownloadAccessesdownloadAccessList); 

begin
  If (FdownloadAccessList=AValue) then exit;
  FdownloadAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccesses.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadAccessesdownloadAccessList
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeolayerdata
  --------------------------------------------------------------------}


Procedure TGeolayerdata.Setcommon(AIndex : Integer; AValue : TGeolayerdatacommon); 

begin
  If (Fcommon=AValue) then exit;
  Fcommon:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdata.Setgeo(AIndex : Integer; AValue : TGeolayerdatageo); 

begin
  If (Fgeo=AValue) then exit;
  Fgeo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdata.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdatacommon
  --------------------------------------------------------------------}


Procedure TGeolayerdatacommon.Setlang(AIndex : Integer; AValue : string); 

begin
  If (Flang=AValue) then exit;
  Flang:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatacommon.SetpreviewImageUrl(AIndex : Integer; AValue : string); 

begin
  If (FpreviewImageUrl=AValue) then exit;
  FpreviewImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatacommon.Setsnippet(AIndex : Integer; AValue : string); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatacommon.SetsnippetUrl(AIndex : Integer; AValue : string); 

begin
  If (FsnippetUrl=AValue) then exit;
  FsnippetUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatacommon.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdatageo
  --------------------------------------------------------------------}


Procedure TGeolayerdatageo.Setboundary(AIndex : Integer; AValue : TGeolayerdatageoboundary); 

begin
  If (Fboundary=AValue) then exit;
  Fboundary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageo.SetcachePolicy(AIndex : Integer; AValue : string); 

begin
  If (FcachePolicy=AValue) then exit;
  FcachePolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageo.SetcountryCode(AIndex : Integer; AValue : string); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageo.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageo.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageo.SetmapType(AIndex : Integer; AValue : string); 

begin
  If (FmapType=AValue) then exit;
  FmapType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageo.Setviewport(AIndex : Integer; AValue : TGeolayerdatageoviewport); 

begin
  If (Fviewport=AValue) then exit;
  Fviewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageo.Setzoom(AIndex : Integer; AValue : integer); 

begin
  If (Fzoom=AValue) then exit;
  Fzoom:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdatageoboundary
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeolayerdatageoviewport
  --------------------------------------------------------------------}


Procedure TGeolayerdatageoviewport.Sethi(AIndex : Integer; AValue : TGeolayerdatageoviewporthi); 

begin
  If (Fhi=AValue) then exit;
  Fhi:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageoviewport.Setlo(AIndex : Integer; AValue : TGeolayerdatageoviewportlo); 

begin
  If (Flo=AValue) then exit;
  Flo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdatageoviewporthi
  --------------------------------------------------------------------}


Procedure TGeolayerdatageoviewporthi.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageoviewporthi.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeolayerdatageoviewportlo
  --------------------------------------------------------------------}


Procedure TGeolayerdatageoviewportlo.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeolayerdatageoviewportlo.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLayersummaries
  --------------------------------------------------------------------}


Procedure TLayersummaries.Setitems(AIndex : Integer; AValue : TLayersummariesitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummaries.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummaries.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLayersummariesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLayersummary
  --------------------------------------------------------------------}


Procedure TLayersummary.SetannotationCount(AIndex : Integer; AValue : integer); 

begin
  If (FannotationCount=AValue) then exit;
  FannotationCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetannotationTypes(AIndex : Integer; AValue : TLayersummaryannotationTypes); 

begin
  If (FannotationTypes=AValue) then exit;
  FannotationTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetannotationsDataLink(AIndex : Integer; AValue : string); 

begin
  If (FannotationsDataLink=AValue) then exit;
  FannotationsDataLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetannotationsLink(AIndex : Integer; AValue : string); 

begin
  If (FannotationsLink=AValue) then exit;
  FannotationsLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetcontentVersion(AIndex : Integer; AValue : string); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetdataCount(AIndex : Integer; AValue : integer); 

begin
  If (FdataCount=AValue) then exit;
  FdataCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetlayerId(AIndex : Integer; AValue : string); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetvolumeAnnotationsVersion(AIndex : Integer; AValue : string); 

begin
  If (FvolumeAnnotationsVersion=AValue) then exit;
  FvolumeAnnotationsVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersummary.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLayersummaryannotationTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.Setitems(AIndex : Integer; AValue : TMetadataitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetadataitems
  --------------------------------------------------------------------}


Procedure TMetadataitems.Setdownload_url(AIndex : Integer; AValue : string); 

begin
  If (Fdownload_url=AValue) then exit;
  Fdownload_url:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataitems.Setencrypted_key(AIndex : Integer; AValue : string); 

begin
  If (Fencrypted_key=AValue) then exit;
  Fencrypted_key:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataitems.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataitems.Setsize(AIndex : Integer; AValue : string); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataitems.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOffers
  --------------------------------------------------------------------}


Procedure TOffers.Setitems(AIndex : Integer; AValue : TOffersitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffers.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOffersitems
  --------------------------------------------------------------------}


Procedure TOffersitems.SetartUrl(AIndex : Integer; AValue : string); 

begin
  If (FartUrl=AValue) then exit;
  FartUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersitems.SetgservicesKey(AIndex : Integer; AValue : string); 

begin
  If (FgservicesKey=AValue) then exit;
  FgservicesKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersitems.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersitems.Setitems(AIndex : Integer; AValue : TOffersitemsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOffersitemsitems
  --------------------------------------------------------------------}


Procedure TOffersitemsitems.Setauthor(AIndex : Integer; AValue : string); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersitemsitems.SetcanonicalVolumeLink(AIndex : Integer; AValue : string); 

begin
  If (FcanonicalVolumeLink=AValue) then exit;
  FcanonicalVolumeLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersitemsitems.SetcoverUrl(AIndex : Integer; AValue : string); 

begin
  If (FcoverUrl=AValue) then exit;
  FcoverUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersitemsitems.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersitemsitems.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffersitemsitems.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadingPosition
  --------------------------------------------------------------------}


Procedure TReadingPosition.SetepubCfiPosition(AIndex : Integer; AValue : string); 

begin
  If (FepubCfiPosition=AValue) then exit;
  FepubCfiPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.SetgbImagePosition(AIndex : Integer; AValue : string); 

begin
  If (FgbImagePosition=AValue) then exit;
  FgbImagePosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.SetgbTextPosition(AIndex : Integer; AValue : string); 

begin
  If (FgbTextPosition=AValue) then exit;
  FgbTextPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.SetpdfPosition(AIndex : Integer; AValue : string); 

begin
  If (FpdfPosition=AValue) then exit;
  FpdfPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadingPosition.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRequestAccess
  --------------------------------------------------------------------}


Procedure TRequestAccess.SetconcurrentAccess(AIndex : Integer; AValue : TConcurrentAccessRestriction); 

begin
  If (FconcurrentAccess=AValue) then exit;
  FconcurrentAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestAccess.SetdownloadAccess(AIndex : Integer; AValue : TDownloadAccessRestriction); 

begin
  If (FdownloadAccess=AValue) then exit;
  FdownloadAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestAccess.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReview
  --------------------------------------------------------------------}


Procedure TReview.Setauthor(AIndex : Integer; AValue : TReviewauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setdate(AIndex : Integer; AValue : string); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.SetfullTextUrl(AIndex : Integer; AValue : string); 

begin
  If (FfullTextUrl=AValue) then exit;
  FfullTextUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setrating(AIndex : Integer; AValue : string); 

begin
  If (Frating=AValue) then exit;
  Frating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Setsource(AIndex : Integer; AValue : TReviewsource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReview.SetvolumeId(AIndex : Integer; AValue : string); 

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
  TReviewauthor
  --------------------------------------------------------------------}


Procedure TReviewauthor.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReviewsource
  --------------------------------------------------------------------}


Procedure TReviewsource.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReviewsource.SetextraDescription(AIndex : Integer; AValue : string); 

begin
  If (FextraDescription=AValue) then exit;
  FextraDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReviewsource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersettings
  --------------------------------------------------------------------}


Procedure TUsersettings.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsersettings.SetnotesExport(AIndex : Integer; AValue : TUsersettingsnotesExport); 

begin
  If (FnotesExport=AValue) then exit;
  FnotesExport:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersettingsnotesExport
  --------------------------------------------------------------------}


Procedure TUsersettingsnotesExport.SetfolderName(AIndex : Integer; AValue : string); 

begin
  If (FfolderName=AValue) then exit;
  FfolderName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsersettingsnotesExport.SetisEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FisEnabled=AValue) then exit;
  FisEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolume
  --------------------------------------------------------------------}


Procedure TVolume.SetaccessInfo(AIndex : Integer; AValue : TVolumeaccessInfo); 

begin
  If (FaccessInfo=AValue) then exit;
  FaccessInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetlayerInfo(AIndex : Integer; AValue : TVolumelayerInfo); 

begin
  If (FlayerInfo=AValue) then exit;
  FlayerInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetrecommendedInfo(AIndex : Integer; AValue : TVolumerecommendedInfo); 

begin
  If (FrecommendedInfo=AValue) then exit;
  FrecommendedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetsaleInfo(AIndex : Integer; AValue : TVolumesaleInfo); 

begin
  If (FsaleInfo=AValue) then exit;
  FsaleInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetsearchInfo(AIndex : Integer; AValue : TVolumesearchInfo); 

begin
  If (FsearchInfo=AValue) then exit;
  FsearchInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetuserInfo(AIndex : Integer; AValue : TVolumeuserInfo); 

begin
  If (FuserInfo=AValue) then exit;
  FuserInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume.SetvolumeInfo(AIndex : Integer; AValue : TVolumevolumeInfo); 

begin
  If (FvolumeInfo=AValue) then exit;
  FvolumeInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeaccessInfo
  --------------------------------------------------------------------}


Procedure TVolumeaccessInfo.SetaccessViewStatus(AIndex : Integer; AValue : string); 

begin
  If (FaccessViewStatus=AValue) then exit;
  FaccessViewStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.SetdownloadAccess(AIndex : Integer; AValue : TDownloadAccessRestriction); 

begin
  If (FdownloadAccess=AValue) then exit;
  FdownloadAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.SetdriveImportedContentLink(AIndex : Integer; AValue : string); 

begin
  If (FdriveImportedContentLink=AValue) then exit;
  FdriveImportedContentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.Setembeddable(AIndex : Integer; AValue : boolean); 

begin
  If (Fembeddable=AValue) then exit;
  Fembeddable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.Setepub(AIndex : Integer; AValue : TVolumeaccessInfoepub); 

begin
  If (Fepub=AValue) then exit;
  Fepub:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.SetexplicitOfflineLicenseManagement(AIndex : Integer; AValue : boolean); 

begin
  If (FexplicitOfflineLicenseManagement=AValue) then exit;
  FexplicitOfflineLicenseManagement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.Setpdf(AIndex : Integer; AValue : TVolumeaccessInfopdf); 

begin
  If (Fpdf=AValue) then exit;
  Fpdf:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.SetpublicDomain(AIndex : Integer; AValue : boolean); 

begin
  If (FpublicDomain=AValue) then exit;
  FpublicDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.SetquoteSharingAllowed(AIndex : Integer; AValue : boolean); 

begin
  If (FquoteSharingAllowed=AValue) then exit;
  FquoteSharingAllowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.SettextToSpeechPermission(AIndex : Integer; AValue : string); 

begin
  If (FtextToSpeechPermission=AValue) then exit;
  FtextToSpeechPermission:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.SetviewOrderUrl(AIndex : Integer; AValue : string); 

begin
  If (FviewOrderUrl=AValue) then exit;
  FviewOrderUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.Setviewability(AIndex : Integer; AValue : string); 

begin
  If (Fviewability=AValue) then exit;
  Fviewability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfo.SetwebReaderLink(AIndex : Integer; AValue : string); 

begin
  If (FwebReaderLink=AValue) then exit;
  FwebReaderLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeaccessInfoepub
  --------------------------------------------------------------------}


Procedure TVolumeaccessInfoepub.SetacsTokenLink(AIndex : Integer; AValue : string); 

begin
  If (FacsTokenLink=AValue) then exit;
  FacsTokenLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfoepub.SetdownloadLink(AIndex : Integer; AValue : string); 

begin
  If (FdownloadLink=AValue) then exit;
  FdownloadLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfoepub.SetisAvailable(AIndex : Integer; AValue : boolean); 

begin
  If (FisAvailable=AValue) then exit;
  FisAvailable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeaccessInfopdf
  --------------------------------------------------------------------}


Procedure TVolumeaccessInfopdf.SetacsTokenLink(AIndex : Integer; AValue : string); 

begin
  If (FacsTokenLink=AValue) then exit;
  FacsTokenLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfopdf.SetdownloadLink(AIndex : Integer; AValue : string); 

begin
  If (FdownloadLink=AValue) then exit;
  FdownloadLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeaccessInfopdf.SetisAvailable(AIndex : Integer; AValue : boolean); 

begin
  If (FisAvailable=AValue) then exit;
  FisAvailable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumelayerInfo
  --------------------------------------------------------------------}


Procedure TVolumelayerInfo.Setlayers(AIndex : Integer; AValue : TVolumelayerInfolayers); 

begin
  If (Flayers=AValue) then exit;
  Flayers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumelayerInfolayers
  --------------------------------------------------------------------}


Procedure TVolumelayerInfolayers.SetlayerId(AIndex : Integer; AValue : string); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumelayerInfolayers.SetvolumeAnnotationsVersion(AIndex : Integer; AValue : string); 

begin
  If (FvolumeAnnotationsVersion=AValue) then exit;
  FvolumeAnnotationsVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumerecommendedInfo
  --------------------------------------------------------------------}


Procedure TVolumerecommendedInfo.Setexplanation(AIndex : Integer; AValue : string); 

begin
  If (Fexplanation=AValue) then exit;
  Fexplanation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumesaleInfo
  --------------------------------------------------------------------}


Procedure TVolumesaleInfo.SetbuyLink(AIndex : Integer; AValue : string); 

begin
  If (FbuyLink=AValue) then exit;
  FbuyLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfo.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfo.SetisEbook(AIndex : Integer; AValue : boolean); 

begin
  If (FisEbook=AValue) then exit;
  FisEbook:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfo.SetlistPrice(AIndex : Integer; AValue : TVolumesaleInfolistPrice); 

begin
  If (FlistPrice=AValue) then exit;
  FlistPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfo.Setoffers(AIndex : Integer; AValue : TVolumesaleInfooffers); 

begin
  If (Foffers=AValue) then exit;
  Foffers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfo.SetonSaleDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FonSaleDate=AValue) then exit;
  FonSaleDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfo.SetretailPrice(AIndex : Integer; AValue : TVolumesaleInforetailPrice); 

begin
  If (FretailPrice=AValue) then exit;
  FretailPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfo.Setsaleability(AIndex : Integer; AValue : string); 

begin
  If (Fsaleability=AValue) then exit;
  Fsaleability:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumesaleInfolistPrice
  --------------------------------------------------------------------}


Procedure TVolumesaleInfolistPrice.Setamount(AIndex : Integer; AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfolistPrice.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumesaleInfooffers
  --------------------------------------------------------------------}


Procedure TVolumesaleInfooffers.SetfinskyOfferType(AIndex : Integer; AValue : integer); 

begin
  If (FfinskyOfferType=AValue) then exit;
  FfinskyOfferType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfooffers.SetlistPrice(AIndex : Integer; AValue : TVolumesaleInfoofferslistPrice); 

begin
  If (FlistPrice=AValue) then exit;
  FlistPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfooffers.SetrentalDuration(AIndex : Integer; AValue : TVolumesaleInfooffersrentalDuration); 

begin
  If (FrentalDuration=AValue) then exit;
  FrentalDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfooffers.SetretailPrice(AIndex : Integer; AValue : TVolumesaleInfooffersretailPrice); 

begin
  If (FretailPrice=AValue) then exit;
  FretailPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumesaleInfoofferslistPrice
  --------------------------------------------------------------------}


Procedure TVolumesaleInfoofferslistPrice.SetamountInMicros(AIndex : Integer; AValue : double); 

begin
  If (FamountInMicros=AValue) then exit;
  FamountInMicros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfoofferslistPrice.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumesaleInfooffersrentalDuration
  --------------------------------------------------------------------}


Procedure TVolumesaleInfooffersrentalDuration.Setcount(AIndex : Integer; AValue : double); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfooffersrentalDuration.Set_unit(AIndex : Integer; AValue : string); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVolumesaleInfooffersrentalDuration.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVolumesaleInfooffersretailPrice
  --------------------------------------------------------------------}


Procedure TVolumesaleInfooffersretailPrice.SetamountInMicros(AIndex : Integer; AValue : double); 

begin
  If (FamountInMicros=AValue) then exit;
  FamountInMicros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInfooffersretailPrice.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumesaleInforetailPrice
  --------------------------------------------------------------------}


Procedure TVolumesaleInforetailPrice.Setamount(AIndex : Integer; AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumesaleInforetailPrice.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumesearchInfo
  --------------------------------------------------------------------}


Procedure TVolumesearchInfo.SettextSnippet(AIndex : Integer; AValue : string); 

begin
  If (FtextSnippet=AValue) then exit;
  FtextSnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeuserInfo
  --------------------------------------------------------------------}


Procedure TVolumeuserInfo.Setcopy(AIndex : Integer; AValue : TVolumeuserInfocopy); 

begin
  If (Fcopy=AValue) then exit;
  Fcopy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.SetisInMyBooks(AIndex : Integer; AValue : boolean); 

begin
  If (FisInMyBooks=AValue) then exit;
  FisInMyBooks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.SetisPreordered(AIndex : Integer; AValue : boolean); 

begin
  If (FisPreordered=AValue) then exit;
  FisPreordered:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.SetisPurchased(AIndex : Integer; AValue : boolean); 

begin
  If (FisPurchased=AValue) then exit;
  FisPurchased:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.SetisUploaded(AIndex : Integer; AValue : boolean); 

begin
  If (FisUploaded=AValue) then exit;
  FisUploaded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.SetreadingPosition(AIndex : Integer; AValue : TReadingPosition); 

begin
  If (FreadingPosition=AValue) then exit;
  FreadingPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.SetrentalPeriod(AIndex : Integer; AValue : TVolumeuserInforentalPeriod); 

begin
  If (FrentalPeriod=AValue) then exit;
  FrentalPeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.SetrentalState(AIndex : Integer; AValue : string); 

begin
  If (FrentalState=AValue) then exit;
  FrentalState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.Setreview(AIndex : Integer; AValue : TReview); 

begin
  If (Freview=AValue) then exit;
  Freview:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfo.SetuserUploadedVolumeInfo(AIndex : Integer; AValue : TVolumeuserInfouserUploadedVolumeInfo); 

begin
  If (FuserUploadedVolumeInfo=AValue) then exit;
  FuserUploadedVolumeInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeuserInfocopy
  --------------------------------------------------------------------}


Procedure TVolumeuserInfocopy.SetallowedCharacterCount(AIndex : Integer; AValue : integer); 

begin
  If (FallowedCharacterCount=AValue) then exit;
  FallowedCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfocopy.SetlimitType(AIndex : Integer; AValue : string); 

begin
  If (FlimitType=AValue) then exit;
  FlimitType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfocopy.SetremainingCharacterCount(AIndex : Integer; AValue : integer); 

begin
  If (FremainingCharacterCount=AValue) then exit;
  FremainingCharacterCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInfocopy.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeuserInforentalPeriod
  --------------------------------------------------------------------}


Procedure TVolumeuserInforentalPeriod.SetendUtcSec(AIndex : Integer; AValue : string); 

begin
  If (FendUtcSec=AValue) then exit;
  FendUtcSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeuserInforentalPeriod.SetstartUtcSec(AIndex : Integer; AValue : string); 

begin
  If (FstartUtcSec=AValue) then exit;
  FstartUtcSec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeuserInfouserUploadedVolumeInfo
  --------------------------------------------------------------------}


Procedure TVolumeuserInfouserUploadedVolumeInfo.SetprocessingState(AIndex : Integer; AValue : string); 

begin
  If (FprocessingState=AValue) then exit;
  FprocessingState:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumevolumeInfo
  --------------------------------------------------------------------}


Procedure TVolumevolumeInfo.SetallowAnonLogging(AIndex : Integer; AValue : boolean); 

begin
  If (FallowAnonLogging=AValue) then exit;
  FallowAnonLogging:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.Setauthors(AIndex : Integer; AValue : TVolumevolumeInfoauthors); 

begin
  If (Fauthors=AValue) then exit;
  Fauthors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetaverageRating(AIndex : Integer; AValue : double); 

begin
  If (FaverageRating=AValue) then exit;
  FaverageRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetcanonicalVolumeLink(AIndex : Integer; AValue : string); 

begin
  If (FcanonicalVolumeLink=AValue) then exit;
  FcanonicalVolumeLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.Setcategories(AIndex : Integer; AValue : TVolumevolumeInfocategories); 

begin
  If (Fcategories=AValue) then exit;
  Fcategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetcontentVersion(AIndex : Integer; AValue : string); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.Setdimensions(AIndex : Integer; AValue : TVolumevolumeInfodimensions); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetimageLinks(AIndex : Integer; AValue : TVolumevolumeInfoimageLinks); 

begin
  If (FimageLinks=AValue) then exit;
  FimageLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetindustryIdentifiers(AIndex : Integer; AValue : TVolumevolumeInfoindustryIdentifiers); 

begin
  If (FindustryIdentifiers=AValue) then exit;
  FindustryIdentifiers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetinfoLink(AIndex : Integer; AValue : string); 

begin
  If (FinfoLink=AValue) then exit;
  FinfoLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetmainCategory(AIndex : Integer; AValue : string); 

begin
  If (FmainCategory=AValue) then exit;
  FmainCategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetmaturityRating(AIndex : Integer; AValue : string); 

begin
  If (FmaturityRating=AValue) then exit;
  FmaturityRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetpageCount(AIndex : Integer; AValue : integer); 

begin
  If (FpageCount=AValue) then exit;
  FpageCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetpreviewLink(AIndex : Integer; AValue : string); 

begin
  If (FpreviewLink=AValue) then exit;
  FpreviewLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetprintType(AIndex : Integer; AValue : string); 

begin
  If (FprintType=AValue) then exit;
  FprintType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetprintedPageCount(AIndex : Integer; AValue : integer); 

begin
  If (FprintedPageCount=AValue) then exit;
  FprintedPageCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetpublishedDate(AIndex : Integer; AValue : string); 

begin
  If (FpublishedDate=AValue) then exit;
  FpublishedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.Setpublisher(AIndex : Integer; AValue : string); 

begin
  If (Fpublisher=AValue) then exit;
  Fpublisher:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetratingsCount(AIndex : Integer; AValue : integer); 

begin
  If (FratingsCount=AValue) then exit;
  FratingsCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetreadingModes(AIndex : Integer; AValue : TJSONSchema); 

begin
  If (FreadingModes=AValue) then exit;
  FreadingModes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.SetsamplePageCount(AIndex : Integer; AValue : integer); 

begin
  If (FsamplePageCount=AValue) then exit;
  FsamplePageCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.Setsubtitle(AIndex : Integer; AValue : string); 

begin
  If (Fsubtitle=AValue) then exit;
  Fsubtitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfo.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumevolumeInfoauthors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVolumevolumeInfocategories
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVolumevolumeInfodimensions
  --------------------------------------------------------------------}


Procedure TVolumevolumeInfodimensions.Setheight(AIndex : Integer; AValue : string); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfodimensions.Setthickness(AIndex : Integer; AValue : string); 

begin
  If (Fthickness=AValue) then exit;
  Fthickness:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfodimensions.Setwidth(AIndex : Integer; AValue : string); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumevolumeInfoimageLinks
  --------------------------------------------------------------------}


Procedure TVolumevolumeInfoimageLinks.SetextraLarge(AIndex : Integer; AValue : string); 

begin
  If (FextraLarge=AValue) then exit;
  FextraLarge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfoimageLinks.Setlarge(AIndex : Integer; AValue : string); 

begin
  If (Flarge=AValue) then exit;
  Flarge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfoimageLinks.Setmedium(AIndex : Integer; AValue : string); 

begin
  If (Fmedium=AValue) then exit;
  Fmedium:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfoimageLinks.Setsmall(AIndex : Integer; AValue : string); 

begin
  If (Fsmall=AValue) then exit;
  Fsmall:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfoimageLinks.SetsmallThumbnail(AIndex : Integer; AValue : string); 

begin
  If (FsmallThumbnail=AValue) then exit;
  FsmallThumbnail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfoimageLinks.Setthumbnail(AIndex : Integer; AValue : string); 

begin
  If (Fthumbnail=AValue) then exit;
  Fthumbnail:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumevolumeInfoindustryIdentifiers
  --------------------------------------------------------------------}


Procedure TVolumevolumeInfoindustryIdentifiers.Setidentifier(AIndex : Integer; AValue : string); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumevolumeInfoindustryIdentifiers.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVolumevolumeInfoindustryIdentifiers.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVolume2
  --------------------------------------------------------------------}


Procedure TVolume2.Setitems(AIndex : Integer; AValue : TVolume2items); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume2.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolume2.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolume2items
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVolumeannotation
  --------------------------------------------------------------------}


Procedure TVolumeannotation.SetannotationDataId(AIndex : Integer; AValue : string); 

begin
  If (FannotationDataId=AValue) then exit;
  FannotationDataId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetannotationDataLink(AIndex : Integer; AValue : string); 

begin
  If (FannotationDataLink=AValue) then exit;
  FannotationDataLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetannotationType(AIndex : Integer; AValue : string); 

begin
  If (FannotationType=AValue) then exit;
  FannotationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetcontentRanges(AIndex : Integer; AValue : TVolumeannotationcontentRanges); 

begin
  If (FcontentRanges=AValue) then exit;
  FcontentRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setdata(AIndex : Integer; AValue : string); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetlayerId(AIndex : Integer; AValue : string); 

begin
  If (FlayerId=AValue) then exit;
  FlayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetpageIds(AIndex : Integer; AValue : TVolumeannotationpageIds); 

begin
  If (FpageIds=AValue) then exit;
  FpageIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetselectedText(AIndex : Integer; AValue : string); 

begin
  If (FselectedText=AValue) then exit;
  FselectedText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotation.SetvolumeId(AIndex : Integer; AValue : string); 

begin
  If (FvolumeId=AValue) then exit;
  FvolumeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeannotationcontentRanges
  --------------------------------------------------------------------}


Procedure TVolumeannotationcontentRanges.SetcfiRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FcfiRange=AValue) then exit;
  FcfiRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotationcontentRanges.SetcontentVersion(AIndex : Integer; AValue : string); 

begin
  If (FcontentVersion=AValue) then exit;
  FcontentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotationcontentRanges.SetgbImageRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FgbImageRange=AValue) then exit;
  FgbImageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotationcontentRanges.SetgbTextRange(AIndex : Integer; AValue : TBooksAnnotationsRange); 

begin
  If (FgbTextRange=AValue) then exit;
  FgbTextRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeannotationpageIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVolumeannotations
  --------------------------------------------------------------------}


Procedure TVolumeannotations.Setitems(AIndex : Integer; AValue : TVolumeannotationsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotations.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotations.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotations.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumeannotations.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumeannotationsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVolumes
  --------------------------------------------------------------------}


Procedure TVolumes.Setitems(AIndex : Integer; AValue : TVolumesitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumes.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVolumes.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVolumesitems
  --------------------------------------------------------------------}




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
  Result:='20150318';
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
  TAnnotation.RegisterObject;
  TAnnotationclientVersionRanges.RegisterObject;
  TAnnotationcurrentVersionRanges.RegisterObject;
  TAnnotationlayerSummary.RegisterObject;
  TAnnotationpageIds.RegisterObject;
  TAnnotationdata.RegisterObject;
  TAnnotations.RegisterObject;
  TAnnotationsitems.RegisterObject;
  TAnnotationsSummary.RegisterObject;
  TAnnotationsSummarylayers.RegisterObject;
  TAnnotationsdata.RegisterObject;
  TAnnotationsdataitems.RegisterObject;
  TBooksAnnotationsRange.RegisterObject;
  TBooksCloudloadingResource.RegisterObject;
  TBooksVolumesRecommendedRateResponse.RegisterObject;
  TBookshelf.RegisterObject;
  TBookshelves.RegisterObject;
  TBookshelvesitems.RegisterObject;
  TCategory.RegisterObject;
  TCategoryitems.RegisterObject;
  TConcurrentAccessRestriction.RegisterObject;
  TDictlayerdata.RegisterObject;
  TDictlayerdatacommon.RegisterObject;
  TDictlayerdatadict.RegisterObject;
  TDictlayerdatadictsource.RegisterObject;
  TDictlayerdatadictwords.RegisterObject;
  TDictlayerdatadictwordsderivatives.RegisterObject;
  TDictlayerdatadictwordsderivativessource.RegisterObject;
  TDictlayerdatadictwordsexamples.RegisterObject;
  TDictlayerdatadictwordsexamplessource.RegisterObject;
  TDictlayerdatadictwordssenses.RegisterObject;
  TDictlayerdatadictwordssensesconjugations.RegisterObject;
  TDictlayerdatadictwordssensesdefinitions.RegisterObject;
  TDictlayerdatadictwordssensesdefinitionsexamples.RegisterObject;
  TDictlayerdatadictwordssensesdefinitionsexamplessource.RegisterObject;
  TDictlayerdatadictwordssensessource.RegisterObject;
  TDictlayerdatadictwordssensessynonyms.RegisterObject;
  TDictlayerdatadictwordssensessynonymssource.RegisterObject;
  TDictlayerdatadictwordssource.RegisterObject;
  TDownloadAccessRestriction.RegisterObject;
  TDownloadAccesses.RegisterObject;
  TDownloadAccessesdownloadAccessList.RegisterObject;
  TGeolayerdata.RegisterObject;
  TGeolayerdatacommon.RegisterObject;
  TGeolayerdatageo.RegisterObject;
  TGeolayerdatageoboundary.RegisterObject;
  TGeolayerdatageoviewport.RegisterObject;
  TGeolayerdatageoviewporthi.RegisterObject;
  TGeolayerdatageoviewportlo.RegisterObject;
  TLayersummaries.RegisterObject;
  TLayersummariesitems.RegisterObject;
  TLayersummary.RegisterObject;
  TLayersummaryannotationTypes.RegisterObject;
  TMetadata.RegisterObject;
  TMetadataitems.RegisterObject;
  TOffers.RegisterObject;
  TOffersitems.RegisterObject;
  TOffersitemsitems.RegisterObject;
  TReadingPosition.RegisterObject;
  TRequestAccess.RegisterObject;
  TReview.RegisterObject;
  TReviewauthor.RegisterObject;
  TReviewsource.RegisterObject;
  TUsersettings.RegisterObject;
  TUsersettingsnotesExport.RegisterObject;
  TVolume.RegisterObject;
  TVolumeaccessInfo.RegisterObject;
  TVolumeaccessInfoepub.RegisterObject;
  TVolumeaccessInfopdf.RegisterObject;
  TVolumelayerInfo.RegisterObject;
  TVolumelayerInfolayers.RegisterObject;
  TVolumerecommendedInfo.RegisterObject;
  TVolumesaleInfo.RegisterObject;
  TVolumesaleInfolistPrice.RegisterObject;
  TVolumesaleInfooffers.RegisterObject;
  TVolumesaleInfoofferslistPrice.RegisterObject;
  TVolumesaleInfooffersrentalDuration.RegisterObject;
  TVolumesaleInfooffersretailPrice.RegisterObject;
  TVolumesaleInforetailPrice.RegisterObject;
  TVolumesearchInfo.RegisterObject;
  TVolumeuserInfo.RegisterObject;
  TVolumeuserInfocopy.RegisterObject;
  TVolumeuserInforentalPeriod.RegisterObject;
  TVolumeuserInfouserUploadedVolumeInfo.RegisterObject;
  TVolumevolumeInfo.RegisterObject;
  TVolumevolumeInfoauthors.RegisterObject;
  TVolumevolumeInfocategories.RegisterObject;
  TVolumevolumeInfodimensions.RegisterObject;
  TVolumevolumeInfoimageLinks.RegisterObject;
  TVolumevolumeInfoindustryIdentifiers.RegisterObject;
  TVolume2.RegisterObject;
  TVolume2items.RegisterObject;
  TVolumeannotation.RegisterObject;
  TVolumeannotationcontentRanges.RegisterObject;
  TVolumeannotationpageIds.RegisterObject;
  TVolumeannotations.RegisterObject;
  TVolumeannotationsitems.RegisterObject;
  TVolumes.RegisterObject;
  TVolumesitems.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TBooksAPI.RegisterAPI;
end.
