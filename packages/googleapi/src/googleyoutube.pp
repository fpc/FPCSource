unit googleyoutube;
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
//Generated on: 9-5-15 13:23:00
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccessPolicy = class;
  TActivity = class;
  TActivityContentDetails = class;
  TActivityContentDetailsBulletin = class;
  TActivityContentDetailsChannelItem = class;
  TActivityContentDetailsComment = class;
  TActivityContentDetailsFavorite = class;
  TActivityContentDetailsLike = class;
  TActivityContentDetailsPlaylistItem = class;
  TActivityContentDetailsPromotedItem = class;
  TActivityContentDetailsRecommendation = class;
  TActivityContentDetailsSocial = class;
  TActivityContentDetailsSubscription = class;
  TActivityContentDetailsUpload = class;
  TActivityListResponse = class;
  TActivitySnippet = class;
  TCaption = class;
  TCaptionListResponse = class;
  TCaptionSnippet = class;
  TCdnSettings = class;
  TChannel = class;
  TChannelAuditDetails = class;
  TChannelBannerResource = class;
  TChannelBrandingSettings = class;
  TChannelContentDetails = class;
  TChannelContentOwnerDetails = class;
  TChannelConversionPing = class;
  TChannelConversionPings = class;
  TChannelId = class;
  TChannelListResponse = class;
  TChannelLocalization = class;
  TChannelSection = class;
  TChannelSectionContentDetails = class;
  TChannelSectionListResponse = class;
  TChannelSectionLocalization = class;
  TChannelSectionSnippet = class;
  TChannelSectionTargeting = class;
  TChannelSettings = class;
  TChannelSnippet = class;
  TChannelStatistics = class;
  TChannelStatus = class;
  TChannelTopicDetails = class;
  TComment = class;
  TCommentListResponse = class;
  TCommentSnippet = class;
  TCommentThread = class;
  TCommentThreadListResponse = class;
  TCommentThreadReplies = class;
  TCommentThreadSnippet = class;
  TContentRating = class;
  TGeoPoint = class;
  TGuideCategory = class;
  TGuideCategoryListResponse = class;
  TGuideCategorySnippet = class;
  TI18nLanguage = class;
  TI18nLanguageListResponse = class;
  TI18nLanguageSnippet = class;
  TI18nRegion = class;
  TI18nRegionListResponse = class;
  TI18nRegionSnippet = class;
  TImageSettings = class;
  TIngestionInfo = class;
  TInvideoBranding = class;
  TInvideoPosition = class;
  TInvideoPromotion = class;
  TInvideoTiming = class;
  TLanguageTag = class;
  TLiveBroadcast = class;
  TLiveBroadcastContentDetails = class;
  TLiveBroadcastListResponse = class;
  TLiveBroadcastSnippet = class;
  TLiveBroadcastStatus = class;
  TLiveStream = class;
  TLiveStreamContentDetails = class;
  TLiveStreamListResponse = class;
  TLiveStreamSnippet = class;
  TLiveStreamStatus = class;
  TLocalizedProperty = class;
  TLocalizedString = class;
  TMonitorStreamInfo = class;
  TPageInfo = class;
  TPlaylist = class;
  TPlaylistContentDetails = class;
  TPlaylistItem = class;
  TPlaylistItemContentDetails = class;
  TPlaylistItemListResponse = class;
  TPlaylistItemSnippet = class;
  TPlaylistItemStatus = class;
  TPlaylistListResponse = class;
  TPlaylistLocalization = class;
  TPlaylistPlayer = class;
  TPlaylistSnippet = class;
  TPlaylistStatus = class;
  TPromotedItem = class;
  TPromotedItemId = class;
  TPropertyValue = class;
  TResourceId = class;
  TSearchListResponse = class;
  TSearchResult = class;
  TSearchResultSnippet = class;
  TSubscription = class;
  TSubscriptionContentDetails = class;
  TSubscriptionListResponse = class;
  TSubscriptionSnippet = class;
  TSubscriptionSubscriberSnippet = class;
  TThumbnail = class;
  TThumbnailDetails = class;
  TThumbnailSetResponse = class;
  TTokenPagination = class;
  TVideo = class;
  TVideoAbuseReport = class;
  TVideoAbuseReportReason = class;
  TVideoAbuseReportReasonListResponse = class;
  TVideoAbuseReportReasonSnippet = class;
  TVideoAbuseReportSecondaryReason = class;
  TVideoAgeGating = class;
  TVideoCategory = class;
  TVideoCategoryListResponse = class;
  TVideoCategorySnippet = class;
  TVideoContentDetails = class;
  TVideoContentDetailsRegionRestriction = class;
  TVideoConversionPing = class;
  TVideoConversionPings = class;
  TVideoFileDetails = class;
  TVideoFileDetailsAudioStream = class;
  TVideoFileDetailsVideoStream = class;
  TVideoGetRatingResponse = class;
  TVideoListResponse = class;
  TVideoLiveStreamingDetails = class;
  TVideoLocalization = class;
  TVideoMonetizationDetails = class;
  TVideoPlayer = class;
  TVideoProcessingDetails = class;
  TVideoProcessingDetailsProcessingProgress = class;
  TVideoProjectDetails = class;
  TVideoRating = class;
  TVideoRecordingDetails = class;
  TVideoSnippet = class;
  TVideoStatistics = class;
  TVideoStatus = class;
  TVideoSuggestions = class;
  TVideoSuggestionsTagSuggestion = class;
  TVideoTopicDetails = class;
  TWatchSettings = class;
  TAccessPolicyArray = Array of TAccessPolicy;
  TActivityArray = Array of TActivity;
  TActivityContentDetailsArray = Array of TActivityContentDetails;
  TActivityContentDetailsBulletinArray = Array of TActivityContentDetailsBulletin;
  TActivityContentDetailsChannelItemArray = Array of TActivityContentDetailsChannelItem;
  TActivityContentDetailsCommentArray = Array of TActivityContentDetailsComment;
  TActivityContentDetailsFavoriteArray = Array of TActivityContentDetailsFavorite;
  TActivityContentDetailsLikeArray = Array of TActivityContentDetailsLike;
  TActivityContentDetailsPlaylistItemArray = Array of TActivityContentDetailsPlaylistItem;
  TActivityContentDetailsPromotedItemArray = Array of TActivityContentDetailsPromotedItem;
  TActivityContentDetailsRecommendationArray = Array of TActivityContentDetailsRecommendation;
  TActivityContentDetailsSocialArray = Array of TActivityContentDetailsSocial;
  TActivityContentDetailsSubscriptionArray = Array of TActivityContentDetailsSubscription;
  TActivityContentDetailsUploadArray = Array of TActivityContentDetailsUpload;
  TActivityListResponseArray = Array of TActivityListResponse;
  TActivitySnippetArray = Array of TActivitySnippet;
  TCaptionArray = Array of TCaption;
  TCaptionListResponseArray = Array of TCaptionListResponse;
  TCaptionSnippetArray = Array of TCaptionSnippet;
  TCdnSettingsArray = Array of TCdnSettings;
  TChannelArray = Array of TChannel;
  TChannelAuditDetailsArray = Array of TChannelAuditDetails;
  TChannelBannerResourceArray = Array of TChannelBannerResource;
  TChannelBrandingSettingsArray = Array of TChannelBrandingSettings;
  TChannelContentDetailsArray = Array of TChannelContentDetails;
  TChannelContentOwnerDetailsArray = Array of TChannelContentOwnerDetails;
  TChannelConversionPingArray = Array of TChannelConversionPing;
  TChannelConversionPingsArray = Array of TChannelConversionPings;
  TChannelIdArray = Array of TChannelId;
  TChannelListResponseArray = Array of TChannelListResponse;
  TChannelLocalizationArray = Array of TChannelLocalization;
  TChannelSectionArray = Array of TChannelSection;
  TChannelSectionContentDetailsArray = Array of TChannelSectionContentDetails;
  TChannelSectionListResponseArray = Array of TChannelSectionListResponse;
  TChannelSectionLocalizationArray = Array of TChannelSectionLocalization;
  TChannelSectionSnippetArray = Array of TChannelSectionSnippet;
  TChannelSectionTargetingArray = Array of TChannelSectionTargeting;
  TChannelSettingsArray = Array of TChannelSettings;
  TChannelSnippetArray = Array of TChannelSnippet;
  TChannelStatisticsArray = Array of TChannelStatistics;
  TChannelStatusArray = Array of TChannelStatus;
  TChannelTopicDetailsArray = Array of TChannelTopicDetails;
  TCommentArray = Array of TComment;
  TCommentListResponseArray = Array of TCommentListResponse;
  TCommentSnippetArray = Array of TCommentSnippet;
  TCommentThreadArray = Array of TCommentThread;
  TCommentThreadListResponseArray = Array of TCommentThreadListResponse;
  TCommentThreadRepliesArray = Array of TCommentThreadReplies;
  TCommentThreadSnippetArray = Array of TCommentThreadSnippet;
  TContentRatingArray = Array of TContentRating;
  TGeoPointArray = Array of TGeoPoint;
  TGuideCategoryArray = Array of TGuideCategory;
  TGuideCategoryListResponseArray = Array of TGuideCategoryListResponse;
  TGuideCategorySnippetArray = Array of TGuideCategorySnippet;
  TI18nLanguageArray = Array of TI18nLanguage;
  TI18nLanguageListResponseArray = Array of TI18nLanguageListResponse;
  TI18nLanguageSnippetArray = Array of TI18nLanguageSnippet;
  TI18nRegionArray = Array of TI18nRegion;
  TI18nRegionListResponseArray = Array of TI18nRegionListResponse;
  TI18nRegionSnippetArray = Array of TI18nRegionSnippet;
  TImageSettingsArray = Array of TImageSettings;
  TIngestionInfoArray = Array of TIngestionInfo;
  TInvideoBrandingArray = Array of TInvideoBranding;
  TInvideoPositionArray = Array of TInvideoPosition;
  TInvideoPromotionArray = Array of TInvideoPromotion;
  TInvideoTimingArray = Array of TInvideoTiming;
  TLanguageTagArray = Array of TLanguageTag;
  TLiveBroadcastArray = Array of TLiveBroadcast;
  TLiveBroadcastContentDetailsArray = Array of TLiveBroadcastContentDetails;
  TLiveBroadcastListResponseArray = Array of TLiveBroadcastListResponse;
  TLiveBroadcastSnippetArray = Array of TLiveBroadcastSnippet;
  TLiveBroadcastStatusArray = Array of TLiveBroadcastStatus;
  TLiveStreamArray = Array of TLiveStream;
  TLiveStreamContentDetailsArray = Array of TLiveStreamContentDetails;
  TLiveStreamListResponseArray = Array of TLiveStreamListResponse;
  TLiveStreamSnippetArray = Array of TLiveStreamSnippet;
  TLiveStreamStatusArray = Array of TLiveStreamStatus;
  TLocalizedPropertyArray = Array of TLocalizedProperty;
  TLocalizedStringArray = Array of TLocalizedString;
  TMonitorStreamInfoArray = Array of TMonitorStreamInfo;
  TPageInfoArray = Array of TPageInfo;
  TPlaylistArray = Array of TPlaylist;
  TPlaylistContentDetailsArray = Array of TPlaylistContentDetails;
  TPlaylistItemArray = Array of TPlaylistItem;
  TPlaylistItemContentDetailsArray = Array of TPlaylistItemContentDetails;
  TPlaylistItemListResponseArray = Array of TPlaylistItemListResponse;
  TPlaylistItemSnippetArray = Array of TPlaylistItemSnippet;
  TPlaylistItemStatusArray = Array of TPlaylistItemStatus;
  TPlaylistListResponseArray = Array of TPlaylistListResponse;
  TPlaylistLocalizationArray = Array of TPlaylistLocalization;
  TPlaylistPlayerArray = Array of TPlaylistPlayer;
  TPlaylistSnippetArray = Array of TPlaylistSnippet;
  TPlaylistStatusArray = Array of TPlaylistStatus;
  TPromotedItemArray = Array of TPromotedItem;
  TPromotedItemIdArray = Array of TPromotedItemId;
  TPropertyValueArray = Array of TPropertyValue;
  TResourceIdArray = Array of TResourceId;
  TSearchListResponseArray = Array of TSearchListResponse;
  TSearchResultArray = Array of TSearchResult;
  TSearchResultSnippetArray = Array of TSearchResultSnippet;
  TSubscriptionArray = Array of TSubscription;
  TSubscriptionContentDetailsArray = Array of TSubscriptionContentDetails;
  TSubscriptionListResponseArray = Array of TSubscriptionListResponse;
  TSubscriptionSnippetArray = Array of TSubscriptionSnippet;
  TSubscriptionSubscriberSnippetArray = Array of TSubscriptionSubscriberSnippet;
  TThumbnailArray = Array of TThumbnail;
  TThumbnailDetailsArray = Array of TThumbnailDetails;
  TThumbnailSetResponseArray = Array of TThumbnailSetResponse;
  TTokenPaginationArray = Array of TTokenPagination;
  TVideoArray = Array of TVideo;
  TVideoAbuseReportArray = Array of TVideoAbuseReport;
  TVideoAbuseReportReasonArray = Array of TVideoAbuseReportReason;
  TVideoAbuseReportReasonListResponseArray = Array of TVideoAbuseReportReasonListResponse;
  TVideoAbuseReportReasonSnippetArray = Array of TVideoAbuseReportReasonSnippet;
  TVideoAbuseReportSecondaryReasonArray = Array of TVideoAbuseReportSecondaryReason;
  TVideoAgeGatingArray = Array of TVideoAgeGating;
  TVideoCategoryArray = Array of TVideoCategory;
  TVideoCategoryListResponseArray = Array of TVideoCategoryListResponse;
  TVideoCategorySnippetArray = Array of TVideoCategorySnippet;
  TVideoContentDetailsArray = Array of TVideoContentDetails;
  TVideoContentDetailsRegionRestrictionArray = Array of TVideoContentDetailsRegionRestriction;
  TVideoConversionPingArray = Array of TVideoConversionPing;
  TVideoConversionPingsArray = Array of TVideoConversionPings;
  TVideoFileDetailsArray = Array of TVideoFileDetails;
  TVideoFileDetailsAudioStreamArray = Array of TVideoFileDetailsAudioStream;
  TVideoFileDetailsVideoStreamArray = Array of TVideoFileDetailsVideoStream;
  TVideoGetRatingResponseArray = Array of TVideoGetRatingResponse;
  TVideoListResponseArray = Array of TVideoListResponse;
  TVideoLiveStreamingDetailsArray = Array of TVideoLiveStreamingDetails;
  TVideoLocalizationArray = Array of TVideoLocalization;
  TVideoMonetizationDetailsArray = Array of TVideoMonetizationDetails;
  TVideoPlayerArray = Array of TVideoPlayer;
  TVideoProcessingDetailsArray = Array of TVideoProcessingDetails;
  TVideoProcessingDetailsProcessingProgressArray = Array of TVideoProcessingDetailsProcessingProgress;
  TVideoProjectDetailsArray = Array of TVideoProjectDetails;
  TVideoRatingArray = Array of TVideoRating;
  TVideoRecordingDetailsArray = Array of TVideoRecordingDetails;
  TVideoSnippetArray = Array of TVideoSnippet;
  TVideoStatisticsArray = Array of TVideoStatistics;
  TVideoStatusArray = Array of TVideoStatus;
  TVideoSuggestionsArray = Array of TVideoSuggestions;
  TVideoSuggestionsTagSuggestionArray = Array of TVideoSuggestionsTagSuggestion;
  TVideoTopicDetailsArray = Array of TVideoTopicDetails;
  TWatchSettingsArray = Array of TWatchSettings;
  //Anonymous types, using auto-generated names
  TChannelTypelocalizations = class;
  TChannelContentDetailsTyperelatedPlaylists = class;
  TChannelSectionTypelocalizations = class;
  TPlaylistTypelocalizations = class;
  TVideoTypelocalizations = class;
  TActivityListResponseTypeitemsArray = Array of TActivity;
  TCaptionListResponseTypeitemsArray = Array of TCaption;
  TChannelBrandingSettingsTypehintsArray = Array of TPropertyValue;
  TChannelConversionPingsTypepingsArray = Array of TChannelConversionPing;
  TChannelListResponseTypeitemsArray = Array of TChannel;
  TChannelSectionListResponseTypeitemsArray = Array of TChannelSection;
  TCommentListResponseTypeitemsArray = Array of TComment;
  TCommentThreadListResponseTypeitemsArray = Array of TCommentThread;
  TCommentThreadRepliesTypecommentsArray = Array of TComment;
  TGuideCategoryListResponseTypeitemsArray = Array of TGuideCategory;
  TI18nLanguageListResponseTypeitemsArray = Array of TI18nLanguage;
  TI18nRegionListResponseTypeitemsArray = Array of TI18nRegion;
  TInvideoPromotionTypeitemsArray = Array of TPromotedItem;
  TLiveBroadcastListResponseTypeitemsArray = Array of TLiveBroadcast;
  TLiveStreamListResponseTypeitemsArray = Array of TLiveStream;
  TLocalizedPropertyTypelocalizedArray = Array of TLocalizedString;
  TPlaylistItemListResponseTypeitemsArray = Array of TPlaylistItem;
  TPlaylistListResponseTypeitemsArray = Array of TPlaylist;
  TSearchListResponseTypeitemsArray = Array of TSearchResult;
  TSubscriptionListResponseTypeitemsArray = Array of TSubscription;
  TThumbnailSetResponseTypeitemsArray = Array of TThumbnailDetails;
  TVideoAbuseReportReasonListResponseTypeitemsArray = Array of TVideoAbuseReportReason;
  TVideoAbuseReportReasonSnippetTypesecondaryReasonsArray = Array of TVideoAbuseReportSecondaryReason;
  TVideoCategoryListResponseTypeitemsArray = Array of TVideoCategory;
  TVideoConversionPingsTypepingsArray = Array of TVideoConversionPing;
  TVideoFileDetailsTypeaudioStreamsArray = Array of TVideoFileDetailsAudioStream;
  TVideoFileDetailsTypevideoStreamsArray = Array of TVideoFileDetailsVideoStream;
  TVideoGetRatingResponseTypeitemsArray = Array of TVideoRating;
  TVideoListResponseTypeitemsArray = Array of TVideo;
  TVideoSuggestionsTypetagSuggestionsArray = Array of TVideoSuggestionsTagSuggestion;
  
  { --------------------------------------------------------------------
    TAccessPolicy
    --------------------------------------------------------------------}
  
  TAccessPolicy = Class(TGoogleBaseObject)
  Private
    Fallowed : boolean;
    Fexception : TStringArray;
  Protected
    //Property setters
    Procedure Setallowed(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setexception(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property allowed : boolean Index 0 Read Fallowed Write Setallowed;
    Property exception : TStringArray Index 8 Read Fexception Write Setexception;
  end;
  TAccessPolicyClass = Class of TAccessPolicy;
  
  { --------------------------------------------------------------------
    TActivity
    --------------------------------------------------------------------}
  
  TActivity = Class(TGoogleBaseObject)
  Private
    FcontentDetails : TActivityContentDetails;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TActivitySnippet;
  Protected
    //Property setters
    Procedure SetcontentDetails(AIndex : Integer; AValue : TActivityContentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TActivitySnippet); virtual;
  Public
  Published
    Property contentDetails : TActivityContentDetails Index 0 Read FcontentDetails Write SetcontentDetails;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property snippet : TActivitySnippet Index 32 Read Fsnippet Write Setsnippet;
  end;
  TActivityClass = Class of TActivity;
  
  { --------------------------------------------------------------------
    TActivityContentDetails
    --------------------------------------------------------------------}
  
  TActivityContentDetails = Class(TGoogleBaseObject)
  Private
    Fbulletin : TActivityContentDetailsBulletin;
    FchannelItem : TActivityContentDetailsChannelItem;
    Fcomment : TActivityContentDetailsComment;
    Ffavorite : TActivityContentDetailsFavorite;
    Flike : TActivityContentDetailsLike;
    FplaylistItem : TActivityContentDetailsPlaylistItem;
    FpromotedItem : TActivityContentDetailsPromotedItem;
    Frecommendation : TActivityContentDetailsRecommendation;
    Fsocial : TActivityContentDetailsSocial;
    Fsubscription : TActivityContentDetailsSubscription;
    Fupload : TActivityContentDetailsUpload;
  Protected
    //Property setters
    Procedure Setbulletin(AIndex : Integer; AValue : TActivityContentDetailsBulletin); virtual;
    Procedure SetchannelItem(AIndex : Integer; AValue : TActivityContentDetailsChannelItem); virtual;
    Procedure Setcomment(AIndex : Integer; AValue : TActivityContentDetailsComment); virtual;
    Procedure Setfavorite(AIndex : Integer; AValue : TActivityContentDetailsFavorite); virtual;
    Procedure Setlike(AIndex : Integer; AValue : TActivityContentDetailsLike); virtual;
    Procedure SetplaylistItem(AIndex : Integer; AValue : TActivityContentDetailsPlaylistItem); virtual;
    Procedure SetpromotedItem(AIndex : Integer; AValue : TActivityContentDetailsPromotedItem); virtual;
    Procedure Setrecommendation(AIndex : Integer; AValue : TActivityContentDetailsRecommendation); virtual;
    Procedure Setsocial(AIndex : Integer; AValue : TActivityContentDetailsSocial); virtual;
    Procedure Setsubscription(AIndex : Integer; AValue : TActivityContentDetailsSubscription); virtual;
    Procedure Setupload(AIndex : Integer; AValue : TActivityContentDetailsUpload); virtual;
  Public
  Published
    Property bulletin : TActivityContentDetailsBulletin Index 0 Read Fbulletin Write Setbulletin;
    Property channelItem : TActivityContentDetailsChannelItem Index 8 Read FchannelItem Write SetchannelItem;
    Property comment : TActivityContentDetailsComment Index 16 Read Fcomment Write Setcomment;
    Property favorite : TActivityContentDetailsFavorite Index 24 Read Ffavorite Write Setfavorite;
    Property like : TActivityContentDetailsLike Index 32 Read Flike Write Setlike;
    Property playlistItem : TActivityContentDetailsPlaylistItem Index 40 Read FplaylistItem Write SetplaylistItem;
    Property promotedItem : TActivityContentDetailsPromotedItem Index 48 Read FpromotedItem Write SetpromotedItem;
    Property recommendation : TActivityContentDetailsRecommendation Index 56 Read Frecommendation Write Setrecommendation;
    Property social : TActivityContentDetailsSocial Index 64 Read Fsocial Write Setsocial;
    Property subscription : TActivityContentDetailsSubscription Index 72 Read Fsubscription Write Setsubscription;
    Property upload : TActivityContentDetailsUpload Index 80 Read Fupload Write Setupload;
  end;
  TActivityContentDetailsClass = Class of TActivityContentDetails;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsBulletin
    --------------------------------------------------------------------}
  
  TActivityContentDetailsBulletin = Class(TGoogleBaseObject)
  Private
    FresourceId : TResourceId;
  Protected
    //Property setters
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
  Public
  Published
    Property resourceId : TResourceId Index 0 Read FresourceId Write SetresourceId;
  end;
  TActivityContentDetailsBulletinClass = Class of TActivityContentDetailsBulletin;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsChannelItem
    --------------------------------------------------------------------}
  
  TActivityContentDetailsChannelItem = Class(TGoogleBaseObject)
  Private
    FresourceId : TResourceId;
  Protected
    //Property setters
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
  Public
  Published
    Property resourceId : TResourceId Index 0 Read FresourceId Write SetresourceId;
  end;
  TActivityContentDetailsChannelItemClass = Class of TActivityContentDetailsChannelItem;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsComment
    --------------------------------------------------------------------}
  
  TActivityContentDetailsComment = Class(TGoogleBaseObject)
  Private
    FresourceId : TResourceId;
  Protected
    //Property setters
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
  Public
  Published
    Property resourceId : TResourceId Index 0 Read FresourceId Write SetresourceId;
  end;
  TActivityContentDetailsCommentClass = Class of TActivityContentDetailsComment;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsFavorite
    --------------------------------------------------------------------}
  
  TActivityContentDetailsFavorite = Class(TGoogleBaseObject)
  Private
    FresourceId : TResourceId;
  Protected
    //Property setters
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
  Public
  Published
    Property resourceId : TResourceId Index 0 Read FresourceId Write SetresourceId;
  end;
  TActivityContentDetailsFavoriteClass = Class of TActivityContentDetailsFavorite;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsLike
    --------------------------------------------------------------------}
  
  TActivityContentDetailsLike = Class(TGoogleBaseObject)
  Private
    FresourceId : TResourceId;
  Protected
    //Property setters
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
  Public
  Published
    Property resourceId : TResourceId Index 0 Read FresourceId Write SetresourceId;
  end;
  TActivityContentDetailsLikeClass = Class of TActivityContentDetailsLike;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsPlaylistItem
    --------------------------------------------------------------------}
  
  TActivityContentDetailsPlaylistItem = Class(TGoogleBaseObject)
  Private
    FplaylistId : String;
    FplaylistItemId : String;
    FresourceId : TResourceId;
  Protected
    //Property setters
    Procedure SetplaylistId(AIndex : Integer; AValue : String); virtual;
    Procedure SetplaylistItemId(AIndex : Integer; AValue : String); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
  Public
  Published
    Property playlistId : String Index 0 Read FplaylistId Write SetplaylistId;
    Property playlistItemId : String Index 8 Read FplaylistItemId Write SetplaylistItemId;
    Property resourceId : TResourceId Index 16 Read FresourceId Write SetresourceId;
  end;
  TActivityContentDetailsPlaylistItemClass = Class of TActivityContentDetailsPlaylistItem;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsPromotedItem
    --------------------------------------------------------------------}
  
  TActivityContentDetailsPromotedItem = Class(TGoogleBaseObject)
  Private
    FadTag : String;
    FclickTrackingUrl : String;
    FcreativeViewUrl : String;
    FctaType : String;
    FcustomCtaButtonText : String;
    FdescriptionText : String;
    FdestinationUrl : String;
    FforecastingUrl : TStringArray;
    FimpressionUrl : TStringArray;
    FvideoId : String;
  Protected
    //Property setters
    Procedure SetadTag(AIndex : Integer; AValue : String); virtual;
    Procedure SetclickTrackingUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreativeViewUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetctaType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomCtaButtonText(AIndex : Integer; AValue : String); virtual;
    Procedure SetdescriptionText(AIndex : Integer; AValue : String); virtual;
    Procedure SetdestinationUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetforecastingUrl(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetimpressionUrl(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adTag : String Index 0 Read FadTag Write SetadTag;
    Property clickTrackingUrl : String Index 8 Read FclickTrackingUrl Write SetclickTrackingUrl;
    Property creativeViewUrl : String Index 16 Read FcreativeViewUrl Write SetcreativeViewUrl;
    Property ctaType : String Index 24 Read FctaType Write SetctaType;
    Property customCtaButtonText : String Index 32 Read FcustomCtaButtonText Write SetcustomCtaButtonText;
    Property descriptionText : String Index 40 Read FdescriptionText Write SetdescriptionText;
    Property destinationUrl : String Index 48 Read FdestinationUrl Write SetdestinationUrl;
    Property forecastingUrl : TStringArray Index 56 Read FforecastingUrl Write SetforecastingUrl;
    Property impressionUrl : TStringArray Index 64 Read FimpressionUrl Write SetimpressionUrl;
    Property videoId : String Index 72 Read FvideoId Write SetvideoId;
  end;
  TActivityContentDetailsPromotedItemClass = Class of TActivityContentDetailsPromotedItem;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsRecommendation
    --------------------------------------------------------------------}
  
  TActivityContentDetailsRecommendation = Class(TGoogleBaseObject)
  Private
    Freason : String;
    FresourceId : TResourceId;
    FseedResourceId : TResourceId;
  Protected
    //Property setters
    Procedure Setreason(AIndex : Integer; AValue : String); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
    Procedure SetseedResourceId(AIndex : Integer; AValue : TResourceId); virtual;
  Public
  Published
    Property reason : String Index 0 Read Freason Write Setreason;
    Property resourceId : TResourceId Index 8 Read FresourceId Write SetresourceId;
    Property seedResourceId : TResourceId Index 16 Read FseedResourceId Write SetseedResourceId;
  end;
  TActivityContentDetailsRecommendationClass = Class of TActivityContentDetailsRecommendation;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsSocial
    --------------------------------------------------------------------}
  
  TActivityContentDetailsSocial = Class(TGoogleBaseObject)
  Private
    Fauthor : String;
    FimageUrl : String;
    FreferenceUrl : String;
    FresourceId : TResourceId;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : String); virtual;
    Procedure SetimageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property author : String Index 0 Read Fauthor Write Setauthor;
    Property imageUrl : String Index 8 Read FimageUrl Write SetimageUrl;
    Property referenceUrl : String Index 16 Read FreferenceUrl Write SetreferenceUrl;
    Property resourceId : TResourceId Index 24 Read FresourceId Write SetresourceId;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TActivityContentDetailsSocialClass = Class of TActivityContentDetailsSocial;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsSubscription
    --------------------------------------------------------------------}
  
  TActivityContentDetailsSubscription = Class(TGoogleBaseObject)
  Private
    FresourceId : TResourceId;
  Protected
    //Property setters
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
  Public
  Published
    Property resourceId : TResourceId Index 0 Read FresourceId Write SetresourceId;
  end;
  TActivityContentDetailsSubscriptionClass = Class of TActivityContentDetailsSubscription;
  
  { --------------------------------------------------------------------
    TActivityContentDetailsUpload
    --------------------------------------------------------------------}
  
  TActivityContentDetailsUpload = Class(TGoogleBaseObject)
  Private
    FvideoId : String;
  Protected
    //Property setters
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property videoId : String Index 0 Read FvideoId Write SetvideoId;
  end;
  TActivityContentDetailsUploadClass = Class of TActivityContentDetailsUpload;
  
  { --------------------------------------------------------------------
    TActivityListResponse
    --------------------------------------------------------------------}
  
  TActivityListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TActivityListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TActivityListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TActivityListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TActivityListResponseClass = Class of TActivityListResponse;
  
  { --------------------------------------------------------------------
    TActivitySnippet
    --------------------------------------------------------------------}
  
  TActivitySnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    FchannelTitle : String;
    Fdescription : String;
    FgroupId : String;
    FpublishedAt : TDatetime;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchannelTitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetgroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property channelTitle : String Index 8 Read FchannelTitle Write SetchannelTitle;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property groupId : String Index 24 Read FgroupId Write SetgroupId;
    Property publishedAt : TDatetime Index 32 Read FpublishedAt Write SetpublishedAt;
    Property thumbnails : TThumbnailDetails Index 40 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 48 Read Ftitle Write Settitle;
    Property _type : String Index 56 Read F_type Write Set_type;
  end;
  TActivitySnippetClass = Class of TActivitySnippet;
  
  { --------------------------------------------------------------------
    TCaption
    --------------------------------------------------------------------}
  
  TCaption = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TCaptionSnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TCaptionSnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property snippet : TCaptionSnippet Index 24 Read Fsnippet Write Setsnippet;
  end;
  TCaptionClass = Class of TCaption;
  
  { --------------------------------------------------------------------
    TCaptionListResponse
    --------------------------------------------------------------------}
  
  TCaptionListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TCaptionListResponseTypeitemsArray;
    Fkind : String;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCaptionListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TCaptionListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property visitorId : String Index 32 Read FvisitorId Write SetvisitorId;
  end;
  TCaptionListResponseClass = Class of TCaptionListResponse;
  
  { --------------------------------------------------------------------
    TCaptionSnippet
    --------------------------------------------------------------------}
  
  TCaptionSnippet = Class(TGoogleBaseObject)
  Private
    FaudioTrackType : String;
    FfailureReason : String;
    FisAutoSynced : boolean;
    FisCC : boolean;
    FisDraft : boolean;
    FisEasyReader : boolean;
    FisLarge : boolean;
    Flanguage : String;
    FlastUpdated : TDatetime;
    Fname : String;
    Fstatus : String;
    FtrackKind : String;
    FvideoId : String;
  Protected
    //Property setters
    Procedure SetaudioTrackType(AIndex : Integer; AValue : String); virtual;
    Procedure SetfailureReason(AIndex : Integer; AValue : String); virtual;
    Procedure SetisAutoSynced(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisCC(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisDraft(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisEasyReader(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisLarge(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastUpdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SettrackKind(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property audioTrackType : String Index 0 Read FaudioTrackType Write SetaudioTrackType;
    Property failureReason : String Index 8 Read FfailureReason Write SetfailureReason;
    Property isAutoSynced : boolean Index 16 Read FisAutoSynced Write SetisAutoSynced;
    Property isCC : boolean Index 24 Read FisCC Write SetisCC;
    Property isDraft : boolean Index 32 Read FisDraft Write SetisDraft;
    Property isEasyReader : boolean Index 40 Read FisEasyReader Write SetisEasyReader;
    Property isLarge : boolean Index 48 Read FisLarge Write SetisLarge;
    Property language : String Index 56 Read Flanguage Write Setlanguage;
    Property lastUpdated : TDatetime Index 64 Read FlastUpdated Write SetlastUpdated;
    Property name : String Index 72 Read Fname Write Setname;
    Property status : String Index 80 Read Fstatus Write Setstatus;
    Property trackKind : String Index 88 Read FtrackKind Write SettrackKind;
    Property videoId : String Index 96 Read FvideoId Write SetvideoId;
  end;
  TCaptionSnippetClass = Class of TCaptionSnippet;
  
  { --------------------------------------------------------------------
    TCdnSettings
    --------------------------------------------------------------------}
  
  TCdnSettings = Class(TGoogleBaseObject)
  Private
    Fformat : String;
    FingestionInfo : TIngestionInfo;
    FingestionType : String;
  Protected
    //Property setters
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure SetingestionInfo(AIndex : Integer; AValue : TIngestionInfo); virtual;
    Procedure SetingestionType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property format : String Index 0 Read Fformat Write Setformat;
    Property ingestionInfo : TIngestionInfo Index 8 Read FingestionInfo Write SetingestionInfo;
    Property ingestionType : String Index 16 Read FingestionType Write SetingestionType;
  end;
  TCdnSettingsClass = Class of TCdnSettings;
  
  { --------------------------------------------------------------------
    TChannelTypelocalizations
    --------------------------------------------------------------------}
  
  TChannelTypelocalizations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TChannelTypelocalizationsClass = Class of TChannelTypelocalizations;
  
  { --------------------------------------------------------------------
    TChannel
    --------------------------------------------------------------------}
  
  TChannel = Class(TGoogleBaseObject)
  Private
    FauditDetails : TChannelAuditDetails;
    FbrandingSettings : TChannelBrandingSettings;
    FcontentDetails : TChannelContentDetails;
    FcontentOwnerDetails : TChannelContentOwnerDetails;
    FconversionPings : TChannelConversionPings;
    Fetag : String;
    Fid : String;
    FinvideoPromotion : TInvideoPromotion;
    Fkind : String;
    Flocalizations : TChannelTypelocalizations;
    Fsnippet : TChannelSnippet;
    Fstatistics : TChannelStatistics;
    Fstatus : TChannelStatus;
    FtopicDetails : TChannelTopicDetails;
  Protected
    //Property setters
    Procedure SetauditDetails(AIndex : Integer; AValue : TChannelAuditDetails); virtual;
    Procedure SetbrandingSettings(AIndex : Integer; AValue : TChannelBrandingSettings); virtual;
    Procedure SetcontentDetails(AIndex : Integer; AValue : TChannelContentDetails); virtual;
    Procedure SetcontentOwnerDetails(AIndex : Integer; AValue : TChannelContentOwnerDetails); virtual;
    Procedure SetconversionPings(AIndex : Integer; AValue : TChannelConversionPings); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinvideoPromotion(AIndex : Integer; AValue : TInvideoPromotion); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocalizations(AIndex : Integer; AValue : TChannelTypelocalizations); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TChannelSnippet); virtual;
    Procedure Setstatistics(AIndex : Integer; AValue : TChannelStatistics); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TChannelStatus); virtual;
    Procedure SettopicDetails(AIndex : Integer; AValue : TChannelTopicDetails); virtual;
  Public
  Published
    Property auditDetails : TChannelAuditDetails Index 0 Read FauditDetails Write SetauditDetails;
    Property brandingSettings : TChannelBrandingSettings Index 8 Read FbrandingSettings Write SetbrandingSettings;
    Property contentDetails : TChannelContentDetails Index 16 Read FcontentDetails Write SetcontentDetails;
    Property contentOwnerDetails : TChannelContentOwnerDetails Index 24 Read FcontentOwnerDetails Write SetcontentOwnerDetails;
    Property conversionPings : TChannelConversionPings Index 32 Read FconversionPings Write SetconversionPings;
    Property etag : String Index 40 Read Fetag Write Setetag;
    Property id : String Index 48 Read Fid Write Setid;
    Property invideoPromotion : TInvideoPromotion Index 56 Read FinvideoPromotion Write SetinvideoPromotion;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property localizations : TChannelTypelocalizations Index 72 Read Flocalizations Write Setlocalizations;
    Property snippet : TChannelSnippet Index 80 Read Fsnippet Write Setsnippet;
    Property statistics : TChannelStatistics Index 88 Read Fstatistics Write Setstatistics;
    Property status : TChannelStatus Index 96 Read Fstatus Write Setstatus;
    Property topicDetails : TChannelTopicDetails Index 104 Read FtopicDetails Write SettopicDetails;
  end;
  TChannelClass = Class of TChannel;
  
  { --------------------------------------------------------------------
    TChannelAuditDetails
    --------------------------------------------------------------------}
  
  TChannelAuditDetails = Class(TGoogleBaseObject)
  Private
    FcommunityGuidelinesGoodStanding : boolean;
    FcontentIdClaimsGoodStanding : boolean;
    FcopyrightStrikesGoodStanding : boolean;
    FoverallGoodStanding : boolean;
  Protected
    //Property setters
    Procedure SetcommunityGuidelinesGoodStanding(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcontentIdClaimsGoodStanding(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcopyrightStrikesGoodStanding(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetoverallGoodStanding(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property communityGuidelinesGoodStanding : boolean Index 0 Read FcommunityGuidelinesGoodStanding Write SetcommunityGuidelinesGoodStanding;
    Property contentIdClaimsGoodStanding : boolean Index 8 Read FcontentIdClaimsGoodStanding Write SetcontentIdClaimsGoodStanding;
    Property copyrightStrikesGoodStanding : boolean Index 16 Read FcopyrightStrikesGoodStanding Write SetcopyrightStrikesGoodStanding;
    Property overallGoodStanding : boolean Index 24 Read FoverallGoodStanding Write SetoverallGoodStanding;
  end;
  TChannelAuditDetailsClass = Class of TChannelAuditDetails;
  
  { --------------------------------------------------------------------
    TChannelBannerResource
    --------------------------------------------------------------------}
  
  TChannelBannerResource = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fkind : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property url : String Index 16 Read Furl Write Seturl;
  end;
  TChannelBannerResourceClass = Class of TChannelBannerResource;
  
  { --------------------------------------------------------------------
    TChannelBrandingSettings
    --------------------------------------------------------------------}
  
  TChannelBrandingSettings = Class(TGoogleBaseObject)
  Private
    Fchannel : TChannelSettings;
    Fhints : TChannelBrandingSettingsTypehintsArray;
    Fimage : TImageSettings;
    Fwatch : TWatchSettings;
  Protected
    //Property setters
    Procedure Setchannel(AIndex : Integer; AValue : TChannelSettings); virtual;
    Procedure Sethints(AIndex : Integer; AValue : TChannelBrandingSettingsTypehintsArray); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TImageSettings); virtual;
    Procedure Setwatch(AIndex : Integer; AValue : TWatchSettings); virtual;
  Public
  Published
    Property channel : TChannelSettings Index 0 Read Fchannel Write Setchannel;
    Property hints : TChannelBrandingSettingsTypehintsArray Index 8 Read Fhints Write Sethints;
    Property image : TImageSettings Index 16 Read Fimage Write Setimage;
    Property watch : TWatchSettings Index 24 Read Fwatch Write Setwatch;
  end;
  TChannelBrandingSettingsClass = Class of TChannelBrandingSettings;
  
  { --------------------------------------------------------------------
    TChannelContentDetailsTyperelatedPlaylists
    --------------------------------------------------------------------}
  
  TChannelContentDetailsTyperelatedPlaylists = Class(TGoogleBaseObject)
  Private
    Ffavorites : String;
    Flikes : String;
    Fuploads : String;
    FwatchHistory : String;
    FwatchLater : String;
  Protected
    //Property setters
    Procedure Setfavorites(AIndex : Integer; AValue : String); virtual;
    Procedure Setlikes(AIndex : Integer; AValue : String); virtual;
    Procedure Setuploads(AIndex : Integer; AValue : String); virtual;
    Procedure SetwatchHistory(AIndex : Integer; AValue : String); virtual;
    Procedure SetwatchLater(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property favorites : String Index 0 Read Ffavorites Write Setfavorites;
    Property likes : String Index 8 Read Flikes Write Setlikes;
    Property uploads : String Index 16 Read Fuploads Write Setuploads;
    Property watchHistory : String Index 24 Read FwatchHistory Write SetwatchHistory;
    Property watchLater : String Index 32 Read FwatchLater Write SetwatchLater;
  end;
  TChannelContentDetailsTyperelatedPlaylistsClass = Class of TChannelContentDetailsTyperelatedPlaylists;
  
  { --------------------------------------------------------------------
    TChannelContentDetails
    --------------------------------------------------------------------}
  
  TChannelContentDetails = Class(TGoogleBaseObject)
  Private
    FgooglePlusUserId : String;
    FrelatedPlaylists : TChannelContentDetailsTyperelatedPlaylists;
  Protected
    //Property setters
    Procedure SetgooglePlusUserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrelatedPlaylists(AIndex : Integer; AValue : TChannelContentDetailsTyperelatedPlaylists); virtual;
  Public
  Published
    Property googlePlusUserId : String Index 0 Read FgooglePlusUserId Write SetgooglePlusUserId;
    Property relatedPlaylists : TChannelContentDetailsTyperelatedPlaylists Index 8 Read FrelatedPlaylists Write SetrelatedPlaylists;
  end;
  TChannelContentDetailsClass = Class of TChannelContentDetails;
  
  { --------------------------------------------------------------------
    TChannelContentOwnerDetails
    --------------------------------------------------------------------}
  
  TChannelContentOwnerDetails = Class(TGoogleBaseObject)
  Private
    FcontentOwner : String;
    FtimeLinked : TDatetime;
  Protected
    //Property setters
    Procedure SetcontentOwner(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeLinked(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property contentOwner : String Index 0 Read FcontentOwner Write SetcontentOwner;
    Property timeLinked : TDatetime Index 8 Read FtimeLinked Write SettimeLinked;
  end;
  TChannelContentOwnerDetailsClass = Class of TChannelContentOwnerDetails;
  
  { --------------------------------------------------------------------
    TChannelConversionPing
    --------------------------------------------------------------------}
  
  TChannelConversionPing = Class(TGoogleBaseObject)
  Private
    Fcontext : String;
    FconversionUrl : String;
  Protected
    //Property setters
    Procedure Setcontext(AIndex : Integer; AValue : String); virtual;
    Procedure SetconversionUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property context : String Index 0 Read Fcontext Write Setcontext;
    Property conversionUrl : String Index 8 Read FconversionUrl Write SetconversionUrl;
  end;
  TChannelConversionPingClass = Class of TChannelConversionPing;
  
  { --------------------------------------------------------------------
    TChannelConversionPings
    --------------------------------------------------------------------}
  
  TChannelConversionPings = Class(TGoogleBaseObject)
  Private
    Fpings : TChannelConversionPingsTypepingsArray;
  Protected
    //Property setters
    Procedure Setpings(AIndex : Integer; AValue : TChannelConversionPingsTypepingsArray); virtual;
  Public
  Published
    Property pings : TChannelConversionPingsTypepingsArray Index 0 Read Fpings Write Setpings;
  end;
  TChannelConversionPingsClass = Class of TChannelConversionPings;
  
  { --------------------------------------------------------------------
    TChannelId
    --------------------------------------------------------------------}
  
  TChannelId = Class(TGoogleBaseObject)
  Private
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property value : String Index 0 Read Fvalue Write Setvalue;
  end;
  TChannelIdClass = Class of TChannelId;
  
  { --------------------------------------------------------------------
    TChannelListResponse
    --------------------------------------------------------------------}
  
  TChannelListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TChannelListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TChannelListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TChannelListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TChannelListResponseClass = Class of TChannelListResponse;
  
  { --------------------------------------------------------------------
    TChannelLocalization
    --------------------------------------------------------------------}
  
  TChannelLocalization = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property title : String Index 8 Read Ftitle Write Settitle;
  end;
  TChannelLocalizationClass = Class of TChannelLocalization;
  
  { --------------------------------------------------------------------
    TChannelSectionTypelocalizations
    --------------------------------------------------------------------}
  
  TChannelSectionTypelocalizations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TChannelSectionTypelocalizationsClass = Class of TChannelSectionTypelocalizations;
  
  { --------------------------------------------------------------------
    TChannelSection
    --------------------------------------------------------------------}
  
  TChannelSection = Class(TGoogleBaseObject)
  Private
    FcontentDetails : TChannelSectionContentDetails;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Flocalizations : TChannelSectionTypelocalizations;
    Fsnippet : TChannelSectionSnippet;
    Ftargeting : TChannelSectionTargeting;
  Protected
    //Property setters
    Procedure SetcontentDetails(AIndex : Integer; AValue : TChannelSectionContentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocalizations(AIndex : Integer; AValue : TChannelSectionTypelocalizations); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TChannelSectionSnippet); virtual;
    Procedure Settargeting(AIndex : Integer; AValue : TChannelSectionTargeting); virtual;
  Public
  Published
    Property contentDetails : TChannelSectionContentDetails Index 0 Read FcontentDetails Write SetcontentDetails;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property localizations : TChannelSectionTypelocalizations Index 32 Read Flocalizations Write Setlocalizations;
    Property snippet : TChannelSectionSnippet Index 40 Read Fsnippet Write Setsnippet;
    Property targeting : TChannelSectionTargeting Index 48 Read Ftargeting Write Settargeting;
  end;
  TChannelSectionClass = Class of TChannelSection;
  
  { --------------------------------------------------------------------
    TChannelSectionContentDetails
    --------------------------------------------------------------------}
  
  TChannelSectionContentDetails = Class(TGoogleBaseObject)
  Private
    Fchannels : TStringArray;
    Fplaylists : TStringArray;
  Protected
    //Property setters
    Procedure Setchannels(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setplaylists(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property channels : TStringArray Index 0 Read Fchannels Write Setchannels;
    Property playlists : TStringArray Index 8 Read Fplaylists Write Setplaylists;
  end;
  TChannelSectionContentDetailsClass = Class of TChannelSectionContentDetails;
  
  { --------------------------------------------------------------------
    TChannelSectionListResponse
    --------------------------------------------------------------------}
  
  TChannelSectionListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TChannelSectionListResponseTypeitemsArray;
    Fkind : String;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TChannelSectionListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TChannelSectionListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property visitorId : String Index 32 Read FvisitorId Write SetvisitorId;
  end;
  TChannelSectionListResponseClass = Class of TChannelSectionListResponse;
  
  { --------------------------------------------------------------------
    TChannelSectionLocalization
    --------------------------------------------------------------------}
  
  TChannelSectionLocalization = Class(TGoogleBaseObject)
  Private
    Ftitle : String;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property title : String Index 0 Read Ftitle Write Settitle;
  end;
  TChannelSectionLocalizationClass = Class of TChannelSectionLocalization;
  
  { --------------------------------------------------------------------
    TChannelSectionSnippet
    --------------------------------------------------------------------}
  
  TChannelSectionSnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    FdefaultLanguage : String;
    Flocalized : TChannelSectionLocalization;
    Fposition : integer;
    Fstyle : String;
    Ftitle : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocalized(AIndex : Integer; AValue : TChannelSectionLocalization); virtual;
    Procedure Setposition(AIndex : Integer; AValue : integer); virtual;
    Procedure Setstyle(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property defaultLanguage : String Index 8 Read FdefaultLanguage Write SetdefaultLanguage;
    Property localized : TChannelSectionLocalization Index 16 Read Flocalized Write Setlocalized;
    Property position : integer Index 24 Read Fposition Write Setposition;
    Property style : String Index 32 Read Fstyle Write Setstyle;
    Property title : String Index 40 Read Ftitle Write Settitle;
    Property _type : String Index 48 Read F_type Write Set_type;
  end;
  TChannelSectionSnippetClass = Class of TChannelSectionSnippet;
  
  { --------------------------------------------------------------------
    TChannelSectionTargeting
    --------------------------------------------------------------------}
  
  TChannelSectionTargeting = Class(TGoogleBaseObject)
  Private
    Fcountries : TStringArray;
    Flanguages : TStringArray;
    Fregions : TStringArray;
  Protected
    //Property setters
    Procedure Setcountries(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setlanguages(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setregions(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property countries : TStringArray Index 0 Read Fcountries Write Setcountries;
    Property languages : TStringArray Index 8 Read Flanguages Write Setlanguages;
    Property regions : TStringArray Index 16 Read Fregions Write Setregions;
  end;
  TChannelSectionTargetingClass = Class of TChannelSectionTargeting;
  
  { --------------------------------------------------------------------
    TChannelSettings
    --------------------------------------------------------------------}
  
  TChannelSettings = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    FdefaultLanguage : String;
    FdefaultTab : String;
    Fdescription : String;
    FfeaturedChannelsTitle : String;
    FfeaturedChannelsUrls : TStringArray;
    Fkeywords : String;
    FmoderateComments : boolean;
    FprofileColor : String;
    FshowBrowseView : boolean;
    FshowRelatedChannels : boolean;
    Ftitle : String;
    FtrackingAnalyticsAccountId : String;
    FunsubscribedTrailer : String;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultTab(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetfeaturedChannelsTitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetfeaturedChannelsUrls(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkeywords(AIndex : Integer; AValue : String); virtual;
    Procedure SetmoderateComments(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetprofileColor(AIndex : Integer; AValue : String); virtual;
    Procedure SetshowBrowseView(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetshowRelatedChannels(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure SettrackingAnalyticsAccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetunsubscribedTrailer(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property defaultLanguage : String Index 8 Read FdefaultLanguage Write SetdefaultLanguage;
    Property defaultTab : String Index 16 Read FdefaultTab Write SetdefaultTab;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property featuredChannelsTitle : String Index 32 Read FfeaturedChannelsTitle Write SetfeaturedChannelsTitle;
    Property featuredChannelsUrls : TStringArray Index 40 Read FfeaturedChannelsUrls Write SetfeaturedChannelsUrls;
    Property keywords : String Index 48 Read Fkeywords Write Setkeywords;
    Property moderateComments : boolean Index 56 Read FmoderateComments Write SetmoderateComments;
    Property profileColor : String Index 64 Read FprofileColor Write SetprofileColor;
    Property showBrowseView : boolean Index 72 Read FshowBrowseView Write SetshowBrowseView;
    Property showRelatedChannels : boolean Index 80 Read FshowRelatedChannels Write SetshowRelatedChannels;
    Property title : String Index 88 Read Ftitle Write Settitle;
    Property trackingAnalyticsAccountId : String Index 96 Read FtrackingAnalyticsAccountId Write SettrackingAnalyticsAccountId;
    Property unsubscribedTrailer : String Index 104 Read FunsubscribedTrailer Write SetunsubscribedTrailer;
  end;
  TChannelSettingsClass = Class of TChannelSettings;
  
  { --------------------------------------------------------------------
    TChannelSnippet
    --------------------------------------------------------------------}
  
  TChannelSnippet = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    FdefaultLanguage : String;
    Fdescription : String;
    Flocalized : TChannelLocalization;
    FpublishedAt : TDatetime;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocalized(AIndex : Integer; AValue : TChannelLocalization); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property defaultLanguage : String Index 8 Read FdefaultLanguage Write SetdefaultLanguage;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property localized : TChannelLocalization Index 24 Read Flocalized Write Setlocalized;
    Property publishedAt : TDatetime Index 32 Read FpublishedAt Write SetpublishedAt;
    Property thumbnails : TThumbnailDetails Index 40 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 48 Read Ftitle Write Settitle;
  end;
  TChannelSnippetClass = Class of TChannelSnippet;
  
  { --------------------------------------------------------------------
    TChannelStatistics
    --------------------------------------------------------------------}
  
  TChannelStatistics = Class(TGoogleBaseObject)
  Private
    FcommentCount : String;
    FhiddenSubscriberCount : boolean;
    FsubscriberCount : String;
    FvideoCount : String;
    FviewCount : String;
  Protected
    //Property setters
    Procedure SetcommentCount(AIndex : Integer; AValue : String); virtual;
    Procedure SethiddenSubscriberCount(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsubscriberCount(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoCount(AIndex : Integer; AValue : String); virtual;
    Procedure SetviewCount(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property commentCount : String Index 0 Read FcommentCount Write SetcommentCount;
    Property hiddenSubscriberCount : boolean Index 8 Read FhiddenSubscriberCount Write SethiddenSubscriberCount;
    Property subscriberCount : String Index 16 Read FsubscriberCount Write SetsubscriberCount;
    Property videoCount : String Index 24 Read FvideoCount Write SetvideoCount;
    Property viewCount : String Index 32 Read FviewCount Write SetviewCount;
  end;
  TChannelStatisticsClass = Class of TChannelStatistics;
  
  { --------------------------------------------------------------------
    TChannelStatus
    --------------------------------------------------------------------}
  
  TChannelStatus = Class(TGoogleBaseObject)
  Private
    FisLinked : boolean;
    FlongUploadsStatus : String;
    FprivacyStatus : String;
  Protected
    //Property setters
    Procedure SetisLinked(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlongUploadsStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetprivacyStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property isLinked : boolean Index 0 Read FisLinked Write SetisLinked;
    Property longUploadsStatus : String Index 8 Read FlongUploadsStatus Write SetlongUploadsStatus;
    Property privacyStatus : String Index 16 Read FprivacyStatus Write SetprivacyStatus;
  end;
  TChannelStatusClass = Class of TChannelStatus;
  
  { --------------------------------------------------------------------
    TChannelTopicDetails
    --------------------------------------------------------------------}
  
  TChannelTopicDetails = Class(TGoogleBaseObject)
  Private
    FtopicIds : TStringArray;
  Protected
    //Property setters
    Procedure SettopicIds(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property topicIds : TStringArray Index 0 Read FtopicIds Write SettopicIds;
  end;
  TChannelTopicDetailsClass = Class of TChannelTopicDetails;
  
  { --------------------------------------------------------------------
    TComment
    --------------------------------------------------------------------}
  
  TComment = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TCommentSnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TCommentSnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property snippet : TCommentSnippet Index 24 Read Fsnippet Write Setsnippet;
  end;
  TCommentClass = Class of TComment;
  
  { --------------------------------------------------------------------
    TCommentListResponse
    --------------------------------------------------------------------}
  
  TCommentListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TCommentListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCommentListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TCommentListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property tokenPagination : TTokenPagination Index 48 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 56 Read FvisitorId Write SetvisitorId;
  end;
  TCommentListResponseClass = Class of TCommentListResponse;
  
  { --------------------------------------------------------------------
    TCommentSnippet
    --------------------------------------------------------------------}
  
  TCommentSnippet = Class(TGoogleBaseObject)
  Private
    FauthorChannelId : TChannelId;
    FauthorChannelUrl : String;
    FauthorDisplayName : String;
    FauthorGoogleplusProfileUrl : String;
    FauthorProfileImageUrl : String;
    FcanRate : boolean;
    FchannelId : String;
    FlikeCount : integer;
    FmoderationStatus : String;
    FparentId : String;
    FpublishedAt : TDatetime;
    FtextDisplay : String;
    FtextOriginal : String;
    FupdatedAt : TDatetime;
    FvideoId : String;
    FviewerRating : String;
  Protected
    //Property setters
    Procedure SetauthorChannelId(AIndex : Integer; AValue : TChannelId); virtual;
    Procedure SetauthorChannelUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetauthorDisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure SetauthorGoogleplusProfileUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetauthorProfileImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetcanRate(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetlikeCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmoderationStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SettextDisplay(AIndex : Integer; AValue : String); virtual;
    Procedure SettextOriginal(AIndex : Integer; AValue : String); virtual;
    Procedure SetupdatedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
    Procedure SetviewerRating(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property authorChannelId : TChannelId Index 0 Read FauthorChannelId Write SetauthorChannelId;
    Property authorChannelUrl : String Index 8 Read FauthorChannelUrl Write SetauthorChannelUrl;
    Property authorDisplayName : String Index 16 Read FauthorDisplayName Write SetauthorDisplayName;
    Property authorGoogleplusProfileUrl : String Index 24 Read FauthorGoogleplusProfileUrl Write SetauthorGoogleplusProfileUrl;
    Property authorProfileImageUrl : String Index 32 Read FauthorProfileImageUrl Write SetauthorProfileImageUrl;
    Property canRate : boolean Index 40 Read FcanRate Write SetcanRate;
    Property channelId : String Index 48 Read FchannelId Write SetchannelId;
    Property likeCount : integer Index 56 Read FlikeCount Write SetlikeCount;
    Property moderationStatus : String Index 64 Read FmoderationStatus Write SetmoderationStatus;
    Property parentId : String Index 72 Read FparentId Write SetparentId;
    Property publishedAt : TDatetime Index 80 Read FpublishedAt Write SetpublishedAt;
    Property textDisplay : String Index 88 Read FtextDisplay Write SettextDisplay;
    Property textOriginal : String Index 96 Read FtextOriginal Write SettextOriginal;
    Property updatedAt : TDatetime Index 104 Read FupdatedAt Write SetupdatedAt;
    Property videoId : String Index 112 Read FvideoId Write SetvideoId;
    Property viewerRating : String Index 120 Read FviewerRating Write SetviewerRating;
  end;
  TCommentSnippetClass = Class of TCommentSnippet;
  
  { --------------------------------------------------------------------
    TCommentThread
    --------------------------------------------------------------------}
  
  TCommentThread = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    Freplies : TCommentThreadReplies;
    Fsnippet : TCommentThreadSnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setreplies(AIndex : Integer; AValue : TCommentThreadReplies); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TCommentThreadSnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property replies : TCommentThreadReplies Index 24 Read Freplies Write Setreplies;
    Property snippet : TCommentThreadSnippet Index 32 Read Fsnippet Write Setsnippet;
  end;
  TCommentThreadClass = Class of TCommentThread;
  
  { --------------------------------------------------------------------
    TCommentThreadListResponse
    --------------------------------------------------------------------}
  
  TCommentThreadListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TCommentThreadListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCommentThreadListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TCommentThreadListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property tokenPagination : TTokenPagination Index 48 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 56 Read FvisitorId Write SetvisitorId;
  end;
  TCommentThreadListResponseClass = Class of TCommentThreadListResponse;
  
  { --------------------------------------------------------------------
    TCommentThreadReplies
    --------------------------------------------------------------------}
  
  TCommentThreadReplies = Class(TGoogleBaseObject)
  Private
    Fcomments : TCommentThreadRepliesTypecommentsArray;
  Protected
    //Property setters
    Procedure Setcomments(AIndex : Integer; AValue : TCommentThreadRepliesTypecommentsArray); virtual;
  Public
  Published
    Property comments : TCommentThreadRepliesTypecommentsArray Index 0 Read Fcomments Write Setcomments;
  end;
  TCommentThreadRepliesClass = Class of TCommentThreadReplies;
  
  { --------------------------------------------------------------------
    TCommentThreadSnippet
    --------------------------------------------------------------------}
  
  TCommentThreadSnippet = Class(TGoogleBaseObject)
  Private
    FcanReply : boolean;
    FchannelId : String;
    FisPublic : boolean;
    FtopLevelComment : TComment;
    FtotalReplyCount : integer;
    FvideoId : String;
  Protected
    //Property setters
    Procedure SetcanReply(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetisPublic(AIndex : Integer; AValue : boolean); virtual;
    Procedure SettopLevelComment(AIndex : Integer; AValue : TComment); virtual;
    Procedure SettotalReplyCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property canReply : boolean Index 0 Read FcanReply Write SetcanReply;
    Property channelId : String Index 8 Read FchannelId Write SetchannelId;
    Property isPublic : boolean Index 16 Read FisPublic Write SetisPublic;
    Property topLevelComment : TComment Index 24 Read FtopLevelComment Write SettopLevelComment;
    Property totalReplyCount : integer Index 32 Read FtotalReplyCount Write SettotalReplyCount;
    Property videoId : String Index 40 Read FvideoId Write SetvideoId;
  end;
  TCommentThreadSnippetClass = Class of TCommentThreadSnippet;
  
  { --------------------------------------------------------------------
    TContentRating
    --------------------------------------------------------------------}
  
  TContentRating = Class(TGoogleBaseObject)
  Private
    FacbRating : String;
    FagcomRating : String;
    FanatelRating : String;
    FbbfcRating : String;
    FbfvcRating : String;
    FbmukkRating : String;
    FcatvRating : String;
    FcatvfrRating : String;
    FcbfcRating : String;
    FcccRating : String;
    FcceRating : String;
    FchfilmRating : String;
    FchvrsRating : String;
    FcicfRating : String;
    FcnaRating : String;
    FcsaRating : String;
    FcscfRating : String;
    FczfilmRating : String;
    FdjctqRating : String;
    FdjctqRatingReasons : TStringArray;
    FeefilmRating : String;
    FegfilmRating : String;
    FeirinRating : String;
    FfcbmRating : String;
    FfcoRating : String;
    FfmocRating : String;
    FfpbRating : String;
    FfskRating : String;
    FgrfilmRating : String;
    FicaaRating : String;
    FifcoRating : String;
    FilfilmRating : String;
    FincaaRating : String;
    FkfcbRating : String;
    FkijkwijzerRating : String;
    FkmrbRating : String;
    FlsfRating : String;
    FmccaaRating : String;
    FmccypRating : String;
    FmdaRating : String;
    FmedietilsynetRating : String;
    FmekuRating : String;
    FmibacRating : String;
    F_mocRating : String;
    FmoctwRating : String;
    FmpaaRating : String;
    FmtrcbRating : String;
    FnbcRating : String;
    FnbcplRating : String;
    FnfrcRating : String;
    FnfvcbRating : String;
    FnkclvRating : String;
    FoflcRating : String;
    FpefilmRating : String;
    FrcnofRating : String;
    FresorteviolenciaRating : String;
    FrtcRating : String;
    FrteRating : String;
    FrussiaRating : String;
    FskfilmRating : String;
    FsmaisRating : String;
    FsmsaRating : String;
    FtvpgRating : String;
    FytRating : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetacbRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetagcomRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetanatelRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetbbfcRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetbfvcRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetbmukkRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcatvRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcatvfrRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcbfcRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcccRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcceRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetchfilmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetchvrsRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcicfRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcnaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcsaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetcscfRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetczfilmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetdjctqRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetdjctqRatingReasons(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SeteefilmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetegfilmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SeteirinRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetfcbmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetfcoRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetfmocRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetfpbRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetfskRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetgrfilmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SeticaaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetifcoRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetilfilmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetincaaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetkfcbRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetkijkwijzerRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetkmrbRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetlsfRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmccaaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmccypRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmdaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmedietilsynetRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmekuRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmibacRating(AIndex : Integer; AValue : String); virtual;
    Procedure Set_mocRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmoctwRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmpaaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetmtrcbRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetnbcRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetnbcplRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetnfrcRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetnfvcbRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetnkclvRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetoflcRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetpefilmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetrcnofRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetresorteviolenciaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetrtcRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetrteRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetrussiaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetskfilmRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetsmaisRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetsmsaRating(AIndex : Integer; AValue : String); virtual;
    Procedure SettvpgRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetytRating(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property acbRating : String Index 0 Read FacbRating Write SetacbRating;
    Property agcomRating : String Index 8 Read FagcomRating Write SetagcomRating;
    Property anatelRating : String Index 16 Read FanatelRating Write SetanatelRating;
    Property bbfcRating : String Index 24 Read FbbfcRating Write SetbbfcRating;
    Property bfvcRating : String Index 32 Read FbfvcRating Write SetbfvcRating;
    Property bmukkRating : String Index 40 Read FbmukkRating Write SetbmukkRating;
    Property catvRating : String Index 48 Read FcatvRating Write SetcatvRating;
    Property catvfrRating : String Index 56 Read FcatvfrRating Write SetcatvfrRating;
    Property cbfcRating : String Index 64 Read FcbfcRating Write SetcbfcRating;
    Property cccRating : String Index 72 Read FcccRating Write SetcccRating;
    Property cceRating : String Index 80 Read FcceRating Write SetcceRating;
    Property chfilmRating : String Index 88 Read FchfilmRating Write SetchfilmRating;
    Property chvrsRating : String Index 96 Read FchvrsRating Write SetchvrsRating;
    Property cicfRating : String Index 104 Read FcicfRating Write SetcicfRating;
    Property cnaRating : String Index 112 Read FcnaRating Write SetcnaRating;
    Property csaRating : String Index 120 Read FcsaRating Write SetcsaRating;
    Property cscfRating : String Index 128 Read FcscfRating Write SetcscfRating;
    Property czfilmRating : String Index 136 Read FczfilmRating Write SetczfilmRating;
    Property djctqRating : String Index 144 Read FdjctqRating Write SetdjctqRating;
    Property djctqRatingReasons : TStringArray Index 152 Read FdjctqRatingReasons Write SetdjctqRatingReasons;
    Property eefilmRating : String Index 160 Read FeefilmRating Write SeteefilmRating;
    Property egfilmRating : String Index 168 Read FegfilmRating Write SetegfilmRating;
    Property eirinRating : String Index 176 Read FeirinRating Write SeteirinRating;
    Property fcbmRating : String Index 184 Read FfcbmRating Write SetfcbmRating;
    Property fcoRating : String Index 192 Read FfcoRating Write SetfcoRating;
    Property fmocRating : String Index 200 Read FfmocRating Write SetfmocRating;
    Property fpbRating : String Index 208 Read FfpbRating Write SetfpbRating;
    Property fskRating : String Index 216 Read FfskRating Write SetfskRating;
    Property grfilmRating : String Index 224 Read FgrfilmRating Write SetgrfilmRating;
    Property icaaRating : String Index 232 Read FicaaRating Write SeticaaRating;
    Property ifcoRating : String Index 240 Read FifcoRating Write SetifcoRating;
    Property ilfilmRating : String Index 248 Read FilfilmRating Write SetilfilmRating;
    Property incaaRating : String Index 256 Read FincaaRating Write SetincaaRating;
    Property kfcbRating : String Index 264 Read FkfcbRating Write SetkfcbRating;
    Property kijkwijzerRating : String Index 272 Read FkijkwijzerRating Write SetkijkwijzerRating;
    Property kmrbRating : String Index 280 Read FkmrbRating Write SetkmrbRating;
    Property lsfRating : String Index 288 Read FlsfRating Write SetlsfRating;
    Property mccaaRating : String Index 296 Read FmccaaRating Write SetmccaaRating;
    Property mccypRating : String Index 304 Read FmccypRating Write SetmccypRating;
    Property mdaRating : String Index 312 Read FmdaRating Write SetmdaRating;
    Property medietilsynetRating : String Index 320 Read FmedietilsynetRating Write SetmedietilsynetRating;
    Property mekuRating : String Index 328 Read FmekuRating Write SetmekuRating;
    Property mibacRating : String Index 336 Read FmibacRating Write SetmibacRating;
    Property _mocRating : String Index 344 Read F_mocRating Write Set_mocRating;
    Property moctwRating : String Index 352 Read FmoctwRating Write SetmoctwRating;
    Property mpaaRating : String Index 360 Read FmpaaRating Write SetmpaaRating;
    Property mtrcbRating : String Index 368 Read FmtrcbRating Write SetmtrcbRating;
    Property nbcRating : String Index 376 Read FnbcRating Write SetnbcRating;
    Property nbcplRating : String Index 384 Read FnbcplRating Write SetnbcplRating;
    Property nfrcRating : String Index 392 Read FnfrcRating Write SetnfrcRating;
    Property nfvcbRating : String Index 400 Read FnfvcbRating Write SetnfvcbRating;
    Property nkclvRating : String Index 408 Read FnkclvRating Write SetnkclvRating;
    Property oflcRating : String Index 416 Read FoflcRating Write SetoflcRating;
    Property pefilmRating : String Index 424 Read FpefilmRating Write SetpefilmRating;
    Property rcnofRating : String Index 432 Read FrcnofRating Write SetrcnofRating;
    Property resorteviolenciaRating : String Index 440 Read FresorteviolenciaRating Write SetresorteviolenciaRating;
    Property rtcRating : String Index 448 Read FrtcRating Write SetrtcRating;
    Property rteRating : String Index 456 Read FrteRating Write SetrteRating;
    Property russiaRating : String Index 464 Read FrussiaRating Write SetrussiaRating;
    Property skfilmRating : String Index 472 Read FskfilmRating Write SetskfilmRating;
    Property smaisRating : String Index 480 Read FsmaisRating Write SetsmaisRating;
    Property smsaRating : String Index 488 Read FsmsaRating Write SetsmsaRating;
    Property tvpgRating : String Index 496 Read FtvpgRating Write SettvpgRating;
    Property ytRating : String Index 504 Read FytRating Write SetytRating;
  end;
  TContentRatingClass = Class of TContentRating;
  
  { --------------------------------------------------------------------
    TGeoPoint
    --------------------------------------------------------------------}
  
  TGeoPoint = Class(TGoogleBaseObject)
  Private
    Faltitude : double;
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setaltitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property altitude : double Index 0 Read Faltitude Write Setaltitude;
    Property latitude : double Index 8 Read Flatitude Write Setlatitude;
    Property longitude : double Index 16 Read Flongitude Write Setlongitude;
  end;
  TGeoPointClass = Class of TGeoPoint;
  
  { --------------------------------------------------------------------
    TGuideCategory
    --------------------------------------------------------------------}
  
  TGuideCategory = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TGuideCategorySnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TGuideCategorySnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property snippet : TGuideCategorySnippet Index 24 Read Fsnippet Write Setsnippet;
  end;
  TGuideCategoryClass = Class of TGuideCategory;
  
  { --------------------------------------------------------------------
    TGuideCategoryListResponse
    --------------------------------------------------------------------}
  
  TGuideCategoryListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TGuideCategoryListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TGuideCategoryListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TGuideCategoryListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TGuideCategoryListResponseClass = Class of TGuideCategoryListResponse;
  
  { --------------------------------------------------------------------
    TGuideCategorySnippet
    --------------------------------------------------------------------}
  
  TGuideCategorySnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property title : String Index 8 Read Ftitle Write Settitle;
  end;
  TGuideCategorySnippetClass = Class of TGuideCategorySnippet;
  
  { --------------------------------------------------------------------
    TI18nLanguage
    --------------------------------------------------------------------}
  
  TI18nLanguage = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TI18nLanguageSnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TI18nLanguageSnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property snippet : TI18nLanguageSnippet Index 24 Read Fsnippet Write Setsnippet;
  end;
  TI18nLanguageClass = Class of TI18nLanguage;
  
  { --------------------------------------------------------------------
    TI18nLanguageListResponse
    --------------------------------------------------------------------}
  
  TI18nLanguageListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TI18nLanguageListResponseTypeitemsArray;
    Fkind : String;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TI18nLanguageListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TI18nLanguageListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property visitorId : String Index 32 Read FvisitorId Write SetvisitorId;
  end;
  TI18nLanguageListResponseClass = Class of TI18nLanguageListResponse;
  
  { --------------------------------------------------------------------
    TI18nLanguageSnippet
    --------------------------------------------------------------------}
  
  TI18nLanguageSnippet = Class(TGoogleBaseObject)
  Private
    Fhl : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Sethl(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property hl : String Index 0 Read Fhl Write Sethl;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TI18nLanguageSnippetClass = Class of TI18nLanguageSnippet;
  
  { --------------------------------------------------------------------
    TI18nRegion
    --------------------------------------------------------------------}
  
  TI18nRegion = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TI18nRegionSnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TI18nRegionSnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property snippet : TI18nRegionSnippet Index 24 Read Fsnippet Write Setsnippet;
  end;
  TI18nRegionClass = Class of TI18nRegion;
  
  { --------------------------------------------------------------------
    TI18nRegionListResponse
    --------------------------------------------------------------------}
  
  TI18nRegionListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TI18nRegionListResponseTypeitemsArray;
    Fkind : String;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TI18nRegionListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TI18nRegionListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property visitorId : String Index 32 Read FvisitorId Write SetvisitorId;
  end;
  TI18nRegionListResponseClass = Class of TI18nRegionListResponse;
  
  { --------------------------------------------------------------------
    TI18nRegionSnippet
    --------------------------------------------------------------------}
  
  TI18nRegionSnippet = Class(TGoogleBaseObject)
  Private
    Fgl : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setgl(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property gl : String Index 0 Read Fgl Write Setgl;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TI18nRegionSnippetClass = Class of TI18nRegionSnippet;
  
  { --------------------------------------------------------------------
    TImageSettings
    --------------------------------------------------------------------}
  
  TImageSettings = Class(TGoogleBaseObject)
  Private
    FbackgroundImageUrl : TLocalizedProperty;
    FbannerExternalUrl : String;
    FbannerImageUrl : String;
    FbannerMobileExtraHdImageUrl : String;
    FbannerMobileHdImageUrl : String;
    FbannerMobileImageUrl : String;
    FbannerMobileLowImageUrl : String;
    FbannerMobileMediumHdImageUrl : String;
    FbannerTabletExtraHdImageUrl : String;
    FbannerTabletHdImageUrl : String;
    FbannerTabletImageUrl : String;
    FbannerTabletLowImageUrl : String;
    FbannerTvHighImageUrl : String;
    FbannerTvImageUrl : String;
    FbannerTvLowImageUrl : String;
    FbannerTvMediumImageUrl : String;
    FlargeBrandedBannerImageImapScript : TLocalizedProperty;
    FlargeBrandedBannerImageUrl : TLocalizedProperty;
    FsmallBrandedBannerImageImapScript : TLocalizedProperty;
    FsmallBrandedBannerImageUrl : TLocalizedProperty;
    FtrackingImageUrl : String;
    FwatchIconImageUrl : String;
  Protected
    //Property setters
    Procedure SetbackgroundImageUrl(AIndex : Integer; AValue : TLocalizedProperty); virtual;
    Procedure SetbannerExternalUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerMobileExtraHdImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerMobileHdImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerMobileImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerMobileLowImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerMobileMediumHdImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerTabletExtraHdImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerTabletHdImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerTabletImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerTabletLowImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerTvHighImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerTvImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerTvLowImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerTvMediumImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetlargeBrandedBannerImageImapScript(AIndex : Integer; AValue : TLocalizedProperty); virtual;
    Procedure SetlargeBrandedBannerImageUrl(AIndex : Integer; AValue : TLocalizedProperty); virtual;
    Procedure SetsmallBrandedBannerImageImapScript(AIndex : Integer; AValue : TLocalizedProperty); virtual;
    Procedure SetsmallBrandedBannerImageUrl(AIndex : Integer; AValue : TLocalizedProperty); virtual;
    Procedure SettrackingImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetwatchIconImageUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property backgroundImageUrl : TLocalizedProperty Index 0 Read FbackgroundImageUrl Write SetbackgroundImageUrl;
    Property bannerExternalUrl : String Index 8 Read FbannerExternalUrl Write SetbannerExternalUrl;
    Property bannerImageUrl : String Index 16 Read FbannerImageUrl Write SetbannerImageUrl;
    Property bannerMobileExtraHdImageUrl : String Index 24 Read FbannerMobileExtraHdImageUrl Write SetbannerMobileExtraHdImageUrl;
    Property bannerMobileHdImageUrl : String Index 32 Read FbannerMobileHdImageUrl Write SetbannerMobileHdImageUrl;
    Property bannerMobileImageUrl : String Index 40 Read FbannerMobileImageUrl Write SetbannerMobileImageUrl;
    Property bannerMobileLowImageUrl : String Index 48 Read FbannerMobileLowImageUrl Write SetbannerMobileLowImageUrl;
    Property bannerMobileMediumHdImageUrl : String Index 56 Read FbannerMobileMediumHdImageUrl Write SetbannerMobileMediumHdImageUrl;
    Property bannerTabletExtraHdImageUrl : String Index 64 Read FbannerTabletExtraHdImageUrl Write SetbannerTabletExtraHdImageUrl;
    Property bannerTabletHdImageUrl : String Index 72 Read FbannerTabletHdImageUrl Write SetbannerTabletHdImageUrl;
    Property bannerTabletImageUrl : String Index 80 Read FbannerTabletImageUrl Write SetbannerTabletImageUrl;
    Property bannerTabletLowImageUrl : String Index 88 Read FbannerTabletLowImageUrl Write SetbannerTabletLowImageUrl;
    Property bannerTvHighImageUrl : String Index 96 Read FbannerTvHighImageUrl Write SetbannerTvHighImageUrl;
    Property bannerTvImageUrl : String Index 104 Read FbannerTvImageUrl Write SetbannerTvImageUrl;
    Property bannerTvLowImageUrl : String Index 112 Read FbannerTvLowImageUrl Write SetbannerTvLowImageUrl;
    Property bannerTvMediumImageUrl : String Index 120 Read FbannerTvMediumImageUrl Write SetbannerTvMediumImageUrl;
    Property largeBrandedBannerImageImapScript : TLocalizedProperty Index 128 Read FlargeBrandedBannerImageImapScript Write SetlargeBrandedBannerImageImapScript;
    Property largeBrandedBannerImageUrl : TLocalizedProperty Index 136 Read FlargeBrandedBannerImageUrl Write SetlargeBrandedBannerImageUrl;
    Property smallBrandedBannerImageImapScript : TLocalizedProperty Index 144 Read FsmallBrandedBannerImageImapScript Write SetsmallBrandedBannerImageImapScript;
    Property smallBrandedBannerImageUrl : TLocalizedProperty Index 152 Read FsmallBrandedBannerImageUrl Write SetsmallBrandedBannerImageUrl;
    Property trackingImageUrl : String Index 160 Read FtrackingImageUrl Write SettrackingImageUrl;
    Property watchIconImageUrl : String Index 168 Read FwatchIconImageUrl Write SetwatchIconImageUrl;
  end;
  TImageSettingsClass = Class of TImageSettings;
  
  { --------------------------------------------------------------------
    TIngestionInfo
    --------------------------------------------------------------------}
  
  TIngestionInfo = Class(TGoogleBaseObject)
  Private
    FbackupIngestionAddress : String;
    FingestionAddress : String;
    FstreamName : String;
  Protected
    //Property setters
    Procedure SetbackupIngestionAddress(AIndex : Integer; AValue : String); virtual;
    Procedure SetingestionAddress(AIndex : Integer; AValue : String); virtual;
    Procedure SetstreamName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property backupIngestionAddress : String Index 0 Read FbackupIngestionAddress Write SetbackupIngestionAddress;
    Property ingestionAddress : String Index 8 Read FingestionAddress Write SetingestionAddress;
    Property streamName : String Index 16 Read FstreamName Write SetstreamName;
  end;
  TIngestionInfoClass = Class of TIngestionInfo;
  
  { --------------------------------------------------------------------
    TInvideoBranding
    --------------------------------------------------------------------}
  
  TInvideoBranding = Class(TGoogleBaseObject)
  Private
    FimageBytes : String;
    FimageUrl : String;
    Fposition : TInvideoPosition;
    FtargetChannelId : String;
    Ftiming : TInvideoTiming;
  Protected
    //Property setters
    Procedure SetimageBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SetimageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TInvideoPosition); virtual;
    Procedure SettargetChannelId(AIndex : Integer; AValue : String); virtual;
    Procedure Settiming(AIndex : Integer; AValue : TInvideoTiming); virtual;
  Public
  Published
    Property imageBytes : String Index 0 Read FimageBytes Write SetimageBytes;
    Property imageUrl : String Index 8 Read FimageUrl Write SetimageUrl;
    Property position : TInvideoPosition Index 16 Read Fposition Write Setposition;
    Property targetChannelId : String Index 24 Read FtargetChannelId Write SettargetChannelId;
    Property timing : TInvideoTiming Index 32 Read Ftiming Write Settiming;
  end;
  TInvideoBrandingClass = Class of TInvideoBranding;
  
  { --------------------------------------------------------------------
    TInvideoPosition
    --------------------------------------------------------------------}
  
  TInvideoPosition = Class(TGoogleBaseObject)
  Private
    FcornerPosition : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcornerPosition(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property cornerPosition : String Index 0 Read FcornerPosition Write SetcornerPosition;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TInvideoPositionClass = Class of TInvideoPosition;
  
  { --------------------------------------------------------------------
    TInvideoPromotion
    --------------------------------------------------------------------}
  
  TInvideoPromotion = Class(TGoogleBaseObject)
  Private
    FdefaultTiming : TInvideoTiming;
    Fitems : TInvideoPromotionTypeitemsArray;
    Fposition : TInvideoPosition;
    FuseSmartTiming : boolean;
  Protected
    //Property setters
    Procedure SetdefaultTiming(AIndex : Integer; AValue : TInvideoTiming); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TInvideoPromotionTypeitemsArray); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TInvideoPosition); virtual;
    Procedure SetuseSmartTiming(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property defaultTiming : TInvideoTiming Index 0 Read FdefaultTiming Write SetdefaultTiming;
    Property items : TInvideoPromotionTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property position : TInvideoPosition Index 16 Read Fposition Write Setposition;
    Property useSmartTiming : boolean Index 24 Read FuseSmartTiming Write SetuseSmartTiming;
  end;
  TInvideoPromotionClass = Class of TInvideoPromotion;
  
  { --------------------------------------------------------------------
    TInvideoTiming
    --------------------------------------------------------------------}
  
  TInvideoTiming = Class(TGoogleBaseObject)
  Private
    FdurationMs : String;
    FoffsetMs : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdurationMs(AIndex : Integer; AValue : String); virtual;
    Procedure SetoffsetMs(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property durationMs : String Index 0 Read FdurationMs Write SetdurationMs;
    Property offsetMs : String Index 8 Read FoffsetMs Write SetoffsetMs;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TInvideoTimingClass = Class of TInvideoTiming;
  
  { --------------------------------------------------------------------
    TLanguageTag
    --------------------------------------------------------------------}
  
  TLanguageTag = Class(TGoogleBaseObject)
  Private
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property value : String Index 0 Read Fvalue Write Setvalue;
  end;
  TLanguageTagClass = Class of TLanguageTag;
  
  { --------------------------------------------------------------------
    TLiveBroadcast
    --------------------------------------------------------------------}
  
  TLiveBroadcast = Class(TGoogleBaseObject)
  Private
    FcontentDetails : TLiveBroadcastContentDetails;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TLiveBroadcastSnippet;
    Fstatus : TLiveBroadcastStatus;
  Protected
    //Property setters
    Procedure SetcontentDetails(AIndex : Integer; AValue : TLiveBroadcastContentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TLiveBroadcastSnippet); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TLiveBroadcastStatus); virtual;
  Public
  Published
    Property contentDetails : TLiveBroadcastContentDetails Index 0 Read FcontentDetails Write SetcontentDetails;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property snippet : TLiveBroadcastSnippet Index 32 Read Fsnippet Write Setsnippet;
    Property status : TLiveBroadcastStatus Index 40 Read Fstatus Write Setstatus;
  end;
  TLiveBroadcastClass = Class of TLiveBroadcast;
  
  { --------------------------------------------------------------------
    TLiveBroadcastContentDetails
    --------------------------------------------------------------------}
  
  TLiveBroadcastContentDetails = Class(TGoogleBaseObject)
  Private
    FboundStreamId : String;
    FenableClosedCaptions : boolean;
    FenableContentEncryption : boolean;
    FenableDvr : boolean;
    FenableEmbed : boolean;
    FmonitorStream : TMonitorStreamInfo;
    FrecordFromStart : boolean;
    FstartWithSlate : boolean;
  Protected
    //Property setters
    Procedure SetboundStreamId(AIndex : Integer; AValue : String); virtual;
    Procedure SetenableClosedCaptions(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetenableContentEncryption(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetenableDvr(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetenableEmbed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetmonitorStream(AIndex : Integer; AValue : TMonitorStreamInfo); virtual;
    Procedure SetrecordFromStart(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstartWithSlate(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property boundStreamId : String Index 0 Read FboundStreamId Write SetboundStreamId;
    Property enableClosedCaptions : boolean Index 8 Read FenableClosedCaptions Write SetenableClosedCaptions;
    Property enableContentEncryption : boolean Index 16 Read FenableContentEncryption Write SetenableContentEncryption;
    Property enableDvr : boolean Index 24 Read FenableDvr Write SetenableDvr;
    Property enableEmbed : boolean Index 32 Read FenableEmbed Write SetenableEmbed;
    Property monitorStream : TMonitorStreamInfo Index 40 Read FmonitorStream Write SetmonitorStream;
    Property recordFromStart : boolean Index 48 Read FrecordFromStart Write SetrecordFromStart;
    Property startWithSlate : boolean Index 56 Read FstartWithSlate Write SetstartWithSlate;
  end;
  TLiveBroadcastContentDetailsClass = Class of TLiveBroadcastContentDetails;
  
  { --------------------------------------------------------------------
    TLiveBroadcastListResponse
    --------------------------------------------------------------------}
  
  TLiveBroadcastListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TLiveBroadcastListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TLiveBroadcastListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TLiveBroadcastListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TLiveBroadcastListResponseClass = Class of TLiveBroadcastListResponse;
  
  { --------------------------------------------------------------------
    TLiveBroadcastSnippet
    --------------------------------------------------------------------}
  
  TLiveBroadcastSnippet = Class(TGoogleBaseObject)
  Private
    FactualEndTime : TDatetime;
    FactualStartTime : TDatetime;
    FchannelId : String;
    Fdescription : String;
    FpublishedAt : TDatetime;
    FscheduledEndTime : TDatetime;
    FscheduledStartTime : TDatetime;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetactualEndTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetactualStartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetscheduledEndTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetscheduledStartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property actualEndTime : TDatetime Index 0 Read FactualEndTime Write SetactualEndTime;
    Property actualStartTime : TDatetime Index 8 Read FactualStartTime Write SetactualStartTime;
    Property channelId : String Index 16 Read FchannelId Write SetchannelId;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property publishedAt : TDatetime Index 32 Read FpublishedAt Write SetpublishedAt;
    Property scheduledEndTime : TDatetime Index 40 Read FscheduledEndTime Write SetscheduledEndTime;
    Property scheduledStartTime : TDatetime Index 48 Read FscheduledStartTime Write SetscheduledStartTime;
    Property thumbnails : TThumbnailDetails Index 56 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 64 Read Ftitle Write Settitle;
  end;
  TLiveBroadcastSnippetClass = Class of TLiveBroadcastSnippet;
  
  { --------------------------------------------------------------------
    TLiveBroadcastStatus
    --------------------------------------------------------------------}
  
  TLiveBroadcastStatus = Class(TGoogleBaseObject)
  Private
    FisDefaultBroadcast : boolean;
    FlifeCycleStatus : String;
    FliveBroadcastPriority : String;
    FprivacyStatus : String;
    FrecordingStatus : String;
  Protected
    //Property setters
    Procedure SetisDefaultBroadcast(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlifeCycleStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetliveBroadcastPriority(AIndex : Integer; AValue : String); virtual;
    Procedure SetprivacyStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetrecordingStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property isDefaultBroadcast : boolean Index 0 Read FisDefaultBroadcast Write SetisDefaultBroadcast;
    Property lifeCycleStatus : String Index 8 Read FlifeCycleStatus Write SetlifeCycleStatus;
    Property liveBroadcastPriority : String Index 16 Read FliveBroadcastPriority Write SetliveBroadcastPriority;
    Property privacyStatus : String Index 24 Read FprivacyStatus Write SetprivacyStatus;
    Property recordingStatus : String Index 32 Read FrecordingStatus Write SetrecordingStatus;
  end;
  TLiveBroadcastStatusClass = Class of TLiveBroadcastStatus;
  
  { --------------------------------------------------------------------
    TLiveStream
    --------------------------------------------------------------------}
  
  TLiveStream = Class(TGoogleBaseObject)
  Private
    Fcdn : TCdnSettings;
    FcontentDetails : TLiveStreamContentDetails;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TLiveStreamSnippet;
    Fstatus : TLiveStreamStatus;
  Protected
    //Property setters
    Procedure Setcdn(AIndex : Integer; AValue : TCdnSettings); virtual;
    Procedure SetcontentDetails(AIndex : Integer; AValue : TLiveStreamContentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TLiveStreamSnippet); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TLiveStreamStatus); virtual;
  Public
  Published
    Property cdn : TCdnSettings Index 0 Read Fcdn Write Setcdn;
    Property contentDetails : TLiveStreamContentDetails Index 8 Read FcontentDetails Write SetcontentDetails;
    Property etag : String Index 16 Read Fetag Write Setetag;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property snippet : TLiveStreamSnippet Index 40 Read Fsnippet Write Setsnippet;
    Property status : TLiveStreamStatus Index 48 Read Fstatus Write Setstatus;
  end;
  TLiveStreamClass = Class of TLiveStream;
  
  { --------------------------------------------------------------------
    TLiveStreamContentDetails
    --------------------------------------------------------------------}
  
  TLiveStreamContentDetails = Class(TGoogleBaseObject)
  Private
    FclosedCaptionsIngestionUrl : String;
    FisReusable : boolean;
  Protected
    //Property setters
    Procedure SetclosedCaptionsIngestionUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetisReusable(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property closedCaptionsIngestionUrl : String Index 0 Read FclosedCaptionsIngestionUrl Write SetclosedCaptionsIngestionUrl;
    Property isReusable : boolean Index 8 Read FisReusable Write SetisReusable;
  end;
  TLiveStreamContentDetailsClass = Class of TLiveStreamContentDetails;
  
  { --------------------------------------------------------------------
    TLiveStreamListResponse
    --------------------------------------------------------------------}
  
  TLiveStreamListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TLiveStreamListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TLiveStreamListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TLiveStreamListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TLiveStreamListResponseClass = Class of TLiveStreamListResponse;
  
  { --------------------------------------------------------------------
    TLiveStreamSnippet
    --------------------------------------------------------------------}
  
  TLiveStreamSnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    Fdescription : String;
    FpublishedAt : TDatetime;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property publishedAt : TDatetime Index 16 Read FpublishedAt Write SetpublishedAt;
    Property title : String Index 24 Read Ftitle Write Settitle;
  end;
  TLiveStreamSnippetClass = Class of TLiveStreamSnippet;
  
  { --------------------------------------------------------------------
    TLiveStreamStatus
    --------------------------------------------------------------------}
  
  TLiveStreamStatus = Class(TGoogleBaseObject)
  Private
    FisDefaultStream : boolean;
    FstreamStatus : String;
  Protected
    //Property setters
    Procedure SetisDefaultStream(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstreamStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property isDefaultStream : boolean Index 0 Read FisDefaultStream Write SetisDefaultStream;
    Property streamStatus : String Index 8 Read FstreamStatus Write SetstreamStatus;
  end;
  TLiveStreamStatusClass = Class of TLiveStreamStatus;
  
  { --------------------------------------------------------------------
    TLocalizedProperty
    --------------------------------------------------------------------}
  
  TLocalizedProperty = Class(TGoogleBaseObject)
  Private
    Fdefault : String;
    FdefaultLanguage : TLanguageTag;
    Flocalized : TLocalizedPropertyTypelocalizedArray;
  Protected
    //Property setters
    Procedure Setdefault(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : TLanguageTag); virtual;
    Procedure Setlocalized(AIndex : Integer; AValue : TLocalizedPropertyTypelocalizedArray); virtual;
  Public
  Published
    Property default : String Index 0 Read Fdefault Write Setdefault;
    Property defaultLanguage : TLanguageTag Index 8 Read FdefaultLanguage Write SetdefaultLanguage;
    Property localized : TLocalizedPropertyTypelocalizedArray Index 16 Read Flocalized Write Setlocalized;
  end;
  TLocalizedPropertyClass = Class of TLocalizedProperty;
  
  { --------------------------------------------------------------------
    TLocalizedString
    --------------------------------------------------------------------}
  
  TLocalizedString = Class(TGoogleBaseObject)
  Private
    Flanguage : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property language : String Index 0 Read Flanguage Write Setlanguage;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TLocalizedStringClass = Class of TLocalizedString;
  
  { --------------------------------------------------------------------
    TMonitorStreamInfo
    --------------------------------------------------------------------}
  
  TMonitorStreamInfo = Class(TGoogleBaseObject)
  Private
    FbroadcastStreamDelayMs : integer;
    FembedHtml : String;
    FenableMonitorStream : boolean;
  Protected
    //Property setters
    Procedure SetbroadcastStreamDelayMs(AIndex : Integer; AValue : integer); virtual;
    Procedure SetembedHtml(AIndex : Integer; AValue : String); virtual;
    Procedure SetenableMonitorStream(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property broadcastStreamDelayMs : integer Index 0 Read FbroadcastStreamDelayMs Write SetbroadcastStreamDelayMs;
    Property embedHtml : String Index 8 Read FembedHtml Write SetembedHtml;
    Property enableMonitorStream : boolean Index 16 Read FenableMonitorStream Write SetenableMonitorStream;
  end;
  TMonitorStreamInfoClass = Class of TMonitorStreamInfo;
  
  { --------------------------------------------------------------------
    TPageInfo
    --------------------------------------------------------------------}
  
  TPageInfo = Class(TGoogleBaseObject)
  Private
    FresultsPerPage : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure SetresultsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property resultsPerPage : integer Index 0 Read FresultsPerPage Write SetresultsPerPage;
    Property totalResults : integer Index 8 Read FtotalResults Write SettotalResults;
  end;
  TPageInfoClass = Class of TPageInfo;
  
  { --------------------------------------------------------------------
    TPlaylistTypelocalizations
    --------------------------------------------------------------------}
  
  TPlaylistTypelocalizations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPlaylistTypelocalizationsClass = Class of TPlaylistTypelocalizations;
  
  { --------------------------------------------------------------------
    TPlaylist
    --------------------------------------------------------------------}
  
  TPlaylist = Class(TGoogleBaseObject)
  Private
    FcontentDetails : TPlaylistContentDetails;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Flocalizations : TPlaylistTypelocalizations;
    Fplayer : TPlaylistPlayer;
    Fsnippet : TPlaylistSnippet;
    Fstatus : TPlaylistStatus;
  Protected
    //Property setters
    Procedure SetcontentDetails(AIndex : Integer; AValue : TPlaylistContentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocalizations(AIndex : Integer; AValue : TPlaylistTypelocalizations); virtual;
    Procedure Setplayer(AIndex : Integer; AValue : TPlaylistPlayer); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TPlaylistSnippet); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TPlaylistStatus); virtual;
  Public
  Published
    Property contentDetails : TPlaylistContentDetails Index 0 Read FcontentDetails Write SetcontentDetails;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property localizations : TPlaylistTypelocalizations Index 32 Read Flocalizations Write Setlocalizations;
    Property player : TPlaylistPlayer Index 40 Read Fplayer Write Setplayer;
    Property snippet : TPlaylistSnippet Index 48 Read Fsnippet Write Setsnippet;
    Property status : TPlaylistStatus Index 56 Read Fstatus Write Setstatus;
  end;
  TPlaylistClass = Class of TPlaylist;
  
  { --------------------------------------------------------------------
    TPlaylistContentDetails
    --------------------------------------------------------------------}
  
  TPlaylistContentDetails = Class(TGoogleBaseObject)
  Private
    FitemCount : integer;
  Protected
    //Property setters
    Procedure SetitemCount(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property itemCount : integer Index 0 Read FitemCount Write SetitemCount;
  end;
  TPlaylistContentDetailsClass = Class of TPlaylistContentDetails;
  
  { --------------------------------------------------------------------
    TPlaylistItem
    --------------------------------------------------------------------}
  
  TPlaylistItem = Class(TGoogleBaseObject)
  Private
    FcontentDetails : TPlaylistItemContentDetails;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TPlaylistItemSnippet;
    Fstatus : TPlaylistItemStatus;
  Protected
    //Property setters
    Procedure SetcontentDetails(AIndex : Integer; AValue : TPlaylistItemContentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TPlaylistItemSnippet); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TPlaylistItemStatus); virtual;
  Public
  Published
    Property contentDetails : TPlaylistItemContentDetails Index 0 Read FcontentDetails Write SetcontentDetails;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property snippet : TPlaylistItemSnippet Index 32 Read Fsnippet Write Setsnippet;
    Property status : TPlaylistItemStatus Index 40 Read Fstatus Write Setstatus;
  end;
  TPlaylistItemClass = Class of TPlaylistItem;
  
  { --------------------------------------------------------------------
    TPlaylistItemContentDetails
    --------------------------------------------------------------------}
  
  TPlaylistItemContentDetails = Class(TGoogleBaseObject)
  Private
    FendAt : String;
    Fnote : String;
    FstartAt : String;
    FvideoId : String;
  Protected
    //Property setters
    Procedure SetendAt(AIndex : Integer; AValue : String); virtual;
    Procedure Setnote(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartAt(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property endAt : String Index 0 Read FendAt Write SetendAt;
    Property note : String Index 8 Read Fnote Write Setnote;
    Property startAt : String Index 16 Read FstartAt Write SetstartAt;
    Property videoId : String Index 24 Read FvideoId Write SetvideoId;
  end;
  TPlaylistItemContentDetailsClass = Class of TPlaylistItemContentDetails;
  
  { --------------------------------------------------------------------
    TPlaylistItemListResponse
    --------------------------------------------------------------------}
  
  TPlaylistItemListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TPlaylistItemListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPlaylistItemListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TPlaylistItemListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TPlaylistItemListResponseClass = Class of TPlaylistItemListResponse;
  
  { --------------------------------------------------------------------
    TPlaylistItemSnippet
    --------------------------------------------------------------------}
  
  TPlaylistItemSnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    FchannelTitle : String;
    Fdescription : String;
    FplaylistId : String;
    Fposition : integer;
    FpublishedAt : TDatetime;
    FresourceId : TResourceId;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchannelTitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetplaylistId(AIndex : Integer; AValue : String); virtual;
    Procedure Setposition(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property channelTitle : String Index 8 Read FchannelTitle Write SetchannelTitle;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property playlistId : String Index 24 Read FplaylistId Write SetplaylistId;
    Property position : integer Index 32 Read Fposition Write Setposition;
    Property publishedAt : TDatetime Index 40 Read FpublishedAt Write SetpublishedAt;
    Property resourceId : TResourceId Index 48 Read FresourceId Write SetresourceId;
    Property thumbnails : TThumbnailDetails Index 56 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 64 Read Ftitle Write Settitle;
  end;
  TPlaylistItemSnippetClass = Class of TPlaylistItemSnippet;
  
  { --------------------------------------------------------------------
    TPlaylistItemStatus
    --------------------------------------------------------------------}
  
  TPlaylistItemStatus = Class(TGoogleBaseObject)
  Private
    FprivacyStatus : String;
  Protected
    //Property setters
    Procedure SetprivacyStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property privacyStatus : String Index 0 Read FprivacyStatus Write SetprivacyStatus;
  end;
  TPlaylistItemStatusClass = Class of TPlaylistItemStatus;
  
  { --------------------------------------------------------------------
    TPlaylistListResponse
    --------------------------------------------------------------------}
  
  TPlaylistListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TPlaylistListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPlaylistListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TPlaylistListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TPlaylistListResponseClass = Class of TPlaylistListResponse;
  
  { --------------------------------------------------------------------
    TPlaylistLocalization
    --------------------------------------------------------------------}
  
  TPlaylistLocalization = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property title : String Index 8 Read Ftitle Write Settitle;
  end;
  TPlaylistLocalizationClass = Class of TPlaylistLocalization;
  
  { --------------------------------------------------------------------
    TPlaylistPlayer
    --------------------------------------------------------------------}
  
  TPlaylistPlayer = Class(TGoogleBaseObject)
  Private
    FembedHtml : String;
  Protected
    //Property setters
    Procedure SetembedHtml(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property embedHtml : String Index 0 Read FembedHtml Write SetembedHtml;
  end;
  TPlaylistPlayerClass = Class of TPlaylistPlayer;
  
  { --------------------------------------------------------------------
    TPlaylistSnippet
    --------------------------------------------------------------------}
  
  TPlaylistSnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    FchannelTitle : String;
    FdefaultLanguage : String;
    Fdescription : String;
    Flocalized : TPlaylistLocalization;
    FpublishedAt : TDatetime;
    Ftags : TStringArray;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchannelTitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocalized(AIndex : Integer; AValue : TPlaylistLocalization); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settags(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property channelTitle : String Index 8 Read FchannelTitle Write SetchannelTitle;
    Property defaultLanguage : String Index 16 Read FdefaultLanguage Write SetdefaultLanguage;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property localized : TPlaylistLocalization Index 32 Read Flocalized Write Setlocalized;
    Property publishedAt : TDatetime Index 40 Read FpublishedAt Write SetpublishedAt;
    Property tags : TStringArray Index 48 Read Ftags Write Settags;
    Property thumbnails : TThumbnailDetails Index 56 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 64 Read Ftitle Write Settitle;
  end;
  TPlaylistSnippetClass = Class of TPlaylistSnippet;
  
  { --------------------------------------------------------------------
    TPlaylistStatus
    --------------------------------------------------------------------}
  
  TPlaylistStatus = Class(TGoogleBaseObject)
  Private
    FprivacyStatus : String;
  Protected
    //Property setters
    Procedure SetprivacyStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property privacyStatus : String Index 0 Read FprivacyStatus Write SetprivacyStatus;
  end;
  TPlaylistStatusClass = Class of TPlaylistStatus;
  
  { --------------------------------------------------------------------
    TPromotedItem
    --------------------------------------------------------------------}
  
  TPromotedItem = Class(TGoogleBaseObject)
  Private
    FcustomMessage : String;
    Fid : TPromotedItemId;
    FpromotedByContentOwner : boolean;
    Ftiming : TInvideoTiming;
  Protected
    //Property setters
    Procedure SetcustomMessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : TPromotedItemId); virtual;
    Procedure SetpromotedByContentOwner(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settiming(AIndex : Integer; AValue : TInvideoTiming); virtual;
  Public
  Published
    Property customMessage : String Index 0 Read FcustomMessage Write SetcustomMessage;
    Property id : TPromotedItemId Index 8 Read Fid Write Setid;
    Property promotedByContentOwner : boolean Index 16 Read FpromotedByContentOwner Write SetpromotedByContentOwner;
    Property timing : TInvideoTiming Index 24 Read Ftiming Write Settiming;
  end;
  TPromotedItemClass = Class of TPromotedItem;
  
  { --------------------------------------------------------------------
    TPromotedItemId
    --------------------------------------------------------------------}
  
  TPromotedItemId = Class(TGoogleBaseObject)
  Private
    FrecentlyUploadedBy : String;
    F_type : String;
    FvideoId : String;
    FwebsiteUrl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetrecentlyUploadedBy(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property recentlyUploadedBy : String Index 0 Read FrecentlyUploadedBy Write SetrecentlyUploadedBy;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property videoId : String Index 16 Read FvideoId Write SetvideoId;
    Property websiteUrl : String Index 24 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TPromotedItemIdClass = Class of TPromotedItemId;
  
  { --------------------------------------------------------------------
    TPropertyValue
    --------------------------------------------------------------------}
  
  TPropertyValue = Class(TGoogleBaseObject)
  Private
    F_property : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_property(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _property : String Index 0 Read F_property Write Set_property;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TPropertyValueClass = Class of TPropertyValue;
  
  { --------------------------------------------------------------------
    TResourceId
    --------------------------------------------------------------------}
  
  TResourceId = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    Fkind : String;
    FplaylistId : String;
    FvideoId : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetplaylistId(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property playlistId : String Index 16 Read FplaylistId Write SetplaylistId;
    Property videoId : String Index 24 Read FvideoId Write SetvideoId;
  end;
  TResourceIdClass = Class of TResourceId;
  
  { --------------------------------------------------------------------
    TSearchListResponse
    --------------------------------------------------------------------}
  
  TSearchListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TSearchListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSearchListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TSearchListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TSearchListResponseClass = Class of TSearchListResponse;
  
  { --------------------------------------------------------------------
    TSearchResult
    --------------------------------------------------------------------}
  
  TSearchResult = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : TResourceId;
    Fkind : String;
    Fsnippet : TSearchResultSnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : TResourceId); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TSearchResultSnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : TResourceId Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property snippet : TSearchResultSnippet Index 24 Read Fsnippet Write Setsnippet;
  end;
  TSearchResultClass = Class of TSearchResult;
  
  { --------------------------------------------------------------------
    TSearchResultSnippet
    --------------------------------------------------------------------}
  
  TSearchResultSnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    FchannelTitle : String;
    Fdescription : String;
    FliveBroadcastContent : String;
    FpublishedAt : TDatetime;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchannelTitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetliveBroadcastContent(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property channelTitle : String Index 8 Read FchannelTitle Write SetchannelTitle;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property liveBroadcastContent : String Index 24 Read FliveBroadcastContent Write SetliveBroadcastContent;
    Property publishedAt : TDatetime Index 32 Read FpublishedAt Write SetpublishedAt;
    Property thumbnails : TThumbnailDetails Index 40 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 48 Read Ftitle Write Settitle;
  end;
  TSearchResultSnippetClass = Class of TSearchResultSnippet;
  
  { --------------------------------------------------------------------
    TSubscription
    --------------------------------------------------------------------}
  
  TSubscription = Class(TGoogleBaseObject)
  Private
    FcontentDetails : TSubscriptionContentDetails;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TSubscriptionSnippet;
    FsubscriberSnippet : TSubscriptionSubscriberSnippet;
  Protected
    //Property setters
    Procedure SetcontentDetails(AIndex : Integer; AValue : TSubscriptionContentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TSubscriptionSnippet); virtual;
    Procedure SetsubscriberSnippet(AIndex : Integer; AValue : TSubscriptionSubscriberSnippet); virtual;
  Public
  Published
    Property contentDetails : TSubscriptionContentDetails Index 0 Read FcontentDetails Write SetcontentDetails;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property snippet : TSubscriptionSnippet Index 32 Read Fsnippet Write Setsnippet;
    Property subscriberSnippet : TSubscriptionSubscriberSnippet Index 40 Read FsubscriberSnippet Write SetsubscriberSnippet;
  end;
  TSubscriptionClass = Class of TSubscription;
  
  { --------------------------------------------------------------------
    TSubscriptionContentDetails
    --------------------------------------------------------------------}
  
  TSubscriptionContentDetails = Class(TGoogleBaseObject)
  Private
    FactivityType : String;
    FnewItemCount : integer;
    FtotalItemCount : integer;
  Protected
    //Property setters
    Procedure SetactivityType(AIndex : Integer; AValue : String); virtual;
    Procedure SetnewItemCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalItemCount(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property activityType : String Index 0 Read FactivityType Write SetactivityType;
    Property newItemCount : integer Index 8 Read FnewItemCount Write SetnewItemCount;
    Property totalItemCount : integer Index 16 Read FtotalItemCount Write SettotalItemCount;
  end;
  TSubscriptionContentDetailsClass = Class of TSubscriptionContentDetails;
  
  { --------------------------------------------------------------------
    TSubscriptionListResponse
    --------------------------------------------------------------------}
  
  TSubscriptionListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TSubscriptionListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSubscriptionListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TSubscriptionListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TSubscriptionListResponseClass = Class of TSubscriptionListResponse;
  
  { --------------------------------------------------------------------
    TSubscriptionSnippet
    --------------------------------------------------------------------}
  
  TSubscriptionSnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    FchannelTitle : String;
    Fdescription : String;
    FpublishedAt : TDatetime;
    FresourceId : TResourceId;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchannelTitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : TResourceId); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property channelTitle : String Index 8 Read FchannelTitle Write SetchannelTitle;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property publishedAt : TDatetime Index 24 Read FpublishedAt Write SetpublishedAt;
    Property resourceId : TResourceId Index 32 Read FresourceId Write SetresourceId;
    Property thumbnails : TThumbnailDetails Index 40 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 48 Read Ftitle Write Settitle;
  end;
  TSubscriptionSnippetClass = Class of TSubscriptionSnippet;
  
  { --------------------------------------------------------------------
    TSubscriptionSubscriberSnippet
    --------------------------------------------------------------------}
  
  TSubscriptionSubscriberSnippet = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    Fdescription : String;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property thumbnails : TThumbnailDetails Index 16 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 24 Read Ftitle Write Settitle;
  end;
  TSubscriptionSubscriberSnippetClass = Class of TSubscriptionSubscriberSnippet;
  
  { --------------------------------------------------------------------
    TThumbnail
    --------------------------------------------------------------------}
  
  TThumbnail = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Furl : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property url : String Index 8 Read Furl Write Seturl;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TThumbnailClass = Class of TThumbnail;
  
  { --------------------------------------------------------------------
    TThumbnailDetails
    --------------------------------------------------------------------}
  
  TThumbnailDetails = Class(TGoogleBaseObject)
  Private
    Fdefault : TThumbnail;
    Fhigh : TThumbnail;
    Fmaxres : TThumbnail;
    Fmedium : TThumbnail;
    Fstandard : TThumbnail;
  Protected
    //Property setters
    Procedure Setdefault(AIndex : Integer; AValue : TThumbnail); virtual;
    Procedure Sethigh(AIndex : Integer; AValue : TThumbnail); virtual;
    Procedure Setmaxres(AIndex : Integer; AValue : TThumbnail); virtual;
    Procedure Setmedium(AIndex : Integer; AValue : TThumbnail); virtual;
    Procedure Setstandard(AIndex : Integer; AValue : TThumbnail); virtual;
  Public
  Published
    Property default : TThumbnail Index 0 Read Fdefault Write Setdefault;
    Property high : TThumbnail Index 8 Read Fhigh Write Sethigh;
    Property maxres : TThumbnail Index 16 Read Fmaxres Write Setmaxres;
    Property medium : TThumbnail Index 24 Read Fmedium Write Setmedium;
    Property standard : TThumbnail Index 32 Read Fstandard Write Setstandard;
  end;
  TThumbnailDetailsClass = Class of TThumbnailDetails;
  
  { --------------------------------------------------------------------
    TThumbnailSetResponse
    --------------------------------------------------------------------}
  
  TThumbnailSetResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TThumbnailSetResponseTypeitemsArray;
    Fkind : String;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TThumbnailSetResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TThumbnailSetResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property visitorId : String Index 32 Read FvisitorId Write SetvisitorId;
  end;
  TThumbnailSetResponseClass = Class of TThumbnailSetResponse;
  
  { --------------------------------------------------------------------
    TTokenPagination
    --------------------------------------------------------------------}
  
  TTokenPagination = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTokenPaginationClass = Class of TTokenPagination;
  
  { --------------------------------------------------------------------
    TVideoTypelocalizations
    --------------------------------------------------------------------}
  
  TVideoTypelocalizations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TVideoTypelocalizationsClass = Class of TVideoTypelocalizations;
  
  { --------------------------------------------------------------------
    TVideo
    --------------------------------------------------------------------}
  
  TVideo = Class(TGoogleBaseObject)
  Private
    FageGating : TVideoAgeGating;
    FcontentDetails : TVideoContentDetails;
    FconversionPings : TVideoConversionPings;
    Fetag : String;
    FfileDetails : TVideoFileDetails;
    Fid : String;
    Fkind : String;
    FliveStreamingDetails : TVideoLiveStreamingDetails;
    Flocalizations : TVideoTypelocalizations;
    FmonetizationDetails : TVideoMonetizationDetails;
    Fplayer : TVideoPlayer;
    FprocessingDetails : TVideoProcessingDetails;
    FprojectDetails : TVideoProjectDetails;
    FrecordingDetails : TVideoRecordingDetails;
    Fsnippet : TVideoSnippet;
    Fstatistics : TVideoStatistics;
    Fstatus : TVideoStatus;
    Fsuggestions : TVideoSuggestions;
    FtopicDetails : TVideoTopicDetails;
  Protected
    //Property setters
    Procedure SetageGating(AIndex : Integer; AValue : TVideoAgeGating); virtual;
    Procedure SetcontentDetails(AIndex : Integer; AValue : TVideoContentDetails); virtual;
    Procedure SetconversionPings(AIndex : Integer; AValue : TVideoConversionPings); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileDetails(AIndex : Integer; AValue : TVideoFileDetails); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetliveStreamingDetails(AIndex : Integer; AValue : TVideoLiveStreamingDetails); virtual;
    Procedure Setlocalizations(AIndex : Integer; AValue : TVideoTypelocalizations); virtual;
    Procedure SetmonetizationDetails(AIndex : Integer; AValue : TVideoMonetizationDetails); virtual;
    Procedure Setplayer(AIndex : Integer; AValue : TVideoPlayer); virtual;
    Procedure SetprocessingDetails(AIndex : Integer; AValue : TVideoProcessingDetails); virtual;
    Procedure SetprojectDetails(AIndex : Integer; AValue : TVideoProjectDetails); virtual;
    Procedure SetrecordingDetails(AIndex : Integer; AValue : TVideoRecordingDetails); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TVideoSnippet); virtual;
    Procedure Setstatistics(AIndex : Integer; AValue : TVideoStatistics); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TVideoStatus); virtual;
    Procedure Setsuggestions(AIndex : Integer; AValue : TVideoSuggestions); virtual;
    Procedure SettopicDetails(AIndex : Integer; AValue : TVideoTopicDetails); virtual;
  Public
  Published
    Property ageGating : TVideoAgeGating Index 0 Read FageGating Write SetageGating;
    Property contentDetails : TVideoContentDetails Index 8 Read FcontentDetails Write SetcontentDetails;
    Property conversionPings : TVideoConversionPings Index 16 Read FconversionPings Write SetconversionPings;
    Property etag : String Index 24 Read Fetag Write Setetag;
    Property fileDetails : TVideoFileDetails Index 32 Read FfileDetails Write SetfileDetails;
    Property id : String Index 40 Read Fid Write Setid;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property liveStreamingDetails : TVideoLiveStreamingDetails Index 56 Read FliveStreamingDetails Write SetliveStreamingDetails;
    Property localizations : TVideoTypelocalizations Index 64 Read Flocalizations Write Setlocalizations;
    Property monetizationDetails : TVideoMonetizationDetails Index 72 Read FmonetizationDetails Write SetmonetizationDetails;
    Property player : TVideoPlayer Index 80 Read Fplayer Write Setplayer;
    Property processingDetails : TVideoProcessingDetails Index 88 Read FprocessingDetails Write SetprocessingDetails;
    Property projectDetails : TVideoProjectDetails Index 96 Read FprojectDetails Write SetprojectDetails;
    Property recordingDetails : TVideoRecordingDetails Index 104 Read FrecordingDetails Write SetrecordingDetails;
    Property snippet : TVideoSnippet Index 112 Read Fsnippet Write Setsnippet;
    Property statistics : TVideoStatistics Index 120 Read Fstatistics Write Setstatistics;
    Property status : TVideoStatus Index 128 Read Fstatus Write Setstatus;
    Property suggestions : TVideoSuggestions Index 136 Read Fsuggestions Write Setsuggestions;
    Property topicDetails : TVideoTopicDetails Index 144 Read FtopicDetails Write SettopicDetails;
  end;
  TVideoClass = Class of TVideo;
  
  { --------------------------------------------------------------------
    TVideoAbuseReport
    --------------------------------------------------------------------}
  
  TVideoAbuseReport = Class(TGoogleBaseObject)
  Private
    Fcomments : String;
    Flanguage : String;
    FreasonId : String;
    FsecondaryReasonId : String;
    FvideoId : String;
  Protected
    //Property setters
    Procedure Setcomments(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetreasonId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsecondaryReasonId(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property comments : String Index 0 Read Fcomments Write Setcomments;
    Property language : String Index 8 Read Flanguage Write Setlanguage;
    Property reasonId : String Index 16 Read FreasonId Write SetreasonId;
    Property secondaryReasonId : String Index 24 Read FsecondaryReasonId Write SetsecondaryReasonId;
    Property videoId : String Index 32 Read FvideoId Write SetvideoId;
  end;
  TVideoAbuseReportClass = Class of TVideoAbuseReport;
  
  { --------------------------------------------------------------------
    TVideoAbuseReportReason
    --------------------------------------------------------------------}
  
  TVideoAbuseReportReason = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TVideoAbuseReportReasonSnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TVideoAbuseReportReasonSnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property snippet : TVideoAbuseReportReasonSnippet Index 24 Read Fsnippet Write Setsnippet;
  end;
  TVideoAbuseReportReasonClass = Class of TVideoAbuseReportReason;
  
  { --------------------------------------------------------------------
    TVideoAbuseReportReasonListResponse
    --------------------------------------------------------------------}
  
  TVideoAbuseReportReasonListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TVideoAbuseReportReasonListResponseTypeitemsArray;
    Fkind : String;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TVideoAbuseReportReasonListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TVideoAbuseReportReasonListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property visitorId : String Index 32 Read FvisitorId Write SetvisitorId;
  end;
  TVideoAbuseReportReasonListResponseClass = Class of TVideoAbuseReportReasonListResponse;
  
  { --------------------------------------------------------------------
    TVideoAbuseReportReasonSnippet
    --------------------------------------------------------------------}
  
  TVideoAbuseReportReasonSnippet = Class(TGoogleBaseObject)
  Private
    F_label : String;
    FsecondaryReasons : TVideoAbuseReportReasonSnippetTypesecondaryReasonsArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
    Procedure SetsecondaryReasons(AIndex : Integer; AValue : TVideoAbuseReportReasonSnippetTypesecondaryReasonsArray); virtual;
  Public
  Published
    Property _label : String Index 0 Read F_label Write Set_label;
    Property secondaryReasons : TVideoAbuseReportReasonSnippetTypesecondaryReasonsArray Index 8 Read FsecondaryReasons Write SetsecondaryReasons;
  end;
  TVideoAbuseReportReasonSnippetClass = Class of TVideoAbuseReportReasonSnippet;
  
  { --------------------------------------------------------------------
    TVideoAbuseReportSecondaryReason
    --------------------------------------------------------------------}
  
  TVideoAbuseReportSecondaryReason = Class(TGoogleBaseObject)
  Private
    Fid : String;
    F_label : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property _label : String Index 8 Read F_label Write Set_label;
  end;
  TVideoAbuseReportSecondaryReasonClass = Class of TVideoAbuseReportSecondaryReason;
  
  { --------------------------------------------------------------------
    TVideoAgeGating
    --------------------------------------------------------------------}
  
  TVideoAgeGating = Class(TGoogleBaseObject)
  Private
    FalcoholContent : boolean;
    Frestricted : boolean;
    FvideoGameRating : String;
  Protected
    //Property setters
    Procedure SetalcoholContent(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setrestricted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetvideoGameRating(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property alcoholContent : boolean Index 0 Read FalcoholContent Write SetalcoholContent;
    Property restricted : boolean Index 8 Read Frestricted Write Setrestricted;
    Property videoGameRating : String Index 16 Read FvideoGameRating Write SetvideoGameRating;
  end;
  TVideoAgeGatingClass = Class of TVideoAgeGating;
  
  { --------------------------------------------------------------------
    TVideoCategory
    --------------------------------------------------------------------}
  
  TVideoCategory = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TVideoCategorySnippet;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TVideoCategorySnippet); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property snippet : TVideoCategorySnippet Index 24 Read Fsnippet Write Setsnippet;
  end;
  TVideoCategoryClass = Class of TVideoCategory;
  
  { --------------------------------------------------------------------
    TVideoCategoryListResponse
    --------------------------------------------------------------------}
  
  TVideoCategoryListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TVideoCategoryListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TVideoCategoryListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TVideoCategoryListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TVideoCategoryListResponseClass = Class of TVideoCategoryListResponse;
  
  { --------------------------------------------------------------------
    TVideoCategorySnippet
    --------------------------------------------------------------------}
  
  TVideoCategorySnippet = Class(TGoogleBaseObject)
  Private
    Fassignable : boolean;
    FchannelId : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setassignable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property assignable : boolean Index 0 Read Fassignable Write Setassignable;
    Property channelId : String Index 8 Read FchannelId Write SetchannelId;
    Property title : String Index 16 Read Ftitle Write Settitle;
  end;
  TVideoCategorySnippetClass = Class of TVideoCategorySnippet;
  
  { --------------------------------------------------------------------
    TVideoContentDetails
    --------------------------------------------------------------------}
  
  TVideoContentDetails = Class(TGoogleBaseObject)
  Private
    Fcaption : String;
    FcontentRating : TContentRating;
    FcountryRestriction : TAccessPolicy;
    Fdefinition : String;
    Fdimension : String;
    Fduration : String;
    FlicensedContent : boolean;
    FregionRestriction : TVideoContentDetailsRegionRestriction;
  Protected
    //Property setters
    Procedure Setcaption(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentRating(AIndex : Integer; AValue : TContentRating); virtual;
    Procedure SetcountryRestriction(AIndex : Integer; AValue : TAccessPolicy); virtual;
    Procedure Setdefinition(AIndex : Integer; AValue : String); virtual;
    Procedure Setdimension(AIndex : Integer; AValue : String); virtual;
    Procedure Setduration(AIndex : Integer; AValue : String); virtual;
    Procedure SetlicensedContent(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetregionRestriction(AIndex : Integer; AValue : TVideoContentDetailsRegionRestriction); virtual;
  Public
  Published
    Property caption : String Index 0 Read Fcaption Write Setcaption;
    Property contentRating : TContentRating Index 8 Read FcontentRating Write SetcontentRating;
    Property countryRestriction : TAccessPolicy Index 16 Read FcountryRestriction Write SetcountryRestriction;
    Property definition : String Index 24 Read Fdefinition Write Setdefinition;
    Property dimension : String Index 32 Read Fdimension Write Setdimension;
    Property duration : String Index 40 Read Fduration Write Setduration;
    Property licensedContent : boolean Index 48 Read FlicensedContent Write SetlicensedContent;
    Property regionRestriction : TVideoContentDetailsRegionRestriction Index 56 Read FregionRestriction Write SetregionRestriction;
  end;
  TVideoContentDetailsClass = Class of TVideoContentDetails;
  
  { --------------------------------------------------------------------
    TVideoContentDetailsRegionRestriction
    --------------------------------------------------------------------}
  
  TVideoContentDetailsRegionRestriction = Class(TGoogleBaseObject)
  Private
    Fallowed : TStringArray;
    Fblocked : TStringArray;
  Protected
    //Property setters
    Procedure Setallowed(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setblocked(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property allowed : TStringArray Index 0 Read Fallowed Write Setallowed;
    Property blocked : TStringArray Index 8 Read Fblocked Write Setblocked;
  end;
  TVideoContentDetailsRegionRestrictionClass = Class of TVideoContentDetailsRegionRestriction;
  
  { --------------------------------------------------------------------
    TVideoConversionPing
    --------------------------------------------------------------------}
  
  TVideoConversionPing = Class(TGoogleBaseObject)
  Private
    Fcontext : String;
    FconversionUrl : String;
  Protected
    //Property setters
    Procedure Setcontext(AIndex : Integer; AValue : String); virtual;
    Procedure SetconversionUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property context : String Index 0 Read Fcontext Write Setcontext;
    Property conversionUrl : String Index 8 Read FconversionUrl Write SetconversionUrl;
  end;
  TVideoConversionPingClass = Class of TVideoConversionPing;
  
  { --------------------------------------------------------------------
    TVideoConversionPings
    --------------------------------------------------------------------}
  
  TVideoConversionPings = Class(TGoogleBaseObject)
  Private
    Fpings : TVideoConversionPingsTypepingsArray;
  Protected
    //Property setters
    Procedure Setpings(AIndex : Integer; AValue : TVideoConversionPingsTypepingsArray); virtual;
  Public
  Published
    Property pings : TVideoConversionPingsTypepingsArray Index 0 Read Fpings Write Setpings;
  end;
  TVideoConversionPingsClass = Class of TVideoConversionPings;
  
  { --------------------------------------------------------------------
    TVideoFileDetails
    --------------------------------------------------------------------}
  
  TVideoFileDetails = Class(TGoogleBaseObject)
  Private
    FaudioStreams : TVideoFileDetailsTypeaudioStreamsArray;
    FbitrateBps : String;
    Fcontainer : String;
    FcreationTime : String;
    FdurationMs : String;
    FfileName : String;
    FfileSize : String;
    FfileType : String;
    FrecordingLocation : TGeoPoint;
    FvideoStreams : TVideoFileDetailsTypevideoStreamsArray;
  Protected
    //Property setters
    Procedure SetaudioStreams(AIndex : Integer; AValue : TVideoFileDetailsTypeaudioStreamsArray); virtual;
    Procedure SetbitrateBps(AIndex : Integer; AValue : String); virtual;
    Procedure Setcontainer(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetdurationMs(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileName(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileType(AIndex : Integer; AValue : String); virtual;
    Procedure SetrecordingLocation(AIndex : Integer; AValue : TGeoPoint); virtual;
    Procedure SetvideoStreams(AIndex : Integer; AValue : TVideoFileDetailsTypevideoStreamsArray); virtual;
  Public
  Published
    Property audioStreams : TVideoFileDetailsTypeaudioStreamsArray Index 0 Read FaudioStreams Write SetaudioStreams;
    Property bitrateBps : String Index 8 Read FbitrateBps Write SetbitrateBps;
    Property container : String Index 16 Read Fcontainer Write Setcontainer;
    Property creationTime : String Index 24 Read FcreationTime Write SetcreationTime;
    Property durationMs : String Index 32 Read FdurationMs Write SetdurationMs;
    Property fileName : String Index 40 Read FfileName Write SetfileName;
    Property fileSize : String Index 48 Read FfileSize Write SetfileSize;
    Property fileType : String Index 56 Read FfileType Write SetfileType;
    Property recordingLocation : TGeoPoint Index 64 Read FrecordingLocation Write SetrecordingLocation;
    Property videoStreams : TVideoFileDetailsTypevideoStreamsArray Index 72 Read FvideoStreams Write SetvideoStreams;
  end;
  TVideoFileDetailsClass = Class of TVideoFileDetails;
  
  { --------------------------------------------------------------------
    TVideoFileDetailsAudioStream
    --------------------------------------------------------------------}
  
  TVideoFileDetailsAudioStream = Class(TGoogleBaseObject)
  Private
    FbitrateBps : String;
    FchannelCount : integer;
    Fcodec : String;
    Fvendor : String;
  Protected
    //Property setters
    Procedure SetbitrateBps(AIndex : Integer; AValue : String); virtual;
    Procedure SetchannelCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setcodec(AIndex : Integer; AValue : String); virtual;
    Procedure Setvendor(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property bitrateBps : String Index 0 Read FbitrateBps Write SetbitrateBps;
    Property channelCount : integer Index 8 Read FchannelCount Write SetchannelCount;
    Property codec : String Index 16 Read Fcodec Write Setcodec;
    Property vendor : String Index 24 Read Fvendor Write Setvendor;
  end;
  TVideoFileDetailsAudioStreamClass = Class of TVideoFileDetailsAudioStream;
  
  { --------------------------------------------------------------------
    TVideoFileDetailsVideoStream
    --------------------------------------------------------------------}
  
  TVideoFileDetailsVideoStream = Class(TGoogleBaseObject)
  Private
    FaspectRatio : double;
    FbitrateBps : String;
    Fcodec : String;
    FframeRateFps : double;
    FheightPixels : integer;
    Frotation : String;
    Fvendor : String;
    FwidthPixels : integer;
  Protected
    //Property setters
    Procedure SetaspectRatio(AIndex : Integer; AValue : double); virtual;
    Procedure SetbitrateBps(AIndex : Integer; AValue : String); virtual;
    Procedure Setcodec(AIndex : Integer; AValue : String); virtual;
    Procedure SetframeRateFps(AIndex : Integer; AValue : double); virtual;
    Procedure SetheightPixels(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrotation(AIndex : Integer; AValue : String); virtual;
    Procedure Setvendor(AIndex : Integer; AValue : String); virtual;
    Procedure SetwidthPixels(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property aspectRatio : double Index 0 Read FaspectRatio Write SetaspectRatio;
    Property bitrateBps : String Index 8 Read FbitrateBps Write SetbitrateBps;
    Property codec : String Index 16 Read Fcodec Write Setcodec;
    Property frameRateFps : double Index 24 Read FframeRateFps Write SetframeRateFps;
    Property heightPixels : integer Index 32 Read FheightPixels Write SetheightPixels;
    Property rotation : String Index 40 Read Frotation Write Setrotation;
    Property vendor : String Index 48 Read Fvendor Write Setvendor;
    Property widthPixels : integer Index 56 Read FwidthPixels Write SetwidthPixels;
  end;
  TVideoFileDetailsVideoStreamClass = Class of TVideoFileDetailsVideoStream;
  
  { --------------------------------------------------------------------
    TVideoGetRatingResponse
    --------------------------------------------------------------------}
  
  TVideoGetRatingResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TVideoGetRatingResponseTypeitemsArray;
    Fkind : String;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TVideoGetRatingResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TVideoGetRatingResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property visitorId : String Index 32 Read FvisitorId Write SetvisitorId;
  end;
  TVideoGetRatingResponseClass = Class of TVideoGetRatingResponse;
  
  { --------------------------------------------------------------------
    TVideoListResponse
    --------------------------------------------------------------------}
  
  TVideoListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FeventId : String;
    Fitems : TVideoListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FpageInfo : TPageInfo;
    FprevPageToken : String;
    FtokenPagination : TTokenPagination;
    FvisitorId : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TVideoListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    Procedure SetvisitorId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property eventId : String Index 8 Read FeventId Write SeteventId;
    Property items : TVideoListResponseTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property pageInfo : TPageInfo Index 40 Read FpageInfo Write SetpageInfo;
    Property prevPageToken : String Index 48 Read FprevPageToken Write SetprevPageToken;
    Property tokenPagination : TTokenPagination Index 56 Read FtokenPagination Write SettokenPagination;
    Property visitorId : String Index 64 Read FvisitorId Write SetvisitorId;
  end;
  TVideoListResponseClass = Class of TVideoListResponse;
  
  { --------------------------------------------------------------------
    TVideoLiveStreamingDetails
    --------------------------------------------------------------------}
  
  TVideoLiveStreamingDetails = Class(TGoogleBaseObject)
  Private
    FactualEndTime : TDatetime;
    FactualStartTime : TDatetime;
    FconcurrentViewers : String;
    FscheduledEndTime : TDatetime;
    FscheduledStartTime : TDatetime;
  Protected
    //Property setters
    Procedure SetactualEndTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetactualStartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetconcurrentViewers(AIndex : Integer; AValue : String); virtual;
    Procedure SetscheduledEndTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetscheduledStartTime(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property actualEndTime : TDatetime Index 0 Read FactualEndTime Write SetactualEndTime;
    Property actualStartTime : TDatetime Index 8 Read FactualStartTime Write SetactualStartTime;
    Property concurrentViewers : String Index 16 Read FconcurrentViewers Write SetconcurrentViewers;
    Property scheduledEndTime : TDatetime Index 24 Read FscheduledEndTime Write SetscheduledEndTime;
    Property scheduledStartTime : TDatetime Index 32 Read FscheduledStartTime Write SetscheduledStartTime;
  end;
  TVideoLiveStreamingDetailsClass = Class of TVideoLiveStreamingDetails;
  
  { --------------------------------------------------------------------
    TVideoLocalization
    --------------------------------------------------------------------}
  
  TVideoLocalization = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property title : String Index 8 Read Ftitle Write Settitle;
  end;
  TVideoLocalizationClass = Class of TVideoLocalization;
  
  { --------------------------------------------------------------------
    TVideoMonetizationDetails
    --------------------------------------------------------------------}
  
  TVideoMonetizationDetails = Class(TGoogleBaseObject)
  Private
    Faccess : TAccessPolicy;
  Protected
    //Property setters
    Procedure Setaccess(AIndex : Integer; AValue : TAccessPolicy); virtual;
  Public
  Published
    Property access : TAccessPolicy Index 0 Read Faccess Write Setaccess;
  end;
  TVideoMonetizationDetailsClass = Class of TVideoMonetizationDetails;
  
  { --------------------------------------------------------------------
    TVideoPlayer
    --------------------------------------------------------------------}
  
  TVideoPlayer = Class(TGoogleBaseObject)
  Private
    FembedHtml : String;
  Protected
    //Property setters
    Procedure SetembedHtml(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property embedHtml : String Index 0 Read FembedHtml Write SetembedHtml;
  end;
  TVideoPlayerClass = Class of TVideoPlayer;
  
  { --------------------------------------------------------------------
    TVideoProcessingDetails
    --------------------------------------------------------------------}
  
  TVideoProcessingDetails = Class(TGoogleBaseObject)
  Private
    FeditorSuggestionsAvailability : String;
    FfileDetailsAvailability : String;
    FprocessingFailureReason : String;
    FprocessingIssuesAvailability : String;
    FprocessingProgress : TVideoProcessingDetailsProcessingProgress;
    FprocessingStatus : String;
    FtagSuggestionsAvailability : String;
    FthumbnailsAvailability : String;
  Protected
    //Property setters
    Procedure SeteditorSuggestionsAvailability(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileDetailsAvailability(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingFailureReason(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingIssuesAvailability(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingProgress(AIndex : Integer; AValue : TVideoProcessingDetailsProcessingProgress); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SettagSuggestionsAvailability(AIndex : Integer; AValue : String); virtual;
    Procedure SetthumbnailsAvailability(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property editorSuggestionsAvailability : String Index 0 Read FeditorSuggestionsAvailability Write SeteditorSuggestionsAvailability;
    Property fileDetailsAvailability : String Index 8 Read FfileDetailsAvailability Write SetfileDetailsAvailability;
    Property processingFailureReason : String Index 16 Read FprocessingFailureReason Write SetprocessingFailureReason;
    Property processingIssuesAvailability : String Index 24 Read FprocessingIssuesAvailability Write SetprocessingIssuesAvailability;
    Property processingProgress : TVideoProcessingDetailsProcessingProgress Index 32 Read FprocessingProgress Write SetprocessingProgress;
    Property processingStatus : String Index 40 Read FprocessingStatus Write SetprocessingStatus;
    Property tagSuggestionsAvailability : String Index 48 Read FtagSuggestionsAvailability Write SettagSuggestionsAvailability;
    Property thumbnailsAvailability : String Index 56 Read FthumbnailsAvailability Write SetthumbnailsAvailability;
  end;
  TVideoProcessingDetailsClass = Class of TVideoProcessingDetails;
  
  { --------------------------------------------------------------------
    TVideoProcessingDetailsProcessingProgress
    --------------------------------------------------------------------}
  
  TVideoProcessingDetailsProcessingProgress = Class(TGoogleBaseObject)
  Private
    FpartsProcessed : String;
    FpartsTotal : String;
    FtimeLeftMs : String;
  Protected
    //Property setters
    Procedure SetpartsProcessed(AIndex : Integer; AValue : String); virtual;
    Procedure SetpartsTotal(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeLeftMs(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property partsProcessed : String Index 0 Read FpartsProcessed Write SetpartsProcessed;
    Property partsTotal : String Index 8 Read FpartsTotal Write SetpartsTotal;
    Property timeLeftMs : String Index 16 Read FtimeLeftMs Write SettimeLeftMs;
  end;
  TVideoProcessingDetailsProcessingProgressClass = Class of TVideoProcessingDetailsProcessingProgress;
  
  { --------------------------------------------------------------------
    TVideoProjectDetails
    --------------------------------------------------------------------}
  
  TVideoProjectDetails = Class(TGoogleBaseObject)
  Private
    Ftags : TStringArray;
  Protected
    //Property setters
    Procedure Settags(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property tags : TStringArray Index 0 Read Ftags Write Settags;
  end;
  TVideoProjectDetailsClass = Class of TVideoProjectDetails;
  
  { --------------------------------------------------------------------
    TVideoRating
    --------------------------------------------------------------------}
  
  TVideoRating = Class(TGoogleBaseObject)
  Private
    Frating : String;
    FvideoId : String;
  Protected
    //Property setters
    Procedure Setrating(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property rating : String Index 0 Read Frating Write Setrating;
    Property videoId : String Index 8 Read FvideoId Write SetvideoId;
  end;
  TVideoRatingClass = Class of TVideoRating;
  
  { --------------------------------------------------------------------
    TVideoRecordingDetails
    --------------------------------------------------------------------}
  
  TVideoRecordingDetails = Class(TGoogleBaseObject)
  Private
    Flocation : TGeoPoint;
    FlocationDescription : String;
    FrecordingDate : TDatetime;
  Protected
    //Property setters
    Procedure Setlocation(AIndex : Integer; AValue : TGeoPoint); virtual;
    Procedure SetlocationDescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetrecordingDate(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property location : TGeoPoint Index 0 Read Flocation Write Setlocation;
    Property locationDescription : String Index 8 Read FlocationDescription Write SetlocationDescription;
    Property recordingDate : TDatetime Index 16 Read FrecordingDate Write SetrecordingDate;
  end;
  TVideoRecordingDetailsClass = Class of TVideoRecordingDetails;
  
  { --------------------------------------------------------------------
    TVideoSnippet
    --------------------------------------------------------------------}
  
  TVideoSnippet = Class(TGoogleBaseObject)
  Private
    FcategoryId : String;
    FchannelId : String;
    FchannelTitle : String;
    FdefaultLanguage : String;
    Fdescription : String;
    FliveBroadcastContent : String;
    Flocalized : TVideoLocalization;
    FpublishedAt : TDatetime;
    Ftags : TStringArray;
    Fthumbnails : TThumbnailDetails;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetcategoryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchannelTitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetliveBroadcastContent(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocalized(AIndex : Integer; AValue : TVideoLocalization); virtual;
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settags(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property categoryId : String Index 0 Read FcategoryId Write SetcategoryId;
    Property channelId : String Index 8 Read FchannelId Write SetchannelId;
    Property channelTitle : String Index 16 Read FchannelTitle Write SetchannelTitle;
    Property defaultLanguage : String Index 24 Read FdefaultLanguage Write SetdefaultLanguage;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property liveBroadcastContent : String Index 40 Read FliveBroadcastContent Write SetliveBroadcastContent;
    Property localized : TVideoLocalization Index 48 Read Flocalized Write Setlocalized;
    Property publishedAt : TDatetime Index 56 Read FpublishedAt Write SetpublishedAt;
    Property tags : TStringArray Index 64 Read Ftags Write Settags;
    Property thumbnails : TThumbnailDetails Index 72 Read Fthumbnails Write Setthumbnails;
    Property title : String Index 80 Read Ftitle Write Settitle;
  end;
  TVideoSnippetClass = Class of TVideoSnippet;
  
  { --------------------------------------------------------------------
    TVideoStatistics
    --------------------------------------------------------------------}
  
  TVideoStatistics = Class(TGoogleBaseObject)
  Private
    FcommentCount : String;
    FdislikeCount : String;
    FfavoriteCount : String;
    FlikeCount : String;
    FviewCount : String;
  Protected
    //Property setters
    Procedure SetcommentCount(AIndex : Integer; AValue : String); virtual;
    Procedure SetdislikeCount(AIndex : Integer; AValue : String); virtual;
    Procedure SetfavoriteCount(AIndex : Integer; AValue : String); virtual;
    Procedure SetlikeCount(AIndex : Integer; AValue : String); virtual;
    Procedure SetviewCount(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property commentCount : String Index 0 Read FcommentCount Write SetcommentCount;
    Property dislikeCount : String Index 8 Read FdislikeCount Write SetdislikeCount;
    Property favoriteCount : String Index 16 Read FfavoriteCount Write SetfavoriteCount;
    Property likeCount : String Index 24 Read FlikeCount Write SetlikeCount;
    Property viewCount : String Index 32 Read FviewCount Write SetviewCount;
  end;
  TVideoStatisticsClass = Class of TVideoStatistics;
  
  { --------------------------------------------------------------------
    TVideoStatus
    --------------------------------------------------------------------}
  
  TVideoStatus = Class(TGoogleBaseObject)
  Private
    Fembeddable : boolean;
    FfailureReason : String;
    Flicense : String;
    FprivacyStatus : String;
    FpublicStatsViewable : boolean;
    FpublishAt : TDatetime;
    FrejectionReason : String;
    FuploadStatus : String;
  Protected
    //Property setters
    Procedure Setembeddable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfailureReason(AIndex : Integer; AValue : String); virtual;
    Procedure Setlicense(AIndex : Integer; AValue : String); virtual;
    Procedure SetprivacyStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublicStatsViewable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpublishAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetrejectionReason(AIndex : Integer; AValue : String); virtual;
    Procedure SetuploadStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property embeddable : boolean Index 0 Read Fembeddable Write Setembeddable;
    Property failureReason : String Index 8 Read FfailureReason Write SetfailureReason;
    Property license : String Index 16 Read Flicense Write Setlicense;
    Property privacyStatus : String Index 24 Read FprivacyStatus Write SetprivacyStatus;
    Property publicStatsViewable : boolean Index 32 Read FpublicStatsViewable Write SetpublicStatsViewable;
    Property publishAt : TDatetime Index 40 Read FpublishAt Write SetpublishAt;
    Property rejectionReason : String Index 48 Read FrejectionReason Write SetrejectionReason;
    Property uploadStatus : String Index 56 Read FuploadStatus Write SetuploadStatus;
  end;
  TVideoStatusClass = Class of TVideoStatus;
  
  { --------------------------------------------------------------------
    TVideoSuggestions
    --------------------------------------------------------------------}
  
  TVideoSuggestions = Class(TGoogleBaseObject)
  Private
    FeditorSuggestions : TStringArray;
    FprocessingErrors : TStringArray;
    FprocessingHints : TStringArray;
    FprocessingWarnings : TStringArray;
    FtagSuggestions : TVideoSuggestionsTypetagSuggestionsArray;
  Protected
    //Property setters
    Procedure SeteditorSuggestions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetprocessingErrors(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetprocessingHints(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetprocessingWarnings(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SettagSuggestions(AIndex : Integer; AValue : TVideoSuggestionsTypetagSuggestionsArray); virtual;
  Public
  Published
    Property editorSuggestions : TStringArray Index 0 Read FeditorSuggestions Write SeteditorSuggestions;
    Property processingErrors : TStringArray Index 8 Read FprocessingErrors Write SetprocessingErrors;
    Property processingHints : TStringArray Index 16 Read FprocessingHints Write SetprocessingHints;
    Property processingWarnings : TStringArray Index 24 Read FprocessingWarnings Write SetprocessingWarnings;
    Property tagSuggestions : TVideoSuggestionsTypetagSuggestionsArray Index 32 Read FtagSuggestions Write SettagSuggestions;
  end;
  TVideoSuggestionsClass = Class of TVideoSuggestions;
  
  { --------------------------------------------------------------------
    TVideoSuggestionsTagSuggestion
    --------------------------------------------------------------------}
  
  TVideoSuggestionsTagSuggestion = Class(TGoogleBaseObject)
  Private
    FcategoryRestricts : TStringArray;
    Ftag : String;
  Protected
    //Property setters
    Procedure SetcategoryRestricts(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Settag(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property categoryRestricts : TStringArray Index 0 Read FcategoryRestricts Write SetcategoryRestricts;
    Property tag : String Index 8 Read Ftag Write Settag;
  end;
  TVideoSuggestionsTagSuggestionClass = Class of TVideoSuggestionsTagSuggestion;
  
  { --------------------------------------------------------------------
    TVideoTopicDetails
    --------------------------------------------------------------------}
  
  TVideoTopicDetails = Class(TGoogleBaseObject)
  Private
    FrelevantTopicIds : TStringArray;
    FtopicIds : TStringArray;
  Protected
    //Property setters
    Procedure SetrelevantTopicIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SettopicIds(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property relevantTopicIds : TStringArray Index 0 Read FrelevantTopicIds Write SetrelevantTopicIds;
    Property topicIds : TStringArray Index 8 Read FtopicIds Write SettopicIds;
  end;
  TVideoTopicDetailsClass = Class of TVideoTopicDetails;
  
  { --------------------------------------------------------------------
    TWatchSettings
    --------------------------------------------------------------------}
  
  TWatchSettings = Class(TGoogleBaseObject)
  Private
    FbackgroundColor : String;
    FfeaturedPlaylistId : String;
    FtextColor : String;
  Protected
    //Property setters
    Procedure SetbackgroundColor(AIndex : Integer; AValue : String); virtual;
    Procedure SetfeaturedPlaylistId(AIndex : Integer; AValue : String); virtual;
    Procedure SettextColor(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property backgroundColor : String Index 0 Read FbackgroundColor Write SetbackgroundColor;
    Property featuredPlaylistId : String Index 8 Read FfeaturedPlaylistId Write SetfeaturedPlaylistId;
    Property textColor : String Index 16 Read FtextColor Write SettextColor;
  end;
  TWatchSettingsClass = Class of TWatchSettings;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method Insert
  
  TActivitiesInsertOptions = Record
    part : String;
  end;
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    channelId : String;
    home : boolean;
    maxResults : integer;
    mine : boolean;
    pageToken : String;
    part : String;
    publishedAfter : TDatetime;
    publishedBefore : TDatetime;
    regionCode : String;
  end;
  
  TActivitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(aActivity : TActivity; AQuery : string  = '') : TActivity;
    Function Insert(aActivity : TActivity; AQuery : TActivitiesinsertOptions) : TActivity;
    Function List(AQuery : string  = '') : TActivityListResponse;
    Function List(AQuery : TActivitieslistOptions) : TActivityListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCaptionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCaptionsResource, method Delete
  
  TCaptionsDeleteOptions = Record
    debugProjectIdOverride : int64;
    id : String;
    onBehalfOf : String;
  end;
  
  
  //Optional query Options for TCaptionsResource, method Download
  
  TCaptionsDownloadOptions = Record
    debugProjectIdOverride : int64;
    onBehalfOf : String;
    tfmt : String;
    tlang : String;
  end;
  
  
  //Optional query Options for TCaptionsResource, method Insert
  
  TCaptionsInsertOptions = Record
    debugProjectIdOverride : int64;
    onBehalfOf : String;
    part : String;
    sync : boolean;
  end;
  
  
  //Optional query Options for TCaptionsResource, method List
  
  TCaptionsListOptions = Record
    debugProjectIdOverride : int64;
    id : String;
    onBehalfOf : String;
    part : String;
    videoId : String;
  end;
  
  
  //Optional query Options for TCaptionsResource, method Update
  
  TCaptionsUpdateOptions = Record
    debugProjectIdOverride : int64;
    onBehalfOf : String;
    part : String;
    sync : boolean;
  end;
  
  TCaptionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TCaptionsdeleteOptions);
    Procedure Download(id: string; AQuery : string  = '');
    Procedure Download(id: string; AQuery : TCaptionsdownloadOptions);
    Function Insert(aCaption : TCaption; AQuery : string  = '') : TCaption;
    Function Insert(aCaption : TCaption; AQuery : TCaptionsinsertOptions) : TCaption;
    Function List(AQuery : string  = '') : TCaptionListResponse;
    Function List(AQuery : TCaptionslistOptions) : TCaptionListResponse;
    Function Update(aCaption : TCaption; AQuery : string  = '') : TCaption;
    Function Update(aCaption : TCaption; AQuery : TCaptionsupdateOptions) : TCaption;
  end;
  
  
  { --------------------------------------------------------------------
    TChannelBannersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TChannelBannersResource, method Insert
  
  TChannelBannersInsertOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  TChannelBannersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(aChannelBannerResource : TChannelBannerResource; AQuery : string  = '') : TChannelBannerResource;
    Function Insert(aChannelBannerResource : TChannelBannerResource; AQuery : TChannelBannersinsertOptions) : TChannelBannerResource;
  end;
  
  
  { --------------------------------------------------------------------
    TChannelSectionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TChannelSectionsResource, method Delete
  
  TChannelSectionsDeleteOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TChannelSectionsResource, method Insert
  
  TChannelSectionsInsertOptions = Record
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
  end;
  
  
  //Optional query Options for TChannelSectionsResource, method List
  
  TChannelSectionsListOptions = Record
    channelId : String;
    hl : String;
    id : String;
    mine : boolean;
    onBehalfOfContentOwner : String;
    part : String;
  end;
  
  
  //Optional query Options for TChannelSectionsResource, method Update
  
  TChannelSectionsUpdateOptions = Record
    onBehalfOfContentOwner : String;
    part : String;
  end;
  
  TChannelSectionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TChannelSectionsdeleteOptions);
    Function Insert(aChannelSection : TChannelSection; AQuery : string  = '') : TChannelSection;
    Function Insert(aChannelSection : TChannelSection; AQuery : TChannelSectionsinsertOptions) : TChannelSection;
    Function List(AQuery : string  = '') : TChannelSectionListResponse;
    Function List(AQuery : TChannelSectionslistOptions) : TChannelSectionListResponse;
    Function Update(aChannelSection : TChannelSection; AQuery : string  = '') : TChannelSection;
    Function Update(aChannelSection : TChannelSection; AQuery : TChannelSectionsupdateOptions) : TChannelSection;
  end;
  
  
  { --------------------------------------------------------------------
    TChannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TChannelsResource, method List
  
  TChannelsListOptions = Record
    categoryId : String;
    forUsername : String;
    hl : String;
    id : String;
    managedByMe : boolean;
    maxResults : integer;
    mine : boolean;
    mySubscribers : boolean;
    onBehalfOfContentOwner : String;
    pageToken : String;
    part : String;
  end;
  
  
  //Optional query Options for TChannelsResource, method Update
  
  TChannelsUpdateOptions = Record
    onBehalfOfContentOwner : String;
    part : String;
  end;
  
  TChannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TChannelListResponse;
    Function List(AQuery : TChannelslistOptions) : TChannelListResponse;
    Function Update(aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Update(aChannel : TChannel; AQuery : TChannelsupdateOptions) : TChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TCommentThreadsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCommentThreadsResource, method Insert
  
  TCommentThreadsInsertOptions = Record
    part : String;
    shareOnGooglePlus : boolean;
  end;
  
  
  //Optional query Options for TCommentThreadsResource, method List
  
  TCommentThreadsListOptions = Record
    allThreadsRelatedToChannelId : String;
    channelId : String;
    id : String;
    maxResults : integer;
    moderationStatus : String;
    pageToken : String;
    part : String;
    searchTerms : String;
    textFormat : String;
    videoId : String;
  end;
  
  
  //Optional query Options for TCommentThreadsResource, method Update
  
  TCommentThreadsUpdateOptions = Record
    part : String;
  end;
  
  TCommentThreadsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(aCommentThread : TCommentThread; AQuery : string  = '') : TCommentThread;
    Function Insert(aCommentThread : TCommentThread; AQuery : TCommentThreadsinsertOptions) : TCommentThread;
    Function List(AQuery : string  = '') : TCommentThreadListResponse;
    Function List(AQuery : TCommentThreadslistOptions) : TCommentThreadListResponse;
    Function Update(aCommentThread : TCommentThread; AQuery : string  = '') : TCommentThread;
    Function Update(aCommentThread : TCommentThread; AQuery : TCommentThreadsupdateOptions) : TCommentThread;
  end;
  
  
  { --------------------------------------------------------------------
    TCommentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCommentsResource, method Delete
  
  TCommentsDeleteOptions = Record
    id : String;
  end;
  
  
  //Optional query Options for TCommentsResource, method Insert
  
  TCommentsInsertOptions = Record
    part : String;
  end;
  
  
  //Optional query Options for TCommentsResource, method List
  
  TCommentsListOptions = Record
    id : String;
    maxResults : integer;
    pageToken : String;
    parentId : String;
    part : String;
    textFormat : String;
  end;
  
  
  //Optional query Options for TCommentsResource, method MarkAsSpam
  
  TCommentsMarkAsSpamOptions = Record
    id : String;
  end;
  
  
  //Optional query Options for TCommentsResource, method SetModerationStatus
  
  TCommentsSetModerationStatusOptions = Record
    banAuthor : boolean;
    id : String;
    moderationStatus : String;
  end;
  
  
  //Optional query Options for TCommentsResource, method Update
  
  TCommentsUpdateOptions = Record
    part : String;
  end;
  
  TCommentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TCommentsdeleteOptions);
    Function Insert(aComment : TComment; AQuery : string  = '') : TComment;
    Function Insert(aComment : TComment; AQuery : TCommentsinsertOptions) : TComment;
    Function List(AQuery : string  = '') : TCommentListResponse;
    Function List(AQuery : TCommentslistOptions) : TCommentListResponse;
    Procedure MarkAsSpam(AQuery : string  = '');
    Procedure MarkAsSpam(AQuery : TCommentsmarkAsSpamOptions);
    Procedure SetModerationStatus(AQuery : string  = '');
    Procedure SetModerationStatus(AQuery : TCommentssetModerationStatusOptions);
    Function Update(aComment : TComment; AQuery : string  = '') : TComment;
    Function Update(aComment : TComment; AQuery : TCommentsupdateOptions) : TComment;
  end;
  
  
  { --------------------------------------------------------------------
    TGuideCategoriesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGuideCategoriesResource, method List
  
  TGuideCategoriesListOptions = Record
    hl : String;
    id : String;
    part : String;
    regionCode : String;
  end;
  
  TGuideCategoriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TGuideCategoryListResponse;
    Function List(AQuery : TGuideCategorieslistOptions) : TGuideCategoryListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TI18nLanguagesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TI18nLanguagesResource, method List
  
  TI18nLanguagesListOptions = Record
    hl : String;
    part : String;
  end;
  
  TI18nLanguagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TI18nLanguageListResponse;
    Function List(AQuery : TI18nLanguageslistOptions) : TI18nLanguageListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TI18nRegionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TI18nRegionsResource, method List
  
  TI18nRegionsListOptions = Record
    hl : String;
    part : String;
  end;
  
  TI18nRegionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TI18nRegionListResponse;
    Function List(AQuery : TI18nRegionslistOptions) : TI18nRegionListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TLiveBroadcastsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLiveBroadcastsResource, method Bind
  
  TLiveBroadcastsBindOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
    streamId : String;
  end;
  
  
  //Optional query Options for TLiveBroadcastsResource, method Control
  
  TLiveBroadcastsControlOptions = Record
    displaySlate : boolean;
    id : String;
    offsetTimeMs : String;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
    walltime : TDatetime;
  end;
  
  
  //Optional query Options for TLiveBroadcastsResource, method Delete
  
  TLiveBroadcastsDeleteOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
  end;
  
  
  //Optional query Options for TLiveBroadcastsResource, method Insert
  
  TLiveBroadcastsInsertOptions = Record
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
  end;
  
  
  //Optional query Options for TLiveBroadcastsResource, method List
  
  TLiveBroadcastsListOptions = Record
    broadcastStatus : String;
    id : String;
    maxResults : integer;
    mine : boolean;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    pageToken : String;
    part : String;
  end;
  
  
  //Optional query Options for TLiveBroadcastsResource, method Transition
  
  TLiveBroadcastsTransitionOptions = Record
    broadcastStatus : String;
    id : String;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
  end;
  
  
  //Optional query Options for TLiveBroadcastsResource, method Update
  
  TLiveBroadcastsUpdateOptions = Record
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
  end;
  
  TLiveBroadcastsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Bind(AQuery : string  = '') : TLiveBroadcast;
    Function Bind(AQuery : TLiveBroadcastsbindOptions) : TLiveBroadcast;
    Function Control(AQuery : string  = '') : TLiveBroadcast;
    Function Control(AQuery : TLiveBroadcastscontrolOptions) : TLiveBroadcast;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TLiveBroadcastsdeleteOptions);
    Function Insert(aLiveBroadcast : TLiveBroadcast; AQuery : string  = '') : TLiveBroadcast;
    Function Insert(aLiveBroadcast : TLiveBroadcast; AQuery : TLiveBroadcastsinsertOptions) : TLiveBroadcast;
    Function List(AQuery : string  = '') : TLiveBroadcastListResponse;
    Function List(AQuery : TLiveBroadcastslistOptions) : TLiveBroadcastListResponse;
    Function Transition(AQuery : string  = '') : TLiveBroadcast;
    Function Transition(AQuery : TLiveBroadcaststransitionOptions) : TLiveBroadcast;
    Function Update(aLiveBroadcast : TLiveBroadcast; AQuery : string  = '') : TLiveBroadcast;
    Function Update(aLiveBroadcast : TLiveBroadcast; AQuery : TLiveBroadcastsupdateOptions) : TLiveBroadcast;
  end;
  
  
  { --------------------------------------------------------------------
    TLiveStreamsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLiveStreamsResource, method Delete
  
  TLiveStreamsDeleteOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
  end;
  
  
  //Optional query Options for TLiveStreamsResource, method Insert
  
  TLiveStreamsInsertOptions = Record
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
  end;
  
  
  //Optional query Options for TLiveStreamsResource, method List
  
  TLiveStreamsListOptions = Record
    id : String;
    maxResults : integer;
    mine : boolean;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    pageToken : String;
    part : String;
  end;
  
  
  //Optional query Options for TLiveStreamsResource, method Update
  
  TLiveStreamsUpdateOptions = Record
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
  end;
  
  TLiveStreamsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TLiveStreamsdeleteOptions);
    Function Insert(aLiveStream : TLiveStream; AQuery : string  = '') : TLiveStream;
    Function Insert(aLiveStream : TLiveStream; AQuery : TLiveStreamsinsertOptions) : TLiveStream;
    Function List(AQuery : string  = '') : TLiveStreamListResponse;
    Function List(AQuery : TLiveStreamslistOptions) : TLiveStreamListResponse;
    Function Update(aLiveStream : TLiveStream; AQuery : string  = '') : TLiveStream;
    Function Update(aLiveStream : TLiveStream; AQuery : TLiveStreamsupdateOptions) : TLiveStream;
  end;
  
  
  { --------------------------------------------------------------------
    TPlaylistItemsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPlaylistItemsResource, method Delete
  
  TPlaylistItemsDeleteOptions = Record
    id : String;
  end;
  
  
  //Optional query Options for TPlaylistItemsResource, method Insert
  
  TPlaylistItemsInsertOptions = Record
    onBehalfOfContentOwner : String;
    part : String;
  end;
  
  
  //Optional query Options for TPlaylistItemsResource, method List
  
  TPlaylistItemsListOptions = Record
    id : String;
    maxResults : integer;
    onBehalfOfContentOwner : String;
    pageToken : String;
    part : String;
    playlistId : String;
    videoId : String;
  end;
  
  
  //Optional query Options for TPlaylistItemsResource, method Update
  
  TPlaylistItemsUpdateOptions = Record
    part : String;
  end;
  
  TPlaylistItemsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TPlaylistItemsdeleteOptions);
    Function Insert(aPlaylistItem : TPlaylistItem; AQuery : string  = '') : TPlaylistItem;
    Function Insert(aPlaylistItem : TPlaylistItem; AQuery : TPlaylistItemsinsertOptions) : TPlaylistItem;
    Function List(AQuery : string  = '') : TPlaylistItemListResponse;
    Function List(AQuery : TPlaylistItemslistOptions) : TPlaylistItemListResponse;
    Function Update(aPlaylistItem : TPlaylistItem; AQuery : string  = '') : TPlaylistItem;
    Function Update(aPlaylistItem : TPlaylistItem; AQuery : TPlaylistItemsupdateOptions) : TPlaylistItem;
  end;
  
  
  { --------------------------------------------------------------------
    TPlaylistsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPlaylistsResource, method Delete
  
  TPlaylistsDeleteOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TPlaylistsResource, method Insert
  
  TPlaylistsInsertOptions = Record
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
  end;
  
  
  //Optional query Options for TPlaylistsResource, method List
  
  TPlaylistsListOptions = Record
    channelId : String;
    hl : String;
    id : String;
    maxResults : integer;
    mine : boolean;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    pageToken : String;
    part : String;
  end;
  
  
  //Optional query Options for TPlaylistsResource, method Update
  
  TPlaylistsUpdateOptions = Record
    onBehalfOfContentOwner : String;
    part : String;
  end;
  
  TPlaylistsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TPlaylistsdeleteOptions);
    Function Insert(aPlaylist : TPlaylist; AQuery : string  = '') : TPlaylist;
    Function Insert(aPlaylist : TPlaylist; AQuery : TPlaylistsinsertOptions) : TPlaylist;
    Function List(AQuery : string  = '') : TPlaylistListResponse;
    Function List(AQuery : TPlaylistslistOptions) : TPlaylistListResponse;
    Function Update(aPlaylist : TPlaylist; AQuery : string  = '') : TPlaylist;
    Function Update(aPlaylist : TPlaylist; AQuery : TPlaylistsupdateOptions) : TPlaylist;
  end;
  
  
  { --------------------------------------------------------------------
    TSearchResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSearchResource, method List
  
  TSearchListOptions = Record
    channelId : String;
    channelType : String;
    eventType : String;
    forContentOwner : boolean;
    forDeveloper : boolean;
    forMine : boolean;
    location : String;
    locationRadius : String;
    maxResults : integer;
    onBehalfOfContentOwner : String;
    order : String;
    pageToken : String;
    part : String;
    publishedAfter : TDatetime;
    publishedBefore : TDatetime;
    q : String;
    regionCode : String;
    relatedToVideoId : String;
    relevanceLanguage : String;
    safeSearch : String;
    topicId : String;
    _type : String;
    videoCaption : String;
    videoCategoryId : String;
    videoDefinition : String;
    videoDimension : String;
    videoDuration : String;
    videoEmbeddable : String;
    videoLicense : String;
    videoSyndicated : String;
    videoType : String;
  end;
  
  TSearchResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TSearchListResponse;
    Function List(AQuery : TSearchlistOptions) : TSearchListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSubscriptionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSubscriptionsResource, method Delete
  
  TSubscriptionsDeleteOptions = Record
    id : String;
  end;
  
  
  //Optional query Options for TSubscriptionsResource, method Insert
  
  TSubscriptionsInsertOptions = Record
    part : String;
  end;
  
  
  //Optional query Options for TSubscriptionsResource, method List
  
  TSubscriptionsListOptions = Record
    channelId : String;
    forChannelId : String;
    id : String;
    maxResults : integer;
    mine : boolean;
    mySubscribers : boolean;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    order : String;
    pageToken : String;
    part : String;
  end;
  
  TSubscriptionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TSubscriptionsdeleteOptions);
    Function Insert(aSubscription : TSubscription; AQuery : string  = '') : TSubscription;
    Function Insert(aSubscription : TSubscription; AQuery : TSubscriptionsinsertOptions) : TSubscription;
    Function List(AQuery : string  = '') : TSubscriptionListResponse;
    Function List(AQuery : TSubscriptionslistOptions) : TSubscriptionListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TThumbnailsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TThumbnailsResource, method Set
  
  TThumbnailsSetOptions = Record
    onBehalfOfContentOwner : String;
    videoId : String;
  end;
  
  TThumbnailsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function _set(AQuery : string  = '') : TThumbnailSetResponse;
    Function _set(AQuery : TThumbnailssetOptions) : TThumbnailSetResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TVideoAbuseReportReasonsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVideoAbuseReportReasonsResource, method List
  
  TVideoAbuseReportReasonsListOptions = Record
    hl : String;
    part : String;
  end;
  
  TVideoAbuseReportReasonsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TVideoAbuseReportReasonListResponse;
    Function List(AQuery : TVideoAbuseReportReasonslistOptions) : TVideoAbuseReportReasonListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TVideoCategoriesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVideoCategoriesResource, method List
  
  TVideoCategoriesListOptions = Record
    hl : String;
    id : String;
    part : String;
    regionCode : String;
  end;
  
  TVideoCategoriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TVideoCategoryListResponse;
    Function List(AQuery : TVideoCategorieslistOptions) : TVideoCategoryListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TVideosResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVideosResource, method Delete
  
  TVideosDeleteOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TVideosResource, method GetRating
  
  TVideosGetRatingOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TVideosResource, method Insert
  
  TVideosInsertOptions = Record
    autoLevels : boolean;
    notifySubscribers : boolean;
    onBehalfOfContentOwner : String;
    onBehalfOfContentOwnerChannel : String;
    part : String;
    stabilize : boolean;
  end;
  
  
  //Optional query Options for TVideosResource, method List
  
  TVideosListOptions = Record
    chart : String;
    hl : String;
    id : String;
    locale : String;
    maxResults : integer;
    myRating : String;
    onBehalfOfContentOwner : String;
    pageToken : String;
    part : String;
    regionCode : String;
    videoCategoryId : String;
  end;
  
  
  //Optional query Options for TVideosResource, method Rate
  
  TVideosRateOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
    rating : String;
  end;
  
  
  //Optional query Options for TVideosResource, method ReportAbuse
  
  TVideosReportAbuseOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TVideosResource, method Update
  
  TVideosUpdateOptions = Record
    onBehalfOfContentOwner : String;
    part : String;
  end;
  
  TVideosResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TVideosdeleteOptions);
    Function GetRating(AQuery : string  = '') : TVideoGetRatingResponse;
    Function GetRating(AQuery : TVideosgetRatingOptions) : TVideoGetRatingResponse;
    Function Insert(aVideo : TVideo; AQuery : string  = '') : TVideo;
    Function Insert(aVideo : TVideo; AQuery : TVideosinsertOptions) : TVideo;
    Function List(AQuery : string  = '') : TVideoListResponse;
    Function List(AQuery : TVideoslistOptions) : TVideoListResponse;
    Procedure Rate(AQuery : string  = '');
    Procedure Rate(AQuery : TVideosrateOptions);
    Procedure ReportAbuse(aVideoAbuseReport : TVideoAbuseReport; AQuery : string  = '');
    Procedure ReportAbuse(aVideoAbuseReport : TVideoAbuseReport; AQuery : TVideosreportAbuseOptions);
    Function Update(aVideo : TVideo; AQuery : string  = '') : TVideo;
    Function Update(aVideo : TVideo; AQuery : TVideosupdateOptions) : TVideo;
  end;
  
  
  { --------------------------------------------------------------------
    TWatermarksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TWatermarksResource, method Set
  
  TWatermarksSetOptions = Record
    channelId : String;
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TWatermarksResource, method Unset
  
  TWatermarksUnsetOptions = Record
    channelId : String;
    onBehalfOfContentOwner : String;
  end;
  
  TWatermarksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure _set(aInvideoBranding : TInvideoBranding; AQuery : string  = '');
    Procedure _set(aInvideoBranding : TInvideoBranding; AQuery : TWatermarkssetOptions);
    Procedure Unset(AQuery : string  = '');
    Procedure Unset(AQuery : TWatermarksunsetOptions);
  end;
  
  
  { --------------------------------------------------------------------
    TYoutubeAPI
    --------------------------------------------------------------------}
  
  TYoutubeAPI = Class(TGoogleAPI)
  Private
    FActivitiesInstance : TActivitiesResource;
    FCaptionsInstance : TCaptionsResource;
    FChannelBannersInstance : TChannelBannersResource;
    FChannelSectionsInstance : TChannelSectionsResource;
    FChannelsInstance : TChannelsResource;
    FCommentThreadsInstance : TCommentThreadsResource;
    FCommentsInstance : TCommentsResource;
    FGuideCategoriesInstance : TGuideCategoriesResource;
    FI18nLanguagesInstance : TI18nLanguagesResource;
    FI18nRegionsInstance : TI18nRegionsResource;
    FLiveBroadcastsInstance : TLiveBroadcastsResource;
    FLiveStreamsInstance : TLiveStreamsResource;
    FPlaylistItemsInstance : TPlaylistItemsResource;
    FPlaylistsInstance : TPlaylistsResource;
    FSearchInstance : TSearchResource;
    FSubscriptionsInstance : TSubscriptionsResource;
    FThumbnailsInstance : TThumbnailsResource;
    FVideoAbuseReportReasonsInstance : TVideoAbuseReportReasonsResource;
    FVideoCategoriesInstance : TVideoCategoriesResource;
    FVideosInstance : TVideosResource;
    FWatermarksInstance : TWatermarksResource;
    Function GetActivitiesInstance : TActivitiesResource;virtual;
    Function GetCaptionsInstance : TCaptionsResource;virtual;
    Function GetChannelBannersInstance : TChannelBannersResource;virtual;
    Function GetChannelSectionsInstance : TChannelSectionsResource;virtual;
    Function GetChannelsInstance : TChannelsResource;virtual;
    Function GetCommentThreadsInstance : TCommentThreadsResource;virtual;
    Function GetCommentsInstance : TCommentsResource;virtual;
    Function GetGuideCategoriesInstance : TGuideCategoriesResource;virtual;
    Function GetI18nLanguagesInstance : TI18nLanguagesResource;virtual;
    Function GetI18nRegionsInstance : TI18nRegionsResource;virtual;
    Function GetLiveBroadcastsInstance : TLiveBroadcastsResource;virtual;
    Function GetLiveStreamsInstance : TLiveStreamsResource;virtual;
    Function GetPlaylistItemsInstance : TPlaylistItemsResource;virtual;
    Function GetPlaylistsInstance : TPlaylistsResource;virtual;
    Function GetSearchInstance : TSearchResource;virtual;
    Function GetSubscriptionsInstance : TSubscriptionsResource;virtual;
    Function GetThumbnailsInstance : TThumbnailsResource;virtual;
    Function GetVideoAbuseReportReasonsInstance : TVideoAbuseReportReasonsResource;virtual;
    Function GetVideoCategoriesInstance : TVideoCategoriesResource;virtual;
    Function GetVideosInstance : TVideosResource;virtual;
    Function GetWatermarksInstance : TWatermarksResource;virtual;
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
    Function CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;virtual;overload;
    Function CreateActivitiesResource : TActivitiesResource;virtual;overload;
    Function CreateCaptionsResource(AOwner : TComponent) : TCaptionsResource;virtual;overload;
    Function CreateCaptionsResource : TCaptionsResource;virtual;overload;
    Function CreateChannelBannersResource(AOwner : TComponent) : TChannelBannersResource;virtual;overload;
    Function CreateChannelBannersResource : TChannelBannersResource;virtual;overload;
    Function CreateChannelSectionsResource(AOwner : TComponent) : TChannelSectionsResource;virtual;overload;
    Function CreateChannelSectionsResource : TChannelSectionsResource;virtual;overload;
    Function CreateChannelsResource(AOwner : TComponent) : TChannelsResource;virtual;overload;
    Function CreateChannelsResource : TChannelsResource;virtual;overload;
    Function CreateCommentThreadsResource(AOwner : TComponent) : TCommentThreadsResource;virtual;overload;
    Function CreateCommentThreadsResource : TCommentThreadsResource;virtual;overload;
    Function CreateCommentsResource(AOwner : TComponent) : TCommentsResource;virtual;overload;
    Function CreateCommentsResource : TCommentsResource;virtual;overload;
    Function CreateGuideCategoriesResource(AOwner : TComponent) : TGuideCategoriesResource;virtual;overload;
    Function CreateGuideCategoriesResource : TGuideCategoriesResource;virtual;overload;
    Function CreateI18nLanguagesResource(AOwner : TComponent) : TI18nLanguagesResource;virtual;overload;
    Function CreateI18nLanguagesResource : TI18nLanguagesResource;virtual;overload;
    Function CreateI18nRegionsResource(AOwner : TComponent) : TI18nRegionsResource;virtual;overload;
    Function CreateI18nRegionsResource : TI18nRegionsResource;virtual;overload;
    Function CreateLiveBroadcastsResource(AOwner : TComponent) : TLiveBroadcastsResource;virtual;overload;
    Function CreateLiveBroadcastsResource : TLiveBroadcastsResource;virtual;overload;
    Function CreateLiveStreamsResource(AOwner : TComponent) : TLiveStreamsResource;virtual;overload;
    Function CreateLiveStreamsResource : TLiveStreamsResource;virtual;overload;
    Function CreatePlaylistItemsResource(AOwner : TComponent) : TPlaylistItemsResource;virtual;overload;
    Function CreatePlaylistItemsResource : TPlaylistItemsResource;virtual;overload;
    Function CreatePlaylistsResource(AOwner : TComponent) : TPlaylistsResource;virtual;overload;
    Function CreatePlaylistsResource : TPlaylistsResource;virtual;overload;
    Function CreateSearchResource(AOwner : TComponent) : TSearchResource;virtual;overload;
    Function CreateSearchResource : TSearchResource;virtual;overload;
    Function CreateSubscriptionsResource(AOwner : TComponent) : TSubscriptionsResource;virtual;overload;
    Function CreateSubscriptionsResource : TSubscriptionsResource;virtual;overload;
    Function CreateThumbnailsResource(AOwner : TComponent) : TThumbnailsResource;virtual;overload;
    Function CreateThumbnailsResource : TThumbnailsResource;virtual;overload;
    Function CreateVideoAbuseReportReasonsResource(AOwner : TComponent) : TVideoAbuseReportReasonsResource;virtual;overload;
    Function CreateVideoAbuseReportReasonsResource : TVideoAbuseReportReasonsResource;virtual;overload;
    Function CreateVideoCategoriesResource(AOwner : TComponent) : TVideoCategoriesResource;virtual;overload;
    Function CreateVideoCategoriesResource : TVideoCategoriesResource;virtual;overload;
    Function CreateVideosResource(AOwner : TComponent) : TVideosResource;virtual;overload;
    Function CreateVideosResource : TVideosResource;virtual;overload;
    Function CreateWatermarksResource(AOwner : TComponent) : TWatermarksResource;virtual;overload;
    Function CreateWatermarksResource : TWatermarksResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ActivitiesResource : TActivitiesResource Read GetActivitiesInstance;
    Property CaptionsResource : TCaptionsResource Read GetCaptionsInstance;
    Property ChannelBannersResource : TChannelBannersResource Read GetChannelBannersInstance;
    Property ChannelSectionsResource : TChannelSectionsResource Read GetChannelSectionsInstance;
    Property ChannelsResource : TChannelsResource Read GetChannelsInstance;
    Property CommentThreadsResource : TCommentThreadsResource Read GetCommentThreadsInstance;
    Property CommentsResource : TCommentsResource Read GetCommentsInstance;
    Property GuideCategoriesResource : TGuideCategoriesResource Read GetGuideCategoriesInstance;
    Property I18nLanguagesResource : TI18nLanguagesResource Read GetI18nLanguagesInstance;
    Property I18nRegionsResource : TI18nRegionsResource Read GetI18nRegionsInstance;
    Property LiveBroadcastsResource : TLiveBroadcastsResource Read GetLiveBroadcastsInstance;
    Property LiveStreamsResource : TLiveStreamsResource Read GetLiveStreamsInstance;
    Property PlaylistItemsResource : TPlaylistItemsResource Read GetPlaylistItemsInstance;
    Property PlaylistsResource : TPlaylistsResource Read GetPlaylistsInstance;
    Property SearchResource : TSearchResource Read GetSearchInstance;
    Property SubscriptionsResource : TSubscriptionsResource Read GetSubscriptionsInstance;
    Property ThumbnailsResource : TThumbnailsResource Read GetThumbnailsInstance;
    Property VideoAbuseReportReasonsResource : TVideoAbuseReportReasonsResource Read GetVideoAbuseReportReasonsInstance;
    Property VideoCategoriesResource : TVideoCategoriesResource Read GetVideoCategoriesInstance;
    Property VideosResource : TVideosResource Read GetVideosInstance;
    Property WatermarksResource : TWatermarksResource Read GetWatermarksInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccessPolicy
  --------------------------------------------------------------------}


Procedure TAccessPolicy.Setallowed(AIndex : Integer; AValue : boolean); 

begin
  If (Fallowed=AValue) then exit;
  Fallowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccessPolicy.Setexception(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fexception=AValue) then exit;
  Fexception:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivity
  --------------------------------------------------------------------}


Procedure TActivity.SetcontentDetails(AIndex : Integer; AValue : TActivityContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setsnippet(AIndex : Integer; AValue : TActivitySnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetails
  --------------------------------------------------------------------}


Procedure TActivityContentDetails.Setbulletin(AIndex : Integer; AValue : TActivityContentDetailsBulletin); 

begin
  If (Fbulletin=AValue) then exit;
  Fbulletin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.SetchannelItem(AIndex : Integer; AValue : TActivityContentDetailsChannelItem); 

begin
  If (FchannelItem=AValue) then exit;
  FchannelItem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.Setcomment(AIndex : Integer; AValue : TActivityContentDetailsComment); 

begin
  If (Fcomment=AValue) then exit;
  Fcomment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.Setfavorite(AIndex : Integer; AValue : TActivityContentDetailsFavorite); 

begin
  If (Ffavorite=AValue) then exit;
  Ffavorite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.Setlike(AIndex : Integer; AValue : TActivityContentDetailsLike); 

begin
  If (Flike=AValue) then exit;
  Flike:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.SetplaylistItem(AIndex : Integer; AValue : TActivityContentDetailsPlaylistItem); 

begin
  If (FplaylistItem=AValue) then exit;
  FplaylistItem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.SetpromotedItem(AIndex : Integer; AValue : TActivityContentDetailsPromotedItem); 

begin
  If (FpromotedItem=AValue) then exit;
  FpromotedItem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.Setrecommendation(AIndex : Integer; AValue : TActivityContentDetailsRecommendation); 

begin
  If (Frecommendation=AValue) then exit;
  Frecommendation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.Setsocial(AIndex : Integer; AValue : TActivityContentDetailsSocial); 

begin
  If (Fsocial=AValue) then exit;
  Fsocial:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.Setsubscription(AIndex : Integer; AValue : TActivityContentDetailsSubscription); 

begin
  If (Fsubscription=AValue) then exit;
  Fsubscription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetails.Setupload(AIndex : Integer; AValue : TActivityContentDetailsUpload); 

begin
  If (Fupload=AValue) then exit;
  Fupload:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsBulletin
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsBulletin.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsChannelItem
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsChannelItem.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsComment
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsComment.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsFavorite
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsFavorite.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsLike
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsLike.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsPlaylistItem
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsPlaylistItem.SetplaylistId(AIndex : Integer; AValue : String); 

begin
  If (FplaylistId=AValue) then exit;
  FplaylistId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPlaylistItem.SetplaylistItemId(AIndex : Integer; AValue : String); 

begin
  If (FplaylistItemId=AValue) then exit;
  FplaylistItemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPlaylistItem.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsPromotedItem
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsPromotedItem.SetadTag(AIndex : Integer; AValue : String); 

begin
  If (FadTag=AValue) then exit;
  FadTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetclickTrackingUrl(AIndex : Integer; AValue : String); 

begin
  If (FclickTrackingUrl=AValue) then exit;
  FclickTrackingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetcreativeViewUrl(AIndex : Integer; AValue : String); 

begin
  If (FcreativeViewUrl=AValue) then exit;
  FcreativeViewUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetctaType(AIndex : Integer; AValue : String); 

begin
  If (FctaType=AValue) then exit;
  FctaType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetcustomCtaButtonText(AIndex : Integer; AValue : String); 

begin
  If (FcustomCtaButtonText=AValue) then exit;
  FcustomCtaButtonText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetdescriptionText(AIndex : Integer; AValue : String); 

begin
  If (FdescriptionText=AValue) then exit;
  FdescriptionText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetdestinationUrl(AIndex : Integer; AValue : String); 

begin
  If (FdestinationUrl=AValue) then exit;
  FdestinationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetforecastingUrl(AIndex : Integer; AValue : TStringArray); 

begin
  If (FforecastingUrl=AValue) then exit;
  FforecastingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetimpressionUrl(AIndex : Integer; AValue : TStringArray); 

begin
  If (FimpressionUrl=AValue) then exit;
  FimpressionUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsPromotedItem.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsRecommendation
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsRecommendation.Setreason(AIndex : Integer; AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsRecommendation.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsRecommendation.SetseedResourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FseedResourceId=AValue) then exit;
  FseedResourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsSocial
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsSocial.Setauthor(AIndex : Integer; AValue : String); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsSocial.SetimageUrl(AIndex : Integer; AValue : String); 

begin
  If (FimageUrl=AValue) then exit;
  FimageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsSocial.SetreferenceUrl(AIndex : Integer; AValue : String); 

begin
  If (FreferenceUrl=AValue) then exit;
  FreferenceUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsSocial.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityContentDetailsSocial.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityContentDetailsSocial.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityContentDetailsSubscription
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsSubscription.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityContentDetailsUpload
  --------------------------------------------------------------------}


Procedure TActivityContentDetailsUpload.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityListResponse
  --------------------------------------------------------------------}


Procedure TActivityListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityListResponse.Setitems(AIndex : Integer; AValue : TActivityListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivitySnippet
  --------------------------------------------------------------------}


Procedure TActivitySnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivitySnippet.SetchannelTitle(AIndex : Integer; AValue : String); 

begin
  If (FchannelTitle=AValue) then exit;
  FchannelTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivitySnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivitySnippet.SetgroupId(AIndex : Integer; AValue : String); 

begin
  If (FgroupId=AValue) then exit;
  FgroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivitySnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivitySnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivitySnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivitySnippet.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivitySnippet.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCaption
  --------------------------------------------------------------------}


Procedure TCaption.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaption.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaption.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaption.Setsnippet(AIndex : Integer; AValue : TCaptionSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCaptionListResponse
  --------------------------------------------------------------------}


Procedure TCaptionListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionListResponse.Setitems(AIndex : Integer; AValue : TCaptionListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCaptionSnippet
  --------------------------------------------------------------------}


Procedure TCaptionSnippet.SetaudioTrackType(AIndex : Integer; AValue : String); 

begin
  If (FaudioTrackType=AValue) then exit;
  FaudioTrackType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SetfailureReason(AIndex : Integer; AValue : String); 

begin
  If (FfailureReason=AValue) then exit;
  FfailureReason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SetisAutoSynced(AIndex : Integer; AValue : boolean); 

begin
  If (FisAutoSynced=AValue) then exit;
  FisAutoSynced:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SetisCC(AIndex : Integer; AValue : boolean); 

begin
  If (FisCC=AValue) then exit;
  FisCC:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SetisDraft(AIndex : Integer; AValue : boolean); 

begin
  If (FisDraft=AValue) then exit;
  FisDraft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SetisEasyReader(AIndex : Integer; AValue : boolean); 

begin
  If (FisEasyReader=AValue) then exit;
  FisEasyReader:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SetisLarge(AIndex : Integer; AValue : boolean); 

begin
  If (FisLarge=AValue) then exit;
  FisLarge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SetlastUpdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastUpdated=AValue) then exit;
  FlastUpdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SettrackKind(AIndex : Integer; AValue : String); 

begin
  If (FtrackKind=AValue) then exit;
  FtrackKind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCaptionSnippet.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCdnSettings
  --------------------------------------------------------------------}


Procedure TCdnSettings.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCdnSettings.SetingestionInfo(AIndex : Integer; AValue : TIngestionInfo); 

begin
  If (FingestionInfo=AValue) then exit;
  FingestionInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCdnSettings.SetingestionType(AIndex : Integer; AValue : String); 

begin
  If (FingestionType=AValue) then exit;
  FingestionType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelTypelocalizations
  --------------------------------------------------------------------}


Class Function TChannelTypelocalizations.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TChannel
  --------------------------------------------------------------------}


Procedure TChannel.SetauditDetails(AIndex : Integer; AValue : TChannelAuditDetails); 

begin
  If (FauditDetails=AValue) then exit;
  FauditDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetbrandingSettings(AIndex : Integer; AValue : TChannelBrandingSettings); 

begin
  If (FbrandingSettings=AValue) then exit;
  FbrandingSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetcontentDetails(AIndex : Integer; AValue : TChannelContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetcontentOwnerDetails(AIndex : Integer; AValue : TChannelContentOwnerDetails); 

begin
  If (FcontentOwnerDetails=AValue) then exit;
  FcontentOwnerDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetconversionPings(AIndex : Integer; AValue : TChannelConversionPings); 

begin
  If (FconversionPings=AValue) then exit;
  FconversionPings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetinvideoPromotion(AIndex : Integer; AValue : TInvideoPromotion); 

begin
  If (FinvideoPromotion=AValue) then exit;
  FinvideoPromotion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setlocalizations(AIndex : Integer; AValue : TChannelTypelocalizations); 

begin
  If (Flocalizations=AValue) then exit;
  Flocalizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setsnippet(AIndex : Integer; AValue : TChannelSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setstatistics(AIndex : Integer; AValue : TChannelStatistics); 

begin
  If (Fstatistics=AValue) then exit;
  Fstatistics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setstatus(AIndex : Integer; AValue : TChannelStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SettopicDetails(AIndex : Integer; AValue : TChannelTopicDetails); 

begin
  If (FtopicDetails=AValue) then exit;
  FtopicDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelAuditDetails
  --------------------------------------------------------------------}


Procedure TChannelAuditDetails.SetcommunityGuidelinesGoodStanding(AIndex : Integer; AValue : boolean); 

begin
  If (FcommunityGuidelinesGoodStanding=AValue) then exit;
  FcommunityGuidelinesGoodStanding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelAuditDetails.SetcontentIdClaimsGoodStanding(AIndex : Integer; AValue : boolean); 

begin
  If (FcontentIdClaimsGoodStanding=AValue) then exit;
  FcontentIdClaimsGoodStanding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelAuditDetails.SetcopyrightStrikesGoodStanding(AIndex : Integer; AValue : boolean); 

begin
  If (FcopyrightStrikesGoodStanding=AValue) then exit;
  FcopyrightStrikesGoodStanding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelAuditDetails.SetoverallGoodStanding(AIndex : Integer; AValue : boolean); 

begin
  If (FoverallGoodStanding=AValue) then exit;
  FoverallGoodStanding:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelBannerResource
  --------------------------------------------------------------------}


Procedure TChannelBannerResource.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelBannerResource.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelBannerResource.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelBrandingSettings
  --------------------------------------------------------------------}


Procedure TChannelBrandingSettings.Setchannel(AIndex : Integer; AValue : TChannelSettings); 

begin
  If (Fchannel=AValue) then exit;
  Fchannel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelBrandingSettings.Sethints(AIndex : Integer; AValue : TChannelBrandingSettingsTypehintsArray); 

begin
  If (Fhints=AValue) then exit;
  Fhints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelBrandingSettings.Setimage(AIndex : Integer; AValue : TImageSettings); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelBrandingSettings.Setwatch(AIndex : Integer; AValue : TWatchSettings); 

begin
  If (Fwatch=AValue) then exit;
  Fwatch:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelContentDetailsTyperelatedPlaylists
  --------------------------------------------------------------------}


Procedure TChannelContentDetailsTyperelatedPlaylists.Setfavorites(AIndex : Integer; AValue : String); 

begin
  If (Ffavorites=AValue) then exit;
  Ffavorites:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelContentDetailsTyperelatedPlaylists.Setlikes(AIndex : Integer; AValue : String); 

begin
  If (Flikes=AValue) then exit;
  Flikes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelContentDetailsTyperelatedPlaylists.Setuploads(AIndex : Integer; AValue : String); 

begin
  If (Fuploads=AValue) then exit;
  Fuploads:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelContentDetailsTyperelatedPlaylists.SetwatchHistory(AIndex : Integer; AValue : String); 

begin
  If (FwatchHistory=AValue) then exit;
  FwatchHistory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelContentDetailsTyperelatedPlaylists.SetwatchLater(AIndex : Integer; AValue : String); 

begin
  If (FwatchLater=AValue) then exit;
  FwatchLater:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelContentDetails
  --------------------------------------------------------------------}


Procedure TChannelContentDetails.SetgooglePlusUserId(AIndex : Integer; AValue : String); 

begin
  If (FgooglePlusUserId=AValue) then exit;
  FgooglePlusUserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelContentDetails.SetrelatedPlaylists(AIndex : Integer; AValue : TChannelContentDetailsTyperelatedPlaylists); 

begin
  If (FrelatedPlaylists=AValue) then exit;
  FrelatedPlaylists:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelContentOwnerDetails
  --------------------------------------------------------------------}


Procedure TChannelContentOwnerDetails.SetcontentOwner(AIndex : Integer; AValue : String); 

begin
  If (FcontentOwner=AValue) then exit;
  FcontentOwner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelContentOwnerDetails.SettimeLinked(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeLinked=AValue) then exit;
  FtimeLinked:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelConversionPing
  --------------------------------------------------------------------}


Procedure TChannelConversionPing.Setcontext(AIndex : Integer; AValue : String); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelConversionPing.SetconversionUrl(AIndex : Integer; AValue : String); 

begin
  If (FconversionUrl=AValue) then exit;
  FconversionUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelConversionPings
  --------------------------------------------------------------------}


Procedure TChannelConversionPings.Setpings(AIndex : Integer; AValue : TChannelConversionPingsTypepingsArray); 

begin
  If (Fpings=AValue) then exit;
  Fpings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelId
  --------------------------------------------------------------------}


Procedure TChannelId.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelListResponse
  --------------------------------------------------------------------}


Procedure TChannelListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelListResponse.Setitems(AIndex : Integer; AValue : TChannelListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelLocalization
  --------------------------------------------------------------------}


Procedure TChannelLocalization.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelLocalization.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelSectionTypelocalizations
  --------------------------------------------------------------------}


Class Function TChannelSectionTypelocalizations.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TChannelSection
  --------------------------------------------------------------------}


Procedure TChannelSection.SetcontentDetails(AIndex : Integer; AValue : TChannelSectionContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSection.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSection.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSection.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSection.Setlocalizations(AIndex : Integer; AValue : TChannelSectionTypelocalizations); 

begin
  If (Flocalizations=AValue) then exit;
  Flocalizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSection.Setsnippet(AIndex : Integer; AValue : TChannelSectionSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSection.Settargeting(AIndex : Integer; AValue : TChannelSectionTargeting); 

begin
  If (Ftargeting=AValue) then exit;
  Ftargeting:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelSectionContentDetails
  --------------------------------------------------------------------}


Procedure TChannelSectionContentDetails.Setchannels(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fchannels=AValue) then exit;
  Fchannels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionContentDetails.Setplaylists(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fplaylists=AValue) then exit;
  Fplaylists:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelSectionListResponse
  --------------------------------------------------------------------}


Procedure TChannelSectionListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionListResponse.Setitems(AIndex : Integer; AValue : TChannelSectionListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelSectionLocalization
  --------------------------------------------------------------------}


Procedure TChannelSectionLocalization.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelSectionSnippet
  --------------------------------------------------------------------}


Procedure TChannelSectionSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionSnippet.SetdefaultLanguage(AIndex : Integer; AValue : String); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionSnippet.Setlocalized(AIndex : Integer; AValue : TChannelSectionLocalization); 

begin
  If (Flocalized=AValue) then exit;
  Flocalized:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionSnippet.Setposition(AIndex : Integer; AValue : integer); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionSnippet.Setstyle(AIndex : Integer; AValue : String); 

begin
  If (Fstyle=AValue) then exit;
  Fstyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionSnippet.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TChannelSectionSnippet.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TChannelSectionTargeting
  --------------------------------------------------------------------}


Procedure TChannelSectionTargeting.Setcountries(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fcountries=AValue) then exit;
  Fcountries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionTargeting.Setlanguages(AIndex : Integer; AValue : TStringArray); 

begin
  If (Flanguages=AValue) then exit;
  Flanguages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSectionTargeting.Setregions(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fregions=AValue) then exit;
  Fregions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelSettings
  --------------------------------------------------------------------}


Procedure TChannelSettings.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetdefaultLanguage(AIndex : Integer; AValue : String); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetdefaultTab(AIndex : Integer; AValue : String); 

begin
  If (FdefaultTab=AValue) then exit;
  FdefaultTab:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetfeaturedChannelsTitle(AIndex : Integer; AValue : String); 

begin
  If (FfeaturedChannelsTitle=AValue) then exit;
  FfeaturedChannelsTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetfeaturedChannelsUrls(AIndex : Integer; AValue : TStringArray); 

begin
  If (FfeaturedChannelsUrls=AValue) then exit;
  FfeaturedChannelsUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.Setkeywords(AIndex : Integer; AValue : String); 

begin
  If (Fkeywords=AValue) then exit;
  Fkeywords:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetmoderateComments(AIndex : Integer; AValue : boolean); 

begin
  If (FmoderateComments=AValue) then exit;
  FmoderateComments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetprofileColor(AIndex : Integer; AValue : String); 

begin
  If (FprofileColor=AValue) then exit;
  FprofileColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetshowBrowseView(AIndex : Integer; AValue : boolean); 

begin
  If (FshowBrowseView=AValue) then exit;
  FshowBrowseView:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetshowRelatedChannels(AIndex : Integer; AValue : boolean); 

begin
  If (FshowRelatedChannels=AValue) then exit;
  FshowRelatedChannels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SettrackingAnalyticsAccountId(AIndex : Integer; AValue : String); 

begin
  If (FtrackingAnalyticsAccountId=AValue) then exit;
  FtrackingAnalyticsAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSettings.SetunsubscribedTrailer(AIndex : Integer; AValue : String); 

begin
  If (FunsubscribedTrailer=AValue) then exit;
  FunsubscribedTrailer:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelSnippet
  --------------------------------------------------------------------}


Procedure TChannelSnippet.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSnippet.SetdefaultLanguage(AIndex : Integer; AValue : String); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSnippet.Setlocalized(AIndex : Integer; AValue : TChannelLocalization); 

begin
  If (Flocalized=AValue) then exit;
  Flocalized:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelStatistics
  --------------------------------------------------------------------}


Procedure TChannelStatistics.SetcommentCount(AIndex : Integer; AValue : String); 

begin
  If (FcommentCount=AValue) then exit;
  FcommentCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelStatistics.SethiddenSubscriberCount(AIndex : Integer; AValue : boolean); 

begin
  If (FhiddenSubscriberCount=AValue) then exit;
  FhiddenSubscriberCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelStatistics.SetsubscriberCount(AIndex : Integer; AValue : String); 

begin
  If (FsubscriberCount=AValue) then exit;
  FsubscriberCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelStatistics.SetvideoCount(AIndex : Integer; AValue : String); 

begin
  If (FvideoCount=AValue) then exit;
  FvideoCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelStatistics.SetviewCount(AIndex : Integer; AValue : String); 

begin
  If (FviewCount=AValue) then exit;
  FviewCount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelStatus
  --------------------------------------------------------------------}


Procedure TChannelStatus.SetisLinked(AIndex : Integer; AValue : boolean); 

begin
  If (FisLinked=AValue) then exit;
  FisLinked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelStatus.SetlongUploadsStatus(AIndex : Integer; AValue : String); 

begin
  If (FlongUploadsStatus=AValue) then exit;
  FlongUploadsStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannelStatus.SetprivacyStatus(AIndex : Integer; AValue : String); 

begin
  If (FprivacyStatus=AValue) then exit;
  FprivacyStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChannelTopicDetails
  --------------------------------------------------------------------}


Procedure TChannelTopicDetails.SettopicIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtopicIds=AValue) then exit;
  FtopicIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComment
  --------------------------------------------------------------------}


Procedure TComment.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setsnippet(AIndex : Integer; AValue : TCommentSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentListResponse
  --------------------------------------------------------------------}


Procedure TCommentListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentListResponse.Setitems(AIndex : Integer; AValue : TCommentListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentSnippet
  --------------------------------------------------------------------}


Procedure TCommentSnippet.SetauthorChannelId(AIndex : Integer; AValue : TChannelId); 

begin
  If (FauthorChannelId=AValue) then exit;
  FauthorChannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetauthorChannelUrl(AIndex : Integer; AValue : String); 

begin
  If (FauthorChannelUrl=AValue) then exit;
  FauthorChannelUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetauthorDisplayName(AIndex : Integer; AValue : String); 

begin
  If (FauthorDisplayName=AValue) then exit;
  FauthorDisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetauthorGoogleplusProfileUrl(AIndex : Integer; AValue : String); 

begin
  If (FauthorGoogleplusProfileUrl=AValue) then exit;
  FauthorGoogleplusProfileUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetauthorProfileImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FauthorProfileImageUrl=AValue) then exit;
  FauthorProfileImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetcanRate(AIndex : Integer; AValue : boolean); 

begin
  If (FcanRate=AValue) then exit;
  FcanRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetlikeCount(AIndex : Integer; AValue : integer); 

begin
  If (FlikeCount=AValue) then exit;
  FlikeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetmoderationStatus(AIndex : Integer; AValue : String); 

begin
  If (FmoderationStatus=AValue) then exit;
  FmoderationStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetparentId(AIndex : Integer; AValue : String); 

begin
  If (FparentId=AValue) then exit;
  FparentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SettextDisplay(AIndex : Integer; AValue : String); 

begin
  If (FtextDisplay=AValue) then exit;
  FtextDisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SettextOriginal(AIndex : Integer; AValue : String); 

begin
  If (FtextOriginal=AValue) then exit;
  FtextOriginal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetupdatedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FupdatedAt=AValue) then exit;
  FupdatedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentSnippet.SetviewerRating(AIndex : Integer; AValue : String); 

begin
  If (FviewerRating=AValue) then exit;
  FviewerRating:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentThread
  --------------------------------------------------------------------}


Procedure TCommentThread.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThread.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThread.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThread.Setreplies(AIndex : Integer; AValue : TCommentThreadReplies); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThread.Setsnippet(AIndex : Integer; AValue : TCommentThreadSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentThreadListResponse
  --------------------------------------------------------------------}


Procedure TCommentThreadListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadListResponse.Setitems(AIndex : Integer; AValue : TCommentThreadListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentThreadReplies
  --------------------------------------------------------------------}


Procedure TCommentThreadReplies.Setcomments(AIndex : Integer; AValue : TCommentThreadRepliesTypecommentsArray); 

begin
  If (Fcomments=AValue) then exit;
  Fcomments:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentThreadSnippet
  --------------------------------------------------------------------}


Procedure TCommentThreadSnippet.SetcanReply(AIndex : Integer; AValue : boolean); 

begin
  If (FcanReply=AValue) then exit;
  FcanReply:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadSnippet.SetisPublic(AIndex : Integer; AValue : boolean); 

begin
  If (FisPublic=AValue) then exit;
  FisPublic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadSnippet.SettopLevelComment(AIndex : Integer; AValue : TComment); 

begin
  If (FtopLevelComment=AValue) then exit;
  FtopLevelComment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadSnippet.SettotalReplyCount(AIndex : Integer; AValue : integer); 

begin
  If (FtotalReplyCount=AValue) then exit;
  FtotalReplyCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentThreadSnippet.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContentRating
  --------------------------------------------------------------------}


Procedure TContentRating.SetacbRating(AIndex : Integer; AValue : String); 

begin
  If (FacbRating=AValue) then exit;
  FacbRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetagcomRating(AIndex : Integer; AValue : String); 

begin
  If (FagcomRating=AValue) then exit;
  FagcomRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetanatelRating(AIndex : Integer; AValue : String); 

begin
  If (FanatelRating=AValue) then exit;
  FanatelRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetbbfcRating(AIndex : Integer; AValue : String); 

begin
  If (FbbfcRating=AValue) then exit;
  FbbfcRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetbfvcRating(AIndex : Integer; AValue : String); 

begin
  If (FbfvcRating=AValue) then exit;
  FbfvcRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetbmukkRating(AIndex : Integer; AValue : String); 

begin
  If (FbmukkRating=AValue) then exit;
  FbmukkRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcatvRating(AIndex : Integer; AValue : String); 

begin
  If (FcatvRating=AValue) then exit;
  FcatvRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcatvfrRating(AIndex : Integer; AValue : String); 

begin
  If (FcatvfrRating=AValue) then exit;
  FcatvfrRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcbfcRating(AIndex : Integer; AValue : String); 

begin
  If (FcbfcRating=AValue) then exit;
  FcbfcRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcccRating(AIndex : Integer; AValue : String); 

begin
  If (FcccRating=AValue) then exit;
  FcccRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcceRating(AIndex : Integer; AValue : String); 

begin
  If (FcceRating=AValue) then exit;
  FcceRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetchfilmRating(AIndex : Integer; AValue : String); 

begin
  If (FchfilmRating=AValue) then exit;
  FchfilmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetchvrsRating(AIndex : Integer; AValue : String); 

begin
  If (FchvrsRating=AValue) then exit;
  FchvrsRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcicfRating(AIndex : Integer; AValue : String); 

begin
  If (FcicfRating=AValue) then exit;
  FcicfRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcnaRating(AIndex : Integer; AValue : String); 

begin
  If (FcnaRating=AValue) then exit;
  FcnaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcsaRating(AIndex : Integer; AValue : String); 

begin
  If (FcsaRating=AValue) then exit;
  FcsaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetcscfRating(AIndex : Integer; AValue : String); 

begin
  If (FcscfRating=AValue) then exit;
  FcscfRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetczfilmRating(AIndex : Integer; AValue : String); 

begin
  If (FczfilmRating=AValue) then exit;
  FczfilmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetdjctqRating(AIndex : Integer; AValue : String); 

begin
  If (FdjctqRating=AValue) then exit;
  FdjctqRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetdjctqRatingReasons(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdjctqRatingReasons=AValue) then exit;
  FdjctqRatingReasons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SeteefilmRating(AIndex : Integer; AValue : String); 

begin
  If (FeefilmRating=AValue) then exit;
  FeefilmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetegfilmRating(AIndex : Integer; AValue : String); 

begin
  If (FegfilmRating=AValue) then exit;
  FegfilmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SeteirinRating(AIndex : Integer; AValue : String); 

begin
  If (FeirinRating=AValue) then exit;
  FeirinRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetfcbmRating(AIndex : Integer; AValue : String); 

begin
  If (FfcbmRating=AValue) then exit;
  FfcbmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetfcoRating(AIndex : Integer; AValue : String); 

begin
  If (FfcoRating=AValue) then exit;
  FfcoRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetfmocRating(AIndex : Integer; AValue : String); 

begin
  If (FfmocRating=AValue) then exit;
  FfmocRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetfpbRating(AIndex : Integer; AValue : String); 

begin
  If (FfpbRating=AValue) then exit;
  FfpbRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetfskRating(AIndex : Integer; AValue : String); 

begin
  If (FfskRating=AValue) then exit;
  FfskRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetgrfilmRating(AIndex : Integer; AValue : String); 

begin
  If (FgrfilmRating=AValue) then exit;
  FgrfilmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SeticaaRating(AIndex : Integer; AValue : String); 

begin
  If (FicaaRating=AValue) then exit;
  FicaaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetifcoRating(AIndex : Integer; AValue : String); 

begin
  If (FifcoRating=AValue) then exit;
  FifcoRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetilfilmRating(AIndex : Integer; AValue : String); 

begin
  If (FilfilmRating=AValue) then exit;
  FilfilmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetincaaRating(AIndex : Integer; AValue : String); 

begin
  If (FincaaRating=AValue) then exit;
  FincaaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetkfcbRating(AIndex : Integer; AValue : String); 

begin
  If (FkfcbRating=AValue) then exit;
  FkfcbRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetkijkwijzerRating(AIndex : Integer; AValue : String); 

begin
  If (FkijkwijzerRating=AValue) then exit;
  FkijkwijzerRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetkmrbRating(AIndex : Integer; AValue : String); 

begin
  If (FkmrbRating=AValue) then exit;
  FkmrbRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetlsfRating(AIndex : Integer; AValue : String); 

begin
  If (FlsfRating=AValue) then exit;
  FlsfRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmccaaRating(AIndex : Integer; AValue : String); 

begin
  If (FmccaaRating=AValue) then exit;
  FmccaaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmccypRating(AIndex : Integer; AValue : String); 

begin
  If (FmccypRating=AValue) then exit;
  FmccypRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmdaRating(AIndex : Integer; AValue : String); 

begin
  If (FmdaRating=AValue) then exit;
  FmdaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmedietilsynetRating(AIndex : Integer; AValue : String); 

begin
  If (FmedietilsynetRating=AValue) then exit;
  FmedietilsynetRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmekuRating(AIndex : Integer; AValue : String); 

begin
  If (FmekuRating=AValue) then exit;
  FmekuRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmibacRating(AIndex : Integer; AValue : String); 

begin
  If (FmibacRating=AValue) then exit;
  FmibacRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.Set_mocRating(AIndex : Integer; AValue : String); 

begin
  If (F_mocRating=AValue) then exit;
  F_mocRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmoctwRating(AIndex : Integer; AValue : String); 

begin
  If (FmoctwRating=AValue) then exit;
  FmoctwRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmpaaRating(AIndex : Integer; AValue : String); 

begin
  If (FmpaaRating=AValue) then exit;
  FmpaaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetmtrcbRating(AIndex : Integer; AValue : String); 

begin
  If (FmtrcbRating=AValue) then exit;
  FmtrcbRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetnbcRating(AIndex : Integer; AValue : String); 

begin
  If (FnbcRating=AValue) then exit;
  FnbcRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetnbcplRating(AIndex : Integer; AValue : String); 

begin
  If (FnbcplRating=AValue) then exit;
  FnbcplRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetnfrcRating(AIndex : Integer; AValue : String); 

begin
  If (FnfrcRating=AValue) then exit;
  FnfrcRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetnfvcbRating(AIndex : Integer; AValue : String); 

begin
  If (FnfvcbRating=AValue) then exit;
  FnfvcbRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetnkclvRating(AIndex : Integer; AValue : String); 

begin
  If (FnkclvRating=AValue) then exit;
  FnkclvRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetoflcRating(AIndex : Integer; AValue : String); 

begin
  If (FoflcRating=AValue) then exit;
  FoflcRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetpefilmRating(AIndex : Integer; AValue : String); 

begin
  If (FpefilmRating=AValue) then exit;
  FpefilmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetrcnofRating(AIndex : Integer; AValue : String); 

begin
  If (FrcnofRating=AValue) then exit;
  FrcnofRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetresorteviolenciaRating(AIndex : Integer; AValue : String); 

begin
  If (FresorteviolenciaRating=AValue) then exit;
  FresorteviolenciaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetrtcRating(AIndex : Integer; AValue : String); 

begin
  If (FrtcRating=AValue) then exit;
  FrtcRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetrteRating(AIndex : Integer; AValue : String); 

begin
  If (FrteRating=AValue) then exit;
  FrteRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetrussiaRating(AIndex : Integer; AValue : String); 

begin
  If (FrussiaRating=AValue) then exit;
  FrussiaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetskfilmRating(AIndex : Integer; AValue : String); 

begin
  If (FskfilmRating=AValue) then exit;
  FskfilmRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetsmaisRating(AIndex : Integer; AValue : String); 

begin
  If (FsmaisRating=AValue) then exit;
  FsmaisRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetsmsaRating(AIndex : Integer; AValue : String); 

begin
  If (FsmsaRating=AValue) then exit;
  FsmsaRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SettvpgRating(AIndex : Integer; AValue : String); 

begin
  If (FtvpgRating=AValue) then exit;
  FtvpgRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentRating.SetytRating(AIndex : Integer; AValue : String); 

begin
  If (FytRating=AValue) then exit;
  FytRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TContentRating.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_mocRating' : Result:='mocRating';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoPoint
  --------------------------------------------------------------------}


Procedure TGeoPoint.Setaltitude(AIndex : Integer; AValue : double); 

begin
  If (Faltitude=AValue) then exit;
  Faltitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoPoint.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoPoint.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGuideCategory
  --------------------------------------------------------------------}


Procedure TGuideCategory.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategory.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategory.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategory.Setsnippet(AIndex : Integer; AValue : TGuideCategorySnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGuideCategoryListResponse
  --------------------------------------------------------------------}


Procedure TGuideCategoryListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategoryListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategoryListResponse.Setitems(AIndex : Integer; AValue : TGuideCategoryListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategoryListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategoryListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategoryListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategoryListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategoryListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategoryListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGuideCategorySnippet
  --------------------------------------------------------------------}


Procedure TGuideCategorySnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGuideCategorySnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TI18nLanguage
  --------------------------------------------------------------------}


Procedure TI18nLanguage.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nLanguage.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nLanguage.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nLanguage.Setsnippet(AIndex : Integer; AValue : TI18nLanguageSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TI18nLanguageListResponse
  --------------------------------------------------------------------}


Procedure TI18nLanguageListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nLanguageListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nLanguageListResponse.Setitems(AIndex : Integer; AValue : TI18nLanguageListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nLanguageListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nLanguageListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TI18nLanguageSnippet
  --------------------------------------------------------------------}


Procedure TI18nLanguageSnippet.Sethl(AIndex : Integer; AValue : String); 

begin
  If (Fhl=AValue) then exit;
  Fhl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nLanguageSnippet.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TI18nRegion
  --------------------------------------------------------------------}


Procedure TI18nRegion.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nRegion.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nRegion.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nRegion.Setsnippet(AIndex : Integer; AValue : TI18nRegionSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TI18nRegionListResponse
  --------------------------------------------------------------------}


Procedure TI18nRegionListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nRegionListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nRegionListResponse.Setitems(AIndex : Integer; AValue : TI18nRegionListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nRegionListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nRegionListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TI18nRegionSnippet
  --------------------------------------------------------------------}


Procedure TI18nRegionSnippet.Setgl(AIndex : Integer; AValue : String); 

begin
  If (Fgl=AValue) then exit;
  Fgl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TI18nRegionSnippet.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImageSettings
  --------------------------------------------------------------------}


Procedure TImageSettings.SetbackgroundImageUrl(AIndex : Integer; AValue : TLocalizedProperty); 

begin
  If (FbackgroundImageUrl=AValue) then exit;
  FbackgroundImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerExternalUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerExternalUrl=AValue) then exit;
  FbannerExternalUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerImageUrl=AValue) then exit;
  FbannerImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerMobileExtraHdImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerMobileExtraHdImageUrl=AValue) then exit;
  FbannerMobileExtraHdImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerMobileHdImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerMobileHdImageUrl=AValue) then exit;
  FbannerMobileHdImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerMobileImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerMobileImageUrl=AValue) then exit;
  FbannerMobileImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerMobileLowImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerMobileLowImageUrl=AValue) then exit;
  FbannerMobileLowImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerMobileMediumHdImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerMobileMediumHdImageUrl=AValue) then exit;
  FbannerMobileMediumHdImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerTabletExtraHdImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerTabletExtraHdImageUrl=AValue) then exit;
  FbannerTabletExtraHdImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerTabletHdImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerTabletHdImageUrl=AValue) then exit;
  FbannerTabletHdImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerTabletImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerTabletImageUrl=AValue) then exit;
  FbannerTabletImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerTabletLowImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerTabletLowImageUrl=AValue) then exit;
  FbannerTabletLowImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerTvHighImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerTvHighImageUrl=AValue) then exit;
  FbannerTvHighImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerTvImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerTvImageUrl=AValue) then exit;
  FbannerTvImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerTvLowImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerTvLowImageUrl=AValue) then exit;
  FbannerTvLowImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetbannerTvMediumImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerTvMediumImageUrl=AValue) then exit;
  FbannerTvMediumImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetlargeBrandedBannerImageImapScript(AIndex : Integer; AValue : TLocalizedProperty); 

begin
  If (FlargeBrandedBannerImageImapScript=AValue) then exit;
  FlargeBrandedBannerImageImapScript:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetlargeBrandedBannerImageUrl(AIndex : Integer; AValue : TLocalizedProperty); 

begin
  If (FlargeBrandedBannerImageUrl=AValue) then exit;
  FlargeBrandedBannerImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetsmallBrandedBannerImageImapScript(AIndex : Integer; AValue : TLocalizedProperty); 

begin
  If (FsmallBrandedBannerImageImapScript=AValue) then exit;
  FsmallBrandedBannerImageImapScript:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetsmallBrandedBannerImageUrl(AIndex : Integer; AValue : TLocalizedProperty); 

begin
  If (FsmallBrandedBannerImageUrl=AValue) then exit;
  FsmallBrandedBannerImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SettrackingImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FtrackingImageUrl=AValue) then exit;
  FtrackingImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageSettings.SetwatchIconImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FwatchIconImageUrl=AValue) then exit;
  FwatchIconImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIngestionInfo
  --------------------------------------------------------------------}


Procedure TIngestionInfo.SetbackupIngestionAddress(AIndex : Integer; AValue : String); 

begin
  If (FbackupIngestionAddress=AValue) then exit;
  FbackupIngestionAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIngestionInfo.SetingestionAddress(AIndex : Integer; AValue : String); 

begin
  If (FingestionAddress=AValue) then exit;
  FingestionAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIngestionInfo.SetstreamName(AIndex : Integer; AValue : String); 

begin
  If (FstreamName=AValue) then exit;
  FstreamName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInvideoBranding
  --------------------------------------------------------------------}


Procedure TInvideoBranding.SetimageBytes(AIndex : Integer; AValue : String); 

begin
  If (FimageBytes=AValue) then exit;
  FimageBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoBranding.SetimageUrl(AIndex : Integer; AValue : String); 

begin
  If (FimageUrl=AValue) then exit;
  FimageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoBranding.Setposition(AIndex : Integer; AValue : TInvideoPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoBranding.SettargetChannelId(AIndex : Integer; AValue : String); 

begin
  If (FtargetChannelId=AValue) then exit;
  FtargetChannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoBranding.Settiming(AIndex : Integer; AValue : TInvideoTiming); 

begin
  If (Ftiming=AValue) then exit;
  Ftiming:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInvideoPosition
  --------------------------------------------------------------------}


Procedure TInvideoPosition.SetcornerPosition(AIndex : Integer; AValue : String); 

begin
  If (FcornerPosition=AValue) then exit;
  FcornerPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoPosition.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TInvideoPosition.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TInvideoPromotion
  --------------------------------------------------------------------}


Procedure TInvideoPromotion.SetdefaultTiming(AIndex : Integer; AValue : TInvideoTiming); 

begin
  If (FdefaultTiming=AValue) then exit;
  FdefaultTiming:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoPromotion.Setitems(AIndex : Integer; AValue : TInvideoPromotionTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoPromotion.Setposition(AIndex : Integer; AValue : TInvideoPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoPromotion.SetuseSmartTiming(AIndex : Integer; AValue : boolean); 

begin
  If (FuseSmartTiming=AValue) then exit;
  FuseSmartTiming:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInvideoTiming
  --------------------------------------------------------------------}


Procedure TInvideoTiming.SetdurationMs(AIndex : Integer; AValue : String); 

begin
  If (FdurationMs=AValue) then exit;
  FdurationMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoTiming.SetoffsetMs(AIndex : Integer; AValue : String); 

begin
  If (FoffsetMs=AValue) then exit;
  FoffsetMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvideoTiming.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TInvideoTiming.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TLanguageTag
  --------------------------------------------------------------------}


Procedure TLanguageTag.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveBroadcast
  --------------------------------------------------------------------}


Procedure TLiveBroadcast.SetcontentDetails(AIndex : Integer; AValue : TLiveBroadcastContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcast.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcast.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcast.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcast.Setsnippet(AIndex : Integer; AValue : TLiveBroadcastSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcast.Setstatus(AIndex : Integer; AValue : TLiveBroadcastStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveBroadcastContentDetails
  --------------------------------------------------------------------}


Procedure TLiveBroadcastContentDetails.SetboundStreamId(AIndex : Integer; AValue : String); 

begin
  If (FboundStreamId=AValue) then exit;
  FboundStreamId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastContentDetails.SetenableClosedCaptions(AIndex : Integer; AValue : boolean); 

begin
  If (FenableClosedCaptions=AValue) then exit;
  FenableClosedCaptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastContentDetails.SetenableContentEncryption(AIndex : Integer; AValue : boolean); 

begin
  If (FenableContentEncryption=AValue) then exit;
  FenableContentEncryption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastContentDetails.SetenableDvr(AIndex : Integer; AValue : boolean); 

begin
  If (FenableDvr=AValue) then exit;
  FenableDvr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastContentDetails.SetenableEmbed(AIndex : Integer; AValue : boolean); 

begin
  If (FenableEmbed=AValue) then exit;
  FenableEmbed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastContentDetails.SetmonitorStream(AIndex : Integer; AValue : TMonitorStreamInfo); 

begin
  If (FmonitorStream=AValue) then exit;
  FmonitorStream:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastContentDetails.SetrecordFromStart(AIndex : Integer; AValue : boolean); 

begin
  If (FrecordFromStart=AValue) then exit;
  FrecordFromStart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastContentDetails.SetstartWithSlate(AIndex : Integer; AValue : boolean); 

begin
  If (FstartWithSlate=AValue) then exit;
  FstartWithSlate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveBroadcastListResponse
  --------------------------------------------------------------------}


Procedure TLiveBroadcastListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastListResponse.Setitems(AIndex : Integer; AValue : TLiveBroadcastListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveBroadcastSnippet
  --------------------------------------------------------------------}


Procedure TLiveBroadcastSnippet.SetactualEndTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FactualEndTime=AValue) then exit;
  FactualEndTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastSnippet.SetactualStartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FactualStartTime=AValue) then exit;
  FactualStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastSnippet.SetscheduledEndTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FscheduledEndTime=AValue) then exit;
  FscheduledEndTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastSnippet.SetscheduledStartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FscheduledStartTime=AValue) then exit;
  FscheduledStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastSnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveBroadcastStatus
  --------------------------------------------------------------------}


Procedure TLiveBroadcastStatus.SetisDefaultBroadcast(AIndex : Integer; AValue : boolean); 

begin
  If (FisDefaultBroadcast=AValue) then exit;
  FisDefaultBroadcast:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastStatus.SetlifeCycleStatus(AIndex : Integer; AValue : String); 

begin
  If (FlifeCycleStatus=AValue) then exit;
  FlifeCycleStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastStatus.SetliveBroadcastPriority(AIndex : Integer; AValue : String); 

begin
  If (FliveBroadcastPriority=AValue) then exit;
  FliveBroadcastPriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastStatus.SetprivacyStatus(AIndex : Integer; AValue : String); 

begin
  If (FprivacyStatus=AValue) then exit;
  FprivacyStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveBroadcastStatus.SetrecordingStatus(AIndex : Integer; AValue : String); 

begin
  If (FrecordingStatus=AValue) then exit;
  FrecordingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveStream
  --------------------------------------------------------------------}


Procedure TLiveStream.Setcdn(AIndex : Integer; AValue : TCdnSettings); 

begin
  If (Fcdn=AValue) then exit;
  Fcdn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStream.SetcontentDetails(AIndex : Integer; AValue : TLiveStreamContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStream.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStream.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStream.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStream.Setsnippet(AIndex : Integer; AValue : TLiveStreamSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStream.Setstatus(AIndex : Integer; AValue : TLiveStreamStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveStreamContentDetails
  --------------------------------------------------------------------}


Procedure TLiveStreamContentDetails.SetclosedCaptionsIngestionUrl(AIndex : Integer; AValue : String); 

begin
  If (FclosedCaptionsIngestionUrl=AValue) then exit;
  FclosedCaptionsIngestionUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamContentDetails.SetisReusable(AIndex : Integer; AValue : boolean); 

begin
  If (FisReusable=AValue) then exit;
  FisReusable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveStreamListResponse
  --------------------------------------------------------------------}


Procedure TLiveStreamListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamListResponse.Setitems(AIndex : Integer; AValue : TLiveStreamListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveStreamSnippet
  --------------------------------------------------------------------}


Procedure TLiveStreamSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLiveStreamStatus
  --------------------------------------------------------------------}


Procedure TLiveStreamStatus.SetisDefaultStream(AIndex : Integer; AValue : boolean); 

begin
  If (FisDefaultStream=AValue) then exit;
  FisDefaultStream:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLiveStreamStatus.SetstreamStatus(AIndex : Integer; AValue : String); 

begin
  If (FstreamStatus=AValue) then exit;
  FstreamStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocalizedProperty
  --------------------------------------------------------------------}


Procedure TLocalizedProperty.Setdefault(AIndex : Integer; AValue : String); 

begin
  If (Fdefault=AValue) then exit;
  Fdefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedProperty.SetdefaultLanguage(AIndex : Integer; AValue : TLanguageTag); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedProperty.Setlocalized(AIndex : Integer; AValue : TLocalizedPropertyTypelocalizedArray); 

begin
  If (Flocalized=AValue) then exit;
  Flocalized:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocalizedString
  --------------------------------------------------------------------}


Procedure TLocalizedString.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedString.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMonitorStreamInfo
  --------------------------------------------------------------------}


Procedure TMonitorStreamInfo.SetbroadcastStreamDelayMs(AIndex : Integer; AValue : integer); 

begin
  If (FbroadcastStreamDelayMs=AValue) then exit;
  FbroadcastStreamDelayMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitorStreamInfo.SetembedHtml(AIndex : Integer; AValue : String); 

begin
  If (FembedHtml=AValue) then exit;
  FembedHtml:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitorStreamInfo.SetenableMonitorStream(AIndex : Integer; AValue : boolean); 

begin
  If (FenableMonitorStream=AValue) then exit;
  FenableMonitorStream:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageInfo
  --------------------------------------------------------------------}


Procedure TPageInfo.SetresultsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FresultsPerPage=AValue) then exit;
  FresultsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageInfo.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistTypelocalizations
  --------------------------------------------------------------------}


Class Function TPlaylistTypelocalizations.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPlaylist
  --------------------------------------------------------------------}


Procedure TPlaylist.SetcontentDetails(AIndex : Integer; AValue : TPlaylistContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylist.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylist.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylist.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylist.Setlocalizations(AIndex : Integer; AValue : TPlaylistTypelocalizations); 

begin
  If (Flocalizations=AValue) then exit;
  Flocalizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylist.Setplayer(AIndex : Integer; AValue : TPlaylistPlayer); 

begin
  If (Fplayer=AValue) then exit;
  Fplayer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylist.Setsnippet(AIndex : Integer; AValue : TPlaylistSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylist.Setstatus(AIndex : Integer; AValue : TPlaylistStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistContentDetails
  --------------------------------------------------------------------}


Procedure TPlaylistContentDetails.SetitemCount(AIndex : Integer; AValue : integer); 

begin
  If (FitemCount=AValue) then exit;
  FitemCount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistItem
  --------------------------------------------------------------------}


Procedure TPlaylistItem.SetcontentDetails(AIndex : Integer; AValue : TPlaylistItemContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItem.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItem.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItem.Setsnippet(AIndex : Integer; AValue : TPlaylistItemSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItem.Setstatus(AIndex : Integer; AValue : TPlaylistItemStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistItemContentDetails
  --------------------------------------------------------------------}


Procedure TPlaylistItemContentDetails.SetendAt(AIndex : Integer; AValue : String); 

begin
  If (FendAt=AValue) then exit;
  FendAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemContentDetails.Setnote(AIndex : Integer; AValue : String); 

begin
  If (Fnote=AValue) then exit;
  Fnote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemContentDetails.SetstartAt(AIndex : Integer; AValue : String); 

begin
  If (FstartAt=AValue) then exit;
  FstartAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemContentDetails.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistItemListResponse
  --------------------------------------------------------------------}


Procedure TPlaylistItemListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemListResponse.Setitems(AIndex : Integer; AValue : TPlaylistItemListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistItemSnippet
  --------------------------------------------------------------------}


Procedure TPlaylistItemSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemSnippet.SetchannelTitle(AIndex : Integer; AValue : String); 

begin
  If (FchannelTitle=AValue) then exit;
  FchannelTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemSnippet.SetplaylistId(AIndex : Integer; AValue : String); 

begin
  If (FplaylistId=AValue) then exit;
  FplaylistId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemSnippet.Setposition(AIndex : Integer; AValue : integer); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemSnippet.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemSnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistItemSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistItemStatus
  --------------------------------------------------------------------}


Procedure TPlaylistItemStatus.SetprivacyStatus(AIndex : Integer; AValue : String); 

begin
  If (FprivacyStatus=AValue) then exit;
  FprivacyStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistListResponse
  --------------------------------------------------------------------}


Procedure TPlaylistListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistListResponse.Setitems(AIndex : Integer; AValue : TPlaylistListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistLocalization
  --------------------------------------------------------------------}


Procedure TPlaylistLocalization.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistLocalization.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistPlayer
  --------------------------------------------------------------------}


Procedure TPlaylistPlayer.SetembedHtml(AIndex : Integer; AValue : String); 

begin
  If (FembedHtml=AValue) then exit;
  FembedHtml:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistSnippet
  --------------------------------------------------------------------}


Procedure TPlaylistSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistSnippet.SetchannelTitle(AIndex : Integer; AValue : String); 

begin
  If (FchannelTitle=AValue) then exit;
  FchannelTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistSnippet.SetdefaultLanguage(AIndex : Integer; AValue : String); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistSnippet.Setlocalized(AIndex : Integer; AValue : TPlaylistLocalization); 

begin
  If (Flocalized=AValue) then exit;
  Flocalized:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistSnippet.Settags(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistSnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaylistSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaylistStatus
  --------------------------------------------------------------------}


Procedure TPlaylistStatus.SetprivacyStatus(AIndex : Integer; AValue : String); 

begin
  If (FprivacyStatus=AValue) then exit;
  FprivacyStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPromotedItem
  --------------------------------------------------------------------}


Procedure TPromotedItem.SetcustomMessage(AIndex : Integer; AValue : String); 

begin
  If (FcustomMessage=AValue) then exit;
  FcustomMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotedItem.Setid(AIndex : Integer; AValue : TPromotedItemId); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotedItem.SetpromotedByContentOwner(AIndex : Integer; AValue : boolean); 

begin
  If (FpromotedByContentOwner=AValue) then exit;
  FpromotedByContentOwner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotedItem.Settiming(AIndex : Integer; AValue : TInvideoTiming); 

begin
  If (Ftiming=AValue) then exit;
  Ftiming:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPromotedItemId
  --------------------------------------------------------------------}


Procedure TPromotedItemId.SetrecentlyUploadedBy(AIndex : Integer; AValue : String); 

begin
  If (FrecentlyUploadedBy=AValue) then exit;
  FrecentlyUploadedBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotedItemId.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotedItemId.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotedItemId.SetwebsiteUrl(AIndex : Integer; AValue : String); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPromotedItemId.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPropertyValue
  --------------------------------------------------------------------}


Procedure TPropertyValue.Set_property(AIndex : Integer; AValue : String); 

begin
  If (F_property=AValue) then exit;
  F_property:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyValue.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPropertyValue.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_property' : Result:='property';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TResourceId
  --------------------------------------------------------------------}


Procedure TResourceId.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceId.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceId.SetplaylistId(AIndex : Integer; AValue : String); 

begin
  If (FplaylistId=AValue) then exit;
  FplaylistId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceId.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchListResponse
  --------------------------------------------------------------------}


Procedure TSearchListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchListResponse.Setitems(AIndex : Integer; AValue : TSearchListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchResult
  --------------------------------------------------------------------}


Procedure TSearchResult.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResult.Setid(AIndex : Integer; AValue : TResourceId); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResult.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResult.Setsnippet(AIndex : Integer; AValue : TSearchResultSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchResultSnippet
  --------------------------------------------------------------------}


Procedure TSearchResultSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResultSnippet.SetchannelTitle(AIndex : Integer; AValue : String); 

begin
  If (FchannelTitle=AValue) then exit;
  FchannelTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResultSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResultSnippet.SetliveBroadcastContent(AIndex : Integer; AValue : String); 

begin
  If (FliveBroadcastContent=AValue) then exit;
  FliveBroadcastContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResultSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResultSnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResultSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscription
  --------------------------------------------------------------------}


Procedure TSubscription.SetcontentDetails(AIndex : Integer; AValue : TSubscriptionContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setsnippet(AIndex : Integer; AValue : TSubscriptionSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetsubscriberSnippet(AIndex : Integer; AValue : TSubscriptionSubscriberSnippet); 

begin
  If (FsubscriberSnippet=AValue) then exit;
  FsubscriberSnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionContentDetails
  --------------------------------------------------------------------}


Procedure TSubscriptionContentDetails.SetactivityType(AIndex : Integer; AValue : String); 

begin
  If (FactivityType=AValue) then exit;
  FactivityType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionContentDetails.SetnewItemCount(AIndex : Integer; AValue : integer); 

begin
  If (FnewItemCount=AValue) then exit;
  FnewItemCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionContentDetails.SettotalItemCount(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItemCount=AValue) then exit;
  FtotalItemCount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionListResponse
  --------------------------------------------------------------------}


Procedure TSubscriptionListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionListResponse.Setitems(AIndex : Integer; AValue : TSubscriptionListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionSnippet
  --------------------------------------------------------------------}


Procedure TSubscriptionSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSnippet.SetchannelTitle(AIndex : Integer; AValue : String); 

begin
  If (FchannelTitle=AValue) then exit;
  FchannelTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSnippet.SetresourceId(AIndex : Integer; AValue : TResourceId); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionSubscriberSnippet
  --------------------------------------------------------------------}


Procedure TSubscriptionSubscriberSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSubscriberSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSubscriberSnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionSubscriberSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThumbnail
  --------------------------------------------------------------------}


Procedure TThumbnail.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnail.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnail.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThumbnailDetails
  --------------------------------------------------------------------}


Procedure TThumbnailDetails.Setdefault(AIndex : Integer; AValue : TThumbnail); 

begin
  If (Fdefault=AValue) then exit;
  Fdefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnailDetails.Sethigh(AIndex : Integer; AValue : TThumbnail); 

begin
  If (Fhigh=AValue) then exit;
  Fhigh:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnailDetails.Setmaxres(AIndex : Integer; AValue : TThumbnail); 

begin
  If (Fmaxres=AValue) then exit;
  Fmaxres:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnailDetails.Setmedium(AIndex : Integer; AValue : TThumbnail); 

begin
  If (Fmedium=AValue) then exit;
  Fmedium:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnailDetails.Setstandard(AIndex : Integer; AValue : TThumbnail); 

begin
  If (Fstandard=AValue) then exit;
  Fstandard:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThumbnailSetResponse
  --------------------------------------------------------------------}


Procedure TThumbnailSetResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnailSetResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnailSetResponse.Setitems(AIndex : Integer; AValue : TThumbnailSetResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnailSetResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnailSetResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTokenPagination
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVideoTypelocalizations
  --------------------------------------------------------------------}


Class Function TVideoTypelocalizations.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TVideo
  --------------------------------------------------------------------}


Procedure TVideo.SetageGating(AIndex : Integer; AValue : TVideoAgeGating); 

begin
  If (FageGating=AValue) then exit;
  FageGating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SetcontentDetails(AIndex : Integer; AValue : TVideoContentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SetconversionPings(AIndex : Integer; AValue : TVideoConversionPings); 

begin
  If (FconversionPings=AValue) then exit;
  FconversionPings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SetfileDetails(AIndex : Integer; AValue : TVideoFileDetails); 

begin
  If (FfileDetails=AValue) then exit;
  FfileDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SetliveStreamingDetails(AIndex : Integer; AValue : TVideoLiveStreamingDetails); 

begin
  If (FliveStreamingDetails=AValue) then exit;
  FliveStreamingDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setlocalizations(AIndex : Integer; AValue : TVideoTypelocalizations); 

begin
  If (Flocalizations=AValue) then exit;
  Flocalizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SetmonetizationDetails(AIndex : Integer; AValue : TVideoMonetizationDetails); 

begin
  If (FmonetizationDetails=AValue) then exit;
  FmonetizationDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setplayer(AIndex : Integer; AValue : TVideoPlayer); 

begin
  If (Fplayer=AValue) then exit;
  Fplayer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SetprocessingDetails(AIndex : Integer; AValue : TVideoProcessingDetails); 

begin
  If (FprocessingDetails=AValue) then exit;
  FprocessingDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SetprojectDetails(AIndex : Integer; AValue : TVideoProjectDetails); 

begin
  If (FprojectDetails=AValue) then exit;
  FprojectDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SetrecordingDetails(AIndex : Integer; AValue : TVideoRecordingDetails); 

begin
  If (FrecordingDetails=AValue) then exit;
  FrecordingDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setsnippet(AIndex : Integer; AValue : TVideoSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setstatistics(AIndex : Integer; AValue : TVideoStatistics); 

begin
  If (Fstatistics=AValue) then exit;
  Fstatistics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setstatus(AIndex : Integer; AValue : TVideoStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.Setsuggestions(AIndex : Integer; AValue : TVideoSuggestions); 

begin
  If (Fsuggestions=AValue) then exit;
  Fsuggestions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideo.SettopicDetails(AIndex : Integer; AValue : TVideoTopicDetails); 

begin
  If (FtopicDetails=AValue) then exit;
  FtopicDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoAbuseReport
  --------------------------------------------------------------------}


Procedure TVideoAbuseReport.Setcomments(AIndex : Integer; AValue : String); 

begin
  If (Fcomments=AValue) then exit;
  Fcomments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReport.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReport.SetreasonId(AIndex : Integer; AValue : String); 

begin
  If (FreasonId=AValue) then exit;
  FreasonId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReport.SetsecondaryReasonId(AIndex : Integer; AValue : String); 

begin
  If (FsecondaryReasonId=AValue) then exit;
  FsecondaryReasonId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReport.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoAbuseReportReason
  --------------------------------------------------------------------}


Procedure TVideoAbuseReportReason.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportReason.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportReason.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportReason.Setsnippet(AIndex : Integer; AValue : TVideoAbuseReportReasonSnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoAbuseReportReasonListResponse
  --------------------------------------------------------------------}


Procedure TVideoAbuseReportReasonListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportReasonListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportReasonListResponse.Setitems(AIndex : Integer; AValue : TVideoAbuseReportReasonListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportReasonListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportReasonListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoAbuseReportReasonSnippet
  --------------------------------------------------------------------}


Procedure TVideoAbuseReportReasonSnippet.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportReasonSnippet.SetsecondaryReasons(AIndex : Integer; AValue : TVideoAbuseReportReasonSnippetTypesecondaryReasonsArray); 

begin
  If (FsecondaryReasons=AValue) then exit;
  FsecondaryReasons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVideoAbuseReportReasonSnippet.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVideoAbuseReportSecondaryReason
  --------------------------------------------------------------------}


Procedure TVideoAbuseReportSecondaryReason.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAbuseReportSecondaryReason.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVideoAbuseReportSecondaryReason.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVideoAgeGating
  --------------------------------------------------------------------}


Procedure TVideoAgeGating.SetalcoholContent(AIndex : Integer; AValue : boolean); 

begin
  If (FalcoholContent=AValue) then exit;
  FalcoholContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAgeGating.Setrestricted(AIndex : Integer; AValue : boolean); 

begin
  If (Frestricted=AValue) then exit;
  Frestricted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoAgeGating.SetvideoGameRating(AIndex : Integer; AValue : String); 

begin
  If (FvideoGameRating=AValue) then exit;
  FvideoGameRating:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoCategory
  --------------------------------------------------------------------}


Procedure TVideoCategory.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategory.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategory.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategory.Setsnippet(AIndex : Integer; AValue : TVideoCategorySnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoCategoryListResponse
  --------------------------------------------------------------------}


Procedure TVideoCategoryListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategoryListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategoryListResponse.Setitems(AIndex : Integer; AValue : TVideoCategoryListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategoryListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategoryListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategoryListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategoryListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategoryListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategoryListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoCategorySnippet
  --------------------------------------------------------------------}


Procedure TVideoCategorySnippet.Setassignable(AIndex : Integer; AValue : boolean); 

begin
  If (Fassignable=AValue) then exit;
  Fassignable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategorySnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoCategorySnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoContentDetails
  --------------------------------------------------------------------}


Procedure TVideoContentDetails.Setcaption(AIndex : Integer; AValue : String); 

begin
  If (Fcaption=AValue) then exit;
  Fcaption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoContentDetails.SetcontentRating(AIndex : Integer; AValue : TContentRating); 

begin
  If (FcontentRating=AValue) then exit;
  FcontentRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoContentDetails.SetcountryRestriction(AIndex : Integer; AValue : TAccessPolicy); 

begin
  If (FcountryRestriction=AValue) then exit;
  FcountryRestriction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoContentDetails.Setdefinition(AIndex : Integer; AValue : String); 

begin
  If (Fdefinition=AValue) then exit;
  Fdefinition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoContentDetails.Setdimension(AIndex : Integer; AValue : String); 

begin
  If (Fdimension=AValue) then exit;
  Fdimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoContentDetails.Setduration(AIndex : Integer; AValue : String); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoContentDetails.SetlicensedContent(AIndex : Integer; AValue : boolean); 

begin
  If (FlicensedContent=AValue) then exit;
  FlicensedContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoContentDetails.SetregionRestriction(AIndex : Integer; AValue : TVideoContentDetailsRegionRestriction); 

begin
  If (FregionRestriction=AValue) then exit;
  FregionRestriction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoContentDetailsRegionRestriction
  --------------------------------------------------------------------}


Procedure TVideoContentDetailsRegionRestriction.Setallowed(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fallowed=AValue) then exit;
  Fallowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoContentDetailsRegionRestriction.Setblocked(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fblocked=AValue) then exit;
  Fblocked:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoConversionPing
  --------------------------------------------------------------------}


Procedure TVideoConversionPing.Setcontext(AIndex : Integer; AValue : String); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoConversionPing.SetconversionUrl(AIndex : Integer; AValue : String); 

begin
  If (FconversionUrl=AValue) then exit;
  FconversionUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoConversionPings
  --------------------------------------------------------------------}


Procedure TVideoConversionPings.Setpings(AIndex : Integer; AValue : TVideoConversionPingsTypepingsArray); 

begin
  If (Fpings=AValue) then exit;
  Fpings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoFileDetails
  --------------------------------------------------------------------}


Procedure TVideoFileDetails.SetaudioStreams(AIndex : Integer; AValue : TVideoFileDetailsTypeaudioStreamsArray); 

begin
  If (FaudioStreams=AValue) then exit;
  FaudioStreams:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.SetbitrateBps(AIndex : Integer; AValue : String); 

begin
  If (FbitrateBps=AValue) then exit;
  FbitrateBps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.Setcontainer(AIndex : Integer; AValue : String); 

begin
  If (Fcontainer=AValue) then exit;
  Fcontainer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.SetcreationTime(AIndex : Integer; AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.SetdurationMs(AIndex : Integer; AValue : String); 

begin
  If (FdurationMs=AValue) then exit;
  FdurationMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.SetfileName(AIndex : Integer; AValue : String); 

begin
  If (FfileName=AValue) then exit;
  FfileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.SetfileSize(AIndex : Integer; AValue : String); 

begin
  If (FfileSize=AValue) then exit;
  FfileSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.SetfileType(AIndex : Integer; AValue : String); 

begin
  If (FfileType=AValue) then exit;
  FfileType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.SetrecordingLocation(AIndex : Integer; AValue : TGeoPoint); 

begin
  If (FrecordingLocation=AValue) then exit;
  FrecordingLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetails.SetvideoStreams(AIndex : Integer; AValue : TVideoFileDetailsTypevideoStreamsArray); 

begin
  If (FvideoStreams=AValue) then exit;
  FvideoStreams:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoFileDetailsAudioStream
  --------------------------------------------------------------------}


Procedure TVideoFileDetailsAudioStream.SetbitrateBps(AIndex : Integer; AValue : String); 

begin
  If (FbitrateBps=AValue) then exit;
  FbitrateBps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsAudioStream.SetchannelCount(AIndex : Integer; AValue : integer); 

begin
  If (FchannelCount=AValue) then exit;
  FchannelCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsAudioStream.Setcodec(AIndex : Integer; AValue : String); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsAudioStream.Setvendor(AIndex : Integer; AValue : String); 

begin
  If (Fvendor=AValue) then exit;
  Fvendor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoFileDetailsVideoStream
  --------------------------------------------------------------------}


Procedure TVideoFileDetailsVideoStream.SetaspectRatio(AIndex : Integer; AValue : double); 

begin
  If (FaspectRatio=AValue) then exit;
  FaspectRatio:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsVideoStream.SetbitrateBps(AIndex : Integer; AValue : String); 

begin
  If (FbitrateBps=AValue) then exit;
  FbitrateBps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsVideoStream.Setcodec(AIndex : Integer; AValue : String); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsVideoStream.SetframeRateFps(AIndex : Integer; AValue : double); 

begin
  If (FframeRateFps=AValue) then exit;
  FframeRateFps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsVideoStream.SetheightPixels(AIndex : Integer; AValue : integer); 

begin
  If (FheightPixels=AValue) then exit;
  FheightPixels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsVideoStream.Setrotation(AIndex : Integer; AValue : String); 

begin
  If (Frotation=AValue) then exit;
  Frotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsVideoStream.Setvendor(AIndex : Integer; AValue : String); 

begin
  If (Fvendor=AValue) then exit;
  Fvendor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoFileDetailsVideoStream.SetwidthPixels(AIndex : Integer; AValue : integer); 

begin
  If (FwidthPixels=AValue) then exit;
  FwidthPixels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoGetRatingResponse
  --------------------------------------------------------------------}


Procedure TVideoGetRatingResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoGetRatingResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoGetRatingResponse.Setitems(AIndex : Integer; AValue : TVideoGetRatingResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoGetRatingResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoGetRatingResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoListResponse
  --------------------------------------------------------------------}


Procedure TVideoListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoListResponse.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoListResponse.Setitems(AIndex : Integer; AValue : TVideoListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoListResponse.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoListResponse.SetvisitorId(AIndex : Integer; AValue : String); 

begin
  If (FvisitorId=AValue) then exit;
  FvisitorId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoLiveStreamingDetails
  --------------------------------------------------------------------}


Procedure TVideoLiveStreamingDetails.SetactualEndTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FactualEndTime=AValue) then exit;
  FactualEndTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoLiveStreamingDetails.SetactualStartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FactualStartTime=AValue) then exit;
  FactualStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoLiveStreamingDetails.SetconcurrentViewers(AIndex : Integer; AValue : String); 

begin
  If (FconcurrentViewers=AValue) then exit;
  FconcurrentViewers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoLiveStreamingDetails.SetscheduledEndTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FscheduledEndTime=AValue) then exit;
  FscheduledEndTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoLiveStreamingDetails.SetscheduledStartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FscheduledStartTime=AValue) then exit;
  FscheduledStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoLocalization
  --------------------------------------------------------------------}


Procedure TVideoLocalization.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoLocalization.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoMonetizationDetails
  --------------------------------------------------------------------}


Procedure TVideoMonetizationDetails.Setaccess(AIndex : Integer; AValue : TAccessPolicy); 

begin
  If (Faccess=AValue) then exit;
  Faccess:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoPlayer
  --------------------------------------------------------------------}


Procedure TVideoPlayer.SetembedHtml(AIndex : Integer; AValue : String); 

begin
  If (FembedHtml=AValue) then exit;
  FembedHtml:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoProcessingDetails
  --------------------------------------------------------------------}


Procedure TVideoProcessingDetails.SeteditorSuggestionsAvailability(AIndex : Integer; AValue : String); 

begin
  If (FeditorSuggestionsAvailability=AValue) then exit;
  FeditorSuggestionsAvailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetails.SetfileDetailsAvailability(AIndex : Integer; AValue : String); 

begin
  If (FfileDetailsAvailability=AValue) then exit;
  FfileDetailsAvailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetails.SetprocessingFailureReason(AIndex : Integer; AValue : String); 

begin
  If (FprocessingFailureReason=AValue) then exit;
  FprocessingFailureReason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetails.SetprocessingIssuesAvailability(AIndex : Integer; AValue : String); 

begin
  If (FprocessingIssuesAvailability=AValue) then exit;
  FprocessingIssuesAvailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetails.SetprocessingProgress(AIndex : Integer; AValue : TVideoProcessingDetailsProcessingProgress); 

begin
  If (FprocessingProgress=AValue) then exit;
  FprocessingProgress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetails.SetprocessingStatus(AIndex : Integer; AValue : String); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetails.SettagSuggestionsAvailability(AIndex : Integer; AValue : String); 

begin
  If (FtagSuggestionsAvailability=AValue) then exit;
  FtagSuggestionsAvailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetails.SetthumbnailsAvailability(AIndex : Integer; AValue : String); 

begin
  If (FthumbnailsAvailability=AValue) then exit;
  FthumbnailsAvailability:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoProcessingDetailsProcessingProgress
  --------------------------------------------------------------------}


Procedure TVideoProcessingDetailsProcessingProgress.SetpartsProcessed(AIndex : Integer; AValue : String); 

begin
  If (FpartsProcessed=AValue) then exit;
  FpartsProcessed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetailsProcessingProgress.SetpartsTotal(AIndex : Integer; AValue : String); 

begin
  If (FpartsTotal=AValue) then exit;
  FpartsTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoProcessingDetailsProcessingProgress.SettimeLeftMs(AIndex : Integer; AValue : String); 

begin
  If (FtimeLeftMs=AValue) then exit;
  FtimeLeftMs:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoProjectDetails
  --------------------------------------------------------------------}


Procedure TVideoProjectDetails.Settags(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoRating
  --------------------------------------------------------------------}


Procedure TVideoRating.Setrating(AIndex : Integer; AValue : String); 

begin
  If (Frating=AValue) then exit;
  Frating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoRating.SetvideoId(AIndex : Integer; AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoRecordingDetails
  --------------------------------------------------------------------}


Procedure TVideoRecordingDetails.Setlocation(AIndex : Integer; AValue : TGeoPoint); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoRecordingDetails.SetlocationDescription(AIndex : Integer; AValue : String); 

begin
  If (FlocationDescription=AValue) then exit;
  FlocationDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoRecordingDetails.SetrecordingDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FrecordingDate=AValue) then exit;
  FrecordingDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoSnippet
  --------------------------------------------------------------------}


Procedure TVideoSnippet.SetcategoryId(AIndex : Integer; AValue : String); 

begin
  If (FcategoryId=AValue) then exit;
  FcategoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.SetchannelTitle(AIndex : Integer; AValue : String); 

begin
  If (FchannelTitle=AValue) then exit;
  FchannelTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.SetdefaultLanguage(AIndex : Integer; AValue : String); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.SetliveBroadcastContent(AIndex : Integer; AValue : String); 

begin
  If (FliveBroadcastContent=AValue) then exit;
  FliveBroadcastContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.Setlocalized(AIndex : Integer; AValue : TVideoLocalization); 

begin
  If (Flocalized=AValue) then exit;
  Flocalized:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.Settags(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.Setthumbnails(AIndex : Integer; AValue : TThumbnailDetails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoStatistics
  --------------------------------------------------------------------}


Procedure TVideoStatistics.SetcommentCount(AIndex : Integer; AValue : String); 

begin
  If (FcommentCount=AValue) then exit;
  FcommentCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatistics.SetdislikeCount(AIndex : Integer; AValue : String); 

begin
  If (FdislikeCount=AValue) then exit;
  FdislikeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatistics.SetfavoriteCount(AIndex : Integer; AValue : String); 

begin
  If (FfavoriteCount=AValue) then exit;
  FfavoriteCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatistics.SetlikeCount(AIndex : Integer; AValue : String); 

begin
  If (FlikeCount=AValue) then exit;
  FlikeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatistics.SetviewCount(AIndex : Integer; AValue : String); 

begin
  If (FviewCount=AValue) then exit;
  FviewCount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoStatus
  --------------------------------------------------------------------}


Procedure TVideoStatus.Setembeddable(AIndex : Integer; AValue : boolean); 

begin
  If (Fembeddable=AValue) then exit;
  Fembeddable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatus.SetfailureReason(AIndex : Integer; AValue : String); 

begin
  If (FfailureReason=AValue) then exit;
  FfailureReason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatus.Setlicense(AIndex : Integer; AValue : String); 

begin
  If (Flicense=AValue) then exit;
  Flicense:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatus.SetprivacyStatus(AIndex : Integer; AValue : String); 

begin
  If (FprivacyStatus=AValue) then exit;
  FprivacyStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatus.SetpublicStatsViewable(AIndex : Integer; AValue : boolean); 

begin
  If (FpublicStatsViewable=AValue) then exit;
  FpublicStatsViewable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatus.SetpublishAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishAt=AValue) then exit;
  FpublishAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatus.SetrejectionReason(AIndex : Integer; AValue : String); 

begin
  If (FrejectionReason=AValue) then exit;
  FrejectionReason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoStatus.SetuploadStatus(AIndex : Integer; AValue : String); 

begin
  If (FuploadStatus=AValue) then exit;
  FuploadStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoSuggestions
  --------------------------------------------------------------------}


Procedure TVideoSuggestions.SeteditorSuggestions(AIndex : Integer; AValue : TStringArray); 

begin
  If (FeditorSuggestions=AValue) then exit;
  FeditorSuggestions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSuggestions.SetprocessingErrors(AIndex : Integer; AValue : TStringArray); 

begin
  If (FprocessingErrors=AValue) then exit;
  FprocessingErrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSuggestions.SetprocessingHints(AIndex : Integer; AValue : TStringArray); 

begin
  If (FprocessingHints=AValue) then exit;
  FprocessingHints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSuggestions.SetprocessingWarnings(AIndex : Integer; AValue : TStringArray); 

begin
  If (FprocessingWarnings=AValue) then exit;
  FprocessingWarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSuggestions.SettagSuggestions(AIndex : Integer; AValue : TVideoSuggestionsTypetagSuggestionsArray); 

begin
  If (FtagSuggestions=AValue) then exit;
  FtagSuggestions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoSuggestionsTagSuggestion
  --------------------------------------------------------------------}


Procedure TVideoSuggestionsTagSuggestion.SetcategoryRestricts(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcategoryRestricts=AValue) then exit;
  FcategoryRestricts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoSuggestionsTagSuggestion.Settag(AIndex : Integer; AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVideoTopicDetails
  --------------------------------------------------------------------}


Procedure TVideoTopicDetails.SetrelevantTopicIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FrelevantTopicIds=AValue) then exit;
  FrelevantTopicIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideoTopicDetails.SettopicIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtopicIds=AValue) then exit;
  FtopicIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWatchSettings
  --------------------------------------------------------------------}


Procedure TWatchSettings.SetbackgroundColor(AIndex : Integer; AValue : String); 

begin
  If (FbackgroundColor=AValue) then exit;
  FbackgroundColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWatchSettings.SetfeaturedPlaylistId(AIndex : Integer; AValue : String); 

begin
  If (FfeaturedPlaylistId=AValue) then exit;
  FfeaturedPlaylistId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWatchSettings.SettextColor(AIndex : Integer; AValue : String); 

begin
  If (FtextColor=AValue) then exit;
  FtextColor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivitiesResource
  --------------------------------------------------------------------}


Class Function TActivitiesResource.ResourceName : String;

begin
  Result:='activities';
end;

Class Function TActivitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TActivitiesResource.Insert(aActivity : TActivity; AQuery : string = '') : TActivity;

Const
  _HTTPMethod = 'POST';
  _Path       = 'activities';
  _Methodid   = 'youtube.activities.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aActivity,TActivity) as TActivity;
end;


Function TActivitiesResource.Insert(aActivity : TActivity; AQuery : TActivitiesinsertOptions) : TActivity;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Insert(aActivity,_Q);
end;

Function TActivitiesResource.List(AQuery : string = '') : TActivityListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activities';
  _Methodid   = 'youtube.activities.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TActivityListResponse) as TActivityListResponse;
end;


Function TActivitiesResource.List(AQuery : TActivitieslistOptions) : TActivityListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'channelId',AQuery.channelId);
  AddToQuery(_Q,'home',AQuery.home);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'mine',AQuery.mine);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'publishedAfter',AQuery.publishedAfter);
  AddToQuery(_Q,'publishedBefore',AQuery.publishedBefore);
  AddToQuery(_Q,'regionCode',AQuery.regionCode);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TCaptionsResource
  --------------------------------------------------------------------}


Class Function TCaptionsResource.ResourceName : String;

begin
  Result:='captions';
end;

Class Function TCaptionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TCaptionsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'captions';
  _Methodid   = 'youtube.captions.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TCaptionsResource.Delete(AQuery : TCaptionsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'debugProjectIdOverride',AQuery.debugProjectIdOverride);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOf',AQuery.onBehalfOf);
  Delete(_Q);
end;

Procedure TCaptionsResource.Download(id: string; AQuery : string = '');

Const
  _HTTPMethod = 'GET';
  _Path       = 'captions/{id}';
  _Methodid   = 'youtube.captions.download';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TCaptionsResource.Download(id: string; AQuery : TCaptionsdownloadOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'debugProjectIdOverride',AQuery.debugProjectIdOverride);
  AddToQuery(_Q,'onBehalfOf',AQuery.onBehalfOf);
  AddToQuery(_Q,'tfmt',AQuery.tfmt);
  AddToQuery(_Q,'tlang',AQuery.tlang);
  Download(id,_Q);
end;

Function TCaptionsResource.Insert(aCaption : TCaption; AQuery : string = '') : TCaption;

Const
  _HTTPMethod = 'POST';
  _Path       = 'captions';
  _Methodid   = 'youtube.captions.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aCaption,TCaption) as TCaption;
end;


Function TCaptionsResource.Insert(aCaption : TCaption; AQuery : TCaptionsinsertOptions) : TCaption;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'debugProjectIdOverride',AQuery.debugProjectIdOverride);
  AddToQuery(_Q,'onBehalfOf',AQuery.onBehalfOf);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'sync',AQuery.sync);
  Result:=Insert(aCaption,_Q);
end;

Function TCaptionsResource.List(AQuery : string = '') : TCaptionListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'captions';
  _Methodid   = 'youtube.captions.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TCaptionListResponse) as TCaptionListResponse;
end;


Function TCaptionsResource.List(AQuery : TCaptionslistOptions) : TCaptionListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'debugProjectIdOverride',AQuery.debugProjectIdOverride);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOf',AQuery.onBehalfOf);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'videoId',AQuery.videoId);
  Result:=List(_Q);
end;

Function TCaptionsResource.Update(aCaption : TCaption; AQuery : string = '') : TCaption;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'captions';
  _Methodid   = 'youtube.captions.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aCaption,TCaption) as TCaption;
end;


Function TCaptionsResource.Update(aCaption : TCaption; AQuery : TCaptionsupdateOptions) : TCaption;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'debugProjectIdOverride',AQuery.debugProjectIdOverride);
  AddToQuery(_Q,'onBehalfOf',AQuery.onBehalfOf);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'sync',AQuery.sync);
  Result:=Update(aCaption,_Q);
end;



{ --------------------------------------------------------------------
  TChannelBannersResource
  --------------------------------------------------------------------}


Class Function TChannelBannersResource.ResourceName : String;

begin
  Result:='channelBanners';
end;

Class Function TChannelBannersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TChannelBannersResource.Insert(aChannelBannerResource : TChannelBannerResource; AQuery : string = '') : TChannelBannerResource;

Const
  _HTTPMethod = 'POST';
  _Path       = 'channelBanners/insert';
  _Methodid   = 'youtube.channelBanners.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aChannelBannerResource,TChannelBannerResource) as TChannelBannerResource;
end;


Function TChannelBannersResource.Insert(aChannelBannerResource : TChannelBannerResource; AQuery : TChannelBannersinsertOptions) : TChannelBannerResource;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=Insert(aChannelBannerResource,_Q);
end;



{ --------------------------------------------------------------------
  TChannelSectionsResource
  --------------------------------------------------------------------}


Class Function TChannelSectionsResource.ResourceName : String;

begin
  Result:='channelSections';
end;

Class Function TChannelSectionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TChannelSectionsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'channelSections';
  _Methodid   = 'youtube.channelSections.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TChannelSectionsResource.Delete(AQuery : TChannelSectionsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Delete(_Q);
end;

Function TChannelSectionsResource.Insert(aChannelSection : TChannelSection; AQuery : string = '') : TChannelSection;

Const
  _HTTPMethod = 'POST';
  _Path       = 'channelSections';
  _Methodid   = 'youtube.channelSections.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aChannelSection,TChannelSection) as TChannelSection;
end;


Function TChannelSectionsResource.Insert(aChannelSection : TChannelSection; AQuery : TChannelSectionsinsertOptions) : TChannelSection;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Insert(aChannelSection,_Q);
end;

Function TChannelSectionsResource.List(AQuery : string = '') : TChannelSectionListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'channelSections';
  _Methodid   = 'youtube.channelSections.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TChannelSectionListResponse) as TChannelSectionListResponse;
end;


Function TChannelSectionsResource.List(AQuery : TChannelSectionslistOptions) : TChannelSectionListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'channelId',AQuery.channelId);
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'mine',AQuery.mine);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;

Function TChannelSectionsResource.Update(aChannelSection : TChannelSection; AQuery : string = '') : TChannelSection;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'channelSections';
  _Methodid   = 'youtube.channelSections.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aChannelSection,TChannelSection) as TChannelSection;
end;


Function TChannelSectionsResource.Update(aChannelSection : TChannelSection; AQuery : TChannelSectionsupdateOptions) : TChannelSection;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aChannelSection,_Q);
end;



{ --------------------------------------------------------------------
  TChannelsResource
  --------------------------------------------------------------------}


Class Function TChannelsResource.ResourceName : String;

begin
  Result:='channels';
end;

Class Function TChannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TChannelsResource.List(AQuery : string = '') : TChannelListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'channels';
  _Methodid   = 'youtube.channels.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TChannelListResponse) as TChannelListResponse;
end;


Function TChannelsResource.List(AQuery : TChannelslistOptions) : TChannelListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'categoryId',AQuery.categoryId);
  AddToQuery(_Q,'forUsername',AQuery.forUsername);
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'managedByMe',AQuery.managedByMe);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'mine',AQuery.mine);
  AddToQuery(_Q,'mySubscribers',AQuery.mySubscribers);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;

Function TChannelsResource.Update(aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'channels';
  _Methodid   = 'youtube.channels.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aChannel,TChannel) as TChannel;
end;


Function TChannelsResource.Update(aChannel : TChannel; AQuery : TChannelsupdateOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aChannel,_Q);
end;



{ --------------------------------------------------------------------
  TCommentThreadsResource
  --------------------------------------------------------------------}


Class Function TCommentThreadsResource.ResourceName : String;

begin
  Result:='commentThreads';
end;

Class Function TCommentThreadsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TCommentThreadsResource.Insert(aCommentThread : TCommentThread; AQuery : string = '') : TCommentThread;

Const
  _HTTPMethod = 'POST';
  _Path       = 'commentThreads';
  _Methodid   = 'youtube.commentThreads.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aCommentThread,TCommentThread) as TCommentThread;
end;


Function TCommentThreadsResource.Insert(aCommentThread : TCommentThread; AQuery : TCommentThreadsinsertOptions) : TCommentThread;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'shareOnGooglePlus',AQuery.shareOnGooglePlus);
  Result:=Insert(aCommentThread,_Q);
end;

Function TCommentThreadsResource.List(AQuery : string = '') : TCommentThreadListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'commentThreads';
  _Methodid   = 'youtube.commentThreads.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TCommentThreadListResponse) as TCommentThreadListResponse;
end;


Function TCommentThreadsResource.List(AQuery : TCommentThreadslistOptions) : TCommentThreadListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'allThreadsRelatedToChannelId',AQuery.allThreadsRelatedToChannelId);
  AddToQuery(_Q,'channelId',AQuery.channelId);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'moderationStatus',AQuery.moderationStatus);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'searchTerms',AQuery.searchTerms);
  AddToQuery(_Q,'textFormat',AQuery.textFormat);
  AddToQuery(_Q,'videoId',AQuery.videoId);
  Result:=List(_Q);
end;

Function TCommentThreadsResource.Update(aCommentThread : TCommentThread; AQuery : string = '') : TCommentThread;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'commentThreads';
  _Methodid   = 'youtube.commentThreads.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aCommentThread,TCommentThread) as TCommentThread;
end;


Function TCommentThreadsResource.Update(aCommentThread : TCommentThread; AQuery : TCommentThreadsupdateOptions) : TCommentThread;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aCommentThread,_Q);
end;



{ --------------------------------------------------------------------
  TCommentsResource
  --------------------------------------------------------------------}


Class Function TCommentsResource.ResourceName : String;

begin
  Result:='comments';
end;

Class Function TCommentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TCommentsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'comments';
  _Methodid   = 'youtube.comments.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TCommentsResource.Delete(AQuery : TCommentsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Delete(_Q);
end;

Function TCommentsResource.Insert(aComment : TComment; AQuery : string = '') : TComment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'comments';
  _Methodid   = 'youtube.comments.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aComment,TComment) as TComment;
end;


Function TCommentsResource.Insert(aComment : TComment; AQuery : TCommentsinsertOptions) : TComment;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Insert(aComment,_Q);
end;

Function TCommentsResource.List(AQuery : string = '') : TCommentListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'comments';
  _Methodid   = 'youtube.comments.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TCommentListResponse) as TCommentListResponse;
end;


Function TCommentsResource.List(AQuery : TCommentslistOptions) : TCommentListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'parentId',AQuery.parentId);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'textFormat',AQuery.textFormat);
  Result:=List(_Q);
end;

Procedure TCommentsResource.MarkAsSpam(AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'comments/markAsSpam';
  _Methodid   = 'youtube.comments.markAsSpam';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TCommentsResource.MarkAsSpam(AQuery : TCommentsmarkAsSpamOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  MarkAsSpam(_Q);
end;

Procedure TCommentsResource.SetModerationStatus(AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'comments/setModerationStatus';
  _Methodid   = 'youtube.comments.setModerationStatus';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TCommentsResource.SetModerationStatus(AQuery : TCommentssetModerationStatusOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'banAuthor',AQuery.banAuthor);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'moderationStatus',AQuery.moderationStatus);
  SetModerationStatus(_Q);
end;

Function TCommentsResource.Update(aComment : TComment; AQuery : string = '') : TComment;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'comments';
  _Methodid   = 'youtube.comments.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aComment,TComment) as TComment;
end;


Function TCommentsResource.Update(aComment : TComment; AQuery : TCommentsupdateOptions) : TComment;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aComment,_Q);
end;



{ --------------------------------------------------------------------
  TGuideCategoriesResource
  --------------------------------------------------------------------}


Class Function TGuideCategoriesResource.ResourceName : String;

begin
  Result:='guideCategories';
end;

Class Function TGuideCategoriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TGuideCategoriesResource.List(AQuery : string = '') : TGuideCategoryListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'guideCategories';
  _Methodid   = 'youtube.guideCategories.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGuideCategoryListResponse) as TGuideCategoryListResponse;
end;


Function TGuideCategoriesResource.List(AQuery : TGuideCategorieslistOptions) : TGuideCategoryListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'regionCode',AQuery.regionCode);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TI18nLanguagesResource
  --------------------------------------------------------------------}


Class Function TI18nLanguagesResource.ResourceName : String;

begin
  Result:='i18nLanguages';
end;

Class Function TI18nLanguagesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TI18nLanguagesResource.List(AQuery : string = '') : TI18nLanguageListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'i18nLanguages';
  _Methodid   = 'youtube.i18nLanguages.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TI18nLanguageListResponse) as TI18nLanguageListResponse;
end;


Function TI18nLanguagesResource.List(AQuery : TI18nLanguageslistOptions) : TI18nLanguageListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TI18nRegionsResource
  --------------------------------------------------------------------}


Class Function TI18nRegionsResource.ResourceName : String;

begin
  Result:='i18nRegions';
end;

Class Function TI18nRegionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TI18nRegionsResource.List(AQuery : string = '') : TI18nRegionListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'i18nRegions';
  _Methodid   = 'youtube.i18nRegions.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TI18nRegionListResponse) as TI18nRegionListResponse;
end;


Function TI18nRegionsResource.List(AQuery : TI18nRegionslistOptions) : TI18nRegionListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TLiveBroadcastsResource
  --------------------------------------------------------------------}


Class Function TLiveBroadcastsResource.ResourceName : String;

begin
  Result:='liveBroadcasts';
end;

Class Function TLiveBroadcastsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TLiveBroadcastsResource.Bind(AQuery : string = '') : TLiveBroadcast;

Const
  _HTTPMethod = 'POST';
  _Path       = 'liveBroadcasts/bind';
  _Methodid   = 'youtube.liveBroadcasts.bind';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TLiveBroadcast) as TLiveBroadcast;
end;


Function TLiveBroadcastsResource.Bind(AQuery : TLiveBroadcastsbindOptions) : TLiveBroadcast;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'streamId',AQuery.streamId);
  Result:=Bind(_Q);
end;

Function TLiveBroadcastsResource.Control(AQuery : string = '') : TLiveBroadcast;

Const
  _HTTPMethod = 'POST';
  _Path       = 'liveBroadcasts/control';
  _Methodid   = 'youtube.liveBroadcasts.control';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TLiveBroadcast) as TLiveBroadcast;
end;


Function TLiveBroadcastsResource.Control(AQuery : TLiveBroadcastscontrolOptions) : TLiveBroadcast;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'displaySlate',AQuery.displaySlate);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'offsetTimeMs',AQuery.offsetTimeMs);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'walltime',AQuery.walltime);
  Result:=Control(_Q);
end;

Procedure TLiveBroadcastsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'liveBroadcasts';
  _Methodid   = 'youtube.liveBroadcasts.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TLiveBroadcastsResource.Delete(AQuery : TLiveBroadcastsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  Delete(_Q);
end;

Function TLiveBroadcastsResource.Insert(aLiveBroadcast : TLiveBroadcast; AQuery : string = '') : TLiveBroadcast;

Const
  _HTTPMethod = 'POST';
  _Path       = 'liveBroadcasts';
  _Methodid   = 'youtube.liveBroadcasts.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aLiveBroadcast,TLiveBroadcast) as TLiveBroadcast;
end;


Function TLiveBroadcastsResource.Insert(aLiveBroadcast : TLiveBroadcast; AQuery : TLiveBroadcastsinsertOptions) : TLiveBroadcast;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Insert(aLiveBroadcast,_Q);
end;

Function TLiveBroadcastsResource.List(AQuery : string = '') : TLiveBroadcastListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'liveBroadcasts';
  _Methodid   = 'youtube.liveBroadcasts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TLiveBroadcastListResponse) as TLiveBroadcastListResponse;
end;


Function TLiveBroadcastsResource.List(AQuery : TLiveBroadcastslistOptions) : TLiveBroadcastListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'broadcastStatus',AQuery.broadcastStatus);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'mine',AQuery.mine);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;

Function TLiveBroadcastsResource.Transition(AQuery : string = '') : TLiveBroadcast;

Const
  _HTTPMethod = 'POST';
  _Path       = 'liveBroadcasts/transition';
  _Methodid   = 'youtube.liveBroadcasts.transition';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TLiveBroadcast) as TLiveBroadcast;
end;


Function TLiveBroadcastsResource.Transition(AQuery : TLiveBroadcaststransitionOptions) : TLiveBroadcast;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'broadcastStatus',AQuery.broadcastStatus);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Transition(_Q);
end;

Function TLiveBroadcastsResource.Update(aLiveBroadcast : TLiveBroadcast; AQuery : string = '') : TLiveBroadcast;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'liveBroadcasts';
  _Methodid   = 'youtube.liveBroadcasts.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aLiveBroadcast,TLiveBroadcast) as TLiveBroadcast;
end;


Function TLiveBroadcastsResource.Update(aLiveBroadcast : TLiveBroadcast; AQuery : TLiveBroadcastsupdateOptions) : TLiveBroadcast;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aLiveBroadcast,_Q);
end;



{ --------------------------------------------------------------------
  TLiveStreamsResource
  --------------------------------------------------------------------}


Class Function TLiveStreamsResource.ResourceName : String;

begin
  Result:='liveStreams';
end;

Class Function TLiveStreamsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TLiveStreamsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'liveStreams';
  _Methodid   = 'youtube.liveStreams.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TLiveStreamsResource.Delete(AQuery : TLiveStreamsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  Delete(_Q);
end;

Function TLiveStreamsResource.Insert(aLiveStream : TLiveStream; AQuery : string = '') : TLiveStream;

Const
  _HTTPMethod = 'POST';
  _Path       = 'liveStreams';
  _Methodid   = 'youtube.liveStreams.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aLiveStream,TLiveStream) as TLiveStream;
end;


Function TLiveStreamsResource.Insert(aLiveStream : TLiveStream; AQuery : TLiveStreamsinsertOptions) : TLiveStream;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Insert(aLiveStream,_Q);
end;

Function TLiveStreamsResource.List(AQuery : string = '') : TLiveStreamListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'liveStreams';
  _Methodid   = 'youtube.liveStreams.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TLiveStreamListResponse) as TLiveStreamListResponse;
end;


Function TLiveStreamsResource.List(AQuery : TLiveStreamslistOptions) : TLiveStreamListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'mine',AQuery.mine);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;

Function TLiveStreamsResource.Update(aLiveStream : TLiveStream; AQuery : string = '') : TLiveStream;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'liveStreams';
  _Methodid   = 'youtube.liveStreams.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aLiveStream,TLiveStream) as TLiveStream;
end;


Function TLiveStreamsResource.Update(aLiveStream : TLiveStream; AQuery : TLiveStreamsupdateOptions) : TLiveStream;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aLiveStream,_Q);
end;



{ --------------------------------------------------------------------
  TPlaylistItemsResource
  --------------------------------------------------------------------}


Class Function TPlaylistItemsResource.ResourceName : String;

begin
  Result:='playlistItems';
end;

Class Function TPlaylistItemsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TPlaylistItemsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'playlistItems';
  _Methodid   = 'youtube.playlistItems.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TPlaylistItemsResource.Delete(AQuery : TPlaylistItemsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Delete(_Q);
end;

Function TPlaylistItemsResource.Insert(aPlaylistItem : TPlaylistItem; AQuery : string = '') : TPlaylistItem;

Const
  _HTTPMethod = 'POST';
  _Path       = 'playlistItems';
  _Methodid   = 'youtube.playlistItems.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aPlaylistItem,TPlaylistItem) as TPlaylistItem;
end;


Function TPlaylistItemsResource.Insert(aPlaylistItem : TPlaylistItem; AQuery : TPlaylistItemsinsertOptions) : TPlaylistItem;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Insert(aPlaylistItem,_Q);
end;

Function TPlaylistItemsResource.List(AQuery : string = '') : TPlaylistItemListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'playlistItems';
  _Methodid   = 'youtube.playlistItems.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TPlaylistItemListResponse) as TPlaylistItemListResponse;
end;


Function TPlaylistItemsResource.List(AQuery : TPlaylistItemslistOptions) : TPlaylistItemListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'playlistId',AQuery.playlistId);
  AddToQuery(_Q,'videoId',AQuery.videoId);
  Result:=List(_Q);
end;

Function TPlaylistItemsResource.Update(aPlaylistItem : TPlaylistItem; AQuery : string = '') : TPlaylistItem;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'playlistItems';
  _Methodid   = 'youtube.playlistItems.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aPlaylistItem,TPlaylistItem) as TPlaylistItem;
end;


Function TPlaylistItemsResource.Update(aPlaylistItem : TPlaylistItem; AQuery : TPlaylistItemsupdateOptions) : TPlaylistItem;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aPlaylistItem,_Q);
end;



{ --------------------------------------------------------------------
  TPlaylistsResource
  --------------------------------------------------------------------}


Class Function TPlaylistsResource.ResourceName : String;

begin
  Result:='playlists';
end;

Class Function TPlaylistsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TPlaylistsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'playlists';
  _Methodid   = 'youtube.playlists.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TPlaylistsResource.Delete(AQuery : TPlaylistsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Delete(_Q);
end;

Function TPlaylistsResource.Insert(aPlaylist : TPlaylist; AQuery : string = '') : TPlaylist;

Const
  _HTTPMethod = 'POST';
  _Path       = 'playlists';
  _Methodid   = 'youtube.playlists.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aPlaylist,TPlaylist) as TPlaylist;
end;


Function TPlaylistsResource.Insert(aPlaylist : TPlaylist; AQuery : TPlaylistsinsertOptions) : TPlaylist;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Insert(aPlaylist,_Q);
end;

Function TPlaylistsResource.List(AQuery : string = '') : TPlaylistListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'playlists';
  _Methodid   = 'youtube.playlists.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TPlaylistListResponse) as TPlaylistListResponse;
end;


Function TPlaylistsResource.List(AQuery : TPlaylistslistOptions) : TPlaylistListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'channelId',AQuery.channelId);
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'mine',AQuery.mine);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;

Function TPlaylistsResource.Update(aPlaylist : TPlaylist; AQuery : string = '') : TPlaylist;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'playlists';
  _Methodid   = 'youtube.playlists.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aPlaylist,TPlaylist) as TPlaylist;
end;


Function TPlaylistsResource.Update(aPlaylist : TPlaylist; AQuery : TPlaylistsupdateOptions) : TPlaylist;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aPlaylist,_Q);
end;



{ --------------------------------------------------------------------
  TSearchResource
  --------------------------------------------------------------------}


Class Function TSearchResource.ResourceName : String;

begin
  Result:='search';
end;

Class Function TSearchResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TSearchResource.List(AQuery : string = '') : TSearchListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'search';
  _Methodid   = 'youtube.search.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSearchListResponse) as TSearchListResponse;
end;


Function TSearchResource.List(AQuery : TSearchlistOptions) : TSearchListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'channelId',AQuery.channelId);
  AddToQuery(_Q,'channelType',AQuery.channelType);
  AddToQuery(_Q,'eventType',AQuery.eventType);
  AddToQuery(_Q,'forContentOwner',AQuery.forContentOwner);
  AddToQuery(_Q,'forDeveloper',AQuery.forDeveloper);
  AddToQuery(_Q,'forMine',AQuery.forMine);
  AddToQuery(_Q,'location',AQuery.location);
  AddToQuery(_Q,'locationRadius',AQuery.locationRadius);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'order',AQuery.order);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'publishedAfter',AQuery.publishedAfter);
  AddToQuery(_Q,'publishedBefore',AQuery.publishedBefore);
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'regionCode',AQuery.regionCode);
  AddToQuery(_Q,'relatedToVideoId',AQuery.relatedToVideoId);
  AddToQuery(_Q,'relevanceLanguage',AQuery.relevanceLanguage);
  AddToQuery(_Q,'safeSearch',AQuery.safeSearch);
  AddToQuery(_Q,'topicId',AQuery.topicId);
  AddToQuery(_Q,'type',AQuery._type);
  AddToQuery(_Q,'videoCaption',AQuery.videoCaption);
  AddToQuery(_Q,'videoCategoryId',AQuery.videoCategoryId);
  AddToQuery(_Q,'videoDefinition',AQuery.videoDefinition);
  AddToQuery(_Q,'videoDimension',AQuery.videoDimension);
  AddToQuery(_Q,'videoDuration',AQuery.videoDuration);
  AddToQuery(_Q,'videoEmbeddable',AQuery.videoEmbeddable);
  AddToQuery(_Q,'videoLicense',AQuery.videoLicense);
  AddToQuery(_Q,'videoSyndicated',AQuery.videoSyndicated);
  AddToQuery(_Q,'videoType',AQuery.videoType);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TSubscriptionsResource
  --------------------------------------------------------------------}


Class Function TSubscriptionsResource.ResourceName : String;

begin
  Result:='subscriptions';
end;

Class Function TSubscriptionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TSubscriptionsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'subscriptions';
  _Methodid   = 'youtube.subscriptions.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TSubscriptionsResource.Delete(AQuery : TSubscriptionsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Delete(_Q);
end;

Function TSubscriptionsResource.Insert(aSubscription : TSubscription; AQuery : string = '') : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'subscriptions';
  _Methodid   = 'youtube.subscriptions.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aSubscription,TSubscription) as TSubscription;
end;


Function TSubscriptionsResource.Insert(aSubscription : TSubscription; AQuery : TSubscriptionsinsertOptions) : TSubscription;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Insert(aSubscription,_Q);
end;

Function TSubscriptionsResource.List(AQuery : string = '') : TSubscriptionListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'subscriptions';
  _Methodid   = 'youtube.subscriptions.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSubscriptionListResponse) as TSubscriptionListResponse;
end;


Function TSubscriptionsResource.List(AQuery : TSubscriptionslistOptions) : TSubscriptionListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'channelId',AQuery.channelId);
  AddToQuery(_Q,'forChannelId',AQuery.forChannelId);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'mine',AQuery.mine);
  AddToQuery(_Q,'mySubscribers',AQuery.mySubscribers);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'order',AQuery.order);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TThumbnailsResource
  --------------------------------------------------------------------}


Class Function TThumbnailsResource.ResourceName : String;

begin
  Result:='thumbnails';
end;

Class Function TThumbnailsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TThumbnailsResource._set(AQuery : string = '') : TThumbnailSetResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'thumbnails/set';
  _Methodid   = 'youtube.thumbnails.set';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TThumbnailSetResponse) as TThumbnailSetResponse;
end;


Function TThumbnailsResource._set(AQuery : TThumbnailssetOptions) : TThumbnailSetResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'videoId',AQuery.videoId);
  Result:=_set(_Q);
end;



{ --------------------------------------------------------------------
  TVideoAbuseReportReasonsResource
  --------------------------------------------------------------------}


Class Function TVideoAbuseReportReasonsResource.ResourceName : String;

begin
  Result:='videoAbuseReportReasons';
end;

Class Function TVideoAbuseReportReasonsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TVideoAbuseReportReasonsResource.List(AQuery : string = '') : TVideoAbuseReportReasonListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'videoAbuseReportReasons';
  _Methodid   = 'youtube.videoAbuseReportReasons.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVideoAbuseReportReasonListResponse) as TVideoAbuseReportReasonListResponse;
end;


Function TVideoAbuseReportReasonsResource.List(AQuery : TVideoAbuseReportReasonslistOptions) : TVideoAbuseReportReasonListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TVideoCategoriesResource
  --------------------------------------------------------------------}


Class Function TVideoCategoriesResource.ResourceName : String;

begin
  Result:='videoCategories';
end;

Class Function TVideoCategoriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Function TVideoCategoriesResource.List(AQuery : string = '') : TVideoCategoryListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'videoCategories';
  _Methodid   = 'youtube.videoCategories.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVideoCategoryListResponse) as TVideoCategoryListResponse;
end;


Function TVideoCategoriesResource.List(AQuery : TVideoCategorieslistOptions) : TVideoCategoryListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'regionCode',AQuery.regionCode);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TVideosResource
  --------------------------------------------------------------------}


Class Function TVideosResource.ResourceName : String;

begin
  Result:='videos';
end;

Class Function TVideosResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TVideosResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'videos';
  _Methodid   = 'youtube.videos.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TVideosResource.Delete(AQuery : TVideosdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Delete(_Q);
end;

Function TVideosResource.GetRating(AQuery : string = '') : TVideoGetRatingResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'videos/getRating';
  _Methodid   = 'youtube.videos.getRating';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVideoGetRatingResponse) as TVideoGetRatingResponse;
end;


Function TVideosResource.GetRating(AQuery : TVideosgetRatingOptions) : TVideoGetRatingResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=GetRating(_Q);
end;

Function TVideosResource.Insert(aVideo : TVideo; AQuery : string = '') : TVideo;

Const
  _HTTPMethod = 'POST';
  _Path       = 'videos';
  _Methodid   = 'youtube.videos.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aVideo,TVideo) as TVideo;
end;


Function TVideosResource.Insert(aVideo : TVideo; AQuery : TVideosinsertOptions) : TVideo;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'autoLevels',AQuery.autoLevels);
  AddToQuery(_Q,'notifySubscribers',AQuery.notifySubscribers);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'onBehalfOfContentOwnerChannel',AQuery.onBehalfOfContentOwnerChannel);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'stabilize',AQuery.stabilize);
  Result:=Insert(aVideo,_Q);
end;

Function TVideosResource.List(AQuery : string = '') : TVideoListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'videos';
  _Methodid   = 'youtube.videos.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVideoListResponse) as TVideoListResponse;
end;


Function TVideosResource.List(AQuery : TVideoslistOptions) : TVideoListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'chart',AQuery.chart);
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'myRating',AQuery.myRating);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'part',AQuery.part);
  AddToQuery(_Q,'regionCode',AQuery.regionCode);
  AddToQuery(_Q,'videoCategoryId',AQuery.videoCategoryId);
  Result:=List(_Q);
end;

Procedure TVideosResource.Rate(AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'videos/rate';
  _Methodid   = 'youtube.videos.rate';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TVideosResource.Rate(AQuery : TVideosrateOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'rating',AQuery.rating);
  Rate(_Q);
end;

Procedure TVideosResource.ReportAbuse(aVideoAbuseReport : TVideoAbuseReport; AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'videos/reportAbuse';
  _Methodid   = 'youtube.videos.reportAbuse';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,aVideoAbuseReport,Nil);
end;


Procedure TVideosResource.ReportAbuse(aVideoAbuseReport : TVideoAbuseReport; AQuery : TVideosreportAbuseOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  ReportAbuse(aVideoAbuseReport,_Q);
end;

Function TVideosResource.Update(aVideo : TVideo; AQuery : string = '') : TVideo;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'videos';
  _Methodid   = 'youtube.videos.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aVideo,TVideo) as TVideo;
end;


Function TVideosResource.Update(aVideo : TVideo; AQuery : TVideosupdateOptions) : TVideo;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'part',AQuery.part);
  Result:=Update(aVideo,_Q);
end;



{ --------------------------------------------------------------------
  TWatermarksResource
  --------------------------------------------------------------------}


Class Function TWatermarksResource.ResourceName : String;

begin
  Result:='watermarks';
end;

Class Function TWatermarksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAPI;
end;

Procedure TWatermarksResource._set(aInvideoBranding : TInvideoBranding; AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'watermarks/set';
  _Methodid   = 'youtube.watermarks.set';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,aInvideoBranding,Nil);
end;


Procedure TWatermarksResource._set(aInvideoBranding : TInvideoBranding; AQuery : TWatermarkssetOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'channelId',AQuery.channelId);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  _set(aInvideoBranding,_Q);
end;

Procedure TWatermarksResource.Unset(AQuery : string = '');

Const
  _HTTPMethod = 'POST';
  _Path       = 'watermarks/unset';
  _Methodid   = 'youtube.watermarks.unset';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TWatermarksResource.Unset(AQuery : TWatermarksunsetOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'channelId',AQuery.channelId);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Unset(_Q);
end;



{ --------------------------------------------------------------------
  TYoutubeAPI
  --------------------------------------------------------------------}

Class Function TYoutubeAPI.APIName : String;

begin
  Result:='youtube';
end;

Class Function TYoutubeAPI.APIVersion : String;

begin
  Result:='v3';
end;

Class Function TYoutubeAPI.APIRevision : String;

begin
  Result:='20150412';
end;

Class Function TYoutubeAPI.APIID : String;

begin
  Result:='youtube:v3';
end;

Class Function TYoutubeAPI.APITitle : String;

begin
  Result:='YouTube Data API';
end;

Class Function TYoutubeAPI.APIDescription : String;

begin
  Result:='Programmatic access to YouTube features.';
end;

Class Function TYoutubeAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TYoutubeAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TYoutubeAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/youtube-16.png';
end;

Class Function TYoutubeAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/youtube-32.png';
end;

Class Function TYoutubeAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/youtube/v3';
end;

Class Function TYoutubeAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TYoutubeAPI.APIbasePath : string;

begin
  Result:='/youtube/v3/';
end;

Class Function TYoutubeAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/youtube/v3/';
end;

Class Function TYoutubeAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TYoutubeAPI.APIservicePath : string;

begin
  Result:='youtube/v3/';
end;

Class Function TYoutubeAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TYoutubeAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,6);
  Result[0].Name:='https://www.googleapis.com/auth/youtube';
  Result[0].Description:='Manage your YouTube account';
  Result[1].Name:='https://www.googleapis.com/auth/youtube.force-ssl';
  Result[1].Description:='Manage your YouTube account';
  Result[2].Name:='https://www.googleapis.com/auth/youtube.readonly';
  Result[2].Description:='View your YouTube account';
  Result[3].Name:='https://www.googleapis.com/auth/youtube.upload';
  Result[3].Description:='Manage your YouTube videos';
  Result[4].Name:='https://www.googleapis.com/auth/youtubepartner';
  Result[4].Description:='View and manage your assets and associated content on YouTube';
  Result[5].Name:='https://www.googleapis.com/auth/youtubepartner-channel-audit';
  Result[5].Description:='View private information of your YouTube channel relevant during the audit process with a YouTube partner';
  
end;

Class Function TYoutubeAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TYoutubeAPI.RegisterAPIResources;

begin
  TAccessPolicy.RegisterObject;
  TActivity.RegisterObject;
  TActivityContentDetails.RegisterObject;
  TActivityContentDetailsBulletin.RegisterObject;
  TActivityContentDetailsChannelItem.RegisterObject;
  TActivityContentDetailsComment.RegisterObject;
  TActivityContentDetailsFavorite.RegisterObject;
  TActivityContentDetailsLike.RegisterObject;
  TActivityContentDetailsPlaylistItem.RegisterObject;
  TActivityContentDetailsPromotedItem.RegisterObject;
  TActivityContentDetailsRecommendation.RegisterObject;
  TActivityContentDetailsSocial.RegisterObject;
  TActivityContentDetailsSubscription.RegisterObject;
  TActivityContentDetailsUpload.RegisterObject;
  TActivityListResponse.RegisterObject;
  TActivitySnippet.RegisterObject;
  TCaption.RegisterObject;
  TCaptionListResponse.RegisterObject;
  TCaptionSnippet.RegisterObject;
  TCdnSettings.RegisterObject;
  TChannelTypelocalizations.RegisterObject;
  TChannel.RegisterObject;
  TChannelAuditDetails.RegisterObject;
  TChannelBannerResource.RegisterObject;
  TChannelBrandingSettings.RegisterObject;
  TChannelContentDetailsTyperelatedPlaylists.RegisterObject;
  TChannelContentDetails.RegisterObject;
  TChannelContentOwnerDetails.RegisterObject;
  TChannelConversionPing.RegisterObject;
  TChannelConversionPings.RegisterObject;
  TChannelId.RegisterObject;
  TChannelListResponse.RegisterObject;
  TChannelLocalization.RegisterObject;
  TChannelSectionTypelocalizations.RegisterObject;
  TChannelSection.RegisterObject;
  TChannelSectionContentDetails.RegisterObject;
  TChannelSectionListResponse.RegisterObject;
  TChannelSectionLocalization.RegisterObject;
  TChannelSectionSnippet.RegisterObject;
  TChannelSectionTargeting.RegisterObject;
  TChannelSettings.RegisterObject;
  TChannelSnippet.RegisterObject;
  TChannelStatistics.RegisterObject;
  TChannelStatus.RegisterObject;
  TChannelTopicDetails.RegisterObject;
  TComment.RegisterObject;
  TCommentListResponse.RegisterObject;
  TCommentSnippet.RegisterObject;
  TCommentThread.RegisterObject;
  TCommentThreadListResponse.RegisterObject;
  TCommentThreadReplies.RegisterObject;
  TCommentThreadSnippet.RegisterObject;
  TContentRating.RegisterObject;
  TGeoPoint.RegisterObject;
  TGuideCategory.RegisterObject;
  TGuideCategoryListResponse.RegisterObject;
  TGuideCategorySnippet.RegisterObject;
  TI18nLanguage.RegisterObject;
  TI18nLanguageListResponse.RegisterObject;
  TI18nLanguageSnippet.RegisterObject;
  TI18nRegion.RegisterObject;
  TI18nRegionListResponse.RegisterObject;
  TI18nRegionSnippet.RegisterObject;
  TImageSettings.RegisterObject;
  TIngestionInfo.RegisterObject;
  TInvideoBranding.RegisterObject;
  TInvideoPosition.RegisterObject;
  TInvideoPromotion.RegisterObject;
  TInvideoTiming.RegisterObject;
  TLanguageTag.RegisterObject;
  TLiveBroadcast.RegisterObject;
  TLiveBroadcastContentDetails.RegisterObject;
  TLiveBroadcastListResponse.RegisterObject;
  TLiveBroadcastSnippet.RegisterObject;
  TLiveBroadcastStatus.RegisterObject;
  TLiveStream.RegisterObject;
  TLiveStreamContentDetails.RegisterObject;
  TLiveStreamListResponse.RegisterObject;
  TLiveStreamSnippet.RegisterObject;
  TLiveStreamStatus.RegisterObject;
  TLocalizedProperty.RegisterObject;
  TLocalizedString.RegisterObject;
  TMonitorStreamInfo.RegisterObject;
  TPageInfo.RegisterObject;
  TPlaylistTypelocalizations.RegisterObject;
  TPlaylist.RegisterObject;
  TPlaylistContentDetails.RegisterObject;
  TPlaylistItem.RegisterObject;
  TPlaylistItemContentDetails.RegisterObject;
  TPlaylistItemListResponse.RegisterObject;
  TPlaylistItemSnippet.RegisterObject;
  TPlaylistItemStatus.RegisterObject;
  TPlaylistListResponse.RegisterObject;
  TPlaylistLocalization.RegisterObject;
  TPlaylistPlayer.RegisterObject;
  TPlaylistSnippet.RegisterObject;
  TPlaylistStatus.RegisterObject;
  TPromotedItem.RegisterObject;
  TPromotedItemId.RegisterObject;
  TPropertyValue.RegisterObject;
  TResourceId.RegisterObject;
  TSearchListResponse.RegisterObject;
  TSearchResult.RegisterObject;
  TSearchResultSnippet.RegisterObject;
  TSubscription.RegisterObject;
  TSubscriptionContentDetails.RegisterObject;
  TSubscriptionListResponse.RegisterObject;
  TSubscriptionSnippet.RegisterObject;
  TSubscriptionSubscriberSnippet.RegisterObject;
  TThumbnail.RegisterObject;
  TThumbnailDetails.RegisterObject;
  TThumbnailSetResponse.RegisterObject;
  TTokenPagination.RegisterObject;
  TVideoTypelocalizations.RegisterObject;
  TVideo.RegisterObject;
  TVideoAbuseReport.RegisterObject;
  TVideoAbuseReportReason.RegisterObject;
  TVideoAbuseReportReasonListResponse.RegisterObject;
  TVideoAbuseReportReasonSnippet.RegisterObject;
  TVideoAbuseReportSecondaryReason.RegisterObject;
  TVideoAgeGating.RegisterObject;
  TVideoCategory.RegisterObject;
  TVideoCategoryListResponse.RegisterObject;
  TVideoCategorySnippet.RegisterObject;
  TVideoContentDetails.RegisterObject;
  TVideoContentDetailsRegionRestriction.RegisterObject;
  TVideoConversionPing.RegisterObject;
  TVideoConversionPings.RegisterObject;
  TVideoFileDetails.RegisterObject;
  TVideoFileDetailsAudioStream.RegisterObject;
  TVideoFileDetailsVideoStream.RegisterObject;
  TVideoGetRatingResponse.RegisterObject;
  TVideoListResponse.RegisterObject;
  TVideoLiveStreamingDetails.RegisterObject;
  TVideoLocalization.RegisterObject;
  TVideoMonetizationDetails.RegisterObject;
  TVideoPlayer.RegisterObject;
  TVideoProcessingDetails.RegisterObject;
  TVideoProcessingDetailsProcessingProgress.RegisterObject;
  TVideoProjectDetails.RegisterObject;
  TVideoRating.RegisterObject;
  TVideoRecordingDetails.RegisterObject;
  TVideoSnippet.RegisterObject;
  TVideoStatistics.RegisterObject;
  TVideoStatus.RegisterObject;
  TVideoSuggestions.RegisterObject;
  TVideoSuggestionsTagSuggestion.RegisterObject;
  TVideoTopicDetails.RegisterObject;
  TWatchSettings.RegisterObject;
end;


Function TYoutubeAPI.GetActivitiesInstance : TActivitiesResource;

begin
  if (FActivitiesInstance=Nil) then
    FActivitiesInstance:=CreateActivitiesResource;
  Result:=FActivitiesInstance;
end;

Function TYoutubeAPI.CreateActivitiesResource : TActivitiesResource;

begin
  Result:=CreateActivitiesResource(Self);
end;


Function TYoutubeAPI.CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;

begin
  Result:=TActivitiesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetCaptionsInstance : TCaptionsResource;

begin
  if (FCaptionsInstance=Nil) then
    FCaptionsInstance:=CreateCaptionsResource;
  Result:=FCaptionsInstance;
end;

Function TYoutubeAPI.CreateCaptionsResource : TCaptionsResource;

begin
  Result:=CreateCaptionsResource(Self);
end;


Function TYoutubeAPI.CreateCaptionsResource(AOwner : TComponent) : TCaptionsResource;

begin
  Result:=TCaptionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetChannelBannersInstance : TChannelBannersResource;

begin
  if (FChannelBannersInstance=Nil) then
    FChannelBannersInstance:=CreateChannelBannersResource;
  Result:=FChannelBannersInstance;
end;

Function TYoutubeAPI.CreateChannelBannersResource : TChannelBannersResource;

begin
  Result:=CreateChannelBannersResource(Self);
end;


Function TYoutubeAPI.CreateChannelBannersResource(AOwner : TComponent) : TChannelBannersResource;

begin
  Result:=TChannelBannersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetChannelSectionsInstance : TChannelSectionsResource;

begin
  if (FChannelSectionsInstance=Nil) then
    FChannelSectionsInstance:=CreateChannelSectionsResource;
  Result:=FChannelSectionsInstance;
end;

Function TYoutubeAPI.CreateChannelSectionsResource : TChannelSectionsResource;

begin
  Result:=CreateChannelSectionsResource(Self);
end;


Function TYoutubeAPI.CreateChannelSectionsResource(AOwner : TComponent) : TChannelSectionsResource;

begin
  Result:=TChannelSectionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetChannelsInstance : TChannelsResource;

begin
  if (FChannelsInstance=Nil) then
    FChannelsInstance:=CreateChannelsResource;
  Result:=FChannelsInstance;
end;

Function TYoutubeAPI.CreateChannelsResource : TChannelsResource;

begin
  Result:=CreateChannelsResource(Self);
end;


Function TYoutubeAPI.CreateChannelsResource(AOwner : TComponent) : TChannelsResource;

begin
  Result:=TChannelsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetCommentThreadsInstance : TCommentThreadsResource;

begin
  if (FCommentThreadsInstance=Nil) then
    FCommentThreadsInstance:=CreateCommentThreadsResource;
  Result:=FCommentThreadsInstance;
end;

Function TYoutubeAPI.CreateCommentThreadsResource : TCommentThreadsResource;

begin
  Result:=CreateCommentThreadsResource(Self);
end;


Function TYoutubeAPI.CreateCommentThreadsResource(AOwner : TComponent) : TCommentThreadsResource;

begin
  Result:=TCommentThreadsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetCommentsInstance : TCommentsResource;

begin
  if (FCommentsInstance=Nil) then
    FCommentsInstance:=CreateCommentsResource;
  Result:=FCommentsInstance;
end;

Function TYoutubeAPI.CreateCommentsResource : TCommentsResource;

begin
  Result:=CreateCommentsResource(Self);
end;


Function TYoutubeAPI.CreateCommentsResource(AOwner : TComponent) : TCommentsResource;

begin
  Result:=TCommentsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetGuideCategoriesInstance : TGuideCategoriesResource;

begin
  if (FGuideCategoriesInstance=Nil) then
    FGuideCategoriesInstance:=CreateGuideCategoriesResource;
  Result:=FGuideCategoriesInstance;
end;

Function TYoutubeAPI.CreateGuideCategoriesResource : TGuideCategoriesResource;

begin
  Result:=CreateGuideCategoriesResource(Self);
end;


Function TYoutubeAPI.CreateGuideCategoriesResource(AOwner : TComponent) : TGuideCategoriesResource;

begin
  Result:=TGuideCategoriesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetI18nLanguagesInstance : TI18nLanguagesResource;

begin
  if (FI18nLanguagesInstance=Nil) then
    FI18nLanguagesInstance:=CreateI18nLanguagesResource;
  Result:=FI18nLanguagesInstance;
end;

Function TYoutubeAPI.CreateI18nLanguagesResource : TI18nLanguagesResource;

begin
  Result:=CreateI18nLanguagesResource(Self);
end;


Function TYoutubeAPI.CreateI18nLanguagesResource(AOwner : TComponent) : TI18nLanguagesResource;

begin
  Result:=TI18nLanguagesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetI18nRegionsInstance : TI18nRegionsResource;

begin
  if (FI18nRegionsInstance=Nil) then
    FI18nRegionsInstance:=CreateI18nRegionsResource;
  Result:=FI18nRegionsInstance;
end;

Function TYoutubeAPI.CreateI18nRegionsResource : TI18nRegionsResource;

begin
  Result:=CreateI18nRegionsResource(Self);
end;


Function TYoutubeAPI.CreateI18nRegionsResource(AOwner : TComponent) : TI18nRegionsResource;

begin
  Result:=TI18nRegionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetLiveBroadcastsInstance : TLiveBroadcastsResource;

begin
  if (FLiveBroadcastsInstance=Nil) then
    FLiveBroadcastsInstance:=CreateLiveBroadcastsResource;
  Result:=FLiveBroadcastsInstance;
end;

Function TYoutubeAPI.CreateLiveBroadcastsResource : TLiveBroadcastsResource;

begin
  Result:=CreateLiveBroadcastsResource(Self);
end;


Function TYoutubeAPI.CreateLiveBroadcastsResource(AOwner : TComponent) : TLiveBroadcastsResource;

begin
  Result:=TLiveBroadcastsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetLiveStreamsInstance : TLiveStreamsResource;

begin
  if (FLiveStreamsInstance=Nil) then
    FLiveStreamsInstance:=CreateLiveStreamsResource;
  Result:=FLiveStreamsInstance;
end;

Function TYoutubeAPI.CreateLiveStreamsResource : TLiveStreamsResource;

begin
  Result:=CreateLiveStreamsResource(Self);
end;


Function TYoutubeAPI.CreateLiveStreamsResource(AOwner : TComponent) : TLiveStreamsResource;

begin
  Result:=TLiveStreamsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetPlaylistItemsInstance : TPlaylistItemsResource;

begin
  if (FPlaylistItemsInstance=Nil) then
    FPlaylistItemsInstance:=CreatePlaylistItemsResource;
  Result:=FPlaylistItemsInstance;
end;

Function TYoutubeAPI.CreatePlaylistItemsResource : TPlaylistItemsResource;

begin
  Result:=CreatePlaylistItemsResource(Self);
end;


Function TYoutubeAPI.CreatePlaylistItemsResource(AOwner : TComponent) : TPlaylistItemsResource;

begin
  Result:=TPlaylistItemsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetPlaylistsInstance : TPlaylistsResource;

begin
  if (FPlaylistsInstance=Nil) then
    FPlaylistsInstance:=CreatePlaylistsResource;
  Result:=FPlaylistsInstance;
end;

Function TYoutubeAPI.CreatePlaylistsResource : TPlaylistsResource;

begin
  Result:=CreatePlaylistsResource(Self);
end;


Function TYoutubeAPI.CreatePlaylistsResource(AOwner : TComponent) : TPlaylistsResource;

begin
  Result:=TPlaylistsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetSearchInstance : TSearchResource;

begin
  if (FSearchInstance=Nil) then
    FSearchInstance:=CreateSearchResource;
  Result:=FSearchInstance;
end;

Function TYoutubeAPI.CreateSearchResource : TSearchResource;

begin
  Result:=CreateSearchResource(Self);
end;


Function TYoutubeAPI.CreateSearchResource(AOwner : TComponent) : TSearchResource;

begin
  Result:=TSearchResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetSubscriptionsInstance : TSubscriptionsResource;

begin
  if (FSubscriptionsInstance=Nil) then
    FSubscriptionsInstance:=CreateSubscriptionsResource;
  Result:=FSubscriptionsInstance;
end;

Function TYoutubeAPI.CreateSubscriptionsResource : TSubscriptionsResource;

begin
  Result:=CreateSubscriptionsResource(Self);
end;


Function TYoutubeAPI.CreateSubscriptionsResource(AOwner : TComponent) : TSubscriptionsResource;

begin
  Result:=TSubscriptionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetThumbnailsInstance : TThumbnailsResource;

begin
  if (FThumbnailsInstance=Nil) then
    FThumbnailsInstance:=CreateThumbnailsResource;
  Result:=FThumbnailsInstance;
end;

Function TYoutubeAPI.CreateThumbnailsResource : TThumbnailsResource;

begin
  Result:=CreateThumbnailsResource(Self);
end;


Function TYoutubeAPI.CreateThumbnailsResource(AOwner : TComponent) : TThumbnailsResource;

begin
  Result:=TThumbnailsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetVideoAbuseReportReasonsInstance : TVideoAbuseReportReasonsResource;

begin
  if (FVideoAbuseReportReasonsInstance=Nil) then
    FVideoAbuseReportReasonsInstance:=CreateVideoAbuseReportReasonsResource;
  Result:=FVideoAbuseReportReasonsInstance;
end;

Function TYoutubeAPI.CreateVideoAbuseReportReasonsResource : TVideoAbuseReportReasonsResource;

begin
  Result:=CreateVideoAbuseReportReasonsResource(Self);
end;


Function TYoutubeAPI.CreateVideoAbuseReportReasonsResource(AOwner : TComponent) : TVideoAbuseReportReasonsResource;

begin
  Result:=TVideoAbuseReportReasonsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetVideoCategoriesInstance : TVideoCategoriesResource;

begin
  if (FVideoCategoriesInstance=Nil) then
    FVideoCategoriesInstance:=CreateVideoCategoriesResource;
  Result:=FVideoCategoriesInstance;
end;

Function TYoutubeAPI.CreateVideoCategoriesResource : TVideoCategoriesResource;

begin
  Result:=CreateVideoCategoriesResource(Self);
end;


Function TYoutubeAPI.CreateVideoCategoriesResource(AOwner : TComponent) : TVideoCategoriesResource;

begin
  Result:=TVideoCategoriesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetVideosInstance : TVideosResource;

begin
  if (FVideosInstance=Nil) then
    FVideosInstance:=CreateVideosResource;
  Result:=FVideosInstance;
end;

Function TYoutubeAPI.CreateVideosResource : TVideosResource;

begin
  Result:=CreateVideosResource(Self);
end;


Function TYoutubeAPI.CreateVideosResource(AOwner : TComponent) : TVideosResource;

begin
  Result:=TVideosResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAPI.GetWatermarksInstance : TWatermarksResource;

begin
  if (FWatermarksInstance=Nil) then
    FWatermarksInstance:=CreateWatermarksResource;
  Result:=FWatermarksInstance;
end;

Function TYoutubeAPI.CreateWatermarksResource : TWatermarksResource;

begin
  Result:=CreateWatermarksResource(Self);
end;


Function TYoutubeAPI.CreateWatermarksResource(AOwner : TComponent) : TWatermarksResource;

begin
  Result:=TWatermarksResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TYoutubeAPI.RegisterAPI;
end.
