unit googledfareporting;
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
//Generated on: 9-5-15 13:22:52
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = class;
  TAccountActiveAdSummary = class;
  TAccountPermission = class;
  TAccountPermissionGroup = class;
  TAccountPermissionGroupsListResponse = class;
  TAccountPermissionsListResponse = class;
  TAccountUserProfile = class;
  TAccountUserProfilesListResponse = class;
  TAccountsListResponse = class;
  TActivities = class;
  TAd = class;
  TAdSlot = class;
  TAdsListResponse = class;
  TAdvertiser = class;
  TAdvertiserGroup = class;
  TAdvertiserGroupsListResponse = class;
  TAdvertisersListResponse = class;
  TAudienceSegment = class;
  TAudienceSegmentGroup = class;
  TBrowser = class;
  TBrowsersListResponse = class;
  TCampaign = class;
  TCampaignCreativeAssociation = class;
  TCampaignCreativeAssociationsListResponse = class;
  TCampaignsListResponse = class;
  TChangeLog = class;
  TChangeLogsListResponse = class;
  TCitiesListResponse = class;
  TCity = class;
  TClickTag = class;
  TClickThroughUrl = class;
  TClickThroughUrlSuffixProperties = class;
  TCompanionClickThroughOverride = class;
  TCompatibleFields = class;
  TConnectionType = class;
  TConnectionTypesListResponse = class;
  TContentCategoriesListResponse = class;
  TContentCategory = class;
  TCountriesListResponse = class;
  TCountry = class;
  TCreative = class;
  TCreativeAsset = class;
  TCreativeAssetId = class;
  TCreativeAssetMetadata = class;
  TCreativeAssignment = class;
  TCreativeCustomEvent = class;
  TCreativeField = class;
  TCreativeFieldAssignment = class;
  TCreativeFieldValue = class;
  TCreativeFieldValuesListResponse = class;
  TCreativeFieldsListResponse = class;
  TCreativeGroup = class;
  TCreativeGroupAssignment = class;
  TCreativeGroupsListResponse = class;
  TCreativeOptimizationConfiguration = class;
  TCreativeRotation = class;
  TCreativeSettings = class;
  TCreativesListResponse = class;
  TCrossDimensionReachReportCompatibleFields = class;
  TCustomRichMediaEvents = class;
  TDateRange = class;
  TDayPartTargeting = class;
  TDefaultClickThroughEventTagProperties = class;
  TDeliverySchedule = class;
  TDfpSettings = class;
  TDimension = class;
  TDimensionFilter = class;
  TDimensionValue = class;
  TDimensionValueList = class;
  TDimensionValueRequest = class;
  TDirectorySite = class;
  TDirectorySiteContact = class;
  TDirectorySiteContactAssignment = class;
  TDirectorySiteContactsListResponse = class;
  TDirectorySiteSettings = class;
  TDirectorySitesListResponse = class;
  TEventTag = class;
  TEventTagOverride = class;
  TEventTagsListResponse = class;
  TFile = class;
  TFileList = class;
  TFlight = class;
  TFloodlightActivitiesGenerateTagResponse = class;
  TFloodlightActivitiesListResponse = class;
  TFloodlightActivity = class;
  TFloodlightActivityDynamicTag = class;
  TFloodlightActivityGroup = class;
  TFloodlightActivityGroupsListResponse = class;
  TFloodlightActivityPublisherDynamicTag = class;
  TFloodlightConfiguration = class;
  TFloodlightConfigurationsListResponse = class;
  TFloodlightReportCompatibleFields = class;
  TFrequencyCap = class;
  TFsCommand = class;
  TGeoTargeting = class;
  TInventoryItem = class;
  TInventoryItemsListResponse = class;
  TKeyValueTargetingExpression = class;
  TLandingPage = class;
  TLandingPagesListResponse = class;
  TLastModifiedInfo = class;
  TListPopulationClause = class;
  TListPopulationRule = class;
  TListPopulationTerm = class;
  TListTargetingExpression = class;
  TLookbackConfiguration = class;
  TMetric = class;
  TMetro = class;
  TMetrosListResponse = class;
  TMobileCarrier = class;
  TMobileCarriersListResponse = class;
  TObjectFilter = class;
  TOffsetPosition = class;
  TOmnitureSettings = class;
  TOperatingSystem = class;
  TOperatingSystemVersion = class;
  TOperatingSystemVersionsListResponse = class;
  TOperatingSystemsListResponse = class;
  TOptimizationActivity = class;
  TOrder = class;
  TOrderContact = class;
  TOrderDocument = class;
  TOrderDocumentsListResponse = class;
  TOrdersListResponse = class;
  TPathToConversionReportCompatibleFields = class;
  TPlacement = class;
  TPlacementAssignment = class;
  TPlacementGroup = class;
  TPlacementGroupsListResponse = class;
  TPlacementStrategiesListResponse = class;
  TPlacementStrategy = class;
  TPlacementTag = class;
  TPlacementsGenerateTagsResponse = class;
  TPlacementsListResponse = class;
  TPlatformType = class;
  TPlatformTypesListResponse = class;
  TPopupWindowProperties = class;
  TPostalCode = class;
  TPostalCodesListResponse = class;
  TPricing = class;
  TPricingSchedule = class;
  TPricingSchedulePricingPeriod = class;
  TProgrammaticSetting = class;
  TProject = class;
  TProjectsListResponse = class;
  TReachReportCompatibleFields = class;
  TRecipient = class;
  TRegion = class;
  TRegionsListResponse = class;
  TRemarketingList = class;
  TRemarketingListShare = class;
  TRemarketingListsListResponse = class;
  TReport = class;
  TReportCompatibleFields = class;
  TReportList = class;
  TReportsConfiguration = class;
  TRichMediaExitOverride = class;
  TSite = class;
  TSiteContact = class;
  TSiteSettings = class;
  TSitesListResponse = class;
  TSize = class;
  TSizesListResponse = class;
  TSortedDimension = class;
  TSubaccount = class;
  TSubaccountsListResponse = class;
  TTagData = class;
  TTagSetting = class;
  TTagSettings = class;
  TTargetWindow = class;
  TTargetableRemarketingList = class;
  TTargetableRemarketingListsListResponse = class;
  TTechnologyTargeting = class;
  TThirdPartyTrackingUrl = class;
  TUserDefinedVariableConfiguration = class;
  TUserProfile = class;
  TUserProfileList = class;
  TUserRole = class;
  TUserRolePermission = class;
  TUserRolePermissionGroup = class;
  TUserRolePermissionGroupsListResponse = class;
  TUserRolePermissionsListResponse = class;
  TUserRolesListResponse = class;
  TAccountArray = Array of TAccount;
  TAccountActiveAdSummaryArray = Array of TAccountActiveAdSummary;
  TAccountPermissionArray = Array of TAccountPermission;
  TAccountPermissionGroupArray = Array of TAccountPermissionGroup;
  TAccountPermissionGroupsListResponseArray = Array of TAccountPermissionGroupsListResponse;
  TAccountPermissionsListResponseArray = Array of TAccountPermissionsListResponse;
  TAccountUserProfileArray = Array of TAccountUserProfile;
  TAccountUserProfilesListResponseArray = Array of TAccountUserProfilesListResponse;
  TAccountsListResponseArray = Array of TAccountsListResponse;
  TActivitiesArray = Array of TActivities;
  TAdArray = Array of TAd;
  TAdSlotArray = Array of TAdSlot;
  TAdsListResponseArray = Array of TAdsListResponse;
  TAdvertiserArray = Array of TAdvertiser;
  TAdvertiserGroupArray = Array of TAdvertiserGroup;
  TAdvertiserGroupsListResponseArray = Array of TAdvertiserGroupsListResponse;
  TAdvertisersListResponseArray = Array of TAdvertisersListResponse;
  TAudienceSegmentArray = Array of TAudienceSegment;
  TAudienceSegmentGroupArray = Array of TAudienceSegmentGroup;
  TBrowserArray = Array of TBrowser;
  TBrowsersListResponseArray = Array of TBrowsersListResponse;
  TCampaignArray = Array of TCampaign;
  TCampaignCreativeAssociationArray = Array of TCampaignCreativeAssociation;
  TCampaignCreativeAssociationsListResponseArray = Array of TCampaignCreativeAssociationsListResponse;
  TCampaignsListResponseArray = Array of TCampaignsListResponse;
  TChangeLogArray = Array of TChangeLog;
  TChangeLogsListResponseArray = Array of TChangeLogsListResponse;
  TCitiesListResponseArray = Array of TCitiesListResponse;
  TCityArray = Array of TCity;
  TClickTagArray = Array of TClickTag;
  TClickThroughUrlArray = Array of TClickThroughUrl;
  TClickThroughUrlSuffixPropertiesArray = Array of TClickThroughUrlSuffixProperties;
  TCompanionClickThroughOverrideArray = Array of TCompanionClickThroughOverride;
  TCompatibleFieldsArray = Array of TCompatibleFields;
  TConnectionTypeArray = Array of TConnectionType;
  TConnectionTypesListResponseArray = Array of TConnectionTypesListResponse;
  TContentCategoriesListResponseArray = Array of TContentCategoriesListResponse;
  TContentCategoryArray = Array of TContentCategory;
  TCountriesListResponseArray = Array of TCountriesListResponse;
  TCountryArray = Array of TCountry;
  TCreativeArray = Array of TCreative;
  TCreativeAssetArray = Array of TCreativeAsset;
  TCreativeAssetIdArray = Array of TCreativeAssetId;
  TCreativeAssetMetadataArray = Array of TCreativeAssetMetadata;
  TCreativeAssignmentArray = Array of TCreativeAssignment;
  TCreativeCustomEventArray = Array of TCreativeCustomEvent;
  TCreativeFieldArray = Array of TCreativeField;
  TCreativeFieldAssignmentArray = Array of TCreativeFieldAssignment;
  TCreativeFieldValueArray = Array of TCreativeFieldValue;
  TCreativeFieldValuesListResponseArray = Array of TCreativeFieldValuesListResponse;
  TCreativeFieldsListResponseArray = Array of TCreativeFieldsListResponse;
  TCreativeGroupArray = Array of TCreativeGroup;
  TCreativeGroupAssignmentArray = Array of TCreativeGroupAssignment;
  TCreativeGroupsListResponseArray = Array of TCreativeGroupsListResponse;
  TCreativeOptimizationConfigurationArray = Array of TCreativeOptimizationConfiguration;
  TCreativeRotationArray = Array of TCreativeRotation;
  TCreativeSettingsArray = Array of TCreativeSettings;
  TCreativesListResponseArray = Array of TCreativesListResponse;
  TCrossDimensionReachReportCompatibleFieldsArray = Array of TCrossDimensionReachReportCompatibleFields;
  TCustomRichMediaEventsArray = Array of TCustomRichMediaEvents;
  TDateRangeArray = Array of TDateRange;
  TDayPartTargetingArray = Array of TDayPartTargeting;
  TDefaultClickThroughEventTagPropertiesArray = Array of TDefaultClickThroughEventTagProperties;
  TDeliveryScheduleArray = Array of TDeliverySchedule;
  TDfpSettingsArray = Array of TDfpSettings;
  TDimensionArray = Array of TDimension;
  TDimensionFilterArray = Array of TDimensionFilter;
  TDimensionValueArray = Array of TDimensionValue;
  TDimensionValueListArray = Array of TDimensionValueList;
  TDimensionValueRequestArray = Array of TDimensionValueRequest;
  TDirectorySiteArray = Array of TDirectorySite;
  TDirectorySiteContactArray = Array of TDirectorySiteContact;
  TDirectorySiteContactAssignmentArray = Array of TDirectorySiteContactAssignment;
  TDirectorySiteContactsListResponseArray = Array of TDirectorySiteContactsListResponse;
  TDirectorySiteSettingsArray = Array of TDirectorySiteSettings;
  TDirectorySitesListResponseArray = Array of TDirectorySitesListResponse;
  TEventTagArray = Array of TEventTag;
  TEventTagOverrideArray = Array of TEventTagOverride;
  TEventTagsListResponseArray = Array of TEventTagsListResponse;
  TFileArray = Array of TFile;
  TFileListArray = Array of TFileList;
  TFlightArray = Array of TFlight;
  TFloodlightActivitiesGenerateTagResponseArray = Array of TFloodlightActivitiesGenerateTagResponse;
  TFloodlightActivitiesListResponseArray = Array of TFloodlightActivitiesListResponse;
  TFloodlightActivityArray = Array of TFloodlightActivity;
  TFloodlightActivityDynamicTagArray = Array of TFloodlightActivityDynamicTag;
  TFloodlightActivityGroupArray = Array of TFloodlightActivityGroup;
  TFloodlightActivityGroupsListResponseArray = Array of TFloodlightActivityGroupsListResponse;
  TFloodlightActivityPublisherDynamicTagArray = Array of TFloodlightActivityPublisherDynamicTag;
  TFloodlightConfigurationArray = Array of TFloodlightConfiguration;
  TFloodlightConfigurationsListResponseArray = Array of TFloodlightConfigurationsListResponse;
  TFloodlightReportCompatibleFieldsArray = Array of TFloodlightReportCompatibleFields;
  TFrequencyCapArray = Array of TFrequencyCap;
  TFsCommandArray = Array of TFsCommand;
  TGeoTargetingArray = Array of TGeoTargeting;
  TInventoryItemArray = Array of TInventoryItem;
  TInventoryItemsListResponseArray = Array of TInventoryItemsListResponse;
  TKeyValueTargetingExpressionArray = Array of TKeyValueTargetingExpression;
  TLandingPageArray = Array of TLandingPage;
  TLandingPagesListResponseArray = Array of TLandingPagesListResponse;
  TLastModifiedInfoArray = Array of TLastModifiedInfo;
  TListPopulationClauseArray = Array of TListPopulationClause;
  TListPopulationRuleArray = Array of TListPopulationRule;
  TListPopulationTermArray = Array of TListPopulationTerm;
  TListTargetingExpressionArray = Array of TListTargetingExpression;
  TLookbackConfigurationArray = Array of TLookbackConfiguration;
  TMetricArray = Array of TMetric;
  TMetroArray = Array of TMetro;
  TMetrosListResponseArray = Array of TMetrosListResponse;
  TMobileCarrierArray = Array of TMobileCarrier;
  TMobileCarriersListResponseArray = Array of TMobileCarriersListResponse;
  TObjectFilterArray = Array of TObjectFilter;
  TOffsetPositionArray = Array of TOffsetPosition;
  TOmnitureSettingsArray = Array of TOmnitureSettings;
  TOperatingSystemArray = Array of TOperatingSystem;
  TOperatingSystemVersionArray = Array of TOperatingSystemVersion;
  TOperatingSystemVersionsListResponseArray = Array of TOperatingSystemVersionsListResponse;
  TOperatingSystemsListResponseArray = Array of TOperatingSystemsListResponse;
  TOptimizationActivityArray = Array of TOptimizationActivity;
  TOrderArray = Array of TOrder;
  TOrderContactArray = Array of TOrderContact;
  TOrderDocumentArray = Array of TOrderDocument;
  TOrderDocumentsListResponseArray = Array of TOrderDocumentsListResponse;
  TOrdersListResponseArray = Array of TOrdersListResponse;
  TPathToConversionReportCompatibleFieldsArray = Array of TPathToConversionReportCompatibleFields;
  TPlacementArray = Array of TPlacement;
  TPlacementAssignmentArray = Array of TPlacementAssignment;
  TPlacementGroupArray = Array of TPlacementGroup;
  TPlacementGroupsListResponseArray = Array of TPlacementGroupsListResponse;
  TPlacementStrategiesListResponseArray = Array of TPlacementStrategiesListResponse;
  TPlacementStrategyArray = Array of TPlacementStrategy;
  TPlacementTagArray = Array of TPlacementTag;
  TPlacementsGenerateTagsResponseArray = Array of TPlacementsGenerateTagsResponse;
  TPlacementsListResponseArray = Array of TPlacementsListResponse;
  TPlatformTypeArray = Array of TPlatformType;
  TPlatformTypesListResponseArray = Array of TPlatformTypesListResponse;
  TPopupWindowPropertiesArray = Array of TPopupWindowProperties;
  TPostalCodeArray = Array of TPostalCode;
  TPostalCodesListResponseArray = Array of TPostalCodesListResponse;
  TPricingArray = Array of TPricing;
  TPricingScheduleArray = Array of TPricingSchedule;
  TPricingSchedulePricingPeriodArray = Array of TPricingSchedulePricingPeriod;
  TProgrammaticSettingArray = Array of TProgrammaticSetting;
  TProjectArray = Array of TProject;
  TProjectsListResponseArray = Array of TProjectsListResponse;
  TReachReportCompatibleFieldsArray = Array of TReachReportCompatibleFields;
  TRecipientArray = Array of TRecipient;
  TRegionArray = Array of TRegion;
  TRegionsListResponseArray = Array of TRegionsListResponse;
  TRemarketingListArray = Array of TRemarketingList;
  TRemarketingListShareArray = Array of TRemarketingListShare;
  TRemarketingListsListResponseArray = Array of TRemarketingListsListResponse;
  TReportArray = Array of TReport;
  TReportCompatibleFieldsArray = Array of TReportCompatibleFields;
  TReportListArray = Array of TReportList;
  TReportsConfigurationArray = Array of TReportsConfiguration;
  TRichMediaExitOverrideArray = Array of TRichMediaExitOverride;
  TSiteArray = Array of TSite;
  TSiteContactArray = Array of TSiteContact;
  TSiteSettingsArray = Array of TSiteSettings;
  TSitesListResponseArray = Array of TSitesListResponse;
  TSizeArray = Array of TSize;
  TSizesListResponseArray = Array of TSizesListResponse;
  TSortedDimensionArray = Array of TSortedDimension;
  TSubaccountArray = Array of TSubaccount;
  TSubaccountsListResponseArray = Array of TSubaccountsListResponse;
  TTagDataArray = Array of TTagData;
  TTagSettingArray = Array of TTagSetting;
  TTagSettingsArray = Array of TTagSettings;
  TTargetWindowArray = Array of TTargetWindow;
  TTargetableRemarketingListArray = Array of TTargetableRemarketingList;
  TTargetableRemarketingListsListResponseArray = Array of TTargetableRemarketingListsListResponse;
  TTechnologyTargetingArray = Array of TTechnologyTargeting;
  TThirdPartyTrackingUrlArray = Array of TThirdPartyTrackingUrl;
  TUserDefinedVariableConfigurationArray = Array of TUserDefinedVariableConfiguration;
  TUserProfileArray = Array of TUserProfile;
  TUserProfileListArray = Array of TUserProfileList;
  TUserRoleArray = Array of TUserRole;
  TUserRolePermissionArray = Array of TUserRolePermission;
  TUserRolePermissionGroupArray = Array of TUserRolePermissionGroup;
  TUserRolePermissionGroupsListResponseArray = Array of TUserRolePermissionGroupsListResponse;
  TUserRolePermissionsListResponseArray = Array of TUserRolePermissionsListResponse;
  TUserRolesListResponseArray = Array of TUserRolesListResponse;
  //Anonymous types, using auto-generated names
  TFileTypeurls = class;
  TReportTypecriteria = class;
  TReportTypecrossDimensionReachCriteria = class;
  TReportTypedelivery = class;
  TReportTypefloodlightCriteriaTypereportProperties = class;
  TReportTypefloodlightCriteria = class;
  TReportTypepathToConversionCriteriaTypereportProperties = class;
  TReportTypepathToConversionCriteria = class;
  TReportTypereachCriteria = class;
  TReportTypeschedule = class;
  TAccountPermissionGroupsListResponseTypeaccountPermissionGroupsArray = Array of TAccountPermissionGroup;
  TAccountPermissionsListResponseTypeaccountPermissionsArray = Array of TAccountPermission;
  TAccountUserProfilesListResponseTypeaccountUserProfilesArray = Array of TAccountUserProfile;
  TAccountsListResponseTypeaccountsArray = Array of TAccount;
  TActivitiesTypefiltersArray = Array of TDimensionValue;
  TAdTypecreativeGroupAssignmentsArray = Array of TCreativeGroupAssignment;
  TAdTypeeventTagOverridesArray = Array of TEventTagOverride;
  TAdTypeplacementAssignmentsArray = Array of TPlacementAssignment;
  TAdsListResponseTypeadsArray = Array of TAd;
  TAdvertiserGroupsListResponseTypeadvertiserGroupsArray = Array of TAdvertiserGroup;
  TAdvertisersListResponseTypeadvertisersArray = Array of TAdvertiser;
  TAudienceSegmentGroupTypeaudienceSegmentsArray = Array of TAudienceSegment;
  TBrowsersListResponseTypebrowsersArray = Array of TBrowser;
  TCampaignTypeadditionalCreativeOptimizationConfigurationsArray = Array of TCreativeOptimizationConfiguration;
  TCampaignTypeaudienceSegmentGroupsArray = Array of TAudienceSegmentGroup;
  TCampaignTypeeventTagOverridesArray = Array of TEventTagOverride;
  TCampaignCreativeAssociationsListResponseTypecampaignCreativeAssociationsArray = Array of TCampaignCreativeAssociation;
  TCampaignsListResponseTypecampaignsArray = Array of TCampaign;
  TChangeLogsListResponseTypechangeLogsArray = Array of TChangeLog;
  TCitiesListResponseTypecitiesArray = Array of TCity;
  TConnectionTypesListResponseTypeconnectionTypesArray = Array of TConnectionType;
  TContentCategoriesListResponseTypecontentCategoriesArray = Array of TContentCategory;
  TCountriesListResponseTypecountriesArray = Array of TCountry;
  TCreativeTypeclickTagsArray = Array of TClickTag;
  TCreativeTypecounterCustomEventsArray = Array of TCreativeCustomEvent;
  TCreativeTypecreativeAssetsArray = Array of TCreativeAsset;
  TCreativeTypecreativeFieldAssignmentsArray = Array of TCreativeFieldAssignment;
  TCreativeTypeexitCustomEventsArray = Array of TCreativeCustomEvent;
  TCreativeTypethirdPartyUrlsArray = Array of TThirdPartyTrackingUrl;
  TCreativeTypetimerCustomEventsArray = Array of TCreativeCustomEvent;
  TCreativeAssetMetadataTypeclickTagsArray = Array of TClickTag;
  TCreativeAssignmentTypecompanionCreativeOverridesArray = Array of TCompanionClickThroughOverride;
  TCreativeAssignmentTypecreativeGroupAssignmentsArray = Array of TCreativeGroupAssignment;
  TCreativeAssignmentTyperichMediaExitOverridesArray = Array of TRichMediaExitOverride;
  TCreativeFieldValuesListResponseTypecreativeFieldValuesArray = Array of TCreativeFieldValue;
  TCreativeFieldsListResponseTypecreativeFieldsArray = Array of TCreativeField;
  TCreativeGroupsListResponseTypecreativeGroupsArray = Array of TCreativeGroup;
  TCreativeOptimizationConfigurationTypeoptimizationActivitysArray = Array of TOptimizationActivity;
  TCreativeRotationTypecreativeAssignmentsArray = Array of TCreativeAssignment;
  TCreativesListResponseTypecreativesArray = Array of TCreative;
  TCrossDimensionReachReportCompatibleFieldsTypebreakdownArray = Array of TDimension;
  TCrossDimensionReachReportCompatibleFieldsTypedimensionFiltersArray = Array of TDimension;
  TCrossDimensionReachReportCompatibleFieldsTypemetricsArray = Array of TMetric;
  TCrossDimensionReachReportCompatibleFieldsTypeoverlapMetricsArray = Array of TMetric;
  TCustomRichMediaEventsTypefilteredEventIdsArray = Array of TDimensionValue;
  TDimensionValueListTypeitemsArray = Array of TDimensionValue;
  TDimensionValueRequestTypefiltersArray = Array of TDimensionFilter;
  TDirectorySiteTypecontactAssignmentsArray = Array of TDirectorySiteContactAssignment;
  TDirectorySiteContactsListResponseTypedirectorySiteContactsArray = Array of TDirectorySiteContact;
  TDirectorySitesListResponseTypedirectorySitesArray = Array of TDirectorySite;
  TEventTagsListResponseTypeeventTagsArray = Array of TEventTag;
  TFileListTypeitemsArray = Array of TFile;
  TFloodlightActivitiesListResponseTypefloodlightActivitiesArray = Array of TFloodlightActivity;
  TFloodlightActivityTypedefaultTagsArray = Array of TFloodlightActivityDynamicTag;
  TFloodlightActivityTypepublisherTagsArray = Array of TFloodlightActivityPublisherDynamicTag;
  TFloodlightActivityGroupsListResponseTypefloodlightActivityGroupsArray = Array of TFloodlightActivityGroup;
  TFloodlightConfigurationTypeuserDefinedVariableConfigurationsArray = Array of TUserDefinedVariableConfiguration;
  TFloodlightConfigurationsListResponseTypefloodlightConfigurationsArray = Array of TFloodlightConfiguration;
  TFloodlightReportCompatibleFieldsTypedimensionFiltersArray = Array of TDimension;
  TFloodlightReportCompatibleFieldsTypedimensionsArray = Array of TDimension;
  TFloodlightReportCompatibleFieldsTypemetricsArray = Array of TMetric;
  TGeoTargetingTypecitiesArray = Array of TCity;
  TGeoTargetingTypecountriesArray = Array of TCountry;
  TGeoTargetingTypemetrosArray = Array of TMetro;
  TGeoTargetingTypepostalCodesArray = Array of TPostalCode;
  TGeoTargetingTyperegionsArray = Array of TRegion;
  TInventoryItemTypeadSlotsArray = Array of TAdSlot;
  TInventoryItemsListResponseTypeinventoryItemsArray = Array of TInventoryItem;
  TLandingPagesListResponseTypelandingPagesArray = Array of TLandingPage;
  TListPopulationClauseTypetermsArray = Array of TListPopulationTerm;
  TListPopulationRuleTypelistPopulationClausesArray = Array of TListPopulationClause;
  TMetrosListResponseTypemetrosArray = Array of TMetro;
  TMobileCarriersListResponseTypemobileCarriersArray = Array of TMobileCarrier;
  TOperatingSystemVersionsListResponseTypeoperatingSystemVersionsArray = Array of TOperatingSystemVersion;
  TOperatingSystemsListResponseTypeoperatingSystemsArray = Array of TOperatingSystem;
  TOrderTypecontactsArray = Array of TOrderContact;
  TOrderDocumentsListResponseTypeorderDocumentsArray = Array of TOrderDocument;
  TOrdersListResponseTypeordersArray = Array of TOrder;
  TPathToConversionReportCompatibleFieldsTypeconversionDimensionsArray = Array of TDimension;
  TPathToConversionReportCompatibleFieldsTypecustomFloodlightVariablesArray = Array of TDimension;
  TPathToConversionReportCompatibleFieldsTypemetricsArray = Array of TMetric;
  TPathToConversionReportCompatibleFieldsTypeperInteractionDimensionsArray = Array of TDimension;
  TPlacementGroupsListResponseTypeplacementGroupsArray = Array of TPlacementGroup;
  TPlacementStrategiesListResponseTypeplacementStrategiesArray = Array of TPlacementStrategy;
  TPlacementTagTypetagDatasArray = Array of TTagData;
  TPlacementsGenerateTagsResponseTypeplacementTagsArray = Array of TPlacementTag;
  TPlacementsListResponseTypeplacementsArray = Array of TPlacement;
  TPlatformTypesListResponseTypeplatformTypesArray = Array of TPlatformType;
  TPostalCodesListResponseTypepostalCodesArray = Array of TPostalCode;
  TPricingTypeflightsArray = Array of TFlight;
  TPricingScheduleTypepricingPeriodsArray = Array of TPricingSchedulePricingPeriod;
  TProjectsListResponseTypeprojectsArray = Array of TProject;
  TReachReportCompatibleFieldsTypedimensionFiltersArray = Array of TDimension;
  TReachReportCompatibleFieldsTypedimensionsArray = Array of TDimension;
  TReachReportCompatibleFieldsTypemetricsArray = Array of TMetric;
  TReachReportCompatibleFieldsTypepivotedActivityMetricsArray = Array of TMetric;
  TReachReportCompatibleFieldsTypereachByFrequencyMetricsArray = Array of TMetric;
  TRegionsListResponseTyperegionsArray = Array of TRegion;
  TRemarketingListsListResponseTyperemarketingListsArray = Array of TRemarketingList;
  TReportTypecriteriaTypedimensionFiltersArray = Array of TDimensionValue;
  TReportTypecriteriaTypedimensionsArray = Array of TSortedDimension;
  TReportTypecrossDimensionReachCriteriaTypebreakdownArray = Array of TSortedDimension;
  TReportTypecrossDimensionReachCriteriaTypedimensionFiltersArray = Array of TDimensionValue;
  TReportTypedeliveryTyperecipientsArray = Array of TRecipient;
  TReportTypefloodlightCriteriaTypecustomRichMediaEventsArray = Array of TDimensionValue;
  TReportTypefloodlightCriteriaTypedimensionFiltersArray = Array of TDimensionValue;
  TReportTypefloodlightCriteriaTypedimensionsArray = Array of TSortedDimension;
  TReportTypepathToConversionCriteriaTypeactivityFiltersArray = Array of TDimensionValue;
  TReportTypepathToConversionCriteriaTypeconversionDimensionsArray = Array of TSortedDimension;
  TReportTypepathToConversionCriteriaTypecustomFloodlightVariablesArray = Array of TSortedDimension;
  TReportTypepathToConversionCriteriaTypecustomRichMediaEventsArray = Array of TDimensionValue;
  TReportTypepathToConversionCriteriaTypeperInteractionDimensionsArray = Array of TSortedDimension;
  TReportTypereachCriteriaTypedimensionFiltersArray = Array of TDimensionValue;
  TReportTypereachCriteriaTypedimensionsArray = Array of TSortedDimension;
  TReportCompatibleFieldsTypedimensionFiltersArray = Array of TDimension;
  TReportCompatibleFieldsTypedimensionsArray = Array of TDimension;
  TReportCompatibleFieldsTypemetricsArray = Array of TMetric;
  TReportCompatibleFieldsTypepivotedActivityMetricsArray = Array of TMetric;
  TReportListTypeitemsArray = Array of TReport;
  TSiteTypesiteContactsArray = Array of TSiteContact;
  TSitesListResponseTypesitesArray = Array of TSite;
  TSizesListResponseTypesizesArray = Array of TSize;
  TSubaccountsListResponseTypesubaccountsArray = Array of TSubaccount;
  TTargetableRemarketingListsListResponseTypetargetableRemarketingListsArray = Array of TTargetableRemarketingList;
  TTechnologyTargetingTypebrowsersArray = Array of TBrowser;
  TTechnologyTargetingTypeconnectionTypesArray = Array of TConnectionType;
  TTechnologyTargetingTypemobileCarriersArray = Array of TMobileCarrier;
  TTechnologyTargetingTypeoperatingSystemVersionsArray = Array of TOperatingSystemVersion;
  TTechnologyTargetingTypeoperatingSystemsArray = Array of TOperatingSystem;
  TTechnologyTargetingTypeplatformTypesArray = Array of TPlatformType;
  TUserProfileListTypeitemsArray = Array of TUserProfile;
  TUserRoleTypepermissionsArray = Array of TUserRolePermission;
  TUserRolePermissionGroupsListResponseTypeuserRolePermissionGroupsArray = Array of TUserRolePermissionGroup;
  TUserRolePermissionsListResponseTypeuserRolePermissionsArray = Array of TUserRolePermission;
  TUserRolesListResponseTypeuserRolesArray = Array of TUserRole;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FaccountPermissionIds : TStringArray;
    FaccountProfile : String;
    Factive : boolean;
    FactiveAdsLimitTier : String;
    FactiveViewOptOut : boolean;
    FavailablePermissionIds : TStringArray;
    FcomscoreVceEnabled : boolean;
    FcountryId : String;
    FcurrencyId : String;
    FdefaultCreativeSizeId : String;
    Fdescription : String;
    Fid : String;
    Fkind : String;
    Flocale : String;
    FmaximumImageSize : String;
    Fname : String;
    FnielsenOcrEnabled : boolean;
    FreportsConfiguration : TReportsConfiguration;
    FteaserSizeLimit : String;
  Protected
    //Property setters
    Procedure SetaccountPermissionIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetaccountProfile(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetactiveAdsLimitTier(AIndex : Integer; AValue : String); virtual;
    Procedure SetactiveViewOptOut(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetavailablePermissionIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetcomscoreVceEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcountryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrencyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultCreativeSizeId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaximumImageSize(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetnielsenOcrEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetreportsConfiguration(AIndex : Integer; AValue : TReportsConfiguration); virtual;
    Procedure SetteaserSizeLimit(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountPermissionIds : TStringArray Index 0 Read FaccountPermissionIds Write SetaccountPermissionIds;
    Property accountProfile : String Index 8 Read FaccountProfile Write SetaccountProfile;
    Property active : boolean Index 16 Read Factive Write Setactive;
    Property activeAdsLimitTier : String Index 24 Read FactiveAdsLimitTier Write SetactiveAdsLimitTier;
    Property activeViewOptOut : boolean Index 32 Read FactiveViewOptOut Write SetactiveViewOptOut;
    Property availablePermissionIds : TStringArray Index 40 Read FavailablePermissionIds Write SetavailablePermissionIds;
    Property comscoreVceEnabled : boolean Index 48 Read FcomscoreVceEnabled Write SetcomscoreVceEnabled;
    Property countryId : String Index 56 Read FcountryId Write SetcountryId;
    Property currencyId : String Index 64 Read FcurrencyId Write SetcurrencyId;
    Property defaultCreativeSizeId : String Index 72 Read FdefaultCreativeSizeId Write SetdefaultCreativeSizeId;
    Property description : String Index 80 Read Fdescription Write Setdescription;
    Property id : String Index 88 Read Fid Write Setid;
    Property kind : String Index 96 Read Fkind Write Setkind;
    Property locale : String Index 104 Read Flocale Write Setlocale;
    Property maximumImageSize : String Index 112 Read FmaximumImageSize Write SetmaximumImageSize;
    Property name : String Index 120 Read Fname Write Setname;
    Property nielsenOcrEnabled : boolean Index 128 Read FnielsenOcrEnabled Write SetnielsenOcrEnabled;
    Property reportsConfiguration : TReportsConfiguration Index 136 Read FreportsConfiguration Write SetreportsConfiguration;
    Property teaserSizeLimit : String Index 144 Read FteaserSizeLimit Write SetteaserSizeLimit;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountActiveAdSummary
    --------------------------------------------------------------------}
  
  TAccountActiveAdSummary = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FactiveAds : String;
    FactiveAdsLimitTier : String;
    FavailableAds : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetactiveAds(AIndex : Integer; AValue : String); virtual;
    Procedure SetactiveAdsLimitTier(AIndex : Integer; AValue : String); virtual;
    Procedure SetavailableAds(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property activeAds : String Index 8 Read FactiveAds Write SetactiveAds;
    Property activeAdsLimitTier : String Index 16 Read FactiveAdsLimitTier Write SetactiveAdsLimitTier;
    Property availableAds : String Index 24 Read FavailableAds Write SetavailableAds;
    Property kind : String Index 32 Read Fkind Write Setkind;
  end;
  TAccountActiveAdSummaryClass = Class of TAccountActiveAdSummary;
  
  { --------------------------------------------------------------------
    TAccountPermission
    --------------------------------------------------------------------}
  
  TAccountPermission = Class(TGoogleBaseObject)
  Private
    FaccountProfiles : TStringArray;
    Fid : String;
    Fkind : String;
    Flevel : String;
    Fname : String;
    FpermissionGroupId : String;
  Protected
    //Property setters
    Procedure SetaccountProfiles(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpermissionGroupId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountProfiles : TStringArray Index 0 Read FaccountProfiles Write SetaccountProfiles;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property level : String Index 24 Read Flevel Write Setlevel;
    Property name : String Index 32 Read Fname Write Setname;
    Property permissionGroupId : String Index 40 Read FpermissionGroupId Write SetpermissionGroupId;
  end;
  TAccountPermissionClass = Class of TAccountPermission;
  
  { --------------------------------------------------------------------
    TAccountPermissionGroup
    --------------------------------------------------------------------}
  
  TAccountPermissionGroup = Class(TGoogleBaseObject)
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
  TAccountPermissionGroupClass = Class of TAccountPermissionGroup;
  
  { --------------------------------------------------------------------
    TAccountPermissionGroupsListResponse
    --------------------------------------------------------------------}
  
  TAccountPermissionGroupsListResponse = Class(TGoogleBaseObject)
  Private
    FaccountPermissionGroups : TAccountPermissionGroupsListResponseTypeaccountPermissionGroupsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountPermissionGroups(AIndex : Integer; AValue : TAccountPermissionGroupsListResponseTypeaccountPermissionGroupsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountPermissionGroups : TAccountPermissionGroupsListResponseTypeaccountPermissionGroupsArray Index 0 Read FaccountPermissionGroups Write SetaccountPermissionGroups;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccountPermissionGroupsListResponseClass = Class of TAccountPermissionGroupsListResponse;
  
  { --------------------------------------------------------------------
    TAccountPermissionsListResponse
    --------------------------------------------------------------------}
  
  TAccountPermissionsListResponse = Class(TGoogleBaseObject)
  Private
    FaccountPermissions : TAccountPermissionsListResponseTypeaccountPermissionsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountPermissions(AIndex : Integer; AValue : TAccountPermissionsListResponseTypeaccountPermissionsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountPermissions : TAccountPermissionsListResponseTypeaccountPermissionsArray Index 0 Read FaccountPermissions Write SetaccountPermissions;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccountPermissionsListResponseClass = Class of TAccountPermissionsListResponse;
  
  { --------------------------------------------------------------------
    TAccountUserProfile
    --------------------------------------------------------------------}
  
  TAccountUserProfile = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Factive : boolean;
    FadvertiserFilter : TObjectFilter;
    FcampaignFilter : TObjectFilter;
    Fcomments : String;
    Femail : String;
    Fid : String;
    Fkind : String;
    Flocale : String;
    Fname : String;
    FsiteFilter : TObjectFilter;
    FsubaccountId : String;
    FtraffickerType : String;
    FuserAccessType : String;
    FuserRoleFilter : TObjectFilter;
    FuserRoleId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadvertiserFilter(AIndex : Integer; AValue : TObjectFilter); virtual;
    Procedure SetcampaignFilter(AIndex : Integer; AValue : TObjectFilter); virtual;
    Procedure Setcomments(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteFilter(AIndex : Integer; AValue : TObjectFilter); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettraffickerType(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserAccessType(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserRoleFilter(AIndex : Integer; AValue : TObjectFilter); virtual;
    Procedure SetuserRoleId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property advertiserFilter : TObjectFilter Index 16 Read FadvertiserFilter Write SetadvertiserFilter;
    Property campaignFilter : TObjectFilter Index 24 Read FcampaignFilter Write SetcampaignFilter;
    Property comments : String Index 32 Read Fcomments Write Setcomments;
    Property email : String Index 40 Read Femail Write Setemail;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property locale : String Index 64 Read Flocale Write Setlocale;
    Property name : String Index 72 Read Fname Write Setname;
    Property siteFilter : TObjectFilter Index 80 Read FsiteFilter Write SetsiteFilter;
    Property subaccountId : String Index 88 Read FsubaccountId Write SetsubaccountId;
    Property traffickerType : String Index 96 Read FtraffickerType Write SettraffickerType;
    Property userAccessType : String Index 104 Read FuserAccessType Write SetuserAccessType;
    Property userRoleFilter : TObjectFilter Index 112 Read FuserRoleFilter Write SetuserRoleFilter;
    Property userRoleId : String Index 120 Read FuserRoleId Write SetuserRoleId;
  end;
  TAccountUserProfileClass = Class of TAccountUserProfile;
  
  { --------------------------------------------------------------------
    TAccountUserProfilesListResponse
    --------------------------------------------------------------------}
  
  TAccountUserProfilesListResponse = Class(TGoogleBaseObject)
  Private
    FaccountUserProfiles : TAccountUserProfilesListResponseTypeaccountUserProfilesArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetaccountUserProfiles(AIndex : Integer; AValue : TAccountUserProfilesListResponseTypeaccountUserProfilesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountUserProfiles : TAccountUserProfilesListResponseTypeaccountUserProfilesArray Index 0 Read FaccountUserProfiles Write SetaccountUserProfiles;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAccountUserProfilesListResponseClass = Class of TAccountUserProfilesListResponse;
  
  { --------------------------------------------------------------------
    TAccountsListResponse
    --------------------------------------------------------------------}
  
  TAccountsListResponse = Class(TGoogleBaseObject)
  Private
    Faccounts : TAccountsListResponseTypeaccountsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setaccounts(AIndex : Integer; AValue : TAccountsListResponseTypeaccountsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accounts : TAccountsListResponseTypeaccountsArray Index 0 Read Faccounts Write Setaccounts;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAccountsListResponseClass = Class of TAccountsListResponse;
  
  { --------------------------------------------------------------------
    TActivities
    --------------------------------------------------------------------}
  
  TActivities = Class(TGoogleBaseObject)
  Private
    Ffilters : TActivitiesTypefiltersArray;
    Fkind : String;
    FmetricNames : TStringArray;
  Protected
    //Property setters
    Procedure Setfilters(AIndex : Integer; AValue : TActivitiesTypefiltersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmetricNames(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property filters : TActivitiesTypefiltersArray Index 0 Read Ffilters Write Setfilters;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property metricNames : TStringArray Index 16 Read FmetricNames Write SetmetricNames;
  end;
  TActivitiesClass = Class of TActivities;
  
  { --------------------------------------------------------------------
    TAd
    --------------------------------------------------------------------}
  
  TAd = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Factive : boolean;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    Farchived : boolean;
    FaudienceSegmentId : String;
    FcampaignId : String;
    FcampaignIdDimensionValue : TDimensionValue;
    FclickThroughUrl : TClickThroughUrl;
    FclickThroughUrlSuffixProperties : TClickThroughUrlSuffixProperties;
    Fcomments : String;
    Fcompatibility : String;
    FcreateInfo : TLastModifiedInfo;
    FcreativeGroupAssignments : TAdTypecreativeGroupAssignmentsArray;
    FcreativeRotation : TCreativeRotation;
    FdayPartTargeting : TDayPartTargeting;
    FdefaultClickThroughEventTagProperties : TDefaultClickThroughEventTagProperties;
    FdeliverySchedule : TDeliverySchedule;
    FdynamicClickTracker : boolean;
    FendTime : TDatetime;
    FeventTagOverrides : TAdTypeeventTagOverridesArray;
    FgeoTargeting : TGeoTargeting;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    FkeyValueTargetingExpression : TKeyValueTargetingExpression;
    Fkind : String;
    FlastModifiedInfo : TLastModifiedInfo;
    Fname : String;
    FplacementAssignments : TAdTypeplacementAssignmentsArray;
    Fremarketing_list_expression : TListTargetingExpression;
    Fsize : TSize;
    FsslCompliant : boolean;
    FsslRequired : boolean;
    FstartTime : TDatetime;
    FsubaccountId : String;
    FtechnologyTargeting : TTechnologyTargeting;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setarchived(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetaudienceSegmentId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcampaignId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcampaignIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetclickThroughUrl(AIndex : Integer; AValue : TClickThroughUrl); virtual;
    Procedure SetclickThroughUrlSuffixProperties(AIndex : Integer; AValue : TClickThroughUrlSuffixProperties); virtual;
    Procedure Setcomments(AIndex : Integer; AValue : String); virtual;
    Procedure Setcompatibility(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreateInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SetcreativeGroupAssignments(AIndex : Integer; AValue : TAdTypecreativeGroupAssignmentsArray); virtual;
    Procedure SetcreativeRotation(AIndex : Integer; AValue : TCreativeRotation); virtual;
    Procedure SetdayPartTargeting(AIndex : Integer; AValue : TDayPartTargeting); virtual;
    Procedure SetdefaultClickThroughEventTagProperties(AIndex : Integer; AValue : TDefaultClickThroughEventTagProperties); virtual;
    Procedure SetdeliverySchedule(AIndex : Integer; AValue : TDeliverySchedule); virtual;
    Procedure SetdynamicClickTracker(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SeteventTagOverrides(AIndex : Integer; AValue : TAdTypeeventTagOverridesArray); virtual;
    Procedure SetgeoTargeting(AIndex : Integer; AValue : TGeoTargeting); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetkeyValueTargetingExpression(AIndex : Integer; AValue : TKeyValueTargetingExpression); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementAssignments(AIndex : Integer; AValue : TAdTypeplacementAssignmentsArray); virtual;
    Procedure Setremarketing_list_expression(AIndex : Integer; AValue : TListTargetingExpression); virtual;
    Procedure Setsize(AIndex : Integer; AValue : TSize); virtual;
    Procedure SetsslCompliant(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsslRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettechnologyTargeting(AIndex : Integer; AValue : TTechnologyTargeting); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property advertiserId : String Index 16 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 24 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property archived : boolean Index 32 Read Farchived Write Setarchived;
    Property audienceSegmentId : String Index 40 Read FaudienceSegmentId Write SetaudienceSegmentId;
    Property campaignId : String Index 48 Read FcampaignId Write SetcampaignId;
    Property campaignIdDimensionValue : TDimensionValue Index 56 Read FcampaignIdDimensionValue Write SetcampaignIdDimensionValue;
    Property clickThroughUrl : TClickThroughUrl Index 64 Read FclickThroughUrl Write SetclickThroughUrl;
    Property clickThroughUrlSuffixProperties : TClickThroughUrlSuffixProperties Index 72 Read FclickThroughUrlSuffixProperties Write SetclickThroughUrlSuffixProperties;
    Property comments : String Index 80 Read Fcomments Write Setcomments;
    Property compatibility : String Index 88 Read Fcompatibility Write Setcompatibility;
    Property createInfo : TLastModifiedInfo Index 96 Read FcreateInfo Write SetcreateInfo;
    Property creativeGroupAssignments : TAdTypecreativeGroupAssignmentsArray Index 104 Read FcreativeGroupAssignments Write SetcreativeGroupAssignments;
    Property creativeRotation : TCreativeRotation Index 112 Read FcreativeRotation Write SetcreativeRotation;
    Property dayPartTargeting : TDayPartTargeting Index 120 Read FdayPartTargeting Write SetdayPartTargeting;
    Property defaultClickThroughEventTagProperties : TDefaultClickThroughEventTagProperties Index 128 Read FdefaultClickThroughEventTagProperties Write SetdefaultClickThroughEventTagProperties;
    Property deliverySchedule : TDeliverySchedule Index 136 Read FdeliverySchedule Write SetdeliverySchedule;
    Property dynamicClickTracker : boolean Index 144 Read FdynamicClickTracker Write SetdynamicClickTracker;
    Property endTime : TDatetime Index 152 Read FendTime Write SetendTime;
    Property eventTagOverrides : TAdTypeeventTagOverridesArray Index 160 Read FeventTagOverrides Write SeteventTagOverrides;
    Property geoTargeting : TGeoTargeting Index 168 Read FgeoTargeting Write SetgeoTargeting;
    Property id : String Index 176 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 184 Read FidDimensionValue Write SetidDimensionValue;
    Property keyValueTargetingExpression : TKeyValueTargetingExpression Index 192 Read FkeyValueTargetingExpression Write SetkeyValueTargetingExpression;
    Property kind : String Index 200 Read Fkind Write Setkind;
    Property lastModifiedInfo : TLastModifiedInfo Index 208 Read FlastModifiedInfo Write SetlastModifiedInfo;
    Property name : String Index 216 Read Fname Write Setname;
    Property placementAssignments : TAdTypeplacementAssignmentsArray Index 224 Read FplacementAssignments Write SetplacementAssignments;
    Property remarketing_list_expression : TListTargetingExpression Index 232 Read Fremarketing_list_expression Write Setremarketing_list_expression;
    Property size : TSize Index 240 Read Fsize Write Setsize;
    Property sslCompliant : boolean Index 248 Read FsslCompliant Write SetsslCompliant;
    Property sslRequired : boolean Index 256 Read FsslRequired Write SetsslRequired;
    Property startTime : TDatetime Index 264 Read FstartTime Write SetstartTime;
    Property subaccountId : String Index 272 Read FsubaccountId Write SetsubaccountId;
    Property technologyTargeting : TTechnologyTargeting Index 280 Read FtechnologyTargeting Write SettechnologyTargeting;
    Property _type : String Index 288 Read F_type Write Set_type;
  end;
  TAdClass = Class of TAd;
  
  { --------------------------------------------------------------------
    TAdSlot
    --------------------------------------------------------------------}
  
  TAdSlot = Class(TGoogleBaseObject)
  Private
    Fcomment : String;
    Fcompatibility : String;
    Fheight : String;
    FlinkedPlacementId : String;
    Fname : String;
    FpaymentSourceType : String;
    Fprimary : boolean;
    Fwidth : String;
  Protected
    //Property setters
    Procedure Setcomment(AIndex : Integer; AValue : String); virtual;
    Procedure Setcompatibility(AIndex : Integer; AValue : String); virtual;
    Procedure Setheight(AIndex : Integer; AValue : String); virtual;
    Procedure SetlinkedPlacementId(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpaymentSourceType(AIndex : Integer; AValue : String); virtual;
    Procedure Setprimary(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property comment : String Index 0 Read Fcomment Write Setcomment;
    Property compatibility : String Index 8 Read Fcompatibility Write Setcompatibility;
    Property height : String Index 16 Read Fheight Write Setheight;
    Property linkedPlacementId : String Index 24 Read FlinkedPlacementId Write SetlinkedPlacementId;
    Property name : String Index 32 Read Fname Write Setname;
    Property paymentSourceType : String Index 40 Read FpaymentSourceType Write SetpaymentSourceType;
    Property primary : boolean Index 48 Read Fprimary Write Setprimary;
    Property width : String Index 56 Read Fwidth Write Setwidth;
  end;
  TAdSlotClass = Class of TAdSlot;
  
  { --------------------------------------------------------------------
    TAdsListResponse
    --------------------------------------------------------------------}
  
  TAdsListResponse = Class(TGoogleBaseObject)
  Private
    Fads : TAdsListResponseTypeadsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setads(AIndex : Integer; AValue : TAdsListResponseTypeadsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ads : TAdsListResponseTypeadsArray Index 0 Read Fads Write Setads;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdsListResponseClass = Class of TAdsListResponse;
  
  { --------------------------------------------------------------------
    TAdvertiser
    --------------------------------------------------------------------}
  
  TAdvertiser = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserGroupId : String;
    FclickThroughUrlSuffix : String;
    FdefaultClickThroughEventTagId : String;
    FdefaultEmail : String;
    FfloodlightConfigurationId : String;
    FfloodlightConfigurationIdDimensionValue : TDimensionValue;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    Fkind : String;
    Fname : String;
    ForiginalFloodlightConfigurationId : String;
    Fstatus : String;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetclickThroughUrlSuffix(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultClickThroughEventTagId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightConfigurationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightConfigurationIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoriginalFloodlightConfigurationId(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserGroupId : String Index 8 Read FadvertiserGroupId Write SetadvertiserGroupId;
    Property clickThroughUrlSuffix : String Index 16 Read FclickThroughUrlSuffix Write SetclickThroughUrlSuffix;
    Property defaultClickThroughEventTagId : String Index 24 Read FdefaultClickThroughEventTagId Write SetdefaultClickThroughEventTagId;
    Property defaultEmail : String Index 32 Read FdefaultEmail Write SetdefaultEmail;
    Property floodlightConfigurationId : String Index 40 Read FfloodlightConfigurationId Write SetfloodlightConfigurationId;
    Property floodlightConfigurationIdDimensionValue : TDimensionValue Index 48 Read FfloodlightConfigurationIdDimensionValue Write SetfloodlightConfigurationIdDimensionValue;
    Property id : String Index 56 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 64 Read FidDimensionValue Write SetidDimensionValue;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property name : String Index 80 Read Fname Write Setname;
    Property originalFloodlightConfigurationId : String Index 88 Read ForiginalFloodlightConfigurationId Write SetoriginalFloodlightConfigurationId;
    Property status : String Index 96 Read Fstatus Write Setstatus;
    Property subaccountId : String Index 104 Read FsubaccountId Write SetsubaccountId;
  end;
  TAdvertiserClass = Class of TAdvertiser;
  
  { --------------------------------------------------------------------
    TAdvertiserGroup
    --------------------------------------------------------------------}
  
  TAdvertiserGroup = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TAdvertiserGroupClass = Class of TAdvertiserGroup;
  
  { --------------------------------------------------------------------
    TAdvertiserGroupsListResponse
    --------------------------------------------------------------------}
  
  TAdvertiserGroupsListResponse = Class(TGoogleBaseObject)
  Private
    FadvertiserGroups : TAdvertiserGroupsListResponseTypeadvertiserGroupsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetadvertiserGroups(AIndex : Integer; AValue : TAdvertiserGroupsListResponseTypeadvertiserGroupsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property advertiserGroups : TAdvertiserGroupsListResponseTypeadvertiserGroupsArray Index 0 Read FadvertiserGroups Write SetadvertiserGroups;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdvertiserGroupsListResponseClass = Class of TAdvertiserGroupsListResponse;
  
  { --------------------------------------------------------------------
    TAdvertisersListResponse
    --------------------------------------------------------------------}
  
  TAdvertisersListResponse = Class(TGoogleBaseObject)
  Private
    Fadvertisers : TAdvertisersListResponseTypeadvertisersArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setadvertisers(AIndex : Integer; AValue : TAdvertisersListResponseTypeadvertisersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property advertisers : TAdvertisersListResponseTypeadvertisersArray Index 0 Read Fadvertisers Write Setadvertisers;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdvertisersListResponseClass = Class of TAdvertisersListResponse;
  
  { --------------------------------------------------------------------
    TAudienceSegment
    --------------------------------------------------------------------}
  
  TAudienceSegment = Class(TGoogleBaseObject)
  Private
    Fallocation : integer;
    Fid : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setallocation(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property allocation : integer Index 0 Read Fallocation Write Setallocation;
    Property id : String Index 8 Read Fid Write Setid;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TAudienceSegmentClass = Class of TAudienceSegment;
  
  { --------------------------------------------------------------------
    TAudienceSegmentGroup
    --------------------------------------------------------------------}
  
  TAudienceSegmentGroup = Class(TGoogleBaseObject)
  Private
    FaudienceSegments : TAudienceSegmentGroupTypeaudienceSegmentsArray;
    Fid : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetaudienceSegments(AIndex : Integer; AValue : TAudienceSegmentGroupTypeaudienceSegmentsArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property audienceSegments : TAudienceSegmentGroupTypeaudienceSegmentsArray Index 0 Read FaudienceSegments Write SetaudienceSegments;
    Property id : String Index 8 Read Fid Write Setid;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TAudienceSegmentGroupClass = Class of TAudienceSegmentGroup;
  
  { --------------------------------------------------------------------
    TBrowser
    --------------------------------------------------------------------}
  
  TBrowser = Class(TGoogleBaseObject)
  Private
    FbrowserVersionId : String;
    FdartId : String;
    Fkind : String;
    FmajorVersion : String;
    FminorVersion : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetbrowserVersionId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdartId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmajorVersion(AIndex : Integer; AValue : String); virtual;
    Procedure SetminorVersion(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property browserVersionId : String Index 0 Read FbrowserVersionId Write SetbrowserVersionId;
    Property dartId : String Index 8 Read FdartId Write SetdartId;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property majorVersion : String Index 24 Read FmajorVersion Write SetmajorVersion;
    Property minorVersion : String Index 32 Read FminorVersion Write SetminorVersion;
    Property name : String Index 40 Read Fname Write Setname;
  end;
  TBrowserClass = Class of TBrowser;
  
  { --------------------------------------------------------------------
    TBrowsersListResponse
    --------------------------------------------------------------------}
  
  TBrowsersListResponse = Class(TGoogleBaseObject)
  Private
    Fbrowsers : TBrowsersListResponseTypebrowsersArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setbrowsers(AIndex : Integer; AValue : TBrowsersListResponseTypebrowsersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property browsers : TBrowsersListResponseTypebrowsersArray Index 0 Read Fbrowsers Write Setbrowsers;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TBrowsersListResponseClass = Class of TBrowsersListResponse;
  
  { --------------------------------------------------------------------
    TCampaign
    --------------------------------------------------------------------}
  
  TCampaign = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadditionalCreativeOptimizationConfigurations : TCampaignTypeadditionalCreativeOptimizationConfigurationsArray;
    FadvertiserGroupId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    Farchived : boolean;
    FaudienceSegmentGroups : TCampaignTypeaudienceSegmentGroupsArray;
    FbillingInvoiceCode : String;
    FclickThroughUrlSuffixProperties : TClickThroughUrlSuffixProperties;
    Fcomment : String;
    FcomscoreVceEnabled : boolean;
    FcreateInfo : TLastModifiedInfo;
    FcreativeGroupIds : TStringArray;
    FcreativeOptimizationConfiguration : TCreativeOptimizationConfiguration;
    FdefaultClickThroughEventTagProperties : TDefaultClickThroughEventTagProperties;
    FendDate : TDate;
    FeventTagOverrides : TCampaignTypeeventTagOverridesArray;
    FexternalId : String;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    Fkind : String;
    FlastModifiedInfo : TLastModifiedInfo;
    FlookbackConfiguration : TLookbackConfiguration;
    Fname : String;
    FnielsenOcrEnabled : boolean;
    FstartDate : TDate;
    FsubaccountId : String;
    FtraffickerEmails : TStringArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadditionalCreativeOptimizationConfigurations(AIndex : Integer; AValue : TCampaignTypeadditionalCreativeOptimizationConfigurationsArray); virtual;
    Procedure SetadvertiserGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setarchived(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetaudienceSegmentGroups(AIndex : Integer; AValue : TCampaignTypeaudienceSegmentGroupsArray); virtual;
    Procedure SetbillingInvoiceCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetclickThroughUrlSuffixProperties(AIndex : Integer; AValue : TClickThroughUrlSuffixProperties); virtual;
    Procedure Setcomment(AIndex : Integer; AValue : String); virtual;
    Procedure SetcomscoreVceEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcreateInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SetcreativeGroupIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetcreativeOptimizationConfiguration(AIndex : Integer; AValue : TCreativeOptimizationConfiguration); virtual;
    Procedure SetdefaultClickThroughEventTagProperties(AIndex : Integer; AValue : TDefaultClickThroughEventTagProperties); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure SeteventTagOverrides(AIndex : Integer; AValue : TCampaignTypeeventTagOverridesArray); virtual;
    Procedure SetexternalId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetnielsenOcrEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettraffickerEmails(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property additionalCreativeOptimizationConfigurations : TCampaignTypeadditionalCreativeOptimizationConfigurationsArray Index 8 Read FadditionalCreativeOptimizationConfigurations Write SetadditionalCreativeOptimizationConfigurations;
    Property advertiserGroupId : String Index 16 Read FadvertiserGroupId Write SetadvertiserGroupId;
    Property advertiserId : String Index 24 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 32 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property archived : boolean Index 40 Read Farchived Write Setarchived;
    Property audienceSegmentGroups : TCampaignTypeaudienceSegmentGroupsArray Index 48 Read FaudienceSegmentGroups Write SetaudienceSegmentGroups;
    Property billingInvoiceCode : String Index 56 Read FbillingInvoiceCode Write SetbillingInvoiceCode;
    Property clickThroughUrlSuffixProperties : TClickThroughUrlSuffixProperties Index 64 Read FclickThroughUrlSuffixProperties Write SetclickThroughUrlSuffixProperties;
    Property comment : String Index 72 Read Fcomment Write Setcomment;
    Property comscoreVceEnabled : boolean Index 80 Read FcomscoreVceEnabled Write SetcomscoreVceEnabled;
    Property createInfo : TLastModifiedInfo Index 88 Read FcreateInfo Write SetcreateInfo;
    Property creativeGroupIds : TStringArray Index 96 Read FcreativeGroupIds Write SetcreativeGroupIds;
    Property creativeOptimizationConfiguration : TCreativeOptimizationConfiguration Index 104 Read FcreativeOptimizationConfiguration Write SetcreativeOptimizationConfiguration;
    Property defaultClickThroughEventTagProperties : TDefaultClickThroughEventTagProperties Index 112 Read FdefaultClickThroughEventTagProperties Write SetdefaultClickThroughEventTagProperties;
    Property endDate : TDate Index 120 Read FendDate Write SetendDate;
    Property eventTagOverrides : TCampaignTypeeventTagOverridesArray Index 128 Read FeventTagOverrides Write SeteventTagOverrides;
    Property externalId : String Index 136 Read FexternalId Write SetexternalId;
    Property id : String Index 144 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 152 Read FidDimensionValue Write SetidDimensionValue;
    Property kind : String Index 160 Read Fkind Write Setkind;
    Property lastModifiedInfo : TLastModifiedInfo Index 168 Read FlastModifiedInfo Write SetlastModifiedInfo;
    Property lookbackConfiguration : TLookbackConfiguration Index 176 Read FlookbackConfiguration Write SetlookbackConfiguration;
    Property name : String Index 184 Read Fname Write Setname;
    Property nielsenOcrEnabled : boolean Index 192 Read FnielsenOcrEnabled Write SetnielsenOcrEnabled;
    Property startDate : TDate Index 200 Read FstartDate Write SetstartDate;
    Property subaccountId : String Index 208 Read FsubaccountId Write SetsubaccountId;
    Property traffickerEmails : TStringArray Index 216 Read FtraffickerEmails Write SettraffickerEmails;
  end;
  TCampaignClass = Class of TCampaign;
  
  { --------------------------------------------------------------------
    TCampaignCreativeAssociation
    --------------------------------------------------------------------}
  
  TCampaignCreativeAssociation = Class(TGoogleBaseObject)
  Private
    FcreativeId : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetcreativeId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creativeId : String Index 0 Read FcreativeId Write SetcreativeId;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCampaignCreativeAssociationClass = Class of TCampaignCreativeAssociation;
  
  { --------------------------------------------------------------------
    TCampaignCreativeAssociationsListResponse
    --------------------------------------------------------------------}
  
  TCampaignCreativeAssociationsListResponse = Class(TGoogleBaseObject)
  Private
    FcampaignCreativeAssociations : TCampaignCreativeAssociationsListResponseTypecampaignCreativeAssociationsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetcampaignCreativeAssociations(AIndex : Integer; AValue : TCampaignCreativeAssociationsListResponseTypecampaignCreativeAssociationsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property campaignCreativeAssociations : TCampaignCreativeAssociationsListResponseTypecampaignCreativeAssociationsArray Index 0 Read FcampaignCreativeAssociations Write SetcampaignCreativeAssociations;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCampaignCreativeAssociationsListResponseClass = Class of TCampaignCreativeAssociationsListResponse;
  
  { --------------------------------------------------------------------
    TCampaignsListResponse
    --------------------------------------------------------------------}
  
  TCampaignsListResponse = Class(TGoogleBaseObject)
  Private
    Fcampaigns : TCampaignsListResponseTypecampaignsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setcampaigns(AIndex : Integer; AValue : TCampaignsListResponseTypecampaignsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property campaigns : TCampaignsListResponseTypecampaignsArray Index 0 Read Fcampaigns Write Setcampaigns;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCampaignsListResponseClass = Class of TCampaignsListResponse;
  
  { --------------------------------------------------------------------
    TChangeLog
    --------------------------------------------------------------------}
  
  TChangeLog = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Faction : String;
    FchangeTime : TDatetime;
    FfieldName : String;
    Fid : String;
    Fkind : String;
    FnewValue : String;
    FobjectId : String;
    FobjectType : String;
    FoldValue : String;
    FsubaccountId : String;
    FtransactionId : String;
    FuserProfileId : String;
    FuserProfileName : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setaction(AIndex : Integer; AValue : String); virtual;
    Procedure SetchangeTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetfieldName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnewValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : String); virtual;
    Procedure SetoldValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettransactionId(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserProfileId(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserProfileName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property action : String Index 8 Read Faction Write Setaction;
    Property changeTime : TDatetime Index 16 Read FchangeTime Write SetchangeTime;
    Property fieldName : String Index 24 Read FfieldName Write SetfieldName;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property newValue : String Index 48 Read FnewValue Write SetnewValue;
    Property objectId : String Index 56 Read FobjectId Write SetobjectId;
    Property objectType : String Index 64 Read FobjectType Write SetobjectType;
    Property oldValue : String Index 72 Read FoldValue Write SetoldValue;
    Property subaccountId : String Index 80 Read FsubaccountId Write SetsubaccountId;
    Property transactionId : String Index 88 Read FtransactionId Write SettransactionId;
    Property userProfileId : String Index 96 Read FuserProfileId Write SetuserProfileId;
    Property userProfileName : String Index 104 Read FuserProfileName Write SetuserProfileName;
  end;
  TChangeLogClass = Class of TChangeLog;
  
  { --------------------------------------------------------------------
    TChangeLogsListResponse
    --------------------------------------------------------------------}
  
  TChangeLogsListResponse = Class(TGoogleBaseObject)
  Private
    FchangeLogs : TChangeLogsListResponseTypechangeLogsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetchangeLogs(AIndex : Integer; AValue : TChangeLogsListResponseTypechangeLogsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property changeLogs : TChangeLogsListResponseTypechangeLogsArray Index 0 Read FchangeLogs Write SetchangeLogs;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TChangeLogsListResponseClass = Class of TChangeLogsListResponse;
  
  { --------------------------------------------------------------------
    TCitiesListResponse
    --------------------------------------------------------------------}
  
  TCitiesListResponse = Class(TGoogleBaseObject)
  Private
    Fcities : TCitiesListResponseTypecitiesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setcities(AIndex : Integer; AValue : TCitiesListResponseTypecitiesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property cities : TCitiesListResponseTypecitiesArray Index 0 Read Fcities Write Setcities;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCitiesListResponseClass = Class of TCitiesListResponse;
  
  { --------------------------------------------------------------------
    TCity
    --------------------------------------------------------------------}
  
  TCity = Class(TGoogleBaseObject)
  Private
    FcountryCode : String;
    FcountryDartId : String;
    FdartId : String;
    Fkind : String;
    FmetroCode : String;
    FmetroDmaId : String;
    Fname : String;
    FregionCode : String;
    FregionDartId : String;
  Protected
    //Property setters
    Procedure SetcountryCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountryDartId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdartId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmetroCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetmetroDmaId(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetregionCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetregionDartId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property countryCode : String Index 0 Read FcountryCode Write SetcountryCode;
    Property countryDartId : String Index 8 Read FcountryDartId Write SetcountryDartId;
    Property dartId : String Index 16 Read FdartId Write SetdartId;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property metroCode : String Index 32 Read FmetroCode Write SetmetroCode;
    Property metroDmaId : String Index 40 Read FmetroDmaId Write SetmetroDmaId;
    Property name : String Index 48 Read Fname Write Setname;
    Property regionCode : String Index 56 Read FregionCode Write SetregionCode;
    Property regionDartId : String Index 64 Read FregionDartId Write SetregionDartId;
  end;
  TCityClass = Class of TCity;
  
  { --------------------------------------------------------------------
    TClickTag
    --------------------------------------------------------------------}
  
  TClickTag = Class(TGoogleBaseObject)
  Private
    FeventName : String;
    Fname : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure SeteventName(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property eventName : String Index 0 Read FeventName Write SeteventName;
    Property name : String Index 8 Read Fname Write Setname;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TClickTagClass = Class of TClickTag;
  
  { --------------------------------------------------------------------
    TClickThroughUrl
    --------------------------------------------------------------------}
  
  TClickThroughUrl = Class(TGoogleBaseObject)
  Private
    FcustomClickThroughUrl : String;
    FdefaultLandingPage : boolean;
    FlandingPageId : String;
  Protected
    //Property setters
    Procedure SetcustomClickThroughUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLandingPage(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlandingPageId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property customClickThroughUrl : String Index 0 Read FcustomClickThroughUrl Write SetcustomClickThroughUrl;
    Property defaultLandingPage : boolean Index 8 Read FdefaultLandingPage Write SetdefaultLandingPage;
    Property landingPageId : String Index 16 Read FlandingPageId Write SetlandingPageId;
  end;
  TClickThroughUrlClass = Class of TClickThroughUrl;
  
  { --------------------------------------------------------------------
    TClickThroughUrlSuffixProperties
    --------------------------------------------------------------------}
  
  TClickThroughUrlSuffixProperties = Class(TGoogleBaseObject)
  Private
    FclickThroughUrlSuffix : String;
    FoverrideInheritedSuffix : boolean;
  Protected
    //Property setters
    Procedure SetclickThroughUrlSuffix(AIndex : Integer; AValue : String); virtual;
    Procedure SetoverrideInheritedSuffix(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property clickThroughUrlSuffix : String Index 0 Read FclickThroughUrlSuffix Write SetclickThroughUrlSuffix;
    Property overrideInheritedSuffix : boolean Index 8 Read FoverrideInheritedSuffix Write SetoverrideInheritedSuffix;
  end;
  TClickThroughUrlSuffixPropertiesClass = Class of TClickThroughUrlSuffixProperties;
  
  { --------------------------------------------------------------------
    TCompanionClickThroughOverride
    --------------------------------------------------------------------}
  
  TCompanionClickThroughOverride = Class(TGoogleBaseObject)
  Private
    FclickThroughUrl : TClickThroughUrl;
    FcreativeId : String;
  Protected
    //Property setters
    Procedure SetclickThroughUrl(AIndex : Integer; AValue : TClickThroughUrl); virtual;
    Procedure SetcreativeId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property clickThroughUrl : TClickThroughUrl Index 0 Read FclickThroughUrl Write SetclickThroughUrl;
    Property creativeId : String Index 8 Read FcreativeId Write SetcreativeId;
  end;
  TCompanionClickThroughOverrideClass = Class of TCompanionClickThroughOverride;
  
  { --------------------------------------------------------------------
    TCompatibleFields
    --------------------------------------------------------------------}
  
  TCompatibleFields = Class(TGoogleBaseObject)
  Private
    FcrossDimensionReachReportCompatibleFields : TCrossDimensionReachReportCompatibleFields;
    FfloodlightReportCompatibleFields : TFloodlightReportCompatibleFields;
    Fkind : String;
    FpathToConversionReportCompatibleFields : TPathToConversionReportCompatibleFields;
    FreachReportCompatibleFields : TReachReportCompatibleFields;
    FreportCompatibleFields : TReportCompatibleFields;
  Protected
    //Property setters
    Procedure SetcrossDimensionReachReportCompatibleFields(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFields); virtual;
    Procedure SetfloodlightReportCompatibleFields(AIndex : Integer; AValue : TFloodlightReportCompatibleFields); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpathToConversionReportCompatibleFields(AIndex : Integer; AValue : TPathToConversionReportCompatibleFields); virtual;
    Procedure SetreachReportCompatibleFields(AIndex : Integer; AValue : TReachReportCompatibleFields); virtual;
    Procedure SetreportCompatibleFields(AIndex : Integer; AValue : TReportCompatibleFields); virtual;
  Public
  Published
    Property crossDimensionReachReportCompatibleFields : TCrossDimensionReachReportCompatibleFields Index 0 Read FcrossDimensionReachReportCompatibleFields Write SetcrossDimensionReachReportCompatibleFields;
    Property floodlightReportCompatibleFields : TFloodlightReportCompatibleFields Index 8 Read FfloodlightReportCompatibleFields Write SetfloodlightReportCompatibleFields;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property pathToConversionReportCompatibleFields : TPathToConversionReportCompatibleFields Index 24 Read FpathToConversionReportCompatibleFields Write SetpathToConversionReportCompatibleFields;
    Property reachReportCompatibleFields : TReachReportCompatibleFields Index 32 Read FreachReportCompatibleFields Write SetreachReportCompatibleFields;
    Property reportCompatibleFields : TReportCompatibleFields Index 40 Read FreportCompatibleFields Write SetreportCompatibleFields;
  end;
  TCompatibleFieldsClass = Class of TCompatibleFields;
  
  { --------------------------------------------------------------------
    TConnectionType
    --------------------------------------------------------------------}
  
  TConnectionType = Class(TGoogleBaseObject)
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
  TConnectionTypeClass = Class of TConnectionType;
  
  { --------------------------------------------------------------------
    TConnectionTypesListResponse
    --------------------------------------------------------------------}
  
  TConnectionTypesListResponse = Class(TGoogleBaseObject)
  Private
    FconnectionTypes : TConnectionTypesListResponseTypeconnectionTypesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetconnectionTypes(AIndex : Integer; AValue : TConnectionTypesListResponseTypeconnectionTypesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property connectionTypes : TConnectionTypesListResponseTypeconnectionTypesArray Index 0 Read FconnectionTypes Write SetconnectionTypes;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TConnectionTypesListResponseClass = Class of TConnectionTypesListResponse;
  
  { --------------------------------------------------------------------
    TContentCategoriesListResponse
    --------------------------------------------------------------------}
  
  TContentCategoriesListResponse = Class(TGoogleBaseObject)
  Private
    FcontentCategories : TContentCategoriesListResponseTypecontentCategoriesArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetcontentCategories(AIndex : Integer; AValue : TContentCategoriesListResponseTypecontentCategoriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property contentCategories : TContentCategoriesListResponseTypecontentCategoriesArray Index 0 Read FcontentCategories Write SetcontentCategories;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TContentCategoriesListResponseClass = Class of TContentCategoriesListResponse;
  
  { --------------------------------------------------------------------
    TContentCategory
    --------------------------------------------------------------------}
  
  TContentCategory = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TContentCategoryClass = Class of TContentCategory;
  
  { --------------------------------------------------------------------
    TCountriesListResponse
    --------------------------------------------------------------------}
  
  TCountriesListResponse = Class(TGoogleBaseObject)
  Private
    Fcountries : TCountriesListResponseTypecountriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setcountries(AIndex : Integer; AValue : TCountriesListResponseTypecountriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property countries : TCountriesListResponseTypecountriesArray Index 0 Read Fcountries Write Setcountries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCountriesListResponseClass = Class of TCountriesListResponse;
  
  { --------------------------------------------------------------------
    TCountry
    --------------------------------------------------------------------}
  
  TCountry = Class(TGoogleBaseObject)
  Private
    FcountryCode : String;
    FdartId : String;
    Fkind : String;
    Fname : String;
    FsslEnabled : boolean;
  Protected
    //Property setters
    Procedure SetcountryCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetdartId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsslEnabled(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property countryCode : String Index 0 Read FcountryCode Write SetcountryCode;
    Property dartId : String Index 8 Read FdartId Write SetdartId;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
    Property sslEnabled : boolean Index 32 Read FsslEnabled Write SetsslEnabled;
  end;
  TCountryClass = Class of TCountry;
  
  { --------------------------------------------------------------------
    TCreative
    --------------------------------------------------------------------}
  
  TCreative = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Factive : boolean;
    FadParameters : String;
    FadTagKeys : TStringArray;
    FadvertiserId : String;
    FallowScriptAccess : boolean;
    Farchived : boolean;
    FartworkType : String;
    FauthoringTool : String;
    Fauto_advance_images : boolean;
    FbackgroundColor : String;
    FbackupImageClickThroughUrl : String;
    FbackupImageFeatures : TStringArray;
    FbackupImageReportingLabel : String;
    FbackupImageTargetWindow : TTargetWindow;
    FclickTags : TCreativeTypeclickTagsArray;
    FcommercialId : String;
    FcompanionCreatives : TStringArray;
    Fcompatibility : TStringArray;
    FconvertFlashToHtml5 : boolean;
    FcounterCustomEvents : TCreativeTypecounterCustomEventsArray;
    FcreativeAssets : TCreativeTypecreativeAssetsArray;
    FcreativeFieldAssignments : TCreativeTypecreativeFieldAssignmentsArray;
    FcustomKeyValues : TStringArray;
    FexitCustomEvents : TCreativeTypeexitCustomEventsArray;
    FfsCommand : TFsCommand;
    FhtmlCode : String;
    FhtmlCodeLocked : boolean;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    Fkind : String;
    FlastModifiedInfo : TLastModifiedInfo;
    FlatestTraffickedCreativeId : String;
    Fname : String;
    FoverrideCss : String;
    FredirectUrl : String;
    FrenderingId : String;
    FrenderingIdDimensionValue : TDimensionValue;
    FrequiredFlashPluginVersion : String;
    FrequiredFlashVersion : integer;
    Fsize : TSize;
    Fskippable : boolean;
    FsslCompliant : boolean;
    FstudioAdvertiserId : String;
    FstudioCreativeId : String;
    FstudioTraffickedCreativeId : String;
    FsubaccountId : String;
    FthirdPartyBackupImageImpressionsUrl : String;
    FthirdPartyRichMediaImpressionsUrl : String;
    FthirdPartyUrls : TCreativeTypethirdPartyUrlsArray;
    FtimerCustomEvents : TCreativeTypetimerCustomEventsArray;
    FtotalFileSize : String;
    F_type : String;
    Fversion : integer;
    FvideoDescription : String;
    FvideoDuration : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadParameters(AIndex : Integer; AValue : String); virtual;
    Procedure SetadTagKeys(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetallowScriptAccess(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setarchived(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetartworkType(AIndex : Integer; AValue : String); virtual;
    Procedure SetauthoringTool(AIndex : Integer; AValue : String); virtual;
    Procedure Setauto_advance_images(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetbackgroundColor(AIndex : Integer; AValue : String); virtual;
    Procedure SetbackupImageClickThroughUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbackupImageFeatures(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetbackupImageReportingLabel(AIndex : Integer; AValue : String); virtual;
    Procedure SetbackupImageTargetWindow(AIndex : Integer; AValue : TTargetWindow); virtual;
    Procedure SetclickTags(AIndex : Integer; AValue : TCreativeTypeclickTagsArray); virtual;
    Procedure SetcommercialId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcompanionCreatives(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setcompatibility(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetconvertFlashToHtml5(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcounterCustomEvents(AIndex : Integer; AValue : TCreativeTypecounterCustomEventsArray); virtual;
    Procedure SetcreativeAssets(AIndex : Integer; AValue : TCreativeTypecreativeAssetsArray); virtual;
    Procedure SetcreativeFieldAssignments(AIndex : Integer; AValue : TCreativeTypecreativeFieldAssignmentsArray); virtual;
    Procedure SetcustomKeyValues(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetexitCustomEvents(AIndex : Integer; AValue : TCreativeTypeexitCustomEventsArray); virtual;
    Procedure SetfsCommand(AIndex : Integer; AValue : TFsCommand); virtual;
    Procedure SethtmlCode(AIndex : Integer; AValue : String); virtual;
    Procedure SethtmlCodeLocked(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SetlatestTraffickedCreativeId(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoverrideCss(AIndex : Integer; AValue : String); virtual;
    Procedure SetredirectUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetrenderingId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrenderingIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetrequiredFlashPluginVersion(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequiredFlashVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure Setsize(AIndex : Integer; AValue : TSize); virtual;
    Procedure Setskippable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsslCompliant(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstudioAdvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetstudioCreativeId(AIndex : Integer; AValue : String); virtual;
    Procedure SetstudioTraffickedCreativeId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetthirdPartyBackupImageImpressionsUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetthirdPartyRichMediaImpressionsUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetthirdPartyUrls(AIndex : Integer; AValue : TCreativeTypethirdPartyUrlsArray); virtual;
    Procedure SettimerCustomEvents(AIndex : Integer; AValue : TCreativeTypetimerCustomEventsArray); virtual;
    Procedure SettotalFileSize(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : integer); virtual;
    Procedure SetvideoDescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoDuration(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property adParameters : String Index 16 Read FadParameters Write SetadParameters;
    Property adTagKeys : TStringArray Index 24 Read FadTagKeys Write SetadTagKeys;
    Property advertiserId : String Index 32 Read FadvertiserId Write SetadvertiserId;
    Property allowScriptAccess : boolean Index 40 Read FallowScriptAccess Write SetallowScriptAccess;
    Property archived : boolean Index 48 Read Farchived Write Setarchived;
    Property artworkType : String Index 56 Read FartworkType Write SetartworkType;
    Property authoringTool : String Index 64 Read FauthoringTool Write SetauthoringTool;
    Property auto_advance_images : boolean Index 72 Read Fauto_advance_images Write Setauto_advance_images;
    Property backgroundColor : String Index 80 Read FbackgroundColor Write SetbackgroundColor;
    Property backupImageClickThroughUrl : String Index 88 Read FbackupImageClickThroughUrl Write SetbackupImageClickThroughUrl;
    Property backupImageFeatures : TStringArray Index 96 Read FbackupImageFeatures Write SetbackupImageFeatures;
    Property backupImageReportingLabel : String Index 104 Read FbackupImageReportingLabel Write SetbackupImageReportingLabel;
    Property backupImageTargetWindow : TTargetWindow Index 112 Read FbackupImageTargetWindow Write SetbackupImageTargetWindow;
    Property clickTags : TCreativeTypeclickTagsArray Index 120 Read FclickTags Write SetclickTags;
    Property commercialId : String Index 128 Read FcommercialId Write SetcommercialId;
    Property companionCreatives : TStringArray Index 136 Read FcompanionCreatives Write SetcompanionCreatives;
    Property compatibility : TStringArray Index 144 Read Fcompatibility Write Setcompatibility;
    Property convertFlashToHtml5 : boolean Index 152 Read FconvertFlashToHtml5 Write SetconvertFlashToHtml5;
    Property counterCustomEvents : TCreativeTypecounterCustomEventsArray Index 160 Read FcounterCustomEvents Write SetcounterCustomEvents;
    Property creativeAssets : TCreativeTypecreativeAssetsArray Index 168 Read FcreativeAssets Write SetcreativeAssets;
    Property creativeFieldAssignments : TCreativeTypecreativeFieldAssignmentsArray Index 176 Read FcreativeFieldAssignments Write SetcreativeFieldAssignments;
    Property customKeyValues : TStringArray Index 184 Read FcustomKeyValues Write SetcustomKeyValues;
    Property exitCustomEvents : TCreativeTypeexitCustomEventsArray Index 192 Read FexitCustomEvents Write SetexitCustomEvents;
    Property fsCommand : TFsCommand Index 200 Read FfsCommand Write SetfsCommand;
    Property htmlCode : String Index 208 Read FhtmlCode Write SethtmlCode;
    Property htmlCodeLocked : boolean Index 216 Read FhtmlCodeLocked Write SethtmlCodeLocked;
    Property id : String Index 224 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 232 Read FidDimensionValue Write SetidDimensionValue;
    Property kind : String Index 240 Read Fkind Write Setkind;
    Property lastModifiedInfo : TLastModifiedInfo Index 248 Read FlastModifiedInfo Write SetlastModifiedInfo;
    Property latestTraffickedCreativeId : String Index 256 Read FlatestTraffickedCreativeId Write SetlatestTraffickedCreativeId;
    Property name : String Index 264 Read Fname Write Setname;
    Property overrideCss : String Index 272 Read FoverrideCss Write SetoverrideCss;
    Property redirectUrl : String Index 280 Read FredirectUrl Write SetredirectUrl;
    Property renderingId : String Index 288 Read FrenderingId Write SetrenderingId;
    Property renderingIdDimensionValue : TDimensionValue Index 296 Read FrenderingIdDimensionValue Write SetrenderingIdDimensionValue;
    Property requiredFlashPluginVersion : String Index 304 Read FrequiredFlashPluginVersion Write SetrequiredFlashPluginVersion;
    Property requiredFlashVersion : integer Index 312 Read FrequiredFlashVersion Write SetrequiredFlashVersion;
    Property size : TSize Index 320 Read Fsize Write Setsize;
    Property skippable : boolean Index 328 Read Fskippable Write Setskippable;
    Property sslCompliant : boolean Index 336 Read FsslCompliant Write SetsslCompliant;
    Property studioAdvertiserId : String Index 344 Read FstudioAdvertiserId Write SetstudioAdvertiserId;
    Property studioCreativeId : String Index 352 Read FstudioCreativeId Write SetstudioCreativeId;
    Property studioTraffickedCreativeId : String Index 360 Read FstudioTraffickedCreativeId Write SetstudioTraffickedCreativeId;
    Property subaccountId : String Index 368 Read FsubaccountId Write SetsubaccountId;
    Property thirdPartyBackupImageImpressionsUrl : String Index 376 Read FthirdPartyBackupImageImpressionsUrl Write SetthirdPartyBackupImageImpressionsUrl;
    Property thirdPartyRichMediaImpressionsUrl : String Index 384 Read FthirdPartyRichMediaImpressionsUrl Write SetthirdPartyRichMediaImpressionsUrl;
    Property thirdPartyUrls : TCreativeTypethirdPartyUrlsArray Index 392 Read FthirdPartyUrls Write SetthirdPartyUrls;
    Property timerCustomEvents : TCreativeTypetimerCustomEventsArray Index 400 Read FtimerCustomEvents Write SettimerCustomEvents;
    Property totalFileSize : String Index 408 Read FtotalFileSize Write SettotalFileSize;
    Property _type : String Index 416 Read F_type Write Set_type;
    Property version : integer Index 424 Read Fversion Write Setversion;
    Property videoDescription : String Index 432 Read FvideoDescription Write SetvideoDescription;
    Property videoDuration : integer Index 440 Read FvideoDuration Write SetvideoDuration;
  end;
  TCreativeClass = Class of TCreative;
  
  { --------------------------------------------------------------------
    TCreativeAsset
    --------------------------------------------------------------------}
  
  TCreativeAsset = Class(TGoogleBaseObject)
  Private
    FactionScript3 : boolean;
    Factive : boolean;
    Falignment : String;
    FartworkType : String;
    FassetIdentifier : TCreativeAssetId;
    FbackupImageExit : TCreativeCustomEvent;
    FbitRate : integer;
    FchildAssetType : String;
    FcollapsedSize : TSize;
    FcustomStartTimeValue : integer;
    FdetectedFeatures : TStringArray;
    FdisplayType : String;
    Fduration : integer;
    FdurationType : String;
    FexpandedDimension : TSize;
    FfileSize : String;
    FflashVersion : integer;
    FhideFlashObjects : boolean;
    FhideSelectionBoxes : boolean;
    FhorizontallyLocked : boolean;
    Fid : String;
    FmimeType : String;
    Foffset : TOffsetPosition;
    ForiginalBackup : boolean;
    Fposition : TOffsetPosition;
    FpositionLeftUnit : String;
    FpositionTopUnit : String;
    FprogressiveServingUrl : String;
    Fpushdown : boolean;
    FpushdownDuration : integer;
    Frole : String;
    Fsize : TSize;
    FsslCompliant : boolean;
    FstartTimeType : String;
    FstreamingServingUrl : String;
    Ftransparency : boolean;
    FverticallyLocked : boolean;
    FvideoDuration : integer;
    FwindowMode : String;
    FzIndex : integer;
    FzipFilename : String;
    FzipFilesize : String;
  Protected
    //Property setters
    Procedure SetactionScript3(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setalignment(AIndex : Integer; AValue : String); virtual;
    Procedure SetartworkType(AIndex : Integer; AValue : String); virtual;
    Procedure SetassetIdentifier(AIndex : Integer; AValue : TCreativeAssetId); virtual;
    Procedure SetbackupImageExit(AIndex : Integer; AValue : TCreativeCustomEvent); virtual;
    Procedure SetbitRate(AIndex : Integer; AValue : integer); virtual;
    Procedure SetchildAssetType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcollapsedSize(AIndex : Integer; AValue : TSize); virtual;
    Procedure SetcustomStartTimeValue(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdetectedFeatures(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetdisplayType(AIndex : Integer; AValue : String); virtual;
    Procedure Setduration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdurationType(AIndex : Integer; AValue : String); virtual;
    Procedure SetexpandedDimension(AIndex : Integer; AValue : TSize); virtual;
    Procedure SetfileSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetflashVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure SethideFlashObjects(AIndex : Integer; AValue : boolean); virtual;
    Procedure SethideSelectionBoxes(AIndex : Integer; AValue : boolean); virtual;
    Procedure SethorizontallyLocked(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetmimeType(AIndex : Integer; AValue : String); virtual;
    Procedure Setoffset(AIndex : Integer; AValue : TOffsetPosition); virtual;
    Procedure SetoriginalBackup(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TOffsetPosition); virtual;
    Procedure SetpositionLeftUnit(AIndex : Integer; AValue : String); virtual;
    Procedure SetpositionTopUnit(AIndex : Integer; AValue : String); virtual;
    Procedure SetprogressiveServingUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setpushdown(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpushdownDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrole(AIndex : Integer; AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; AValue : TSize); virtual;
    Procedure SetsslCompliant(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstartTimeType(AIndex : Integer; AValue : String); virtual;
    Procedure SetstreamingServingUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Settransparency(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetverticallyLocked(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetvideoDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetwindowMode(AIndex : Integer; AValue : String); virtual;
    Procedure SetzIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SetzipFilename(AIndex : Integer; AValue : String); virtual;
    Procedure SetzipFilesize(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property actionScript3 : boolean Index 0 Read FactionScript3 Write SetactionScript3;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property alignment : String Index 16 Read Falignment Write Setalignment;
    Property artworkType : String Index 24 Read FartworkType Write SetartworkType;
    Property assetIdentifier : TCreativeAssetId Index 32 Read FassetIdentifier Write SetassetIdentifier;
    Property backupImageExit : TCreativeCustomEvent Index 40 Read FbackupImageExit Write SetbackupImageExit;
    Property bitRate : integer Index 48 Read FbitRate Write SetbitRate;
    Property childAssetType : String Index 56 Read FchildAssetType Write SetchildAssetType;
    Property collapsedSize : TSize Index 64 Read FcollapsedSize Write SetcollapsedSize;
    Property customStartTimeValue : integer Index 72 Read FcustomStartTimeValue Write SetcustomStartTimeValue;
    Property detectedFeatures : TStringArray Index 80 Read FdetectedFeatures Write SetdetectedFeatures;
    Property displayType : String Index 88 Read FdisplayType Write SetdisplayType;
    Property duration : integer Index 96 Read Fduration Write Setduration;
    Property durationType : String Index 104 Read FdurationType Write SetdurationType;
    Property expandedDimension : TSize Index 112 Read FexpandedDimension Write SetexpandedDimension;
    Property fileSize : String Index 120 Read FfileSize Write SetfileSize;
    Property flashVersion : integer Index 128 Read FflashVersion Write SetflashVersion;
    Property hideFlashObjects : boolean Index 136 Read FhideFlashObjects Write SethideFlashObjects;
    Property hideSelectionBoxes : boolean Index 144 Read FhideSelectionBoxes Write SethideSelectionBoxes;
    Property horizontallyLocked : boolean Index 152 Read FhorizontallyLocked Write SethorizontallyLocked;
    Property id : String Index 160 Read Fid Write Setid;
    Property mimeType : String Index 168 Read FmimeType Write SetmimeType;
    Property offset : TOffsetPosition Index 176 Read Foffset Write Setoffset;
    Property originalBackup : boolean Index 184 Read ForiginalBackup Write SetoriginalBackup;
    Property position : TOffsetPosition Index 192 Read Fposition Write Setposition;
    Property positionLeftUnit : String Index 200 Read FpositionLeftUnit Write SetpositionLeftUnit;
    Property positionTopUnit : String Index 208 Read FpositionTopUnit Write SetpositionTopUnit;
    Property progressiveServingUrl : String Index 216 Read FprogressiveServingUrl Write SetprogressiveServingUrl;
    Property pushdown : boolean Index 224 Read Fpushdown Write Setpushdown;
    Property pushdownDuration : integer Index 232 Read FpushdownDuration Write SetpushdownDuration;
    Property role : String Index 240 Read Frole Write Setrole;
    Property size : TSize Index 248 Read Fsize Write Setsize;
    Property sslCompliant : boolean Index 256 Read FsslCompliant Write SetsslCompliant;
    Property startTimeType : String Index 264 Read FstartTimeType Write SetstartTimeType;
    Property streamingServingUrl : String Index 272 Read FstreamingServingUrl Write SetstreamingServingUrl;
    Property transparency : boolean Index 280 Read Ftransparency Write Settransparency;
    Property verticallyLocked : boolean Index 288 Read FverticallyLocked Write SetverticallyLocked;
    Property videoDuration : integer Index 296 Read FvideoDuration Write SetvideoDuration;
    Property windowMode : String Index 304 Read FwindowMode Write SetwindowMode;
    Property zIndex : integer Index 312 Read FzIndex Write SetzIndex;
    Property zipFilename : String Index 320 Read FzipFilename Write SetzipFilename;
    Property zipFilesize : String Index 328 Read FzipFilesize Write SetzipFilesize;
  end;
  TCreativeAssetClass = Class of TCreativeAsset;
  
  { --------------------------------------------------------------------
    TCreativeAssetId
    --------------------------------------------------------------------}
  
  TCreativeAssetId = Class(TGoogleBaseObject)
  Private
    Fname : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TCreativeAssetIdClass = Class of TCreativeAssetId;
  
  { --------------------------------------------------------------------
    TCreativeAssetMetadata
    --------------------------------------------------------------------}
  
  TCreativeAssetMetadata = Class(TGoogleBaseObject)
  Private
    FassetIdentifier : TCreativeAssetId;
    FclickTags : TCreativeAssetMetadataTypeclickTagsArray;
    FdetectedFeatures : TStringArray;
    Fkind : String;
    FwarnedValidationRules : TStringArray;
  Protected
    //Property setters
    Procedure SetassetIdentifier(AIndex : Integer; AValue : TCreativeAssetId); virtual;
    Procedure SetclickTags(AIndex : Integer; AValue : TCreativeAssetMetadataTypeclickTagsArray); virtual;
    Procedure SetdetectedFeatures(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetwarnedValidationRules(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property assetIdentifier : TCreativeAssetId Index 0 Read FassetIdentifier Write SetassetIdentifier;
    Property clickTags : TCreativeAssetMetadataTypeclickTagsArray Index 8 Read FclickTags Write SetclickTags;
    Property detectedFeatures : TStringArray Index 16 Read FdetectedFeatures Write SetdetectedFeatures;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property warnedValidationRules : TStringArray Index 32 Read FwarnedValidationRules Write SetwarnedValidationRules;
  end;
  TCreativeAssetMetadataClass = Class of TCreativeAssetMetadata;
  
  { --------------------------------------------------------------------
    TCreativeAssignment
    --------------------------------------------------------------------}
  
  TCreativeAssignment = Class(TGoogleBaseObject)
  Private
    Factive : boolean;
    FapplyEventTags : boolean;
    FclickThroughUrl : TClickThroughUrl;
    FcompanionCreativeOverrides : TCreativeAssignmentTypecompanionCreativeOverridesArray;
    FcreativeGroupAssignments : TCreativeAssignmentTypecreativeGroupAssignmentsArray;
    FcreativeId : String;
    FcreativeIdDimensionValue : TDimensionValue;
    FendTime : TDatetime;
    FrichMediaExitOverrides : TCreativeAssignmentTyperichMediaExitOverridesArray;
    Fsequence : integer;
    FsslCompliant : boolean;
    FstartTime : TDatetime;
    Fweight : integer;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetapplyEventTags(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetclickThroughUrl(AIndex : Integer; AValue : TClickThroughUrl); virtual;
    Procedure SetcompanionCreativeOverrides(AIndex : Integer; AValue : TCreativeAssignmentTypecompanionCreativeOverridesArray); virtual;
    Procedure SetcreativeGroupAssignments(AIndex : Integer; AValue : TCreativeAssignmentTypecreativeGroupAssignmentsArray); virtual;
    Procedure SetcreativeId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreativeIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetrichMediaExitOverrides(AIndex : Integer; AValue : TCreativeAssignmentTyperichMediaExitOverridesArray); virtual;
    Procedure Setsequence(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsslCompliant(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setweight(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property applyEventTags : boolean Index 8 Read FapplyEventTags Write SetapplyEventTags;
    Property clickThroughUrl : TClickThroughUrl Index 16 Read FclickThroughUrl Write SetclickThroughUrl;
    Property companionCreativeOverrides : TCreativeAssignmentTypecompanionCreativeOverridesArray Index 24 Read FcompanionCreativeOverrides Write SetcompanionCreativeOverrides;
    Property creativeGroupAssignments : TCreativeAssignmentTypecreativeGroupAssignmentsArray Index 32 Read FcreativeGroupAssignments Write SetcreativeGroupAssignments;
    Property creativeId : String Index 40 Read FcreativeId Write SetcreativeId;
    Property creativeIdDimensionValue : TDimensionValue Index 48 Read FcreativeIdDimensionValue Write SetcreativeIdDimensionValue;
    Property endTime : TDatetime Index 56 Read FendTime Write SetendTime;
    Property richMediaExitOverrides : TCreativeAssignmentTyperichMediaExitOverridesArray Index 64 Read FrichMediaExitOverrides Write SetrichMediaExitOverrides;
    Property sequence : integer Index 72 Read Fsequence Write Setsequence;
    Property sslCompliant : boolean Index 80 Read FsslCompliant Write SetsslCompliant;
    Property startTime : TDatetime Index 88 Read FstartTime Write SetstartTime;
    Property weight : integer Index 96 Read Fweight Write Setweight;
  end;
  TCreativeAssignmentClass = Class of TCreativeAssignment;
  
  { --------------------------------------------------------------------
    TCreativeCustomEvent
    --------------------------------------------------------------------}
  
  TCreativeCustomEvent = Class(TGoogleBaseObject)
  Private
    Factive : boolean;
    FadvertiserCustomEventName : String;
    FadvertiserCustomEventType : String;
    FartworkLabel : String;
    FartworkType : String;
    FexitUrl : String;
    Fid : String;
    FpopupWindowProperties : TPopupWindowProperties;
    FtargetType : String;
    FvideoReportingId : String;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadvertiserCustomEventName(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserCustomEventType(AIndex : Integer; AValue : String); virtual;
    Procedure SetartworkLabel(AIndex : Integer; AValue : String); virtual;
    Procedure SetartworkType(AIndex : Integer; AValue : String); virtual;
    Procedure SetexitUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetpopupWindowProperties(AIndex : Integer; AValue : TPopupWindowProperties); virtual;
    Procedure SettargetType(AIndex : Integer; AValue : String); virtual;
    Procedure SetvideoReportingId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property advertiserCustomEventName : String Index 8 Read FadvertiserCustomEventName Write SetadvertiserCustomEventName;
    Property advertiserCustomEventType : String Index 16 Read FadvertiserCustomEventType Write SetadvertiserCustomEventType;
    Property artworkLabel : String Index 24 Read FartworkLabel Write SetartworkLabel;
    Property artworkType : String Index 32 Read FartworkType Write SetartworkType;
    Property exitUrl : String Index 40 Read FexitUrl Write SetexitUrl;
    Property id : String Index 48 Read Fid Write Setid;
    Property popupWindowProperties : TPopupWindowProperties Index 56 Read FpopupWindowProperties Write SetpopupWindowProperties;
    Property targetType : String Index 64 Read FtargetType Write SettargetType;
    Property videoReportingId : String Index 72 Read FvideoReportingId Write SetvideoReportingId;
  end;
  TCreativeCustomEventClass = Class of TCreativeCustomEvent;
  
  { --------------------------------------------------------------------
    TCreativeField
    --------------------------------------------------------------------}
  
  TCreativeField = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    Fid : String;
    Fkind : String;
    Fname : String;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 16 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property name : String Index 40 Read Fname Write Setname;
    Property subaccountId : String Index 48 Read FsubaccountId Write SetsubaccountId;
  end;
  TCreativeFieldClass = Class of TCreativeField;
  
  { --------------------------------------------------------------------
    TCreativeFieldAssignment
    --------------------------------------------------------------------}
  
  TCreativeFieldAssignment = Class(TGoogleBaseObject)
  Private
    FcreativeFieldId : String;
    FcreativeFieldValueId : String;
  Protected
    //Property setters
    Procedure SetcreativeFieldId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreativeFieldValueId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creativeFieldId : String Index 0 Read FcreativeFieldId Write SetcreativeFieldId;
    Property creativeFieldValueId : String Index 8 Read FcreativeFieldValueId Write SetcreativeFieldValueId;
  end;
  TCreativeFieldAssignmentClass = Class of TCreativeFieldAssignment;
  
  { --------------------------------------------------------------------
    TCreativeFieldValue
    --------------------------------------------------------------------}
  
  TCreativeFieldValue = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TCreativeFieldValueClass = Class of TCreativeFieldValue;
  
  { --------------------------------------------------------------------
    TCreativeFieldValuesListResponse
    --------------------------------------------------------------------}
  
  TCreativeFieldValuesListResponse = Class(TGoogleBaseObject)
  Private
    FcreativeFieldValues : TCreativeFieldValuesListResponseTypecreativeFieldValuesArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetcreativeFieldValues(AIndex : Integer; AValue : TCreativeFieldValuesListResponseTypecreativeFieldValuesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creativeFieldValues : TCreativeFieldValuesListResponseTypecreativeFieldValuesArray Index 0 Read FcreativeFieldValues Write SetcreativeFieldValues;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCreativeFieldValuesListResponseClass = Class of TCreativeFieldValuesListResponse;
  
  { --------------------------------------------------------------------
    TCreativeFieldsListResponse
    --------------------------------------------------------------------}
  
  TCreativeFieldsListResponse = Class(TGoogleBaseObject)
  Private
    FcreativeFields : TCreativeFieldsListResponseTypecreativeFieldsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetcreativeFields(AIndex : Integer; AValue : TCreativeFieldsListResponseTypecreativeFieldsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creativeFields : TCreativeFieldsListResponseTypecreativeFieldsArray Index 0 Read FcreativeFields Write SetcreativeFields;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCreativeFieldsListResponseClass = Class of TCreativeFieldsListResponse;
  
  { --------------------------------------------------------------------
    TCreativeGroup
    --------------------------------------------------------------------}
  
  TCreativeGroup = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    FgroupNumber : integer;
    Fid : String;
    Fkind : String;
    Fname : String;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetgroupNumber(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 16 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property groupNumber : integer Index 24 Read FgroupNumber Write SetgroupNumber;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property name : String Index 48 Read Fname Write Setname;
    Property subaccountId : String Index 56 Read FsubaccountId Write SetsubaccountId;
  end;
  TCreativeGroupClass = Class of TCreativeGroup;
  
  { --------------------------------------------------------------------
    TCreativeGroupAssignment
    --------------------------------------------------------------------}
  
  TCreativeGroupAssignment = Class(TGoogleBaseObject)
  Private
    FcreativeGroupId : String;
    FcreativeGroupNumber : String;
  Protected
    //Property setters
    Procedure SetcreativeGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreativeGroupNumber(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creativeGroupId : String Index 0 Read FcreativeGroupId Write SetcreativeGroupId;
    Property creativeGroupNumber : String Index 8 Read FcreativeGroupNumber Write SetcreativeGroupNumber;
  end;
  TCreativeGroupAssignmentClass = Class of TCreativeGroupAssignment;
  
  { --------------------------------------------------------------------
    TCreativeGroupsListResponse
    --------------------------------------------------------------------}
  
  TCreativeGroupsListResponse = Class(TGoogleBaseObject)
  Private
    FcreativeGroups : TCreativeGroupsListResponseTypecreativeGroupsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetcreativeGroups(AIndex : Integer; AValue : TCreativeGroupsListResponseTypecreativeGroupsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creativeGroups : TCreativeGroupsListResponseTypecreativeGroupsArray Index 0 Read FcreativeGroups Write SetcreativeGroups;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCreativeGroupsListResponseClass = Class of TCreativeGroupsListResponse;
  
  { --------------------------------------------------------------------
    TCreativeOptimizationConfiguration
    --------------------------------------------------------------------}
  
  TCreativeOptimizationConfiguration = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : String;
    FoptimizationActivitys : TCreativeOptimizationConfigurationTypeoptimizationActivitysArray;
    FoptimizationModel : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoptimizationActivitys(AIndex : Integer; AValue : TCreativeOptimizationConfigurationTypeoptimizationActivitysArray); virtual;
    Procedure SetoptimizationModel(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
    Property optimizationActivitys : TCreativeOptimizationConfigurationTypeoptimizationActivitysArray Index 16 Read FoptimizationActivitys Write SetoptimizationActivitys;
    Property optimizationModel : String Index 24 Read FoptimizationModel Write SetoptimizationModel;
  end;
  TCreativeOptimizationConfigurationClass = Class of TCreativeOptimizationConfiguration;
  
  { --------------------------------------------------------------------
    TCreativeRotation
    --------------------------------------------------------------------}
  
  TCreativeRotation = Class(TGoogleBaseObject)
  Private
    FcreativeAssignments : TCreativeRotationTypecreativeAssignmentsArray;
    FcreativeOptimizationConfigurationId : String;
    F_type : String;
    FweightCalculationStrategy : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcreativeAssignments(AIndex : Integer; AValue : TCreativeRotationTypecreativeAssignmentsArray); virtual;
    Procedure SetcreativeOptimizationConfigurationId(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure SetweightCalculationStrategy(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creativeAssignments : TCreativeRotationTypecreativeAssignmentsArray Index 0 Read FcreativeAssignments Write SetcreativeAssignments;
    Property creativeOptimizationConfigurationId : String Index 8 Read FcreativeOptimizationConfigurationId Write SetcreativeOptimizationConfigurationId;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property weightCalculationStrategy : String Index 24 Read FweightCalculationStrategy Write SetweightCalculationStrategy;
  end;
  TCreativeRotationClass = Class of TCreativeRotation;
  
  { --------------------------------------------------------------------
    TCreativeSettings
    --------------------------------------------------------------------}
  
  TCreativeSettings = Class(TGoogleBaseObject)
  Private
    FiFrameFooter : String;
    FiFrameHeader : String;
  Protected
    //Property setters
    Procedure SetiFrameFooter(AIndex : Integer; AValue : String); virtual;
    Procedure SetiFrameHeader(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property iFrameFooter : String Index 0 Read FiFrameFooter Write SetiFrameFooter;
    Property iFrameHeader : String Index 8 Read FiFrameHeader Write SetiFrameHeader;
  end;
  TCreativeSettingsClass = Class of TCreativeSettings;
  
  { --------------------------------------------------------------------
    TCreativesListResponse
    --------------------------------------------------------------------}
  
  TCreativesListResponse = Class(TGoogleBaseObject)
  Private
    Fcreatives : TCreativesListResponseTypecreativesArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setcreatives(AIndex : Integer; AValue : TCreativesListResponseTypecreativesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creatives : TCreativesListResponseTypecreativesArray Index 0 Read Fcreatives Write Setcreatives;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCreativesListResponseClass = Class of TCreativesListResponse;
  
  { --------------------------------------------------------------------
    TCrossDimensionReachReportCompatibleFields
    --------------------------------------------------------------------}
  
  TCrossDimensionReachReportCompatibleFields = Class(TGoogleBaseObject)
  Private
    Fbreakdown : TCrossDimensionReachReportCompatibleFieldsTypebreakdownArray;
    FdimensionFilters : TCrossDimensionReachReportCompatibleFieldsTypedimensionFiltersArray;
    Fkind : String;
    Fmetrics : TCrossDimensionReachReportCompatibleFieldsTypemetricsArray;
    FoverlapMetrics : TCrossDimensionReachReportCompatibleFieldsTypeoverlapMetricsArray;
  Protected
    //Property setters
    Procedure Setbreakdown(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFieldsTypebreakdownArray); virtual;
    Procedure SetdimensionFilters(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFieldsTypedimensionFiltersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFieldsTypemetricsArray); virtual;
    Procedure SetoverlapMetrics(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFieldsTypeoverlapMetricsArray); virtual;
  Public
  Published
    Property breakdown : TCrossDimensionReachReportCompatibleFieldsTypebreakdownArray Index 0 Read Fbreakdown Write Setbreakdown;
    Property dimensionFilters : TCrossDimensionReachReportCompatibleFieldsTypedimensionFiltersArray Index 8 Read FdimensionFilters Write SetdimensionFilters;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property metrics : TCrossDimensionReachReportCompatibleFieldsTypemetricsArray Index 24 Read Fmetrics Write Setmetrics;
    Property overlapMetrics : TCrossDimensionReachReportCompatibleFieldsTypeoverlapMetricsArray Index 32 Read FoverlapMetrics Write SetoverlapMetrics;
  end;
  TCrossDimensionReachReportCompatibleFieldsClass = Class of TCrossDimensionReachReportCompatibleFields;
  
  { --------------------------------------------------------------------
    TCustomRichMediaEvents
    --------------------------------------------------------------------}
  
  TCustomRichMediaEvents = Class(TGoogleBaseObject)
  Private
    FfilteredEventIds : TCustomRichMediaEventsTypefilteredEventIdsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetfilteredEventIds(AIndex : Integer; AValue : TCustomRichMediaEventsTypefilteredEventIdsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property filteredEventIds : TCustomRichMediaEventsTypefilteredEventIdsArray Index 0 Read FfilteredEventIds Write SetfilteredEventIds;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCustomRichMediaEventsClass = Class of TCustomRichMediaEvents;
  
  { --------------------------------------------------------------------
    TDateRange
    --------------------------------------------------------------------}
  
  TDateRange = Class(TGoogleBaseObject)
  Private
    FendDate : TDate;
    Fkind : String;
    FrelativeDateRange : String;
    FstartDate : TDate;
  Protected
    //Property setters
    Procedure SetendDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrelativeDateRange(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
  Public
  Published
    Property endDate : TDate Index 0 Read FendDate Write SetendDate;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property relativeDateRange : String Index 16 Read FrelativeDateRange Write SetrelativeDateRange;
    Property startDate : TDate Index 24 Read FstartDate Write SetstartDate;
  end;
  TDateRangeClass = Class of TDateRange;
  
  { --------------------------------------------------------------------
    TDayPartTargeting
    --------------------------------------------------------------------}
  
  TDayPartTargeting = Class(TGoogleBaseObject)
  Private
    FdaysOfWeek : TStringArray;
    FhoursOfDay : TintegerArray;
    FuserLocalTime : boolean;
  Protected
    //Property setters
    Procedure SetdaysOfWeek(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SethoursOfDay(AIndex : Integer; AValue : TintegerArray); virtual;
    Procedure SetuserLocalTime(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property daysOfWeek : TStringArray Index 0 Read FdaysOfWeek Write SetdaysOfWeek;
    Property hoursOfDay : TintegerArray Index 8 Read FhoursOfDay Write SethoursOfDay;
    Property userLocalTime : boolean Index 16 Read FuserLocalTime Write SetuserLocalTime;
  end;
  TDayPartTargetingClass = Class of TDayPartTargeting;
  
  { --------------------------------------------------------------------
    TDefaultClickThroughEventTagProperties
    --------------------------------------------------------------------}
  
  TDefaultClickThroughEventTagProperties = Class(TGoogleBaseObject)
  Private
    FdefaultClickThroughEventTagId : String;
    FoverrideInheritedEventTag : boolean;
  Protected
    //Property setters
    Procedure SetdefaultClickThroughEventTagId(AIndex : Integer; AValue : String); virtual;
    Procedure SetoverrideInheritedEventTag(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property defaultClickThroughEventTagId : String Index 0 Read FdefaultClickThroughEventTagId Write SetdefaultClickThroughEventTagId;
    Property overrideInheritedEventTag : boolean Index 8 Read FoverrideInheritedEventTag Write SetoverrideInheritedEventTag;
  end;
  TDefaultClickThroughEventTagPropertiesClass = Class of TDefaultClickThroughEventTagProperties;
  
  { --------------------------------------------------------------------
    TDeliverySchedule
    --------------------------------------------------------------------}
  
  TDeliverySchedule = Class(TGoogleBaseObject)
  Private
    FfrequencyCap : TFrequencyCap;
    FhardCutoff : boolean;
    FimpressionRatio : String;
    Fpriority : String;
  Protected
    //Property setters
    Procedure SetfrequencyCap(AIndex : Integer; AValue : TFrequencyCap); virtual;
    Procedure SethardCutoff(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetimpressionRatio(AIndex : Integer; AValue : String); virtual;
    Procedure Setpriority(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property frequencyCap : TFrequencyCap Index 0 Read FfrequencyCap Write SetfrequencyCap;
    Property hardCutoff : boolean Index 8 Read FhardCutoff Write SethardCutoff;
    Property impressionRatio : String Index 16 Read FimpressionRatio Write SetimpressionRatio;
    Property priority : String Index 24 Read Fpriority Write Setpriority;
  end;
  TDeliveryScheduleClass = Class of TDeliverySchedule;
  
  { --------------------------------------------------------------------
    TDfpSettings
    --------------------------------------------------------------------}
  
  TDfpSettings = Class(TGoogleBaseObject)
  Private
    Fdfp_network_code : String;
    Fdfp_network_name : String;
    FprogrammaticPlacementAccepted : boolean;
    FpubPaidPlacementAccepted : boolean;
    FpublisherPortalOnly : boolean;
  Protected
    //Property setters
    Procedure Setdfp_network_code(AIndex : Integer; AValue : String); virtual;
    Procedure Setdfp_network_name(AIndex : Integer; AValue : String); virtual;
    Procedure SetprogrammaticPlacementAccepted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpubPaidPlacementAccepted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpublisherPortalOnly(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property dfp_network_code : String Index 0 Read Fdfp_network_code Write Setdfp_network_code;
    Property dfp_network_name : String Index 8 Read Fdfp_network_name Write Setdfp_network_name;
    Property programmaticPlacementAccepted : boolean Index 16 Read FprogrammaticPlacementAccepted Write SetprogrammaticPlacementAccepted;
    Property pubPaidPlacementAccepted : boolean Index 24 Read FpubPaidPlacementAccepted Write SetpubPaidPlacementAccepted;
    Property publisherPortalOnly : boolean Index 32 Read FpublisherPortalOnly Write SetpublisherPortalOnly;
  end;
  TDfpSettingsClass = Class of TDfpSettings;
  
  { --------------------------------------------------------------------
    TDimension
    --------------------------------------------------------------------}
  
  TDimension = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TDimensionClass = Class of TDimension;
  
  { --------------------------------------------------------------------
    TDimensionFilter
    --------------------------------------------------------------------}
  
  TDimensionFilter = Class(TGoogleBaseObject)
  Private
    FdimensionName : String;
    Fkind : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure SetdimensionName(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dimensionName : String Index 0 Read FdimensionName Write SetdimensionName;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TDimensionFilterClass = Class of TDimensionFilter;
  
  { --------------------------------------------------------------------
    TDimensionValue
    --------------------------------------------------------------------}
  
  TDimensionValue = Class(TGoogleBaseObject)
  Private
    FdimensionName : String;
    Fetag : String;
    Fid : String;
    Fkind : String;
    FmatchType : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure SetdimensionName(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmatchType(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dimensionName : String Index 0 Read FdimensionName Write SetdimensionName;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property matchType : String Index 32 Read FmatchType Write SetmatchType;
    Property value : String Index 40 Read Fvalue Write Setvalue;
  end;
  TDimensionValueClass = Class of TDimensionValue;
  
  { --------------------------------------------------------------------
    TDimensionValueList
    --------------------------------------------------------------------}
  
  TDimensionValueList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TDimensionValueListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TDimensionValueListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TDimensionValueListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TDimensionValueListClass = Class of TDimensionValueList;
  
  { --------------------------------------------------------------------
    TDimensionValueRequest
    --------------------------------------------------------------------}
  
  TDimensionValueRequest = Class(TGoogleBaseObject)
  Private
    FdimensionName : String;
    FendDate : TDate;
    Ffilters : TDimensionValueRequestTypefiltersArray;
    Fkind : String;
    FstartDate : TDate;
  Protected
    //Property setters
    Procedure SetdimensionName(AIndex : Integer; AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : TDimensionValueRequestTypefiltersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
  Public
  Published
    Property dimensionName : String Index 0 Read FdimensionName Write SetdimensionName;
    Property endDate : TDate Index 8 Read FendDate Write SetendDate;
    Property filters : TDimensionValueRequestTypefiltersArray Index 16 Read Ffilters Write Setfilters;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property startDate : TDate Index 32 Read FstartDate Write SetstartDate;
  end;
  TDimensionValueRequestClass = Class of TDimensionValueRequest;
  
  { --------------------------------------------------------------------
    TDirectorySite
    --------------------------------------------------------------------}
  
  TDirectorySite = Class(TGoogleBaseObject)
  Private
    Factive : boolean;
    FcontactAssignments : TDirectorySiteTypecontactAssignmentsArray;
    FcountryId : String;
    FcurrencyId : String;
    Fdescription : String;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    FinpageTagFormats : TStringArray;
    FinterstitialTagFormats : TStringArray;
    Fkind : String;
    Fname : String;
    FparentId : String;
    Fsettings : TDirectorySiteSettings;
    Furl : String;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcontactAssignments(AIndex : Integer; AValue : TDirectorySiteTypecontactAssignmentsArray); virtual;
    Procedure SetcountryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrencyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetinpageTagFormats(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetinterstitialTagFormats(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentId(AIndex : Integer; AValue : String); virtual;
    Procedure Setsettings(AIndex : Integer; AValue : TDirectorySiteSettings); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property contactAssignments : TDirectorySiteTypecontactAssignmentsArray Index 8 Read FcontactAssignments Write SetcontactAssignments;
    Property countryId : String Index 16 Read FcountryId Write SetcountryId;
    Property currencyId : String Index 24 Read FcurrencyId Write SetcurrencyId;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property id : String Index 40 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 48 Read FidDimensionValue Write SetidDimensionValue;
    Property inpageTagFormats : TStringArray Index 56 Read FinpageTagFormats Write SetinpageTagFormats;
    Property interstitialTagFormats : TStringArray Index 64 Read FinterstitialTagFormats Write SetinterstitialTagFormats;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property name : String Index 80 Read Fname Write Setname;
    Property parentId : String Index 88 Read FparentId Write SetparentId;
    Property settings : TDirectorySiteSettings Index 96 Read Fsettings Write Setsettings;
    Property url : String Index 104 Read Furl Write Seturl;
  end;
  TDirectorySiteClass = Class of TDirectorySite;
  
  { --------------------------------------------------------------------
    TDirectorySiteContact
    --------------------------------------------------------------------}
  
  TDirectorySiteContact = Class(TGoogleBaseObject)
  Private
    Faddress : String;
    Femail : String;
    FfirstName : String;
    Fid : String;
    Fkind : String;
    FlastName : String;
    Fphone : String;
    Frole : String;
    Ftitle : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetfirstName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastName(AIndex : Integer; AValue : String); virtual;
    Procedure Setphone(AIndex : Integer; AValue : String); virtual;
    Procedure Setrole(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property address : String Index 0 Read Faddress Write Setaddress;
    Property email : String Index 8 Read Femail Write Setemail;
    Property firstName : String Index 16 Read FfirstName Write SetfirstName;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property lastName : String Index 40 Read FlastName Write SetlastName;
    Property phone : String Index 48 Read Fphone Write Setphone;
    Property role : String Index 56 Read Frole Write Setrole;
    Property title : String Index 64 Read Ftitle Write Settitle;
    Property _type : String Index 72 Read F_type Write Set_type;
  end;
  TDirectorySiteContactClass = Class of TDirectorySiteContact;
  
  { --------------------------------------------------------------------
    TDirectorySiteContactAssignment
    --------------------------------------------------------------------}
  
  TDirectorySiteContactAssignment = Class(TGoogleBaseObject)
  Private
    FcontactId : String;
    Fvisibility : String;
  Protected
    //Property setters
    Procedure SetcontactId(AIndex : Integer; AValue : String); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property contactId : String Index 0 Read FcontactId Write SetcontactId;
    Property visibility : String Index 8 Read Fvisibility Write Setvisibility;
  end;
  TDirectorySiteContactAssignmentClass = Class of TDirectorySiteContactAssignment;
  
  { --------------------------------------------------------------------
    TDirectorySiteContactsListResponse
    --------------------------------------------------------------------}
  
  TDirectorySiteContactsListResponse = Class(TGoogleBaseObject)
  Private
    FdirectorySiteContacts : TDirectorySiteContactsListResponseTypedirectorySiteContactsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetdirectorySiteContacts(AIndex : Integer; AValue : TDirectorySiteContactsListResponseTypedirectorySiteContactsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property directorySiteContacts : TDirectorySiteContactsListResponseTypedirectorySiteContactsArray Index 0 Read FdirectorySiteContacts Write SetdirectorySiteContacts;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TDirectorySiteContactsListResponseClass = Class of TDirectorySiteContactsListResponse;
  
  { --------------------------------------------------------------------
    TDirectorySiteSettings
    --------------------------------------------------------------------}
  
  TDirectorySiteSettings = Class(TGoogleBaseObject)
  Private
    FactiveViewOptOut : boolean;
    Fdfp_settings : TDfpSettings;
    Finstream_video_placement_accepted : boolean;
    FinterstitialPlacementAccepted : boolean;
    FnielsenOcrOptOut : boolean;
    FverificationTagOptOut : boolean;
    FvideoActiveViewOptOut : boolean;
  Protected
    //Property setters
    Procedure SetactiveViewOptOut(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setdfp_settings(AIndex : Integer; AValue : TDfpSettings); virtual;
    Procedure Setinstream_video_placement_accepted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetinterstitialPlacementAccepted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetnielsenOcrOptOut(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetverificationTagOptOut(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetvideoActiveViewOptOut(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property activeViewOptOut : boolean Index 0 Read FactiveViewOptOut Write SetactiveViewOptOut;
    Property dfp_settings : TDfpSettings Index 8 Read Fdfp_settings Write Setdfp_settings;
    Property instream_video_placement_accepted : boolean Index 16 Read Finstream_video_placement_accepted Write Setinstream_video_placement_accepted;
    Property interstitialPlacementAccepted : boolean Index 24 Read FinterstitialPlacementAccepted Write SetinterstitialPlacementAccepted;
    Property nielsenOcrOptOut : boolean Index 32 Read FnielsenOcrOptOut Write SetnielsenOcrOptOut;
    Property verificationTagOptOut : boolean Index 40 Read FverificationTagOptOut Write SetverificationTagOptOut;
    Property videoActiveViewOptOut : boolean Index 48 Read FvideoActiveViewOptOut Write SetvideoActiveViewOptOut;
  end;
  TDirectorySiteSettingsClass = Class of TDirectorySiteSettings;
  
  { --------------------------------------------------------------------
    TDirectorySitesListResponse
    --------------------------------------------------------------------}
  
  TDirectorySitesListResponse = Class(TGoogleBaseObject)
  Private
    FdirectorySites : TDirectorySitesListResponseTypedirectorySitesArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetdirectorySites(AIndex : Integer; AValue : TDirectorySitesListResponseTypedirectorySitesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property directorySites : TDirectorySitesListResponseTypedirectorySitesArray Index 0 Read FdirectorySites Write SetdirectorySites;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TDirectorySitesListResponseClass = Class of TDirectorySitesListResponse;
  
  { --------------------------------------------------------------------
    TEventTag
    --------------------------------------------------------------------}
  
  TEventTag = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    FcampaignId : String;
    FcampaignIdDimensionValue : TDimensionValue;
    FenabledByDefault : boolean;
    Fid : String;
    Fkind : String;
    Fname : String;
    FsiteFilterType : String;
    FsiteIds : TStringArray;
    FsslCompliant : boolean;
    Fstatus : String;
    FsubaccountId : String;
    F_type : String;
    Furl : String;
    FurlEscapeLevels : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetcampaignId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcampaignIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetenabledByDefault(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteFilterType(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsslCompliant(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure SeturlEscapeLevels(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 16 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property campaignId : String Index 24 Read FcampaignId Write SetcampaignId;
    Property campaignIdDimensionValue : TDimensionValue Index 32 Read FcampaignIdDimensionValue Write SetcampaignIdDimensionValue;
    Property enabledByDefault : boolean Index 40 Read FenabledByDefault Write SetenabledByDefault;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property name : String Index 64 Read Fname Write Setname;
    Property siteFilterType : String Index 72 Read FsiteFilterType Write SetsiteFilterType;
    Property siteIds : TStringArray Index 80 Read FsiteIds Write SetsiteIds;
    Property sslCompliant : boolean Index 88 Read FsslCompliant Write SetsslCompliant;
    Property status : String Index 96 Read Fstatus Write Setstatus;
    Property subaccountId : String Index 104 Read FsubaccountId Write SetsubaccountId;
    Property _type : String Index 112 Read F_type Write Set_type;
    Property url : String Index 120 Read Furl Write Seturl;
    Property urlEscapeLevels : integer Index 128 Read FurlEscapeLevels Write SeturlEscapeLevels;
  end;
  TEventTagClass = Class of TEventTag;
  
  { --------------------------------------------------------------------
    TEventTagOverride
    --------------------------------------------------------------------}
  
  TEventTagOverride = Class(TGoogleBaseObject)
  Private
    Fenabled : boolean;
    Fid : String;
  Protected
    //Property setters
    Procedure Setenabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property enabled : boolean Index 0 Read Fenabled Write Setenabled;
    Property id : String Index 8 Read Fid Write Setid;
  end;
  TEventTagOverrideClass = Class of TEventTagOverride;
  
  { --------------------------------------------------------------------
    TEventTagsListResponse
    --------------------------------------------------------------------}
  
  TEventTagsListResponse = Class(TGoogleBaseObject)
  Private
    FeventTags : TEventTagsListResponseTypeeventTagsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SeteventTags(AIndex : Integer; AValue : TEventTagsListResponseTypeeventTagsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property eventTags : TEventTagsListResponseTypeeventTagsArray Index 0 Read FeventTags Write SeteventTags;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TEventTagsListResponseClass = Class of TEventTagsListResponse;
  
  { --------------------------------------------------------------------
    TFileTypeurls
    --------------------------------------------------------------------}
  
  TFileTypeurls = Class(TGoogleBaseObject)
  Private
    FapiUrl : String;
    FbrowserUrl : String;
  Protected
    //Property setters
    Procedure SetapiUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbrowserUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property apiUrl : String Index 0 Read FapiUrl Write SetapiUrl;
    Property browserUrl : String Index 8 Read FbrowserUrl Write SetbrowserUrl;
  end;
  TFileTypeurlsClass = Class of TFileTypeurls;
  
  { --------------------------------------------------------------------
    TFile
    --------------------------------------------------------------------}
  
  TFile = Class(TGoogleBaseObject)
  Private
    FdateRange : TDateRange;
    Fetag : String;
    FfileName : String;
    Fformat : String;
    Fid : String;
    Fkind : String;
    FlastModifiedTime : String;
    FreportId : String;
    Fstatus : String;
    Furls : TFileTypeurls;
  Protected
    //Property setters
    Procedure SetdateRange(AIndex : Integer; AValue : TDateRange); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileName(AIndex : Integer; AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportId(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Seturls(AIndex : Integer; AValue : TFileTypeurls); virtual;
  Public
  Published
    Property dateRange : TDateRange Index 0 Read FdateRange Write SetdateRange;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property fileName : String Index 16 Read FfileName Write SetfileName;
    Property format : String Index 24 Read Fformat Write Setformat;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property lastModifiedTime : String Index 48 Read FlastModifiedTime Write SetlastModifiedTime;
    Property reportId : String Index 56 Read FreportId Write SetreportId;
    Property status : String Index 64 Read Fstatus Write Setstatus;
    Property urls : TFileTypeurls Index 72 Read Furls Write Seturls;
  end;
  TFileClass = Class of TFile;
  
  { --------------------------------------------------------------------
    TFileList
    --------------------------------------------------------------------}
  
  TFileList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TFileListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TFileListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TFileListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TFileListClass = Class of TFileList;
  
  { --------------------------------------------------------------------
    TFlight
    --------------------------------------------------------------------}
  
  TFlight = Class(TGoogleBaseObject)
  Private
    FendDate : TDate;
    FrateOrCost : String;
    FstartDate : TDate;
    Funits : String;
  Protected
    //Property setters
    Procedure SetendDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure SetrateOrCost(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setunits(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property endDate : TDate Index 0 Read FendDate Write SetendDate;
    Property rateOrCost : String Index 8 Read FrateOrCost Write SetrateOrCost;
    Property startDate : TDate Index 16 Read FstartDate Write SetstartDate;
    Property units : String Index 24 Read Funits Write Setunits;
  end;
  TFlightClass = Class of TFlight;
  
  { --------------------------------------------------------------------
    TFloodlightActivitiesGenerateTagResponse
    --------------------------------------------------------------------}
  
  TFloodlightActivitiesGenerateTagResponse = Class(TGoogleBaseObject)
  Private
    FfloodlightActivityTag : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetfloodlightActivityTag(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property floodlightActivityTag : String Index 0 Read FfloodlightActivityTag Write SetfloodlightActivityTag;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TFloodlightActivitiesGenerateTagResponseClass = Class of TFloodlightActivitiesGenerateTagResponse;
  
  { --------------------------------------------------------------------
    TFloodlightActivitiesListResponse
    --------------------------------------------------------------------}
  
  TFloodlightActivitiesListResponse = Class(TGoogleBaseObject)
  Private
    FfloodlightActivities : TFloodlightActivitiesListResponseTypefloodlightActivitiesArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetfloodlightActivities(AIndex : Integer; AValue : TFloodlightActivitiesListResponseTypefloodlightActivitiesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property floodlightActivities : TFloodlightActivitiesListResponseTypefloodlightActivitiesArray Index 0 Read FfloodlightActivities Write SetfloodlightActivities;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TFloodlightActivitiesListResponseClass = Class of TFloodlightActivitiesListResponse;
  
  { --------------------------------------------------------------------
    TFloodlightActivity
    --------------------------------------------------------------------}
  
  TFloodlightActivity = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    FcacheBustingType : String;
    FcountingMethod : String;
    FdefaultTags : TFloodlightActivityTypedefaultTagsArray;
    FexpectedUrl : String;
    FfloodlightActivityGroupId : String;
    FfloodlightActivityGroupName : String;
    FfloodlightActivityGroupTagString : String;
    FfloodlightActivityGroupType : String;
    FfloodlightConfigurationId : String;
    FfloodlightConfigurationIdDimensionValue : TDimensionValue;
    Fhidden : boolean;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    FimageTagEnabled : boolean;
    Fkind : String;
    Fname : String;
    Fnotes : String;
    FpublisherTags : TFloodlightActivityTypepublisherTagsArray;
    Fsecure : boolean;
    FsslCompliant : boolean;
    FsslRequired : boolean;
    FsubaccountId : String;
    FtagFormat : String;
    FtagString : String;
    FuserDefinedVariableTypes : TStringArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetcacheBustingType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountingMethod(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultTags(AIndex : Integer; AValue : TFloodlightActivityTypedefaultTagsArray); virtual;
    Procedure SetexpectedUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightActivityGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightActivityGroupName(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightActivityGroupTagString(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightActivityGroupType(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightConfigurationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightConfigurationIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Sethidden(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetimageTagEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublisherTags(AIndex : Integer; AValue : TFloodlightActivityTypepublisherTagsArray); virtual;
    Procedure Setsecure(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsslCompliant(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsslRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettagFormat(AIndex : Integer; AValue : String); virtual;
    Procedure SettagString(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserDefinedVariableTypes(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 16 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property cacheBustingType : String Index 24 Read FcacheBustingType Write SetcacheBustingType;
    Property countingMethod : String Index 32 Read FcountingMethod Write SetcountingMethod;
    Property defaultTags : TFloodlightActivityTypedefaultTagsArray Index 40 Read FdefaultTags Write SetdefaultTags;
    Property expectedUrl : String Index 48 Read FexpectedUrl Write SetexpectedUrl;
    Property floodlightActivityGroupId : String Index 56 Read FfloodlightActivityGroupId Write SetfloodlightActivityGroupId;
    Property floodlightActivityGroupName : String Index 64 Read FfloodlightActivityGroupName Write SetfloodlightActivityGroupName;
    Property floodlightActivityGroupTagString : String Index 72 Read FfloodlightActivityGroupTagString Write SetfloodlightActivityGroupTagString;
    Property floodlightActivityGroupType : String Index 80 Read FfloodlightActivityGroupType Write SetfloodlightActivityGroupType;
    Property floodlightConfigurationId : String Index 88 Read FfloodlightConfigurationId Write SetfloodlightConfigurationId;
    Property floodlightConfigurationIdDimensionValue : TDimensionValue Index 96 Read FfloodlightConfigurationIdDimensionValue Write SetfloodlightConfigurationIdDimensionValue;
    Property hidden : boolean Index 104 Read Fhidden Write Sethidden;
    Property id : String Index 112 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 120 Read FidDimensionValue Write SetidDimensionValue;
    Property imageTagEnabled : boolean Index 128 Read FimageTagEnabled Write SetimageTagEnabled;
    Property kind : String Index 136 Read Fkind Write Setkind;
    Property name : String Index 144 Read Fname Write Setname;
    Property notes : String Index 152 Read Fnotes Write Setnotes;
    Property publisherTags : TFloodlightActivityTypepublisherTagsArray Index 160 Read FpublisherTags Write SetpublisherTags;
    Property secure : boolean Index 168 Read Fsecure Write Setsecure;
    Property sslCompliant : boolean Index 176 Read FsslCompliant Write SetsslCompliant;
    Property sslRequired : boolean Index 184 Read FsslRequired Write SetsslRequired;
    Property subaccountId : String Index 192 Read FsubaccountId Write SetsubaccountId;
    Property tagFormat : String Index 200 Read FtagFormat Write SettagFormat;
    Property tagString : String Index 208 Read FtagString Write SettagString;
    Property userDefinedVariableTypes : TStringArray Index 216 Read FuserDefinedVariableTypes Write SetuserDefinedVariableTypes;
  end;
  TFloodlightActivityClass = Class of TFloodlightActivity;
  
  { --------------------------------------------------------------------
    TFloodlightActivityDynamicTag
    --------------------------------------------------------------------}
  
  TFloodlightActivityDynamicTag = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : String;
    Ftag : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Settag(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
    Property tag : String Index 16 Read Ftag Write Settag;
  end;
  TFloodlightActivityDynamicTagClass = Class of TFloodlightActivityDynamicTag;
  
  { --------------------------------------------------------------------
    TFloodlightActivityGroup
    --------------------------------------------------------------------}
  
  TFloodlightActivityGroup = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    FfloodlightConfigurationId : String;
    FfloodlightConfigurationIdDimensionValue : TDimensionValue;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    Fkind : String;
    Fname : String;
    FsubaccountId : String;
    FtagString : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetfloodlightConfigurationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightConfigurationIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettagString(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 16 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property floodlightConfigurationId : String Index 24 Read FfloodlightConfigurationId Write SetfloodlightConfigurationId;
    Property floodlightConfigurationIdDimensionValue : TDimensionValue Index 32 Read FfloodlightConfigurationIdDimensionValue Write SetfloodlightConfigurationIdDimensionValue;
    Property id : String Index 40 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 48 Read FidDimensionValue Write SetidDimensionValue;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property name : String Index 64 Read Fname Write Setname;
    Property subaccountId : String Index 72 Read FsubaccountId Write SetsubaccountId;
    Property tagString : String Index 80 Read FtagString Write SettagString;
    Property _type : String Index 88 Read F_type Write Set_type;
  end;
  TFloodlightActivityGroupClass = Class of TFloodlightActivityGroup;
  
  { --------------------------------------------------------------------
    TFloodlightActivityGroupsListResponse
    --------------------------------------------------------------------}
  
  TFloodlightActivityGroupsListResponse = Class(TGoogleBaseObject)
  Private
    FfloodlightActivityGroups : TFloodlightActivityGroupsListResponseTypefloodlightActivityGroupsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetfloodlightActivityGroups(AIndex : Integer; AValue : TFloodlightActivityGroupsListResponseTypefloodlightActivityGroupsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property floodlightActivityGroups : TFloodlightActivityGroupsListResponseTypefloodlightActivityGroupsArray Index 0 Read FfloodlightActivityGroups Write SetfloodlightActivityGroups;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TFloodlightActivityGroupsListResponseClass = Class of TFloodlightActivityGroupsListResponse;
  
  { --------------------------------------------------------------------
    TFloodlightActivityPublisherDynamicTag
    --------------------------------------------------------------------}
  
  TFloodlightActivityPublisherDynamicTag = Class(TGoogleBaseObject)
  Private
    FclickThrough : boolean;
    FdirectorySiteId : String;
    FdynamicTag : TFloodlightActivityDynamicTag;
    FsiteId : String;
    FsiteIdDimensionValue : TDimensionValue;
    FviewThrough : boolean;
  Protected
    //Property setters
    Procedure SetclickThrough(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdirectorySiteId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdynamicTag(AIndex : Integer; AValue : TFloodlightActivityDynamicTag); virtual;
    Procedure SetsiteId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetviewThrough(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property clickThrough : boolean Index 0 Read FclickThrough Write SetclickThrough;
    Property directorySiteId : String Index 8 Read FdirectorySiteId Write SetdirectorySiteId;
    Property dynamicTag : TFloodlightActivityDynamicTag Index 16 Read FdynamicTag Write SetdynamicTag;
    Property siteId : String Index 24 Read FsiteId Write SetsiteId;
    Property siteIdDimensionValue : TDimensionValue Index 32 Read FsiteIdDimensionValue Write SetsiteIdDimensionValue;
    Property viewThrough : boolean Index 40 Read FviewThrough Write SetviewThrough;
  end;
  TFloodlightActivityPublisherDynamicTagClass = Class of TFloodlightActivityPublisherDynamicTag;
  
  { --------------------------------------------------------------------
    TFloodlightConfiguration
    --------------------------------------------------------------------}
  
  TFloodlightConfiguration = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    FanalyticsDataSharingEnabled : boolean;
    FexposureToConversionEnabled : boolean;
    FfirstDayOfWeek : String;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    Fkind : String;
    FlookbackConfiguration : TLookbackConfiguration;
    FnaturalSearchConversionAttributionOption : String;
    FomnitureSettings : TOmnitureSettings;
    FsslRequired : boolean;
    FstandardVariableTypes : TStringArray;
    FsubaccountId : String;
    FtagSettings : TTagSettings;
    FuserDefinedVariableConfigurations : TFloodlightConfigurationTypeuserDefinedVariableConfigurationsArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetanalyticsDataSharingEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetexposureToConversionEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfirstDayOfWeek(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); virtual;
    Procedure SetnaturalSearchConversionAttributionOption(AIndex : Integer; AValue : String); virtual;
    Procedure SetomnitureSettings(AIndex : Integer; AValue : TOmnitureSettings); virtual;
    Procedure SetsslRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstandardVariableTypes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettagSettings(AIndex : Integer; AValue : TTagSettings); virtual;
    Procedure SetuserDefinedVariableConfigurations(AIndex : Integer; AValue : TFloodlightConfigurationTypeuserDefinedVariableConfigurationsArray); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 16 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property analyticsDataSharingEnabled : boolean Index 24 Read FanalyticsDataSharingEnabled Write SetanalyticsDataSharingEnabled;
    Property exposureToConversionEnabled : boolean Index 32 Read FexposureToConversionEnabled Write SetexposureToConversionEnabled;
    Property firstDayOfWeek : String Index 40 Read FfirstDayOfWeek Write SetfirstDayOfWeek;
    Property id : String Index 48 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 56 Read FidDimensionValue Write SetidDimensionValue;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property lookbackConfiguration : TLookbackConfiguration Index 72 Read FlookbackConfiguration Write SetlookbackConfiguration;
    Property naturalSearchConversionAttributionOption : String Index 80 Read FnaturalSearchConversionAttributionOption Write SetnaturalSearchConversionAttributionOption;
    Property omnitureSettings : TOmnitureSettings Index 88 Read FomnitureSettings Write SetomnitureSettings;
    Property sslRequired : boolean Index 96 Read FsslRequired Write SetsslRequired;
    Property standardVariableTypes : TStringArray Index 104 Read FstandardVariableTypes Write SetstandardVariableTypes;
    Property subaccountId : String Index 112 Read FsubaccountId Write SetsubaccountId;
    Property tagSettings : TTagSettings Index 120 Read FtagSettings Write SettagSettings;
    Property userDefinedVariableConfigurations : TFloodlightConfigurationTypeuserDefinedVariableConfigurationsArray Index 128 Read FuserDefinedVariableConfigurations Write SetuserDefinedVariableConfigurations;
  end;
  TFloodlightConfigurationClass = Class of TFloodlightConfiguration;
  
  { --------------------------------------------------------------------
    TFloodlightConfigurationsListResponse
    --------------------------------------------------------------------}
  
  TFloodlightConfigurationsListResponse = Class(TGoogleBaseObject)
  Private
    FfloodlightConfigurations : TFloodlightConfigurationsListResponseTypefloodlightConfigurationsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetfloodlightConfigurations(AIndex : Integer; AValue : TFloodlightConfigurationsListResponseTypefloodlightConfigurationsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property floodlightConfigurations : TFloodlightConfigurationsListResponseTypefloodlightConfigurationsArray Index 0 Read FfloodlightConfigurations Write SetfloodlightConfigurations;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TFloodlightConfigurationsListResponseClass = Class of TFloodlightConfigurationsListResponse;
  
  { --------------------------------------------------------------------
    TFloodlightReportCompatibleFields
    --------------------------------------------------------------------}
  
  TFloodlightReportCompatibleFields = Class(TGoogleBaseObject)
  Private
    FdimensionFilters : TFloodlightReportCompatibleFieldsTypedimensionFiltersArray;
    Fdimensions : TFloodlightReportCompatibleFieldsTypedimensionsArray;
    Fkind : String;
    Fmetrics : TFloodlightReportCompatibleFieldsTypemetricsArray;
  Protected
    //Property setters
    Procedure SetdimensionFilters(AIndex : Integer; AValue : TFloodlightReportCompatibleFieldsTypedimensionFiltersArray); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TFloodlightReportCompatibleFieldsTypedimensionsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TFloodlightReportCompatibleFieldsTypemetricsArray); virtual;
  Public
  Published
    Property dimensionFilters : TFloodlightReportCompatibleFieldsTypedimensionFiltersArray Index 0 Read FdimensionFilters Write SetdimensionFilters;
    Property dimensions : TFloodlightReportCompatibleFieldsTypedimensionsArray Index 8 Read Fdimensions Write Setdimensions;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property metrics : TFloodlightReportCompatibleFieldsTypemetricsArray Index 24 Read Fmetrics Write Setmetrics;
  end;
  TFloodlightReportCompatibleFieldsClass = Class of TFloodlightReportCompatibleFields;
  
  { --------------------------------------------------------------------
    TFrequencyCap
    --------------------------------------------------------------------}
  
  TFrequencyCap = Class(TGoogleBaseObject)
  Private
    Fduration : String;
    Fimpressions : String;
  Protected
    //Property setters
    Procedure Setduration(AIndex : Integer; AValue : String); virtual;
    Procedure Setimpressions(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property duration : String Index 0 Read Fduration Write Setduration;
    Property impressions : String Index 8 Read Fimpressions Write Setimpressions;
  end;
  TFrequencyCapClass = Class of TFrequencyCap;
  
  { --------------------------------------------------------------------
    TFsCommand
    --------------------------------------------------------------------}
  
  TFsCommand = Class(TGoogleBaseObject)
  Private
    Fleft : integer;
    FpositionOption : String;
    Ftop : integer;
    FwindowHeight : integer;
    FwindowWidth : integer;
  Protected
    //Property setters
    Procedure Setleft(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpositionOption(AIndex : Integer; AValue : String); virtual;
    Procedure Settop(AIndex : Integer; AValue : integer); virtual;
    Procedure SetwindowHeight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetwindowWidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property left : integer Index 0 Read Fleft Write Setleft;
    Property positionOption : String Index 8 Read FpositionOption Write SetpositionOption;
    Property top : integer Index 16 Read Ftop Write Settop;
    Property windowHeight : integer Index 24 Read FwindowHeight Write SetwindowHeight;
    Property windowWidth : integer Index 32 Read FwindowWidth Write SetwindowWidth;
  end;
  TFsCommandClass = Class of TFsCommand;
  
  { --------------------------------------------------------------------
    TGeoTargeting
    --------------------------------------------------------------------}
  
  TGeoTargeting = Class(TGoogleBaseObject)
  Private
    Fcities : TGeoTargetingTypecitiesArray;
    Fcountries : TGeoTargetingTypecountriesArray;
    FexcludeCountries : boolean;
    Fmetros : TGeoTargetingTypemetrosArray;
    FpostalCodes : TGeoTargetingTypepostalCodesArray;
    Fregions : TGeoTargetingTyperegionsArray;
  Protected
    //Property setters
    Procedure Setcities(AIndex : Integer; AValue : TGeoTargetingTypecitiesArray); virtual;
    Procedure Setcountries(AIndex : Integer; AValue : TGeoTargetingTypecountriesArray); virtual;
    Procedure SetexcludeCountries(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmetros(AIndex : Integer; AValue : TGeoTargetingTypemetrosArray); virtual;
    Procedure SetpostalCodes(AIndex : Integer; AValue : TGeoTargetingTypepostalCodesArray); virtual;
    Procedure Setregions(AIndex : Integer; AValue : TGeoTargetingTyperegionsArray); virtual;
  Public
  Published
    Property cities : TGeoTargetingTypecitiesArray Index 0 Read Fcities Write Setcities;
    Property countries : TGeoTargetingTypecountriesArray Index 8 Read Fcountries Write Setcountries;
    Property excludeCountries : boolean Index 16 Read FexcludeCountries Write SetexcludeCountries;
    Property metros : TGeoTargetingTypemetrosArray Index 24 Read Fmetros Write Setmetros;
    Property postalCodes : TGeoTargetingTypepostalCodesArray Index 32 Read FpostalCodes Write SetpostalCodes;
    Property regions : TGeoTargetingTyperegionsArray Index 40 Read Fregions Write Setregions;
  end;
  TGeoTargetingClass = Class of TGeoTargeting;
  
  { --------------------------------------------------------------------
    TInventoryItem
    --------------------------------------------------------------------}
  
  TInventoryItem = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadSlots : TInventoryItemTypeadSlotsArray;
    FadvertiserId : String;
    FcontentCategoryId : String;
    FestimatedClickThroughRate : String;
    FestimatedConversionRate : String;
    Fid : String;
    FinPlan : boolean;
    Fkind : String;
    FlastModifiedInfo : TLastModifiedInfo;
    Fname : String;
    FnegotiationChannelId : String;
    ForderId : String;
    FplacementStrategyId : String;
    Fpricing : TPricing;
    FprojectId : String;
    FrfpId : String;
    FsiteId : String;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadSlots(AIndex : Integer; AValue : TInventoryItemTypeadSlotsArray); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentCategoryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetestimatedClickThroughRate(AIndex : Integer; AValue : String); virtual;
    Procedure SetestimatedConversionRate(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinPlan(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetnegotiationChannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetorderId(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementStrategyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setpricing(AIndex : Integer; AValue : TPricing); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrfpId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property adSlots : TInventoryItemTypeadSlotsArray Index 8 Read FadSlots Write SetadSlots;
    Property advertiserId : String Index 16 Read FadvertiserId Write SetadvertiserId;
    Property contentCategoryId : String Index 24 Read FcontentCategoryId Write SetcontentCategoryId;
    Property estimatedClickThroughRate : String Index 32 Read FestimatedClickThroughRate Write SetestimatedClickThroughRate;
    Property estimatedConversionRate : String Index 40 Read FestimatedConversionRate Write SetestimatedConversionRate;
    Property id : String Index 48 Read Fid Write Setid;
    Property inPlan : boolean Index 56 Read FinPlan Write SetinPlan;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property lastModifiedInfo : TLastModifiedInfo Index 72 Read FlastModifiedInfo Write SetlastModifiedInfo;
    Property name : String Index 80 Read Fname Write Setname;
    Property negotiationChannelId : String Index 88 Read FnegotiationChannelId Write SetnegotiationChannelId;
    Property orderId : String Index 96 Read ForderId Write SetorderId;
    Property placementStrategyId : String Index 104 Read FplacementStrategyId Write SetplacementStrategyId;
    Property pricing : TPricing Index 112 Read Fpricing Write Setpricing;
    Property projectId : String Index 120 Read FprojectId Write SetprojectId;
    Property rfpId : String Index 128 Read FrfpId Write SetrfpId;
    Property siteId : String Index 136 Read FsiteId Write SetsiteId;
    Property subaccountId : String Index 144 Read FsubaccountId Write SetsubaccountId;
  end;
  TInventoryItemClass = Class of TInventoryItem;
  
  { --------------------------------------------------------------------
    TInventoryItemsListResponse
    --------------------------------------------------------------------}
  
  TInventoryItemsListResponse = Class(TGoogleBaseObject)
  Private
    FinventoryItems : TInventoryItemsListResponseTypeinventoryItemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetinventoryItems(AIndex : Integer; AValue : TInventoryItemsListResponseTypeinventoryItemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property inventoryItems : TInventoryItemsListResponseTypeinventoryItemsArray Index 0 Read FinventoryItems Write SetinventoryItems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TInventoryItemsListResponseClass = Class of TInventoryItemsListResponse;
  
  { --------------------------------------------------------------------
    TKeyValueTargetingExpression
    --------------------------------------------------------------------}
  
  TKeyValueTargetingExpression = Class(TGoogleBaseObject)
  Private
    Fexpression : String;
  Protected
    //Property setters
    Procedure Setexpression(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property expression : String Index 0 Read Fexpression Write Setexpression;
  end;
  TKeyValueTargetingExpressionClass = Class of TKeyValueTargetingExpression;
  
  { --------------------------------------------------------------------
    TLandingPage
    --------------------------------------------------------------------}
  
  TLandingPage = Class(TGoogleBaseObject)
  Private
    Fdefault : boolean;
    Fid : String;
    Fkind : String;
    Fname : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setdefault(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property default : boolean Index 0 Read Fdefault Write Setdefault;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
    Property url : String Index 32 Read Furl Write Seturl;
  end;
  TLandingPageClass = Class of TLandingPage;
  
  { --------------------------------------------------------------------
    TLandingPagesListResponse
    --------------------------------------------------------------------}
  
  TLandingPagesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FlandingPages : TLandingPagesListResponseTypelandingPagesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlandingPages(AIndex : Integer; AValue : TLandingPagesListResponseTypelandingPagesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property landingPages : TLandingPagesListResponseTypelandingPagesArray Index 8 Read FlandingPages Write SetlandingPages;
  end;
  TLandingPagesListResponseClass = Class of TLandingPagesListResponse;
  
  { --------------------------------------------------------------------
    TLastModifiedInfo
    --------------------------------------------------------------------}
  
  TLastModifiedInfo = Class(TGoogleBaseObject)
  Private
    Ftime : String;
  Protected
    //Property setters
    Procedure Settime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property time : String Index 0 Read Ftime Write Settime;
  end;
  TLastModifiedInfoClass = Class of TLastModifiedInfo;
  
  { --------------------------------------------------------------------
    TListPopulationClause
    --------------------------------------------------------------------}
  
  TListPopulationClause = Class(TGoogleBaseObject)
  Private
    Fterms : TListPopulationClauseTypetermsArray;
  Protected
    //Property setters
    Procedure Setterms(AIndex : Integer; AValue : TListPopulationClauseTypetermsArray); virtual;
  Public
  Published
    Property terms : TListPopulationClauseTypetermsArray Index 0 Read Fterms Write Setterms;
  end;
  TListPopulationClauseClass = Class of TListPopulationClause;
  
  { --------------------------------------------------------------------
    TListPopulationRule
    --------------------------------------------------------------------}
  
  TListPopulationRule = Class(TGoogleBaseObject)
  Private
    FfloodlightActivityId : String;
    FfloodlightActivityName : String;
    FlistPopulationClauses : TListPopulationRuleTypelistPopulationClausesArray;
  Protected
    //Property setters
    Procedure SetfloodlightActivityId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightActivityName(AIndex : Integer; AValue : String); virtual;
    Procedure SetlistPopulationClauses(AIndex : Integer; AValue : TListPopulationRuleTypelistPopulationClausesArray); virtual;
  Public
  Published
    Property floodlightActivityId : String Index 0 Read FfloodlightActivityId Write SetfloodlightActivityId;
    Property floodlightActivityName : String Index 8 Read FfloodlightActivityName Write SetfloodlightActivityName;
    Property listPopulationClauses : TListPopulationRuleTypelistPopulationClausesArray Index 16 Read FlistPopulationClauses Write SetlistPopulationClauses;
  end;
  TListPopulationRuleClass = Class of TListPopulationRule;
  
  { --------------------------------------------------------------------
    TListPopulationTerm
    --------------------------------------------------------------------}
  
  TListPopulationTerm = Class(TGoogleBaseObject)
  Private
    Fcontains : boolean;
    Fnegation : boolean;
    F_operator : String;
    FremarketingListId : String;
    F_type : String;
    Fvalue : String;
    FvariableFriendlyName : String;
    FvariableName : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcontains(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setnegation(AIndex : Integer; AValue : boolean); virtual;
    Procedure Set_operator(AIndex : Integer; AValue : String); virtual;
    Procedure SetremarketingListId(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariableFriendlyName(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariableName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property contains : boolean Index 0 Read Fcontains Write Setcontains;
    Property negation : boolean Index 8 Read Fnegation Write Setnegation;
    Property _operator : String Index 16 Read F_operator Write Set_operator;
    Property remarketingListId : String Index 24 Read FremarketingListId Write SetremarketingListId;
    Property _type : String Index 32 Read F_type Write Set_type;
    Property value : String Index 40 Read Fvalue Write Setvalue;
    Property variableFriendlyName : String Index 48 Read FvariableFriendlyName Write SetvariableFriendlyName;
    Property variableName : String Index 56 Read FvariableName Write SetvariableName;
  end;
  TListPopulationTermClass = Class of TListPopulationTerm;
  
  { --------------------------------------------------------------------
    TListTargetingExpression
    --------------------------------------------------------------------}
  
  TListTargetingExpression = Class(TGoogleBaseObject)
  Private
    Fexpression : String;
  Protected
    //Property setters
    Procedure Setexpression(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property expression : String Index 0 Read Fexpression Write Setexpression;
  end;
  TListTargetingExpressionClass = Class of TListTargetingExpression;
  
  { --------------------------------------------------------------------
    TLookbackConfiguration
    --------------------------------------------------------------------}
  
  TLookbackConfiguration = Class(TGoogleBaseObject)
  Private
    FclickDuration : integer;
    FpostImpressionActivitiesDuration : integer;
  Protected
    //Property setters
    Procedure SetclickDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpostImpressionActivitiesDuration(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property clickDuration : integer Index 0 Read FclickDuration Write SetclickDuration;
    Property postImpressionActivitiesDuration : integer Index 8 Read FpostImpressionActivitiesDuration Write SetpostImpressionActivitiesDuration;
  end;
  TLookbackConfigurationClass = Class of TLookbackConfiguration;
  
  { --------------------------------------------------------------------
    TMetric
    --------------------------------------------------------------------}
  
  TMetric = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TMetricClass = Class of TMetric;
  
  { --------------------------------------------------------------------
    TMetro
    --------------------------------------------------------------------}
  
  TMetro = Class(TGoogleBaseObject)
  Private
    FcountryCode : String;
    FcountryDartId : String;
    FdartId : String;
    FdmaId : String;
    Fkind : String;
    FmetroCode : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetcountryCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountryDartId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdartId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdmaId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmetroCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property countryCode : String Index 0 Read FcountryCode Write SetcountryCode;
    Property countryDartId : String Index 8 Read FcountryDartId Write SetcountryDartId;
    Property dartId : String Index 16 Read FdartId Write SetdartId;
    Property dmaId : String Index 24 Read FdmaId Write SetdmaId;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property metroCode : String Index 40 Read FmetroCode Write SetmetroCode;
    Property name : String Index 48 Read Fname Write Setname;
  end;
  TMetroClass = Class of TMetro;
  
  { --------------------------------------------------------------------
    TMetrosListResponse
    --------------------------------------------------------------------}
  
  TMetrosListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fmetros : TMetrosListResponseTypemetrosArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetros(AIndex : Integer; AValue : TMetrosListResponseTypemetrosArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property metros : TMetrosListResponseTypemetrosArray Index 8 Read Fmetros Write Setmetros;
  end;
  TMetrosListResponseClass = Class of TMetrosListResponse;
  
  { --------------------------------------------------------------------
    TMobileCarrier
    --------------------------------------------------------------------}
  
  TMobileCarrier = Class(TGoogleBaseObject)
  Private
    FcountryCode : String;
    FcountryDartId : String;
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetcountryCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountryDartId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property countryCode : String Index 0 Read FcountryCode Write SetcountryCode;
    Property countryDartId : String Index 8 Read FcountryDartId Write SetcountryDartId;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
  end;
  TMobileCarrierClass = Class of TMobileCarrier;
  
  { --------------------------------------------------------------------
    TMobileCarriersListResponse
    --------------------------------------------------------------------}
  
  TMobileCarriersListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FmobileCarriers : TMobileCarriersListResponseTypemobileCarriersArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmobileCarriers(AIndex : Integer; AValue : TMobileCarriersListResponseTypemobileCarriersArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property mobileCarriers : TMobileCarriersListResponseTypemobileCarriersArray Index 8 Read FmobileCarriers Write SetmobileCarriers;
  end;
  TMobileCarriersListResponseClass = Class of TMobileCarriersListResponse;
  
  { --------------------------------------------------------------------
    TObjectFilter
    --------------------------------------------------------------------}
  
  TObjectFilter = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FobjectIds : TStringArray;
    Fstatus : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property objectIds : TStringArray Index 8 Read FobjectIds Write SetobjectIds;
    Property status : String Index 16 Read Fstatus Write Setstatus;
  end;
  TObjectFilterClass = Class of TObjectFilter;
  
  { --------------------------------------------------------------------
    TOffsetPosition
    --------------------------------------------------------------------}
  
  TOffsetPosition = Class(TGoogleBaseObject)
  Private
    Fleft : integer;
    Ftop : integer;
  Protected
    //Property setters
    Procedure Setleft(AIndex : Integer; AValue : integer); virtual;
    Procedure Settop(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property left : integer Index 0 Read Fleft Write Setleft;
    Property top : integer Index 8 Read Ftop Write Settop;
  end;
  TOffsetPositionClass = Class of TOffsetPosition;
  
  { --------------------------------------------------------------------
    TOmnitureSettings
    --------------------------------------------------------------------}
  
  TOmnitureSettings = Class(TGoogleBaseObject)
  Private
    FomnitureCostDataEnabled : boolean;
    FomnitureIntegrationEnabled : boolean;
  Protected
    //Property setters
    Procedure SetomnitureCostDataEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetomnitureIntegrationEnabled(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property omnitureCostDataEnabled : boolean Index 0 Read FomnitureCostDataEnabled Write SetomnitureCostDataEnabled;
    Property omnitureIntegrationEnabled : boolean Index 8 Read FomnitureIntegrationEnabled Write SetomnitureIntegrationEnabled;
  end;
  TOmnitureSettingsClass = Class of TOmnitureSettings;
  
  { --------------------------------------------------------------------
    TOperatingSystem
    --------------------------------------------------------------------}
  
  TOperatingSystem = Class(TGoogleBaseObject)
  Private
    FdartId : String;
    Fdesktop : boolean;
    Fkind : String;
    Fmobile : boolean;
    Fname : String;
  Protected
    //Property setters
    Procedure SetdartId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdesktop(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmobile(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dartId : String Index 0 Read FdartId Write SetdartId;
    Property desktop : boolean Index 8 Read Fdesktop Write Setdesktop;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property mobile : boolean Index 24 Read Fmobile Write Setmobile;
    Property name : String Index 32 Read Fname Write Setname;
  end;
  TOperatingSystemClass = Class of TOperatingSystem;
  
  { --------------------------------------------------------------------
    TOperatingSystemVersion
    --------------------------------------------------------------------}
  
  TOperatingSystemVersion = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    FmajorVersion : String;
    FminorVersion : String;
    Fname : String;
    FoperatingSystem : TOperatingSystem;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmajorVersion(AIndex : Integer; AValue : String); virtual;
    Procedure SetminorVersion(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoperatingSystem(AIndex : Integer; AValue : TOperatingSystem); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property majorVersion : String Index 16 Read FmajorVersion Write SetmajorVersion;
    Property minorVersion : String Index 24 Read FminorVersion Write SetminorVersion;
    Property name : String Index 32 Read Fname Write Setname;
    Property operatingSystem : TOperatingSystem Index 40 Read FoperatingSystem Write SetoperatingSystem;
  end;
  TOperatingSystemVersionClass = Class of TOperatingSystemVersion;
  
  { --------------------------------------------------------------------
    TOperatingSystemVersionsListResponse
    --------------------------------------------------------------------}
  
  TOperatingSystemVersionsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FoperatingSystemVersions : TOperatingSystemVersionsListResponseTypeoperatingSystemVersionsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetoperatingSystemVersions(AIndex : Integer; AValue : TOperatingSystemVersionsListResponseTypeoperatingSystemVersionsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property operatingSystemVersions : TOperatingSystemVersionsListResponseTypeoperatingSystemVersionsArray Index 8 Read FoperatingSystemVersions Write SetoperatingSystemVersions;
  end;
  TOperatingSystemVersionsListResponseClass = Class of TOperatingSystemVersionsListResponse;
  
  { --------------------------------------------------------------------
    TOperatingSystemsListResponse
    --------------------------------------------------------------------}
  
  TOperatingSystemsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FoperatingSystems : TOperatingSystemsListResponseTypeoperatingSystemsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetoperatingSystems(AIndex : Integer; AValue : TOperatingSystemsListResponseTypeoperatingSystemsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property operatingSystems : TOperatingSystemsListResponseTypeoperatingSystemsArray Index 8 Read FoperatingSystems Write SetoperatingSystems;
  end;
  TOperatingSystemsListResponseClass = Class of TOperatingSystemsListResponse;
  
  { --------------------------------------------------------------------
    TOptimizationActivity
    --------------------------------------------------------------------}
  
  TOptimizationActivity = Class(TGoogleBaseObject)
  Private
    FfloodlightActivityId : String;
    FfloodlightActivityIdDimensionValue : TDimensionValue;
    Fweight : integer;
  Protected
    //Property setters
    Procedure SetfloodlightActivityId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightActivityIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setweight(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property floodlightActivityId : String Index 0 Read FfloodlightActivityId Write SetfloodlightActivityId;
    Property floodlightActivityIdDimensionValue : TDimensionValue Index 8 Read FfloodlightActivityIdDimensionValue Write SetfloodlightActivityIdDimensionValue;
    Property weight : integer Index 16 Read Fweight Write Setweight;
  end;
  TOptimizationActivityClass = Class of TOptimizationActivity;
  
  { --------------------------------------------------------------------
    TOrder
    --------------------------------------------------------------------}
  
  TOrder = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FapproverUserProfileIds : TStringArray;
    FbuyerInvoiceId : String;
    FbuyerOrganizationName : String;
    Fcomments : String;
    Fcontacts : TOrderTypecontactsArray;
    Fid : String;
    Fkind : String;
    FlastModifiedInfo : TLastModifiedInfo;
    Fname : String;
    Fnotes : String;
    FplanningTermId : String;
    FprojectId : String;
    FsellerOrderId : String;
    FsellerOrganizationName : String;
    FsiteId : TStringArray;
    FsiteNames : TStringArray;
    FsubaccountId : String;
    FtermsAndConditions : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetapproverUserProfileIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetbuyerInvoiceId(AIndex : Integer; AValue : String); virtual;
    Procedure SetbuyerOrganizationName(AIndex : Integer; AValue : String); virtual;
    Procedure Setcomments(AIndex : Integer; AValue : String); virtual;
    Procedure Setcontacts(AIndex : Integer; AValue : TOrderTypecontactsArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure SetplanningTermId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsellerOrderId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsellerOrganizationName(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsiteNames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettermsAndConditions(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property approverUserProfileIds : TStringArray Index 16 Read FapproverUserProfileIds Write SetapproverUserProfileIds;
    Property buyerInvoiceId : String Index 24 Read FbuyerInvoiceId Write SetbuyerInvoiceId;
    Property buyerOrganizationName : String Index 32 Read FbuyerOrganizationName Write SetbuyerOrganizationName;
    Property comments : String Index 40 Read Fcomments Write Setcomments;
    Property contacts : TOrderTypecontactsArray Index 48 Read Fcontacts Write Setcontacts;
    Property id : String Index 56 Read Fid Write Setid;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property lastModifiedInfo : TLastModifiedInfo Index 72 Read FlastModifiedInfo Write SetlastModifiedInfo;
    Property name : String Index 80 Read Fname Write Setname;
    Property notes : String Index 88 Read Fnotes Write Setnotes;
    Property planningTermId : String Index 96 Read FplanningTermId Write SetplanningTermId;
    Property projectId : String Index 104 Read FprojectId Write SetprojectId;
    Property sellerOrderId : String Index 112 Read FsellerOrderId Write SetsellerOrderId;
    Property sellerOrganizationName : String Index 120 Read FsellerOrganizationName Write SetsellerOrganizationName;
    Property siteId : TStringArray Index 128 Read FsiteId Write SetsiteId;
    Property siteNames : TStringArray Index 136 Read FsiteNames Write SetsiteNames;
    Property subaccountId : String Index 144 Read FsubaccountId Write SetsubaccountId;
    Property termsAndConditions : String Index 152 Read FtermsAndConditions Write SettermsAndConditions;
  end;
  TOrderClass = Class of TOrder;
  
  { --------------------------------------------------------------------
    TOrderContact
    --------------------------------------------------------------------}
  
  TOrderContact = Class(TGoogleBaseObject)
  Private
    FcontactInfo : String;
    FcontactName : String;
    FcontactTitle : String;
    FcontactType : String;
    FsignatureUserProfileId : String;
  Protected
    //Property setters
    Procedure SetcontactInfo(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontactName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontactTitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontactType(AIndex : Integer; AValue : String); virtual;
    Procedure SetsignatureUserProfileId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property contactInfo : String Index 0 Read FcontactInfo Write SetcontactInfo;
    Property contactName : String Index 8 Read FcontactName Write SetcontactName;
    Property contactTitle : String Index 16 Read FcontactTitle Write SetcontactTitle;
    Property contactType : String Index 24 Read FcontactType Write SetcontactType;
    Property signatureUserProfileId : String Index 32 Read FsignatureUserProfileId Write SetsignatureUserProfileId;
  end;
  TOrderContactClass = Class of TOrderContact;
  
  { --------------------------------------------------------------------
    TOrderDocument
    --------------------------------------------------------------------}
  
  TOrderDocument = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FamendedOrderDocumentId : String;
    FapprovedByUserProfileIds : TStringArray;
    Fcancelled : boolean;
    FcreatedInfo : TLastModifiedInfo;
    FeffectiveDate : TDate;
    Fid : String;
    Fkind : String;
    ForderId : String;
    FprojectId : String;
    Fsigned : boolean;
    FsubaccountId : String;
    Ftitle : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetamendedOrderDocumentId(AIndex : Integer; AValue : String); virtual;
    Procedure SetapprovedByUserProfileIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setcancelled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcreatedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SeteffectiveDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetorderId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure Setsigned(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property amendedOrderDocumentId : String Index 16 Read FamendedOrderDocumentId Write SetamendedOrderDocumentId;
    Property approvedByUserProfileIds : TStringArray Index 24 Read FapprovedByUserProfileIds Write SetapprovedByUserProfileIds;
    Property cancelled : boolean Index 32 Read Fcancelled Write Setcancelled;
    Property createdInfo : TLastModifiedInfo Index 40 Read FcreatedInfo Write SetcreatedInfo;
    Property effectiveDate : TDate Index 48 Read FeffectiveDate Write SeteffectiveDate;
    Property id : String Index 56 Read Fid Write Setid;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property orderId : String Index 72 Read ForderId Write SetorderId;
    Property projectId : String Index 80 Read FprojectId Write SetprojectId;
    Property signed : boolean Index 88 Read Fsigned Write Setsigned;
    Property subaccountId : String Index 96 Read FsubaccountId Write SetsubaccountId;
    Property title : String Index 104 Read Ftitle Write Settitle;
    Property _type : String Index 112 Read F_type Write Set_type;
  end;
  TOrderDocumentClass = Class of TOrderDocument;
  
  { --------------------------------------------------------------------
    TOrderDocumentsListResponse
    --------------------------------------------------------------------}
  
  TOrderDocumentsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    ForderDocuments : TOrderDocumentsListResponseTypeorderDocumentsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetorderDocuments(AIndex : Integer; AValue : TOrderDocumentsListResponseTypeorderDocumentsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property orderDocuments : TOrderDocumentsListResponseTypeorderDocumentsArray Index 16 Read ForderDocuments Write SetorderDocuments;
  end;
  TOrderDocumentsListResponseClass = Class of TOrderDocumentsListResponse;
  
  { --------------------------------------------------------------------
    TOrdersListResponse
    --------------------------------------------------------------------}
  
  TOrdersListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Forders : TOrdersListResponseTypeordersArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setorders(AIndex : Integer; AValue : TOrdersListResponseTypeordersArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property orders : TOrdersListResponseTypeordersArray Index 16 Read Forders Write Setorders;
  end;
  TOrdersListResponseClass = Class of TOrdersListResponse;
  
  { --------------------------------------------------------------------
    TPathToConversionReportCompatibleFields
    --------------------------------------------------------------------}
  
  TPathToConversionReportCompatibleFields = Class(TGoogleBaseObject)
  Private
    FconversionDimensions : TPathToConversionReportCompatibleFieldsTypeconversionDimensionsArray;
    FcustomFloodlightVariables : TPathToConversionReportCompatibleFieldsTypecustomFloodlightVariablesArray;
    Fkind : String;
    Fmetrics : TPathToConversionReportCompatibleFieldsTypemetricsArray;
    FperInteractionDimensions : TPathToConversionReportCompatibleFieldsTypeperInteractionDimensionsArray;
  Protected
    //Property setters
    Procedure SetconversionDimensions(AIndex : Integer; AValue : TPathToConversionReportCompatibleFieldsTypeconversionDimensionsArray); virtual;
    Procedure SetcustomFloodlightVariables(AIndex : Integer; AValue : TPathToConversionReportCompatibleFieldsTypecustomFloodlightVariablesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TPathToConversionReportCompatibleFieldsTypemetricsArray); virtual;
    Procedure SetperInteractionDimensions(AIndex : Integer; AValue : TPathToConversionReportCompatibleFieldsTypeperInteractionDimensionsArray); virtual;
  Public
  Published
    Property conversionDimensions : TPathToConversionReportCompatibleFieldsTypeconversionDimensionsArray Index 0 Read FconversionDimensions Write SetconversionDimensions;
    Property customFloodlightVariables : TPathToConversionReportCompatibleFieldsTypecustomFloodlightVariablesArray Index 8 Read FcustomFloodlightVariables Write SetcustomFloodlightVariables;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property metrics : TPathToConversionReportCompatibleFieldsTypemetricsArray Index 24 Read Fmetrics Write Setmetrics;
    Property perInteractionDimensions : TPathToConversionReportCompatibleFieldsTypeperInteractionDimensionsArray Index 32 Read FperInteractionDimensions Write SetperInteractionDimensions;
  end;
  TPathToConversionReportCompatibleFieldsClass = Class of TPathToConversionReportCompatibleFields;
  
  { --------------------------------------------------------------------
    TPlacement
    --------------------------------------------------------------------}
  
  TPlacement = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    Farchived : boolean;
    FcampaignId : String;
    FcampaignIdDimensionValue : TDimensionValue;
    Fcomment : String;
    Fcompatibility : String;
    FcontentCategoryId : String;
    FcreateInfo : TLastModifiedInfo;
    FdirectorySiteId : String;
    FdirectorySiteIdDimensionValue : TDimensionValue;
    FexternalId : String;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    FkeyName : String;
    Fkind : String;
    FlastModifiedInfo : TLastModifiedInfo;
    FlookbackConfiguration : TLookbackConfiguration;
    Fname : String;
    FpaymentApproved : boolean;
    FpaymentSource : String;
    FplacementGroupId : String;
    FplacementGroupIdDimensionValue : TDimensionValue;
    FplacementStrategyId : String;
    FpricingSchedule : TPricingSchedule;
    Fprimary : boolean;
    FpublisherUpdateInfo : TLastModifiedInfo;
    FsiteId : String;
    FsiteIdDimensionValue : TDimensionValue;
    Fsize : TSize;
    FsslRequired : boolean;
    Fstatus : String;
    FsubaccountId : String;
    FtagFormats : TStringArray;
    FtagSetting : TTagSetting;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setarchived(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcampaignId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcampaignIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setcomment(AIndex : Integer; AValue : String); virtual;
    Procedure Setcompatibility(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentCategoryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreateInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SetdirectorySiteId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdirectorySiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetexternalId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetkeyName(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpaymentApproved(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpaymentSource(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementGroupIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetplacementStrategyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpricingSchedule(AIndex : Integer; AValue : TPricingSchedule); virtual;
    Procedure Setprimary(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpublisherUpdateInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SetsiteId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setsize(AIndex : Integer; AValue : TSize); virtual;
    Procedure SetsslRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettagFormats(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SettagSetting(AIndex : Integer; AValue : TTagSetting); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 16 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property archived : boolean Index 24 Read Farchived Write Setarchived;
    Property campaignId : String Index 32 Read FcampaignId Write SetcampaignId;
    Property campaignIdDimensionValue : TDimensionValue Index 40 Read FcampaignIdDimensionValue Write SetcampaignIdDimensionValue;
    Property comment : String Index 48 Read Fcomment Write Setcomment;
    Property compatibility : String Index 56 Read Fcompatibility Write Setcompatibility;
    Property contentCategoryId : String Index 64 Read FcontentCategoryId Write SetcontentCategoryId;
    Property createInfo : TLastModifiedInfo Index 72 Read FcreateInfo Write SetcreateInfo;
    Property directorySiteId : String Index 80 Read FdirectorySiteId Write SetdirectorySiteId;
    Property directorySiteIdDimensionValue : TDimensionValue Index 88 Read FdirectorySiteIdDimensionValue Write SetdirectorySiteIdDimensionValue;
    Property externalId : String Index 96 Read FexternalId Write SetexternalId;
    Property id : String Index 104 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 112 Read FidDimensionValue Write SetidDimensionValue;
    Property keyName : String Index 120 Read FkeyName Write SetkeyName;
    Property kind : String Index 128 Read Fkind Write Setkind;
    Property lastModifiedInfo : TLastModifiedInfo Index 136 Read FlastModifiedInfo Write SetlastModifiedInfo;
    Property lookbackConfiguration : TLookbackConfiguration Index 144 Read FlookbackConfiguration Write SetlookbackConfiguration;
    Property name : String Index 152 Read Fname Write Setname;
    Property paymentApproved : boolean Index 160 Read FpaymentApproved Write SetpaymentApproved;
    Property paymentSource : String Index 168 Read FpaymentSource Write SetpaymentSource;
    Property placementGroupId : String Index 176 Read FplacementGroupId Write SetplacementGroupId;
    Property placementGroupIdDimensionValue : TDimensionValue Index 184 Read FplacementGroupIdDimensionValue Write SetplacementGroupIdDimensionValue;
    Property placementStrategyId : String Index 192 Read FplacementStrategyId Write SetplacementStrategyId;
    Property pricingSchedule : TPricingSchedule Index 200 Read FpricingSchedule Write SetpricingSchedule;
    Property primary : boolean Index 208 Read Fprimary Write Setprimary;
    Property publisherUpdateInfo : TLastModifiedInfo Index 216 Read FpublisherUpdateInfo Write SetpublisherUpdateInfo;
    Property siteId : String Index 224 Read FsiteId Write SetsiteId;
    Property siteIdDimensionValue : TDimensionValue Index 232 Read FsiteIdDimensionValue Write SetsiteIdDimensionValue;
    Property size : TSize Index 240 Read Fsize Write Setsize;
    Property sslRequired : boolean Index 248 Read FsslRequired Write SetsslRequired;
    Property status : String Index 256 Read Fstatus Write Setstatus;
    Property subaccountId : String Index 264 Read FsubaccountId Write SetsubaccountId;
    Property tagFormats : TStringArray Index 272 Read FtagFormats Write SettagFormats;
    Property tagSetting : TTagSetting Index 280 Read FtagSetting Write SettagSetting;
  end;
  TPlacementClass = Class of TPlacement;
  
  { --------------------------------------------------------------------
    TPlacementAssignment
    --------------------------------------------------------------------}
  
  TPlacementAssignment = Class(TGoogleBaseObject)
  Private
    Factive : boolean;
    FplacementId : String;
    FplacementIdDimensionValue : TDimensionValue;
    FsslRequired : boolean;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetplacementId(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetsslRequired(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property placementId : String Index 8 Read FplacementId Write SetplacementId;
    Property placementIdDimensionValue : TDimensionValue Index 16 Read FplacementIdDimensionValue Write SetplacementIdDimensionValue;
    Property sslRequired : boolean Index 24 Read FsslRequired Write SetsslRequired;
  end;
  TPlacementAssignmentClass = Class of TPlacementAssignment;
  
  { --------------------------------------------------------------------
    TPlacementGroup
    --------------------------------------------------------------------}
  
  TPlacementGroup = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    Farchived : boolean;
    FcampaignId : String;
    FcampaignIdDimensionValue : TDimensionValue;
    FchildPlacementIds : TStringArray;
    Fcomment : String;
    FcontentCategoryId : String;
    FcreateInfo : TLastModifiedInfo;
    FdirectorySiteId : String;
    FdirectorySiteIdDimensionValue : TDimensionValue;
    FexternalId : String;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    Fkind : String;
    FlastModifiedInfo : TLastModifiedInfo;
    Fname : String;
    FplacementGroupType : String;
    FplacementStrategyId : String;
    FpricingSchedule : TPricingSchedule;
    FprimaryPlacementId : String;
    FprimaryPlacementIdDimensionValue : TDimensionValue;
    FprogrammaticSetting : TProgrammaticSetting;
    FsiteId : String;
    FsiteIdDimensionValue : TDimensionValue;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setarchived(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcampaignId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcampaignIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetchildPlacementIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setcomment(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentCategoryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreateInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure SetdirectorySiteId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdirectorySiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetexternalId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementGroupType(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementStrategyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpricingSchedule(AIndex : Integer; AValue : TPricingSchedule); virtual;
    Procedure SetprimaryPlacementId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprimaryPlacementIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetprogrammaticSetting(AIndex : Integer; AValue : TProgrammaticSetting); virtual;
    Procedure SetsiteId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 16 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property archived : boolean Index 24 Read Farchived Write Setarchived;
    Property campaignId : String Index 32 Read FcampaignId Write SetcampaignId;
    Property campaignIdDimensionValue : TDimensionValue Index 40 Read FcampaignIdDimensionValue Write SetcampaignIdDimensionValue;
    Property childPlacementIds : TStringArray Index 48 Read FchildPlacementIds Write SetchildPlacementIds;
    Property comment : String Index 56 Read Fcomment Write Setcomment;
    Property contentCategoryId : String Index 64 Read FcontentCategoryId Write SetcontentCategoryId;
    Property createInfo : TLastModifiedInfo Index 72 Read FcreateInfo Write SetcreateInfo;
    Property directorySiteId : String Index 80 Read FdirectorySiteId Write SetdirectorySiteId;
    Property directorySiteIdDimensionValue : TDimensionValue Index 88 Read FdirectorySiteIdDimensionValue Write SetdirectorySiteIdDimensionValue;
    Property externalId : String Index 96 Read FexternalId Write SetexternalId;
    Property id : String Index 104 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 112 Read FidDimensionValue Write SetidDimensionValue;
    Property kind : String Index 120 Read Fkind Write Setkind;
    Property lastModifiedInfo : TLastModifiedInfo Index 128 Read FlastModifiedInfo Write SetlastModifiedInfo;
    Property name : String Index 136 Read Fname Write Setname;
    Property placementGroupType : String Index 144 Read FplacementGroupType Write SetplacementGroupType;
    Property placementStrategyId : String Index 152 Read FplacementStrategyId Write SetplacementStrategyId;
    Property pricingSchedule : TPricingSchedule Index 160 Read FpricingSchedule Write SetpricingSchedule;
    Property primaryPlacementId : String Index 168 Read FprimaryPlacementId Write SetprimaryPlacementId;
    Property primaryPlacementIdDimensionValue : TDimensionValue Index 176 Read FprimaryPlacementIdDimensionValue Write SetprimaryPlacementIdDimensionValue;
    Property programmaticSetting : TProgrammaticSetting Index 184 Read FprogrammaticSetting Write SetprogrammaticSetting;
    Property siteId : String Index 192 Read FsiteId Write SetsiteId;
    Property siteIdDimensionValue : TDimensionValue Index 200 Read FsiteIdDimensionValue Write SetsiteIdDimensionValue;
    Property subaccountId : String Index 208 Read FsubaccountId Write SetsubaccountId;
  end;
  TPlacementGroupClass = Class of TPlacementGroup;
  
  { --------------------------------------------------------------------
    TPlacementGroupsListResponse
    --------------------------------------------------------------------}
  
  TPlacementGroupsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    FplacementGroups : TPlacementGroupsListResponseTypeplacementGroupsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementGroups(AIndex : Integer; AValue : TPlacementGroupsListResponseTypeplacementGroupsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property placementGroups : TPlacementGroupsListResponseTypeplacementGroupsArray Index 16 Read FplacementGroups Write SetplacementGroups;
  end;
  TPlacementGroupsListResponseClass = Class of TPlacementGroupsListResponse;
  
  { --------------------------------------------------------------------
    TPlacementStrategiesListResponse
    --------------------------------------------------------------------}
  
  TPlacementStrategiesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    FplacementStrategies : TPlacementStrategiesListResponseTypeplacementStrategiesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementStrategies(AIndex : Integer; AValue : TPlacementStrategiesListResponseTypeplacementStrategiesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property placementStrategies : TPlacementStrategiesListResponseTypeplacementStrategiesArray Index 16 Read FplacementStrategies Write SetplacementStrategies;
  end;
  TPlacementStrategiesListResponseClass = Class of TPlacementStrategiesListResponse;
  
  { --------------------------------------------------------------------
    TPlacementStrategy
    --------------------------------------------------------------------}
  
  TPlacementStrategy = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TPlacementStrategyClass = Class of TPlacementStrategy;
  
  { --------------------------------------------------------------------
    TPlacementTag
    --------------------------------------------------------------------}
  
  TPlacementTag = Class(TGoogleBaseObject)
  Private
    FplacementId : String;
    FtagDatas : TPlacementTagTypetagDatasArray;
  Protected
    //Property setters
    Procedure SetplacementId(AIndex : Integer; AValue : String); virtual;
    Procedure SettagDatas(AIndex : Integer; AValue : TPlacementTagTypetagDatasArray); virtual;
  Public
  Published
    Property placementId : String Index 0 Read FplacementId Write SetplacementId;
    Property tagDatas : TPlacementTagTypetagDatasArray Index 8 Read FtagDatas Write SettagDatas;
  end;
  TPlacementTagClass = Class of TPlacementTag;
  
  { --------------------------------------------------------------------
    TPlacementsGenerateTagsResponse
    --------------------------------------------------------------------}
  
  TPlacementsGenerateTagsResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FplacementTags : TPlacementsGenerateTagsResponseTypeplacementTagsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetplacementTags(AIndex : Integer; AValue : TPlacementsGenerateTagsResponseTypeplacementTagsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property placementTags : TPlacementsGenerateTagsResponseTypeplacementTagsArray Index 8 Read FplacementTags Write SetplacementTags;
  end;
  TPlacementsGenerateTagsResponseClass = Class of TPlacementsGenerateTagsResponse;
  
  { --------------------------------------------------------------------
    TPlacementsListResponse
    --------------------------------------------------------------------}
  
  TPlacementsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fplacements : TPlacementsListResponseTypeplacementsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setplacements(AIndex : Integer; AValue : TPlacementsListResponseTypeplacementsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property placements : TPlacementsListResponseTypeplacementsArray Index 16 Read Fplacements Write Setplacements;
  end;
  TPlacementsListResponseClass = Class of TPlacementsListResponse;
  
  { --------------------------------------------------------------------
    TPlatformType
    --------------------------------------------------------------------}
  
  TPlatformType = Class(TGoogleBaseObject)
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
  TPlatformTypeClass = Class of TPlatformType;
  
  { --------------------------------------------------------------------
    TPlatformTypesListResponse
    --------------------------------------------------------------------}
  
  TPlatformTypesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FplatformTypes : TPlatformTypesListResponseTypeplatformTypesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetplatformTypes(AIndex : Integer; AValue : TPlatformTypesListResponseTypeplatformTypesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property platformTypes : TPlatformTypesListResponseTypeplatformTypesArray Index 8 Read FplatformTypes Write SetplatformTypes;
  end;
  TPlatformTypesListResponseClass = Class of TPlatformTypesListResponse;
  
  { --------------------------------------------------------------------
    TPopupWindowProperties
    --------------------------------------------------------------------}
  
  TPopupWindowProperties = Class(TGoogleBaseObject)
  Private
    Fdimension : TSize;
    Foffset : TOffsetPosition;
    FpositionType : String;
    FshowAddressBar : boolean;
    FshowMenuBar : boolean;
    FshowScrollBar : boolean;
    FshowStatusBar : boolean;
    FshowToolBar : boolean;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setdimension(AIndex : Integer; AValue : TSize); virtual;
    Procedure Setoffset(AIndex : Integer; AValue : TOffsetPosition); virtual;
    Procedure SetpositionType(AIndex : Integer; AValue : String); virtual;
    Procedure SetshowAddressBar(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetshowMenuBar(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetshowScrollBar(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetshowStatusBar(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetshowToolBar(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dimension : TSize Index 0 Read Fdimension Write Setdimension;
    Property offset : TOffsetPosition Index 8 Read Foffset Write Setoffset;
    Property positionType : String Index 16 Read FpositionType Write SetpositionType;
    Property showAddressBar : boolean Index 24 Read FshowAddressBar Write SetshowAddressBar;
    Property showMenuBar : boolean Index 32 Read FshowMenuBar Write SetshowMenuBar;
    Property showScrollBar : boolean Index 40 Read FshowScrollBar Write SetshowScrollBar;
    Property showStatusBar : boolean Index 48 Read FshowStatusBar Write SetshowStatusBar;
    Property showToolBar : boolean Index 56 Read FshowToolBar Write SetshowToolBar;
    Property title : String Index 64 Read Ftitle Write Settitle;
  end;
  TPopupWindowPropertiesClass = Class of TPopupWindowProperties;
  
  { --------------------------------------------------------------------
    TPostalCode
    --------------------------------------------------------------------}
  
  TPostalCode = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    FcountryCode : String;
    FcountryDartId : String;
    Fid : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountryCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountryDartId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property countryCode : String Index 8 Read FcountryCode Write SetcountryCode;
    Property countryDartId : String Index 16 Read FcountryDartId Write SetcountryDartId;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
  end;
  TPostalCodeClass = Class of TPostalCode;
  
  { --------------------------------------------------------------------
    TPostalCodesListResponse
    --------------------------------------------------------------------}
  
  TPostalCodesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FpostalCodes : TPostalCodesListResponseTypepostalCodesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostalCodes(AIndex : Integer; AValue : TPostalCodesListResponseTypepostalCodesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property postalCodes : TPostalCodesListResponseTypepostalCodesArray Index 8 Read FpostalCodes Write SetpostalCodes;
  end;
  TPostalCodesListResponseClass = Class of TPostalCodesListResponse;
  
  { --------------------------------------------------------------------
    TPricing
    --------------------------------------------------------------------}
  
  TPricing = Class(TGoogleBaseObject)
  Private
    FcapCostType : String;
    FendDate : TDate;
    Fflights : TPricingTypeflightsArray;
    FgroupType : String;
    FpricingType : String;
    FstartDate : TDate;
  Protected
    //Property setters
    Procedure SetcapCostType(AIndex : Integer; AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setflights(AIndex : Integer; AValue : TPricingTypeflightsArray); virtual;
    Procedure SetgroupType(AIndex : Integer; AValue : String); virtual;
    Procedure SetpricingType(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
  Public
  Published
    Property capCostType : String Index 0 Read FcapCostType Write SetcapCostType;
    Property endDate : TDate Index 8 Read FendDate Write SetendDate;
    Property flights : TPricingTypeflightsArray Index 16 Read Fflights Write Setflights;
    Property groupType : String Index 24 Read FgroupType Write SetgroupType;
    Property pricingType : String Index 32 Read FpricingType Write SetpricingType;
    Property startDate : TDate Index 40 Read FstartDate Write SetstartDate;
  end;
  TPricingClass = Class of TPricing;
  
  { --------------------------------------------------------------------
    TPricingSchedule
    --------------------------------------------------------------------}
  
  TPricingSchedule = Class(TGoogleBaseObject)
  Private
    FcapCostOption : String;
    FdisregardOverdelivery : boolean;
    FendDate : TDate;
    Fflighted : boolean;
    FfloodlightActivityId : String;
    FpricingPeriods : TPricingScheduleTypepricingPeriodsArray;
    FpricingType : String;
    FstartDate : TDate;
    FtestingStartDate : TDate;
  Protected
    //Property setters
    Procedure SetcapCostOption(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisregardOverdelivery(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setflighted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfloodlightActivityId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpricingPeriods(AIndex : Integer; AValue : TPricingScheduleTypepricingPeriodsArray); virtual;
    Procedure SetpricingType(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure SettestingStartDate(AIndex : Integer; AValue : TDate); virtual;
  Public
  Published
    Property capCostOption : String Index 0 Read FcapCostOption Write SetcapCostOption;
    Property disregardOverdelivery : boolean Index 8 Read FdisregardOverdelivery Write SetdisregardOverdelivery;
    Property endDate : TDate Index 16 Read FendDate Write SetendDate;
    Property flighted : boolean Index 24 Read Fflighted Write Setflighted;
    Property floodlightActivityId : String Index 32 Read FfloodlightActivityId Write SetfloodlightActivityId;
    Property pricingPeriods : TPricingScheduleTypepricingPeriodsArray Index 40 Read FpricingPeriods Write SetpricingPeriods;
    Property pricingType : String Index 48 Read FpricingType Write SetpricingType;
    Property startDate : TDate Index 56 Read FstartDate Write SetstartDate;
    Property testingStartDate : TDate Index 64 Read FtestingStartDate Write SettestingStartDate;
  end;
  TPricingScheduleClass = Class of TPricingSchedule;
  
  { --------------------------------------------------------------------
    TPricingSchedulePricingPeriod
    --------------------------------------------------------------------}
  
  TPricingSchedulePricingPeriod = Class(TGoogleBaseObject)
  Private
    FendDate : TDate;
    FpricingComment : String;
    FrateOrCostNanos : String;
    FstartDate : TDate;
    Funits : String;
  Protected
    //Property setters
    Procedure SetendDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure SetpricingComment(AIndex : Integer; AValue : String); virtual;
    Procedure SetrateOrCostNanos(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setunits(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property endDate : TDate Index 0 Read FendDate Write SetendDate;
    Property pricingComment : String Index 8 Read FpricingComment Write SetpricingComment;
    Property rateOrCostNanos : String Index 16 Read FrateOrCostNanos Write SetrateOrCostNanos;
    Property startDate : TDate Index 24 Read FstartDate Write SetstartDate;
    Property units : String Index 32 Read Funits Write Setunits;
  end;
  TPricingSchedulePricingPeriodClass = Class of TPricingSchedulePricingPeriod;
  
  { --------------------------------------------------------------------
    TProgrammaticSetting
    --------------------------------------------------------------------}
  
  TProgrammaticSetting = Class(TGoogleBaseObject)
  Private
    FadxDealIds : TStringArray;
    FinsertionOrderId : String;
    FinsertionOrderIdStatus : boolean;
    FmediaCostNanos : String;
    Fprogrammatic : boolean;
    FtraffickerEmails : TStringArray;
  Protected
    //Property setters
    Procedure SetadxDealIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetinsertionOrderId(AIndex : Integer; AValue : String); virtual;
    Procedure SetinsertionOrderIdStatus(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetmediaCostNanos(AIndex : Integer; AValue : String); virtual;
    Procedure Setprogrammatic(AIndex : Integer; AValue : boolean); virtual;
    Procedure SettraffickerEmails(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property adxDealIds : TStringArray Index 0 Read FadxDealIds Write SetadxDealIds;
    Property insertionOrderId : String Index 8 Read FinsertionOrderId Write SetinsertionOrderId;
    Property insertionOrderIdStatus : boolean Index 16 Read FinsertionOrderIdStatus Write SetinsertionOrderIdStatus;
    Property mediaCostNanos : String Index 24 Read FmediaCostNanos Write SetmediaCostNanos;
    Property programmatic : boolean Index 32 Read Fprogrammatic Write Setprogrammatic;
    Property traffickerEmails : TStringArray Index 40 Read FtraffickerEmails Write SettraffickerEmails;
  end;
  TProgrammaticSettingClass = Class of TProgrammaticSetting;
  
  { --------------------------------------------------------------------
    TProject
    --------------------------------------------------------------------}
  
  TProject = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvertiserId : String;
    FaudienceAgeGroup : String;
    FaudienceGender : String;
    Fbudget : String;
    FclientBillingCode : String;
    FclientName : String;
    FendDate : TDate;
    Fid : String;
    Fkind : String;
    FlastModifiedInfo : TLastModifiedInfo;
    Fname : String;
    Foverview : String;
    FstartDate : TDate;
    FsubaccountId : String;
    FtargetClicks : String;
    FtargetConversions : String;
    FtargetCpaNanos : String;
    FtargetCpcNanos : String;
    FtargetCpmNanos : String;
    FtargetImpressions : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetaudienceAgeGroup(AIndex : Integer; AValue : String); virtual;
    Procedure SetaudienceGender(AIndex : Integer; AValue : String); virtual;
    Procedure Setbudget(AIndex : Integer; AValue : String); virtual;
    Procedure SetclientBillingCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetclientName(AIndex : Integer; AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setoverview(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetClicks(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetConversions(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetCpaNanos(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetCpcNanos(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetCpmNanos(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetImpressions(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advertiserId : String Index 8 Read FadvertiserId Write SetadvertiserId;
    Property audienceAgeGroup : String Index 16 Read FaudienceAgeGroup Write SetaudienceAgeGroup;
    Property audienceGender : String Index 24 Read FaudienceGender Write SetaudienceGender;
    Property budget : String Index 32 Read Fbudget Write Setbudget;
    Property clientBillingCode : String Index 40 Read FclientBillingCode Write SetclientBillingCode;
    Property clientName : String Index 48 Read FclientName Write SetclientName;
    Property endDate : TDate Index 56 Read FendDate Write SetendDate;
    Property id : String Index 64 Read Fid Write Setid;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property lastModifiedInfo : TLastModifiedInfo Index 80 Read FlastModifiedInfo Write SetlastModifiedInfo;
    Property name : String Index 88 Read Fname Write Setname;
    Property overview : String Index 96 Read Foverview Write Setoverview;
    Property startDate : TDate Index 104 Read FstartDate Write SetstartDate;
    Property subaccountId : String Index 112 Read FsubaccountId Write SetsubaccountId;
    Property targetClicks : String Index 120 Read FtargetClicks Write SettargetClicks;
    Property targetConversions : String Index 128 Read FtargetConversions Write SettargetConversions;
    Property targetCpaNanos : String Index 136 Read FtargetCpaNanos Write SettargetCpaNanos;
    Property targetCpcNanos : String Index 144 Read FtargetCpcNanos Write SettargetCpcNanos;
    Property targetCpmNanos : String Index 152 Read FtargetCpmNanos Write SettargetCpmNanos;
    Property targetImpressions : String Index 160 Read FtargetImpressions Write SettargetImpressions;
  end;
  TProjectClass = Class of TProject;
  
  { --------------------------------------------------------------------
    TProjectsListResponse
    --------------------------------------------------------------------}
  
  TProjectsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fprojects : TProjectsListResponseTypeprojectsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setprojects(AIndex : Integer; AValue : TProjectsListResponseTypeprojectsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property projects : TProjectsListResponseTypeprojectsArray Index 16 Read Fprojects Write Setprojects;
  end;
  TProjectsListResponseClass = Class of TProjectsListResponse;
  
  { --------------------------------------------------------------------
    TReachReportCompatibleFields
    --------------------------------------------------------------------}
  
  TReachReportCompatibleFields = Class(TGoogleBaseObject)
  Private
    FdimensionFilters : TReachReportCompatibleFieldsTypedimensionFiltersArray;
    Fdimensions : TReachReportCompatibleFieldsTypedimensionsArray;
    Fkind : String;
    Fmetrics : TReachReportCompatibleFieldsTypemetricsArray;
    FpivotedActivityMetrics : TReachReportCompatibleFieldsTypepivotedActivityMetricsArray;
    FreachByFrequencyMetrics : TReachReportCompatibleFieldsTypereachByFrequencyMetricsArray;
  Protected
    //Property setters
    Procedure SetdimensionFilters(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypedimensionFiltersArray); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypedimensionsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypemetricsArray); virtual;
    Procedure SetpivotedActivityMetrics(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypepivotedActivityMetricsArray); virtual;
    Procedure SetreachByFrequencyMetrics(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypereachByFrequencyMetricsArray); virtual;
  Public
  Published
    Property dimensionFilters : TReachReportCompatibleFieldsTypedimensionFiltersArray Index 0 Read FdimensionFilters Write SetdimensionFilters;
    Property dimensions : TReachReportCompatibleFieldsTypedimensionsArray Index 8 Read Fdimensions Write Setdimensions;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property metrics : TReachReportCompatibleFieldsTypemetricsArray Index 24 Read Fmetrics Write Setmetrics;
    Property pivotedActivityMetrics : TReachReportCompatibleFieldsTypepivotedActivityMetricsArray Index 32 Read FpivotedActivityMetrics Write SetpivotedActivityMetrics;
    Property reachByFrequencyMetrics : TReachReportCompatibleFieldsTypereachByFrequencyMetricsArray Index 40 Read FreachByFrequencyMetrics Write SetreachByFrequencyMetrics;
  end;
  TReachReportCompatibleFieldsClass = Class of TReachReportCompatibleFields;
  
  { --------------------------------------------------------------------
    TRecipient
    --------------------------------------------------------------------}
  
  TRecipient = Class(TGoogleBaseObject)
  Private
    FdeliveryType : String;
    Femail : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetdeliveryType(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property deliveryType : String Index 0 Read FdeliveryType Write SetdeliveryType;
    Property email : String Index 8 Read Femail Write Setemail;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TRecipientClass = Class of TRecipient;
  
  { --------------------------------------------------------------------
    TRegion
    --------------------------------------------------------------------}
  
  TRegion = Class(TGoogleBaseObject)
  Private
    FcountryCode : String;
    FcountryDartId : String;
    FdartId : String;
    Fkind : String;
    Fname : String;
    FregionCode : String;
  Protected
    //Property setters
    Procedure SetcountryCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountryDartId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdartId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetregionCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property countryCode : String Index 0 Read FcountryCode Write SetcountryCode;
    Property countryDartId : String Index 8 Read FcountryDartId Write SetcountryDartId;
    Property dartId : String Index 16 Read FdartId Write SetdartId;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property regionCode : String Index 40 Read FregionCode Write SetregionCode;
  end;
  TRegionClass = Class of TRegion;
  
  { --------------------------------------------------------------------
    TRegionsListResponse
    --------------------------------------------------------------------}
  
  TRegionsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fregions : TRegionsListResponseTyperegionsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setregions(AIndex : Integer; AValue : TRegionsListResponseTyperegionsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property regions : TRegionsListResponseTyperegionsArray Index 8 Read Fregions Write Setregions;
  end;
  TRegionsListResponseClass = Class of TRegionsListResponse;
  
  { --------------------------------------------------------------------
    TRemarketingList
    --------------------------------------------------------------------}
  
  TRemarketingList = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Factive : boolean;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    Fdescription : String;
    Fid : String;
    Fkind : String;
    FlifeSpan : String;
    FlistPopulationRule : TListPopulationRule;
    FlistSize : String;
    FlistSource : String;
    Fname : String;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlifeSpan(AIndex : Integer; AValue : String); virtual;
    Procedure SetlistPopulationRule(AIndex : Integer; AValue : TListPopulationRule); virtual;
    Procedure SetlistSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetlistSource(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property advertiserId : String Index 16 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 24 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property id : String Index 40 Read Fid Write Setid;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property lifeSpan : String Index 56 Read FlifeSpan Write SetlifeSpan;
    Property listPopulationRule : TListPopulationRule Index 64 Read FlistPopulationRule Write SetlistPopulationRule;
    Property listSize : String Index 72 Read FlistSize Write SetlistSize;
    Property listSource : String Index 80 Read FlistSource Write SetlistSource;
    Property name : String Index 88 Read Fname Write Setname;
    Property subaccountId : String Index 96 Read FsubaccountId Write SetsubaccountId;
  end;
  TRemarketingListClass = Class of TRemarketingList;
  
  { --------------------------------------------------------------------
    TRemarketingListShare
    --------------------------------------------------------------------}
  
  TRemarketingListShare = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FremarketingListId : String;
    FsharedAccountIds : TStringArray;
    FsharedAdvertiserIds : TStringArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetremarketingListId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsharedAccountIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsharedAdvertiserIds(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property remarketingListId : String Index 8 Read FremarketingListId Write SetremarketingListId;
    Property sharedAccountIds : TStringArray Index 16 Read FsharedAccountIds Write SetsharedAccountIds;
    Property sharedAdvertiserIds : TStringArray Index 24 Read FsharedAdvertiserIds Write SetsharedAdvertiserIds;
  end;
  TRemarketingListShareClass = Class of TRemarketingListShare;
  
  { --------------------------------------------------------------------
    TRemarketingListsListResponse
    --------------------------------------------------------------------}
  
  TRemarketingListsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    FremarketingLists : TRemarketingListsListResponseTyperemarketingListsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetremarketingLists(AIndex : Integer; AValue : TRemarketingListsListResponseTyperemarketingListsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property remarketingLists : TRemarketingListsListResponseTyperemarketingListsArray Index 16 Read FremarketingLists Write SetremarketingLists;
  end;
  TRemarketingListsListResponseClass = Class of TRemarketingListsListResponse;
  
  { --------------------------------------------------------------------
    TReportTypecriteria
    --------------------------------------------------------------------}
  
  TReportTypecriteria = Class(TGoogleBaseObject)
  Private
    Factivities : TActivities;
    FcustomRichMediaEvents : TCustomRichMediaEvents;
    FdateRange : TDateRange;
    FdimensionFilters : TReportTypecriteriaTypedimensionFiltersArray;
    Fdimensions : TReportTypecriteriaTypedimensionsArray;
    FmetricNames : TStringArray;
  Protected
    //Property setters
    Procedure Setactivities(AIndex : Integer; AValue : TActivities); virtual;
    Procedure SetcustomRichMediaEvents(AIndex : Integer; AValue : TCustomRichMediaEvents); virtual;
    Procedure SetdateRange(AIndex : Integer; AValue : TDateRange); virtual;
    Procedure SetdimensionFilters(AIndex : Integer; AValue : TReportTypecriteriaTypedimensionFiltersArray); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TReportTypecriteriaTypedimensionsArray); virtual;
    Procedure SetmetricNames(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property activities : TActivities Index 0 Read Factivities Write Setactivities;
    Property customRichMediaEvents : TCustomRichMediaEvents Index 8 Read FcustomRichMediaEvents Write SetcustomRichMediaEvents;
    Property dateRange : TDateRange Index 16 Read FdateRange Write SetdateRange;
    Property dimensionFilters : TReportTypecriteriaTypedimensionFiltersArray Index 24 Read FdimensionFilters Write SetdimensionFilters;
    Property dimensions : TReportTypecriteriaTypedimensionsArray Index 32 Read Fdimensions Write Setdimensions;
    Property metricNames : TStringArray Index 40 Read FmetricNames Write SetmetricNames;
  end;
  TReportTypecriteriaClass = Class of TReportTypecriteria;
  
  { --------------------------------------------------------------------
    TReportTypecrossDimensionReachCriteria
    --------------------------------------------------------------------}
  
  TReportTypecrossDimensionReachCriteria = Class(TGoogleBaseObject)
  Private
    Fbreakdown : TReportTypecrossDimensionReachCriteriaTypebreakdownArray;
    FdateRange : TDateRange;
    Fdimension : String;
    FdimensionFilters : TReportTypecrossDimensionReachCriteriaTypedimensionFiltersArray;
    FmetricNames : TStringArray;
    FoverlapMetricNames : TStringArray;
    Fpivoted : boolean;
  Protected
    //Property setters
    Procedure Setbreakdown(AIndex : Integer; AValue : TReportTypecrossDimensionReachCriteriaTypebreakdownArray); virtual;
    Procedure SetdateRange(AIndex : Integer; AValue : TDateRange); virtual;
    Procedure Setdimension(AIndex : Integer; AValue : String); virtual;
    Procedure SetdimensionFilters(AIndex : Integer; AValue : TReportTypecrossDimensionReachCriteriaTypedimensionFiltersArray); virtual;
    Procedure SetmetricNames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetoverlapMetricNames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setpivoted(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property breakdown : TReportTypecrossDimensionReachCriteriaTypebreakdownArray Index 0 Read Fbreakdown Write Setbreakdown;
    Property dateRange : TDateRange Index 8 Read FdateRange Write SetdateRange;
    Property dimension : String Index 16 Read Fdimension Write Setdimension;
    Property dimensionFilters : TReportTypecrossDimensionReachCriteriaTypedimensionFiltersArray Index 24 Read FdimensionFilters Write SetdimensionFilters;
    Property metricNames : TStringArray Index 32 Read FmetricNames Write SetmetricNames;
    Property overlapMetricNames : TStringArray Index 40 Read FoverlapMetricNames Write SetoverlapMetricNames;
    Property pivoted : boolean Index 48 Read Fpivoted Write Setpivoted;
  end;
  TReportTypecrossDimensionReachCriteriaClass = Class of TReportTypecrossDimensionReachCriteria;
  
  { --------------------------------------------------------------------
    TReportTypedelivery
    --------------------------------------------------------------------}
  
  TReportTypedelivery = Class(TGoogleBaseObject)
  Private
    FemailOwner : boolean;
    FemailOwnerDeliveryType : String;
    Fmessage : String;
    Frecipients : TReportTypedeliveryTyperecipientsArray;
  Protected
    //Property setters
    Procedure SetemailOwner(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetemailOwnerDeliveryType(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setrecipients(AIndex : Integer; AValue : TReportTypedeliveryTyperecipientsArray); virtual;
  Public
  Published
    Property emailOwner : boolean Index 0 Read FemailOwner Write SetemailOwner;
    Property emailOwnerDeliveryType : String Index 8 Read FemailOwnerDeliveryType Write SetemailOwnerDeliveryType;
    Property message : String Index 16 Read Fmessage Write Setmessage;
    Property recipients : TReportTypedeliveryTyperecipientsArray Index 24 Read Frecipients Write Setrecipients;
  end;
  TReportTypedeliveryClass = Class of TReportTypedelivery;
  
  { --------------------------------------------------------------------
    TReportTypefloodlightCriteriaTypereportProperties
    --------------------------------------------------------------------}
  
  TReportTypefloodlightCriteriaTypereportProperties = Class(TGoogleBaseObject)
  Private
    FincludeAttributedIPConversions : boolean;
    FincludeUnattributedCookieConversions : boolean;
    FincludeUnattributedIPConversions : boolean;
  Protected
    //Property setters
    Procedure SetincludeAttributedIPConversions(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetincludeUnattributedCookieConversions(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetincludeUnattributedIPConversions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property includeAttributedIPConversions : boolean Index 0 Read FincludeAttributedIPConversions Write SetincludeAttributedIPConversions;
    Property includeUnattributedCookieConversions : boolean Index 8 Read FincludeUnattributedCookieConversions Write SetincludeUnattributedCookieConversions;
    Property includeUnattributedIPConversions : boolean Index 16 Read FincludeUnattributedIPConversions Write SetincludeUnattributedIPConversions;
  end;
  TReportTypefloodlightCriteriaTypereportPropertiesClass = Class of TReportTypefloodlightCriteriaTypereportProperties;
  
  { --------------------------------------------------------------------
    TReportTypefloodlightCriteria
    --------------------------------------------------------------------}
  
  TReportTypefloodlightCriteria = Class(TGoogleBaseObject)
  Private
    FcustomRichMediaEvents : TReportTypefloodlightCriteriaTypecustomRichMediaEventsArray;
    FdateRange : TDateRange;
    FdimensionFilters : TReportTypefloodlightCriteriaTypedimensionFiltersArray;
    Fdimensions : TReportTypefloodlightCriteriaTypedimensionsArray;
    FfloodlightConfigId : TDimensionValue;
    FmetricNames : TStringArray;
    FreportProperties : TReportTypefloodlightCriteriaTypereportProperties;
  Protected
    //Property setters
    Procedure SetcustomRichMediaEvents(AIndex : Integer; AValue : TReportTypefloodlightCriteriaTypecustomRichMediaEventsArray); virtual;
    Procedure SetdateRange(AIndex : Integer; AValue : TDateRange); virtual;
    Procedure SetdimensionFilters(AIndex : Integer; AValue : TReportTypefloodlightCriteriaTypedimensionFiltersArray); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TReportTypefloodlightCriteriaTypedimensionsArray); virtual;
    Procedure SetfloodlightConfigId(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetmetricNames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetreportProperties(AIndex : Integer; AValue : TReportTypefloodlightCriteriaTypereportProperties); virtual;
  Public
  Published
    Property customRichMediaEvents : TReportTypefloodlightCriteriaTypecustomRichMediaEventsArray Index 0 Read FcustomRichMediaEvents Write SetcustomRichMediaEvents;
    Property dateRange : TDateRange Index 8 Read FdateRange Write SetdateRange;
    Property dimensionFilters : TReportTypefloodlightCriteriaTypedimensionFiltersArray Index 16 Read FdimensionFilters Write SetdimensionFilters;
    Property dimensions : TReportTypefloodlightCriteriaTypedimensionsArray Index 24 Read Fdimensions Write Setdimensions;
    Property floodlightConfigId : TDimensionValue Index 32 Read FfloodlightConfigId Write SetfloodlightConfigId;
    Property metricNames : TStringArray Index 40 Read FmetricNames Write SetmetricNames;
    Property reportProperties : TReportTypefloodlightCriteriaTypereportProperties Index 48 Read FreportProperties Write SetreportProperties;
  end;
  TReportTypefloodlightCriteriaClass = Class of TReportTypefloodlightCriteria;
  
  { --------------------------------------------------------------------
    TReportTypepathToConversionCriteriaTypereportProperties
    --------------------------------------------------------------------}
  
  TReportTypepathToConversionCriteriaTypereportProperties = Class(TGoogleBaseObject)
  Private
    FclicksLookbackWindow : integer;
    FimpressionsLookbackWindow : integer;
    FincludeAttributedIPConversions : boolean;
    FincludeUnattributedCookieConversions : boolean;
    FincludeUnattributedIPConversions : boolean;
    FmaximumClickInteractions : integer;
    FmaximumImpressionInteractions : integer;
    FmaximumInteractionGap : integer;
    FpivotOnInteractionPath : boolean;
  Protected
    //Property setters
    Procedure SetclicksLookbackWindow(AIndex : Integer; AValue : integer); virtual;
    Procedure SetimpressionsLookbackWindow(AIndex : Integer; AValue : integer); virtual;
    Procedure SetincludeAttributedIPConversions(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetincludeUnattributedCookieConversions(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetincludeUnattributedIPConversions(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetmaximumClickInteractions(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaximumImpressionInteractions(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaximumInteractionGap(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpivotOnInteractionPath(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property clicksLookbackWindow : integer Index 0 Read FclicksLookbackWindow Write SetclicksLookbackWindow;
    Property impressionsLookbackWindow : integer Index 8 Read FimpressionsLookbackWindow Write SetimpressionsLookbackWindow;
    Property includeAttributedIPConversions : boolean Index 16 Read FincludeAttributedIPConversions Write SetincludeAttributedIPConversions;
    Property includeUnattributedCookieConversions : boolean Index 24 Read FincludeUnattributedCookieConversions Write SetincludeUnattributedCookieConversions;
    Property includeUnattributedIPConversions : boolean Index 32 Read FincludeUnattributedIPConversions Write SetincludeUnattributedIPConversions;
    Property maximumClickInteractions : integer Index 40 Read FmaximumClickInteractions Write SetmaximumClickInteractions;
    Property maximumImpressionInteractions : integer Index 48 Read FmaximumImpressionInteractions Write SetmaximumImpressionInteractions;
    Property maximumInteractionGap : integer Index 56 Read FmaximumInteractionGap Write SetmaximumInteractionGap;
    Property pivotOnInteractionPath : boolean Index 64 Read FpivotOnInteractionPath Write SetpivotOnInteractionPath;
  end;
  TReportTypepathToConversionCriteriaTypereportPropertiesClass = Class of TReportTypepathToConversionCriteriaTypereportProperties;
  
  { --------------------------------------------------------------------
    TReportTypepathToConversionCriteria
    --------------------------------------------------------------------}
  
  TReportTypepathToConversionCriteria = Class(TGoogleBaseObject)
  Private
    FactivityFilters : TReportTypepathToConversionCriteriaTypeactivityFiltersArray;
    FconversionDimensions : TReportTypepathToConversionCriteriaTypeconversionDimensionsArray;
    FcustomFloodlightVariables : TReportTypepathToConversionCriteriaTypecustomFloodlightVariablesArray;
    FcustomRichMediaEvents : TReportTypepathToConversionCriteriaTypecustomRichMediaEventsArray;
    FdateRange : TDateRange;
    FfloodlightConfigId : TDimensionValue;
    FmetricNames : TStringArray;
    FperInteractionDimensions : TReportTypepathToConversionCriteriaTypeperInteractionDimensionsArray;
    FreportProperties : TReportTypepathToConversionCriteriaTypereportProperties;
  Protected
    //Property setters
    Procedure SetactivityFilters(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypeactivityFiltersArray); virtual;
    Procedure SetconversionDimensions(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypeconversionDimensionsArray); virtual;
    Procedure SetcustomFloodlightVariables(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypecustomFloodlightVariablesArray); virtual;
    Procedure SetcustomRichMediaEvents(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypecustomRichMediaEventsArray); virtual;
    Procedure SetdateRange(AIndex : Integer; AValue : TDateRange); virtual;
    Procedure SetfloodlightConfigId(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetmetricNames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetperInteractionDimensions(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypeperInteractionDimensionsArray); virtual;
    Procedure SetreportProperties(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypereportProperties); virtual;
  Public
  Published
    Property activityFilters : TReportTypepathToConversionCriteriaTypeactivityFiltersArray Index 0 Read FactivityFilters Write SetactivityFilters;
    Property conversionDimensions : TReportTypepathToConversionCriteriaTypeconversionDimensionsArray Index 8 Read FconversionDimensions Write SetconversionDimensions;
    Property customFloodlightVariables : TReportTypepathToConversionCriteriaTypecustomFloodlightVariablesArray Index 16 Read FcustomFloodlightVariables Write SetcustomFloodlightVariables;
    Property customRichMediaEvents : TReportTypepathToConversionCriteriaTypecustomRichMediaEventsArray Index 24 Read FcustomRichMediaEvents Write SetcustomRichMediaEvents;
    Property dateRange : TDateRange Index 32 Read FdateRange Write SetdateRange;
    Property floodlightConfigId : TDimensionValue Index 40 Read FfloodlightConfigId Write SetfloodlightConfigId;
    Property metricNames : TStringArray Index 48 Read FmetricNames Write SetmetricNames;
    Property perInteractionDimensions : TReportTypepathToConversionCriteriaTypeperInteractionDimensionsArray Index 56 Read FperInteractionDimensions Write SetperInteractionDimensions;
    Property reportProperties : TReportTypepathToConversionCriteriaTypereportProperties Index 64 Read FreportProperties Write SetreportProperties;
  end;
  TReportTypepathToConversionCriteriaClass = Class of TReportTypepathToConversionCriteria;
  
  { --------------------------------------------------------------------
    TReportTypereachCriteria
    --------------------------------------------------------------------}
  
  TReportTypereachCriteria = Class(TGoogleBaseObject)
  Private
    Factivities : TActivities;
    FcustomRichMediaEvents : TCustomRichMediaEvents;
    FdateRange : TDateRange;
    FdimensionFilters : TReportTypereachCriteriaTypedimensionFiltersArray;
    Fdimensions : TReportTypereachCriteriaTypedimensionsArray;
    FenableAllDimensionCombinations : boolean;
    FmetricNames : TStringArray;
    FreachByFrequencyMetricNames : TStringArray;
  Protected
    //Property setters
    Procedure Setactivities(AIndex : Integer; AValue : TActivities); virtual;
    Procedure SetcustomRichMediaEvents(AIndex : Integer; AValue : TCustomRichMediaEvents); virtual;
    Procedure SetdateRange(AIndex : Integer; AValue : TDateRange); virtual;
    Procedure SetdimensionFilters(AIndex : Integer; AValue : TReportTypereachCriteriaTypedimensionFiltersArray); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TReportTypereachCriteriaTypedimensionsArray); virtual;
    Procedure SetenableAllDimensionCombinations(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetmetricNames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetreachByFrequencyMetricNames(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property activities : TActivities Index 0 Read Factivities Write Setactivities;
    Property customRichMediaEvents : TCustomRichMediaEvents Index 8 Read FcustomRichMediaEvents Write SetcustomRichMediaEvents;
    Property dateRange : TDateRange Index 16 Read FdateRange Write SetdateRange;
    Property dimensionFilters : TReportTypereachCriteriaTypedimensionFiltersArray Index 24 Read FdimensionFilters Write SetdimensionFilters;
    Property dimensions : TReportTypereachCriteriaTypedimensionsArray Index 32 Read Fdimensions Write Setdimensions;
    Property enableAllDimensionCombinations : boolean Index 40 Read FenableAllDimensionCombinations Write SetenableAllDimensionCombinations;
    Property metricNames : TStringArray Index 48 Read FmetricNames Write SetmetricNames;
    Property reachByFrequencyMetricNames : TStringArray Index 56 Read FreachByFrequencyMetricNames Write SetreachByFrequencyMetricNames;
  end;
  TReportTypereachCriteriaClass = Class of TReportTypereachCriteria;
  
  { --------------------------------------------------------------------
    TReportTypeschedule
    --------------------------------------------------------------------}
  
  TReportTypeschedule = Class(TGoogleBaseObject)
  Private
    Factive : boolean;
    Fevery : integer;
    FexpirationDate : TDate;
    Frepeats : String;
    FrepeatsOnWeekDays : TStringArray;
    FrunsOnDayOfMonth : String;
    FstartDate : TDate;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setevery(AIndex : Integer; AValue : integer); virtual;
    Procedure SetexpirationDate(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setrepeats(AIndex : Integer; AValue : String); virtual;
    Procedure SetrepeatsOnWeekDays(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetrunsOnDayOfMonth(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDate); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property every : integer Index 8 Read Fevery Write Setevery;
    Property expirationDate : TDate Index 16 Read FexpirationDate Write SetexpirationDate;
    Property repeats : String Index 24 Read Frepeats Write Setrepeats;
    Property repeatsOnWeekDays : TStringArray Index 32 Read FrepeatsOnWeekDays Write SetrepeatsOnWeekDays;
    Property runsOnDayOfMonth : String Index 40 Read FrunsOnDayOfMonth Write SetrunsOnDayOfMonth;
    Property startDate : TDate Index 48 Read FstartDate Write SetstartDate;
  end;
  TReportTypescheduleClass = Class of TReportTypeschedule;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fcriteria : TReportTypecriteria;
    FcrossDimensionReachCriteria : TReportTypecrossDimensionReachCriteria;
    Fdelivery : TReportTypedelivery;
    Fetag : String;
    FfileName : String;
    FfloodlightCriteria : TReportTypefloodlightCriteria;
    Fformat : String;
    Fid : String;
    Fkind : String;
    FlastModifiedTime : String;
    Fname : String;
    FownerProfileId : String;
    FpathToConversionCriteria : TReportTypepathToConversionCriteria;
    FreachCriteria : TReportTypereachCriteria;
    Fschedule : TReportTypeschedule;
    FsubAccountId : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setcriteria(AIndex : Integer; AValue : TReportTypecriteria); virtual;
    Procedure SetcrossDimensionReachCriteria(AIndex : Integer; AValue : TReportTypecrossDimensionReachCriteria); virtual;
    Procedure Setdelivery(AIndex : Integer; AValue : TReportTypedelivery); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileName(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightCriteria(AIndex : Integer; AValue : TReportTypefloodlightCriteria); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetownerProfileId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpathToConversionCriteria(AIndex : Integer; AValue : TReportTypepathToConversionCriteria); virtual;
    Procedure SetreachCriteria(AIndex : Integer; AValue : TReportTypereachCriteria); virtual;
    Procedure Setschedule(AIndex : Integer; AValue : TReportTypeschedule); virtual;
    Procedure SetsubAccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property criteria : TReportTypecriteria Index 8 Read Fcriteria Write Setcriteria;
    Property crossDimensionReachCriteria : TReportTypecrossDimensionReachCriteria Index 16 Read FcrossDimensionReachCriteria Write SetcrossDimensionReachCriteria;
    Property delivery : TReportTypedelivery Index 24 Read Fdelivery Write Setdelivery;
    Property etag : String Index 32 Read Fetag Write Setetag;
    Property fileName : String Index 40 Read FfileName Write SetfileName;
    Property floodlightCriteria : TReportTypefloodlightCriteria Index 48 Read FfloodlightCriteria Write SetfloodlightCriteria;
    Property format : String Index 56 Read Fformat Write Setformat;
    Property id : String Index 64 Read Fid Write Setid;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property lastModifiedTime : String Index 80 Read FlastModifiedTime Write SetlastModifiedTime;
    Property name : String Index 88 Read Fname Write Setname;
    Property ownerProfileId : String Index 96 Read FownerProfileId Write SetownerProfileId;
    Property pathToConversionCriteria : TReportTypepathToConversionCriteria Index 104 Read FpathToConversionCriteria Write SetpathToConversionCriteria;
    Property reachCriteria : TReportTypereachCriteria Index 112 Read FreachCriteria Write SetreachCriteria;
    Property schedule : TReportTypeschedule Index 120 Read Fschedule Write Setschedule;
    Property subAccountId : String Index 128 Read FsubAccountId Write SetsubAccountId;
    Property _type : String Index 136 Read F_type Write Set_type;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TReportCompatibleFields
    --------------------------------------------------------------------}
  
  TReportCompatibleFields = Class(TGoogleBaseObject)
  Private
    FdimensionFilters : TReportCompatibleFieldsTypedimensionFiltersArray;
    Fdimensions : TReportCompatibleFieldsTypedimensionsArray;
    Fkind : String;
    Fmetrics : TReportCompatibleFieldsTypemetricsArray;
    FpivotedActivityMetrics : TReportCompatibleFieldsTypepivotedActivityMetricsArray;
  Protected
    //Property setters
    Procedure SetdimensionFilters(AIndex : Integer; AValue : TReportCompatibleFieldsTypedimensionFiltersArray); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TReportCompatibleFieldsTypedimensionsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TReportCompatibleFieldsTypemetricsArray); virtual;
    Procedure SetpivotedActivityMetrics(AIndex : Integer; AValue : TReportCompatibleFieldsTypepivotedActivityMetricsArray); virtual;
  Public
  Published
    Property dimensionFilters : TReportCompatibleFieldsTypedimensionFiltersArray Index 0 Read FdimensionFilters Write SetdimensionFilters;
    Property dimensions : TReportCompatibleFieldsTypedimensionsArray Index 8 Read Fdimensions Write Setdimensions;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property metrics : TReportCompatibleFieldsTypemetricsArray Index 24 Read Fmetrics Write Setmetrics;
    Property pivotedActivityMetrics : TReportCompatibleFieldsTypepivotedActivityMetricsArray Index 32 Read FpivotedActivityMetrics Write SetpivotedActivityMetrics;
  end;
  TReportCompatibleFieldsClass = Class of TReportCompatibleFields;
  
  { --------------------------------------------------------------------
    TReportList
    --------------------------------------------------------------------}
  
  TReportList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TReportListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TReportListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TReportListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TReportListClass = Class of TReportList;
  
  { --------------------------------------------------------------------
    TReportsConfiguration
    --------------------------------------------------------------------}
  
  TReportsConfiguration = Class(TGoogleBaseObject)
  Private
    FexposureToConversionEnabled : boolean;
    FlookbackConfiguration : TLookbackConfiguration;
    FreportGenerationTimeZoneId : String;
  Protected
    //Property setters
    Procedure SetexposureToConversionEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); virtual;
    Procedure SetreportGenerationTimeZoneId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property exposureToConversionEnabled : boolean Index 0 Read FexposureToConversionEnabled Write SetexposureToConversionEnabled;
    Property lookbackConfiguration : TLookbackConfiguration Index 8 Read FlookbackConfiguration Write SetlookbackConfiguration;
    Property reportGenerationTimeZoneId : String Index 16 Read FreportGenerationTimeZoneId Write SetreportGenerationTimeZoneId;
  end;
  TReportsConfigurationClass = Class of TReportsConfiguration;
  
  { --------------------------------------------------------------------
    TRichMediaExitOverride
    --------------------------------------------------------------------}
  
  TRichMediaExitOverride = Class(TGoogleBaseObject)
  Private
    FcustomExitUrl : String;
    FexitId : String;
    FuseCustomExitUrl : boolean;
  Protected
    //Property setters
    Procedure SetcustomExitUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetexitId(AIndex : Integer; AValue : String); virtual;
    Procedure SetuseCustomExitUrl(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property customExitUrl : String Index 0 Read FcustomExitUrl Write SetcustomExitUrl;
    Property exitId : String Index 8 Read FexitId Write SetexitId;
    Property useCustomExitUrl : boolean Index 16 Read FuseCustomExitUrl Write SetuseCustomExitUrl;
  end;
  TRichMediaExitOverrideClass = Class of TRichMediaExitOverride;
  
  { --------------------------------------------------------------------
    TSite
    --------------------------------------------------------------------}
  
  TSite = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fapproved : boolean;
    FdirectorySiteId : String;
    FdirectorySiteIdDimensionValue : TDimensionValue;
    Fid : String;
    FidDimensionValue : TDimensionValue;
    FkeyName : String;
    Fkind : String;
    Fname : String;
    FsiteContacts : TSiteTypesiteContactsArray;
    FsiteSettings : TSiteSettings;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setapproved(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdirectorySiteId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdirectorySiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure SetkeyName(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteContacts(AIndex : Integer; AValue : TSiteTypesiteContactsArray); virtual;
    Procedure SetsiteSettings(AIndex : Integer; AValue : TSiteSettings); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property approved : boolean Index 8 Read Fapproved Write Setapproved;
    Property directorySiteId : String Index 16 Read FdirectorySiteId Write SetdirectorySiteId;
    Property directorySiteIdDimensionValue : TDimensionValue Index 24 Read FdirectorySiteIdDimensionValue Write SetdirectorySiteIdDimensionValue;
    Property id : String Index 32 Read Fid Write Setid;
    Property idDimensionValue : TDimensionValue Index 40 Read FidDimensionValue Write SetidDimensionValue;
    Property keyName : String Index 48 Read FkeyName Write SetkeyName;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property name : String Index 64 Read Fname Write Setname;
    Property siteContacts : TSiteTypesiteContactsArray Index 72 Read FsiteContacts Write SetsiteContacts;
    Property siteSettings : TSiteSettings Index 80 Read FsiteSettings Write SetsiteSettings;
    Property subaccountId : String Index 88 Read FsubaccountId Write SetsubaccountId;
  end;
  TSiteClass = Class of TSite;
  
  { --------------------------------------------------------------------
    TSiteContact
    --------------------------------------------------------------------}
  
  TSiteContact = Class(TGoogleBaseObject)
  Private
    Faddress : String;
    FcontactType : String;
    Femail : String;
    FfirstName : String;
    Fid : String;
    FlastName : String;
    Fphone : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontactType(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetfirstName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastName(AIndex : Integer; AValue : String); virtual;
    Procedure Setphone(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property address : String Index 0 Read Faddress Write Setaddress;
    Property contactType : String Index 8 Read FcontactType Write SetcontactType;
    Property email : String Index 16 Read Femail Write Setemail;
    Property firstName : String Index 24 Read FfirstName Write SetfirstName;
    Property id : String Index 32 Read Fid Write Setid;
    Property lastName : String Index 40 Read FlastName Write SetlastName;
    Property phone : String Index 48 Read Fphone Write Setphone;
    Property title : String Index 56 Read Ftitle Write Settitle;
  end;
  TSiteContactClass = Class of TSiteContact;
  
  { --------------------------------------------------------------------
    TSiteSettings
    --------------------------------------------------------------------}
  
  TSiteSettings = Class(TGoogleBaseObject)
  Private
    FactiveViewOptOut : boolean;
    FcreativeSettings : TCreativeSettings;
    FdisableBrandSafeAds : boolean;
    FdisableNewCookie : boolean;
    FlookbackConfiguration : TLookbackConfiguration;
    FtagSetting : TTagSetting;
  Protected
    //Property setters
    Procedure SetactiveViewOptOut(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcreativeSettings(AIndex : Integer; AValue : TCreativeSettings); virtual;
    Procedure SetdisableBrandSafeAds(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdisableNewCookie(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); virtual;
    Procedure SettagSetting(AIndex : Integer; AValue : TTagSetting); virtual;
  Public
  Published
    Property activeViewOptOut : boolean Index 0 Read FactiveViewOptOut Write SetactiveViewOptOut;
    Property creativeSettings : TCreativeSettings Index 8 Read FcreativeSettings Write SetcreativeSettings;
    Property disableBrandSafeAds : boolean Index 16 Read FdisableBrandSafeAds Write SetdisableBrandSafeAds;
    Property disableNewCookie : boolean Index 24 Read FdisableNewCookie Write SetdisableNewCookie;
    Property lookbackConfiguration : TLookbackConfiguration Index 32 Read FlookbackConfiguration Write SetlookbackConfiguration;
    Property tagSetting : TTagSetting Index 40 Read FtagSetting Write SettagSetting;
  end;
  TSiteSettingsClass = Class of TSiteSettings;
  
  { --------------------------------------------------------------------
    TSitesListResponse
    --------------------------------------------------------------------}
  
  TSitesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fsites : TSitesListResponseTypesitesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setsites(AIndex : Integer; AValue : TSitesListResponseTypesitesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property sites : TSitesListResponseTypesitesArray Index 16 Read Fsites Write Setsites;
  end;
  TSitesListResponseClass = Class of TSitesListResponse;
  
  { --------------------------------------------------------------------
    TSize
    --------------------------------------------------------------------}
  
  TSize = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fiab : boolean;
    Fid : String;
    Fkind : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setiab(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property iab : boolean Index 8 Read Fiab Write Setiab;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property width : integer Index 32 Read Fwidth Write Setwidth;
  end;
  TSizeClass = Class of TSize;
  
  { --------------------------------------------------------------------
    TSizesListResponse
    --------------------------------------------------------------------}
  
  TSizesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fsizes : TSizesListResponseTypesizesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsizes(AIndex : Integer; AValue : TSizesListResponseTypesizesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property sizes : TSizesListResponseTypesizesArray Index 8 Read Fsizes Write Setsizes;
  end;
  TSizesListResponseClass = Class of TSizesListResponse;
  
  { --------------------------------------------------------------------
    TSortedDimension
    --------------------------------------------------------------------}
  
  TSortedDimension = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fname : String;
    FsortOrder : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsortOrder(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property name : String Index 8 Read Fname Write Setname;
    Property sortOrder : String Index 16 Read FsortOrder Write SetsortOrder;
  end;
  TSortedDimensionClass = Class of TSortedDimension;
  
  { --------------------------------------------------------------------
    TSubaccount
    --------------------------------------------------------------------}
  
  TSubaccount = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FavailablePermissionIds : TStringArray;
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetavailablePermissionIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property availablePermissionIds : TStringArray Index 8 Read FavailablePermissionIds Write SetavailablePermissionIds;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
  end;
  TSubaccountClass = Class of TSubaccount;
  
  { --------------------------------------------------------------------
    TSubaccountsListResponse
    --------------------------------------------------------------------}
  
  TSubaccountsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fsubaccounts : TSubaccountsListResponseTypesubaccountsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setsubaccounts(AIndex : Integer; AValue : TSubaccountsListResponseTypesubaccountsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property subaccounts : TSubaccountsListResponseTypesubaccountsArray Index 16 Read Fsubaccounts Write Setsubaccounts;
  end;
  TSubaccountsListResponseClass = Class of TSubaccountsListResponse;
  
  { --------------------------------------------------------------------
    TTagData
    --------------------------------------------------------------------}
  
  TTagData = Class(TGoogleBaseObject)
  Private
    FadId : String;
    FclickTag : String;
    FcreativeId : String;
    Fformat : String;
    FimpressionTag : String;
  Protected
    //Property setters
    Procedure SetadId(AIndex : Integer; AValue : String); virtual;
    Procedure SetclickTag(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreativeId(AIndex : Integer; AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure SetimpressionTag(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adId : String Index 0 Read FadId Write SetadId;
    Property clickTag : String Index 8 Read FclickTag Write SetclickTag;
    Property creativeId : String Index 16 Read FcreativeId Write SetcreativeId;
    Property format : String Index 24 Read Fformat Write Setformat;
    Property impressionTag : String Index 32 Read FimpressionTag Write SetimpressionTag;
  end;
  TTagDataClass = Class of TTagData;
  
  { --------------------------------------------------------------------
    TTagSetting
    --------------------------------------------------------------------}
  
  TTagSetting = Class(TGoogleBaseObject)
  Private
    FadditionalKeyValues : String;
    FincludeClickThroughUrls : boolean;
    FincludeClickTracking : boolean;
    FkeywordOption : String;
  Protected
    //Property setters
    Procedure SetadditionalKeyValues(AIndex : Integer; AValue : String); virtual;
    Procedure SetincludeClickThroughUrls(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetincludeClickTracking(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetkeywordOption(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property additionalKeyValues : String Index 0 Read FadditionalKeyValues Write SetadditionalKeyValues;
    Property includeClickThroughUrls : boolean Index 8 Read FincludeClickThroughUrls Write SetincludeClickThroughUrls;
    Property includeClickTracking : boolean Index 16 Read FincludeClickTracking Write SetincludeClickTracking;
    Property keywordOption : String Index 24 Read FkeywordOption Write SetkeywordOption;
  end;
  TTagSettingClass = Class of TTagSetting;
  
  { --------------------------------------------------------------------
    TTagSettings
    --------------------------------------------------------------------}
  
  TTagSettings = Class(TGoogleBaseObject)
  Private
    FdynamicTagEnabled : boolean;
    FimageTagEnabled : boolean;
  Protected
    //Property setters
    Procedure SetdynamicTagEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetimageTagEnabled(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property dynamicTagEnabled : boolean Index 0 Read FdynamicTagEnabled Write SetdynamicTagEnabled;
    Property imageTagEnabled : boolean Index 8 Read FimageTagEnabled Write SetimageTagEnabled;
  end;
  TTagSettingsClass = Class of TTagSettings;
  
  { --------------------------------------------------------------------
    TTargetWindow
    --------------------------------------------------------------------}
  
  TTargetWindow = Class(TGoogleBaseObject)
  Private
    FcustomHtml : String;
    FtargetWindowOption : String;
  Protected
    //Property setters
    Procedure SetcustomHtml(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetWindowOption(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property customHtml : String Index 0 Read FcustomHtml Write SetcustomHtml;
    Property targetWindowOption : String Index 8 Read FtargetWindowOption Write SettargetWindowOption;
  end;
  TTargetWindowClass = Class of TTargetWindow;
  
  { --------------------------------------------------------------------
    TTargetableRemarketingList
    --------------------------------------------------------------------}
  
  TTargetableRemarketingList = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Factive : boolean;
    FadvertiserId : String;
    FadvertiserIdDimensionValue : TDimensionValue;
    Fdescription : String;
    Fid : String;
    Fkind : String;
    FlifeSpan : String;
    FlistSize : String;
    FlistSource : String;
    Fname : String;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlifeSpan(AIndex : Integer; AValue : String); virtual;
    Procedure SetlistSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetlistSource(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property advertiserId : String Index 16 Read FadvertiserId Write SetadvertiserId;
    Property advertiserIdDimensionValue : TDimensionValue Index 24 Read FadvertiserIdDimensionValue Write SetadvertiserIdDimensionValue;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property id : String Index 40 Read Fid Write Setid;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property lifeSpan : String Index 56 Read FlifeSpan Write SetlifeSpan;
    Property listSize : String Index 64 Read FlistSize Write SetlistSize;
    Property listSource : String Index 72 Read FlistSource Write SetlistSource;
    Property name : String Index 80 Read Fname Write Setname;
    Property subaccountId : String Index 88 Read FsubaccountId Write SetsubaccountId;
  end;
  TTargetableRemarketingListClass = Class of TTargetableRemarketingList;
  
  { --------------------------------------------------------------------
    TTargetableRemarketingListsListResponse
    --------------------------------------------------------------------}
  
  TTargetableRemarketingListsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    FtargetableRemarketingLists : TTargetableRemarketingListsListResponseTypetargetableRemarketingListsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetableRemarketingLists(AIndex : Integer; AValue : TTargetableRemarketingListsListResponseTypetargetableRemarketingListsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property targetableRemarketingLists : TTargetableRemarketingListsListResponseTypetargetableRemarketingListsArray Index 16 Read FtargetableRemarketingLists Write SettargetableRemarketingLists;
  end;
  TTargetableRemarketingListsListResponseClass = Class of TTargetableRemarketingListsListResponse;
  
  { --------------------------------------------------------------------
    TTechnologyTargeting
    --------------------------------------------------------------------}
  
  TTechnologyTargeting = Class(TGoogleBaseObject)
  Private
    Fbrowsers : TTechnologyTargetingTypebrowsersArray;
    FconnectionTypes : TTechnologyTargetingTypeconnectionTypesArray;
    FmobileCarriers : TTechnologyTargetingTypemobileCarriersArray;
    FoperatingSystemVersions : TTechnologyTargetingTypeoperatingSystemVersionsArray;
    FoperatingSystems : TTechnologyTargetingTypeoperatingSystemsArray;
    FplatformTypes : TTechnologyTargetingTypeplatformTypesArray;
  Protected
    //Property setters
    Procedure Setbrowsers(AIndex : Integer; AValue : TTechnologyTargetingTypebrowsersArray); virtual;
    Procedure SetconnectionTypes(AIndex : Integer; AValue : TTechnologyTargetingTypeconnectionTypesArray); virtual;
    Procedure SetmobileCarriers(AIndex : Integer; AValue : TTechnologyTargetingTypemobileCarriersArray); virtual;
    Procedure SetoperatingSystemVersions(AIndex : Integer; AValue : TTechnologyTargetingTypeoperatingSystemVersionsArray); virtual;
    Procedure SetoperatingSystems(AIndex : Integer; AValue : TTechnologyTargetingTypeoperatingSystemsArray); virtual;
    Procedure SetplatformTypes(AIndex : Integer; AValue : TTechnologyTargetingTypeplatformTypesArray); virtual;
  Public
  Published
    Property browsers : TTechnologyTargetingTypebrowsersArray Index 0 Read Fbrowsers Write Setbrowsers;
    Property connectionTypes : TTechnologyTargetingTypeconnectionTypesArray Index 8 Read FconnectionTypes Write SetconnectionTypes;
    Property mobileCarriers : TTechnologyTargetingTypemobileCarriersArray Index 16 Read FmobileCarriers Write SetmobileCarriers;
    Property operatingSystemVersions : TTechnologyTargetingTypeoperatingSystemVersionsArray Index 24 Read FoperatingSystemVersions Write SetoperatingSystemVersions;
    Property operatingSystems : TTechnologyTargetingTypeoperatingSystemsArray Index 32 Read FoperatingSystems Write SetoperatingSystems;
    Property platformTypes : TTechnologyTargetingTypeplatformTypesArray Index 40 Read FplatformTypes Write SetplatformTypes;
  end;
  TTechnologyTargetingClass = Class of TTechnologyTargeting;
  
  { --------------------------------------------------------------------
    TThirdPartyTrackingUrl
    --------------------------------------------------------------------}
  
  TThirdPartyTrackingUrl = Class(TGoogleBaseObject)
  Private
    FthirdPartyUrlType : String;
    Furl : String;
  Protected
    //Property setters
    Procedure SetthirdPartyUrlType(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property thirdPartyUrlType : String Index 0 Read FthirdPartyUrlType Write SetthirdPartyUrlType;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TThirdPartyTrackingUrlClass = Class of TThirdPartyTrackingUrl;
  
  { --------------------------------------------------------------------
    TUserDefinedVariableConfiguration
    --------------------------------------------------------------------}
  
  TUserDefinedVariableConfiguration = Class(TGoogleBaseObject)
  Private
    FdataType : String;
    FreportName : String;
    FvariableType : String;
  Protected
    //Property setters
    Procedure SetdataType(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportName(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariableType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dataType : String Index 0 Read FdataType Write SetdataType;
    Property reportName : String Index 8 Read FreportName Write SetreportName;
    Property variableType : String Index 16 Read FvariableType Write SetvariableType;
  end;
  TUserDefinedVariableConfigurationClass = Class of TUserDefinedVariableConfiguration;
  
  { --------------------------------------------------------------------
    TUserProfile
    --------------------------------------------------------------------}
  
  TUserProfile = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FaccountName : String;
    Fetag : String;
    Fkind : String;
    FprofileId : String;
    FsubAccountId : String;
    FsubAccountName : String;
    FuserName : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetaccountName(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubAccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubAccountName(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property accountName : String Index 8 Read FaccountName Write SetaccountName;
    Property etag : String Index 16 Read Fetag Write Setetag;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property profileId : String Index 32 Read FprofileId Write SetprofileId;
    Property subAccountId : String Index 40 Read FsubAccountId Write SetsubAccountId;
    Property subAccountName : String Index 48 Read FsubAccountName Write SetsubAccountName;
    Property userName : String Index 56 Read FuserName Write SetuserName;
  end;
  TUserProfileClass = Class of TUserProfile;
  
  { --------------------------------------------------------------------
    TUserProfileList
    --------------------------------------------------------------------}
  
  TUserProfileList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TUserProfileListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TUserProfileListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TUserProfileListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TUserProfileListClass = Class of TUserProfileList;
  
  { --------------------------------------------------------------------
    TUserRole
    --------------------------------------------------------------------}
  
  TUserRole = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FdefaultUserRole : boolean;
    Fid : String;
    Fkind : String;
    Fname : String;
    FparentUserRoleId : String;
    Fpermissions : TUserRoleTypepermissionsArray;
    FsubaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultUserRole(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentUserRoleId(AIndex : Integer; AValue : String); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TUserRoleTypepermissionsArray); virtual;
    Procedure SetsubaccountId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property defaultUserRole : boolean Index 8 Read FdefaultUserRole Write SetdefaultUserRole;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property parentUserRoleId : String Index 40 Read FparentUserRoleId Write SetparentUserRoleId;
    Property permissions : TUserRoleTypepermissionsArray Index 48 Read Fpermissions Write Setpermissions;
    Property subaccountId : String Index 56 Read FsubaccountId Write SetsubaccountId;
  end;
  TUserRoleClass = Class of TUserRole;
  
  { --------------------------------------------------------------------
    TUserRolePermission
    --------------------------------------------------------------------}
  
  TUserRolePermission = Class(TGoogleBaseObject)
  Private
    Favailability : String;
    Fid : String;
    Fkind : String;
    Fname : String;
    FpermissionGroupId : String;
  Protected
    //Property setters
    Procedure Setavailability(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpermissionGroupId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property availability : String Index 0 Read Favailability Write Setavailability;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
    Property permissionGroupId : String Index 32 Read FpermissionGroupId Write SetpermissionGroupId;
  end;
  TUserRolePermissionClass = Class of TUserRolePermission;
  
  { --------------------------------------------------------------------
    TUserRolePermissionGroup
    --------------------------------------------------------------------}
  
  TUserRolePermissionGroup = Class(TGoogleBaseObject)
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
  TUserRolePermissionGroupClass = Class of TUserRolePermissionGroup;
  
  { --------------------------------------------------------------------
    TUserRolePermissionGroupsListResponse
    --------------------------------------------------------------------}
  
  TUserRolePermissionGroupsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FuserRolePermissionGroups : TUserRolePermissionGroupsListResponseTypeuserRolePermissionGroupsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserRolePermissionGroups(AIndex : Integer; AValue : TUserRolePermissionGroupsListResponseTypeuserRolePermissionGroupsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property userRolePermissionGroups : TUserRolePermissionGroupsListResponseTypeuserRolePermissionGroupsArray Index 8 Read FuserRolePermissionGroups Write SetuserRolePermissionGroups;
  end;
  TUserRolePermissionGroupsListResponseClass = Class of TUserRolePermissionGroupsListResponse;
  
  { --------------------------------------------------------------------
    TUserRolePermissionsListResponse
    --------------------------------------------------------------------}
  
  TUserRolePermissionsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FuserRolePermissions : TUserRolePermissionsListResponseTypeuserRolePermissionsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserRolePermissions(AIndex : Integer; AValue : TUserRolePermissionsListResponseTypeuserRolePermissionsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property userRolePermissions : TUserRolePermissionsListResponseTypeuserRolePermissionsArray Index 8 Read FuserRolePermissions Write SetuserRolePermissions;
  end;
  TUserRolePermissionsListResponseClass = Class of TUserRolePermissionsListResponse;
  
  { --------------------------------------------------------------------
    TUserRolesListResponse
    --------------------------------------------------------------------}
  
  TUserRolesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    FuserRoles : TUserRolesListResponseTypeuserRolesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserRoles(AIndex : Integer; AValue : TUserRolesListResponseTypeuserRolesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property userRoles : TUserRolesListResponseTypeuserRolesArray Index 16 Read FuserRoles Write SetuserRoles;
  end;
  TUserRolesListResponseClass = Class of TUserRolesListResponse;
  
  { --------------------------------------------------------------------
    TAccountActiveAdSummariesResource
    --------------------------------------------------------------------}
  
  TAccountActiveAdSummariesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(profileId: string; summaryAccountId: string) : TAccountActiveAdSummary;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountPermissionGroupsResource
    --------------------------------------------------------------------}
  
  TAccountPermissionGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TAccountPermissionGroup;
    Function List(profileId: string) : TAccountPermissionGroupsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountPermissionsResource
    --------------------------------------------------------------------}
  
  TAccountPermissionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TAccountPermission;
    Function List(profileId: string) : TAccountPermissionsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountUserProfilesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountUserProfilesResource, method List
  
  TAccountUserProfilesListOptions = Record
    active : boolean;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
    subaccountId : int64;
    userRoleId : int64;
  end;
  
  
  //Optional query Options for TAccountUserProfilesResource, method Patch
  
  TAccountUserProfilesPatchOptions = Record
    id : int64;
  end;
  
  TAccountUserProfilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TAccountUserProfile;
    Function Insert(profileId: string; aAccountUserProfile : TAccountUserProfile) : TAccountUserProfile;
    Function List(profileId: string; AQuery : string  = '') : TAccountUserProfilesListResponse;
    Function List(profileId: string; AQuery : TAccountUserProfileslistOptions) : TAccountUserProfilesListResponse;
    Function Patch(profileId: string; aAccountUserProfile : TAccountUserProfile; AQuery : string  = '') : TAccountUserProfile;
    Function Patch(profileId: string; aAccountUserProfile : TAccountUserProfile; AQuery : TAccountUserProfilespatchOptions) : TAccountUserProfile;
    Function Update(profileId: string; aAccountUserProfile : TAccountUserProfile) : TAccountUserProfile;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsResource, method List
  
  TAccountsListOptions = Record
    active : boolean;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TAccountsResource, method Patch
  
  TAccountsPatchOptions = Record
    id : int64;
  end;
  
  TAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TAccount;
    Function List(profileId: string; AQuery : string  = '') : TAccountsListResponse;
    Function List(profileId: string; AQuery : TAccountslistOptions) : TAccountsListResponse;
    Function Patch(profileId: string; aAccount : TAccount; AQuery : string  = '') : TAccount;
    Function Patch(profileId: string; aAccount : TAccount; AQuery : TAccountspatchOptions) : TAccount;
    Function Update(profileId: string; aAccount : TAccount) : TAccount;
  end;
  
  
  { --------------------------------------------------------------------
    TAdsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdsResource, method List
  
  TAdsListOptions = Record
    active : boolean;
    advertiserId : int64;
    archived : boolean;
    audienceSegmentIds : int64;
    campaignIds : int64;
    compatibility : String;
    creativeIds : int64;
    creativeOptimizationConfigurationIds : int64;
    creativeType : String;
    dynamicClickTracker : boolean;
    ids : int64;
    landingPageIds : int64;
    maxResults : integer;
    overriddenEventTagId : int64;
    pageToken : String;
    placementIds : int64;
    remarketingListIds : int64;
    searchString : String;
    sizeIds : int64;
    sortField : String;
    sortOrder : String;
    sslCompliant : boolean;
    sslRequired : boolean;
    _type : String;
  end;
  
  
  //Optional query Options for TAdsResource, method Patch
  
  TAdsPatchOptions = Record
    id : int64;
  end;
  
  TAdsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TAd;
    Function Insert(profileId: string; aAd : TAd) : TAd;
    Function List(profileId: string; AQuery : string  = '') : TAdsListResponse;
    Function List(profileId: string; AQuery : TAdslistOptions) : TAdsListResponse;
    Function Patch(profileId: string; aAd : TAd; AQuery : string  = '') : TAd;
    Function Patch(profileId: string; aAd : TAd; AQuery : TAdspatchOptions) : TAd;
    Function Update(profileId: string; aAd : TAd) : TAd;
  end;
  
  
  { --------------------------------------------------------------------
    TAdvertiserGroupsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdvertiserGroupsResource, method List
  
  TAdvertiserGroupsListOptions = Record
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TAdvertiserGroupsResource, method Patch
  
  TAdvertiserGroupsPatchOptions = Record
    id : int64;
  end;
  
  TAdvertiserGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; profileId: string);
    Function Get(id: string; profileId: string) : TAdvertiserGroup;
    Function Insert(profileId: string; aAdvertiserGroup : TAdvertiserGroup) : TAdvertiserGroup;
    Function List(profileId: string; AQuery : string  = '') : TAdvertiserGroupsListResponse;
    Function List(profileId: string; AQuery : TAdvertiserGroupslistOptions) : TAdvertiserGroupsListResponse;
    Function Patch(profileId: string; aAdvertiserGroup : TAdvertiserGroup; AQuery : string  = '') : TAdvertiserGroup;
    Function Patch(profileId: string; aAdvertiserGroup : TAdvertiserGroup; AQuery : TAdvertiserGroupspatchOptions) : TAdvertiserGroup;
    Function Update(profileId: string; aAdvertiserGroup : TAdvertiserGroup) : TAdvertiserGroup;
  end;
  
  
  { --------------------------------------------------------------------
    TAdvertisersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdvertisersResource, method List
  
  TAdvertisersListOptions = Record
    advertiserGroupIds : int64;
    floodlightConfigurationIds : int64;
    ids : int64;
    includeAdvertisersWithoutGroupsOnly : boolean;
    maxResults : integer;
    onlyParent : boolean;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
    status : String;
    subaccountId : int64;
  end;
  
  
  //Optional query Options for TAdvertisersResource, method Patch
  
  TAdvertisersPatchOptions = Record
    id : int64;
  end;
  
  TAdvertisersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TAdvertiser;
    Function Insert(profileId: string; aAdvertiser : TAdvertiser) : TAdvertiser;
    Function List(profileId: string; AQuery : string  = '') : TAdvertisersListResponse;
    Function List(profileId: string; AQuery : TAdvertiserslistOptions) : TAdvertisersListResponse;
    Function Patch(profileId: string; aAdvertiser : TAdvertiser; AQuery : string  = '') : TAdvertiser;
    Function Patch(profileId: string; aAdvertiser : TAdvertiser; AQuery : TAdvertiserspatchOptions) : TAdvertiser;
    Function Update(profileId: string; aAdvertiser : TAdvertiser) : TAdvertiser;
  end;
  
  
  { --------------------------------------------------------------------
    TBrowsersResource
    --------------------------------------------------------------------}
  
  TBrowsersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(profileId: string) : TBrowsersListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCampaignCreativeAssociationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCampaignCreativeAssociationsResource, method List
  
  TCampaignCreativeAssociationsListOptions = Record
    maxResults : integer;
    pageToken : String;
    sortOrder : String;
  end;
  
  TCampaignCreativeAssociationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(campaignId: string; profileId: string; aCampaignCreativeAssociation : TCampaignCreativeAssociation) : TCampaignCreativeAssociation;
    Function List(campaignId: string; profileId: string; AQuery : string  = '') : TCampaignCreativeAssociationsListResponse;
    Function List(campaignId: string; profileId: string; AQuery : TCampaignCreativeAssociationslistOptions) : TCampaignCreativeAssociationsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCampaignsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCampaignsResource, method Insert
  
  TCampaignsInsertOptions = Record
    defaultLandingPageName : String;
    defaultLandingPageUrl : String;
  end;
  
  
  //Optional query Options for TCampaignsResource, method List
  
  TCampaignsListOptions = Record
    advertiserGroupIds : int64;
    advertiserIds : int64;
    archived : boolean;
    atLeastOneOptimizationActivity : boolean;
    excludedIds : int64;
    ids : int64;
    maxResults : integer;
    overriddenEventTagId : int64;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
    subaccountId : int64;
  end;
  
  
  //Optional query Options for TCampaignsResource, method Patch
  
  TCampaignsPatchOptions = Record
    id : int64;
  end;
  
  TCampaignsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TCampaign;
    Function Insert(profileId: string; aCampaign : TCampaign; AQuery : string  = '') : TCampaign;
    Function Insert(profileId: string; aCampaign : TCampaign; AQuery : TCampaignsinsertOptions) : TCampaign;
    Function List(profileId: string; AQuery : string  = '') : TCampaignsListResponse;
    Function List(profileId: string; AQuery : TCampaignslistOptions) : TCampaignsListResponse;
    Function Patch(profileId: string; aCampaign : TCampaign; AQuery : string  = '') : TCampaign;
    Function Patch(profileId: string; aCampaign : TCampaign; AQuery : TCampaignspatchOptions) : TCampaign;
    Function Update(profileId: string; aCampaign : TCampaign) : TCampaign;
  end;
  
  
  { --------------------------------------------------------------------
    TChangeLogsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TChangeLogsResource, method List
  
  TChangeLogsListOptions = Record
    action : String;
    ids : int64;
    maxChangeTime : String;
    maxResults : integer;
    minChangeTime : String;
    objectIds : int64;
    objectType : String;
    pageToken : String;
    searchString : String;
    userProfileIds : int64;
  end;
  
  TChangeLogsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TChangeLog;
    Function List(profileId: string; AQuery : string  = '') : TChangeLogsListResponse;
    Function List(profileId: string; AQuery : TChangeLogslistOptions) : TChangeLogsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCitiesResource, method List
  
  TCitiesListOptions = Record
    countryDartIds : int64;
    dartIds : int64;
    namePrefix : String;
    regionDartIds : int64;
  end;
  
  TCitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(profileId: string; AQuery : string  = '') : TCitiesListResponse;
    Function List(profileId: string; AQuery : TCitieslistOptions) : TCitiesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TConnectionTypesResource
    --------------------------------------------------------------------}
  
  TConnectionTypesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TConnectionType;
    Function List(profileId: string) : TConnectionTypesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TContentCategoriesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TContentCategoriesResource, method List
  
  TContentCategoriesListOptions = Record
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TContentCategoriesResource, method Patch
  
  TContentCategoriesPatchOptions = Record
    id : int64;
  end;
  
  TContentCategoriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; profileId: string);
    Function Get(id: string; profileId: string) : TContentCategory;
    Function Insert(profileId: string; aContentCategory : TContentCategory) : TContentCategory;
    Function List(profileId: string; AQuery : string  = '') : TContentCategoriesListResponse;
    Function List(profileId: string; AQuery : TContentCategorieslistOptions) : TContentCategoriesListResponse;
    Function Patch(profileId: string; aContentCategory : TContentCategory; AQuery : string  = '') : TContentCategory;
    Function Patch(profileId: string; aContentCategory : TContentCategory; AQuery : TContentCategoriespatchOptions) : TContentCategory;
    Function Update(profileId: string; aContentCategory : TContentCategory) : TContentCategory;
  end;
  
  
  { --------------------------------------------------------------------
    TCountriesResource
    --------------------------------------------------------------------}
  
  TCountriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(dartId: string; profileId: string) : TCountry;
    Function List(profileId: string) : TCountriesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCreativeAssetsResource
    --------------------------------------------------------------------}
  
  TCreativeAssetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(advertiserId: string; profileId: string; aCreativeAssetMetadata : TCreativeAssetMetadata) : TCreativeAssetMetadata;
  end;
  
  
  { --------------------------------------------------------------------
    TCreativeFieldValuesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCreativeFieldValuesResource, method List
  
  TCreativeFieldValuesListOptions = Record
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TCreativeFieldValuesResource, method Patch
  
  TCreativeFieldValuesPatchOptions = Record
    id : int64;
  end;
  
  TCreativeFieldValuesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(creativeFieldId: string; id: string; profileId: string);
    Function Get(creativeFieldId: string; id: string; profileId: string) : TCreativeFieldValue;
    Function Insert(creativeFieldId: string; profileId: string; aCreativeFieldValue : TCreativeFieldValue) : TCreativeFieldValue;
    Function List(creativeFieldId: string; profileId: string; AQuery : string  = '') : TCreativeFieldValuesListResponse;
    Function List(creativeFieldId: string; profileId: string; AQuery : TCreativeFieldValueslistOptions) : TCreativeFieldValuesListResponse;
    Function Patch(creativeFieldId: string; profileId: string; aCreativeFieldValue : TCreativeFieldValue; AQuery : string  = '') : TCreativeFieldValue;
    Function Patch(creativeFieldId: string; profileId: string; aCreativeFieldValue : TCreativeFieldValue; AQuery : TCreativeFieldValuespatchOptions) : TCreativeFieldValue;
    Function Update(creativeFieldId: string; profileId: string; aCreativeFieldValue : TCreativeFieldValue) : TCreativeFieldValue;
  end;
  
  
  { --------------------------------------------------------------------
    TCreativeFieldsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCreativeFieldsResource, method List
  
  TCreativeFieldsListOptions = Record
    advertiserIds : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TCreativeFieldsResource, method Patch
  
  TCreativeFieldsPatchOptions = Record
    id : int64;
  end;
  
  TCreativeFieldsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; profileId: string);
    Function Get(id: string; profileId: string) : TCreativeField;
    Function Insert(profileId: string; aCreativeField : TCreativeField) : TCreativeField;
    Function List(profileId: string; AQuery : string  = '') : TCreativeFieldsListResponse;
    Function List(profileId: string; AQuery : TCreativeFieldslistOptions) : TCreativeFieldsListResponse;
    Function Patch(profileId: string; aCreativeField : TCreativeField; AQuery : string  = '') : TCreativeField;
    Function Patch(profileId: string; aCreativeField : TCreativeField; AQuery : TCreativeFieldspatchOptions) : TCreativeField;
    Function Update(profileId: string; aCreativeField : TCreativeField) : TCreativeField;
  end;
  
  
  { --------------------------------------------------------------------
    TCreativeGroupsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCreativeGroupsResource, method List
  
  TCreativeGroupsListOptions = Record
    advertiserIds : int64;
    groupNumber : integer;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TCreativeGroupsResource, method Patch
  
  TCreativeGroupsPatchOptions = Record
    id : int64;
  end;
  
  TCreativeGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TCreativeGroup;
    Function Insert(profileId: string; aCreativeGroup : TCreativeGroup) : TCreativeGroup;
    Function List(profileId: string; AQuery : string  = '') : TCreativeGroupsListResponse;
    Function List(profileId: string; AQuery : TCreativeGroupslistOptions) : TCreativeGroupsListResponse;
    Function Patch(profileId: string; aCreativeGroup : TCreativeGroup; AQuery : string  = '') : TCreativeGroup;
    Function Patch(profileId: string; aCreativeGroup : TCreativeGroup; AQuery : TCreativeGroupspatchOptions) : TCreativeGroup;
    Function Update(profileId: string; aCreativeGroup : TCreativeGroup) : TCreativeGroup;
  end;
  
  
  { --------------------------------------------------------------------
    TCreativesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCreativesResource, method List
  
  TCreativesListOptions = Record
    active : boolean;
    advertiserId : int64;
    archived : boolean;
    campaignId : int64;
    companionCreativeIds : int64;
    creativeFieldIds : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    renderingIds : int64;
    searchString : String;
    sizeIds : int64;
    sortField : String;
    sortOrder : String;
    studioCreativeId : int64;
    types : String;
  end;
  
  
  //Optional query Options for TCreativesResource, method Patch
  
  TCreativesPatchOptions = Record
    id : int64;
  end;
  
  TCreativesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TCreative;
    Function Insert(profileId: string; aCreative : TCreative) : TCreative;
    Function List(profileId: string; AQuery : string  = '') : TCreativesListResponse;
    Function List(profileId: string; AQuery : TCreativeslistOptions) : TCreativesListResponse;
    Function Patch(profileId: string; aCreative : TCreative; AQuery : string  = '') : TCreative;
    Function Patch(profileId: string; aCreative : TCreative; AQuery : TCreativespatchOptions) : TCreative;
    Function Update(profileId: string; aCreative : TCreative) : TCreative;
  end;
  
  
  { --------------------------------------------------------------------
    TDimensionValuesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDimensionValuesResource, method Query
  
  TDimensionValuesQueryOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TDimensionValuesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Query(profileId: string; aDimensionValueRequest : TDimensionValueRequest; AQuery : string  = '') : TDimensionValueList;
    Function Query(profileId: string; aDimensionValueRequest : TDimensionValueRequest; AQuery : TDimensionValuesqueryOptions) : TDimensionValueList;
  end;
  
  
  { --------------------------------------------------------------------
    TDirectorySiteContactsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDirectorySiteContactsResource, method List
  
  TDirectorySiteContactsListOptions = Record
    directorySiteIds : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  TDirectorySiteContactsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TDirectorySiteContact;
    Function List(profileId: string; AQuery : string  = '') : TDirectorySiteContactsListResponse;
    Function List(profileId: string; AQuery : TDirectorySiteContactslistOptions) : TDirectorySiteContactsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDirectorySitesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDirectorySitesResource, method List
  
  TDirectorySitesListOptions = Record
    acceptsInStreamVideoPlacements : boolean;
    acceptsInterstitialPlacements : boolean;
    acceptsPublisherPaidPlacements : boolean;
    active : boolean;
    countryId : int64;
    dfp_network_code : String;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    parentId : int64;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  TDirectorySitesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TDirectorySite;
    Function Insert(profileId: string; aDirectorySite : TDirectorySite) : TDirectorySite;
    Function List(profileId: string; AQuery : string  = '') : TDirectorySitesListResponse;
    Function List(profileId: string; AQuery : TDirectorySiteslistOptions) : TDirectorySitesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TEventTagsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEventTagsResource, method List
  
  TEventTagsListOptions = Record
    adId : int64;
    advertiserId : int64;
    campaignId : int64;
    definitionsOnly : boolean;
    enabled : boolean;
    eventTagTypes : String;
    ids : int64;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TEventTagsResource, method Patch
  
  TEventTagsPatchOptions = Record
    id : int64;
  end;
  
  TEventTagsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; profileId: string);
    Function Get(id: string; profileId: string) : TEventTag;
    Function Insert(profileId: string; aEventTag : TEventTag) : TEventTag;
    Function List(profileId: string; AQuery : string  = '') : TEventTagsListResponse;
    Function List(profileId: string; AQuery : TEventTagslistOptions) : TEventTagsListResponse;
    Function Patch(profileId: string; aEventTag : TEventTag; AQuery : string  = '') : TEventTag;
    Function Patch(profileId: string; aEventTag : TEventTag; AQuery : TEventTagspatchOptions) : TEventTag;
    Function Update(profileId: string; aEventTag : TEventTag) : TEventTag;
  end;
  
  
  { --------------------------------------------------------------------
    TFilesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TFilesResource, method List
  
  TFilesListOptions = Record
    maxResults : integer;
    pageToken : String;
    scope : String;
    sortField : String;
    sortOrder : String;
  end;
  
  TFilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(fileId: string; reportId: string) : TFile;
    Function List(profileId: string; AQuery : string  = '') : TFileList;
    Function List(profileId: string; AQuery : TFileslistOptions) : TFileList;
  end;
  
  
  { --------------------------------------------------------------------
    TFloodlightActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TFloodlightActivitiesResource, method Generatetag
  
  TFloodlightActivitiesGeneratetagOptions = Record
    floodlightActivityId : int64;
  end;
  
  
  //Optional query Options for TFloodlightActivitiesResource, method List
  
  TFloodlightActivitiesListOptions = Record
    advertiserId : int64;
    floodlightActivityGroupIds : int64;
    floodlightActivityGroupName : String;
    floodlightActivityGroupTagString : String;
    floodlightActivityGroupType : String;
    floodlightConfigurationId : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
    tagString : String;
  end;
  
  
  //Optional query Options for TFloodlightActivitiesResource, method Patch
  
  TFloodlightActivitiesPatchOptions = Record
    id : int64;
  end;
  
  TFloodlightActivitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; profileId: string);
    Function Generatetag(profileId: string; AQuery : string  = '') : TFloodlightActivitiesGenerateTagResponse;
    Function Generatetag(profileId: string; AQuery : TFloodlightActivitiesgeneratetagOptions) : TFloodlightActivitiesGenerateTagResponse;
    Function Get(id: string; profileId: string) : TFloodlightActivity;
    Function Insert(profileId: string; aFloodlightActivity : TFloodlightActivity) : TFloodlightActivity;
    Function List(profileId: string; AQuery : string  = '') : TFloodlightActivitiesListResponse;
    Function List(profileId: string; AQuery : TFloodlightActivitieslistOptions) : TFloodlightActivitiesListResponse;
    Function Patch(profileId: string; aFloodlightActivity : TFloodlightActivity; AQuery : string  = '') : TFloodlightActivity;
    Function Patch(profileId: string; aFloodlightActivity : TFloodlightActivity; AQuery : TFloodlightActivitiespatchOptions) : TFloodlightActivity;
    Function Update(profileId: string; aFloodlightActivity : TFloodlightActivity) : TFloodlightActivity;
  end;
  
  
  { --------------------------------------------------------------------
    TFloodlightActivityGroupsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TFloodlightActivityGroupsResource, method List
  
  TFloodlightActivityGroupsListOptions = Record
    advertiserId : int64;
    floodlightConfigurationId : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
    _type : String;
  end;
  
  
  //Optional query Options for TFloodlightActivityGroupsResource, method Patch
  
  TFloodlightActivityGroupsPatchOptions = Record
    id : int64;
  end;
  
  TFloodlightActivityGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; profileId: string);
    Function Get(id: string; profileId: string) : TFloodlightActivityGroup;
    Function Insert(profileId: string; aFloodlightActivityGroup : TFloodlightActivityGroup) : TFloodlightActivityGroup;
    Function List(profileId: string; AQuery : string  = '') : TFloodlightActivityGroupsListResponse;
    Function List(profileId: string; AQuery : TFloodlightActivityGroupslistOptions) : TFloodlightActivityGroupsListResponse;
    Function Patch(profileId: string; aFloodlightActivityGroup : TFloodlightActivityGroup; AQuery : string  = '') : TFloodlightActivityGroup;
    Function Patch(profileId: string; aFloodlightActivityGroup : TFloodlightActivityGroup; AQuery : TFloodlightActivityGroupspatchOptions) : TFloodlightActivityGroup;
    Function Update(profileId: string; aFloodlightActivityGroup : TFloodlightActivityGroup) : TFloodlightActivityGroup;
  end;
  
  
  { --------------------------------------------------------------------
    TFloodlightConfigurationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TFloodlightConfigurationsResource, method List
  
  TFloodlightConfigurationsListOptions = Record
    ids : int64;
  end;
  
  
  //Optional query Options for TFloodlightConfigurationsResource, method Patch
  
  TFloodlightConfigurationsPatchOptions = Record
    id : int64;
  end;
  
  TFloodlightConfigurationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TFloodlightConfiguration;
    Function List(profileId: string; AQuery : string  = '') : TFloodlightConfigurationsListResponse;
    Function List(profileId: string; AQuery : TFloodlightConfigurationslistOptions) : TFloodlightConfigurationsListResponse;
    Function Patch(profileId: string; aFloodlightConfiguration : TFloodlightConfiguration; AQuery : string  = '') : TFloodlightConfiguration;
    Function Patch(profileId: string; aFloodlightConfiguration : TFloodlightConfiguration; AQuery : TFloodlightConfigurationspatchOptions) : TFloodlightConfiguration;
    Function Update(profileId: string; aFloodlightConfiguration : TFloodlightConfiguration) : TFloodlightConfiguration;
  end;
  
  
  { --------------------------------------------------------------------
    TInventoryItemsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TInventoryItemsResource, method List
  
  TInventoryItemsListOptions = Record
    ids : int64;
    inPlan : boolean;
    maxResults : integer;
    orderId : int64;
    pageToken : String;
    siteId : int64;
    sortField : String;
    sortOrder : String;
  end;
  
  TInventoryItemsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string; projectId: string) : TInventoryItem;
    Function List(profileId: string; projectId: string; AQuery : string  = '') : TInventoryItemsListResponse;
    Function List(profileId: string; projectId: string; AQuery : TInventoryItemslistOptions) : TInventoryItemsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TLandingPagesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLandingPagesResource, method Patch
  
  TLandingPagesPatchOptions = Record
    id : int64;
  end;
  
  TLandingPagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(campaignId: string; id: string; profileId: string);
    Function Get(campaignId: string; id: string; profileId: string) : TLandingPage;
    Function Insert(campaignId: string; profileId: string; aLandingPage : TLandingPage) : TLandingPage;
    Function List(campaignId: string; profileId: string) : TLandingPagesListResponse;
    Function Patch(campaignId: string; profileId: string; aLandingPage : TLandingPage; AQuery : string  = '') : TLandingPage;
    Function Patch(campaignId: string; profileId: string; aLandingPage : TLandingPage; AQuery : TLandingPagespatchOptions) : TLandingPage;
    Function Update(campaignId: string; profileId: string; aLandingPage : TLandingPage) : TLandingPage;
  end;
  
  
  { --------------------------------------------------------------------
    TMetrosResource
    --------------------------------------------------------------------}
  
  TMetrosResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(profileId: string) : TMetrosListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TMobileCarriersResource
    --------------------------------------------------------------------}
  
  TMobileCarriersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TMobileCarrier;
    Function List(profileId: string) : TMobileCarriersListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TOperatingSystemVersionsResource
    --------------------------------------------------------------------}
  
  TOperatingSystemVersionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TOperatingSystemVersion;
    Function List(profileId: string) : TOperatingSystemVersionsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TOperatingSystemsResource
    --------------------------------------------------------------------}
  
  TOperatingSystemsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(dartId: string; profileId: string) : TOperatingSystem;
    Function List(profileId: string) : TOperatingSystemsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TOrderDocumentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOrderDocumentsResource, method List
  
  TOrderDocumentsListOptions = Record
    approved : boolean;
    ids : int64;
    maxResults : integer;
    orderId : int64;
    pageToken : String;
    searchString : String;
    siteId : int64;
    sortField : String;
    sortOrder : String;
  end;
  
  TOrderDocumentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string; projectId: string) : TOrderDocument;
    Function List(profileId: string; projectId: string; AQuery : string  = '') : TOrderDocumentsListResponse;
    Function List(profileId: string; projectId: string; AQuery : TOrderDocumentslistOptions) : TOrderDocumentsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TOrdersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOrdersResource, method List
  
  TOrdersListOptions = Record
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    siteId : int64;
    sortField : String;
    sortOrder : String;
  end;
  
  TOrdersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string; projectId: string) : TOrder;
    Function List(profileId: string; projectId: string; AQuery : string  = '') : TOrdersListResponse;
    Function List(profileId: string; projectId: string; AQuery : TOrderslistOptions) : TOrdersListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TPlacementGroupsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPlacementGroupsResource, method List
  
  TPlacementGroupsListOptions = Record
    advertiserIds : int64;
    archived : boolean;
    campaignIds : int64;
    contentCategoryIds : int64;
    directorySiteIds : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    placementGroupType : String;
    placementStrategyIds : int64;
    pricingTypes : String;
    searchString : String;
    siteIds : int64;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TPlacementGroupsResource, method Patch
  
  TPlacementGroupsPatchOptions = Record
    id : int64;
  end;
  
  TPlacementGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TPlacementGroup;
    Function Insert(profileId: string; aPlacementGroup : TPlacementGroup) : TPlacementGroup;
    Function List(profileId: string; AQuery : string  = '') : TPlacementGroupsListResponse;
    Function List(profileId: string; AQuery : TPlacementGroupslistOptions) : TPlacementGroupsListResponse;
    Function Patch(profileId: string; aPlacementGroup : TPlacementGroup; AQuery : string  = '') : TPlacementGroup;
    Function Patch(profileId: string; aPlacementGroup : TPlacementGroup; AQuery : TPlacementGroupspatchOptions) : TPlacementGroup;
    Function Update(profileId: string; aPlacementGroup : TPlacementGroup) : TPlacementGroup;
  end;
  
  
  { --------------------------------------------------------------------
    TPlacementStrategiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPlacementStrategiesResource, method List
  
  TPlacementStrategiesListOptions = Record
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TPlacementStrategiesResource, method Patch
  
  TPlacementStrategiesPatchOptions = Record
    id : int64;
  end;
  
  TPlacementStrategiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; profileId: string);
    Function Get(id: string; profileId: string) : TPlacementStrategy;
    Function Insert(profileId: string; aPlacementStrategy : TPlacementStrategy) : TPlacementStrategy;
    Function List(profileId: string; AQuery : string  = '') : TPlacementStrategiesListResponse;
    Function List(profileId: string; AQuery : TPlacementStrategieslistOptions) : TPlacementStrategiesListResponse;
    Function Patch(profileId: string; aPlacementStrategy : TPlacementStrategy; AQuery : string  = '') : TPlacementStrategy;
    Function Patch(profileId: string; aPlacementStrategy : TPlacementStrategy; AQuery : TPlacementStrategiespatchOptions) : TPlacementStrategy;
    Function Update(profileId: string; aPlacementStrategy : TPlacementStrategy) : TPlacementStrategy;
  end;
  
  
  { --------------------------------------------------------------------
    TPlacementsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPlacementsResource, method Generatetags
  
  TPlacementsGeneratetagsOptions = Record
    campaignId : int64;
    placementIds : int64;
    tagFormats : String;
  end;
  
  
  //Optional query Options for TPlacementsResource, method List
  
  TPlacementsListOptions = Record
    advertiserIds : int64;
    archived : boolean;
    campaignIds : int64;
    compatibilities : String;
    contentCategoryIds : int64;
    directorySiteIds : int64;
    groupIds : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    paymentSource : String;
    placementStrategyIds : int64;
    pricingTypes : String;
    searchString : String;
    siteIds : int64;
    sizeIds : int64;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TPlacementsResource, method Patch
  
  TPlacementsPatchOptions = Record
    id : int64;
  end;
  
  TPlacementsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generatetags(profileId: string; AQuery : string  = '') : TPlacementsGenerateTagsResponse;
    Function Generatetags(profileId: string; AQuery : TPlacementsgeneratetagsOptions) : TPlacementsGenerateTagsResponse;
    Function Get(id: string; profileId: string) : TPlacement;
    Function Insert(profileId: string; aPlacement : TPlacement) : TPlacement;
    Function List(profileId: string; AQuery : string  = '') : TPlacementsListResponse;
    Function List(profileId: string; AQuery : TPlacementslistOptions) : TPlacementsListResponse;
    Function Patch(profileId: string; aPlacement : TPlacement; AQuery : string  = '') : TPlacement;
    Function Patch(profileId: string; aPlacement : TPlacement; AQuery : TPlacementspatchOptions) : TPlacement;
    Function Update(profileId: string; aPlacement : TPlacement) : TPlacement;
  end;
  
  
  { --------------------------------------------------------------------
    TPlatformTypesResource
    --------------------------------------------------------------------}
  
  TPlatformTypesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TPlatformType;
    Function List(profileId: string) : TPlatformTypesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TPostalCodesResource
    --------------------------------------------------------------------}
  
  TPostalCodesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(code: string; profileId: string) : TPostalCode;
    Function List(profileId: string) : TPostalCodesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsResource, method List
  
  TProjectsListOptions = Record
    advertiserIds : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TProject;
    Function List(profileId: string; AQuery : string  = '') : TProjectsListResponse;
    Function List(profileId: string; AQuery : TProjectslistOptions) : TProjectsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TRegionsResource
    --------------------------------------------------------------------}
  
  TRegionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(profileId: string) : TRegionsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TRemarketingListSharesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRemarketingListSharesResource, method Patch
  
  TRemarketingListSharesPatchOptions = Record
    remarketingListId : int64;
  end;
  
  TRemarketingListSharesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(profileId: string; remarketingListId: string) : TRemarketingListShare;
    Function Patch(profileId: string; aRemarketingListShare : TRemarketingListShare; AQuery : string  = '') : TRemarketingListShare;
    Function Patch(profileId: string; aRemarketingListShare : TRemarketingListShare; AQuery : TRemarketingListSharespatchOptions) : TRemarketingListShare;
    Function Update(profileId: string; aRemarketingListShare : TRemarketingListShare) : TRemarketingListShare;
  end;
  
  
  { --------------------------------------------------------------------
    TRemarketingListsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRemarketingListsResource, method List
  
  TRemarketingListsListOptions = Record
    active : boolean;
    advertiserId : int64;
    floodlightActivityId : int64;
    maxResults : integer;
    _name : String;
    pageToken : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TRemarketingListsResource, method Patch
  
  TRemarketingListsPatchOptions = Record
    id : int64;
  end;
  
  TRemarketingListsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TRemarketingList;
    Function Insert(profileId: string; aRemarketingList : TRemarketingList) : TRemarketingList;
    Function List(profileId: string; AQuery : string  = '') : TRemarketingListsListResponse;
    Function List(profileId: string; AQuery : TRemarketingListslistOptions) : TRemarketingListsListResponse;
    Function Patch(profileId: string; aRemarketingList : TRemarketingList; AQuery : string  = '') : TRemarketingList;
    Function Patch(profileId: string; aRemarketingList : TRemarketingList; AQuery : TRemarketingListspatchOptions) : TRemarketingList;
    Function Update(profileId: string; aRemarketingList : TRemarketingList) : TRemarketingList;
  end;
  
  
  { --------------------------------------------------------------------
    TReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReportsResource, method List
  
  TReportsListOptions = Record
    maxResults : integer;
    pageToken : String;
    scope : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TReportsResource, method Run
  
  TReportsRunOptions = Record
    synchronous : boolean;
  end;
  
  TReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(profileId: string; reportId: string);
    Function Get(profileId: string; reportId: string) : TReport;
    Function Insert(profileId: string; aReport : TReport) : TReport;
    Function List(profileId: string; AQuery : string  = '') : TReportList;
    Function List(profileId: string; AQuery : TReportslistOptions) : TReportList;
    Function Patch(profileId: string; reportId: string; aReport : TReport) : TReport;
    Function Run(profileId: string; reportId: string; AQuery : string  = '') : TFile;
    Function Run(profileId: string; reportId: string; AQuery : TReportsrunOptions) : TFile;
    Function Update(profileId: string; reportId: string; aReport : TReport) : TReport;
  end;
  
  
  { --------------------------------------------------------------------
    TSitesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSitesResource, method List
  
  TSitesListOptions = Record
    acceptsInStreamVideoPlacements : boolean;
    acceptsInterstitialPlacements : boolean;
    acceptsPublisherPaidPlacements : boolean;
    adWordsSite : boolean;
    approved : boolean;
    campaignIds : int64;
    directorySiteIds : int64;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
    subaccountId : int64;
    unmappedSite : boolean;
  end;
  
  
  //Optional query Options for TSitesResource, method Patch
  
  TSitesPatchOptions = Record
    id : int64;
  end;
  
  TSitesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TSite;
    Function Insert(profileId: string; aSite : TSite) : TSite;
    Function List(profileId: string; AQuery : string  = '') : TSitesListResponse;
    Function List(profileId: string; AQuery : TSiteslistOptions) : TSitesListResponse;
    Function Patch(profileId: string; aSite : TSite; AQuery : string  = '') : TSite;
    Function Patch(profileId: string; aSite : TSite; AQuery : TSitespatchOptions) : TSite;
    Function Update(profileId: string; aSite : TSite) : TSite;
  end;
  
  
  { --------------------------------------------------------------------
    TSizesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSizesResource, method List
  
  TSizesListOptions = Record
    height : integer;
    iabStandard : boolean;
    ids : int64;
    width : integer;
  end;
  
  TSizesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TSize;
    Function Insert(profileId: string; aSize : TSize) : TSize;
    Function List(profileId: string; AQuery : string  = '') : TSizesListResponse;
    Function List(profileId: string; AQuery : TSizeslistOptions) : TSizesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSubaccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSubaccountsResource, method List
  
  TSubaccountsListOptions = Record
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
  end;
  
  
  //Optional query Options for TSubaccountsResource, method Patch
  
  TSubaccountsPatchOptions = Record
    id : int64;
  end;
  
  TSubaccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TSubaccount;
    Function Insert(profileId: string; aSubaccount : TSubaccount) : TSubaccount;
    Function List(profileId: string; AQuery : string  = '') : TSubaccountsListResponse;
    Function List(profileId: string; AQuery : TSubaccountslistOptions) : TSubaccountsListResponse;
    Function Patch(profileId: string; aSubaccount : TSubaccount; AQuery : string  = '') : TSubaccount;
    Function Patch(profileId: string; aSubaccount : TSubaccount; AQuery : TSubaccountspatchOptions) : TSubaccount;
    Function Update(profileId: string; aSubaccount : TSubaccount) : TSubaccount;
  end;
  
  
  { --------------------------------------------------------------------
    TTargetableRemarketingListsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTargetableRemarketingListsResource, method List
  
  TTargetableRemarketingListsListOptions = Record
    active : boolean;
    advertiserId : int64;
    maxResults : integer;
    _name : String;
    pageToken : String;
    sortField : String;
    sortOrder : String;
  end;
  
  TTargetableRemarketingListsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TTargetableRemarketingList;
    Function List(profileId: string; AQuery : string  = '') : TTargetableRemarketingListsListResponse;
    Function List(profileId: string; AQuery : TTargetableRemarketingListslistOptions) : TTargetableRemarketingListsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUserProfilesResource
    --------------------------------------------------------------------}
  
  TUserProfilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(profileId: string) : TUserProfile;
    Function List : TUserProfileList;
  end;
  
  
  { --------------------------------------------------------------------
    TUserRolePermissionGroupsResource
    --------------------------------------------------------------------}
  
  TUserRolePermissionGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TUserRolePermissionGroup;
    Function List(profileId: string) : TUserRolePermissionGroupsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUserRolePermissionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUserRolePermissionsResource, method List
  
  TUserRolePermissionsListOptions = Record
    ids : int64;
  end;
  
  TUserRolePermissionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; profileId: string) : TUserRolePermission;
    Function List(profileId: string; AQuery : string  = '') : TUserRolePermissionsListResponse;
    Function List(profileId: string; AQuery : TUserRolePermissionslistOptions) : TUserRolePermissionsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUserRolesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUserRolesResource, method List
  
  TUserRolesListOptions = Record
    accountUserRoleOnly : boolean;
    ids : int64;
    maxResults : integer;
    pageToken : String;
    searchString : String;
    sortField : String;
    sortOrder : String;
    subaccountId : int64;
  end;
  
  
  //Optional query Options for TUserRolesResource, method Patch
  
  TUserRolesPatchOptions = Record
    id : int64;
  end;
  
  TUserRolesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; profileId: string);
    Function Get(id: string; profileId: string) : TUserRole;
    Function Insert(profileId: string; aUserRole : TUserRole) : TUserRole;
    Function List(profileId: string; AQuery : string  = '') : TUserRolesListResponse;
    Function List(profileId: string; AQuery : TUserRoleslistOptions) : TUserRolesListResponse;
    Function Patch(profileId: string; aUserRole : TUserRole; AQuery : string  = '') : TUserRole;
    Function Patch(profileId: string; aUserRole : TUserRole; AQuery : TUserRolespatchOptions) : TUserRole;
    Function Update(profileId: string; aUserRole : TUserRole) : TUserRole;
  end;
  
  
  { --------------------------------------------------------------------
    TDfareportingAPI
    --------------------------------------------------------------------}
  
  TDfareportingAPI = Class(TGoogleAPI)
  Private
    FAccountActiveAdSummariesInstance : TAccountActiveAdSummariesResource;
    FAccountPermissionGroupsInstance : TAccountPermissionGroupsResource;
    FAccountPermissionsInstance : TAccountPermissionsResource;
    FAccountUserProfilesInstance : TAccountUserProfilesResource;
    FAccountsInstance : TAccountsResource;
    FAdsInstance : TAdsResource;
    FAdvertiserGroupsInstance : TAdvertiserGroupsResource;
    FAdvertisersInstance : TAdvertisersResource;
    FBrowsersInstance : TBrowsersResource;
    FCampaignCreativeAssociationsInstance : TCampaignCreativeAssociationsResource;
    FCampaignsInstance : TCampaignsResource;
    FChangeLogsInstance : TChangeLogsResource;
    FCitiesInstance : TCitiesResource;
    FConnectionTypesInstance : TConnectionTypesResource;
    FContentCategoriesInstance : TContentCategoriesResource;
    FCountriesInstance : TCountriesResource;
    FCreativeAssetsInstance : TCreativeAssetsResource;
    FCreativeFieldValuesInstance : TCreativeFieldValuesResource;
    FCreativeFieldsInstance : TCreativeFieldsResource;
    FCreativeGroupsInstance : TCreativeGroupsResource;
    FCreativesInstance : TCreativesResource;
    FDimensionValuesInstance : TDimensionValuesResource;
    FDirectorySiteContactsInstance : TDirectorySiteContactsResource;
    FDirectorySitesInstance : TDirectorySitesResource;
    FEventTagsInstance : TEventTagsResource;
    FFilesInstance : TFilesResource;
    FFloodlightActivitiesInstance : TFloodlightActivitiesResource;
    FFloodlightActivityGroupsInstance : TFloodlightActivityGroupsResource;
    FFloodlightConfigurationsInstance : TFloodlightConfigurationsResource;
    FInventoryItemsInstance : TInventoryItemsResource;
    FLandingPagesInstance : TLandingPagesResource;
    FMetrosInstance : TMetrosResource;
    FMobileCarriersInstance : TMobileCarriersResource;
    FOperatingSystemVersionsInstance : TOperatingSystemVersionsResource;
    FOperatingSystemsInstance : TOperatingSystemsResource;
    FOrderDocumentsInstance : TOrderDocumentsResource;
    FOrdersInstance : TOrdersResource;
    FPlacementGroupsInstance : TPlacementGroupsResource;
    FPlacementStrategiesInstance : TPlacementStrategiesResource;
    FPlacementsInstance : TPlacementsResource;
    FPlatformTypesInstance : TPlatformTypesResource;
    FPostalCodesInstance : TPostalCodesResource;
    FProjectsInstance : TProjectsResource;
    FRegionsInstance : TRegionsResource;
    FRemarketingListSharesInstance : TRemarketingListSharesResource;
    FRemarketingListsInstance : TRemarketingListsResource;
    FReportsInstance : TReportsResource;
    FSitesInstance : TSitesResource;
    FSizesInstance : TSizesResource;
    FSubaccountsInstance : TSubaccountsResource;
    FTargetableRemarketingListsInstance : TTargetableRemarketingListsResource;
    FUserProfilesInstance : TUserProfilesResource;
    FUserRolePermissionGroupsInstance : TUserRolePermissionGroupsResource;
    FUserRolePermissionsInstance : TUserRolePermissionsResource;
    FUserRolesInstance : TUserRolesResource;
    Function GetAccountActiveAdSummariesInstance : TAccountActiveAdSummariesResource;virtual;
    Function GetAccountPermissionGroupsInstance : TAccountPermissionGroupsResource;virtual;
    Function GetAccountPermissionsInstance : TAccountPermissionsResource;virtual;
    Function GetAccountUserProfilesInstance : TAccountUserProfilesResource;virtual;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetAdsInstance : TAdsResource;virtual;
    Function GetAdvertiserGroupsInstance : TAdvertiserGroupsResource;virtual;
    Function GetAdvertisersInstance : TAdvertisersResource;virtual;
    Function GetBrowsersInstance : TBrowsersResource;virtual;
    Function GetCampaignCreativeAssociationsInstance : TCampaignCreativeAssociationsResource;virtual;
    Function GetCampaignsInstance : TCampaignsResource;virtual;
    Function GetChangeLogsInstance : TChangeLogsResource;virtual;
    Function GetCitiesInstance : TCitiesResource;virtual;
    Function GetConnectionTypesInstance : TConnectionTypesResource;virtual;
    Function GetContentCategoriesInstance : TContentCategoriesResource;virtual;
    Function GetCountriesInstance : TCountriesResource;virtual;
    Function GetCreativeAssetsInstance : TCreativeAssetsResource;virtual;
    Function GetCreativeFieldValuesInstance : TCreativeFieldValuesResource;virtual;
    Function GetCreativeFieldsInstance : TCreativeFieldsResource;virtual;
    Function GetCreativeGroupsInstance : TCreativeGroupsResource;virtual;
    Function GetCreativesInstance : TCreativesResource;virtual;
    Function GetDimensionValuesInstance : TDimensionValuesResource;virtual;
    Function GetDirectorySiteContactsInstance : TDirectorySiteContactsResource;virtual;
    Function GetDirectorySitesInstance : TDirectorySitesResource;virtual;
    Function GetEventTagsInstance : TEventTagsResource;virtual;
    Function GetFilesInstance : TFilesResource;virtual;
    Function GetFloodlightActivitiesInstance : TFloodlightActivitiesResource;virtual;
    Function GetFloodlightActivityGroupsInstance : TFloodlightActivityGroupsResource;virtual;
    Function GetFloodlightConfigurationsInstance : TFloodlightConfigurationsResource;virtual;
    Function GetInventoryItemsInstance : TInventoryItemsResource;virtual;
    Function GetLandingPagesInstance : TLandingPagesResource;virtual;
    Function GetMetrosInstance : TMetrosResource;virtual;
    Function GetMobileCarriersInstance : TMobileCarriersResource;virtual;
    Function GetOperatingSystemVersionsInstance : TOperatingSystemVersionsResource;virtual;
    Function GetOperatingSystemsInstance : TOperatingSystemsResource;virtual;
    Function GetOrderDocumentsInstance : TOrderDocumentsResource;virtual;
    Function GetOrdersInstance : TOrdersResource;virtual;
    Function GetPlacementGroupsInstance : TPlacementGroupsResource;virtual;
    Function GetPlacementStrategiesInstance : TPlacementStrategiesResource;virtual;
    Function GetPlacementsInstance : TPlacementsResource;virtual;
    Function GetPlatformTypesInstance : TPlatformTypesResource;virtual;
    Function GetPostalCodesInstance : TPostalCodesResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
    Function GetRegionsInstance : TRegionsResource;virtual;
    Function GetRemarketingListSharesInstance : TRemarketingListSharesResource;virtual;
    Function GetRemarketingListsInstance : TRemarketingListsResource;virtual;
    Function GetReportsInstance : TReportsResource;virtual;
    Function GetSitesInstance : TSitesResource;virtual;
    Function GetSizesInstance : TSizesResource;virtual;
    Function GetSubaccountsInstance : TSubaccountsResource;virtual;
    Function GetTargetableRemarketingListsInstance : TTargetableRemarketingListsResource;virtual;
    Function GetUserProfilesInstance : TUserProfilesResource;virtual;
    Function GetUserRolePermissionGroupsInstance : TUserRolePermissionGroupsResource;virtual;
    Function GetUserRolePermissionsInstance : TUserRolePermissionsResource;virtual;
    Function GetUserRolesInstance : TUserRolesResource;virtual;
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
    Function CreateAccountActiveAdSummariesResource(AOwner : TComponent) : TAccountActiveAdSummariesResource;virtual;overload;
    Function CreateAccountActiveAdSummariesResource : TAccountActiveAdSummariesResource;virtual;overload;
    Function CreateAccountPermissionGroupsResource(AOwner : TComponent) : TAccountPermissionGroupsResource;virtual;overload;
    Function CreateAccountPermissionGroupsResource : TAccountPermissionGroupsResource;virtual;overload;
    Function CreateAccountPermissionsResource(AOwner : TComponent) : TAccountPermissionsResource;virtual;overload;
    Function CreateAccountPermissionsResource : TAccountPermissionsResource;virtual;overload;
    Function CreateAccountUserProfilesResource(AOwner : TComponent) : TAccountUserProfilesResource;virtual;overload;
    Function CreateAccountUserProfilesResource : TAccountUserProfilesResource;virtual;overload;
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    Function CreateAdsResource(AOwner : TComponent) : TAdsResource;virtual;overload;
    Function CreateAdsResource : TAdsResource;virtual;overload;
    Function CreateAdvertiserGroupsResource(AOwner : TComponent) : TAdvertiserGroupsResource;virtual;overload;
    Function CreateAdvertiserGroupsResource : TAdvertiserGroupsResource;virtual;overload;
    Function CreateAdvertisersResource(AOwner : TComponent) : TAdvertisersResource;virtual;overload;
    Function CreateAdvertisersResource : TAdvertisersResource;virtual;overload;
    Function CreateBrowsersResource(AOwner : TComponent) : TBrowsersResource;virtual;overload;
    Function CreateBrowsersResource : TBrowsersResource;virtual;overload;
    Function CreateCampaignCreativeAssociationsResource(AOwner : TComponent) : TCampaignCreativeAssociationsResource;virtual;overload;
    Function CreateCampaignCreativeAssociationsResource : TCampaignCreativeAssociationsResource;virtual;overload;
    Function CreateCampaignsResource(AOwner : TComponent) : TCampaignsResource;virtual;overload;
    Function CreateCampaignsResource : TCampaignsResource;virtual;overload;
    Function CreateChangeLogsResource(AOwner : TComponent) : TChangeLogsResource;virtual;overload;
    Function CreateChangeLogsResource : TChangeLogsResource;virtual;overload;
    Function CreateCitiesResource(AOwner : TComponent) : TCitiesResource;virtual;overload;
    Function CreateCitiesResource : TCitiesResource;virtual;overload;
    Function CreateConnectionTypesResource(AOwner : TComponent) : TConnectionTypesResource;virtual;overload;
    Function CreateConnectionTypesResource : TConnectionTypesResource;virtual;overload;
    Function CreateContentCategoriesResource(AOwner : TComponent) : TContentCategoriesResource;virtual;overload;
    Function CreateContentCategoriesResource : TContentCategoriesResource;virtual;overload;
    Function CreateCountriesResource(AOwner : TComponent) : TCountriesResource;virtual;overload;
    Function CreateCountriesResource : TCountriesResource;virtual;overload;
    Function CreateCreativeAssetsResource(AOwner : TComponent) : TCreativeAssetsResource;virtual;overload;
    Function CreateCreativeAssetsResource : TCreativeAssetsResource;virtual;overload;
    Function CreateCreativeFieldValuesResource(AOwner : TComponent) : TCreativeFieldValuesResource;virtual;overload;
    Function CreateCreativeFieldValuesResource : TCreativeFieldValuesResource;virtual;overload;
    Function CreateCreativeFieldsResource(AOwner : TComponent) : TCreativeFieldsResource;virtual;overload;
    Function CreateCreativeFieldsResource : TCreativeFieldsResource;virtual;overload;
    Function CreateCreativeGroupsResource(AOwner : TComponent) : TCreativeGroupsResource;virtual;overload;
    Function CreateCreativeGroupsResource : TCreativeGroupsResource;virtual;overload;
    Function CreateCreativesResource(AOwner : TComponent) : TCreativesResource;virtual;overload;
    Function CreateCreativesResource : TCreativesResource;virtual;overload;
    Function CreateDimensionValuesResource(AOwner : TComponent) : TDimensionValuesResource;virtual;overload;
    Function CreateDimensionValuesResource : TDimensionValuesResource;virtual;overload;
    Function CreateDirectorySiteContactsResource(AOwner : TComponent) : TDirectorySiteContactsResource;virtual;overload;
    Function CreateDirectorySiteContactsResource : TDirectorySiteContactsResource;virtual;overload;
    Function CreateDirectorySitesResource(AOwner : TComponent) : TDirectorySitesResource;virtual;overload;
    Function CreateDirectorySitesResource : TDirectorySitesResource;virtual;overload;
    Function CreateEventTagsResource(AOwner : TComponent) : TEventTagsResource;virtual;overload;
    Function CreateEventTagsResource : TEventTagsResource;virtual;overload;
    Function CreateFilesResource(AOwner : TComponent) : TFilesResource;virtual;overload;
    Function CreateFilesResource : TFilesResource;virtual;overload;
    Function CreateFloodlightActivitiesResource(AOwner : TComponent) : TFloodlightActivitiesResource;virtual;overload;
    Function CreateFloodlightActivitiesResource : TFloodlightActivitiesResource;virtual;overload;
    Function CreateFloodlightActivityGroupsResource(AOwner : TComponent) : TFloodlightActivityGroupsResource;virtual;overload;
    Function CreateFloodlightActivityGroupsResource : TFloodlightActivityGroupsResource;virtual;overload;
    Function CreateFloodlightConfigurationsResource(AOwner : TComponent) : TFloodlightConfigurationsResource;virtual;overload;
    Function CreateFloodlightConfigurationsResource : TFloodlightConfigurationsResource;virtual;overload;
    Function CreateInventoryItemsResource(AOwner : TComponent) : TInventoryItemsResource;virtual;overload;
    Function CreateInventoryItemsResource : TInventoryItemsResource;virtual;overload;
    Function CreateLandingPagesResource(AOwner : TComponent) : TLandingPagesResource;virtual;overload;
    Function CreateLandingPagesResource : TLandingPagesResource;virtual;overload;
    Function CreateMetrosResource(AOwner : TComponent) : TMetrosResource;virtual;overload;
    Function CreateMetrosResource : TMetrosResource;virtual;overload;
    Function CreateMobileCarriersResource(AOwner : TComponent) : TMobileCarriersResource;virtual;overload;
    Function CreateMobileCarriersResource : TMobileCarriersResource;virtual;overload;
    Function CreateOperatingSystemVersionsResource(AOwner : TComponent) : TOperatingSystemVersionsResource;virtual;overload;
    Function CreateOperatingSystemVersionsResource : TOperatingSystemVersionsResource;virtual;overload;
    Function CreateOperatingSystemsResource(AOwner : TComponent) : TOperatingSystemsResource;virtual;overload;
    Function CreateOperatingSystemsResource : TOperatingSystemsResource;virtual;overload;
    Function CreateOrderDocumentsResource(AOwner : TComponent) : TOrderDocumentsResource;virtual;overload;
    Function CreateOrderDocumentsResource : TOrderDocumentsResource;virtual;overload;
    Function CreateOrdersResource(AOwner : TComponent) : TOrdersResource;virtual;overload;
    Function CreateOrdersResource : TOrdersResource;virtual;overload;
    Function CreatePlacementGroupsResource(AOwner : TComponent) : TPlacementGroupsResource;virtual;overload;
    Function CreatePlacementGroupsResource : TPlacementGroupsResource;virtual;overload;
    Function CreatePlacementStrategiesResource(AOwner : TComponent) : TPlacementStrategiesResource;virtual;overload;
    Function CreatePlacementStrategiesResource : TPlacementStrategiesResource;virtual;overload;
    Function CreatePlacementsResource(AOwner : TComponent) : TPlacementsResource;virtual;overload;
    Function CreatePlacementsResource : TPlacementsResource;virtual;overload;
    Function CreatePlatformTypesResource(AOwner : TComponent) : TPlatformTypesResource;virtual;overload;
    Function CreatePlatformTypesResource : TPlatformTypesResource;virtual;overload;
    Function CreatePostalCodesResource(AOwner : TComponent) : TPostalCodesResource;virtual;overload;
    Function CreatePostalCodesResource : TPostalCodesResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    Function CreateRegionsResource(AOwner : TComponent) : TRegionsResource;virtual;overload;
    Function CreateRegionsResource : TRegionsResource;virtual;overload;
    Function CreateRemarketingListSharesResource(AOwner : TComponent) : TRemarketingListSharesResource;virtual;overload;
    Function CreateRemarketingListSharesResource : TRemarketingListSharesResource;virtual;overload;
    Function CreateRemarketingListsResource(AOwner : TComponent) : TRemarketingListsResource;virtual;overload;
    Function CreateRemarketingListsResource : TRemarketingListsResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    Function CreateSitesResource(AOwner : TComponent) : TSitesResource;virtual;overload;
    Function CreateSitesResource : TSitesResource;virtual;overload;
    Function CreateSizesResource(AOwner : TComponent) : TSizesResource;virtual;overload;
    Function CreateSizesResource : TSizesResource;virtual;overload;
    Function CreateSubaccountsResource(AOwner : TComponent) : TSubaccountsResource;virtual;overload;
    Function CreateSubaccountsResource : TSubaccountsResource;virtual;overload;
    Function CreateTargetableRemarketingListsResource(AOwner : TComponent) : TTargetableRemarketingListsResource;virtual;overload;
    Function CreateTargetableRemarketingListsResource : TTargetableRemarketingListsResource;virtual;overload;
    Function CreateUserProfilesResource(AOwner : TComponent) : TUserProfilesResource;virtual;overload;
    Function CreateUserProfilesResource : TUserProfilesResource;virtual;overload;
    Function CreateUserRolePermissionGroupsResource(AOwner : TComponent) : TUserRolePermissionGroupsResource;virtual;overload;
    Function CreateUserRolePermissionGroupsResource : TUserRolePermissionGroupsResource;virtual;overload;
    Function CreateUserRolePermissionsResource(AOwner : TComponent) : TUserRolePermissionsResource;virtual;overload;
    Function CreateUserRolePermissionsResource : TUserRolePermissionsResource;virtual;overload;
    Function CreateUserRolesResource(AOwner : TComponent) : TUserRolesResource;virtual;overload;
    Function CreateUserRolesResource : TUserRolesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountActiveAdSummariesResource : TAccountActiveAdSummariesResource Read GetAccountActiveAdSummariesInstance;
    Property AccountPermissionGroupsResource : TAccountPermissionGroupsResource Read GetAccountPermissionGroupsInstance;
    Property AccountPermissionsResource : TAccountPermissionsResource Read GetAccountPermissionsInstance;
    Property AccountUserProfilesResource : TAccountUserProfilesResource Read GetAccountUserProfilesInstance;
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property AdsResource : TAdsResource Read GetAdsInstance;
    Property AdvertiserGroupsResource : TAdvertiserGroupsResource Read GetAdvertiserGroupsInstance;
    Property AdvertisersResource : TAdvertisersResource Read GetAdvertisersInstance;
    Property BrowsersResource : TBrowsersResource Read GetBrowsersInstance;
    Property CampaignCreativeAssociationsResource : TCampaignCreativeAssociationsResource Read GetCampaignCreativeAssociationsInstance;
    Property CampaignsResource : TCampaignsResource Read GetCampaignsInstance;
    Property ChangeLogsResource : TChangeLogsResource Read GetChangeLogsInstance;
    Property CitiesResource : TCitiesResource Read GetCitiesInstance;
    Property ConnectionTypesResource : TConnectionTypesResource Read GetConnectionTypesInstance;
    Property ContentCategoriesResource : TContentCategoriesResource Read GetContentCategoriesInstance;
    Property CountriesResource : TCountriesResource Read GetCountriesInstance;
    Property CreativeAssetsResource : TCreativeAssetsResource Read GetCreativeAssetsInstance;
    Property CreativeFieldValuesResource : TCreativeFieldValuesResource Read GetCreativeFieldValuesInstance;
    Property CreativeFieldsResource : TCreativeFieldsResource Read GetCreativeFieldsInstance;
    Property CreativeGroupsResource : TCreativeGroupsResource Read GetCreativeGroupsInstance;
    Property CreativesResource : TCreativesResource Read GetCreativesInstance;
    Property DimensionValuesResource : TDimensionValuesResource Read GetDimensionValuesInstance;
    Property DirectorySiteContactsResource : TDirectorySiteContactsResource Read GetDirectorySiteContactsInstance;
    Property DirectorySitesResource : TDirectorySitesResource Read GetDirectorySitesInstance;
    Property EventTagsResource : TEventTagsResource Read GetEventTagsInstance;
    Property FilesResource : TFilesResource Read GetFilesInstance;
    Property FloodlightActivitiesResource : TFloodlightActivitiesResource Read GetFloodlightActivitiesInstance;
    Property FloodlightActivityGroupsResource : TFloodlightActivityGroupsResource Read GetFloodlightActivityGroupsInstance;
    Property FloodlightConfigurationsResource : TFloodlightConfigurationsResource Read GetFloodlightConfigurationsInstance;
    Property InventoryItemsResource : TInventoryItemsResource Read GetInventoryItemsInstance;
    Property LandingPagesResource : TLandingPagesResource Read GetLandingPagesInstance;
    Property MetrosResource : TMetrosResource Read GetMetrosInstance;
    Property MobileCarriersResource : TMobileCarriersResource Read GetMobileCarriersInstance;
    Property OperatingSystemVersionsResource : TOperatingSystemVersionsResource Read GetOperatingSystemVersionsInstance;
    Property OperatingSystemsResource : TOperatingSystemsResource Read GetOperatingSystemsInstance;
    Property OrderDocumentsResource : TOrderDocumentsResource Read GetOrderDocumentsInstance;
    Property OrdersResource : TOrdersResource Read GetOrdersInstance;
    Property PlacementGroupsResource : TPlacementGroupsResource Read GetPlacementGroupsInstance;
    Property PlacementStrategiesResource : TPlacementStrategiesResource Read GetPlacementStrategiesInstance;
    Property PlacementsResource : TPlacementsResource Read GetPlacementsInstance;
    Property PlatformTypesResource : TPlatformTypesResource Read GetPlatformTypesInstance;
    Property PostalCodesResource : TPostalCodesResource Read GetPostalCodesInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
    Property RegionsResource : TRegionsResource Read GetRegionsInstance;
    Property RemarketingListSharesResource : TRemarketingListSharesResource Read GetRemarketingListSharesInstance;
    Property RemarketingListsResource : TRemarketingListsResource Read GetRemarketingListsInstance;
    Property ReportsResource : TReportsResource Read GetReportsInstance;
    Property SitesResource : TSitesResource Read GetSitesInstance;
    Property SizesResource : TSizesResource Read GetSizesInstance;
    Property SubaccountsResource : TSubaccountsResource Read GetSubaccountsInstance;
    Property TargetableRemarketingListsResource : TTargetableRemarketingListsResource Read GetTargetableRemarketingListsInstance;
    Property UserProfilesResource : TUserProfilesResource Read GetUserProfilesInstance;
    Property UserRolePermissionGroupsResource : TUserRolePermissionGroupsResource Read GetUserRolePermissionGroupsInstance;
    Property UserRolePermissionsResource : TUserRolePermissionsResource Read GetUserRolePermissionsInstance;
    Property UserRolesResource : TUserRolesResource Read GetUserRolesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetaccountPermissionIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FaccountPermissionIds=AValue) then exit;
  FaccountPermissionIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetaccountProfile(AIndex : Integer; AValue : String); 

begin
  If (FaccountProfile=AValue) then exit;
  FaccountProfile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetactiveAdsLimitTier(AIndex : Integer; AValue : String); 

begin
  If (FactiveAdsLimitTier=AValue) then exit;
  FactiveAdsLimitTier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetactiveViewOptOut(AIndex : Integer; AValue : boolean); 

begin
  If (FactiveViewOptOut=AValue) then exit;
  FactiveViewOptOut:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetavailablePermissionIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FavailablePermissionIds=AValue) then exit;
  FavailablePermissionIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcomscoreVceEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FcomscoreVceEnabled=AValue) then exit;
  FcomscoreVceEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcountryId(AIndex : Integer; AValue : String); 

begin
  If (FcountryId=AValue) then exit;
  FcountryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcurrencyId(AIndex : Integer; AValue : String); 

begin
  If (FcurrencyId=AValue) then exit;
  FcurrencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetdefaultCreativeSizeId(AIndex : Integer; AValue : String); 

begin
  If (FdefaultCreativeSizeId=AValue) then exit;
  FdefaultCreativeSizeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setlocale(AIndex : Integer; AValue : String); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetmaximumImageSize(AIndex : Integer; AValue : String); 

begin
  If (FmaximumImageSize=AValue) then exit;
  FmaximumImageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetnielsenOcrEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FnielsenOcrEnabled=AValue) then exit;
  FnielsenOcrEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetreportsConfiguration(AIndex : Integer; AValue : TReportsConfiguration); 

begin
  If (FreportsConfiguration=AValue) then exit;
  FreportsConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetteaserSizeLimit(AIndex : Integer; AValue : String); 

begin
  If (FteaserSizeLimit=AValue) then exit;
  FteaserSizeLimit:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountActiveAdSummary
  --------------------------------------------------------------------}


Procedure TAccountActiveAdSummary.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountActiveAdSummary.SetactiveAds(AIndex : Integer; AValue : String); 

begin
  If (FactiveAds=AValue) then exit;
  FactiveAds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountActiveAdSummary.SetactiveAdsLimitTier(AIndex : Integer; AValue : String); 

begin
  If (FactiveAdsLimitTier=AValue) then exit;
  FactiveAdsLimitTier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountActiveAdSummary.SetavailableAds(AIndex : Integer; AValue : String); 

begin
  If (FavailableAds=AValue) then exit;
  FavailableAds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountActiveAdSummary.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountPermission
  --------------------------------------------------------------------}


Procedure TAccountPermission.SetaccountProfiles(AIndex : Integer; AValue : TStringArray); 

begin
  If (FaccountProfiles=AValue) then exit;
  FaccountProfiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermission.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermission.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermission.Setlevel(AIndex : Integer; AValue : String); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermission.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermission.SetpermissionGroupId(AIndex : Integer; AValue : String); 

begin
  If (FpermissionGroupId=AValue) then exit;
  FpermissionGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountPermissionGroup
  --------------------------------------------------------------------}


Procedure TAccountPermissionGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermissionGroup.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermissionGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountPermissionGroupsListResponse
  --------------------------------------------------------------------}


Procedure TAccountPermissionGroupsListResponse.SetaccountPermissionGroups(AIndex : Integer; AValue : TAccountPermissionGroupsListResponseTypeaccountPermissionGroupsArray); 

begin
  If (FaccountPermissionGroups=AValue) then exit;
  FaccountPermissionGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermissionGroupsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountPermissionsListResponse
  --------------------------------------------------------------------}


Procedure TAccountPermissionsListResponse.SetaccountPermissions(AIndex : Integer; AValue : TAccountPermissionsListResponseTypeaccountPermissionsArray); 

begin
  If (FaccountPermissions=AValue) then exit;
  FaccountPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountPermissionsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountUserProfile
  --------------------------------------------------------------------}


Procedure TAccountUserProfile.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.SetadvertiserFilter(AIndex : Integer; AValue : TObjectFilter); 

begin
  If (FadvertiserFilter=AValue) then exit;
  FadvertiserFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.SetcampaignFilter(AIndex : Integer; AValue : TObjectFilter); 

begin
  If (FcampaignFilter=AValue) then exit;
  FcampaignFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.Setcomments(AIndex : Integer; AValue : String); 

begin
  If (Fcomments=AValue) then exit;
  Fcomments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.Setlocale(AIndex : Integer; AValue : String); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.SetsiteFilter(AIndex : Integer; AValue : TObjectFilter); 

begin
  If (FsiteFilter=AValue) then exit;
  FsiteFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.SettraffickerType(AIndex : Integer; AValue : String); 

begin
  If (FtraffickerType=AValue) then exit;
  FtraffickerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.SetuserAccessType(AIndex : Integer; AValue : String); 

begin
  If (FuserAccessType=AValue) then exit;
  FuserAccessType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.SetuserRoleFilter(AIndex : Integer; AValue : TObjectFilter); 

begin
  If (FuserRoleFilter=AValue) then exit;
  FuserRoleFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfile.SetuserRoleId(AIndex : Integer; AValue : String); 

begin
  If (FuserRoleId=AValue) then exit;
  FuserRoleId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountUserProfilesListResponse
  --------------------------------------------------------------------}


Procedure TAccountUserProfilesListResponse.SetaccountUserProfiles(AIndex : Integer; AValue : TAccountUserProfilesListResponseTypeaccountUserProfilesArray); 

begin
  If (FaccountUserProfiles=AValue) then exit;
  FaccountUserProfiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfilesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUserProfilesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsListResponse
  --------------------------------------------------------------------}


Procedure TAccountsListResponse.Setaccounts(AIndex : Integer; AValue : TAccountsListResponseTypeaccountsArray); 

begin
  If (Faccounts=AValue) then exit;
  Faccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivities
  --------------------------------------------------------------------}


Procedure TActivities.Setfilters(AIndex : Integer; AValue : TActivitiesTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.SetmetricNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmetricNames=AValue) then exit;
  FmetricNames:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAd
  --------------------------------------------------------------------}


Procedure TAd.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setarchived(AIndex : Integer; AValue : boolean); 

begin
  If (Farchived=AValue) then exit;
  Farchived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetaudienceSegmentId(AIndex : Integer; AValue : String); 

begin
  If (FaudienceSegmentId=AValue) then exit;
  FaudienceSegmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetcampaignId(AIndex : Integer; AValue : String); 

begin
  If (FcampaignId=AValue) then exit;
  FcampaignId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetcampaignIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FcampaignIdDimensionValue=AValue) then exit;
  FcampaignIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetclickThroughUrl(AIndex : Integer; AValue : TClickThroughUrl); 

begin
  If (FclickThroughUrl=AValue) then exit;
  FclickThroughUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetclickThroughUrlSuffixProperties(AIndex : Integer; AValue : TClickThroughUrlSuffixProperties); 

begin
  If (FclickThroughUrlSuffixProperties=AValue) then exit;
  FclickThroughUrlSuffixProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setcomments(AIndex : Integer; AValue : String); 

begin
  If (Fcomments=AValue) then exit;
  Fcomments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setcompatibility(AIndex : Integer; AValue : String); 

begin
  If (Fcompatibility=AValue) then exit;
  Fcompatibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetcreateInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FcreateInfo=AValue) then exit;
  FcreateInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetcreativeGroupAssignments(AIndex : Integer; AValue : TAdTypecreativeGroupAssignmentsArray); 

begin
  If (FcreativeGroupAssignments=AValue) then exit;
  FcreativeGroupAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetcreativeRotation(AIndex : Integer; AValue : TCreativeRotation); 

begin
  If (FcreativeRotation=AValue) then exit;
  FcreativeRotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetdayPartTargeting(AIndex : Integer; AValue : TDayPartTargeting); 

begin
  If (FdayPartTargeting=AValue) then exit;
  FdayPartTargeting:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetdefaultClickThroughEventTagProperties(AIndex : Integer; AValue : TDefaultClickThroughEventTagProperties); 

begin
  If (FdefaultClickThroughEventTagProperties=AValue) then exit;
  FdefaultClickThroughEventTagProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetdeliverySchedule(AIndex : Integer; AValue : TDeliverySchedule); 

begin
  If (FdeliverySchedule=AValue) then exit;
  FdeliverySchedule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetdynamicClickTracker(AIndex : Integer; AValue : boolean); 

begin
  If (FdynamicClickTracker=AValue) then exit;
  FdynamicClickTracker:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetendTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SeteventTagOverrides(AIndex : Integer; AValue : TAdTypeeventTagOverridesArray); 

begin
  If (FeventTagOverrides=AValue) then exit;
  FeventTagOverrides:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetgeoTargeting(AIndex : Integer; AValue : TGeoTargeting); 

begin
  If (FgeoTargeting=AValue) then exit;
  FgeoTargeting:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetkeyValueTargetingExpression(AIndex : Integer; AValue : TKeyValueTargetingExpression); 

begin
  If (FkeyValueTargetingExpression=AValue) then exit;
  FkeyValueTargetingExpression:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FlastModifiedInfo=AValue) then exit;
  FlastModifiedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetplacementAssignments(AIndex : Integer; AValue : TAdTypeplacementAssignmentsArray); 

begin
  If (FplacementAssignments=AValue) then exit;
  FplacementAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setremarketing_list_expression(AIndex : Integer; AValue : TListTargetingExpression); 

begin
  If (Fremarketing_list_expression=AValue) then exit;
  Fremarketing_list_expression:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Setsize(AIndex : Integer; AValue : TSize); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetsslCompliant(AIndex : Integer; AValue : boolean); 

begin
  If (FsslCompliant=AValue) then exit;
  FsslCompliant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetsslRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FsslRequired=AValue) then exit;
  FsslRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetstartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.SettechnologyTargeting(AIndex : Integer; AValue : TTechnologyTargeting); 

begin
  If (FtechnologyTargeting=AValue) then exit;
  FtechnologyTargeting:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAd.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAd.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdSlot
  --------------------------------------------------------------------}


Procedure TAdSlot.Setcomment(AIndex : Integer; AValue : String); 

begin
  If (Fcomment=AValue) then exit;
  Fcomment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdSlot.Setcompatibility(AIndex : Integer; AValue : String); 

begin
  If (Fcompatibility=AValue) then exit;
  Fcompatibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdSlot.Setheight(AIndex : Integer; AValue : String); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdSlot.SetlinkedPlacementId(AIndex : Integer; AValue : String); 

begin
  If (FlinkedPlacementId=AValue) then exit;
  FlinkedPlacementId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdSlot.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdSlot.SetpaymentSourceType(AIndex : Integer; AValue : String); 

begin
  If (FpaymentSourceType=AValue) then exit;
  FpaymentSourceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdSlot.Setprimary(AIndex : Integer; AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdSlot.Setwidth(AIndex : Integer; AValue : String); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdsListResponse
  --------------------------------------------------------------------}


Procedure TAdsListResponse.Setads(AIndex : Integer; AValue : TAdsListResponseTypeadsArray); 

begin
  If (Fads=AValue) then exit;
  Fads:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdvertiser
  --------------------------------------------------------------------}


Procedure TAdvertiser.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetadvertiserGroupId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserGroupId=AValue) then exit;
  FadvertiserGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetclickThroughUrlSuffix(AIndex : Integer; AValue : String); 

begin
  If (FclickThroughUrlSuffix=AValue) then exit;
  FclickThroughUrlSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetdefaultClickThroughEventTagId(AIndex : Integer; AValue : String); 

begin
  If (FdefaultClickThroughEventTagId=AValue) then exit;
  FdefaultClickThroughEventTagId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetdefaultEmail(AIndex : Integer; AValue : String); 

begin
  If (FdefaultEmail=AValue) then exit;
  FdefaultEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetfloodlightConfigurationId(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightConfigurationId=AValue) then exit;
  FfloodlightConfigurationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetfloodlightConfigurationIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FfloodlightConfigurationIdDimensionValue=AValue) then exit;
  FfloodlightConfigurationIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetoriginalFloodlightConfigurationId(AIndex : Integer; AValue : String); 

begin
  If (ForiginalFloodlightConfigurationId=AValue) then exit;
  ForiginalFloodlightConfigurationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdvertiserGroup
  --------------------------------------------------------------------}


Procedure TAdvertiserGroup.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiserGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiserGroup.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiserGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdvertiserGroupsListResponse
  --------------------------------------------------------------------}


Procedure TAdvertiserGroupsListResponse.SetadvertiserGroups(AIndex : Integer; AValue : TAdvertiserGroupsListResponseTypeadvertiserGroupsArray); 

begin
  If (FadvertiserGroups=AValue) then exit;
  FadvertiserGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiserGroupsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiserGroupsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdvertisersListResponse
  --------------------------------------------------------------------}


Procedure TAdvertisersListResponse.Setadvertisers(AIndex : Integer; AValue : TAdvertisersListResponseTypeadvertisersArray); 

begin
  If (Fadvertisers=AValue) then exit;
  Fadvertisers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertisersListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertisersListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAudienceSegment
  --------------------------------------------------------------------}


Procedure TAudienceSegment.Setallocation(AIndex : Integer; AValue : integer); 

begin
  If (Fallocation=AValue) then exit;
  Fallocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudienceSegment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudienceSegment.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAudienceSegmentGroup
  --------------------------------------------------------------------}


Procedure TAudienceSegmentGroup.SetaudienceSegments(AIndex : Integer; AValue : TAudienceSegmentGroupTypeaudienceSegmentsArray); 

begin
  If (FaudienceSegments=AValue) then exit;
  FaudienceSegments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudienceSegmentGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudienceSegmentGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBrowser
  --------------------------------------------------------------------}


Procedure TBrowser.SetbrowserVersionId(AIndex : Integer; AValue : String); 

begin
  If (FbrowserVersionId=AValue) then exit;
  FbrowserVersionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBrowser.SetdartId(AIndex : Integer; AValue : String); 

begin
  If (FdartId=AValue) then exit;
  FdartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBrowser.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBrowser.SetmajorVersion(AIndex : Integer; AValue : String); 

begin
  If (FmajorVersion=AValue) then exit;
  FmajorVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBrowser.SetminorVersion(AIndex : Integer; AValue : String); 

begin
  If (FminorVersion=AValue) then exit;
  FminorVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBrowser.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBrowsersListResponse
  --------------------------------------------------------------------}


Procedure TBrowsersListResponse.Setbrowsers(AIndex : Integer; AValue : TBrowsersListResponseTypebrowsersArray); 

begin
  If (Fbrowsers=AValue) then exit;
  Fbrowsers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBrowsersListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCampaign
  --------------------------------------------------------------------}


Procedure TCampaign.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetadditionalCreativeOptimizationConfigurations(AIndex : Integer; AValue : TCampaignTypeadditionalCreativeOptimizationConfigurationsArray); 

begin
  If (FadditionalCreativeOptimizationConfigurations=AValue) then exit;
  FadditionalCreativeOptimizationConfigurations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetadvertiserGroupId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserGroupId=AValue) then exit;
  FadvertiserGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.Setarchived(AIndex : Integer; AValue : boolean); 

begin
  If (Farchived=AValue) then exit;
  Farchived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetaudienceSegmentGroups(AIndex : Integer; AValue : TCampaignTypeaudienceSegmentGroupsArray); 

begin
  If (FaudienceSegmentGroups=AValue) then exit;
  FaudienceSegmentGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetbillingInvoiceCode(AIndex : Integer; AValue : String); 

begin
  If (FbillingInvoiceCode=AValue) then exit;
  FbillingInvoiceCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetclickThroughUrlSuffixProperties(AIndex : Integer; AValue : TClickThroughUrlSuffixProperties); 

begin
  If (FclickThroughUrlSuffixProperties=AValue) then exit;
  FclickThroughUrlSuffixProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.Setcomment(AIndex : Integer; AValue : String); 

begin
  If (Fcomment=AValue) then exit;
  Fcomment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetcomscoreVceEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FcomscoreVceEnabled=AValue) then exit;
  FcomscoreVceEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetcreateInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FcreateInfo=AValue) then exit;
  FcreateInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetcreativeGroupIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcreativeGroupIds=AValue) then exit;
  FcreativeGroupIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetcreativeOptimizationConfiguration(AIndex : Integer; AValue : TCreativeOptimizationConfiguration); 

begin
  If (FcreativeOptimizationConfiguration=AValue) then exit;
  FcreativeOptimizationConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetdefaultClickThroughEventTagProperties(AIndex : Integer; AValue : TDefaultClickThroughEventTagProperties); 

begin
  If (FdefaultClickThroughEventTagProperties=AValue) then exit;
  FdefaultClickThroughEventTagProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetendDate(AIndex : Integer; AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SeteventTagOverrides(AIndex : Integer; AValue : TCampaignTypeeventTagOverridesArray); 

begin
  If (FeventTagOverrides=AValue) then exit;
  FeventTagOverrides:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetexternalId(AIndex : Integer; AValue : String); 

begin
  If (FexternalId=AValue) then exit;
  FexternalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FlastModifiedInfo=AValue) then exit;
  FlastModifiedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); 

begin
  If (FlookbackConfiguration=AValue) then exit;
  FlookbackConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetnielsenOcrEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FnielsenOcrEnabled=AValue) then exit;
  FnielsenOcrEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaign.SettraffickerEmails(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtraffickerEmails=AValue) then exit;
  FtraffickerEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCampaignCreativeAssociation
  --------------------------------------------------------------------}


Procedure TCampaignCreativeAssociation.SetcreativeId(AIndex : Integer; AValue : String); 

begin
  If (FcreativeId=AValue) then exit;
  FcreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaignCreativeAssociation.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCampaignCreativeAssociationsListResponse
  --------------------------------------------------------------------}


Procedure TCampaignCreativeAssociationsListResponse.SetcampaignCreativeAssociations(AIndex : Integer; AValue : TCampaignCreativeAssociationsListResponseTypecampaignCreativeAssociationsArray); 

begin
  If (FcampaignCreativeAssociations=AValue) then exit;
  FcampaignCreativeAssociations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaignCreativeAssociationsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaignCreativeAssociationsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCampaignsListResponse
  --------------------------------------------------------------------}


Procedure TCampaignsListResponse.Setcampaigns(AIndex : Integer; AValue : TCampaignsListResponseTypecampaignsArray); 

begin
  If (Fcampaigns=AValue) then exit;
  Fcampaigns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaignsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCampaignsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChangeLog
  --------------------------------------------------------------------}


Procedure TChangeLog.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.Setaction(AIndex : Integer; AValue : String); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetchangeTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FchangeTime=AValue) then exit;
  FchangeTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetfieldName(AIndex : Integer; AValue : String); 

begin
  If (FfieldName=AValue) then exit;
  FfieldName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetnewValue(AIndex : Integer; AValue : String); 

begin
  If (FnewValue=AValue) then exit;
  FnewValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetobjectId(AIndex : Integer; AValue : String); 

begin
  If (FobjectId=AValue) then exit;
  FobjectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetobjectType(AIndex : Integer; AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetoldValue(AIndex : Integer; AValue : String); 

begin
  If (FoldValue=AValue) then exit;
  FoldValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SettransactionId(AIndex : Integer; AValue : String); 

begin
  If (FtransactionId=AValue) then exit;
  FtransactionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetuserProfileId(AIndex : Integer; AValue : String); 

begin
  If (FuserProfileId=AValue) then exit;
  FuserProfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLog.SetuserProfileName(AIndex : Integer; AValue : String); 

begin
  If (FuserProfileName=AValue) then exit;
  FuserProfileName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChangeLogsListResponse
  --------------------------------------------------------------------}


Procedure TChangeLogsListResponse.SetchangeLogs(AIndex : Integer; AValue : TChangeLogsListResponseTypechangeLogsArray); 

begin
  If (FchangeLogs=AValue) then exit;
  FchangeLogs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLogsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeLogsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCitiesListResponse
  --------------------------------------------------------------------}


Procedure TCitiesListResponse.Setcities(AIndex : Integer; AValue : TCitiesListResponseTypecitiesArray); 

begin
  If (Fcities=AValue) then exit;
  Fcities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCitiesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCity
  --------------------------------------------------------------------}


Procedure TCity.SetcountryCode(AIndex : Integer; AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCity.SetcountryDartId(AIndex : Integer; AValue : String); 

begin
  If (FcountryDartId=AValue) then exit;
  FcountryDartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCity.SetdartId(AIndex : Integer; AValue : String); 

begin
  If (FdartId=AValue) then exit;
  FdartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCity.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCity.SetmetroCode(AIndex : Integer; AValue : String); 

begin
  If (FmetroCode=AValue) then exit;
  FmetroCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCity.SetmetroDmaId(AIndex : Integer; AValue : String); 

begin
  If (FmetroDmaId=AValue) then exit;
  FmetroDmaId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCity.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCity.SetregionCode(AIndex : Integer; AValue : String); 

begin
  If (FregionCode=AValue) then exit;
  FregionCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCity.SetregionDartId(AIndex : Integer; AValue : String); 

begin
  If (FregionDartId=AValue) then exit;
  FregionDartId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TClickTag
  --------------------------------------------------------------------}


Procedure TClickTag.SeteventName(AIndex : Integer; AValue : String); 

begin
  If (FeventName=AValue) then exit;
  FeventName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClickTag.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClickTag.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TClickThroughUrl
  --------------------------------------------------------------------}


Procedure TClickThroughUrl.SetcustomClickThroughUrl(AIndex : Integer; AValue : String); 

begin
  If (FcustomClickThroughUrl=AValue) then exit;
  FcustomClickThroughUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClickThroughUrl.SetdefaultLandingPage(AIndex : Integer; AValue : boolean); 

begin
  If (FdefaultLandingPage=AValue) then exit;
  FdefaultLandingPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClickThroughUrl.SetlandingPageId(AIndex : Integer; AValue : String); 

begin
  If (FlandingPageId=AValue) then exit;
  FlandingPageId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TClickThroughUrlSuffixProperties
  --------------------------------------------------------------------}


Procedure TClickThroughUrlSuffixProperties.SetclickThroughUrlSuffix(AIndex : Integer; AValue : String); 

begin
  If (FclickThroughUrlSuffix=AValue) then exit;
  FclickThroughUrlSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClickThroughUrlSuffixProperties.SetoverrideInheritedSuffix(AIndex : Integer; AValue : boolean); 

begin
  If (FoverrideInheritedSuffix=AValue) then exit;
  FoverrideInheritedSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCompanionClickThroughOverride
  --------------------------------------------------------------------}


Procedure TCompanionClickThroughOverride.SetclickThroughUrl(AIndex : Integer; AValue : TClickThroughUrl); 

begin
  If (FclickThroughUrl=AValue) then exit;
  FclickThroughUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompanionClickThroughOverride.SetcreativeId(AIndex : Integer; AValue : String); 

begin
  If (FcreativeId=AValue) then exit;
  FcreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCompatibleFields
  --------------------------------------------------------------------}


Procedure TCompatibleFields.SetcrossDimensionReachReportCompatibleFields(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFields); 

begin
  If (FcrossDimensionReachReportCompatibleFields=AValue) then exit;
  FcrossDimensionReachReportCompatibleFields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompatibleFields.SetfloodlightReportCompatibleFields(AIndex : Integer; AValue : TFloodlightReportCompatibleFields); 

begin
  If (FfloodlightReportCompatibleFields=AValue) then exit;
  FfloodlightReportCompatibleFields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompatibleFields.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompatibleFields.SetpathToConversionReportCompatibleFields(AIndex : Integer; AValue : TPathToConversionReportCompatibleFields); 

begin
  If (FpathToConversionReportCompatibleFields=AValue) then exit;
  FpathToConversionReportCompatibleFields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompatibleFields.SetreachReportCompatibleFields(AIndex : Integer; AValue : TReachReportCompatibleFields); 

begin
  If (FreachReportCompatibleFields=AValue) then exit;
  FreachReportCompatibleFields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompatibleFields.SetreportCompatibleFields(AIndex : Integer; AValue : TReportCompatibleFields); 

begin
  If (FreportCompatibleFields=AValue) then exit;
  FreportCompatibleFields:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConnectionType
  --------------------------------------------------------------------}


Procedure TConnectionType.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConnectionType.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConnectionType.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConnectionTypesListResponse
  --------------------------------------------------------------------}


Procedure TConnectionTypesListResponse.SetconnectionTypes(AIndex : Integer; AValue : TConnectionTypesListResponseTypeconnectionTypesArray); 

begin
  If (FconnectionTypes=AValue) then exit;
  FconnectionTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConnectionTypesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContentCategoriesListResponse
  --------------------------------------------------------------------}


Procedure TContentCategoriesListResponse.SetcontentCategories(AIndex : Integer; AValue : TContentCategoriesListResponseTypecontentCategoriesArray); 

begin
  If (FcontentCategories=AValue) then exit;
  FcontentCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentCategoriesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentCategoriesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContentCategory
  --------------------------------------------------------------------}


Procedure TContentCategory.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentCategory.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentCategory.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContentCategory.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCountriesListResponse
  --------------------------------------------------------------------}


Procedure TCountriesListResponse.Setcountries(AIndex : Integer; AValue : TCountriesListResponseTypecountriesArray); 

begin
  If (Fcountries=AValue) then exit;
  Fcountries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCountriesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCountry
  --------------------------------------------------------------------}


Procedure TCountry.SetcountryCode(AIndex : Integer; AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCountry.SetdartId(AIndex : Integer; AValue : String); 

begin
  If (FdartId=AValue) then exit;
  FdartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCountry.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCountry.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCountry.SetsslEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FsslEnabled=AValue) then exit;
  FsslEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreative
  --------------------------------------------------------------------}


Procedure TCreative.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadParameters(AIndex : Integer; AValue : String); 

begin
  If (FadParameters=AValue) then exit;
  FadParameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadTagKeys(AIndex : Integer; AValue : TStringArray); 

begin
  If (FadTagKeys=AValue) then exit;
  FadTagKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetallowScriptAccess(AIndex : Integer; AValue : boolean); 

begin
  If (FallowScriptAccess=AValue) then exit;
  FallowScriptAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setarchived(AIndex : Integer; AValue : boolean); 

begin
  If (Farchived=AValue) then exit;
  Farchived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetartworkType(AIndex : Integer; AValue : String); 

begin
  If (FartworkType=AValue) then exit;
  FartworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetauthoringTool(AIndex : Integer; AValue : String); 

begin
  If (FauthoringTool=AValue) then exit;
  FauthoringTool:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setauto_advance_images(AIndex : Integer; AValue : boolean); 

begin
  If (Fauto_advance_images=AValue) then exit;
  Fauto_advance_images:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetbackgroundColor(AIndex : Integer; AValue : String); 

begin
  If (FbackgroundColor=AValue) then exit;
  FbackgroundColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetbackupImageClickThroughUrl(AIndex : Integer; AValue : String); 

begin
  If (FbackupImageClickThroughUrl=AValue) then exit;
  FbackupImageClickThroughUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetbackupImageFeatures(AIndex : Integer; AValue : TStringArray); 

begin
  If (FbackupImageFeatures=AValue) then exit;
  FbackupImageFeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetbackupImageReportingLabel(AIndex : Integer; AValue : String); 

begin
  If (FbackupImageReportingLabel=AValue) then exit;
  FbackupImageReportingLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetbackupImageTargetWindow(AIndex : Integer; AValue : TTargetWindow); 

begin
  If (FbackupImageTargetWindow=AValue) then exit;
  FbackupImageTargetWindow:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetclickTags(AIndex : Integer; AValue : TCreativeTypeclickTagsArray); 

begin
  If (FclickTags=AValue) then exit;
  FclickTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetcommercialId(AIndex : Integer; AValue : String); 

begin
  If (FcommercialId=AValue) then exit;
  FcommercialId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetcompanionCreatives(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcompanionCreatives=AValue) then exit;
  FcompanionCreatives:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setcompatibility(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fcompatibility=AValue) then exit;
  Fcompatibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetconvertFlashToHtml5(AIndex : Integer; AValue : boolean); 

begin
  If (FconvertFlashToHtml5=AValue) then exit;
  FconvertFlashToHtml5:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetcounterCustomEvents(AIndex : Integer; AValue : TCreativeTypecounterCustomEventsArray); 

begin
  If (FcounterCustomEvents=AValue) then exit;
  FcounterCustomEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetcreativeAssets(AIndex : Integer; AValue : TCreativeTypecreativeAssetsArray); 

begin
  If (FcreativeAssets=AValue) then exit;
  FcreativeAssets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetcreativeFieldAssignments(AIndex : Integer; AValue : TCreativeTypecreativeFieldAssignmentsArray); 

begin
  If (FcreativeFieldAssignments=AValue) then exit;
  FcreativeFieldAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetcustomKeyValues(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcustomKeyValues=AValue) then exit;
  FcustomKeyValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetexitCustomEvents(AIndex : Integer; AValue : TCreativeTypeexitCustomEventsArray); 

begin
  If (FexitCustomEvents=AValue) then exit;
  FexitCustomEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetfsCommand(AIndex : Integer; AValue : TFsCommand); 

begin
  If (FfsCommand=AValue) then exit;
  FfsCommand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SethtmlCode(AIndex : Integer; AValue : String); 

begin
  If (FhtmlCode=AValue) then exit;
  FhtmlCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SethtmlCodeLocked(AIndex : Integer; AValue : boolean); 

begin
  If (FhtmlCodeLocked=AValue) then exit;
  FhtmlCodeLocked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FlastModifiedInfo=AValue) then exit;
  FlastModifiedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetlatestTraffickedCreativeId(AIndex : Integer; AValue : String); 

begin
  If (FlatestTraffickedCreativeId=AValue) then exit;
  FlatestTraffickedCreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetoverrideCss(AIndex : Integer; AValue : String); 

begin
  If (FoverrideCss=AValue) then exit;
  FoverrideCss:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetredirectUrl(AIndex : Integer; AValue : String); 

begin
  If (FredirectUrl=AValue) then exit;
  FredirectUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetrenderingId(AIndex : Integer; AValue : String); 

begin
  If (FrenderingId=AValue) then exit;
  FrenderingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetrenderingIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FrenderingIdDimensionValue=AValue) then exit;
  FrenderingIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetrequiredFlashPluginVersion(AIndex : Integer; AValue : String); 

begin
  If (FrequiredFlashPluginVersion=AValue) then exit;
  FrequiredFlashPluginVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetrequiredFlashVersion(AIndex : Integer; AValue : integer); 

begin
  If (FrequiredFlashVersion=AValue) then exit;
  FrequiredFlashVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setsize(AIndex : Integer; AValue : TSize); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setskippable(AIndex : Integer; AValue : boolean); 

begin
  If (Fskippable=AValue) then exit;
  Fskippable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetsslCompliant(AIndex : Integer; AValue : boolean); 

begin
  If (FsslCompliant=AValue) then exit;
  FsslCompliant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetstudioAdvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FstudioAdvertiserId=AValue) then exit;
  FstudioAdvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetstudioCreativeId(AIndex : Integer; AValue : String); 

begin
  If (FstudioCreativeId=AValue) then exit;
  FstudioCreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetstudioTraffickedCreativeId(AIndex : Integer; AValue : String); 

begin
  If (FstudioTraffickedCreativeId=AValue) then exit;
  FstudioTraffickedCreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetthirdPartyBackupImageImpressionsUrl(AIndex : Integer; AValue : String); 

begin
  If (FthirdPartyBackupImageImpressionsUrl=AValue) then exit;
  FthirdPartyBackupImageImpressionsUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetthirdPartyRichMediaImpressionsUrl(AIndex : Integer; AValue : String); 

begin
  If (FthirdPartyRichMediaImpressionsUrl=AValue) then exit;
  FthirdPartyRichMediaImpressionsUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetthirdPartyUrls(AIndex : Integer; AValue : TCreativeTypethirdPartyUrlsArray); 

begin
  If (FthirdPartyUrls=AValue) then exit;
  FthirdPartyUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SettimerCustomEvents(AIndex : Integer; AValue : TCreativeTypetimerCustomEventsArray); 

begin
  If (FtimerCustomEvents=AValue) then exit;
  FtimerCustomEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SettotalFileSize(AIndex : Integer; AValue : String); 

begin
  If (FtotalFileSize=AValue) then exit;
  FtotalFileSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setversion(AIndex : Integer; AValue : integer); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetvideoDescription(AIndex : Integer; AValue : String); 

begin
  If (FvideoDescription=AValue) then exit;
  FvideoDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetvideoDuration(AIndex : Integer; AValue : integer); 

begin
  If (FvideoDuration=AValue) then exit;
  FvideoDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCreative.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCreativeAsset
  --------------------------------------------------------------------}


Procedure TCreativeAsset.SetactionScript3(AIndex : Integer; AValue : boolean); 

begin
  If (FactionScript3=AValue) then exit;
  FactionScript3:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setalignment(AIndex : Integer; AValue : String); 

begin
  If (Falignment=AValue) then exit;
  Falignment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetartworkType(AIndex : Integer; AValue : String); 

begin
  If (FartworkType=AValue) then exit;
  FartworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetassetIdentifier(AIndex : Integer; AValue : TCreativeAssetId); 

begin
  If (FassetIdentifier=AValue) then exit;
  FassetIdentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetbackupImageExit(AIndex : Integer; AValue : TCreativeCustomEvent); 

begin
  If (FbackupImageExit=AValue) then exit;
  FbackupImageExit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetbitRate(AIndex : Integer; AValue : integer); 

begin
  If (FbitRate=AValue) then exit;
  FbitRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetchildAssetType(AIndex : Integer; AValue : String); 

begin
  If (FchildAssetType=AValue) then exit;
  FchildAssetType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetcollapsedSize(AIndex : Integer; AValue : TSize); 

begin
  If (FcollapsedSize=AValue) then exit;
  FcollapsedSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetcustomStartTimeValue(AIndex : Integer; AValue : integer); 

begin
  If (FcustomStartTimeValue=AValue) then exit;
  FcustomStartTimeValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetdetectedFeatures(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdetectedFeatures=AValue) then exit;
  FdetectedFeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetdisplayType(AIndex : Integer; AValue : String); 

begin
  If (FdisplayType=AValue) then exit;
  FdisplayType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setduration(AIndex : Integer; AValue : integer); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetdurationType(AIndex : Integer; AValue : String); 

begin
  If (FdurationType=AValue) then exit;
  FdurationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetexpandedDimension(AIndex : Integer; AValue : TSize); 

begin
  If (FexpandedDimension=AValue) then exit;
  FexpandedDimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetfileSize(AIndex : Integer; AValue : String); 

begin
  If (FfileSize=AValue) then exit;
  FfileSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetflashVersion(AIndex : Integer; AValue : integer); 

begin
  If (FflashVersion=AValue) then exit;
  FflashVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SethideFlashObjects(AIndex : Integer; AValue : boolean); 

begin
  If (FhideFlashObjects=AValue) then exit;
  FhideFlashObjects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SethideSelectionBoxes(AIndex : Integer; AValue : boolean); 

begin
  If (FhideSelectionBoxes=AValue) then exit;
  FhideSelectionBoxes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SethorizontallyLocked(AIndex : Integer; AValue : boolean); 

begin
  If (FhorizontallyLocked=AValue) then exit;
  FhorizontallyLocked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetmimeType(AIndex : Integer; AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setoffset(AIndex : Integer; AValue : TOffsetPosition); 

begin
  If (Foffset=AValue) then exit;
  Foffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetoriginalBackup(AIndex : Integer; AValue : boolean); 

begin
  If (ForiginalBackup=AValue) then exit;
  ForiginalBackup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setposition(AIndex : Integer; AValue : TOffsetPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetpositionLeftUnit(AIndex : Integer; AValue : String); 

begin
  If (FpositionLeftUnit=AValue) then exit;
  FpositionLeftUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetpositionTopUnit(AIndex : Integer; AValue : String); 

begin
  If (FpositionTopUnit=AValue) then exit;
  FpositionTopUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetprogressiveServingUrl(AIndex : Integer; AValue : String); 

begin
  If (FprogressiveServingUrl=AValue) then exit;
  FprogressiveServingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setpushdown(AIndex : Integer; AValue : boolean); 

begin
  If (Fpushdown=AValue) then exit;
  Fpushdown:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetpushdownDuration(AIndex : Integer; AValue : integer); 

begin
  If (FpushdownDuration=AValue) then exit;
  FpushdownDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setrole(AIndex : Integer; AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Setsize(AIndex : Integer; AValue : TSize); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetsslCompliant(AIndex : Integer; AValue : boolean); 

begin
  If (FsslCompliant=AValue) then exit;
  FsslCompliant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetstartTimeType(AIndex : Integer; AValue : String); 

begin
  If (FstartTimeType=AValue) then exit;
  FstartTimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetstreamingServingUrl(AIndex : Integer; AValue : String); 

begin
  If (FstreamingServingUrl=AValue) then exit;
  FstreamingServingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.Settransparency(AIndex : Integer; AValue : boolean); 

begin
  If (Ftransparency=AValue) then exit;
  Ftransparency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetverticallyLocked(AIndex : Integer; AValue : boolean); 

begin
  If (FverticallyLocked=AValue) then exit;
  FverticallyLocked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetvideoDuration(AIndex : Integer; AValue : integer); 

begin
  If (FvideoDuration=AValue) then exit;
  FvideoDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetwindowMode(AIndex : Integer; AValue : String); 

begin
  If (FwindowMode=AValue) then exit;
  FwindowMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetzIndex(AIndex : Integer; AValue : integer); 

begin
  If (FzIndex=AValue) then exit;
  FzIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetzipFilename(AIndex : Integer; AValue : String); 

begin
  If (FzipFilename=AValue) then exit;
  FzipFilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAsset.SetzipFilesize(AIndex : Integer; AValue : String); 

begin
  If (FzipFilesize=AValue) then exit;
  FzipFilesize:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeAssetId
  --------------------------------------------------------------------}


Procedure TCreativeAssetId.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssetId.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCreativeAssetId.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCreativeAssetMetadata
  --------------------------------------------------------------------}


Procedure TCreativeAssetMetadata.SetassetIdentifier(AIndex : Integer; AValue : TCreativeAssetId); 

begin
  If (FassetIdentifier=AValue) then exit;
  FassetIdentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssetMetadata.SetclickTags(AIndex : Integer; AValue : TCreativeAssetMetadataTypeclickTagsArray); 

begin
  If (FclickTags=AValue) then exit;
  FclickTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssetMetadata.SetdetectedFeatures(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdetectedFeatures=AValue) then exit;
  FdetectedFeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssetMetadata.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssetMetadata.SetwarnedValidationRules(AIndex : Integer; AValue : TStringArray); 

begin
  If (FwarnedValidationRules=AValue) then exit;
  FwarnedValidationRules:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeAssignment
  --------------------------------------------------------------------}


Procedure TCreativeAssignment.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetapplyEventTags(AIndex : Integer; AValue : boolean); 

begin
  If (FapplyEventTags=AValue) then exit;
  FapplyEventTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetclickThroughUrl(AIndex : Integer; AValue : TClickThroughUrl); 

begin
  If (FclickThroughUrl=AValue) then exit;
  FclickThroughUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetcompanionCreativeOverrides(AIndex : Integer; AValue : TCreativeAssignmentTypecompanionCreativeOverridesArray); 

begin
  If (FcompanionCreativeOverrides=AValue) then exit;
  FcompanionCreativeOverrides:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetcreativeGroupAssignments(AIndex : Integer; AValue : TCreativeAssignmentTypecreativeGroupAssignmentsArray); 

begin
  If (FcreativeGroupAssignments=AValue) then exit;
  FcreativeGroupAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetcreativeId(AIndex : Integer; AValue : String); 

begin
  If (FcreativeId=AValue) then exit;
  FcreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetcreativeIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FcreativeIdDimensionValue=AValue) then exit;
  FcreativeIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetendTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetrichMediaExitOverrides(AIndex : Integer; AValue : TCreativeAssignmentTyperichMediaExitOverridesArray); 

begin
  If (FrichMediaExitOverrides=AValue) then exit;
  FrichMediaExitOverrides:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.Setsequence(AIndex : Integer; AValue : integer); 

begin
  If (Fsequence=AValue) then exit;
  Fsequence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetsslCompliant(AIndex : Integer; AValue : boolean); 

begin
  If (FsslCompliant=AValue) then exit;
  FsslCompliant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.SetstartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeAssignment.Setweight(AIndex : Integer; AValue : integer); 

begin
  If (Fweight=AValue) then exit;
  Fweight:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeCustomEvent
  --------------------------------------------------------------------}


Procedure TCreativeCustomEvent.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.SetadvertiserCustomEventName(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserCustomEventName=AValue) then exit;
  FadvertiserCustomEventName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.SetadvertiserCustomEventType(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserCustomEventType=AValue) then exit;
  FadvertiserCustomEventType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.SetartworkLabel(AIndex : Integer; AValue : String); 

begin
  If (FartworkLabel=AValue) then exit;
  FartworkLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.SetartworkType(AIndex : Integer; AValue : String); 

begin
  If (FartworkType=AValue) then exit;
  FartworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.SetexitUrl(AIndex : Integer; AValue : String); 

begin
  If (FexitUrl=AValue) then exit;
  FexitUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.SetpopupWindowProperties(AIndex : Integer; AValue : TPopupWindowProperties); 

begin
  If (FpopupWindowProperties=AValue) then exit;
  FpopupWindowProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.SettargetType(AIndex : Integer; AValue : String); 

begin
  If (FtargetType=AValue) then exit;
  FtargetType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeCustomEvent.SetvideoReportingId(AIndex : Integer; AValue : String); 

begin
  If (FvideoReportingId=AValue) then exit;
  FvideoReportingId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeField
  --------------------------------------------------------------------}


Procedure TCreativeField.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeField.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeField.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeField.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeField.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeField.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeField.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeFieldAssignment
  --------------------------------------------------------------------}


Procedure TCreativeFieldAssignment.SetcreativeFieldId(AIndex : Integer; AValue : String); 

begin
  If (FcreativeFieldId=AValue) then exit;
  FcreativeFieldId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeFieldAssignment.SetcreativeFieldValueId(AIndex : Integer; AValue : String); 

begin
  If (FcreativeFieldValueId=AValue) then exit;
  FcreativeFieldValueId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeFieldValue
  --------------------------------------------------------------------}


Procedure TCreativeFieldValue.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeFieldValue.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeFieldValue.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeFieldValuesListResponse
  --------------------------------------------------------------------}


Procedure TCreativeFieldValuesListResponse.SetcreativeFieldValues(AIndex : Integer; AValue : TCreativeFieldValuesListResponseTypecreativeFieldValuesArray); 

begin
  If (FcreativeFieldValues=AValue) then exit;
  FcreativeFieldValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeFieldValuesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeFieldValuesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeFieldsListResponse
  --------------------------------------------------------------------}


Procedure TCreativeFieldsListResponse.SetcreativeFields(AIndex : Integer; AValue : TCreativeFieldsListResponseTypecreativeFieldsArray); 

begin
  If (FcreativeFields=AValue) then exit;
  FcreativeFields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeFieldsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeFieldsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeGroup
  --------------------------------------------------------------------}


Procedure TCreativeGroup.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroup.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroup.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroup.SetgroupNumber(AIndex : Integer; AValue : integer); 

begin
  If (FgroupNumber=AValue) then exit;
  FgroupNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroup.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroup.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeGroupAssignment
  --------------------------------------------------------------------}


Procedure TCreativeGroupAssignment.SetcreativeGroupId(AIndex : Integer; AValue : String); 

begin
  If (FcreativeGroupId=AValue) then exit;
  FcreativeGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroupAssignment.SetcreativeGroupNumber(AIndex : Integer; AValue : String); 

begin
  If (FcreativeGroupNumber=AValue) then exit;
  FcreativeGroupNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeGroupsListResponse
  --------------------------------------------------------------------}


Procedure TCreativeGroupsListResponse.SetcreativeGroups(AIndex : Integer; AValue : TCreativeGroupsListResponseTypecreativeGroupsArray); 

begin
  If (FcreativeGroups=AValue) then exit;
  FcreativeGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroupsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeGroupsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeOptimizationConfiguration
  --------------------------------------------------------------------}


Procedure TCreativeOptimizationConfiguration.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeOptimizationConfiguration.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeOptimizationConfiguration.SetoptimizationActivitys(AIndex : Integer; AValue : TCreativeOptimizationConfigurationTypeoptimizationActivitysArray); 

begin
  If (FoptimizationActivitys=AValue) then exit;
  FoptimizationActivitys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeOptimizationConfiguration.SetoptimizationModel(AIndex : Integer; AValue : String); 

begin
  If (FoptimizationModel=AValue) then exit;
  FoptimizationModel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeRotation
  --------------------------------------------------------------------}


Procedure TCreativeRotation.SetcreativeAssignments(AIndex : Integer; AValue : TCreativeRotationTypecreativeAssignmentsArray); 

begin
  If (FcreativeAssignments=AValue) then exit;
  FcreativeAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeRotation.SetcreativeOptimizationConfigurationId(AIndex : Integer; AValue : String); 

begin
  If (FcreativeOptimizationConfigurationId=AValue) then exit;
  FcreativeOptimizationConfigurationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeRotation.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeRotation.SetweightCalculationStrategy(AIndex : Integer; AValue : String); 

begin
  If (FweightCalculationStrategy=AValue) then exit;
  FweightCalculationStrategy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCreativeRotation.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCreativeSettings
  --------------------------------------------------------------------}


Procedure TCreativeSettings.SetiFrameFooter(AIndex : Integer; AValue : String); 

begin
  If (FiFrameFooter=AValue) then exit;
  FiFrameFooter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeSettings.SetiFrameHeader(AIndex : Integer; AValue : String); 

begin
  If (FiFrameHeader=AValue) then exit;
  FiFrameHeader:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativesListResponse
  --------------------------------------------------------------------}


Procedure TCreativesListResponse.Setcreatives(AIndex : Integer; AValue : TCreativesListResponseTypecreativesArray); 

begin
  If (Fcreatives=AValue) then exit;
  Fcreatives:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCrossDimensionReachReportCompatibleFields
  --------------------------------------------------------------------}


Procedure TCrossDimensionReachReportCompatibleFields.Setbreakdown(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFieldsTypebreakdownArray); 

begin
  If (Fbreakdown=AValue) then exit;
  Fbreakdown:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCrossDimensionReachReportCompatibleFields.SetdimensionFilters(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFieldsTypedimensionFiltersArray); 

begin
  If (FdimensionFilters=AValue) then exit;
  FdimensionFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCrossDimensionReachReportCompatibleFields.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCrossDimensionReachReportCompatibleFields.Setmetrics(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFieldsTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCrossDimensionReachReportCompatibleFields.SetoverlapMetrics(AIndex : Integer; AValue : TCrossDimensionReachReportCompatibleFieldsTypeoverlapMetricsArray); 

begin
  If (FoverlapMetrics=AValue) then exit;
  FoverlapMetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomRichMediaEvents
  --------------------------------------------------------------------}


Procedure TCustomRichMediaEvents.SetfilteredEventIds(AIndex : Integer; AValue : TCustomRichMediaEventsTypefilteredEventIdsArray); 

begin
  If (FfilteredEventIds=AValue) then exit;
  FfilteredEventIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomRichMediaEvents.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDateRange
  --------------------------------------------------------------------}


Procedure TDateRange.SetendDate(AIndex : Integer; AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDateRange.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDateRange.SetrelativeDateRange(AIndex : Integer; AValue : String); 

begin
  If (FrelativeDateRange=AValue) then exit;
  FrelativeDateRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDateRange.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDayPartTargeting
  --------------------------------------------------------------------}


Procedure TDayPartTargeting.SetdaysOfWeek(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdaysOfWeek=AValue) then exit;
  FdaysOfWeek:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDayPartTargeting.SethoursOfDay(AIndex : Integer; AValue : TintegerArray); 

begin
  If (FhoursOfDay=AValue) then exit;
  FhoursOfDay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDayPartTargeting.SetuserLocalTime(AIndex : Integer; AValue : boolean); 

begin
  If (FuserLocalTime=AValue) then exit;
  FuserLocalTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDefaultClickThroughEventTagProperties
  --------------------------------------------------------------------}


Procedure TDefaultClickThroughEventTagProperties.SetdefaultClickThroughEventTagId(AIndex : Integer; AValue : String); 

begin
  If (FdefaultClickThroughEventTagId=AValue) then exit;
  FdefaultClickThroughEventTagId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDefaultClickThroughEventTagProperties.SetoverrideInheritedEventTag(AIndex : Integer; AValue : boolean); 

begin
  If (FoverrideInheritedEventTag=AValue) then exit;
  FoverrideInheritedEventTag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeliverySchedule
  --------------------------------------------------------------------}


Procedure TDeliverySchedule.SetfrequencyCap(AIndex : Integer; AValue : TFrequencyCap); 

begin
  If (FfrequencyCap=AValue) then exit;
  FfrequencyCap:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeliverySchedule.SethardCutoff(AIndex : Integer; AValue : boolean); 

begin
  If (FhardCutoff=AValue) then exit;
  FhardCutoff:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeliverySchedule.SetimpressionRatio(AIndex : Integer; AValue : String); 

begin
  If (FimpressionRatio=AValue) then exit;
  FimpressionRatio:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeliverySchedule.Setpriority(AIndex : Integer; AValue : String); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDfpSettings
  --------------------------------------------------------------------}


Procedure TDfpSettings.Setdfp_network_code(AIndex : Integer; AValue : String); 

begin
  If (Fdfp_network_code=AValue) then exit;
  Fdfp_network_code:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDfpSettings.Setdfp_network_name(AIndex : Integer; AValue : String); 

begin
  If (Fdfp_network_name=AValue) then exit;
  Fdfp_network_name:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDfpSettings.SetprogrammaticPlacementAccepted(AIndex : Integer; AValue : boolean); 

begin
  If (FprogrammaticPlacementAccepted=AValue) then exit;
  FprogrammaticPlacementAccepted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDfpSettings.SetpubPaidPlacementAccepted(AIndex : Integer; AValue : boolean); 

begin
  If (FpubPaidPlacementAccepted=AValue) then exit;
  FpubPaidPlacementAccepted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDfpSettings.SetpublisherPortalOnly(AIndex : Integer; AValue : boolean); 

begin
  If (FpublisherPortalOnly=AValue) then exit;
  FpublisherPortalOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDimension
  --------------------------------------------------------------------}


Procedure TDimension.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimension.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDimensionFilter
  --------------------------------------------------------------------}


Procedure TDimensionFilter.SetdimensionName(AIndex : Integer; AValue : String); 

begin
  If (FdimensionName=AValue) then exit;
  FdimensionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionFilter.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionFilter.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDimensionValue
  --------------------------------------------------------------------}


Procedure TDimensionValue.SetdimensionName(AIndex : Integer; AValue : String); 

begin
  If (FdimensionName=AValue) then exit;
  FdimensionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValue.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValue.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValue.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValue.SetmatchType(AIndex : Integer; AValue : String); 

begin
  If (FmatchType=AValue) then exit;
  FmatchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValue.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDimensionValueList
  --------------------------------------------------------------------}


Procedure TDimensionValueList.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValueList.Setitems(AIndex : Integer; AValue : TDimensionValueListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValueList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValueList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDimensionValueRequest
  --------------------------------------------------------------------}


Procedure TDimensionValueRequest.SetdimensionName(AIndex : Integer; AValue : String); 

begin
  If (FdimensionName=AValue) then exit;
  FdimensionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValueRequest.SetendDate(AIndex : Integer; AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValueRequest.Setfilters(AIndex : Integer; AValue : TDimensionValueRequestTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValueRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionValueRequest.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectorySite
  --------------------------------------------------------------------}


Procedure TDirectorySite.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.SetcontactAssignments(AIndex : Integer; AValue : TDirectorySiteTypecontactAssignmentsArray); 

begin
  If (FcontactAssignments=AValue) then exit;
  FcontactAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.SetcountryId(AIndex : Integer; AValue : String); 

begin
  If (FcountryId=AValue) then exit;
  FcountryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.SetcurrencyId(AIndex : Integer; AValue : String); 

begin
  If (FcurrencyId=AValue) then exit;
  FcurrencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.SetinpageTagFormats(AIndex : Integer; AValue : TStringArray); 

begin
  If (FinpageTagFormats=AValue) then exit;
  FinpageTagFormats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.SetinterstitialTagFormats(AIndex : Integer; AValue : TStringArray); 

begin
  If (FinterstitialTagFormats=AValue) then exit;
  FinterstitialTagFormats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.SetparentId(AIndex : Integer; AValue : String); 

begin
  If (FparentId=AValue) then exit;
  FparentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.Setsettings(AIndex : Integer; AValue : TDirectorySiteSettings); 

begin
  If (Fsettings=AValue) then exit;
  Fsettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySite.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectorySiteContact
  --------------------------------------------------------------------}


Procedure TDirectorySiteContact.Setaddress(AIndex : Integer; AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.SetfirstName(AIndex : Integer; AValue : String); 

begin
  If (FfirstName=AValue) then exit;
  FfirstName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.SetlastName(AIndex : Integer; AValue : String); 

begin
  If (FlastName=AValue) then exit;
  FlastName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.Setphone(AIndex : Integer; AValue : String); 

begin
  If (Fphone=AValue) then exit;
  Fphone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.Setrole(AIndex : Integer; AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContact.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDirectorySiteContact.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDirectorySiteContactAssignment
  --------------------------------------------------------------------}


Procedure TDirectorySiteContactAssignment.SetcontactId(AIndex : Integer; AValue : String); 

begin
  If (FcontactId=AValue) then exit;
  FcontactId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContactAssignment.Setvisibility(AIndex : Integer; AValue : String); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectorySiteContactsListResponse
  --------------------------------------------------------------------}


Procedure TDirectorySiteContactsListResponse.SetdirectorySiteContacts(AIndex : Integer; AValue : TDirectorySiteContactsListResponseTypedirectorySiteContactsArray); 

begin
  If (FdirectorySiteContacts=AValue) then exit;
  FdirectorySiteContacts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContactsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteContactsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectorySiteSettings
  --------------------------------------------------------------------}


Procedure TDirectorySiteSettings.SetactiveViewOptOut(AIndex : Integer; AValue : boolean); 

begin
  If (FactiveViewOptOut=AValue) then exit;
  FactiveViewOptOut:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteSettings.Setdfp_settings(AIndex : Integer; AValue : TDfpSettings); 

begin
  If (Fdfp_settings=AValue) then exit;
  Fdfp_settings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteSettings.Setinstream_video_placement_accepted(AIndex : Integer; AValue : boolean); 

begin
  If (Finstream_video_placement_accepted=AValue) then exit;
  Finstream_video_placement_accepted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteSettings.SetinterstitialPlacementAccepted(AIndex : Integer; AValue : boolean); 

begin
  If (FinterstitialPlacementAccepted=AValue) then exit;
  FinterstitialPlacementAccepted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteSettings.SetnielsenOcrOptOut(AIndex : Integer; AValue : boolean); 

begin
  If (FnielsenOcrOptOut=AValue) then exit;
  FnielsenOcrOptOut:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteSettings.SetverificationTagOptOut(AIndex : Integer; AValue : boolean); 

begin
  If (FverificationTagOptOut=AValue) then exit;
  FverificationTagOptOut:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySiteSettings.SetvideoActiveViewOptOut(AIndex : Integer; AValue : boolean); 

begin
  If (FvideoActiveViewOptOut=AValue) then exit;
  FvideoActiveViewOptOut:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectorySitesListResponse
  --------------------------------------------------------------------}


Procedure TDirectorySitesListResponse.SetdirectorySites(AIndex : Integer; AValue : TDirectorySitesListResponseTypedirectorySitesArray); 

begin
  If (FdirectorySites=AValue) then exit;
  FdirectorySites:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySitesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectorySitesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventTag
  --------------------------------------------------------------------}


Procedure TEventTag.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetcampaignId(AIndex : Integer; AValue : String); 

begin
  If (FcampaignId=AValue) then exit;
  FcampaignId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetcampaignIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FcampaignIdDimensionValue=AValue) then exit;
  FcampaignIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetenabledByDefault(AIndex : Integer; AValue : boolean); 

begin
  If (FenabledByDefault=AValue) then exit;
  FenabledByDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetsiteFilterType(AIndex : Integer; AValue : String); 

begin
  If (FsiteFilterType=AValue) then exit;
  FsiteFilterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetsiteIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsiteIds=AValue) then exit;
  FsiteIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetsslCompliant(AIndex : Integer; AValue : boolean); 

begin
  If (FsslCompliant=AValue) then exit;
  FsslCompliant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTag.SeturlEscapeLevels(AIndex : Integer; AValue : integer); 

begin
  If (FurlEscapeLevels=AValue) then exit;
  FurlEscapeLevels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEventTag.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TEventTagOverride
  --------------------------------------------------------------------}


Procedure TEventTagOverride.Setenabled(AIndex : Integer; AValue : boolean); 

begin
  If (Fenabled=AValue) then exit;
  Fenabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTagOverride.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventTagsListResponse
  --------------------------------------------------------------------}


Procedure TEventTagsListResponse.SeteventTags(AIndex : Integer; AValue : TEventTagsListResponseTypeeventTagsArray); 

begin
  If (FeventTags=AValue) then exit;
  FeventTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTagsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileTypeurls
  --------------------------------------------------------------------}


Procedure TFileTypeurls.SetapiUrl(AIndex : Integer; AValue : String); 

begin
  If (FapiUrl=AValue) then exit;
  FapiUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeurls.SetbrowserUrl(AIndex : Integer; AValue : String); 

begin
  If (FbrowserUrl=AValue) then exit;
  FbrowserUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFile
  --------------------------------------------------------------------}


Procedure TFile.SetdateRange(AIndex : Integer; AValue : TDateRange); 

begin
  If (FdateRange=AValue) then exit;
  FdateRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetfileName(AIndex : Integer; AValue : String); 

begin
  If (FfileName=AValue) then exit;
  FfileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetlastModifiedTime(AIndex : Integer; AValue : String); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetreportId(AIndex : Integer; AValue : String); 

begin
  If (FreportId=AValue) then exit;
  FreportId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Seturls(AIndex : Integer; AValue : TFileTypeurls); 

begin
  If (Furls=AValue) then exit;
  Furls:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileList
  --------------------------------------------------------------------}


Procedure TFileList.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.Setitems(AIndex : Integer; AValue : TFileListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFlight
  --------------------------------------------------------------------}


Procedure TFlight.SetendDate(AIndex : Integer; AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlight.SetrateOrCost(AIndex : Integer; AValue : String); 

begin
  If (FrateOrCost=AValue) then exit;
  FrateOrCost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlight.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlight.Setunits(AIndex : Integer; AValue : String); 

begin
  If (Funits=AValue) then exit;
  Funits:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightActivitiesGenerateTagResponse
  --------------------------------------------------------------------}


Procedure TFloodlightActivitiesGenerateTagResponse.SetfloodlightActivityTag(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityTag=AValue) then exit;
  FfloodlightActivityTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivitiesGenerateTagResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightActivitiesListResponse
  --------------------------------------------------------------------}


Procedure TFloodlightActivitiesListResponse.SetfloodlightActivities(AIndex : Integer; AValue : TFloodlightActivitiesListResponseTypefloodlightActivitiesArray); 

begin
  If (FfloodlightActivities=AValue) then exit;
  FfloodlightActivities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivitiesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivitiesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightActivity
  --------------------------------------------------------------------}


Procedure TFloodlightActivity.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetcacheBustingType(AIndex : Integer; AValue : String); 

begin
  If (FcacheBustingType=AValue) then exit;
  FcacheBustingType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetcountingMethod(AIndex : Integer; AValue : String); 

begin
  If (FcountingMethod=AValue) then exit;
  FcountingMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetdefaultTags(AIndex : Integer; AValue : TFloodlightActivityTypedefaultTagsArray); 

begin
  If (FdefaultTags=AValue) then exit;
  FdefaultTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetexpectedUrl(AIndex : Integer; AValue : String); 

begin
  If (FexpectedUrl=AValue) then exit;
  FexpectedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetfloodlightActivityGroupId(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityGroupId=AValue) then exit;
  FfloodlightActivityGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetfloodlightActivityGroupName(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityGroupName=AValue) then exit;
  FfloodlightActivityGroupName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetfloodlightActivityGroupTagString(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityGroupTagString=AValue) then exit;
  FfloodlightActivityGroupTagString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetfloodlightActivityGroupType(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityGroupType=AValue) then exit;
  FfloodlightActivityGroupType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetfloodlightConfigurationId(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightConfigurationId=AValue) then exit;
  FfloodlightConfigurationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetfloodlightConfigurationIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FfloodlightConfigurationIdDimensionValue=AValue) then exit;
  FfloodlightConfigurationIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.Sethidden(AIndex : Integer; AValue : boolean); 

begin
  If (Fhidden=AValue) then exit;
  Fhidden:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetimageTagEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FimageTagEnabled=AValue) then exit;
  FimageTagEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetpublisherTags(AIndex : Integer; AValue : TFloodlightActivityTypepublisherTagsArray); 

begin
  If (FpublisherTags=AValue) then exit;
  FpublisherTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.Setsecure(AIndex : Integer; AValue : boolean); 

begin
  If (Fsecure=AValue) then exit;
  Fsecure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetsslCompliant(AIndex : Integer; AValue : boolean); 

begin
  If (FsslCompliant=AValue) then exit;
  FsslCompliant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetsslRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FsslRequired=AValue) then exit;
  FsslRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SettagFormat(AIndex : Integer; AValue : String); 

begin
  If (FtagFormat=AValue) then exit;
  FtagFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SettagString(AIndex : Integer; AValue : String); 

begin
  If (FtagString=AValue) then exit;
  FtagString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivity.SetuserDefinedVariableTypes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FuserDefinedVariableTypes=AValue) then exit;
  FuserDefinedVariableTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightActivityDynamicTag
  --------------------------------------------------------------------}


Procedure TFloodlightActivityDynamicTag.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityDynamicTag.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityDynamicTag.Settag(AIndex : Integer; AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightActivityGroup
  --------------------------------------------------------------------}


Procedure TFloodlightActivityGroup.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.SetfloodlightConfigurationId(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightConfigurationId=AValue) then exit;
  FfloodlightConfigurationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.SetfloodlightConfigurationIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FfloodlightConfigurationIdDimensionValue=AValue) then exit;
  FfloodlightConfigurationIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.SettagString(AIndex : Integer; AValue : String); 

begin
  If (FtagString=AValue) then exit;
  FtagString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroup.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFloodlightActivityGroup.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFloodlightActivityGroupsListResponse
  --------------------------------------------------------------------}


Procedure TFloodlightActivityGroupsListResponse.SetfloodlightActivityGroups(AIndex : Integer; AValue : TFloodlightActivityGroupsListResponseTypefloodlightActivityGroupsArray); 

begin
  If (FfloodlightActivityGroups=AValue) then exit;
  FfloodlightActivityGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroupsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityGroupsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightActivityPublisherDynamicTag
  --------------------------------------------------------------------}


Procedure TFloodlightActivityPublisherDynamicTag.SetclickThrough(AIndex : Integer; AValue : boolean); 

begin
  If (FclickThrough=AValue) then exit;
  FclickThrough:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityPublisherDynamicTag.SetdirectorySiteId(AIndex : Integer; AValue : String); 

begin
  If (FdirectorySiteId=AValue) then exit;
  FdirectorySiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityPublisherDynamicTag.SetdynamicTag(AIndex : Integer; AValue : TFloodlightActivityDynamicTag); 

begin
  If (FdynamicTag=AValue) then exit;
  FdynamicTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityPublisherDynamicTag.SetsiteId(AIndex : Integer; AValue : String); 

begin
  If (FsiteId=AValue) then exit;
  FsiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityPublisherDynamicTag.SetsiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FsiteIdDimensionValue=AValue) then exit;
  FsiteIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightActivityPublisherDynamicTag.SetviewThrough(AIndex : Integer; AValue : boolean); 

begin
  If (FviewThrough=AValue) then exit;
  FviewThrough:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightConfiguration
  --------------------------------------------------------------------}


Procedure TFloodlightConfiguration.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetanalyticsDataSharingEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FanalyticsDataSharingEnabled=AValue) then exit;
  FanalyticsDataSharingEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetexposureToConversionEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FexposureToConversionEnabled=AValue) then exit;
  FexposureToConversionEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetfirstDayOfWeek(AIndex : Integer; AValue : String); 

begin
  If (FfirstDayOfWeek=AValue) then exit;
  FfirstDayOfWeek:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); 

begin
  If (FlookbackConfiguration=AValue) then exit;
  FlookbackConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetnaturalSearchConversionAttributionOption(AIndex : Integer; AValue : String); 

begin
  If (FnaturalSearchConversionAttributionOption=AValue) then exit;
  FnaturalSearchConversionAttributionOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetomnitureSettings(AIndex : Integer; AValue : TOmnitureSettings); 

begin
  If (FomnitureSettings=AValue) then exit;
  FomnitureSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetsslRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FsslRequired=AValue) then exit;
  FsslRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetstandardVariableTypes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FstandardVariableTypes=AValue) then exit;
  FstandardVariableTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SettagSettings(AIndex : Integer; AValue : TTagSettings); 

begin
  If (FtagSettings=AValue) then exit;
  FtagSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfiguration.SetuserDefinedVariableConfigurations(AIndex : Integer; AValue : TFloodlightConfigurationTypeuserDefinedVariableConfigurationsArray); 

begin
  If (FuserDefinedVariableConfigurations=AValue) then exit;
  FuserDefinedVariableConfigurations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightConfigurationsListResponse
  --------------------------------------------------------------------}


Procedure TFloodlightConfigurationsListResponse.SetfloodlightConfigurations(AIndex : Integer; AValue : TFloodlightConfigurationsListResponseTypefloodlightConfigurationsArray); 

begin
  If (FfloodlightConfigurations=AValue) then exit;
  FfloodlightConfigurations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightConfigurationsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFloodlightReportCompatibleFields
  --------------------------------------------------------------------}


Procedure TFloodlightReportCompatibleFields.SetdimensionFilters(AIndex : Integer; AValue : TFloodlightReportCompatibleFieldsTypedimensionFiltersArray); 

begin
  If (FdimensionFilters=AValue) then exit;
  FdimensionFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightReportCompatibleFields.Setdimensions(AIndex : Integer; AValue : TFloodlightReportCompatibleFieldsTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightReportCompatibleFields.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFloodlightReportCompatibleFields.Setmetrics(AIndex : Integer; AValue : TFloodlightReportCompatibleFieldsTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFrequencyCap
  --------------------------------------------------------------------}


Procedure TFrequencyCap.Setduration(AIndex : Integer; AValue : String); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFrequencyCap.Setimpressions(AIndex : Integer; AValue : String); 

begin
  If (Fimpressions=AValue) then exit;
  Fimpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFsCommand
  --------------------------------------------------------------------}


Procedure TFsCommand.Setleft(AIndex : Integer; AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFsCommand.SetpositionOption(AIndex : Integer; AValue : String); 

begin
  If (FpositionOption=AValue) then exit;
  FpositionOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFsCommand.Settop(AIndex : Integer; AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFsCommand.SetwindowHeight(AIndex : Integer; AValue : integer); 

begin
  If (FwindowHeight=AValue) then exit;
  FwindowHeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFsCommand.SetwindowWidth(AIndex : Integer; AValue : integer); 

begin
  If (FwindowWidth=AValue) then exit;
  FwindowWidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeoTargeting
  --------------------------------------------------------------------}


Procedure TGeoTargeting.Setcities(AIndex : Integer; AValue : TGeoTargetingTypecitiesArray); 

begin
  If (Fcities=AValue) then exit;
  Fcities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoTargeting.Setcountries(AIndex : Integer; AValue : TGeoTargetingTypecountriesArray); 

begin
  If (Fcountries=AValue) then exit;
  Fcountries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoTargeting.SetexcludeCountries(AIndex : Integer; AValue : boolean); 

begin
  If (FexcludeCountries=AValue) then exit;
  FexcludeCountries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoTargeting.Setmetros(AIndex : Integer; AValue : TGeoTargetingTypemetrosArray); 

begin
  If (Fmetros=AValue) then exit;
  Fmetros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoTargeting.SetpostalCodes(AIndex : Integer; AValue : TGeoTargetingTypepostalCodesArray); 

begin
  If (FpostalCodes=AValue) then exit;
  FpostalCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoTargeting.Setregions(AIndex : Integer; AValue : TGeoTargetingTyperegionsArray); 

begin
  If (Fregions=AValue) then exit;
  Fregions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventoryItem
  --------------------------------------------------------------------}


Procedure TInventoryItem.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetadSlots(AIndex : Integer; AValue : TInventoryItemTypeadSlotsArray); 

begin
  If (FadSlots=AValue) then exit;
  FadSlots:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetcontentCategoryId(AIndex : Integer; AValue : String); 

begin
  If (FcontentCategoryId=AValue) then exit;
  FcontentCategoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetestimatedClickThroughRate(AIndex : Integer; AValue : String); 

begin
  If (FestimatedClickThroughRate=AValue) then exit;
  FestimatedClickThroughRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetestimatedConversionRate(AIndex : Integer; AValue : String); 

begin
  If (FestimatedConversionRate=AValue) then exit;
  FestimatedConversionRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetinPlan(AIndex : Integer; AValue : boolean); 

begin
  If (FinPlan=AValue) then exit;
  FinPlan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FlastModifiedInfo=AValue) then exit;
  FlastModifiedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetnegotiationChannelId(AIndex : Integer; AValue : String); 

begin
  If (FnegotiationChannelId=AValue) then exit;
  FnegotiationChannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetorderId(AIndex : Integer; AValue : String); 

begin
  If (ForderId=AValue) then exit;
  ForderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetplacementStrategyId(AIndex : Integer; AValue : String); 

begin
  If (FplacementStrategyId=AValue) then exit;
  FplacementStrategyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.Setpricing(AIndex : Integer; AValue : TPricing); 

begin
  If (Fpricing=AValue) then exit;
  Fpricing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetrfpId(AIndex : Integer; AValue : String); 

begin
  If (FrfpId=AValue) then exit;
  FrfpId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetsiteId(AIndex : Integer; AValue : String); 

begin
  If (FsiteId=AValue) then exit;
  FsiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItem.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventoryItemsListResponse
  --------------------------------------------------------------------}


Procedure TInventoryItemsListResponse.SetinventoryItems(AIndex : Integer; AValue : TInventoryItemsListResponseTypeinventoryItemsArray); 

begin
  If (FinventoryItems=AValue) then exit;
  FinventoryItems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItemsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryItemsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TKeyValueTargetingExpression
  --------------------------------------------------------------------}


Procedure TKeyValueTargetingExpression.Setexpression(AIndex : Integer; AValue : String); 

begin
  If (Fexpression=AValue) then exit;
  Fexpression:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLandingPage
  --------------------------------------------------------------------}


Procedure TLandingPage.Setdefault(AIndex : Integer; AValue : boolean); 

begin
  If (Fdefault=AValue) then exit;
  Fdefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLandingPage.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLandingPage.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLandingPage.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLandingPage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLandingPagesListResponse
  --------------------------------------------------------------------}


Procedure TLandingPagesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLandingPagesListResponse.SetlandingPages(AIndex : Integer; AValue : TLandingPagesListResponseTypelandingPagesArray); 

begin
  If (FlandingPages=AValue) then exit;
  FlandingPages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLastModifiedInfo
  --------------------------------------------------------------------}


Procedure TLastModifiedInfo.Settime(AIndex : Integer; AValue : String); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListPopulationClause
  --------------------------------------------------------------------}


Procedure TListPopulationClause.Setterms(AIndex : Integer; AValue : TListPopulationClauseTypetermsArray); 

begin
  If (Fterms=AValue) then exit;
  Fterms:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListPopulationRule
  --------------------------------------------------------------------}


Procedure TListPopulationRule.SetfloodlightActivityId(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityId=AValue) then exit;
  FfloodlightActivityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationRule.SetfloodlightActivityName(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityName=AValue) then exit;
  FfloodlightActivityName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationRule.SetlistPopulationClauses(AIndex : Integer; AValue : TListPopulationRuleTypelistPopulationClausesArray); 

begin
  If (FlistPopulationClauses=AValue) then exit;
  FlistPopulationClauses:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListPopulationTerm
  --------------------------------------------------------------------}


Procedure TListPopulationTerm.Setcontains(AIndex : Integer; AValue : boolean); 

begin
  If (Fcontains=AValue) then exit;
  Fcontains:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationTerm.Setnegation(AIndex : Integer; AValue : boolean); 

begin
  If (Fnegation=AValue) then exit;
  Fnegation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationTerm.Set_operator(AIndex : Integer; AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationTerm.SetremarketingListId(AIndex : Integer; AValue : String); 

begin
  If (FremarketingListId=AValue) then exit;
  FremarketingListId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationTerm.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationTerm.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationTerm.SetvariableFriendlyName(AIndex : Integer; AValue : String); 

begin
  If (FvariableFriendlyName=AValue) then exit;
  FvariableFriendlyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListPopulationTerm.SetvariableName(AIndex : Integer; AValue : String); 

begin
  If (FvariableName=AValue) then exit;
  FvariableName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TListPopulationTerm.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TListTargetingExpression
  --------------------------------------------------------------------}


Procedure TListTargetingExpression.Setexpression(AIndex : Integer; AValue : String); 

begin
  If (Fexpression=AValue) then exit;
  Fexpression:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLookbackConfiguration
  --------------------------------------------------------------------}


Procedure TLookbackConfiguration.SetclickDuration(AIndex : Integer; AValue : integer); 

begin
  If (FclickDuration=AValue) then exit;
  FclickDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookbackConfiguration.SetpostImpressionActivitiesDuration(AIndex : Integer; AValue : integer); 

begin
  If (FpostImpressionActivitiesDuration=AValue) then exit;
  FpostImpressionActivitiesDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetric
  --------------------------------------------------------------------}


Procedure TMetric.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetric.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetro
  --------------------------------------------------------------------}


Procedure TMetro.SetcountryCode(AIndex : Integer; AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetro.SetcountryDartId(AIndex : Integer; AValue : String); 

begin
  If (FcountryDartId=AValue) then exit;
  FcountryDartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetro.SetdartId(AIndex : Integer; AValue : String); 

begin
  If (FdartId=AValue) then exit;
  FdartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetro.SetdmaId(AIndex : Integer; AValue : String); 

begin
  If (FdmaId=AValue) then exit;
  FdmaId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetro.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetro.SetmetroCode(AIndex : Integer; AValue : String); 

begin
  If (FmetroCode=AValue) then exit;
  FmetroCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetro.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetrosListResponse
  --------------------------------------------------------------------}


Procedure TMetrosListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetrosListResponse.Setmetros(AIndex : Integer; AValue : TMetrosListResponseTypemetrosArray); 

begin
  If (Fmetros=AValue) then exit;
  Fmetros:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMobileCarrier
  --------------------------------------------------------------------}


Procedure TMobileCarrier.SetcountryCode(AIndex : Integer; AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMobileCarrier.SetcountryDartId(AIndex : Integer; AValue : String); 

begin
  If (FcountryDartId=AValue) then exit;
  FcountryDartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMobileCarrier.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMobileCarrier.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMobileCarrier.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMobileCarriersListResponse
  --------------------------------------------------------------------}


Procedure TMobileCarriersListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMobileCarriersListResponse.SetmobileCarriers(AIndex : Integer; AValue : TMobileCarriersListResponseTypemobileCarriersArray); 

begin
  If (FmobileCarriers=AValue) then exit;
  FmobileCarriers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObjectFilter
  --------------------------------------------------------------------}


Procedure TObjectFilter.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectFilter.SetobjectIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FobjectIds=AValue) then exit;
  FobjectIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectFilter.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOffsetPosition
  --------------------------------------------------------------------}


Procedure TOffsetPosition.Setleft(AIndex : Integer; AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffsetPosition.Settop(AIndex : Integer; AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOmnitureSettings
  --------------------------------------------------------------------}


Procedure TOmnitureSettings.SetomnitureCostDataEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FomnitureCostDataEnabled=AValue) then exit;
  FomnitureCostDataEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOmnitureSettings.SetomnitureIntegrationEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FomnitureIntegrationEnabled=AValue) then exit;
  FomnitureIntegrationEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperatingSystem
  --------------------------------------------------------------------}


Procedure TOperatingSystem.SetdartId(AIndex : Integer; AValue : String); 

begin
  If (FdartId=AValue) then exit;
  FdartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystem.Setdesktop(AIndex : Integer; AValue : boolean); 

begin
  If (Fdesktop=AValue) then exit;
  Fdesktop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystem.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystem.Setmobile(AIndex : Integer; AValue : boolean); 

begin
  If (Fmobile=AValue) then exit;
  Fmobile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperatingSystemVersion
  --------------------------------------------------------------------}


Procedure TOperatingSystemVersion.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystemVersion.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystemVersion.SetmajorVersion(AIndex : Integer; AValue : String); 

begin
  If (FmajorVersion=AValue) then exit;
  FmajorVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystemVersion.SetminorVersion(AIndex : Integer; AValue : String); 

begin
  If (FminorVersion=AValue) then exit;
  FminorVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystemVersion.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystemVersion.SetoperatingSystem(AIndex : Integer; AValue : TOperatingSystem); 

begin
  If (FoperatingSystem=AValue) then exit;
  FoperatingSystem:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperatingSystemVersionsListResponse
  --------------------------------------------------------------------}


Procedure TOperatingSystemVersionsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystemVersionsListResponse.SetoperatingSystemVersions(AIndex : Integer; AValue : TOperatingSystemVersionsListResponseTypeoperatingSystemVersionsArray); 

begin
  If (FoperatingSystemVersions=AValue) then exit;
  FoperatingSystemVersions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperatingSystemsListResponse
  --------------------------------------------------------------------}


Procedure TOperatingSystemsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperatingSystemsListResponse.SetoperatingSystems(AIndex : Integer; AValue : TOperatingSystemsListResponseTypeoperatingSystemsArray); 

begin
  If (FoperatingSystems=AValue) then exit;
  FoperatingSystems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOptimizationActivity
  --------------------------------------------------------------------}


Procedure TOptimizationActivity.SetfloodlightActivityId(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityId=AValue) then exit;
  FfloodlightActivityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOptimizationActivity.SetfloodlightActivityIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FfloodlightActivityIdDimensionValue=AValue) then exit;
  FfloodlightActivityIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOptimizationActivity.Setweight(AIndex : Integer; AValue : integer); 

begin
  If (Fweight=AValue) then exit;
  Fweight:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOrder
  --------------------------------------------------------------------}


Procedure TOrder.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetapproverUserProfileIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FapproverUserProfileIds=AValue) then exit;
  FapproverUserProfileIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetbuyerInvoiceId(AIndex : Integer; AValue : String); 

begin
  If (FbuyerInvoiceId=AValue) then exit;
  FbuyerInvoiceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetbuyerOrganizationName(AIndex : Integer; AValue : String); 

begin
  If (FbuyerOrganizationName=AValue) then exit;
  FbuyerOrganizationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setcomments(AIndex : Integer; AValue : String); 

begin
  If (Fcomments=AValue) then exit;
  Fcomments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setcontacts(AIndex : Integer; AValue : TOrderTypecontactsArray); 

begin
  If (Fcontacts=AValue) then exit;
  Fcontacts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FlastModifiedInfo=AValue) then exit;
  FlastModifiedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetplanningTermId(AIndex : Integer; AValue : String); 

begin
  If (FplanningTermId=AValue) then exit;
  FplanningTermId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetsellerOrderId(AIndex : Integer; AValue : String); 

begin
  If (FsellerOrderId=AValue) then exit;
  FsellerOrderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetsellerOrganizationName(AIndex : Integer; AValue : String); 

begin
  If (FsellerOrganizationName=AValue) then exit;
  FsellerOrganizationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetsiteId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsiteId=AValue) then exit;
  FsiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetsiteNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsiteNames=AValue) then exit;
  FsiteNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SettermsAndConditions(AIndex : Integer; AValue : String); 

begin
  If (FtermsAndConditions=AValue) then exit;
  FtermsAndConditions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOrderContact
  --------------------------------------------------------------------}


Procedure TOrderContact.SetcontactInfo(AIndex : Integer; AValue : String); 

begin
  If (FcontactInfo=AValue) then exit;
  FcontactInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderContact.SetcontactName(AIndex : Integer; AValue : String); 

begin
  If (FcontactName=AValue) then exit;
  FcontactName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderContact.SetcontactTitle(AIndex : Integer; AValue : String); 

begin
  If (FcontactTitle=AValue) then exit;
  FcontactTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderContact.SetcontactType(AIndex : Integer; AValue : String); 

begin
  If (FcontactType=AValue) then exit;
  FcontactType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderContact.SetsignatureUserProfileId(AIndex : Integer; AValue : String); 

begin
  If (FsignatureUserProfileId=AValue) then exit;
  FsignatureUserProfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOrderDocument
  --------------------------------------------------------------------}


Procedure TOrderDocument.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.SetamendedOrderDocumentId(AIndex : Integer; AValue : String); 

begin
  If (FamendedOrderDocumentId=AValue) then exit;
  FamendedOrderDocumentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.SetapprovedByUserProfileIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FapprovedByUserProfileIds=AValue) then exit;
  FapprovedByUserProfileIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.Setcancelled(AIndex : Integer; AValue : boolean); 

begin
  If (Fcancelled=AValue) then exit;
  Fcancelled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.SetcreatedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FcreatedInfo=AValue) then exit;
  FcreatedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.SeteffectiveDate(AIndex : Integer; AValue : TDate); 

begin
  If (FeffectiveDate=AValue) then exit;
  FeffectiveDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.SetorderId(AIndex : Integer; AValue : String); 

begin
  If (ForderId=AValue) then exit;
  ForderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.Setsigned(AIndex : Integer; AValue : boolean); 

begin
  If (Fsigned=AValue) then exit;
  Fsigned:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocument.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TOrderDocument.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TOrderDocumentsListResponse
  --------------------------------------------------------------------}


Procedure TOrderDocumentsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocumentsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderDocumentsListResponse.SetorderDocuments(AIndex : Integer; AValue : TOrderDocumentsListResponseTypeorderDocumentsArray); 

begin
  If (ForderDocuments=AValue) then exit;
  ForderDocuments:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOrdersListResponse
  --------------------------------------------------------------------}


Procedure TOrdersListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrdersListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrdersListResponse.Setorders(AIndex : Integer; AValue : TOrdersListResponseTypeordersArray); 

begin
  If (Forders=AValue) then exit;
  Forders:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPathToConversionReportCompatibleFields
  --------------------------------------------------------------------}


Procedure TPathToConversionReportCompatibleFields.SetconversionDimensions(AIndex : Integer; AValue : TPathToConversionReportCompatibleFieldsTypeconversionDimensionsArray); 

begin
  If (FconversionDimensions=AValue) then exit;
  FconversionDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathToConversionReportCompatibleFields.SetcustomFloodlightVariables(AIndex : Integer; AValue : TPathToConversionReportCompatibleFieldsTypecustomFloodlightVariablesArray); 

begin
  If (FcustomFloodlightVariables=AValue) then exit;
  FcustomFloodlightVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathToConversionReportCompatibleFields.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathToConversionReportCompatibleFields.Setmetrics(AIndex : Integer; AValue : TPathToConversionReportCompatibleFieldsTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathToConversionReportCompatibleFields.SetperInteractionDimensions(AIndex : Integer; AValue : TPathToConversionReportCompatibleFieldsTypeperInteractionDimensionsArray); 

begin
  If (FperInteractionDimensions=AValue) then exit;
  FperInteractionDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacement
  --------------------------------------------------------------------}


Procedure TPlacement.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setarchived(AIndex : Integer; AValue : boolean); 

begin
  If (Farchived=AValue) then exit;
  Farchived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetcampaignId(AIndex : Integer; AValue : String); 

begin
  If (FcampaignId=AValue) then exit;
  FcampaignId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetcampaignIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FcampaignIdDimensionValue=AValue) then exit;
  FcampaignIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setcomment(AIndex : Integer; AValue : String); 

begin
  If (Fcomment=AValue) then exit;
  Fcomment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setcompatibility(AIndex : Integer; AValue : String); 

begin
  If (Fcompatibility=AValue) then exit;
  Fcompatibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetcontentCategoryId(AIndex : Integer; AValue : String); 

begin
  If (FcontentCategoryId=AValue) then exit;
  FcontentCategoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetcreateInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FcreateInfo=AValue) then exit;
  FcreateInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetdirectorySiteId(AIndex : Integer; AValue : String); 

begin
  If (FdirectorySiteId=AValue) then exit;
  FdirectorySiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetdirectorySiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FdirectorySiteIdDimensionValue=AValue) then exit;
  FdirectorySiteIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetexternalId(AIndex : Integer; AValue : String); 

begin
  If (FexternalId=AValue) then exit;
  FexternalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetkeyName(AIndex : Integer; AValue : String); 

begin
  If (FkeyName=AValue) then exit;
  FkeyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FlastModifiedInfo=AValue) then exit;
  FlastModifiedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); 

begin
  If (FlookbackConfiguration=AValue) then exit;
  FlookbackConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetpaymentApproved(AIndex : Integer; AValue : boolean); 

begin
  If (FpaymentApproved=AValue) then exit;
  FpaymentApproved:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetpaymentSource(AIndex : Integer; AValue : String); 

begin
  If (FpaymentSource=AValue) then exit;
  FpaymentSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetplacementGroupId(AIndex : Integer; AValue : String); 

begin
  If (FplacementGroupId=AValue) then exit;
  FplacementGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetplacementGroupIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FplacementGroupIdDimensionValue=AValue) then exit;
  FplacementGroupIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetplacementStrategyId(AIndex : Integer; AValue : String); 

begin
  If (FplacementStrategyId=AValue) then exit;
  FplacementStrategyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetpricingSchedule(AIndex : Integer; AValue : TPricingSchedule); 

begin
  If (FpricingSchedule=AValue) then exit;
  FpricingSchedule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setprimary(AIndex : Integer; AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetpublisherUpdateInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FpublisherUpdateInfo=AValue) then exit;
  FpublisherUpdateInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetsiteId(AIndex : Integer; AValue : String); 

begin
  If (FsiteId=AValue) then exit;
  FsiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetsiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FsiteIdDimensionValue=AValue) then exit;
  FsiteIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setsize(AIndex : Integer; AValue : TSize); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetsslRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FsslRequired=AValue) then exit;
  FsslRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SettagFormats(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtagFormats=AValue) then exit;
  FtagFormats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacement.SettagSetting(AIndex : Integer; AValue : TTagSetting); 

begin
  If (FtagSetting=AValue) then exit;
  FtagSetting:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacementAssignment
  --------------------------------------------------------------------}


Procedure TPlacementAssignment.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementAssignment.SetplacementId(AIndex : Integer; AValue : String); 

begin
  If (FplacementId=AValue) then exit;
  FplacementId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementAssignment.SetplacementIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FplacementIdDimensionValue=AValue) then exit;
  FplacementIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementAssignment.SetsslRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FsslRequired=AValue) then exit;
  FsslRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacementGroup
  --------------------------------------------------------------------}


Procedure TPlacementGroup.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.Setarchived(AIndex : Integer; AValue : boolean); 

begin
  If (Farchived=AValue) then exit;
  Farchived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetcampaignId(AIndex : Integer; AValue : String); 

begin
  If (FcampaignId=AValue) then exit;
  FcampaignId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetcampaignIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FcampaignIdDimensionValue=AValue) then exit;
  FcampaignIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetchildPlacementIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FchildPlacementIds=AValue) then exit;
  FchildPlacementIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.Setcomment(AIndex : Integer; AValue : String); 

begin
  If (Fcomment=AValue) then exit;
  Fcomment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetcontentCategoryId(AIndex : Integer; AValue : String); 

begin
  If (FcontentCategoryId=AValue) then exit;
  FcontentCategoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetcreateInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FcreateInfo=AValue) then exit;
  FcreateInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetdirectorySiteId(AIndex : Integer; AValue : String); 

begin
  If (FdirectorySiteId=AValue) then exit;
  FdirectorySiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetdirectorySiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FdirectorySiteIdDimensionValue=AValue) then exit;
  FdirectorySiteIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetexternalId(AIndex : Integer; AValue : String); 

begin
  If (FexternalId=AValue) then exit;
  FexternalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FlastModifiedInfo=AValue) then exit;
  FlastModifiedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetplacementGroupType(AIndex : Integer; AValue : String); 

begin
  If (FplacementGroupType=AValue) then exit;
  FplacementGroupType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetplacementStrategyId(AIndex : Integer; AValue : String); 

begin
  If (FplacementStrategyId=AValue) then exit;
  FplacementStrategyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetpricingSchedule(AIndex : Integer; AValue : TPricingSchedule); 

begin
  If (FpricingSchedule=AValue) then exit;
  FpricingSchedule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetprimaryPlacementId(AIndex : Integer; AValue : String); 

begin
  If (FprimaryPlacementId=AValue) then exit;
  FprimaryPlacementId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetprimaryPlacementIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FprimaryPlacementIdDimensionValue=AValue) then exit;
  FprimaryPlacementIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetprogrammaticSetting(AIndex : Integer; AValue : TProgrammaticSetting); 

begin
  If (FprogrammaticSetting=AValue) then exit;
  FprogrammaticSetting:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetsiteId(AIndex : Integer; AValue : String); 

begin
  If (FsiteId=AValue) then exit;
  FsiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetsiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FsiteIdDimensionValue=AValue) then exit;
  FsiteIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroup.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacementGroupsListResponse
  --------------------------------------------------------------------}


Procedure TPlacementGroupsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroupsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementGroupsListResponse.SetplacementGroups(AIndex : Integer; AValue : TPlacementGroupsListResponseTypeplacementGroupsArray); 

begin
  If (FplacementGroups=AValue) then exit;
  FplacementGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacementStrategiesListResponse
  --------------------------------------------------------------------}


Procedure TPlacementStrategiesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementStrategiesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementStrategiesListResponse.SetplacementStrategies(AIndex : Integer; AValue : TPlacementStrategiesListResponseTypeplacementStrategiesArray); 

begin
  If (FplacementStrategies=AValue) then exit;
  FplacementStrategies:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacementStrategy
  --------------------------------------------------------------------}


Procedure TPlacementStrategy.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementStrategy.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementStrategy.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementStrategy.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacementTag
  --------------------------------------------------------------------}


Procedure TPlacementTag.SetplacementId(AIndex : Integer; AValue : String); 

begin
  If (FplacementId=AValue) then exit;
  FplacementId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementTag.SettagDatas(AIndex : Integer; AValue : TPlacementTagTypetagDatasArray); 

begin
  If (FtagDatas=AValue) then exit;
  FtagDatas:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacementsGenerateTagsResponse
  --------------------------------------------------------------------}


Procedure TPlacementsGenerateTagsResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementsGenerateTagsResponse.SetplacementTags(AIndex : Integer; AValue : TPlacementsGenerateTagsResponseTypeplacementTagsArray); 

begin
  If (FplacementTags=AValue) then exit;
  FplacementTags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlacementsListResponse
  --------------------------------------------------------------------}


Procedure TPlacementsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlacementsListResponse.Setplacements(AIndex : Integer; AValue : TPlacementsListResponseTypeplacementsArray); 

begin
  If (Fplacements=AValue) then exit;
  Fplacements:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlatformType
  --------------------------------------------------------------------}


Procedure TPlatformType.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlatformType.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlatformType.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlatformTypesListResponse
  --------------------------------------------------------------------}


Procedure TPlatformTypesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlatformTypesListResponse.SetplatformTypes(AIndex : Integer; AValue : TPlatformTypesListResponseTypeplatformTypesArray); 

begin
  If (FplatformTypes=AValue) then exit;
  FplatformTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPopupWindowProperties
  --------------------------------------------------------------------}


Procedure TPopupWindowProperties.Setdimension(AIndex : Integer; AValue : TSize); 

begin
  If (Fdimension=AValue) then exit;
  Fdimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPopupWindowProperties.Setoffset(AIndex : Integer; AValue : TOffsetPosition); 

begin
  If (Foffset=AValue) then exit;
  Foffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPopupWindowProperties.SetpositionType(AIndex : Integer; AValue : String); 

begin
  If (FpositionType=AValue) then exit;
  FpositionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPopupWindowProperties.SetshowAddressBar(AIndex : Integer; AValue : boolean); 

begin
  If (FshowAddressBar=AValue) then exit;
  FshowAddressBar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPopupWindowProperties.SetshowMenuBar(AIndex : Integer; AValue : boolean); 

begin
  If (FshowMenuBar=AValue) then exit;
  FshowMenuBar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPopupWindowProperties.SetshowScrollBar(AIndex : Integer; AValue : boolean); 

begin
  If (FshowScrollBar=AValue) then exit;
  FshowScrollBar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPopupWindowProperties.SetshowStatusBar(AIndex : Integer; AValue : boolean); 

begin
  If (FshowStatusBar=AValue) then exit;
  FshowStatusBar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPopupWindowProperties.SetshowToolBar(AIndex : Integer; AValue : boolean); 

begin
  If (FshowToolBar=AValue) then exit;
  FshowToolBar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPopupWindowProperties.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostalCode
  --------------------------------------------------------------------}


Procedure TPostalCode.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostalCode.SetcountryCode(AIndex : Integer; AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostalCode.SetcountryDartId(AIndex : Integer; AValue : String); 

begin
  If (FcountryDartId=AValue) then exit;
  FcountryDartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostalCode.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostalCode.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostalCodesListResponse
  --------------------------------------------------------------------}


Procedure TPostalCodesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostalCodesListResponse.SetpostalCodes(AIndex : Integer; AValue : TPostalCodesListResponseTypepostalCodesArray); 

begin
  If (FpostalCodes=AValue) then exit;
  FpostalCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPricing
  --------------------------------------------------------------------}


Procedure TPricing.SetcapCostType(AIndex : Integer; AValue : String); 

begin
  If (FcapCostType=AValue) then exit;
  FcapCostType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricing.SetendDate(AIndex : Integer; AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricing.Setflights(AIndex : Integer; AValue : TPricingTypeflightsArray); 

begin
  If (Fflights=AValue) then exit;
  Fflights:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricing.SetgroupType(AIndex : Integer; AValue : String); 

begin
  If (FgroupType=AValue) then exit;
  FgroupType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricing.SetpricingType(AIndex : Integer; AValue : String); 

begin
  If (FpricingType=AValue) then exit;
  FpricingType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricing.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPricingSchedule
  --------------------------------------------------------------------}


Procedure TPricingSchedule.SetcapCostOption(AIndex : Integer; AValue : String); 

begin
  If (FcapCostOption=AValue) then exit;
  FcapCostOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedule.SetdisregardOverdelivery(AIndex : Integer; AValue : boolean); 

begin
  If (FdisregardOverdelivery=AValue) then exit;
  FdisregardOverdelivery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedule.SetendDate(AIndex : Integer; AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedule.Setflighted(AIndex : Integer; AValue : boolean); 

begin
  If (Fflighted=AValue) then exit;
  Fflighted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedule.SetfloodlightActivityId(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightActivityId=AValue) then exit;
  FfloodlightActivityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedule.SetpricingPeriods(AIndex : Integer; AValue : TPricingScheduleTypepricingPeriodsArray); 

begin
  If (FpricingPeriods=AValue) then exit;
  FpricingPeriods:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedule.SetpricingType(AIndex : Integer; AValue : String); 

begin
  If (FpricingType=AValue) then exit;
  FpricingType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedule.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedule.SettestingStartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FtestingStartDate=AValue) then exit;
  FtestingStartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPricingSchedulePricingPeriod
  --------------------------------------------------------------------}


Procedure TPricingSchedulePricingPeriod.SetendDate(AIndex : Integer; AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedulePricingPeriod.SetpricingComment(AIndex : Integer; AValue : String); 

begin
  If (FpricingComment=AValue) then exit;
  FpricingComment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedulePricingPeriod.SetrateOrCostNanos(AIndex : Integer; AValue : String); 

begin
  If (FrateOrCostNanos=AValue) then exit;
  FrateOrCostNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedulePricingPeriod.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingSchedulePricingPeriod.Setunits(AIndex : Integer; AValue : String); 

begin
  If (Funits=AValue) then exit;
  Funits:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProgrammaticSetting
  --------------------------------------------------------------------}


Procedure TProgrammaticSetting.SetadxDealIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FadxDealIds=AValue) then exit;
  FadxDealIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgrammaticSetting.SetinsertionOrderId(AIndex : Integer; AValue : String); 

begin
  If (FinsertionOrderId=AValue) then exit;
  FinsertionOrderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgrammaticSetting.SetinsertionOrderIdStatus(AIndex : Integer; AValue : boolean); 

begin
  If (FinsertionOrderIdStatus=AValue) then exit;
  FinsertionOrderIdStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgrammaticSetting.SetmediaCostNanos(AIndex : Integer; AValue : String); 

begin
  If (FmediaCostNanos=AValue) then exit;
  FmediaCostNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgrammaticSetting.Setprogrammatic(AIndex : Integer; AValue : boolean); 

begin
  If (Fprogrammatic=AValue) then exit;
  Fprogrammatic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgrammaticSetting.SettraffickerEmails(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtraffickerEmails=AValue) then exit;
  FtraffickerEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProject
  --------------------------------------------------------------------}


Procedure TProject.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetaudienceAgeGroup(AIndex : Integer; AValue : String); 

begin
  If (FaudienceAgeGroup=AValue) then exit;
  FaudienceAgeGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetaudienceGender(AIndex : Integer; AValue : String); 

begin
  If (FaudienceGender=AValue) then exit;
  FaudienceGender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setbudget(AIndex : Integer; AValue : String); 

begin
  If (Fbudget=AValue) then exit;
  Fbudget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetclientBillingCode(AIndex : Integer; AValue : String); 

begin
  If (FclientBillingCode=AValue) then exit;
  FclientBillingCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetclientName(AIndex : Integer; AValue : String); 

begin
  If (FclientName=AValue) then exit;
  FclientName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetendDate(AIndex : Integer; AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetlastModifiedInfo(AIndex : Integer; AValue : TLastModifiedInfo); 

begin
  If (FlastModifiedInfo=AValue) then exit;
  FlastModifiedInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setoverview(AIndex : Integer; AValue : String); 

begin
  If (Foverview=AValue) then exit;
  Foverview:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SettargetClicks(AIndex : Integer; AValue : String); 

begin
  If (FtargetClicks=AValue) then exit;
  FtargetClicks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SettargetConversions(AIndex : Integer; AValue : String); 

begin
  If (FtargetConversions=AValue) then exit;
  FtargetConversions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SettargetCpaNanos(AIndex : Integer; AValue : String); 

begin
  If (FtargetCpaNanos=AValue) then exit;
  FtargetCpaNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SettargetCpcNanos(AIndex : Integer; AValue : String); 

begin
  If (FtargetCpcNanos=AValue) then exit;
  FtargetCpcNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SettargetCpmNanos(AIndex : Integer; AValue : String); 

begin
  If (FtargetCpmNanos=AValue) then exit;
  FtargetCpmNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SettargetImpressions(AIndex : Integer; AValue : String); 

begin
  If (FtargetImpressions=AValue) then exit;
  FtargetImpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsListResponse
  --------------------------------------------------------------------}


Procedure TProjectsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectsListResponse.Setprojects(AIndex : Integer; AValue : TProjectsListResponseTypeprojectsArray); 

begin
  If (Fprojects=AValue) then exit;
  Fprojects:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReachReportCompatibleFields
  --------------------------------------------------------------------}


Procedure TReachReportCompatibleFields.SetdimensionFilters(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypedimensionFiltersArray); 

begin
  If (FdimensionFilters=AValue) then exit;
  FdimensionFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReachReportCompatibleFields.Setdimensions(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReachReportCompatibleFields.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReachReportCompatibleFields.Setmetrics(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReachReportCompatibleFields.SetpivotedActivityMetrics(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypepivotedActivityMetricsArray); 

begin
  If (FpivotedActivityMetrics=AValue) then exit;
  FpivotedActivityMetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReachReportCompatibleFields.SetreachByFrequencyMetrics(AIndex : Integer; AValue : TReachReportCompatibleFieldsTypereachByFrequencyMetricsArray); 

begin
  If (FreachByFrequencyMetrics=AValue) then exit;
  FreachByFrequencyMetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRecipient
  --------------------------------------------------------------------}


Procedure TRecipient.SetdeliveryType(AIndex : Integer; AValue : String); 

begin
  If (FdeliveryType=AValue) then exit;
  FdeliveryType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRecipient.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRecipient.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRegion
  --------------------------------------------------------------------}


Procedure TRegion.SetcountryCode(AIndex : Integer; AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.SetcountryDartId(AIndex : Integer; AValue : String); 

begin
  If (FcountryDartId=AValue) then exit;
  FcountryDartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.SetdartId(AIndex : Integer; AValue : String); 

begin
  If (FdartId=AValue) then exit;
  FdartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.SetregionCode(AIndex : Integer; AValue : String); 

begin
  If (FregionCode=AValue) then exit;
  FregionCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRegionsListResponse
  --------------------------------------------------------------------}


Procedure TRegionsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegionsListResponse.Setregions(AIndex : Integer; AValue : TRegionsListResponseTyperegionsArray); 

begin
  If (Fregions=AValue) then exit;
  Fregions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRemarketingList
  --------------------------------------------------------------------}


Procedure TRemarketingList.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.SetlifeSpan(AIndex : Integer; AValue : String); 

begin
  If (FlifeSpan=AValue) then exit;
  FlifeSpan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.SetlistPopulationRule(AIndex : Integer; AValue : TListPopulationRule); 

begin
  If (FlistPopulationRule=AValue) then exit;
  FlistPopulationRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.SetlistSize(AIndex : Integer; AValue : String); 

begin
  If (FlistSize=AValue) then exit;
  FlistSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.SetlistSource(AIndex : Integer; AValue : String); 

begin
  If (FlistSource=AValue) then exit;
  FlistSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingList.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRemarketingListShare
  --------------------------------------------------------------------}


Procedure TRemarketingListShare.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingListShare.SetremarketingListId(AIndex : Integer; AValue : String); 

begin
  If (FremarketingListId=AValue) then exit;
  FremarketingListId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingListShare.SetsharedAccountIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsharedAccountIds=AValue) then exit;
  FsharedAccountIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingListShare.SetsharedAdvertiserIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsharedAdvertiserIds=AValue) then exit;
  FsharedAdvertiserIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRemarketingListsListResponse
  --------------------------------------------------------------------}


Procedure TRemarketingListsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingListsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRemarketingListsListResponse.SetremarketingLists(AIndex : Integer; AValue : TRemarketingListsListResponseTyperemarketingListsArray); 

begin
  If (FremarketingLists=AValue) then exit;
  FremarketingLists:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypecriteria
  --------------------------------------------------------------------}


Procedure TReportTypecriteria.Setactivities(AIndex : Integer; AValue : TActivities); 

begin
  If (Factivities=AValue) then exit;
  Factivities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecriteria.SetcustomRichMediaEvents(AIndex : Integer; AValue : TCustomRichMediaEvents); 

begin
  If (FcustomRichMediaEvents=AValue) then exit;
  FcustomRichMediaEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecriteria.SetdateRange(AIndex : Integer; AValue : TDateRange); 

begin
  If (FdateRange=AValue) then exit;
  FdateRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecriteria.SetdimensionFilters(AIndex : Integer; AValue : TReportTypecriteriaTypedimensionFiltersArray); 

begin
  If (FdimensionFilters=AValue) then exit;
  FdimensionFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecriteria.Setdimensions(AIndex : Integer; AValue : TReportTypecriteriaTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecriteria.SetmetricNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmetricNames=AValue) then exit;
  FmetricNames:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypecrossDimensionReachCriteria
  --------------------------------------------------------------------}


Procedure TReportTypecrossDimensionReachCriteria.Setbreakdown(AIndex : Integer; AValue : TReportTypecrossDimensionReachCriteriaTypebreakdownArray); 

begin
  If (Fbreakdown=AValue) then exit;
  Fbreakdown:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecrossDimensionReachCriteria.SetdateRange(AIndex : Integer; AValue : TDateRange); 

begin
  If (FdateRange=AValue) then exit;
  FdateRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecrossDimensionReachCriteria.Setdimension(AIndex : Integer; AValue : String); 

begin
  If (Fdimension=AValue) then exit;
  Fdimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecrossDimensionReachCriteria.SetdimensionFilters(AIndex : Integer; AValue : TReportTypecrossDimensionReachCriteriaTypedimensionFiltersArray); 

begin
  If (FdimensionFilters=AValue) then exit;
  FdimensionFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecrossDimensionReachCriteria.SetmetricNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmetricNames=AValue) then exit;
  FmetricNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecrossDimensionReachCriteria.SetoverlapMetricNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FoverlapMetricNames=AValue) then exit;
  FoverlapMetricNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypecrossDimensionReachCriteria.Setpivoted(AIndex : Integer; AValue : boolean); 

begin
  If (Fpivoted=AValue) then exit;
  Fpivoted:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypedelivery
  --------------------------------------------------------------------}


Procedure TReportTypedelivery.SetemailOwner(AIndex : Integer; AValue : boolean); 

begin
  If (FemailOwner=AValue) then exit;
  FemailOwner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypedelivery.SetemailOwnerDeliveryType(AIndex : Integer; AValue : String); 

begin
  If (FemailOwnerDeliveryType=AValue) then exit;
  FemailOwnerDeliveryType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypedelivery.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypedelivery.Setrecipients(AIndex : Integer; AValue : TReportTypedeliveryTyperecipientsArray); 

begin
  If (Frecipients=AValue) then exit;
  Frecipients:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypefloodlightCriteriaTypereportProperties
  --------------------------------------------------------------------}


Procedure TReportTypefloodlightCriteriaTypereportProperties.SetincludeAttributedIPConversions(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeAttributedIPConversions=AValue) then exit;
  FincludeAttributedIPConversions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefloodlightCriteriaTypereportProperties.SetincludeUnattributedCookieConversions(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeUnattributedCookieConversions=AValue) then exit;
  FincludeUnattributedCookieConversions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefloodlightCriteriaTypereportProperties.SetincludeUnattributedIPConversions(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeUnattributedIPConversions=AValue) then exit;
  FincludeUnattributedIPConversions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypefloodlightCriteria
  --------------------------------------------------------------------}


Procedure TReportTypefloodlightCriteria.SetcustomRichMediaEvents(AIndex : Integer; AValue : TReportTypefloodlightCriteriaTypecustomRichMediaEventsArray); 

begin
  If (FcustomRichMediaEvents=AValue) then exit;
  FcustomRichMediaEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefloodlightCriteria.SetdateRange(AIndex : Integer; AValue : TDateRange); 

begin
  If (FdateRange=AValue) then exit;
  FdateRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefloodlightCriteria.SetdimensionFilters(AIndex : Integer; AValue : TReportTypefloodlightCriteriaTypedimensionFiltersArray); 

begin
  If (FdimensionFilters=AValue) then exit;
  FdimensionFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefloodlightCriteria.Setdimensions(AIndex : Integer; AValue : TReportTypefloodlightCriteriaTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefloodlightCriteria.SetfloodlightConfigId(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FfloodlightConfigId=AValue) then exit;
  FfloodlightConfigId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefloodlightCriteria.SetmetricNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmetricNames=AValue) then exit;
  FmetricNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefloodlightCriteria.SetreportProperties(AIndex : Integer; AValue : TReportTypefloodlightCriteriaTypereportProperties); 

begin
  If (FreportProperties=AValue) then exit;
  FreportProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypepathToConversionCriteriaTypereportProperties
  --------------------------------------------------------------------}


Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetclicksLookbackWindow(AIndex : Integer; AValue : integer); 

begin
  If (FclicksLookbackWindow=AValue) then exit;
  FclicksLookbackWindow:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetimpressionsLookbackWindow(AIndex : Integer; AValue : integer); 

begin
  If (FimpressionsLookbackWindow=AValue) then exit;
  FimpressionsLookbackWindow:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetincludeAttributedIPConversions(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeAttributedIPConversions=AValue) then exit;
  FincludeAttributedIPConversions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetincludeUnattributedCookieConversions(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeUnattributedCookieConversions=AValue) then exit;
  FincludeUnattributedCookieConversions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetincludeUnattributedIPConversions(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeUnattributedIPConversions=AValue) then exit;
  FincludeUnattributedIPConversions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetmaximumClickInteractions(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumClickInteractions=AValue) then exit;
  FmaximumClickInteractions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetmaximumImpressionInteractions(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumImpressionInteractions=AValue) then exit;
  FmaximumImpressionInteractions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetmaximumInteractionGap(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumInteractionGap=AValue) then exit;
  FmaximumInteractionGap:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteriaTypereportProperties.SetpivotOnInteractionPath(AIndex : Integer; AValue : boolean); 

begin
  If (FpivotOnInteractionPath=AValue) then exit;
  FpivotOnInteractionPath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypepathToConversionCriteria
  --------------------------------------------------------------------}


Procedure TReportTypepathToConversionCriteria.SetactivityFilters(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypeactivityFiltersArray); 

begin
  If (FactivityFilters=AValue) then exit;
  FactivityFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteria.SetconversionDimensions(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypeconversionDimensionsArray); 

begin
  If (FconversionDimensions=AValue) then exit;
  FconversionDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteria.SetcustomFloodlightVariables(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypecustomFloodlightVariablesArray); 

begin
  If (FcustomFloodlightVariables=AValue) then exit;
  FcustomFloodlightVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteria.SetcustomRichMediaEvents(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypecustomRichMediaEventsArray); 

begin
  If (FcustomRichMediaEvents=AValue) then exit;
  FcustomRichMediaEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteria.SetdateRange(AIndex : Integer; AValue : TDateRange); 

begin
  If (FdateRange=AValue) then exit;
  FdateRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteria.SetfloodlightConfigId(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FfloodlightConfigId=AValue) then exit;
  FfloodlightConfigId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteria.SetmetricNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmetricNames=AValue) then exit;
  FmetricNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteria.SetperInteractionDimensions(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypeperInteractionDimensionsArray); 

begin
  If (FperInteractionDimensions=AValue) then exit;
  FperInteractionDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypepathToConversionCriteria.SetreportProperties(AIndex : Integer; AValue : TReportTypepathToConversionCriteriaTypereportProperties); 

begin
  If (FreportProperties=AValue) then exit;
  FreportProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypereachCriteria
  --------------------------------------------------------------------}


Procedure TReportTypereachCriteria.Setactivities(AIndex : Integer; AValue : TActivities); 

begin
  If (Factivities=AValue) then exit;
  Factivities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypereachCriteria.SetcustomRichMediaEvents(AIndex : Integer; AValue : TCustomRichMediaEvents); 

begin
  If (FcustomRichMediaEvents=AValue) then exit;
  FcustomRichMediaEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypereachCriteria.SetdateRange(AIndex : Integer; AValue : TDateRange); 

begin
  If (FdateRange=AValue) then exit;
  FdateRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypereachCriteria.SetdimensionFilters(AIndex : Integer; AValue : TReportTypereachCriteriaTypedimensionFiltersArray); 

begin
  If (FdimensionFilters=AValue) then exit;
  FdimensionFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypereachCriteria.Setdimensions(AIndex : Integer; AValue : TReportTypereachCriteriaTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypereachCriteria.SetenableAllDimensionCombinations(AIndex : Integer; AValue : boolean); 

begin
  If (FenableAllDimensionCombinations=AValue) then exit;
  FenableAllDimensionCombinations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypereachCriteria.SetmetricNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmetricNames=AValue) then exit;
  FmetricNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypereachCriteria.SetreachByFrequencyMetricNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FreachByFrequencyMetricNames=AValue) then exit;
  FreachByFrequencyMetricNames:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypeschedule
  --------------------------------------------------------------------}


Procedure TReportTypeschedule.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypeschedule.Setevery(AIndex : Integer; AValue : integer); 

begin
  If (Fevery=AValue) then exit;
  Fevery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypeschedule.SetexpirationDate(AIndex : Integer; AValue : TDate); 

begin
  If (FexpirationDate=AValue) then exit;
  FexpirationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypeschedule.Setrepeats(AIndex : Integer; AValue : String); 

begin
  If (Frepeats=AValue) then exit;
  Frepeats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypeschedule.SetrepeatsOnWeekDays(AIndex : Integer; AValue : TStringArray); 

begin
  If (FrepeatsOnWeekDays=AValue) then exit;
  FrepeatsOnWeekDays:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypeschedule.SetrunsOnDayOfMonth(AIndex : Integer; AValue : String); 

begin
  If (FrunsOnDayOfMonth=AValue) then exit;
  FrunsOnDayOfMonth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypeschedule.SetstartDate(AIndex : Integer; AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setcriteria(AIndex : Integer; AValue : TReportTypecriteria); 

begin
  If (Fcriteria=AValue) then exit;
  Fcriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetcrossDimensionReachCriteria(AIndex : Integer; AValue : TReportTypecrossDimensionReachCriteria); 

begin
  If (FcrossDimensionReachCriteria=AValue) then exit;
  FcrossDimensionReachCriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setdelivery(AIndex : Integer; AValue : TReportTypedelivery); 

begin
  If (Fdelivery=AValue) then exit;
  Fdelivery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetfileName(AIndex : Integer; AValue : String); 

begin
  If (FfileName=AValue) then exit;
  FfileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetfloodlightCriteria(AIndex : Integer; AValue : TReportTypefloodlightCriteria); 

begin
  If (FfloodlightCriteria=AValue) then exit;
  FfloodlightCriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetlastModifiedTime(AIndex : Integer; AValue : String); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetownerProfileId(AIndex : Integer; AValue : String); 

begin
  If (FownerProfileId=AValue) then exit;
  FownerProfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetpathToConversionCriteria(AIndex : Integer; AValue : TReportTypepathToConversionCriteria); 

begin
  If (FpathToConversionCriteria=AValue) then exit;
  FpathToConversionCriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetreachCriteria(AIndex : Integer; AValue : TReportTypereachCriteria); 

begin
  If (FreachCriteria=AValue) then exit;
  FreachCriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setschedule(AIndex : Integer; AValue : TReportTypeschedule); 

begin
  If (Fschedule=AValue) then exit;
  Fschedule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetsubAccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubAccountId=AValue) then exit;
  FsubAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TReport.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TReportCompatibleFields
  --------------------------------------------------------------------}


Procedure TReportCompatibleFields.SetdimensionFilters(AIndex : Integer; AValue : TReportCompatibleFieldsTypedimensionFiltersArray); 

begin
  If (FdimensionFilters=AValue) then exit;
  FdimensionFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportCompatibleFields.Setdimensions(AIndex : Integer; AValue : TReportCompatibleFieldsTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportCompatibleFields.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportCompatibleFields.Setmetrics(AIndex : Integer; AValue : TReportCompatibleFieldsTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportCompatibleFields.SetpivotedActivityMetrics(AIndex : Integer; AValue : TReportCompatibleFieldsTypepivotedActivityMetricsArray); 

begin
  If (FpivotedActivityMetrics=AValue) then exit;
  FpivotedActivityMetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportList
  --------------------------------------------------------------------}


Procedure TReportList.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportList.Setitems(AIndex : Integer; AValue : TReportListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportsConfiguration
  --------------------------------------------------------------------}


Procedure TReportsConfiguration.SetexposureToConversionEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FexposureToConversionEnabled=AValue) then exit;
  FexposureToConversionEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportsConfiguration.SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); 

begin
  If (FlookbackConfiguration=AValue) then exit;
  FlookbackConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportsConfiguration.SetreportGenerationTimeZoneId(AIndex : Integer; AValue : String); 

begin
  If (FreportGenerationTimeZoneId=AValue) then exit;
  FreportGenerationTimeZoneId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRichMediaExitOverride
  --------------------------------------------------------------------}


Procedure TRichMediaExitOverride.SetcustomExitUrl(AIndex : Integer; AValue : String); 

begin
  If (FcustomExitUrl=AValue) then exit;
  FcustomExitUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRichMediaExitOverride.SetexitId(AIndex : Integer; AValue : String); 

begin
  If (FexitId=AValue) then exit;
  FexitId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRichMediaExitOverride.SetuseCustomExitUrl(AIndex : Integer; AValue : boolean); 

begin
  If (FuseCustomExitUrl=AValue) then exit;
  FuseCustomExitUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSite
  --------------------------------------------------------------------}


Procedure TSite.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.Setapproved(AIndex : Integer; AValue : boolean); 

begin
  If (Fapproved=AValue) then exit;
  Fapproved:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.SetdirectorySiteId(AIndex : Integer; AValue : String); 

begin
  If (FdirectorySiteId=AValue) then exit;
  FdirectorySiteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.SetdirectorySiteIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FdirectorySiteIdDimensionValue=AValue) then exit;
  FdirectorySiteIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.SetidDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FidDimensionValue=AValue) then exit;
  FidDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.SetkeyName(AIndex : Integer; AValue : String); 

begin
  If (FkeyName=AValue) then exit;
  FkeyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.SetsiteContacts(AIndex : Integer; AValue : TSiteTypesiteContactsArray); 

begin
  If (FsiteContacts=AValue) then exit;
  FsiteContacts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.SetsiteSettings(AIndex : Integer; AValue : TSiteSettings); 

begin
  If (FsiteSettings=AValue) then exit;
  FsiteSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSite.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSiteContact
  --------------------------------------------------------------------}


Procedure TSiteContact.Setaddress(AIndex : Integer; AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteContact.SetcontactType(AIndex : Integer; AValue : String); 

begin
  If (FcontactType=AValue) then exit;
  FcontactType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteContact.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteContact.SetfirstName(AIndex : Integer; AValue : String); 

begin
  If (FfirstName=AValue) then exit;
  FfirstName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteContact.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteContact.SetlastName(AIndex : Integer; AValue : String); 

begin
  If (FlastName=AValue) then exit;
  FlastName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteContact.Setphone(AIndex : Integer; AValue : String); 

begin
  If (Fphone=AValue) then exit;
  Fphone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteContact.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSiteSettings
  --------------------------------------------------------------------}


Procedure TSiteSettings.SetactiveViewOptOut(AIndex : Integer; AValue : boolean); 

begin
  If (FactiveViewOptOut=AValue) then exit;
  FactiveViewOptOut:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteSettings.SetcreativeSettings(AIndex : Integer; AValue : TCreativeSettings); 

begin
  If (FcreativeSettings=AValue) then exit;
  FcreativeSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteSettings.SetdisableBrandSafeAds(AIndex : Integer; AValue : boolean); 

begin
  If (FdisableBrandSafeAds=AValue) then exit;
  FdisableBrandSafeAds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteSettings.SetdisableNewCookie(AIndex : Integer; AValue : boolean); 

begin
  If (FdisableNewCookie=AValue) then exit;
  FdisableNewCookie:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteSettings.SetlookbackConfiguration(AIndex : Integer; AValue : TLookbackConfiguration); 

begin
  If (FlookbackConfiguration=AValue) then exit;
  FlookbackConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteSettings.SettagSetting(AIndex : Integer; AValue : TTagSetting); 

begin
  If (FtagSetting=AValue) then exit;
  FtagSetting:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSitesListResponse
  --------------------------------------------------------------------}


Procedure TSitesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSitesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSitesListResponse.Setsites(AIndex : Integer; AValue : TSitesListResponseTypesitesArray); 

begin
  If (Fsites=AValue) then exit;
  Fsites:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSize
  --------------------------------------------------------------------}


Procedure TSize.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSize.Setiab(AIndex : Integer; AValue : boolean); 

begin
  If (Fiab=AValue) then exit;
  Fiab:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSize.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSize.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSize.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSizesListResponse
  --------------------------------------------------------------------}


Procedure TSizesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSizesListResponse.Setsizes(AIndex : Integer; AValue : TSizesListResponseTypesizesArray); 

begin
  If (Fsizes=AValue) then exit;
  Fsizes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSortedDimension
  --------------------------------------------------------------------}


Procedure TSortedDimension.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSortedDimension.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSortedDimension.SetsortOrder(AIndex : Integer; AValue : String); 

begin
  If (FsortOrder=AValue) then exit;
  FsortOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubaccount
  --------------------------------------------------------------------}


Procedure TSubaccount.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubaccount.SetavailablePermissionIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FavailablePermissionIds=AValue) then exit;
  FavailablePermissionIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubaccount.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubaccount.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubaccount.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubaccountsListResponse
  --------------------------------------------------------------------}


Procedure TSubaccountsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubaccountsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubaccountsListResponse.Setsubaccounts(AIndex : Integer; AValue : TSubaccountsListResponseTypesubaccountsArray); 

begin
  If (Fsubaccounts=AValue) then exit;
  Fsubaccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTagData
  --------------------------------------------------------------------}


Procedure TTagData.SetadId(AIndex : Integer; AValue : String); 

begin
  If (FadId=AValue) then exit;
  FadId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagData.SetclickTag(AIndex : Integer; AValue : String); 

begin
  If (FclickTag=AValue) then exit;
  FclickTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagData.SetcreativeId(AIndex : Integer; AValue : String); 

begin
  If (FcreativeId=AValue) then exit;
  FcreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagData.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagData.SetimpressionTag(AIndex : Integer; AValue : String); 

begin
  If (FimpressionTag=AValue) then exit;
  FimpressionTag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTagSetting
  --------------------------------------------------------------------}


Procedure TTagSetting.SetadditionalKeyValues(AIndex : Integer; AValue : String); 

begin
  If (FadditionalKeyValues=AValue) then exit;
  FadditionalKeyValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagSetting.SetincludeClickThroughUrls(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeClickThroughUrls=AValue) then exit;
  FincludeClickThroughUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagSetting.SetincludeClickTracking(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeClickTracking=AValue) then exit;
  FincludeClickTracking:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagSetting.SetkeywordOption(AIndex : Integer; AValue : String); 

begin
  If (FkeywordOption=AValue) then exit;
  FkeywordOption:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTagSettings
  --------------------------------------------------------------------}


Procedure TTagSettings.SetdynamicTagEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FdynamicTagEnabled=AValue) then exit;
  FdynamicTagEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagSettings.SetimageTagEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FimageTagEnabled=AValue) then exit;
  FimageTagEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetWindow
  --------------------------------------------------------------------}


Procedure TTargetWindow.SetcustomHtml(AIndex : Integer; AValue : String); 

begin
  If (FcustomHtml=AValue) then exit;
  FcustomHtml:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetWindow.SettargetWindowOption(AIndex : Integer; AValue : String); 

begin
  If (FtargetWindowOption=AValue) then exit;
  FtargetWindowOption:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetableRemarketingList
  --------------------------------------------------------------------}


Procedure TTargetableRemarketingList.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.SetadvertiserIdDimensionValue(AIndex : Integer; AValue : TDimensionValue); 

begin
  If (FadvertiserIdDimensionValue=AValue) then exit;
  FadvertiserIdDimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.SetlifeSpan(AIndex : Integer; AValue : String); 

begin
  If (FlifeSpan=AValue) then exit;
  FlifeSpan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.SetlistSize(AIndex : Integer; AValue : String); 

begin
  If (FlistSize=AValue) then exit;
  FlistSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.SetlistSource(AIndex : Integer; AValue : String); 

begin
  If (FlistSource=AValue) then exit;
  FlistSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingList.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetableRemarketingListsListResponse
  --------------------------------------------------------------------}


Procedure TTargetableRemarketingListsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingListsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetableRemarketingListsListResponse.SettargetableRemarketingLists(AIndex : Integer; AValue : TTargetableRemarketingListsListResponseTypetargetableRemarketingListsArray); 

begin
  If (FtargetableRemarketingLists=AValue) then exit;
  FtargetableRemarketingLists:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTechnologyTargeting
  --------------------------------------------------------------------}


Procedure TTechnologyTargeting.Setbrowsers(AIndex : Integer; AValue : TTechnologyTargetingTypebrowsersArray); 

begin
  If (Fbrowsers=AValue) then exit;
  Fbrowsers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTechnologyTargeting.SetconnectionTypes(AIndex : Integer; AValue : TTechnologyTargetingTypeconnectionTypesArray); 

begin
  If (FconnectionTypes=AValue) then exit;
  FconnectionTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTechnologyTargeting.SetmobileCarriers(AIndex : Integer; AValue : TTechnologyTargetingTypemobileCarriersArray); 

begin
  If (FmobileCarriers=AValue) then exit;
  FmobileCarriers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTechnologyTargeting.SetoperatingSystemVersions(AIndex : Integer; AValue : TTechnologyTargetingTypeoperatingSystemVersionsArray); 

begin
  If (FoperatingSystemVersions=AValue) then exit;
  FoperatingSystemVersions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTechnologyTargeting.SetoperatingSystems(AIndex : Integer; AValue : TTechnologyTargetingTypeoperatingSystemsArray); 

begin
  If (FoperatingSystems=AValue) then exit;
  FoperatingSystems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTechnologyTargeting.SetplatformTypes(AIndex : Integer; AValue : TTechnologyTargetingTypeplatformTypesArray); 

begin
  If (FplatformTypes=AValue) then exit;
  FplatformTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThirdPartyTrackingUrl
  --------------------------------------------------------------------}


Procedure TThirdPartyTrackingUrl.SetthirdPartyUrlType(AIndex : Integer; AValue : String); 

begin
  If (FthirdPartyUrlType=AValue) then exit;
  FthirdPartyUrlType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThirdPartyTrackingUrl.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserDefinedVariableConfiguration
  --------------------------------------------------------------------}


Procedure TUserDefinedVariableConfiguration.SetdataType(AIndex : Integer; AValue : String); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserDefinedVariableConfiguration.SetreportName(AIndex : Integer; AValue : String); 

begin
  If (FreportName=AValue) then exit;
  FreportName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserDefinedVariableConfiguration.SetvariableType(AIndex : Integer; AValue : String); 

begin
  If (FvariableType=AValue) then exit;
  FvariableType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserProfile
  --------------------------------------------------------------------}


Procedure TUserProfile.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.SetaccountName(AIndex : Integer; AValue : String); 

begin
  If (FaccountName=AValue) then exit;
  FaccountName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.SetprofileId(AIndex : Integer; AValue : String); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.SetsubAccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubAccountId=AValue) then exit;
  FsubAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.SetsubAccountName(AIndex : Integer; AValue : String); 

begin
  If (FsubAccountName=AValue) then exit;
  FsubAccountName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.SetuserName(AIndex : Integer; AValue : String); 

begin
  If (FuserName=AValue) then exit;
  FuserName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserProfileList
  --------------------------------------------------------------------}


Procedure TUserProfileList.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfileList.Setitems(AIndex : Integer; AValue : TUserProfileListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfileList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserRole
  --------------------------------------------------------------------}


Procedure TUserRole.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRole.SetdefaultUserRole(AIndex : Integer; AValue : boolean); 

begin
  If (FdefaultUserRole=AValue) then exit;
  FdefaultUserRole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRole.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRole.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRole.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRole.SetparentUserRoleId(AIndex : Integer; AValue : String); 

begin
  If (FparentUserRoleId=AValue) then exit;
  FparentUserRoleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRole.Setpermissions(AIndex : Integer; AValue : TUserRoleTypepermissionsArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRole.SetsubaccountId(AIndex : Integer; AValue : String); 

begin
  If (FsubaccountId=AValue) then exit;
  FsubaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserRolePermission
  --------------------------------------------------------------------}


Procedure TUserRolePermission.Setavailability(AIndex : Integer; AValue : String); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolePermission.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolePermission.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolePermission.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolePermission.SetpermissionGroupId(AIndex : Integer; AValue : String); 

begin
  If (FpermissionGroupId=AValue) then exit;
  FpermissionGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserRolePermissionGroup
  --------------------------------------------------------------------}


Procedure TUserRolePermissionGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolePermissionGroup.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolePermissionGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserRolePermissionGroupsListResponse
  --------------------------------------------------------------------}


Procedure TUserRolePermissionGroupsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolePermissionGroupsListResponse.SetuserRolePermissionGroups(AIndex : Integer; AValue : TUserRolePermissionGroupsListResponseTypeuserRolePermissionGroupsArray); 

begin
  If (FuserRolePermissionGroups=AValue) then exit;
  FuserRolePermissionGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserRolePermissionsListResponse
  --------------------------------------------------------------------}


Procedure TUserRolePermissionsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolePermissionsListResponse.SetuserRolePermissions(AIndex : Integer; AValue : TUserRolePermissionsListResponseTypeuserRolePermissionsArray); 

begin
  If (FuserRolePermissions=AValue) then exit;
  FuserRolePermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserRolesListResponse
  --------------------------------------------------------------------}


Procedure TUserRolesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRolesListResponse.SetuserRoles(AIndex : Integer; AValue : TUserRolesListResponseTypeuserRolesArray); 

begin
  If (FuserRoles=AValue) then exit;
  FuserRoles:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountActiveAdSummariesResource
  --------------------------------------------------------------------}


Class Function TAccountActiveAdSummariesResource.ResourceName : String;

begin
  Result:='accountActiveAdSummaries';
end;

Class Function TAccountActiveAdSummariesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TAccountActiveAdSummariesResource.Get(profileId: string; summaryAccountId: string) : TAccountActiveAdSummary;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accountActiveAdSummaries/{summaryAccountId}';
  _Methodid   = 'dfareporting.accountActiveAdSummaries.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'summaryAccountId',summaryAccountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountActiveAdSummary) as TAccountActiveAdSummary;
end;



{ --------------------------------------------------------------------
  TAccountPermissionGroupsResource
  --------------------------------------------------------------------}


Class Function TAccountPermissionGroupsResource.ResourceName : String;

begin
  Result:='accountPermissionGroups';
end;

Class Function TAccountPermissionGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TAccountPermissionGroupsResource.Get(id: string; profileId: string) : TAccountPermissionGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accountPermissionGroups/{id}';
  _Methodid   = 'dfareporting.accountPermissionGroups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountPermissionGroup) as TAccountPermissionGroup;
end;

Function TAccountPermissionGroupsResource.List(profileId: string) : TAccountPermissionGroupsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accountPermissionGroups';
  _Methodid   = 'dfareporting.accountPermissionGroups.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountPermissionGroupsListResponse) as TAccountPermissionGroupsListResponse;
end;



{ --------------------------------------------------------------------
  TAccountPermissionsResource
  --------------------------------------------------------------------}


Class Function TAccountPermissionsResource.ResourceName : String;

begin
  Result:='accountPermissions';
end;

Class Function TAccountPermissionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TAccountPermissionsResource.Get(id: string; profileId: string) : TAccountPermission;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accountPermissions/{id}';
  _Methodid   = 'dfareporting.accountPermissions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountPermission) as TAccountPermission;
end;

Function TAccountPermissionsResource.List(profileId: string) : TAccountPermissionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accountPermissions';
  _Methodid   = 'dfareporting.accountPermissions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountPermissionsListResponse) as TAccountPermissionsListResponse;
end;



{ --------------------------------------------------------------------
  TAccountUserProfilesResource
  --------------------------------------------------------------------}


Class Function TAccountUserProfilesResource.ResourceName : String;

begin
  Result:='accountUserProfiles';
end;

Class Function TAccountUserProfilesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TAccountUserProfilesResource.Get(id: string; profileId: string) : TAccountUserProfile;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accountUserProfiles/{id}';
  _Methodid   = 'dfareporting.accountUserProfiles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountUserProfile) as TAccountUserProfile;
end;

Function TAccountUserProfilesResource.Insert(profileId: string; aAccountUserProfile : TAccountUserProfile) : TAccountUserProfile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/accountUserProfiles';
  _Methodid   = 'dfareporting.accountUserProfiles.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccountUserProfile,TAccountUserProfile) as TAccountUserProfile;
end;

Function TAccountUserProfilesResource.List(profileId: string; AQuery : string = '') : TAccountUserProfilesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accountUserProfiles';
  _Methodid   = 'dfareporting.accountUserProfiles.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAccountUserProfilesListResponse) as TAccountUserProfilesListResponse;
end;


Function TAccountUserProfilesResource.List(profileId: string; AQuery : TAccountUserProfileslistOptions) : TAccountUserProfilesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'active',AQuery.active);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'subaccountId',AQuery.subaccountId);
  AddToQuery(_Q,'userRoleId',AQuery.userRoleId);
  Result:=List(profileId,_Q);
end;

Function TAccountUserProfilesResource.Patch(profileId: string; aAccountUserProfile : TAccountUserProfile; AQuery : string = '') : TAccountUserProfile;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/accountUserProfiles';
  _Methodid   = 'dfareporting.accountUserProfiles.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAccountUserProfile,TAccountUserProfile) as TAccountUserProfile;
end;


Function TAccountUserProfilesResource.Patch(profileId: string; aAccountUserProfile : TAccountUserProfile; AQuery : TAccountUserProfilespatchOptions) : TAccountUserProfile;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aAccountUserProfile,_Q);
end;

Function TAccountUserProfilesResource.Update(profileId: string; aAccountUserProfile : TAccountUserProfile) : TAccountUserProfile;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/accountUserProfiles';
  _Methodid   = 'dfareporting.accountUserProfiles.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccountUserProfile,TAccountUserProfile) as TAccountUserProfile;
end;



{ --------------------------------------------------------------------
  TAccountsResource
  --------------------------------------------------------------------}


Class Function TAccountsResource.ResourceName : String;

begin
  Result:='accounts';
end;

Class Function TAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TAccountsResource.Get(id: string; profileId: string) : TAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accounts/{id}';
  _Methodid   = 'dfareporting.accounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccount) as TAccount;
end;

Function TAccountsResource.List(profileId: string; AQuery : string = '') : TAccountsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/accounts';
  _Methodid   = 'dfareporting.accounts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAccountsListResponse) as TAccountsListResponse;
end;


Function TAccountsResource.List(profileId: string; AQuery : TAccountslistOptions) : TAccountsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'active',AQuery.active);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TAccountsResource.Patch(profileId: string; aAccount : TAccount; AQuery : string = '') : TAccount;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/accounts';
  _Methodid   = 'dfareporting.accounts.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAccount,TAccount) as TAccount;
end;


Function TAccountsResource.Patch(profileId: string; aAccount : TAccount; AQuery : TAccountspatchOptions) : TAccount;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aAccount,_Q);
end;

Function TAccountsResource.Update(profileId: string; aAccount : TAccount) : TAccount;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/accounts';
  _Methodid   = 'dfareporting.accounts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccount,TAccount) as TAccount;
end;



{ --------------------------------------------------------------------
  TAdsResource
  --------------------------------------------------------------------}


Class Function TAdsResource.ResourceName : String;

begin
  Result:='ads';
end;

Class Function TAdsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TAdsResource.Get(id: string; profileId: string) : TAd;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/ads/{id}';
  _Methodid   = 'dfareporting.ads.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAd) as TAd;
end;

Function TAdsResource.Insert(profileId: string; aAd : TAd) : TAd;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/ads';
  _Methodid   = 'dfareporting.ads.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAd,TAd) as TAd;
end;

Function TAdsResource.List(profileId: string; AQuery : string = '') : TAdsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/ads';
  _Methodid   = 'dfareporting.ads.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdsListResponse) as TAdsListResponse;
end;


Function TAdsResource.List(profileId: string; AQuery : TAdslistOptions) : TAdsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'active',AQuery.active);
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'archived',AQuery.archived);
  AddToQuery(_Q,'audienceSegmentIds',AQuery.audienceSegmentIds);
  AddToQuery(_Q,'campaignIds',AQuery.campaignIds);
  AddToQuery(_Q,'compatibility',AQuery.compatibility);
  AddToQuery(_Q,'creativeIds',AQuery.creativeIds);
  AddToQuery(_Q,'creativeOptimizationConfigurationIds',AQuery.creativeOptimizationConfigurationIds);
  AddToQuery(_Q,'creativeType',AQuery.creativeType);
  AddToQuery(_Q,'dynamicClickTracker',AQuery.dynamicClickTracker);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'landingPageIds',AQuery.landingPageIds);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'overriddenEventTagId',AQuery.overriddenEventTagId);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'placementIds',AQuery.placementIds);
  AddToQuery(_Q,'remarketingListIds',AQuery.remarketingListIds);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sizeIds',AQuery.sizeIds);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'sslCompliant',AQuery.sslCompliant);
  AddToQuery(_Q,'sslRequired',AQuery.sslRequired);
  AddToQuery(_Q,'type',AQuery._type);
  Result:=List(profileId,_Q);
end;

Function TAdsResource.Patch(profileId: string; aAd : TAd; AQuery : string = '') : TAd;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/ads';
  _Methodid   = 'dfareporting.ads.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAd,TAd) as TAd;
end;


Function TAdsResource.Patch(profileId: string; aAd : TAd; AQuery : TAdspatchOptions) : TAd;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aAd,_Q);
end;

Function TAdsResource.Update(profileId: string; aAd : TAd) : TAd;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/ads';
  _Methodid   = 'dfareporting.ads.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAd,TAd) as TAd;
end;



{ --------------------------------------------------------------------
  TAdvertiserGroupsResource
  --------------------------------------------------------------------}


Class Function TAdvertiserGroupsResource.ResourceName : String;

begin
  Result:='advertiserGroups';
end;

Class Function TAdvertiserGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TAdvertiserGroupsResource.Delete(id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/advertiserGroups/{id}';
  _Methodid   = 'dfareporting.advertiserGroups.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAdvertiserGroupsResource.Get(id: string; profileId: string) : TAdvertiserGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/advertiserGroups/{id}';
  _Methodid   = 'dfareporting.advertiserGroups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAdvertiserGroup) as TAdvertiserGroup;
end;

Function TAdvertiserGroupsResource.Insert(profileId: string; aAdvertiserGroup : TAdvertiserGroup) : TAdvertiserGroup;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/advertiserGroups';
  _Methodid   = 'dfareporting.advertiserGroups.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAdvertiserGroup,TAdvertiserGroup) as TAdvertiserGroup;
end;

Function TAdvertiserGroupsResource.List(profileId: string; AQuery : string = '') : TAdvertiserGroupsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/advertiserGroups';
  _Methodid   = 'dfareporting.advertiserGroups.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdvertiserGroupsListResponse) as TAdvertiserGroupsListResponse;
end;


Function TAdvertiserGroupsResource.List(profileId: string; AQuery : TAdvertiserGroupslistOptions) : TAdvertiserGroupsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TAdvertiserGroupsResource.Patch(profileId: string; aAdvertiserGroup : TAdvertiserGroup; AQuery : string = '') : TAdvertiserGroup;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/advertiserGroups';
  _Methodid   = 'dfareporting.advertiserGroups.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAdvertiserGroup,TAdvertiserGroup) as TAdvertiserGroup;
end;


Function TAdvertiserGroupsResource.Patch(profileId: string; aAdvertiserGroup : TAdvertiserGroup; AQuery : TAdvertiserGroupspatchOptions) : TAdvertiserGroup;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aAdvertiserGroup,_Q);
end;

Function TAdvertiserGroupsResource.Update(profileId: string; aAdvertiserGroup : TAdvertiserGroup) : TAdvertiserGroup;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/advertiserGroups';
  _Methodid   = 'dfareporting.advertiserGroups.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAdvertiserGroup,TAdvertiserGroup) as TAdvertiserGroup;
end;



{ --------------------------------------------------------------------
  TAdvertisersResource
  --------------------------------------------------------------------}


Class Function TAdvertisersResource.ResourceName : String;

begin
  Result:='advertisers';
end;

Class Function TAdvertisersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TAdvertisersResource.Get(id: string; profileId: string) : TAdvertiser;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/advertisers/{id}';
  _Methodid   = 'dfareporting.advertisers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAdvertiser) as TAdvertiser;
end;

Function TAdvertisersResource.Insert(profileId: string; aAdvertiser : TAdvertiser) : TAdvertiser;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/advertisers';
  _Methodid   = 'dfareporting.advertisers.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAdvertiser,TAdvertiser) as TAdvertiser;
end;

Function TAdvertisersResource.List(profileId: string; AQuery : string = '') : TAdvertisersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/advertisers';
  _Methodid   = 'dfareporting.advertisers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdvertisersListResponse) as TAdvertisersListResponse;
end;


Function TAdvertisersResource.List(profileId: string; AQuery : TAdvertiserslistOptions) : TAdvertisersListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserGroupIds',AQuery.advertiserGroupIds);
  AddToQuery(_Q,'floodlightConfigurationIds',AQuery.floodlightConfigurationIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'includeAdvertisersWithoutGroupsOnly',AQuery.includeAdvertisersWithoutGroupsOnly);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'onlyParent',AQuery.onlyParent);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'subaccountId',AQuery.subaccountId);
  Result:=List(profileId,_Q);
end;

Function TAdvertisersResource.Patch(profileId: string; aAdvertiser : TAdvertiser; AQuery : string = '') : TAdvertiser;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/advertisers';
  _Methodid   = 'dfareporting.advertisers.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAdvertiser,TAdvertiser) as TAdvertiser;
end;


Function TAdvertisersResource.Patch(profileId: string; aAdvertiser : TAdvertiser; AQuery : TAdvertiserspatchOptions) : TAdvertiser;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aAdvertiser,_Q);
end;

Function TAdvertisersResource.Update(profileId: string; aAdvertiser : TAdvertiser) : TAdvertiser;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/advertisers';
  _Methodid   = 'dfareporting.advertisers.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAdvertiser,TAdvertiser) as TAdvertiser;
end;



{ --------------------------------------------------------------------
  TBrowsersResource
  --------------------------------------------------------------------}


Class Function TBrowsersResource.ResourceName : String;

begin
  Result:='browsers';
end;

Class Function TBrowsersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TBrowsersResource.List(profileId: string) : TBrowsersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/browsers';
  _Methodid   = 'dfareporting.browsers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBrowsersListResponse) as TBrowsersListResponse;
end;



{ --------------------------------------------------------------------
  TCampaignCreativeAssociationsResource
  --------------------------------------------------------------------}


Class Function TCampaignCreativeAssociationsResource.ResourceName : String;

begin
  Result:='campaignCreativeAssociations';
end;

Class Function TCampaignCreativeAssociationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TCampaignCreativeAssociationsResource.Insert(campaignId: string; profileId: string; aCampaignCreativeAssociation : TCampaignCreativeAssociation) : TCampaignCreativeAssociation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/campaigns/{campaignId}/campaignCreativeAssociations';
  _Methodid   = 'dfareporting.campaignCreativeAssociations.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['campaignId',campaignId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCampaignCreativeAssociation,TCampaignCreativeAssociation) as TCampaignCreativeAssociation;
end;

Function TCampaignCreativeAssociationsResource.List(campaignId: string; profileId: string; AQuery : string = '') : TCampaignCreativeAssociationsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/campaigns/{campaignId}/campaignCreativeAssociations';
  _Methodid   = 'dfareporting.campaignCreativeAssociations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['campaignId',campaignId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCampaignCreativeAssociationsListResponse) as TCampaignCreativeAssociationsListResponse;
end;


Function TCampaignCreativeAssociationsResource.List(campaignId: string; profileId: string; AQuery : TCampaignCreativeAssociationslistOptions) : TCampaignCreativeAssociationsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(campaignId,profileId,_Q);
end;



{ --------------------------------------------------------------------
  TCampaignsResource
  --------------------------------------------------------------------}


Class Function TCampaignsResource.ResourceName : String;

begin
  Result:='campaigns';
end;

Class Function TCampaignsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TCampaignsResource.Get(id: string; profileId: string) : TCampaign;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/campaigns/{id}';
  _Methodid   = 'dfareporting.campaigns.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCampaign) as TCampaign;
end;

Function TCampaignsResource.Insert(profileId: string; aCampaign : TCampaign; AQuery : string = '') : TCampaign;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/campaigns';
  _Methodid   = 'dfareporting.campaigns.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCampaign,TCampaign) as TCampaign;
end;


Function TCampaignsResource.Insert(profileId: string; aCampaign : TCampaign; AQuery : TCampaignsinsertOptions) : TCampaign;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'defaultLandingPageName',AQuery.defaultLandingPageName);
  AddToQuery(_Q,'defaultLandingPageUrl',AQuery.defaultLandingPageUrl);
  Result:=Insert(profileId,aCampaign,_Q);
end;

Function TCampaignsResource.List(profileId: string; AQuery : string = '') : TCampaignsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/campaigns';
  _Methodid   = 'dfareporting.campaigns.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCampaignsListResponse) as TCampaignsListResponse;
end;


Function TCampaignsResource.List(profileId: string; AQuery : TCampaignslistOptions) : TCampaignsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserGroupIds',AQuery.advertiserGroupIds);
  AddToQuery(_Q,'advertiserIds',AQuery.advertiserIds);
  AddToQuery(_Q,'archived',AQuery.archived);
  AddToQuery(_Q,'atLeastOneOptimizationActivity',AQuery.atLeastOneOptimizationActivity);
  AddToQuery(_Q,'excludedIds',AQuery.excludedIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'overriddenEventTagId',AQuery.overriddenEventTagId);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'subaccountId',AQuery.subaccountId);
  Result:=List(profileId,_Q);
end;

Function TCampaignsResource.Patch(profileId: string; aCampaign : TCampaign; AQuery : string = '') : TCampaign;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/campaigns';
  _Methodid   = 'dfareporting.campaigns.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCampaign,TCampaign) as TCampaign;
end;


Function TCampaignsResource.Patch(profileId: string; aCampaign : TCampaign; AQuery : TCampaignspatchOptions) : TCampaign;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aCampaign,_Q);
end;

Function TCampaignsResource.Update(profileId: string; aCampaign : TCampaign) : TCampaign;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/campaigns';
  _Methodid   = 'dfareporting.campaigns.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCampaign,TCampaign) as TCampaign;
end;



{ --------------------------------------------------------------------
  TChangeLogsResource
  --------------------------------------------------------------------}


Class Function TChangeLogsResource.ResourceName : String;

begin
  Result:='changeLogs';
end;

Class Function TChangeLogsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TChangeLogsResource.Get(id: string; profileId: string) : TChangeLog;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/changeLogs/{id}';
  _Methodid   = 'dfareporting.changeLogs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TChangeLog) as TChangeLog;
end;

Function TChangeLogsResource.List(profileId: string; AQuery : string = '') : TChangeLogsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/changeLogs';
  _Methodid   = 'dfareporting.changeLogs.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TChangeLogsListResponse) as TChangeLogsListResponse;
end;


Function TChangeLogsResource.List(profileId: string; AQuery : TChangeLogslistOptions) : TChangeLogsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'action',AQuery.action);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxChangeTime',AQuery.maxChangeTime);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'minChangeTime',AQuery.minChangeTime);
  AddToQuery(_Q,'objectIds',AQuery.objectIds);
  AddToQuery(_Q,'objectType',AQuery.objectType);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'userProfileIds',AQuery.userProfileIds);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TCitiesResource
  --------------------------------------------------------------------}


Class Function TCitiesResource.ResourceName : String;

begin
  Result:='cities';
end;

Class Function TCitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TCitiesResource.List(profileId: string; AQuery : string = '') : TCitiesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/cities';
  _Methodid   = 'dfareporting.cities.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCitiesListResponse) as TCitiesListResponse;
end;


Function TCitiesResource.List(profileId: string; AQuery : TCitieslistOptions) : TCitiesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'countryDartIds',AQuery.countryDartIds);
  AddToQuery(_Q,'dartIds',AQuery.dartIds);
  AddToQuery(_Q,'namePrefix',AQuery.namePrefix);
  AddToQuery(_Q,'regionDartIds',AQuery.regionDartIds);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TConnectionTypesResource
  --------------------------------------------------------------------}


Class Function TConnectionTypesResource.ResourceName : String;

begin
  Result:='connectionTypes';
end;

Class Function TConnectionTypesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TConnectionTypesResource.Get(id: string; profileId: string) : TConnectionType;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/connectionTypes/{id}';
  _Methodid   = 'dfareporting.connectionTypes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TConnectionType) as TConnectionType;
end;

Function TConnectionTypesResource.List(profileId: string) : TConnectionTypesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/connectionTypes';
  _Methodid   = 'dfareporting.connectionTypes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TConnectionTypesListResponse) as TConnectionTypesListResponse;
end;



{ --------------------------------------------------------------------
  TContentCategoriesResource
  --------------------------------------------------------------------}


Class Function TContentCategoriesResource.ResourceName : String;

begin
  Result:='contentCategories';
end;

Class Function TContentCategoriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TContentCategoriesResource.Delete(id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/contentCategories/{id}';
  _Methodid   = 'dfareporting.contentCategories.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TContentCategoriesResource.Get(id: string; profileId: string) : TContentCategory;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/contentCategories/{id}';
  _Methodid   = 'dfareporting.contentCategories.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TContentCategory) as TContentCategory;
end;

Function TContentCategoriesResource.Insert(profileId: string; aContentCategory : TContentCategory) : TContentCategory;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/contentCategories';
  _Methodid   = 'dfareporting.contentCategories.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aContentCategory,TContentCategory) as TContentCategory;
end;

Function TContentCategoriesResource.List(profileId: string; AQuery : string = '') : TContentCategoriesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/contentCategories';
  _Methodid   = 'dfareporting.contentCategories.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TContentCategoriesListResponse) as TContentCategoriesListResponse;
end;


Function TContentCategoriesResource.List(profileId: string; AQuery : TContentCategorieslistOptions) : TContentCategoriesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TContentCategoriesResource.Patch(profileId: string; aContentCategory : TContentCategory; AQuery : string = '') : TContentCategory;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/contentCategories';
  _Methodid   = 'dfareporting.contentCategories.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aContentCategory,TContentCategory) as TContentCategory;
end;


Function TContentCategoriesResource.Patch(profileId: string; aContentCategory : TContentCategory; AQuery : TContentCategoriespatchOptions) : TContentCategory;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aContentCategory,_Q);
end;

Function TContentCategoriesResource.Update(profileId: string; aContentCategory : TContentCategory) : TContentCategory;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/contentCategories';
  _Methodid   = 'dfareporting.contentCategories.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aContentCategory,TContentCategory) as TContentCategory;
end;



{ --------------------------------------------------------------------
  TCountriesResource
  --------------------------------------------------------------------}


Class Function TCountriesResource.ResourceName : String;

begin
  Result:='countries';
end;

Class Function TCountriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TCountriesResource.Get(dartId: string; profileId: string) : TCountry;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/countries/{dartId}';
  _Methodid   = 'dfareporting.countries.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dartId',dartId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCountry) as TCountry;
end;

Function TCountriesResource.List(profileId: string) : TCountriesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/countries';
  _Methodid   = 'dfareporting.countries.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCountriesListResponse) as TCountriesListResponse;
end;



{ --------------------------------------------------------------------
  TCreativeAssetsResource
  --------------------------------------------------------------------}


Class Function TCreativeAssetsResource.ResourceName : String;

begin
  Result:='creativeAssets';
end;

Class Function TCreativeAssetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TCreativeAssetsResource.Insert(advertiserId: string; profileId: string; aCreativeAssetMetadata : TCreativeAssetMetadata) : TCreativeAssetMetadata;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/creativeAssets/{advertiserId}/creativeAssets';
  _Methodid   = 'dfareporting.creativeAssets.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['advertiserId',advertiserId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreativeAssetMetadata,TCreativeAssetMetadata) as TCreativeAssetMetadata;
end;



{ --------------------------------------------------------------------
  TCreativeFieldValuesResource
  --------------------------------------------------------------------}


Class Function TCreativeFieldValuesResource.ResourceName : String;

begin
  Result:='creativeFieldValues';
end;

Class Function TCreativeFieldValuesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TCreativeFieldValuesResource.Delete(creativeFieldId: string; id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/creativeFields/{creativeFieldId}/creativeFieldValues/{id}';
  _Methodid   = 'dfareporting.creativeFieldValues.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['creativeFieldId',creativeFieldId,'id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCreativeFieldValuesResource.Get(creativeFieldId: string; id: string; profileId: string) : TCreativeFieldValue;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/creativeFields/{creativeFieldId}/creativeFieldValues/{id}';
  _Methodid   = 'dfareporting.creativeFieldValues.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['creativeFieldId',creativeFieldId,'id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCreativeFieldValue) as TCreativeFieldValue;
end;

Function TCreativeFieldValuesResource.Insert(creativeFieldId: string; profileId: string; aCreativeFieldValue : TCreativeFieldValue) : TCreativeFieldValue;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/creativeFields/{creativeFieldId}/creativeFieldValues';
  _Methodid   = 'dfareporting.creativeFieldValues.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['creativeFieldId',creativeFieldId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreativeFieldValue,TCreativeFieldValue) as TCreativeFieldValue;
end;

Function TCreativeFieldValuesResource.List(creativeFieldId: string; profileId: string; AQuery : string = '') : TCreativeFieldValuesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/creativeFields/{creativeFieldId}/creativeFieldValues';
  _Methodid   = 'dfareporting.creativeFieldValues.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['creativeFieldId',creativeFieldId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCreativeFieldValuesListResponse) as TCreativeFieldValuesListResponse;
end;


Function TCreativeFieldValuesResource.List(creativeFieldId: string; profileId: string; AQuery : TCreativeFieldValueslistOptions) : TCreativeFieldValuesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(creativeFieldId,profileId,_Q);
end;

Function TCreativeFieldValuesResource.Patch(creativeFieldId: string; profileId: string; aCreativeFieldValue : TCreativeFieldValue; AQuery : string = '') : TCreativeFieldValue;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/creativeFields/{creativeFieldId}/creativeFieldValues';
  _Methodid   = 'dfareporting.creativeFieldValues.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['creativeFieldId',creativeFieldId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCreativeFieldValue,TCreativeFieldValue) as TCreativeFieldValue;
end;


Function TCreativeFieldValuesResource.Patch(creativeFieldId: string; profileId: string; aCreativeFieldValue : TCreativeFieldValue; AQuery : TCreativeFieldValuespatchOptions) : TCreativeFieldValue;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(creativeFieldId,profileId,aCreativeFieldValue,_Q);
end;

Function TCreativeFieldValuesResource.Update(creativeFieldId: string; profileId: string; aCreativeFieldValue : TCreativeFieldValue) : TCreativeFieldValue;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/creativeFields/{creativeFieldId}/creativeFieldValues';
  _Methodid   = 'dfareporting.creativeFieldValues.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['creativeFieldId',creativeFieldId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreativeFieldValue,TCreativeFieldValue) as TCreativeFieldValue;
end;



{ --------------------------------------------------------------------
  TCreativeFieldsResource
  --------------------------------------------------------------------}


Class Function TCreativeFieldsResource.ResourceName : String;

begin
  Result:='creativeFields';
end;

Class Function TCreativeFieldsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TCreativeFieldsResource.Delete(id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/creativeFields/{id}';
  _Methodid   = 'dfareporting.creativeFields.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCreativeFieldsResource.Get(id: string; profileId: string) : TCreativeField;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/creativeFields/{id}';
  _Methodid   = 'dfareporting.creativeFields.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCreativeField) as TCreativeField;
end;

Function TCreativeFieldsResource.Insert(profileId: string; aCreativeField : TCreativeField) : TCreativeField;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/creativeFields';
  _Methodid   = 'dfareporting.creativeFields.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreativeField,TCreativeField) as TCreativeField;
end;

Function TCreativeFieldsResource.List(profileId: string; AQuery : string = '') : TCreativeFieldsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/creativeFields';
  _Methodid   = 'dfareporting.creativeFields.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCreativeFieldsListResponse) as TCreativeFieldsListResponse;
end;


Function TCreativeFieldsResource.List(profileId: string; AQuery : TCreativeFieldslistOptions) : TCreativeFieldsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserIds',AQuery.advertiserIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TCreativeFieldsResource.Patch(profileId: string; aCreativeField : TCreativeField; AQuery : string = '') : TCreativeField;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/creativeFields';
  _Methodid   = 'dfareporting.creativeFields.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCreativeField,TCreativeField) as TCreativeField;
end;


Function TCreativeFieldsResource.Patch(profileId: string; aCreativeField : TCreativeField; AQuery : TCreativeFieldspatchOptions) : TCreativeField;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aCreativeField,_Q);
end;

Function TCreativeFieldsResource.Update(profileId: string; aCreativeField : TCreativeField) : TCreativeField;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/creativeFields';
  _Methodid   = 'dfareporting.creativeFields.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreativeField,TCreativeField) as TCreativeField;
end;



{ --------------------------------------------------------------------
  TCreativeGroupsResource
  --------------------------------------------------------------------}


Class Function TCreativeGroupsResource.ResourceName : String;

begin
  Result:='creativeGroups';
end;

Class Function TCreativeGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TCreativeGroupsResource.Get(id: string; profileId: string) : TCreativeGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/creativeGroups/{id}';
  _Methodid   = 'dfareporting.creativeGroups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCreativeGroup) as TCreativeGroup;
end;

Function TCreativeGroupsResource.Insert(profileId: string; aCreativeGroup : TCreativeGroup) : TCreativeGroup;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/creativeGroups';
  _Methodid   = 'dfareporting.creativeGroups.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreativeGroup,TCreativeGroup) as TCreativeGroup;
end;

Function TCreativeGroupsResource.List(profileId: string; AQuery : string = '') : TCreativeGroupsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/creativeGroups';
  _Methodid   = 'dfareporting.creativeGroups.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCreativeGroupsListResponse) as TCreativeGroupsListResponse;
end;


Function TCreativeGroupsResource.List(profileId: string; AQuery : TCreativeGroupslistOptions) : TCreativeGroupsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserIds',AQuery.advertiserIds);
  AddToQuery(_Q,'groupNumber',AQuery.groupNumber);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TCreativeGroupsResource.Patch(profileId: string; aCreativeGroup : TCreativeGroup; AQuery : string = '') : TCreativeGroup;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/creativeGroups';
  _Methodid   = 'dfareporting.creativeGroups.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCreativeGroup,TCreativeGroup) as TCreativeGroup;
end;


Function TCreativeGroupsResource.Patch(profileId: string; aCreativeGroup : TCreativeGroup; AQuery : TCreativeGroupspatchOptions) : TCreativeGroup;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aCreativeGroup,_Q);
end;

Function TCreativeGroupsResource.Update(profileId: string; aCreativeGroup : TCreativeGroup) : TCreativeGroup;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/creativeGroups';
  _Methodid   = 'dfareporting.creativeGroups.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreativeGroup,TCreativeGroup) as TCreativeGroup;
end;



{ --------------------------------------------------------------------
  TCreativesResource
  --------------------------------------------------------------------}


Class Function TCreativesResource.ResourceName : String;

begin
  Result:='creatives';
end;

Class Function TCreativesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TCreativesResource.Get(id: string; profileId: string) : TCreative;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/creatives/{id}';
  _Methodid   = 'dfareporting.creatives.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCreative) as TCreative;
end;

Function TCreativesResource.Insert(profileId: string; aCreative : TCreative) : TCreative;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/creatives';
  _Methodid   = 'dfareporting.creatives.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreative,TCreative) as TCreative;
end;

Function TCreativesResource.List(profileId: string; AQuery : string = '') : TCreativesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/creatives';
  _Methodid   = 'dfareporting.creatives.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCreativesListResponse) as TCreativesListResponse;
end;


Function TCreativesResource.List(profileId: string; AQuery : TCreativeslistOptions) : TCreativesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'active',AQuery.active);
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'archived',AQuery.archived);
  AddToQuery(_Q,'campaignId',AQuery.campaignId);
  AddToQuery(_Q,'companionCreativeIds',AQuery.companionCreativeIds);
  AddToQuery(_Q,'creativeFieldIds',AQuery.creativeFieldIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'renderingIds',AQuery.renderingIds);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sizeIds',AQuery.sizeIds);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'studioCreativeId',AQuery.studioCreativeId);
  AddToQuery(_Q,'types',AQuery.types);
  Result:=List(profileId,_Q);
end;

Function TCreativesResource.Patch(profileId: string; aCreative : TCreative; AQuery : string = '') : TCreative;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/creatives';
  _Methodid   = 'dfareporting.creatives.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCreative,TCreative) as TCreative;
end;


Function TCreativesResource.Patch(profileId: string; aCreative : TCreative; AQuery : TCreativespatchOptions) : TCreative;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aCreative,_Q);
end;

Function TCreativesResource.Update(profileId: string; aCreative : TCreative) : TCreative;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/creatives';
  _Methodid   = 'dfareporting.creatives.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreative,TCreative) as TCreative;
end;



{ --------------------------------------------------------------------
  TDimensionValuesResource
  --------------------------------------------------------------------}


Class Function TDimensionValuesResource.ResourceName : String;

begin
  Result:='dimensionValues';
end;

Class Function TDimensionValuesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TDimensionValuesResource.Query(profileId: string; aDimensionValueRequest : TDimensionValueRequest; AQuery : string = '') : TDimensionValueList;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/dimensionvalues/query';
  _Methodid   = 'dfareporting.dimensionValues.query';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aDimensionValueRequest,TDimensionValueList) as TDimensionValueList;
end;


Function TDimensionValuesResource.Query(profileId: string; aDimensionValueRequest : TDimensionValueRequest; AQuery : TDimensionValuesqueryOptions) : TDimensionValueList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=Query(profileId,aDimensionValueRequest,_Q);
end;



{ --------------------------------------------------------------------
  TDirectorySiteContactsResource
  --------------------------------------------------------------------}


Class Function TDirectorySiteContactsResource.ResourceName : String;

begin
  Result:='directorySiteContacts';
end;

Class Function TDirectorySiteContactsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TDirectorySiteContactsResource.Get(id: string; profileId: string) : TDirectorySiteContact;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/directorySiteContacts/{id}';
  _Methodid   = 'dfareporting.directorySiteContacts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDirectorySiteContact) as TDirectorySiteContact;
end;

Function TDirectorySiteContactsResource.List(profileId: string; AQuery : string = '') : TDirectorySiteContactsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/directorySiteContacts';
  _Methodid   = 'dfareporting.directorySiteContacts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDirectorySiteContactsListResponse) as TDirectorySiteContactsListResponse;
end;


Function TDirectorySiteContactsResource.List(profileId: string; AQuery : TDirectorySiteContactslistOptions) : TDirectorySiteContactsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'directorySiteIds',AQuery.directorySiteIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TDirectorySitesResource
  --------------------------------------------------------------------}


Class Function TDirectorySitesResource.ResourceName : String;

begin
  Result:='directorySites';
end;

Class Function TDirectorySitesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TDirectorySitesResource.Get(id: string; profileId: string) : TDirectorySite;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/directorySites/{id}';
  _Methodid   = 'dfareporting.directorySites.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDirectorySite) as TDirectorySite;
end;

Function TDirectorySitesResource.Insert(profileId: string; aDirectorySite : TDirectorySite) : TDirectorySite;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/directorySites';
  _Methodid   = 'dfareporting.directorySites.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDirectorySite,TDirectorySite) as TDirectorySite;
end;

Function TDirectorySitesResource.List(profileId: string; AQuery : string = '') : TDirectorySitesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/directorySites';
  _Methodid   = 'dfareporting.directorySites.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDirectorySitesListResponse) as TDirectorySitesListResponse;
end;


Function TDirectorySitesResource.List(profileId: string; AQuery : TDirectorySiteslistOptions) : TDirectorySitesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'acceptsInStreamVideoPlacements',AQuery.acceptsInStreamVideoPlacements);
  AddToQuery(_Q,'acceptsInterstitialPlacements',AQuery.acceptsInterstitialPlacements);
  AddToQuery(_Q,'acceptsPublisherPaidPlacements',AQuery.acceptsPublisherPaidPlacements);
  AddToQuery(_Q,'active',AQuery.active);
  AddToQuery(_Q,'countryId',AQuery.countryId);
  AddToQuery(_Q,'dfp_network_code',AQuery.dfp_network_code);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'parentId',AQuery.parentId);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TEventTagsResource
  --------------------------------------------------------------------}


Class Function TEventTagsResource.ResourceName : String;

begin
  Result:='eventTags';
end;

Class Function TEventTagsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TEventTagsResource.Delete(id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/eventTags/{id}';
  _Methodid   = 'dfareporting.eventTags.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TEventTagsResource.Get(id: string; profileId: string) : TEventTag;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/eventTags/{id}';
  _Methodid   = 'dfareporting.eventTags.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEventTag) as TEventTag;
end;

Function TEventTagsResource.Insert(profileId: string; aEventTag : TEventTag) : TEventTag;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/eventTags';
  _Methodid   = 'dfareporting.eventTags.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEventTag,TEventTag) as TEventTag;
end;

Function TEventTagsResource.List(profileId: string; AQuery : string = '') : TEventTagsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/eventTags';
  _Methodid   = 'dfareporting.eventTags.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEventTagsListResponse) as TEventTagsListResponse;
end;


Function TEventTagsResource.List(profileId: string; AQuery : TEventTagslistOptions) : TEventTagsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'adId',AQuery.adId);
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'campaignId',AQuery.campaignId);
  AddToQuery(_Q,'definitionsOnly',AQuery.definitionsOnly);
  AddToQuery(_Q,'enabled',AQuery.enabled);
  AddToQuery(_Q,'eventTagTypes',AQuery.eventTagTypes);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TEventTagsResource.Patch(profileId: string; aEventTag : TEventTag; AQuery : string = '') : TEventTag;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/eventTags';
  _Methodid   = 'dfareporting.eventTags.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aEventTag,TEventTag) as TEventTag;
end;


Function TEventTagsResource.Patch(profileId: string; aEventTag : TEventTag; AQuery : TEventTagspatchOptions) : TEventTag;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aEventTag,_Q);
end;

Function TEventTagsResource.Update(profileId: string; aEventTag : TEventTag) : TEventTag;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/eventTags';
  _Methodid   = 'dfareporting.eventTags.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEventTag,TEventTag) as TEventTag;
end;



{ --------------------------------------------------------------------
  TFilesResource
  --------------------------------------------------------------------}


Class Function TFilesResource.ResourceName : String;

begin
  Result:='files';
end;

Class Function TFilesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TFilesResource.Get(fileId: string; reportId: string) : TFile;

Const
  _HTTPMethod = 'GET';
  _Path       = 'reports/{reportId}/files/{fileId}';
  _Methodid   = 'dfareporting.files.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'reportId',reportId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFile) as TFile;
end;

Function TFilesResource.List(profileId: string; AQuery : string = '') : TFileList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/files';
  _Methodid   = 'dfareporting.files.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFileList) as TFileList;
end;


Function TFilesResource.List(profileId: string; AQuery : TFileslistOptions) : TFileList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'scope',AQuery.scope);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TFloodlightActivitiesResource
  --------------------------------------------------------------------}


Class Function TFloodlightActivitiesResource.ResourceName : String;

begin
  Result:='floodlightActivities';
end;

Class Function TFloodlightActivitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TFloodlightActivitiesResource.Delete(id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/floodlightActivities/{id}';
  _Methodid   = 'dfareporting.floodlightActivities.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TFloodlightActivitiesResource.Generatetag(profileId: string; AQuery : string = '') : TFloodlightActivitiesGenerateTagResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/floodlightActivities/generatetag';
  _Methodid   = 'dfareporting.floodlightActivities.generatetag';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFloodlightActivitiesGenerateTagResponse) as TFloodlightActivitiesGenerateTagResponse;
end;


Function TFloodlightActivitiesResource.Generatetag(profileId: string; AQuery : TFloodlightActivitiesgeneratetagOptions) : TFloodlightActivitiesGenerateTagResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'floodlightActivityId',AQuery.floodlightActivityId);
  Result:=Generatetag(profileId,_Q);
end;

Function TFloodlightActivitiesResource.Get(id: string; profileId: string) : TFloodlightActivity;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/floodlightActivities/{id}';
  _Methodid   = 'dfareporting.floodlightActivities.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFloodlightActivity) as TFloodlightActivity;
end;

Function TFloodlightActivitiesResource.Insert(profileId: string; aFloodlightActivity : TFloodlightActivity) : TFloodlightActivity;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/floodlightActivities';
  _Methodid   = 'dfareporting.floodlightActivities.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFloodlightActivity,TFloodlightActivity) as TFloodlightActivity;
end;

Function TFloodlightActivitiesResource.List(profileId: string; AQuery : string = '') : TFloodlightActivitiesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/floodlightActivities';
  _Methodid   = 'dfareporting.floodlightActivities.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFloodlightActivitiesListResponse) as TFloodlightActivitiesListResponse;
end;


Function TFloodlightActivitiesResource.List(profileId: string; AQuery : TFloodlightActivitieslistOptions) : TFloodlightActivitiesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'floodlightActivityGroupIds',AQuery.floodlightActivityGroupIds);
  AddToQuery(_Q,'floodlightActivityGroupName',AQuery.floodlightActivityGroupName);
  AddToQuery(_Q,'floodlightActivityGroupTagString',AQuery.floodlightActivityGroupTagString);
  AddToQuery(_Q,'floodlightActivityGroupType',AQuery.floodlightActivityGroupType);
  AddToQuery(_Q,'floodlightConfigurationId',AQuery.floodlightConfigurationId);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'tagString',AQuery.tagString);
  Result:=List(profileId,_Q);
end;

Function TFloodlightActivitiesResource.Patch(profileId: string; aFloodlightActivity : TFloodlightActivity; AQuery : string = '') : TFloodlightActivity;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/floodlightActivities';
  _Methodid   = 'dfareporting.floodlightActivities.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aFloodlightActivity,TFloodlightActivity) as TFloodlightActivity;
end;


Function TFloodlightActivitiesResource.Patch(profileId: string; aFloodlightActivity : TFloodlightActivity; AQuery : TFloodlightActivitiespatchOptions) : TFloodlightActivity;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aFloodlightActivity,_Q);
end;

Function TFloodlightActivitiesResource.Update(profileId: string; aFloodlightActivity : TFloodlightActivity) : TFloodlightActivity;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/floodlightActivities';
  _Methodid   = 'dfareporting.floodlightActivities.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFloodlightActivity,TFloodlightActivity) as TFloodlightActivity;
end;



{ --------------------------------------------------------------------
  TFloodlightActivityGroupsResource
  --------------------------------------------------------------------}


Class Function TFloodlightActivityGroupsResource.ResourceName : String;

begin
  Result:='floodlightActivityGroups';
end;

Class Function TFloodlightActivityGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TFloodlightActivityGroupsResource.Delete(id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/floodlightActivityGroups/{id}';
  _Methodid   = 'dfareporting.floodlightActivityGroups.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TFloodlightActivityGroupsResource.Get(id: string; profileId: string) : TFloodlightActivityGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/floodlightActivityGroups/{id}';
  _Methodid   = 'dfareporting.floodlightActivityGroups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFloodlightActivityGroup) as TFloodlightActivityGroup;
end;

Function TFloodlightActivityGroupsResource.Insert(profileId: string; aFloodlightActivityGroup : TFloodlightActivityGroup) : TFloodlightActivityGroup;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/floodlightActivityGroups';
  _Methodid   = 'dfareporting.floodlightActivityGroups.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFloodlightActivityGroup,TFloodlightActivityGroup) as TFloodlightActivityGroup;
end;

Function TFloodlightActivityGroupsResource.List(profileId: string; AQuery : string = '') : TFloodlightActivityGroupsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/floodlightActivityGroups';
  _Methodid   = 'dfareporting.floodlightActivityGroups.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFloodlightActivityGroupsListResponse) as TFloodlightActivityGroupsListResponse;
end;


Function TFloodlightActivityGroupsResource.List(profileId: string; AQuery : TFloodlightActivityGroupslistOptions) : TFloodlightActivityGroupsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'floodlightConfigurationId',AQuery.floodlightConfigurationId);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'type',AQuery._type);
  Result:=List(profileId,_Q);
end;

Function TFloodlightActivityGroupsResource.Patch(profileId: string; aFloodlightActivityGroup : TFloodlightActivityGroup; AQuery : string = '') : TFloodlightActivityGroup;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/floodlightActivityGroups';
  _Methodid   = 'dfareporting.floodlightActivityGroups.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aFloodlightActivityGroup,TFloodlightActivityGroup) as TFloodlightActivityGroup;
end;


Function TFloodlightActivityGroupsResource.Patch(profileId: string; aFloodlightActivityGroup : TFloodlightActivityGroup; AQuery : TFloodlightActivityGroupspatchOptions) : TFloodlightActivityGroup;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aFloodlightActivityGroup,_Q);
end;

Function TFloodlightActivityGroupsResource.Update(profileId: string; aFloodlightActivityGroup : TFloodlightActivityGroup) : TFloodlightActivityGroup;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/floodlightActivityGroups';
  _Methodid   = 'dfareporting.floodlightActivityGroups.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFloodlightActivityGroup,TFloodlightActivityGroup) as TFloodlightActivityGroup;
end;



{ --------------------------------------------------------------------
  TFloodlightConfigurationsResource
  --------------------------------------------------------------------}


Class Function TFloodlightConfigurationsResource.ResourceName : String;

begin
  Result:='floodlightConfigurations';
end;

Class Function TFloodlightConfigurationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TFloodlightConfigurationsResource.Get(id: string; profileId: string) : TFloodlightConfiguration;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/floodlightConfigurations/{id}';
  _Methodid   = 'dfareporting.floodlightConfigurations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFloodlightConfiguration) as TFloodlightConfiguration;
end;

Function TFloodlightConfigurationsResource.List(profileId: string; AQuery : string = '') : TFloodlightConfigurationsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/floodlightConfigurations';
  _Methodid   = 'dfareporting.floodlightConfigurations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFloodlightConfigurationsListResponse) as TFloodlightConfigurationsListResponse;
end;


Function TFloodlightConfigurationsResource.List(profileId: string; AQuery : TFloodlightConfigurationslistOptions) : TFloodlightConfigurationsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  Result:=List(profileId,_Q);
end;

Function TFloodlightConfigurationsResource.Patch(profileId: string; aFloodlightConfiguration : TFloodlightConfiguration; AQuery : string = '') : TFloodlightConfiguration;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/floodlightConfigurations';
  _Methodid   = 'dfareporting.floodlightConfigurations.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aFloodlightConfiguration,TFloodlightConfiguration) as TFloodlightConfiguration;
end;


Function TFloodlightConfigurationsResource.Patch(profileId: string; aFloodlightConfiguration : TFloodlightConfiguration; AQuery : TFloodlightConfigurationspatchOptions) : TFloodlightConfiguration;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aFloodlightConfiguration,_Q);
end;

Function TFloodlightConfigurationsResource.Update(profileId: string; aFloodlightConfiguration : TFloodlightConfiguration) : TFloodlightConfiguration;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/floodlightConfigurations';
  _Methodid   = 'dfareporting.floodlightConfigurations.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFloodlightConfiguration,TFloodlightConfiguration) as TFloodlightConfiguration;
end;



{ --------------------------------------------------------------------
  TInventoryItemsResource
  --------------------------------------------------------------------}


Class Function TInventoryItemsResource.ResourceName : String;

begin
  Result:='inventoryItems';
end;

Class Function TInventoryItemsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TInventoryItemsResource.Get(id: string; profileId: string; projectId: string) : TInventoryItem;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/projects/{projectId}/inventoryItems/{id}';
  _Methodid   = 'dfareporting.inventoryItems.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInventoryItem) as TInventoryItem;
end;

Function TInventoryItemsResource.List(profileId: string; projectId: string; AQuery : string = '') : TInventoryItemsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/projects/{projectId}/inventoryItems';
  _Methodid   = 'dfareporting.inventoryItems.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInventoryItemsListResponse) as TInventoryItemsListResponse;
end;


Function TInventoryItemsResource.List(profileId: string; projectId: string; AQuery : TInventoryItemslistOptions) : TInventoryItemsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'inPlan',AQuery.inPlan);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderId',AQuery.orderId);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'siteId',AQuery.siteId);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,projectId,_Q);
end;



{ --------------------------------------------------------------------
  TLandingPagesResource
  --------------------------------------------------------------------}


Class Function TLandingPagesResource.ResourceName : String;

begin
  Result:='landingPages';
end;

Class Function TLandingPagesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TLandingPagesResource.Delete(campaignId: string; id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/campaigns/{campaignId}/landingPages/{id}';
  _Methodid   = 'dfareporting.landingPages.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['campaignId',campaignId,'id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TLandingPagesResource.Get(campaignId: string; id: string; profileId: string) : TLandingPage;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/campaigns/{campaignId}/landingPages/{id}';
  _Methodid   = 'dfareporting.landingPages.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['campaignId',campaignId,'id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLandingPage) as TLandingPage;
end;

Function TLandingPagesResource.Insert(campaignId: string; profileId: string; aLandingPage : TLandingPage) : TLandingPage;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/campaigns/{campaignId}/landingPages';
  _Methodid   = 'dfareporting.landingPages.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['campaignId',campaignId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLandingPage,TLandingPage) as TLandingPage;
end;

Function TLandingPagesResource.List(campaignId: string; profileId: string) : TLandingPagesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/campaigns/{campaignId}/landingPages';
  _Methodid   = 'dfareporting.landingPages.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['campaignId',campaignId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLandingPagesListResponse) as TLandingPagesListResponse;
end;

Function TLandingPagesResource.Patch(campaignId: string; profileId: string; aLandingPage : TLandingPage; AQuery : string = '') : TLandingPage;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/campaigns/{campaignId}/landingPages';
  _Methodid   = 'dfareporting.landingPages.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['campaignId',campaignId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aLandingPage,TLandingPage) as TLandingPage;
end;


Function TLandingPagesResource.Patch(campaignId: string; profileId: string; aLandingPage : TLandingPage; AQuery : TLandingPagespatchOptions) : TLandingPage;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(campaignId,profileId,aLandingPage,_Q);
end;

Function TLandingPagesResource.Update(campaignId: string; profileId: string; aLandingPage : TLandingPage) : TLandingPage;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/campaigns/{campaignId}/landingPages';
  _Methodid   = 'dfareporting.landingPages.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['campaignId',campaignId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLandingPage,TLandingPage) as TLandingPage;
end;



{ --------------------------------------------------------------------
  TMetrosResource
  --------------------------------------------------------------------}


Class Function TMetrosResource.ResourceName : String;

begin
  Result:='metros';
end;

Class Function TMetrosResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TMetrosResource.List(profileId: string) : TMetrosListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/metros';
  _Methodid   = 'dfareporting.metros.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMetrosListResponse) as TMetrosListResponse;
end;



{ --------------------------------------------------------------------
  TMobileCarriersResource
  --------------------------------------------------------------------}


Class Function TMobileCarriersResource.ResourceName : String;

begin
  Result:='mobileCarriers';
end;

Class Function TMobileCarriersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TMobileCarriersResource.Get(id: string; profileId: string) : TMobileCarrier;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/mobileCarriers/{id}';
  _Methodid   = 'dfareporting.mobileCarriers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMobileCarrier) as TMobileCarrier;
end;

Function TMobileCarriersResource.List(profileId: string) : TMobileCarriersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/mobileCarriers';
  _Methodid   = 'dfareporting.mobileCarriers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMobileCarriersListResponse) as TMobileCarriersListResponse;
end;



{ --------------------------------------------------------------------
  TOperatingSystemVersionsResource
  --------------------------------------------------------------------}


Class Function TOperatingSystemVersionsResource.ResourceName : String;

begin
  Result:='operatingSystemVersions';
end;

Class Function TOperatingSystemVersionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TOperatingSystemVersionsResource.Get(id: string; profileId: string) : TOperatingSystemVersion;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/operatingSystemVersions/{id}';
  _Methodid   = 'dfareporting.operatingSystemVersions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperatingSystemVersion) as TOperatingSystemVersion;
end;

Function TOperatingSystemVersionsResource.List(profileId: string) : TOperatingSystemVersionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/operatingSystemVersions';
  _Methodid   = 'dfareporting.operatingSystemVersions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperatingSystemVersionsListResponse) as TOperatingSystemVersionsListResponse;
end;



{ --------------------------------------------------------------------
  TOperatingSystemsResource
  --------------------------------------------------------------------}


Class Function TOperatingSystemsResource.ResourceName : String;

begin
  Result:='operatingSystems';
end;

Class Function TOperatingSystemsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TOperatingSystemsResource.Get(dartId: string; profileId: string) : TOperatingSystem;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/operatingSystems/{dartId}';
  _Methodid   = 'dfareporting.operatingSystems.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dartId',dartId,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperatingSystem) as TOperatingSystem;
end;

Function TOperatingSystemsResource.List(profileId: string) : TOperatingSystemsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/operatingSystems';
  _Methodid   = 'dfareporting.operatingSystems.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperatingSystemsListResponse) as TOperatingSystemsListResponse;
end;



{ --------------------------------------------------------------------
  TOrderDocumentsResource
  --------------------------------------------------------------------}


Class Function TOrderDocumentsResource.ResourceName : String;

begin
  Result:='orderDocuments';
end;

Class Function TOrderDocumentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TOrderDocumentsResource.Get(id: string; profileId: string; projectId: string) : TOrderDocument;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/projects/{projectId}/orderDocuments/{id}';
  _Methodid   = 'dfareporting.orderDocuments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOrderDocument) as TOrderDocument;
end;

Function TOrderDocumentsResource.List(profileId: string; projectId: string; AQuery : string = '') : TOrderDocumentsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/projects/{projectId}/orderDocuments';
  _Methodid   = 'dfareporting.orderDocuments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOrderDocumentsListResponse) as TOrderDocumentsListResponse;
end;


Function TOrderDocumentsResource.List(profileId: string; projectId: string; AQuery : TOrderDocumentslistOptions) : TOrderDocumentsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'approved',AQuery.approved);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderId',AQuery.orderId);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'siteId',AQuery.siteId);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,projectId,_Q);
end;



{ --------------------------------------------------------------------
  TOrdersResource
  --------------------------------------------------------------------}


Class Function TOrdersResource.ResourceName : String;

begin
  Result:='orders';
end;

Class Function TOrdersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TOrdersResource.Get(id: string; profileId: string; projectId: string) : TOrder;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/projects/{projectId}/orders/{id}';
  _Methodid   = 'dfareporting.orders.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOrder) as TOrder;
end;

Function TOrdersResource.List(profileId: string; projectId: string; AQuery : string = '') : TOrdersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/projects/{projectId}/orders';
  _Methodid   = 'dfareporting.orders.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOrdersListResponse) as TOrdersListResponse;
end;


Function TOrdersResource.List(profileId: string; projectId: string; AQuery : TOrderslistOptions) : TOrdersListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'siteId',AQuery.siteId);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,projectId,_Q);
end;



{ --------------------------------------------------------------------
  TPlacementGroupsResource
  --------------------------------------------------------------------}


Class Function TPlacementGroupsResource.ResourceName : String;

begin
  Result:='placementGroups';
end;

Class Function TPlacementGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TPlacementGroupsResource.Get(id: string; profileId: string) : TPlacementGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/placementGroups/{id}';
  _Methodid   = 'dfareporting.placementGroups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPlacementGroup) as TPlacementGroup;
end;

Function TPlacementGroupsResource.Insert(profileId: string; aPlacementGroup : TPlacementGroup) : TPlacementGroup;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/placementGroups';
  _Methodid   = 'dfareporting.placementGroups.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPlacementGroup,TPlacementGroup) as TPlacementGroup;
end;

Function TPlacementGroupsResource.List(profileId: string; AQuery : string = '') : TPlacementGroupsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/placementGroups';
  _Methodid   = 'dfareporting.placementGroups.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlacementGroupsListResponse) as TPlacementGroupsListResponse;
end;


Function TPlacementGroupsResource.List(profileId: string; AQuery : TPlacementGroupslistOptions) : TPlacementGroupsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserIds',AQuery.advertiserIds);
  AddToQuery(_Q,'archived',AQuery.archived);
  AddToQuery(_Q,'campaignIds',AQuery.campaignIds);
  AddToQuery(_Q,'contentCategoryIds',AQuery.contentCategoryIds);
  AddToQuery(_Q,'directorySiteIds',AQuery.directorySiteIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'placementGroupType',AQuery.placementGroupType);
  AddToQuery(_Q,'placementStrategyIds',AQuery.placementStrategyIds);
  AddToQuery(_Q,'pricingTypes',AQuery.pricingTypes);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'siteIds',AQuery.siteIds);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TPlacementGroupsResource.Patch(profileId: string; aPlacementGroup : TPlacementGroup; AQuery : string = '') : TPlacementGroup;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/placementGroups';
  _Methodid   = 'dfareporting.placementGroups.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPlacementGroup,TPlacementGroup) as TPlacementGroup;
end;


Function TPlacementGroupsResource.Patch(profileId: string; aPlacementGroup : TPlacementGroup; AQuery : TPlacementGroupspatchOptions) : TPlacementGroup;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aPlacementGroup,_Q);
end;

Function TPlacementGroupsResource.Update(profileId: string; aPlacementGroup : TPlacementGroup) : TPlacementGroup;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/placementGroups';
  _Methodid   = 'dfareporting.placementGroups.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPlacementGroup,TPlacementGroup) as TPlacementGroup;
end;



{ --------------------------------------------------------------------
  TPlacementStrategiesResource
  --------------------------------------------------------------------}


Class Function TPlacementStrategiesResource.ResourceName : String;

begin
  Result:='placementStrategies';
end;

Class Function TPlacementStrategiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TPlacementStrategiesResource.Delete(id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/placementStrategies/{id}';
  _Methodid   = 'dfareporting.placementStrategies.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TPlacementStrategiesResource.Get(id: string; profileId: string) : TPlacementStrategy;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/placementStrategies/{id}';
  _Methodid   = 'dfareporting.placementStrategies.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPlacementStrategy) as TPlacementStrategy;
end;

Function TPlacementStrategiesResource.Insert(profileId: string; aPlacementStrategy : TPlacementStrategy) : TPlacementStrategy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/placementStrategies';
  _Methodid   = 'dfareporting.placementStrategies.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPlacementStrategy,TPlacementStrategy) as TPlacementStrategy;
end;

Function TPlacementStrategiesResource.List(profileId: string; AQuery : string = '') : TPlacementStrategiesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/placementStrategies';
  _Methodid   = 'dfareporting.placementStrategies.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlacementStrategiesListResponse) as TPlacementStrategiesListResponse;
end;


Function TPlacementStrategiesResource.List(profileId: string; AQuery : TPlacementStrategieslistOptions) : TPlacementStrategiesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TPlacementStrategiesResource.Patch(profileId: string; aPlacementStrategy : TPlacementStrategy; AQuery : string = '') : TPlacementStrategy;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/placementStrategies';
  _Methodid   = 'dfareporting.placementStrategies.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPlacementStrategy,TPlacementStrategy) as TPlacementStrategy;
end;


Function TPlacementStrategiesResource.Patch(profileId: string; aPlacementStrategy : TPlacementStrategy; AQuery : TPlacementStrategiespatchOptions) : TPlacementStrategy;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aPlacementStrategy,_Q);
end;

Function TPlacementStrategiesResource.Update(profileId: string; aPlacementStrategy : TPlacementStrategy) : TPlacementStrategy;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/placementStrategies';
  _Methodid   = 'dfareporting.placementStrategies.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPlacementStrategy,TPlacementStrategy) as TPlacementStrategy;
end;



{ --------------------------------------------------------------------
  TPlacementsResource
  --------------------------------------------------------------------}


Class Function TPlacementsResource.ResourceName : String;

begin
  Result:='placements';
end;

Class Function TPlacementsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TPlacementsResource.Generatetags(profileId: string; AQuery : string = '') : TPlacementsGenerateTagsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/placements/generatetags';
  _Methodid   = 'dfareporting.placements.generatetags';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlacementsGenerateTagsResponse) as TPlacementsGenerateTagsResponse;
end;


Function TPlacementsResource.Generatetags(profileId: string; AQuery : TPlacementsgeneratetagsOptions) : TPlacementsGenerateTagsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'campaignId',AQuery.campaignId);
  AddToQuery(_Q,'placementIds',AQuery.placementIds);
  AddToQuery(_Q,'tagFormats',AQuery.tagFormats);
  Result:=Generatetags(profileId,_Q);
end;

Function TPlacementsResource.Get(id: string; profileId: string) : TPlacement;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/placements/{id}';
  _Methodid   = 'dfareporting.placements.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPlacement) as TPlacement;
end;

Function TPlacementsResource.Insert(profileId: string; aPlacement : TPlacement) : TPlacement;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/placements';
  _Methodid   = 'dfareporting.placements.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPlacement,TPlacement) as TPlacement;
end;

Function TPlacementsResource.List(profileId: string; AQuery : string = '') : TPlacementsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/placements';
  _Methodid   = 'dfareporting.placements.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlacementsListResponse) as TPlacementsListResponse;
end;


Function TPlacementsResource.List(profileId: string; AQuery : TPlacementslistOptions) : TPlacementsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserIds',AQuery.advertiserIds);
  AddToQuery(_Q,'archived',AQuery.archived);
  AddToQuery(_Q,'campaignIds',AQuery.campaignIds);
  AddToQuery(_Q,'compatibilities',AQuery.compatibilities);
  AddToQuery(_Q,'contentCategoryIds',AQuery.contentCategoryIds);
  AddToQuery(_Q,'directorySiteIds',AQuery.directorySiteIds);
  AddToQuery(_Q,'groupIds',AQuery.groupIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'paymentSource',AQuery.paymentSource);
  AddToQuery(_Q,'placementStrategyIds',AQuery.placementStrategyIds);
  AddToQuery(_Q,'pricingTypes',AQuery.pricingTypes);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'siteIds',AQuery.siteIds);
  AddToQuery(_Q,'sizeIds',AQuery.sizeIds);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TPlacementsResource.Patch(profileId: string; aPlacement : TPlacement; AQuery : string = '') : TPlacement;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/placements';
  _Methodid   = 'dfareporting.placements.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPlacement,TPlacement) as TPlacement;
end;


Function TPlacementsResource.Patch(profileId: string; aPlacement : TPlacement; AQuery : TPlacementspatchOptions) : TPlacement;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aPlacement,_Q);
end;

Function TPlacementsResource.Update(profileId: string; aPlacement : TPlacement) : TPlacement;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/placements';
  _Methodid   = 'dfareporting.placements.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPlacement,TPlacement) as TPlacement;
end;



{ --------------------------------------------------------------------
  TPlatformTypesResource
  --------------------------------------------------------------------}


Class Function TPlatformTypesResource.ResourceName : String;

begin
  Result:='platformTypes';
end;

Class Function TPlatformTypesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TPlatformTypesResource.Get(id: string; profileId: string) : TPlatformType;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/platformTypes/{id}';
  _Methodid   = 'dfareporting.platformTypes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPlatformType) as TPlatformType;
end;

Function TPlatformTypesResource.List(profileId: string) : TPlatformTypesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/platformTypes';
  _Methodid   = 'dfareporting.platformTypes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPlatformTypesListResponse) as TPlatformTypesListResponse;
end;



{ --------------------------------------------------------------------
  TPostalCodesResource
  --------------------------------------------------------------------}


Class Function TPostalCodesResource.ResourceName : String;

begin
  Result:='postalCodes';
end;

Class Function TPostalCodesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TPostalCodesResource.Get(code: string; profileId: string) : TPostalCode;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/postalCodes/{code}';
  _Methodid   = 'dfareporting.postalCodes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['code',code,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPostalCode) as TPostalCode;
end;

Function TPostalCodesResource.List(profileId: string) : TPostalCodesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/postalCodes';
  _Methodid   = 'dfareporting.postalCodes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPostalCodesListResponse) as TPostalCodesListResponse;
end;



{ --------------------------------------------------------------------
  TProjectsResource
  --------------------------------------------------------------------}


Class Function TProjectsResource.ResourceName : String;

begin
  Result:='projects';
end;

Class Function TProjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TProjectsResource.Get(id: string; profileId: string) : TProject;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/projects/{id}';
  _Methodid   = 'dfareporting.projects.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProject) as TProject;
end;

Function TProjectsResource.List(profileId: string; AQuery : string = '') : TProjectsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/projects';
  _Methodid   = 'dfareporting.projects.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TProjectsListResponse) as TProjectsListResponse;
end;


Function TProjectsResource.List(profileId: string; AQuery : TProjectslistOptions) : TProjectsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserIds',AQuery.advertiserIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TRegionsResource
  --------------------------------------------------------------------}


Class Function TRegionsResource.ResourceName : String;

begin
  Result:='regions';
end;

Class Function TRegionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TRegionsResource.List(profileId: string) : TRegionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/regions';
  _Methodid   = 'dfareporting.regions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRegionsListResponse) as TRegionsListResponse;
end;



{ --------------------------------------------------------------------
  TRemarketingListSharesResource
  --------------------------------------------------------------------}


Class Function TRemarketingListSharesResource.ResourceName : String;

begin
  Result:='remarketingListShares';
end;

Class Function TRemarketingListSharesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TRemarketingListSharesResource.Get(profileId: string; remarketingListId: string) : TRemarketingListShare;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/remarketingListShares/{remarketingListId}';
  _Methodid   = 'dfareporting.remarketingListShares.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'remarketingListId',remarketingListId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRemarketingListShare) as TRemarketingListShare;
end;

Function TRemarketingListSharesResource.Patch(profileId: string; aRemarketingListShare : TRemarketingListShare; AQuery : string = '') : TRemarketingListShare;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/remarketingListShares';
  _Methodid   = 'dfareporting.remarketingListShares.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aRemarketingListShare,TRemarketingListShare) as TRemarketingListShare;
end;


Function TRemarketingListSharesResource.Patch(profileId: string; aRemarketingListShare : TRemarketingListShare; AQuery : TRemarketingListSharespatchOptions) : TRemarketingListShare;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'remarketingListId',AQuery.remarketingListId);
  Result:=Patch(profileId,aRemarketingListShare,_Q);
end;

Function TRemarketingListSharesResource.Update(profileId: string; aRemarketingListShare : TRemarketingListShare) : TRemarketingListShare;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/remarketingListShares';
  _Methodid   = 'dfareporting.remarketingListShares.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRemarketingListShare,TRemarketingListShare) as TRemarketingListShare;
end;



{ --------------------------------------------------------------------
  TRemarketingListsResource
  --------------------------------------------------------------------}


Class Function TRemarketingListsResource.ResourceName : String;

begin
  Result:='remarketingLists';
end;

Class Function TRemarketingListsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TRemarketingListsResource.Get(id: string; profileId: string) : TRemarketingList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/remarketingLists/{id}';
  _Methodid   = 'dfareporting.remarketingLists.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRemarketingList) as TRemarketingList;
end;

Function TRemarketingListsResource.Insert(profileId: string; aRemarketingList : TRemarketingList) : TRemarketingList;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/remarketingLists';
  _Methodid   = 'dfareporting.remarketingLists.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRemarketingList,TRemarketingList) as TRemarketingList;
end;

Function TRemarketingListsResource.List(profileId: string; AQuery : string = '') : TRemarketingListsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/remarketingLists';
  _Methodid   = 'dfareporting.remarketingLists.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRemarketingListsListResponse) as TRemarketingListsListResponse;
end;


Function TRemarketingListsResource.List(profileId: string; AQuery : TRemarketingListslistOptions) : TRemarketingListsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'active',AQuery.active);
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'floodlightActivityId',AQuery.floodlightActivityId);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'name',AQuery._name);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TRemarketingListsResource.Patch(profileId: string; aRemarketingList : TRemarketingList; AQuery : string = '') : TRemarketingList;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/remarketingLists';
  _Methodid   = 'dfareporting.remarketingLists.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aRemarketingList,TRemarketingList) as TRemarketingList;
end;


Function TRemarketingListsResource.Patch(profileId: string; aRemarketingList : TRemarketingList; AQuery : TRemarketingListspatchOptions) : TRemarketingList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aRemarketingList,_Q);
end;

Function TRemarketingListsResource.Update(profileId: string; aRemarketingList : TRemarketingList) : TRemarketingList;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/remarketingLists';
  _Methodid   = 'dfareporting.remarketingLists.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRemarketingList,TRemarketingList) as TRemarketingList;
end;



{ --------------------------------------------------------------------
  TReportsResource
  --------------------------------------------------------------------}


Class Function TReportsResource.ResourceName : String;

begin
  Result:='reports';
end;

Class Function TReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TReportsResource.Delete(profileId: string; reportId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/reports/{reportId}';
  _Methodid   = 'dfareporting.reports.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'reportId',reportId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TReportsResource.Get(profileId: string; reportId: string) : TReport;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/reports/{reportId}';
  _Methodid   = 'dfareporting.reports.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'reportId',reportId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TReport) as TReport;
end;

Function TReportsResource.Insert(profileId: string; aReport : TReport) : TReport;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/reports';
  _Methodid   = 'dfareporting.reports.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReport,TReport) as TReport;
end;

Function TReportsResource.List(profileId: string; AQuery : string = '') : TReportList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/reports';
  _Methodid   = 'dfareporting.reports.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TReportList) as TReportList;
end;


Function TReportsResource.List(profileId: string; AQuery : TReportslistOptions) : TReportList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'scope',AQuery.scope);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TReportsResource.Patch(profileId: string; reportId: string; aReport : TReport) : TReport;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/reports/{reportId}';
  _Methodid   = 'dfareporting.reports.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'reportId',reportId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReport,TReport) as TReport;
end;

Function TReportsResource.Run(profileId: string; reportId: string; AQuery : string = '') : TFile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/reports/{reportId}/run';
  _Methodid   = 'dfareporting.reports.run';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'reportId',reportId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFile) as TFile;
end;


Function TReportsResource.Run(profileId: string; reportId: string; AQuery : TReportsrunOptions) : TFile;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'synchronous',AQuery.synchronous);
  Result:=Run(profileId,reportId,_Q);
end;

Function TReportsResource.Update(profileId: string; reportId: string; aReport : TReport) : TReport;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/reports/{reportId}';
  _Methodid   = 'dfareporting.reports.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId,'reportId',reportId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReport,TReport) as TReport;
end;



{ --------------------------------------------------------------------
  TSitesResource
  --------------------------------------------------------------------}


Class Function TSitesResource.ResourceName : String;

begin
  Result:='sites';
end;

Class Function TSitesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TSitesResource.Get(id: string; profileId: string) : TSite;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/sites/{id}';
  _Methodid   = 'dfareporting.sites.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSite) as TSite;
end;

Function TSitesResource.Insert(profileId: string; aSite : TSite) : TSite;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/sites';
  _Methodid   = 'dfareporting.sites.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSite,TSite) as TSite;
end;

Function TSitesResource.List(profileId: string; AQuery : string = '') : TSitesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/sites';
  _Methodid   = 'dfareporting.sites.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSitesListResponse) as TSitesListResponse;
end;


Function TSitesResource.List(profileId: string; AQuery : TSiteslistOptions) : TSitesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'acceptsInStreamVideoPlacements',AQuery.acceptsInStreamVideoPlacements);
  AddToQuery(_Q,'acceptsInterstitialPlacements',AQuery.acceptsInterstitialPlacements);
  AddToQuery(_Q,'acceptsPublisherPaidPlacements',AQuery.acceptsPublisherPaidPlacements);
  AddToQuery(_Q,'adWordsSite',AQuery.adWordsSite);
  AddToQuery(_Q,'approved',AQuery.approved);
  AddToQuery(_Q,'campaignIds',AQuery.campaignIds);
  AddToQuery(_Q,'directorySiteIds',AQuery.directorySiteIds);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'subaccountId',AQuery.subaccountId);
  AddToQuery(_Q,'unmappedSite',AQuery.unmappedSite);
  Result:=List(profileId,_Q);
end;

Function TSitesResource.Patch(profileId: string; aSite : TSite; AQuery : string = '') : TSite;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/sites';
  _Methodid   = 'dfareporting.sites.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aSite,TSite) as TSite;
end;


Function TSitesResource.Patch(profileId: string; aSite : TSite; AQuery : TSitespatchOptions) : TSite;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aSite,_Q);
end;

Function TSitesResource.Update(profileId: string; aSite : TSite) : TSite;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/sites';
  _Methodid   = 'dfareporting.sites.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSite,TSite) as TSite;
end;



{ --------------------------------------------------------------------
  TSizesResource
  --------------------------------------------------------------------}


Class Function TSizesResource.ResourceName : String;

begin
  Result:='sizes';
end;

Class Function TSizesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TSizesResource.Get(id: string; profileId: string) : TSize;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/sizes/{id}';
  _Methodid   = 'dfareporting.sizes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSize) as TSize;
end;

Function TSizesResource.Insert(profileId: string; aSize : TSize) : TSize;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/sizes';
  _Methodid   = 'dfareporting.sizes.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSize,TSize) as TSize;
end;

Function TSizesResource.List(profileId: string; AQuery : string = '') : TSizesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/sizes';
  _Methodid   = 'dfareporting.sizes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSizesListResponse) as TSizesListResponse;
end;


Function TSizesResource.List(profileId: string; AQuery : TSizeslistOptions) : TSizesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'height',AQuery.height);
  AddToQuery(_Q,'iabStandard',AQuery.iabStandard);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'width',AQuery.width);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TSubaccountsResource
  --------------------------------------------------------------------}


Class Function TSubaccountsResource.ResourceName : String;

begin
  Result:='subaccounts';
end;

Class Function TSubaccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TSubaccountsResource.Get(id: string; profileId: string) : TSubaccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/subaccounts/{id}';
  _Methodid   = 'dfareporting.subaccounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSubaccount) as TSubaccount;
end;

Function TSubaccountsResource.Insert(profileId: string; aSubaccount : TSubaccount) : TSubaccount;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/subaccounts';
  _Methodid   = 'dfareporting.subaccounts.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSubaccount,TSubaccount) as TSubaccount;
end;

Function TSubaccountsResource.List(profileId: string; AQuery : string = '') : TSubaccountsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/subaccounts';
  _Methodid   = 'dfareporting.subaccounts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSubaccountsListResponse) as TSubaccountsListResponse;
end;


Function TSubaccountsResource.List(profileId: string; AQuery : TSubaccountslistOptions) : TSubaccountsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;

Function TSubaccountsResource.Patch(profileId: string; aSubaccount : TSubaccount; AQuery : string = '') : TSubaccount;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/subaccounts';
  _Methodid   = 'dfareporting.subaccounts.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aSubaccount,TSubaccount) as TSubaccount;
end;


Function TSubaccountsResource.Patch(profileId: string; aSubaccount : TSubaccount; AQuery : TSubaccountspatchOptions) : TSubaccount;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aSubaccount,_Q);
end;

Function TSubaccountsResource.Update(profileId: string; aSubaccount : TSubaccount) : TSubaccount;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/subaccounts';
  _Methodid   = 'dfareporting.subaccounts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSubaccount,TSubaccount) as TSubaccount;
end;



{ --------------------------------------------------------------------
  TTargetableRemarketingListsResource
  --------------------------------------------------------------------}


Class Function TTargetableRemarketingListsResource.ResourceName : String;

begin
  Result:='targetableRemarketingLists';
end;

Class Function TTargetableRemarketingListsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TTargetableRemarketingListsResource.Get(id: string; profileId: string) : TTargetableRemarketingList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/targetableRemarketingLists/{id}';
  _Methodid   = 'dfareporting.targetableRemarketingLists.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTargetableRemarketingList) as TTargetableRemarketingList;
end;

Function TTargetableRemarketingListsResource.List(profileId: string; AQuery : string = '') : TTargetableRemarketingListsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/targetableRemarketingLists';
  _Methodid   = 'dfareporting.targetableRemarketingLists.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTargetableRemarketingListsListResponse) as TTargetableRemarketingListsListResponse;
end;


Function TTargetableRemarketingListsResource.List(profileId: string; AQuery : TTargetableRemarketingListslistOptions) : TTargetableRemarketingListsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'active',AQuery.active);
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'name',AQuery._name);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TUserProfilesResource
  --------------------------------------------------------------------}


Class Function TUserProfilesResource.ResourceName : String;

begin
  Result:='userProfiles';
end;

Class Function TUserProfilesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TUserProfilesResource.Get(profileId: string) : TUserProfile;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}';
  _Methodid   = 'dfareporting.userProfiles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUserProfile) as TUserProfile;
end;

Function TUserProfilesResource.List : TUserProfileList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles';
  _Methodid   = 'dfareporting.userProfiles.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TUserProfileList) as TUserProfileList;
end;



{ --------------------------------------------------------------------
  TUserRolePermissionGroupsResource
  --------------------------------------------------------------------}


Class Function TUserRolePermissionGroupsResource.ResourceName : String;

begin
  Result:='userRolePermissionGroups';
end;

Class Function TUserRolePermissionGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TUserRolePermissionGroupsResource.Get(id: string; profileId: string) : TUserRolePermissionGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/userRolePermissionGroups/{id}';
  _Methodid   = 'dfareporting.userRolePermissionGroups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUserRolePermissionGroup) as TUserRolePermissionGroup;
end;

Function TUserRolePermissionGroupsResource.List(profileId: string) : TUserRolePermissionGroupsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/userRolePermissionGroups';
  _Methodid   = 'dfareporting.userRolePermissionGroups.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUserRolePermissionGroupsListResponse) as TUserRolePermissionGroupsListResponse;
end;



{ --------------------------------------------------------------------
  TUserRolePermissionsResource
  --------------------------------------------------------------------}


Class Function TUserRolePermissionsResource.ResourceName : String;

begin
  Result:='userRolePermissions';
end;

Class Function TUserRolePermissionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Function TUserRolePermissionsResource.Get(id: string; profileId: string) : TUserRolePermission;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/userRolePermissions/{id}';
  _Methodid   = 'dfareporting.userRolePermissions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUserRolePermission) as TUserRolePermission;
end;

Function TUserRolePermissionsResource.List(profileId: string; AQuery : string = '') : TUserRolePermissionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/userRolePermissions';
  _Methodid   = 'dfareporting.userRolePermissions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUserRolePermissionsListResponse) as TUserRolePermissionsListResponse;
end;


Function TUserRolePermissionsResource.List(profileId: string; AQuery : TUserRolePermissionslistOptions) : TUserRolePermissionsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ids',AQuery.ids);
  Result:=List(profileId,_Q);
end;



{ --------------------------------------------------------------------
  TUserRolesResource
  --------------------------------------------------------------------}


Class Function TUserRolesResource.ResourceName : String;

begin
  Result:='userRoles';
end;

Class Function TUserRolesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdfareportingAPI;
end;

Procedure TUserRolesResource.Delete(id: string; profileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'userprofiles/{profileId}/userRoles/{id}';
  _Methodid   = 'dfareporting.userRoles.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TUserRolesResource.Get(id: string; profileId: string) : TUserRole;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/userRoles/{id}';
  _Methodid   = 'dfareporting.userRoles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUserRole) as TUserRole;
end;

Function TUserRolesResource.Insert(profileId: string; aUserRole : TUserRole) : TUserRole;

Const
  _HTTPMethod = 'POST';
  _Path       = 'userprofiles/{profileId}/userRoles';
  _Methodid   = 'dfareporting.userRoles.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUserRole,TUserRole) as TUserRole;
end;

Function TUserRolesResource.List(profileId: string; AQuery : string = '') : TUserRolesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'userprofiles/{profileId}/userRoles';
  _Methodid   = 'dfareporting.userRoles.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUserRolesListResponse) as TUserRolesListResponse;
end;


Function TUserRolesResource.List(profileId: string; AQuery : TUserRoleslistOptions) : TUserRolesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'accountUserRoleOnly',AQuery.accountUserRoleOnly);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'searchString',AQuery.searchString);
  AddToQuery(_Q,'sortField',AQuery.sortField);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'subaccountId',AQuery.subaccountId);
  Result:=List(profileId,_Q);
end;

Function TUserRolesResource.Patch(profileId: string; aUserRole : TUserRole; AQuery : string = '') : TUserRole;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'userprofiles/{profileId}/userRoles';
  _Methodid   = 'dfareporting.userRoles.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aUserRole,TUserRole) as TUserRole;
end;


Function TUserRolesResource.Patch(profileId: string; aUserRole : TUserRole; AQuery : TUserRolespatchOptions) : TUserRole;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  Result:=Patch(profileId,aUserRole,_Q);
end;

Function TUserRolesResource.Update(profileId: string; aUserRole : TUserRole) : TUserRole;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'userprofiles/{profileId}/userRoles';
  _Methodid   = 'dfareporting.userRoles.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['profileId',profileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUserRole,TUserRole) as TUserRole;
end;



{ --------------------------------------------------------------------
  TDfareportingAPI
  --------------------------------------------------------------------}

Class Function TDfareportingAPI.APIName : String;

begin
  Result:='dfareporting';
end;

Class Function TDfareportingAPI.APIVersion : String;

begin
  Result:='v2.1';
end;

Class Function TDfareportingAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TDfareportingAPI.APIID : String;

begin
  Result:='dfareporting:v2.1';
end;

Class Function TDfareportingAPI.APITitle : String;

begin
  Result:='DCM/DFA Reporting And Trafficking API';
end;

Class Function TDfareportingAPI.APIDescription : String;

begin
  Result:='Manage your DoubleClick Campaign Manager ad campaigns and reports.';
end;

Class Function TDfareportingAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDfareportingAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDfareportingAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/doubleclick-16.gif';
end;

Class Function TDfareportingAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/doubleclick-32.gif';
end;

Class Function TDfareportingAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/doubleclick-advertisers/reporting/';
end;

Class Function TDfareportingAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TDfareportingAPI.APIbasePath : string;

begin
  Result:='/dfareporting/v2.1/';
end;

Class Function TDfareportingAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/dfareporting/v2.1/';
end;

Class Function TDfareportingAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDfareportingAPI.APIservicePath : string;

begin
  Result:='dfareporting/v2.1/';
end;

Class Function TDfareportingAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDfareportingAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/dfareporting';
  Result[0].Description:='View and manage DoubleClick for Advertisers reports';
  Result[1].Name:='https://www.googleapis.com/auth/dfatrafficking';
  Result[1].Description:='View and manage your DoubleClick Campaign Manager''s (DCM) display ad campaigns';
  
end;

Class Function TDfareportingAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDfareportingAPI.RegisterAPIResources;

begin
  TAccount.RegisterObject;
  TAccountActiveAdSummary.RegisterObject;
  TAccountPermission.RegisterObject;
  TAccountPermissionGroup.RegisterObject;
  TAccountPermissionGroupsListResponse.RegisterObject;
  TAccountPermissionsListResponse.RegisterObject;
  TAccountUserProfile.RegisterObject;
  TAccountUserProfilesListResponse.RegisterObject;
  TAccountsListResponse.RegisterObject;
  TActivities.RegisterObject;
  TAd.RegisterObject;
  TAdSlot.RegisterObject;
  TAdsListResponse.RegisterObject;
  TAdvertiser.RegisterObject;
  TAdvertiserGroup.RegisterObject;
  TAdvertiserGroupsListResponse.RegisterObject;
  TAdvertisersListResponse.RegisterObject;
  TAudienceSegment.RegisterObject;
  TAudienceSegmentGroup.RegisterObject;
  TBrowser.RegisterObject;
  TBrowsersListResponse.RegisterObject;
  TCampaign.RegisterObject;
  TCampaignCreativeAssociation.RegisterObject;
  TCampaignCreativeAssociationsListResponse.RegisterObject;
  TCampaignsListResponse.RegisterObject;
  TChangeLog.RegisterObject;
  TChangeLogsListResponse.RegisterObject;
  TCitiesListResponse.RegisterObject;
  TCity.RegisterObject;
  TClickTag.RegisterObject;
  TClickThroughUrl.RegisterObject;
  TClickThroughUrlSuffixProperties.RegisterObject;
  TCompanionClickThroughOverride.RegisterObject;
  TCompatibleFields.RegisterObject;
  TConnectionType.RegisterObject;
  TConnectionTypesListResponse.RegisterObject;
  TContentCategoriesListResponse.RegisterObject;
  TContentCategory.RegisterObject;
  TCountriesListResponse.RegisterObject;
  TCountry.RegisterObject;
  TCreative.RegisterObject;
  TCreativeAsset.RegisterObject;
  TCreativeAssetId.RegisterObject;
  TCreativeAssetMetadata.RegisterObject;
  TCreativeAssignment.RegisterObject;
  TCreativeCustomEvent.RegisterObject;
  TCreativeField.RegisterObject;
  TCreativeFieldAssignment.RegisterObject;
  TCreativeFieldValue.RegisterObject;
  TCreativeFieldValuesListResponse.RegisterObject;
  TCreativeFieldsListResponse.RegisterObject;
  TCreativeGroup.RegisterObject;
  TCreativeGroupAssignment.RegisterObject;
  TCreativeGroupsListResponse.RegisterObject;
  TCreativeOptimizationConfiguration.RegisterObject;
  TCreativeRotation.RegisterObject;
  TCreativeSettings.RegisterObject;
  TCreativesListResponse.RegisterObject;
  TCrossDimensionReachReportCompatibleFields.RegisterObject;
  TCustomRichMediaEvents.RegisterObject;
  TDateRange.RegisterObject;
  TDayPartTargeting.RegisterObject;
  TDefaultClickThroughEventTagProperties.RegisterObject;
  TDeliverySchedule.RegisterObject;
  TDfpSettings.RegisterObject;
  TDimension.RegisterObject;
  TDimensionFilter.RegisterObject;
  TDimensionValue.RegisterObject;
  TDimensionValueList.RegisterObject;
  TDimensionValueRequest.RegisterObject;
  TDirectorySite.RegisterObject;
  TDirectorySiteContact.RegisterObject;
  TDirectorySiteContactAssignment.RegisterObject;
  TDirectorySiteContactsListResponse.RegisterObject;
  TDirectorySiteSettings.RegisterObject;
  TDirectorySitesListResponse.RegisterObject;
  TEventTag.RegisterObject;
  TEventTagOverride.RegisterObject;
  TEventTagsListResponse.RegisterObject;
  TFileTypeurls.RegisterObject;
  TFile.RegisterObject;
  TFileList.RegisterObject;
  TFlight.RegisterObject;
  TFloodlightActivitiesGenerateTagResponse.RegisterObject;
  TFloodlightActivitiesListResponse.RegisterObject;
  TFloodlightActivity.RegisterObject;
  TFloodlightActivityDynamicTag.RegisterObject;
  TFloodlightActivityGroup.RegisterObject;
  TFloodlightActivityGroupsListResponse.RegisterObject;
  TFloodlightActivityPublisherDynamicTag.RegisterObject;
  TFloodlightConfiguration.RegisterObject;
  TFloodlightConfigurationsListResponse.RegisterObject;
  TFloodlightReportCompatibleFields.RegisterObject;
  TFrequencyCap.RegisterObject;
  TFsCommand.RegisterObject;
  TGeoTargeting.RegisterObject;
  TInventoryItem.RegisterObject;
  TInventoryItemsListResponse.RegisterObject;
  TKeyValueTargetingExpression.RegisterObject;
  TLandingPage.RegisterObject;
  TLandingPagesListResponse.RegisterObject;
  TLastModifiedInfo.RegisterObject;
  TListPopulationClause.RegisterObject;
  TListPopulationRule.RegisterObject;
  TListPopulationTerm.RegisterObject;
  TListTargetingExpression.RegisterObject;
  TLookbackConfiguration.RegisterObject;
  TMetric.RegisterObject;
  TMetro.RegisterObject;
  TMetrosListResponse.RegisterObject;
  TMobileCarrier.RegisterObject;
  TMobileCarriersListResponse.RegisterObject;
  TObjectFilter.RegisterObject;
  TOffsetPosition.RegisterObject;
  TOmnitureSettings.RegisterObject;
  TOperatingSystem.RegisterObject;
  TOperatingSystemVersion.RegisterObject;
  TOperatingSystemVersionsListResponse.RegisterObject;
  TOperatingSystemsListResponse.RegisterObject;
  TOptimizationActivity.RegisterObject;
  TOrder.RegisterObject;
  TOrderContact.RegisterObject;
  TOrderDocument.RegisterObject;
  TOrderDocumentsListResponse.RegisterObject;
  TOrdersListResponse.RegisterObject;
  TPathToConversionReportCompatibleFields.RegisterObject;
  TPlacement.RegisterObject;
  TPlacementAssignment.RegisterObject;
  TPlacementGroup.RegisterObject;
  TPlacementGroupsListResponse.RegisterObject;
  TPlacementStrategiesListResponse.RegisterObject;
  TPlacementStrategy.RegisterObject;
  TPlacementTag.RegisterObject;
  TPlacementsGenerateTagsResponse.RegisterObject;
  TPlacementsListResponse.RegisterObject;
  TPlatformType.RegisterObject;
  TPlatformTypesListResponse.RegisterObject;
  TPopupWindowProperties.RegisterObject;
  TPostalCode.RegisterObject;
  TPostalCodesListResponse.RegisterObject;
  TPricing.RegisterObject;
  TPricingSchedule.RegisterObject;
  TPricingSchedulePricingPeriod.RegisterObject;
  TProgrammaticSetting.RegisterObject;
  TProject.RegisterObject;
  TProjectsListResponse.RegisterObject;
  TReachReportCompatibleFields.RegisterObject;
  TRecipient.RegisterObject;
  TRegion.RegisterObject;
  TRegionsListResponse.RegisterObject;
  TRemarketingList.RegisterObject;
  TRemarketingListShare.RegisterObject;
  TRemarketingListsListResponse.RegisterObject;
  TReportTypecriteria.RegisterObject;
  TReportTypecrossDimensionReachCriteria.RegisterObject;
  TReportTypedelivery.RegisterObject;
  TReportTypefloodlightCriteriaTypereportProperties.RegisterObject;
  TReportTypefloodlightCriteria.RegisterObject;
  TReportTypepathToConversionCriteriaTypereportProperties.RegisterObject;
  TReportTypepathToConversionCriteria.RegisterObject;
  TReportTypereachCriteria.RegisterObject;
  TReportTypeschedule.RegisterObject;
  TReport.RegisterObject;
  TReportCompatibleFields.RegisterObject;
  TReportList.RegisterObject;
  TReportsConfiguration.RegisterObject;
  TRichMediaExitOverride.RegisterObject;
  TSite.RegisterObject;
  TSiteContact.RegisterObject;
  TSiteSettings.RegisterObject;
  TSitesListResponse.RegisterObject;
  TSize.RegisterObject;
  TSizesListResponse.RegisterObject;
  TSortedDimension.RegisterObject;
  TSubaccount.RegisterObject;
  TSubaccountsListResponse.RegisterObject;
  TTagData.RegisterObject;
  TTagSetting.RegisterObject;
  TTagSettings.RegisterObject;
  TTargetWindow.RegisterObject;
  TTargetableRemarketingList.RegisterObject;
  TTargetableRemarketingListsListResponse.RegisterObject;
  TTechnologyTargeting.RegisterObject;
  TThirdPartyTrackingUrl.RegisterObject;
  TUserDefinedVariableConfiguration.RegisterObject;
  TUserProfile.RegisterObject;
  TUserProfileList.RegisterObject;
  TUserRole.RegisterObject;
  TUserRolePermission.RegisterObject;
  TUserRolePermissionGroup.RegisterObject;
  TUserRolePermissionGroupsListResponse.RegisterObject;
  TUserRolePermissionsListResponse.RegisterObject;
  TUserRolesListResponse.RegisterObject;
end;


Function TDfareportingAPI.GetAccountActiveAdSummariesInstance : TAccountActiveAdSummariesResource;

begin
  if (FAccountActiveAdSummariesInstance=Nil) then
    FAccountActiveAdSummariesInstance:=CreateAccountActiveAdSummariesResource;
  Result:=FAccountActiveAdSummariesInstance;
end;

Function TDfareportingAPI.CreateAccountActiveAdSummariesResource : TAccountActiveAdSummariesResource;

begin
  Result:=CreateAccountActiveAdSummariesResource(Self);
end;


Function TDfareportingAPI.CreateAccountActiveAdSummariesResource(AOwner : TComponent) : TAccountActiveAdSummariesResource;

begin
  Result:=TAccountActiveAdSummariesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetAccountPermissionGroupsInstance : TAccountPermissionGroupsResource;

begin
  if (FAccountPermissionGroupsInstance=Nil) then
    FAccountPermissionGroupsInstance:=CreateAccountPermissionGroupsResource;
  Result:=FAccountPermissionGroupsInstance;
end;

Function TDfareportingAPI.CreateAccountPermissionGroupsResource : TAccountPermissionGroupsResource;

begin
  Result:=CreateAccountPermissionGroupsResource(Self);
end;


Function TDfareportingAPI.CreateAccountPermissionGroupsResource(AOwner : TComponent) : TAccountPermissionGroupsResource;

begin
  Result:=TAccountPermissionGroupsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetAccountPermissionsInstance : TAccountPermissionsResource;

begin
  if (FAccountPermissionsInstance=Nil) then
    FAccountPermissionsInstance:=CreateAccountPermissionsResource;
  Result:=FAccountPermissionsInstance;
end;

Function TDfareportingAPI.CreateAccountPermissionsResource : TAccountPermissionsResource;

begin
  Result:=CreateAccountPermissionsResource(Self);
end;


Function TDfareportingAPI.CreateAccountPermissionsResource(AOwner : TComponent) : TAccountPermissionsResource;

begin
  Result:=TAccountPermissionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetAccountUserProfilesInstance : TAccountUserProfilesResource;

begin
  if (FAccountUserProfilesInstance=Nil) then
    FAccountUserProfilesInstance:=CreateAccountUserProfilesResource;
  Result:=FAccountUserProfilesInstance;
end;

Function TDfareportingAPI.CreateAccountUserProfilesResource : TAccountUserProfilesResource;

begin
  Result:=CreateAccountUserProfilesResource(Self);
end;


Function TDfareportingAPI.CreateAccountUserProfilesResource(AOwner : TComponent) : TAccountUserProfilesResource;

begin
  Result:=TAccountUserProfilesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TDfareportingAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TDfareportingAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetAdsInstance : TAdsResource;

begin
  if (FAdsInstance=Nil) then
    FAdsInstance:=CreateAdsResource;
  Result:=FAdsInstance;
end;

Function TDfareportingAPI.CreateAdsResource : TAdsResource;

begin
  Result:=CreateAdsResource(Self);
end;


Function TDfareportingAPI.CreateAdsResource(AOwner : TComponent) : TAdsResource;

begin
  Result:=TAdsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetAdvertiserGroupsInstance : TAdvertiserGroupsResource;

begin
  if (FAdvertiserGroupsInstance=Nil) then
    FAdvertiserGroupsInstance:=CreateAdvertiserGroupsResource;
  Result:=FAdvertiserGroupsInstance;
end;

Function TDfareportingAPI.CreateAdvertiserGroupsResource : TAdvertiserGroupsResource;

begin
  Result:=CreateAdvertiserGroupsResource(Self);
end;


Function TDfareportingAPI.CreateAdvertiserGroupsResource(AOwner : TComponent) : TAdvertiserGroupsResource;

begin
  Result:=TAdvertiserGroupsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetAdvertisersInstance : TAdvertisersResource;

begin
  if (FAdvertisersInstance=Nil) then
    FAdvertisersInstance:=CreateAdvertisersResource;
  Result:=FAdvertisersInstance;
end;

Function TDfareportingAPI.CreateAdvertisersResource : TAdvertisersResource;

begin
  Result:=CreateAdvertisersResource(Self);
end;


Function TDfareportingAPI.CreateAdvertisersResource(AOwner : TComponent) : TAdvertisersResource;

begin
  Result:=TAdvertisersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetBrowsersInstance : TBrowsersResource;

begin
  if (FBrowsersInstance=Nil) then
    FBrowsersInstance:=CreateBrowsersResource;
  Result:=FBrowsersInstance;
end;

Function TDfareportingAPI.CreateBrowsersResource : TBrowsersResource;

begin
  Result:=CreateBrowsersResource(Self);
end;


Function TDfareportingAPI.CreateBrowsersResource(AOwner : TComponent) : TBrowsersResource;

begin
  Result:=TBrowsersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCampaignCreativeAssociationsInstance : TCampaignCreativeAssociationsResource;

begin
  if (FCampaignCreativeAssociationsInstance=Nil) then
    FCampaignCreativeAssociationsInstance:=CreateCampaignCreativeAssociationsResource;
  Result:=FCampaignCreativeAssociationsInstance;
end;

Function TDfareportingAPI.CreateCampaignCreativeAssociationsResource : TCampaignCreativeAssociationsResource;

begin
  Result:=CreateCampaignCreativeAssociationsResource(Self);
end;


Function TDfareportingAPI.CreateCampaignCreativeAssociationsResource(AOwner : TComponent) : TCampaignCreativeAssociationsResource;

begin
  Result:=TCampaignCreativeAssociationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCampaignsInstance : TCampaignsResource;

begin
  if (FCampaignsInstance=Nil) then
    FCampaignsInstance:=CreateCampaignsResource;
  Result:=FCampaignsInstance;
end;

Function TDfareportingAPI.CreateCampaignsResource : TCampaignsResource;

begin
  Result:=CreateCampaignsResource(Self);
end;


Function TDfareportingAPI.CreateCampaignsResource(AOwner : TComponent) : TCampaignsResource;

begin
  Result:=TCampaignsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetChangeLogsInstance : TChangeLogsResource;

begin
  if (FChangeLogsInstance=Nil) then
    FChangeLogsInstance:=CreateChangeLogsResource;
  Result:=FChangeLogsInstance;
end;

Function TDfareportingAPI.CreateChangeLogsResource : TChangeLogsResource;

begin
  Result:=CreateChangeLogsResource(Self);
end;


Function TDfareportingAPI.CreateChangeLogsResource(AOwner : TComponent) : TChangeLogsResource;

begin
  Result:=TChangeLogsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCitiesInstance : TCitiesResource;

begin
  if (FCitiesInstance=Nil) then
    FCitiesInstance:=CreateCitiesResource;
  Result:=FCitiesInstance;
end;

Function TDfareportingAPI.CreateCitiesResource : TCitiesResource;

begin
  Result:=CreateCitiesResource(Self);
end;


Function TDfareportingAPI.CreateCitiesResource(AOwner : TComponent) : TCitiesResource;

begin
  Result:=TCitiesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetConnectionTypesInstance : TConnectionTypesResource;

begin
  if (FConnectionTypesInstance=Nil) then
    FConnectionTypesInstance:=CreateConnectionTypesResource;
  Result:=FConnectionTypesInstance;
end;

Function TDfareportingAPI.CreateConnectionTypesResource : TConnectionTypesResource;

begin
  Result:=CreateConnectionTypesResource(Self);
end;


Function TDfareportingAPI.CreateConnectionTypesResource(AOwner : TComponent) : TConnectionTypesResource;

begin
  Result:=TConnectionTypesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetContentCategoriesInstance : TContentCategoriesResource;

begin
  if (FContentCategoriesInstance=Nil) then
    FContentCategoriesInstance:=CreateContentCategoriesResource;
  Result:=FContentCategoriesInstance;
end;

Function TDfareportingAPI.CreateContentCategoriesResource : TContentCategoriesResource;

begin
  Result:=CreateContentCategoriesResource(Self);
end;


Function TDfareportingAPI.CreateContentCategoriesResource(AOwner : TComponent) : TContentCategoriesResource;

begin
  Result:=TContentCategoriesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCountriesInstance : TCountriesResource;

begin
  if (FCountriesInstance=Nil) then
    FCountriesInstance:=CreateCountriesResource;
  Result:=FCountriesInstance;
end;

Function TDfareportingAPI.CreateCountriesResource : TCountriesResource;

begin
  Result:=CreateCountriesResource(Self);
end;


Function TDfareportingAPI.CreateCountriesResource(AOwner : TComponent) : TCountriesResource;

begin
  Result:=TCountriesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCreativeAssetsInstance : TCreativeAssetsResource;

begin
  if (FCreativeAssetsInstance=Nil) then
    FCreativeAssetsInstance:=CreateCreativeAssetsResource;
  Result:=FCreativeAssetsInstance;
end;

Function TDfareportingAPI.CreateCreativeAssetsResource : TCreativeAssetsResource;

begin
  Result:=CreateCreativeAssetsResource(Self);
end;


Function TDfareportingAPI.CreateCreativeAssetsResource(AOwner : TComponent) : TCreativeAssetsResource;

begin
  Result:=TCreativeAssetsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCreativeFieldValuesInstance : TCreativeFieldValuesResource;

begin
  if (FCreativeFieldValuesInstance=Nil) then
    FCreativeFieldValuesInstance:=CreateCreativeFieldValuesResource;
  Result:=FCreativeFieldValuesInstance;
end;

Function TDfareportingAPI.CreateCreativeFieldValuesResource : TCreativeFieldValuesResource;

begin
  Result:=CreateCreativeFieldValuesResource(Self);
end;


Function TDfareportingAPI.CreateCreativeFieldValuesResource(AOwner : TComponent) : TCreativeFieldValuesResource;

begin
  Result:=TCreativeFieldValuesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCreativeFieldsInstance : TCreativeFieldsResource;

begin
  if (FCreativeFieldsInstance=Nil) then
    FCreativeFieldsInstance:=CreateCreativeFieldsResource;
  Result:=FCreativeFieldsInstance;
end;

Function TDfareportingAPI.CreateCreativeFieldsResource : TCreativeFieldsResource;

begin
  Result:=CreateCreativeFieldsResource(Self);
end;


Function TDfareportingAPI.CreateCreativeFieldsResource(AOwner : TComponent) : TCreativeFieldsResource;

begin
  Result:=TCreativeFieldsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCreativeGroupsInstance : TCreativeGroupsResource;

begin
  if (FCreativeGroupsInstance=Nil) then
    FCreativeGroupsInstance:=CreateCreativeGroupsResource;
  Result:=FCreativeGroupsInstance;
end;

Function TDfareportingAPI.CreateCreativeGroupsResource : TCreativeGroupsResource;

begin
  Result:=CreateCreativeGroupsResource(Self);
end;


Function TDfareportingAPI.CreateCreativeGroupsResource(AOwner : TComponent) : TCreativeGroupsResource;

begin
  Result:=TCreativeGroupsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetCreativesInstance : TCreativesResource;

begin
  if (FCreativesInstance=Nil) then
    FCreativesInstance:=CreateCreativesResource;
  Result:=FCreativesInstance;
end;

Function TDfareportingAPI.CreateCreativesResource : TCreativesResource;

begin
  Result:=CreateCreativesResource(Self);
end;


Function TDfareportingAPI.CreateCreativesResource(AOwner : TComponent) : TCreativesResource;

begin
  Result:=TCreativesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetDimensionValuesInstance : TDimensionValuesResource;

begin
  if (FDimensionValuesInstance=Nil) then
    FDimensionValuesInstance:=CreateDimensionValuesResource;
  Result:=FDimensionValuesInstance;
end;

Function TDfareportingAPI.CreateDimensionValuesResource : TDimensionValuesResource;

begin
  Result:=CreateDimensionValuesResource(Self);
end;


Function TDfareportingAPI.CreateDimensionValuesResource(AOwner : TComponent) : TDimensionValuesResource;

begin
  Result:=TDimensionValuesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetDirectorySiteContactsInstance : TDirectorySiteContactsResource;

begin
  if (FDirectorySiteContactsInstance=Nil) then
    FDirectorySiteContactsInstance:=CreateDirectorySiteContactsResource;
  Result:=FDirectorySiteContactsInstance;
end;

Function TDfareportingAPI.CreateDirectorySiteContactsResource : TDirectorySiteContactsResource;

begin
  Result:=CreateDirectorySiteContactsResource(Self);
end;


Function TDfareportingAPI.CreateDirectorySiteContactsResource(AOwner : TComponent) : TDirectorySiteContactsResource;

begin
  Result:=TDirectorySiteContactsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetDirectorySitesInstance : TDirectorySitesResource;

begin
  if (FDirectorySitesInstance=Nil) then
    FDirectorySitesInstance:=CreateDirectorySitesResource;
  Result:=FDirectorySitesInstance;
end;

Function TDfareportingAPI.CreateDirectorySitesResource : TDirectorySitesResource;

begin
  Result:=CreateDirectorySitesResource(Self);
end;


Function TDfareportingAPI.CreateDirectorySitesResource(AOwner : TComponent) : TDirectorySitesResource;

begin
  Result:=TDirectorySitesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetEventTagsInstance : TEventTagsResource;

begin
  if (FEventTagsInstance=Nil) then
    FEventTagsInstance:=CreateEventTagsResource;
  Result:=FEventTagsInstance;
end;

Function TDfareportingAPI.CreateEventTagsResource : TEventTagsResource;

begin
  Result:=CreateEventTagsResource(Self);
end;


Function TDfareportingAPI.CreateEventTagsResource(AOwner : TComponent) : TEventTagsResource;

begin
  Result:=TEventTagsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetFilesInstance : TFilesResource;

begin
  if (FFilesInstance=Nil) then
    FFilesInstance:=CreateFilesResource;
  Result:=FFilesInstance;
end;

Function TDfareportingAPI.CreateFilesResource : TFilesResource;

begin
  Result:=CreateFilesResource(Self);
end;


Function TDfareportingAPI.CreateFilesResource(AOwner : TComponent) : TFilesResource;

begin
  Result:=TFilesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetFloodlightActivitiesInstance : TFloodlightActivitiesResource;

begin
  if (FFloodlightActivitiesInstance=Nil) then
    FFloodlightActivitiesInstance:=CreateFloodlightActivitiesResource;
  Result:=FFloodlightActivitiesInstance;
end;

Function TDfareportingAPI.CreateFloodlightActivitiesResource : TFloodlightActivitiesResource;

begin
  Result:=CreateFloodlightActivitiesResource(Self);
end;


Function TDfareportingAPI.CreateFloodlightActivitiesResource(AOwner : TComponent) : TFloodlightActivitiesResource;

begin
  Result:=TFloodlightActivitiesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetFloodlightActivityGroupsInstance : TFloodlightActivityGroupsResource;

begin
  if (FFloodlightActivityGroupsInstance=Nil) then
    FFloodlightActivityGroupsInstance:=CreateFloodlightActivityGroupsResource;
  Result:=FFloodlightActivityGroupsInstance;
end;

Function TDfareportingAPI.CreateFloodlightActivityGroupsResource : TFloodlightActivityGroupsResource;

begin
  Result:=CreateFloodlightActivityGroupsResource(Self);
end;


Function TDfareportingAPI.CreateFloodlightActivityGroupsResource(AOwner : TComponent) : TFloodlightActivityGroupsResource;

begin
  Result:=TFloodlightActivityGroupsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetFloodlightConfigurationsInstance : TFloodlightConfigurationsResource;

begin
  if (FFloodlightConfigurationsInstance=Nil) then
    FFloodlightConfigurationsInstance:=CreateFloodlightConfigurationsResource;
  Result:=FFloodlightConfigurationsInstance;
end;

Function TDfareportingAPI.CreateFloodlightConfigurationsResource : TFloodlightConfigurationsResource;

begin
  Result:=CreateFloodlightConfigurationsResource(Self);
end;


Function TDfareportingAPI.CreateFloodlightConfigurationsResource(AOwner : TComponent) : TFloodlightConfigurationsResource;

begin
  Result:=TFloodlightConfigurationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetInventoryItemsInstance : TInventoryItemsResource;

begin
  if (FInventoryItemsInstance=Nil) then
    FInventoryItemsInstance:=CreateInventoryItemsResource;
  Result:=FInventoryItemsInstance;
end;

Function TDfareportingAPI.CreateInventoryItemsResource : TInventoryItemsResource;

begin
  Result:=CreateInventoryItemsResource(Self);
end;


Function TDfareportingAPI.CreateInventoryItemsResource(AOwner : TComponent) : TInventoryItemsResource;

begin
  Result:=TInventoryItemsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetLandingPagesInstance : TLandingPagesResource;

begin
  if (FLandingPagesInstance=Nil) then
    FLandingPagesInstance:=CreateLandingPagesResource;
  Result:=FLandingPagesInstance;
end;

Function TDfareportingAPI.CreateLandingPagesResource : TLandingPagesResource;

begin
  Result:=CreateLandingPagesResource(Self);
end;


Function TDfareportingAPI.CreateLandingPagesResource(AOwner : TComponent) : TLandingPagesResource;

begin
  Result:=TLandingPagesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetMetrosInstance : TMetrosResource;

begin
  if (FMetrosInstance=Nil) then
    FMetrosInstance:=CreateMetrosResource;
  Result:=FMetrosInstance;
end;

Function TDfareportingAPI.CreateMetrosResource : TMetrosResource;

begin
  Result:=CreateMetrosResource(Self);
end;


Function TDfareportingAPI.CreateMetrosResource(AOwner : TComponent) : TMetrosResource;

begin
  Result:=TMetrosResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetMobileCarriersInstance : TMobileCarriersResource;

begin
  if (FMobileCarriersInstance=Nil) then
    FMobileCarriersInstance:=CreateMobileCarriersResource;
  Result:=FMobileCarriersInstance;
end;

Function TDfareportingAPI.CreateMobileCarriersResource : TMobileCarriersResource;

begin
  Result:=CreateMobileCarriersResource(Self);
end;


Function TDfareportingAPI.CreateMobileCarriersResource(AOwner : TComponent) : TMobileCarriersResource;

begin
  Result:=TMobileCarriersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetOperatingSystemVersionsInstance : TOperatingSystemVersionsResource;

begin
  if (FOperatingSystemVersionsInstance=Nil) then
    FOperatingSystemVersionsInstance:=CreateOperatingSystemVersionsResource;
  Result:=FOperatingSystemVersionsInstance;
end;

Function TDfareportingAPI.CreateOperatingSystemVersionsResource : TOperatingSystemVersionsResource;

begin
  Result:=CreateOperatingSystemVersionsResource(Self);
end;


Function TDfareportingAPI.CreateOperatingSystemVersionsResource(AOwner : TComponent) : TOperatingSystemVersionsResource;

begin
  Result:=TOperatingSystemVersionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetOperatingSystemsInstance : TOperatingSystemsResource;

begin
  if (FOperatingSystemsInstance=Nil) then
    FOperatingSystemsInstance:=CreateOperatingSystemsResource;
  Result:=FOperatingSystemsInstance;
end;

Function TDfareportingAPI.CreateOperatingSystemsResource : TOperatingSystemsResource;

begin
  Result:=CreateOperatingSystemsResource(Self);
end;


Function TDfareportingAPI.CreateOperatingSystemsResource(AOwner : TComponent) : TOperatingSystemsResource;

begin
  Result:=TOperatingSystemsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetOrderDocumentsInstance : TOrderDocumentsResource;

begin
  if (FOrderDocumentsInstance=Nil) then
    FOrderDocumentsInstance:=CreateOrderDocumentsResource;
  Result:=FOrderDocumentsInstance;
end;

Function TDfareportingAPI.CreateOrderDocumentsResource : TOrderDocumentsResource;

begin
  Result:=CreateOrderDocumentsResource(Self);
end;


Function TDfareportingAPI.CreateOrderDocumentsResource(AOwner : TComponent) : TOrderDocumentsResource;

begin
  Result:=TOrderDocumentsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetOrdersInstance : TOrdersResource;

begin
  if (FOrdersInstance=Nil) then
    FOrdersInstance:=CreateOrdersResource;
  Result:=FOrdersInstance;
end;

Function TDfareportingAPI.CreateOrdersResource : TOrdersResource;

begin
  Result:=CreateOrdersResource(Self);
end;


Function TDfareportingAPI.CreateOrdersResource(AOwner : TComponent) : TOrdersResource;

begin
  Result:=TOrdersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetPlacementGroupsInstance : TPlacementGroupsResource;

begin
  if (FPlacementGroupsInstance=Nil) then
    FPlacementGroupsInstance:=CreatePlacementGroupsResource;
  Result:=FPlacementGroupsInstance;
end;

Function TDfareportingAPI.CreatePlacementGroupsResource : TPlacementGroupsResource;

begin
  Result:=CreatePlacementGroupsResource(Self);
end;


Function TDfareportingAPI.CreatePlacementGroupsResource(AOwner : TComponent) : TPlacementGroupsResource;

begin
  Result:=TPlacementGroupsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetPlacementStrategiesInstance : TPlacementStrategiesResource;

begin
  if (FPlacementStrategiesInstance=Nil) then
    FPlacementStrategiesInstance:=CreatePlacementStrategiesResource;
  Result:=FPlacementStrategiesInstance;
end;

Function TDfareportingAPI.CreatePlacementStrategiesResource : TPlacementStrategiesResource;

begin
  Result:=CreatePlacementStrategiesResource(Self);
end;


Function TDfareportingAPI.CreatePlacementStrategiesResource(AOwner : TComponent) : TPlacementStrategiesResource;

begin
  Result:=TPlacementStrategiesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetPlacementsInstance : TPlacementsResource;

begin
  if (FPlacementsInstance=Nil) then
    FPlacementsInstance:=CreatePlacementsResource;
  Result:=FPlacementsInstance;
end;

Function TDfareportingAPI.CreatePlacementsResource : TPlacementsResource;

begin
  Result:=CreatePlacementsResource(Self);
end;


Function TDfareportingAPI.CreatePlacementsResource(AOwner : TComponent) : TPlacementsResource;

begin
  Result:=TPlacementsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetPlatformTypesInstance : TPlatformTypesResource;

begin
  if (FPlatformTypesInstance=Nil) then
    FPlatformTypesInstance:=CreatePlatformTypesResource;
  Result:=FPlatformTypesInstance;
end;

Function TDfareportingAPI.CreatePlatformTypesResource : TPlatformTypesResource;

begin
  Result:=CreatePlatformTypesResource(Self);
end;


Function TDfareportingAPI.CreatePlatformTypesResource(AOwner : TComponent) : TPlatformTypesResource;

begin
  Result:=TPlatformTypesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetPostalCodesInstance : TPostalCodesResource;

begin
  if (FPostalCodesInstance=Nil) then
    FPostalCodesInstance:=CreatePostalCodesResource;
  Result:=FPostalCodesInstance;
end;

Function TDfareportingAPI.CreatePostalCodesResource : TPostalCodesResource;

begin
  Result:=CreatePostalCodesResource(Self);
end;


Function TDfareportingAPI.CreatePostalCodesResource(AOwner : TComponent) : TPostalCodesResource;

begin
  Result:=TPostalCodesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TDfareportingAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TDfareportingAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetRegionsInstance : TRegionsResource;

begin
  if (FRegionsInstance=Nil) then
    FRegionsInstance:=CreateRegionsResource;
  Result:=FRegionsInstance;
end;

Function TDfareportingAPI.CreateRegionsResource : TRegionsResource;

begin
  Result:=CreateRegionsResource(Self);
end;


Function TDfareportingAPI.CreateRegionsResource(AOwner : TComponent) : TRegionsResource;

begin
  Result:=TRegionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetRemarketingListSharesInstance : TRemarketingListSharesResource;

begin
  if (FRemarketingListSharesInstance=Nil) then
    FRemarketingListSharesInstance:=CreateRemarketingListSharesResource;
  Result:=FRemarketingListSharesInstance;
end;

Function TDfareportingAPI.CreateRemarketingListSharesResource : TRemarketingListSharesResource;

begin
  Result:=CreateRemarketingListSharesResource(Self);
end;


Function TDfareportingAPI.CreateRemarketingListSharesResource(AOwner : TComponent) : TRemarketingListSharesResource;

begin
  Result:=TRemarketingListSharesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetRemarketingListsInstance : TRemarketingListsResource;

begin
  if (FRemarketingListsInstance=Nil) then
    FRemarketingListsInstance:=CreateRemarketingListsResource;
  Result:=FRemarketingListsInstance;
end;

Function TDfareportingAPI.CreateRemarketingListsResource : TRemarketingListsResource;

begin
  Result:=CreateRemarketingListsResource(Self);
end;


Function TDfareportingAPI.CreateRemarketingListsResource(AOwner : TComponent) : TRemarketingListsResource;

begin
  Result:=TRemarketingListsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetReportsInstance : TReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TDfareportingAPI.CreateReportsResource : TReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TDfareportingAPI.CreateReportsResource(AOwner : TComponent) : TReportsResource;

begin
  Result:=TReportsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetSitesInstance : TSitesResource;

begin
  if (FSitesInstance=Nil) then
    FSitesInstance:=CreateSitesResource;
  Result:=FSitesInstance;
end;

Function TDfareportingAPI.CreateSitesResource : TSitesResource;

begin
  Result:=CreateSitesResource(Self);
end;


Function TDfareportingAPI.CreateSitesResource(AOwner : TComponent) : TSitesResource;

begin
  Result:=TSitesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetSizesInstance : TSizesResource;

begin
  if (FSizesInstance=Nil) then
    FSizesInstance:=CreateSizesResource;
  Result:=FSizesInstance;
end;

Function TDfareportingAPI.CreateSizesResource : TSizesResource;

begin
  Result:=CreateSizesResource(Self);
end;


Function TDfareportingAPI.CreateSizesResource(AOwner : TComponent) : TSizesResource;

begin
  Result:=TSizesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetSubaccountsInstance : TSubaccountsResource;

begin
  if (FSubaccountsInstance=Nil) then
    FSubaccountsInstance:=CreateSubaccountsResource;
  Result:=FSubaccountsInstance;
end;

Function TDfareportingAPI.CreateSubaccountsResource : TSubaccountsResource;

begin
  Result:=CreateSubaccountsResource(Self);
end;


Function TDfareportingAPI.CreateSubaccountsResource(AOwner : TComponent) : TSubaccountsResource;

begin
  Result:=TSubaccountsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetTargetableRemarketingListsInstance : TTargetableRemarketingListsResource;

begin
  if (FTargetableRemarketingListsInstance=Nil) then
    FTargetableRemarketingListsInstance:=CreateTargetableRemarketingListsResource;
  Result:=FTargetableRemarketingListsInstance;
end;

Function TDfareportingAPI.CreateTargetableRemarketingListsResource : TTargetableRemarketingListsResource;

begin
  Result:=CreateTargetableRemarketingListsResource(Self);
end;


Function TDfareportingAPI.CreateTargetableRemarketingListsResource(AOwner : TComponent) : TTargetableRemarketingListsResource;

begin
  Result:=TTargetableRemarketingListsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetUserProfilesInstance : TUserProfilesResource;

begin
  if (FUserProfilesInstance=Nil) then
    FUserProfilesInstance:=CreateUserProfilesResource;
  Result:=FUserProfilesInstance;
end;

Function TDfareportingAPI.CreateUserProfilesResource : TUserProfilesResource;

begin
  Result:=CreateUserProfilesResource(Self);
end;


Function TDfareportingAPI.CreateUserProfilesResource(AOwner : TComponent) : TUserProfilesResource;

begin
  Result:=TUserProfilesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetUserRolePermissionGroupsInstance : TUserRolePermissionGroupsResource;

begin
  if (FUserRolePermissionGroupsInstance=Nil) then
    FUserRolePermissionGroupsInstance:=CreateUserRolePermissionGroupsResource;
  Result:=FUserRolePermissionGroupsInstance;
end;

Function TDfareportingAPI.CreateUserRolePermissionGroupsResource : TUserRolePermissionGroupsResource;

begin
  Result:=CreateUserRolePermissionGroupsResource(Self);
end;


Function TDfareportingAPI.CreateUserRolePermissionGroupsResource(AOwner : TComponent) : TUserRolePermissionGroupsResource;

begin
  Result:=TUserRolePermissionGroupsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetUserRolePermissionsInstance : TUserRolePermissionsResource;

begin
  if (FUserRolePermissionsInstance=Nil) then
    FUserRolePermissionsInstance:=CreateUserRolePermissionsResource;
  Result:=FUserRolePermissionsInstance;
end;

Function TDfareportingAPI.CreateUserRolePermissionsResource : TUserRolePermissionsResource;

begin
  Result:=CreateUserRolePermissionsResource(Self);
end;


Function TDfareportingAPI.CreateUserRolePermissionsResource(AOwner : TComponent) : TUserRolePermissionsResource;

begin
  Result:=TUserRolePermissionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDfareportingAPI.GetUserRolesInstance : TUserRolesResource;

begin
  if (FUserRolesInstance=Nil) then
    FUserRolesInstance:=CreateUserRolesResource;
  Result:=FUserRolesInstance;
end;

Function TDfareportingAPI.CreateUserRolesResource : TUserRolesResource;

begin
  Result:=CreateUserRolesResource(Self);
end;


Function TDfareportingAPI.CreateUserRolesResource(AOwner : TComponent) : TUserRolesResource;

begin
  Result:=TUserRolesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TDfareportingAPI.RegisterAPI;
end.
