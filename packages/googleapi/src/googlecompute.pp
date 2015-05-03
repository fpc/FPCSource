unit googlecompute;
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
  TAccessConfig = class;
  TAccessConfigArray = Array of TAccessConfig;
  TAddress = class;
  TAddressArray = Array of TAddress;
  TAddressusers = class;
  TAddressusersArray = Array of TAddressusers;
  TAddressAggregatedList = class;
  TAddressAggregatedListArray = Array of TAddressAggregatedList;
  TAddressAggregatedListitems = class;
  TAddressAggregatedListitemsArray = Array of TAddressAggregatedListitems;
  TAddressList = class;
  TAddressListArray = Array of TAddressList;
  TAddressListitems = class;
  TAddressListitemsArray = Array of TAddressListitems;
  TAddressesScopedList = class;
  TAddressesScopedListArray = Array of TAddressesScopedList;
  TAddressesScopedListaddresses = class;
  TAddressesScopedListaddressesArray = Array of TAddressesScopedListaddresses;
  TAddressesScopedListwarning = class;
  TAddressesScopedListwarningArray = Array of TAddressesScopedListwarning;
  TAddressesScopedListwarningdata = class;
  TAddressesScopedListwarningdataArray = Array of TAddressesScopedListwarningdata;
  TAttachedDisk = class;
  TAttachedDiskArray = Array of TAttachedDisk;
  TAttachedDisklicenses = class;
  TAttachedDisklicensesArray = Array of TAttachedDisklicenses;
  TAttachedDiskInitializeParams = class;
  TAttachedDiskInitializeParamsArray = Array of TAttachedDiskInitializeParams;
  TBackend = class;
  TBackendArray = Array of TBackend;
  TBackendService = class;
  TBackendServiceArray = Array of TBackendService;
  TBackendServicebackends = class;
  TBackendServicebackendsArray = Array of TBackendServicebackends;
  TBackendServicehealthChecks = class;
  TBackendServicehealthChecksArray = Array of TBackendServicehealthChecks;
  TBackendServiceGroupHealth = class;
  TBackendServiceGroupHealthArray = Array of TBackendServiceGroupHealth;
  TBackendServiceGroupHealthhealthStatus = class;
  TBackendServiceGroupHealthhealthStatusArray = Array of TBackendServiceGroupHealthhealthStatus;
  TBackendServiceList = class;
  TBackendServiceListArray = Array of TBackendServiceList;
  TBackendServiceListitems = class;
  TBackendServiceListitemsArray = Array of TBackendServiceListitems;
  TDeprecationStatus = class;
  TDeprecationStatusArray = Array of TDeprecationStatus;
  TDisk = class;
  TDiskArray = Array of TDisk;
  TDisklicenses = class;
  TDisklicensesArray = Array of TDisklicenses;
  TDiskAggregatedList = class;
  TDiskAggregatedListArray = Array of TDiskAggregatedList;
  TDiskAggregatedListitems = class;
  TDiskAggregatedListitemsArray = Array of TDiskAggregatedListitems;
  TDiskList = class;
  TDiskListArray = Array of TDiskList;
  TDiskListitems = class;
  TDiskListitemsArray = Array of TDiskListitems;
  TDiskMoveRequest = class;
  TDiskMoveRequestArray = Array of TDiskMoveRequest;
  TDiskType = class;
  TDiskTypeArray = Array of TDiskType;
  TDiskTypeAggregatedList = class;
  TDiskTypeAggregatedListArray = Array of TDiskTypeAggregatedList;
  TDiskTypeAggregatedListitems = class;
  TDiskTypeAggregatedListitemsArray = Array of TDiskTypeAggregatedListitems;
  TDiskTypeList = class;
  TDiskTypeListArray = Array of TDiskTypeList;
  TDiskTypeListitems = class;
  TDiskTypeListitemsArray = Array of TDiskTypeListitems;
  TDiskTypesScopedList = class;
  TDiskTypesScopedListArray = Array of TDiskTypesScopedList;
  TDiskTypesScopedListdiskTypes = class;
  TDiskTypesScopedListdiskTypesArray = Array of TDiskTypesScopedListdiskTypes;
  TDiskTypesScopedListwarning = class;
  TDiskTypesScopedListwarningArray = Array of TDiskTypesScopedListwarning;
  TDiskTypesScopedListwarningdata = class;
  TDiskTypesScopedListwarningdataArray = Array of TDiskTypesScopedListwarningdata;
  TDisksScopedList = class;
  TDisksScopedListArray = Array of TDisksScopedList;
  TDisksScopedListdisks = class;
  TDisksScopedListdisksArray = Array of TDisksScopedListdisks;
  TDisksScopedListwarning = class;
  TDisksScopedListwarningArray = Array of TDisksScopedListwarning;
  TDisksScopedListwarningdata = class;
  TDisksScopedListwarningdataArray = Array of TDisksScopedListwarningdata;
  TFirewall = class;
  TFirewallArray = Array of TFirewall;
  TFirewallallowed = class;
  TFirewallallowedArray = Array of TFirewallallowed;
  TFirewallallowedports = class;
  TFirewallallowedportsArray = Array of TFirewallallowedports;
  TFirewallsourceRanges = class;
  TFirewallsourceRangesArray = Array of TFirewallsourceRanges;
  TFirewallsourceTags = class;
  TFirewallsourceTagsArray = Array of TFirewallsourceTags;
  TFirewalltargetTags = class;
  TFirewalltargetTagsArray = Array of TFirewalltargetTags;
  TFirewallList = class;
  TFirewallListArray = Array of TFirewallList;
  TFirewallListitems = class;
  TFirewallListitemsArray = Array of TFirewallListitems;
  TForwardingRule = class;
  TForwardingRuleArray = Array of TForwardingRule;
  TForwardingRuleAggregatedList = class;
  TForwardingRuleAggregatedListArray = Array of TForwardingRuleAggregatedList;
  TForwardingRuleAggregatedListitems = class;
  TForwardingRuleAggregatedListitemsArray = Array of TForwardingRuleAggregatedListitems;
  TForwardingRuleList = class;
  TForwardingRuleListArray = Array of TForwardingRuleList;
  TForwardingRuleListitems = class;
  TForwardingRuleListitemsArray = Array of TForwardingRuleListitems;
  TForwardingRulesScopedList = class;
  TForwardingRulesScopedListArray = Array of TForwardingRulesScopedList;
  TForwardingRulesScopedListforwardingRules = class;
  TForwardingRulesScopedListforwardingRulesArray = Array of TForwardingRulesScopedListforwardingRules;
  TForwardingRulesScopedListwarning = class;
  TForwardingRulesScopedListwarningArray = Array of TForwardingRulesScopedListwarning;
  TForwardingRulesScopedListwarningdata = class;
  TForwardingRulesScopedListwarningdataArray = Array of TForwardingRulesScopedListwarningdata;
  THealthCheckReference = class;
  THealthCheckReferenceArray = Array of THealthCheckReference;
  THealthStatus = class;
  THealthStatusArray = Array of THealthStatus;
  THostRule = class;
  THostRuleArray = Array of THostRule;
  THostRulehosts = class;
  THostRulehostsArray = Array of THostRulehosts;
  THttpHealthCheck = class;
  THttpHealthCheckArray = Array of THttpHealthCheck;
  THttpHealthCheckList = class;
  THttpHealthCheckListArray = Array of THttpHealthCheckList;
  THttpHealthCheckListitems = class;
  THttpHealthCheckListitemsArray = Array of THttpHealthCheckListitems;
  TImage = class;
  TImageArray = Array of TImage;
  TImagelicenses = class;
  TImagelicensesArray = Array of TImagelicenses;
  TImagerawDisk = class;
  TImagerawDiskArray = Array of TImagerawDisk;
  TImageList = class;
  TImageListArray = Array of TImageList;
  TImageListitems = class;
  TImageListitemsArray = Array of TImageListitems;
  TInstance = class;
  TInstanceArray = Array of TInstance;
  TInstancedisks = class;
  TInstancedisksArray = Array of TInstancedisks;
  TInstancenetworkInterfaces = class;
  TInstancenetworkInterfacesArray = Array of TInstancenetworkInterfaces;
  TInstanceserviceAccounts = class;
  TInstanceserviceAccountsArray = Array of TInstanceserviceAccounts;
  TInstanceAggregatedList = class;
  TInstanceAggregatedListArray = Array of TInstanceAggregatedList;
  TInstanceAggregatedListitems = class;
  TInstanceAggregatedListitemsArray = Array of TInstanceAggregatedListitems;
  TInstanceList = class;
  TInstanceListArray = Array of TInstanceList;
  TInstanceListitems = class;
  TInstanceListitemsArray = Array of TInstanceListitems;
  TInstanceMoveRequest = class;
  TInstanceMoveRequestArray = Array of TInstanceMoveRequest;
  TInstanceProperties = class;
  TInstancePropertiesArray = Array of TInstanceProperties;
  TInstancePropertiesdisks = class;
  TInstancePropertiesdisksArray = Array of TInstancePropertiesdisks;
  TInstancePropertiesnetworkInterfaces = class;
  TInstancePropertiesnetworkInterfacesArray = Array of TInstancePropertiesnetworkInterfaces;
  TInstancePropertiesserviceAccounts = class;
  TInstancePropertiesserviceAccountsArray = Array of TInstancePropertiesserviceAccounts;
  TInstanceReference = class;
  TInstanceReferenceArray = Array of TInstanceReference;
  TInstanceTemplate = class;
  TInstanceTemplateArray = Array of TInstanceTemplate;
  TInstanceTemplateList = class;
  TInstanceTemplateListArray = Array of TInstanceTemplateList;
  TInstanceTemplateListitems = class;
  TInstanceTemplateListitemsArray = Array of TInstanceTemplateListitems;
  TInstancesScopedList = class;
  TInstancesScopedListArray = Array of TInstancesScopedList;
  TInstancesScopedListinstances = class;
  TInstancesScopedListinstancesArray = Array of TInstancesScopedListinstances;
  TInstancesScopedListwarning = class;
  TInstancesScopedListwarningArray = Array of TInstancesScopedListwarning;
  TInstancesScopedListwarningdata = class;
  TInstancesScopedListwarningdataArray = Array of TInstancesScopedListwarningdata;
  TLicense = class;
  TLicenseArray = Array of TLicense;
  TMachineType = class;
  TMachineTypeArray = Array of TMachineType;
  TMachineTypescratchDisks = class;
  TMachineTypescratchDisksArray = Array of TMachineTypescratchDisks;
  TMachineTypeAggregatedList = class;
  TMachineTypeAggregatedListArray = Array of TMachineTypeAggregatedList;
  TMachineTypeAggregatedListitems = class;
  TMachineTypeAggregatedListitemsArray = Array of TMachineTypeAggregatedListitems;
  TMachineTypeList = class;
  TMachineTypeListArray = Array of TMachineTypeList;
  TMachineTypeListitems = class;
  TMachineTypeListitemsArray = Array of TMachineTypeListitems;
  TMachineTypesScopedList = class;
  TMachineTypesScopedListArray = Array of TMachineTypesScopedList;
  TMachineTypesScopedListmachineTypes = class;
  TMachineTypesScopedListmachineTypesArray = Array of TMachineTypesScopedListmachineTypes;
  TMachineTypesScopedListwarning = class;
  TMachineTypesScopedListwarningArray = Array of TMachineTypesScopedListwarning;
  TMachineTypesScopedListwarningdata = class;
  TMachineTypesScopedListwarningdataArray = Array of TMachineTypesScopedListwarningdata;
  TMetadata = class;
  TMetadataArray = Array of TMetadata;
  TMetadataitems = class;
  TMetadataitemsArray = Array of TMetadataitems;
  TNetwork = class;
  TNetworkArray = Array of TNetwork;
  TNetworkInterface = class;
  TNetworkInterfaceArray = Array of TNetworkInterface;
  TNetworkInterfaceaccessConfigs = class;
  TNetworkInterfaceaccessConfigsArray = Array of TNetworkInterfaceaccessConfigs;
  TNetworkList = class;
  TNetworkListArray = Array of TNetworkList;
  TNetworkListitems = class;
  TNetworkListitemsArray = Array of TNetworkListitems;
  TOperation = class;
  TOperationArray = Array of TOperation;
  TOperationerror = class;
  TOperationerrorArray = Array of TOperationerror;
  TOperationerrorerrors = class;
  TOperationerrorerrorsArray = Array of TOperationerrorerrors;
  TOperationwarnings = class;
  TOperationwarningsArray = Array of TOperationwarnings;
  TOperationwarningsdata = class;
  TOperationwarningsdataArray = Array of TOperationwarningsdata;
  TOperationAggregatedList = class;
  TOperationAggregatedListArray = Array of TOperationAggregatedList;
  TOperationAggregatedListitems = class;
  TOperationAggregatedListitemsArray = Array of TOperationAggregatedListitems;
  TOperationList = class;
  TOperationListArray = Array of TOperationList;
  TOperationListitems = class;
  TOperationListitemsArray = Array of TOperationListitems;
  TOperationsScopedList = class;
  TOperationsScopedListArray = Array of TOperationsScopedList;
  TOperationsScopedListoperations = class;
  TOperationsScopedListoperationsArray = Array of TOperationsScopedListoperations;
  TOperationsScopedListwarning = class;
  TOperationsScopedListwarningArray = Array of TOperationsScopedListwarning;
  TOperationsScopedListwarningdata = class;
  TOperationsScopedListwarningdataArray = Array of TOperationsScopedListwarningdata;
  TPathMatcher = class;
  TPathMatcherArray = Array of TPathMatcher;
  TPathMatcherpathRules = class;
  TPathMatcherpathRulesArray = Array of TPathMatcherpathRules;
  TPathRule = class;
  TPathRuleArray = Array of TPathRule;
  TPathRulepaths = class;
  TPathRulepathsArray = Array of TPathRulepaths;
  TProject = class;
  TProjectArray = Array of TProject;
  TProjectquotas = class;
  TProjectquotasArray = Array of TProjectquotas;
  TQuota = class;
  TQuotaArray = Array of TQuota;
  TRegion = class;
  TRegionArray = Array of TRegion;
  TRegionquotas = class;
  TRegionquotasArray = Array of TRegionquotas;
  TRegionzones = class;
  TRegionzonesArray = Array of TRegionzones;
  TRegionList = class;
  TRegionListArray = Array of TRegionList;
  TRegionListitems = class;
  TRegionListitemsArray = Array of TRegionListitems;
  TResourceGroupReference = class;
  TResourceGroupReferenceArray = Array of TResourceGroupReference;
  TRoute = class;
  TRouteArray = Array of TRoute;
  TRoutetags = class;
  TRoutetagsArray = Array of TRoutetags;
  TRoutewarnings = class;
  TRoutewarningsArray = Array of TRoutewarnings;
  TRoutewarningsdata = class;
  TRoutewarningsdataArray = Array of TRoutewarningsdata;
  TRouteList = class;
  TRouteListArray = Array of TRouteList;
  TRouteListitems = class;
  TRouteListitemsArray = Array of TRouteListitems;
  TScheduling = class;
  TSchedulingArray = Array of TScheduling;
  TSerialPortOutput = class;
  TSerialPortOutputArray = Array of TSerialPortOutput;
  TServiceAccount = class;
  TServiceAccountArray = Array of TServiceAccount;
  TServiceAccountscopes = class;
  TServiceAccountscopesArray = Array of TServiceAccountscopes;
  TSnapshot = class;
  TSnapshotArray = Array of TSnapshot;
  TSnapshotlicenses = class;
  TSnapshotlicensesArray = Array of TSnapshotlicenses;
  TSnapshotList = class;
  TSnapshotListArray = Array of TSnapshotList;
  TSnapshotListitems = class;
  TSnapshotListitemsArray = Array of TSnapshotListitems;
  TTags = class;
  TTagsArray = Array of TTags;
  TTagsitems = class;
  TTagsitemsArray = Array of TTagsitems;
  TTargetHttpProxy = class;
  TTargetHttpProxyArray = Array of TTargetHttpProxy;
  TTargetHttpProxyList = class;
  TTargetHttpProxyListArray = Array of TTargetHttpProxyList;
  TTargetHttpProxyListitems = class;
  TTargetHttpProxyListitemsArray = Array of TTargetHttpProxyListitems;
  TTargetInstance = class;
  TTargetInstanceArray = Array of TTargetInstance;
  TTargetInstanceAggregatedList = class;
  TTargetInstanceAggregatedListArray = Array of TTargetInstanceAggregatedList;
  TTargetInstanceAggregatedListitems = class;
  TTargetInstanceAggregatedListitemsArray = Array of TTargetInstanceAggregatedListitems;
  TTargetInstanceList = class;
  TTargetInstanceListArray = Array of TTargetInstanceList;
  TTargetInstanceListitems = class;
  TTargetInstanceListitemsArray = Array of TTargetInstanceListitems;
  TTargetInstancesScopedList = class;
  TTargetInstancesScopedListArray = Array of TTargetInstancesScopedList;
  TTargetInstancesScopedListtargetInstances = class;
  TTargetInstancesScopedListtargetInstancesArray = Array of TTargetInstancesScopedListtargetInstances;
  TTargetInstancesScopedListwarning = class;
  TTargetInstancesScopedListwarningArray = Array of TTargetInstancesScopedListwarning;
  TTargetInstancesScopedListwarningdata = class;
  TTargetInstancesScopedListwarningdataArray = Array of TTargetInstancesScopedListwarningdata;
  TTargetPool = class;
  TTargetPoolArray = Array of TTargetPool;
  TTargetPoolhealthChecks = class;
  TTargetPoolhealthChecksArray = Array of TTargetPoolhealthChecks;
  TTargetPoolinstances = class;
  TTargetPoolinstancesArray = Array of TTargetPoolinstances;
  TTargetPoolAggregatedList = class;
  TTargetPoolAggregatedListArray = Array of TTargetPoolAggregatedList;
  TTargetPoolAggregatedListitems = class;
  TTargetPoolAggregatedListitemsArray = Array of TTargetPoolAggregatedListitems;
  TTargetPoolInstanceHealth = class;
  TTargetPoolInstanceHealthArray = Array of TTargetPoolInstanceHealth;
  TTargetPoolInstanceHealthhealthStatus = class;
  TTargetPoolInstanceHealthhealthStatusArray = Array of TTargetPoolInstanceHealthhealthStatus;
  TTargetPoolList = class;
  TTargetPoolListArray = Array of TTargetPoolList;
  TTargetPoolListitems = class;
  TTargetPoolListitemsArray = Array of TTargetPoolListitems;
  TTargetPoolsAddHealthCheckRequest = class;
  TTargetPoolsAddHealthCheckRequestArray = Array of TTargetPoolsAddHealthCheckRequest;
  TTargetPoolsAddHealthCheckRequesthealthChecks = class;
  TTargetPoolsAddHealthCheckRequesthealthChecksArray = Array of TTargetPoolsAddHealthCheckRequesthealthChecks;
  TTargetPoolsAddInstanceRequest = class;
  TTargetPoolsAddInstanceRequestArray = Array of TTargetPoolsAddInstanceRequest;
  TTargetPoolsAddInstanceRequestinstances = class;
  TTargetPoolsAddInstanceRequestinstancesArray = Array of TTargetPoolsAddInstanceRequestinstances;
  TTargetPoolsRemoveHealthCheckRequest = class;
  TTargetPoolsRemoveHealthCheckRequestArray = Array of TTargetPoolsRemoveHealthCheckRequest;
  TTargetPoolsRemoveHealthCheckRequesthealthChecks = class;
  TTargetPoolsRemoveHealthCheckRequesthealthChecksArray = Array of TTargetPoolsRemoveHealthCheckRequesthealthChecks;
  TTargetPoolsRemoveInstanceRequest = class;
  TTargetPoolsRemoveInstanceRequestArray = Array of TTargetPoolsRemoveInstanceRequest;
  TTargetPoolsRemoveInstanceRequestinstances = class;
  TTargetPoolsRemoveInstanceRequestinstancesArray = Array of TTargetPoolsRemoveInstanceRequestinstances;
  TTargetPoolsScopedList = class;
  TTargetPoolsScopedListArray = Array of TTargetPoolsScopedList;
  TTargetPoolsScopedListtargetPools = class;
  TTargetPoolsScopedListtargetPoolsArray = Array of TTargetPoolsScopedListtargetPools;
  TTargetPoolsScopedListwarning = class;
  TTargetPoolsScopedListwarningArray = Array of TTargetPoolsScopedListwarning;
  TTargetPoolsScopedListwarningdata = class;
  TTargetPoolsScopedListwarningdataArray = Array of TTargetPoolsScopedListwarningdata;
  TTargetReference = class;
  TTargetReferenceArray = Array of TTargetReference;
  TTargetVpnGateway = class;
  TTargetVpnGatewayArray = Array of TTargetVpnGateway;
  TTargetVpnGatewayforwardingRules = class;
  TTargetVpnGatewayforwardingRulesArray = Array of TTargetVpnGatewayforwardingRules;
  TTargetVpnGatewaytunnels = class;
  TTargetVpnGatewaytunnelsArray = Array of TTargetVpnGatewaytunnels;
  TTargetVpnGatewayAggregatedList = class;
  TTargetVpnGatewayAggregatedListArray = Array of TTargetVpnGatewayAggregatedList;
  TTargetVpnGatewayAggregatedListitems = class;
  TTargetVpnGatewayAggregatedListitemsArray = Array of TTargetVpnGatewayAggregatedListitems;
  TTargetVpnGatewayList = class;
  TTargetVpnGatewayListArray = Array of TTargetVpnGatewayList;
  TTargetVpnGatewayListitems = class;
  TTargetVpnGatewayListitemsArray = Array of TTargetVpnGatewayListitems;
  TTargetVpnGatewaysScopedList = class;
  TTargetVpnGatewaysScopedListArray = Array of TTargetVpnGatewaysScopedList;
  TTargetVpnGatewaysScopedListtargetVpnGateways = class;
  TTargetVpnGatewaysScopedListtargetVpnGatewaysArray = Array of TTargetVpnGatewaysScopedListtargetVpnGateways;
  TTargetVpnGatewaysScopedListwarning = class;
  TTargetVpnGatewaysScopedListwarningArray = Array of TTargetVpnGatewaysScopedListwarning;
  TTargetVpnGatewaysScopedListwarningdata = class;
  TTargetVpnGatewaysScopedListwarningdataArray = Array of TTargetVpnGatewaysScopedListwarningdata;
  TTestFailure = class;
  TTestFailureArray = Array of TTestFailure;
  TUrlMap = class;
  TUrlMapArray = Array of TUrlMap;
  TUrlMaphostRules = class;
  TUrlMaphostRulesArray = Array of TUrlMaphostRules;
  TUrlMappathMatchers = class;
  TUrlMappathMatchersArray = Array of TUrlMappathMatchers;
  TUrlMaptests = class;
  TUrlMaptestsArray = Array of TUrlMaptests;
  TUrlMapList = class;
  TUrlMapListArray = Array of TUrlMapList;
  TUrlMapListitems = class;
  TUrlMapListitemsArray = Array of TUrlMapListitems;
  TUrlMapReference = class;
  TUrlMapReferenceArray = Array of TUrlMapReference;
  TUrlMapTest = class;
  TUrlMapTestArray = Array of TUrlMapTest;
  TUrlMapValidationResult = class;
  TUrlMapValidationResultArray = Array of TUrlMapValidationResult;
  TUrlMapValidationResultloadErrors = class;
  TUrlMapValidationResultloadErrorsArray = Array of TUrlMapValidationResultloadErrors;
  TUrlMapValidationResulttestFailures = class;
  TUrlMapValidationResulttestFailuresArray = Array of TUrlMapValidationResulttestFailures;
  TUrlMapsValidateRequest = class;
  TUrlMapsValidateRequestArray = Array of TUrlMapsValidateRequest;
  TUrlMapsValidateResponse = class;
  TUrlMapsValidateResponseArray = Array of TUrlMapsValidateResponse;
  TUsageExportLocation = class;
  TUsageExportLocationArray = Array of TUsageExportLocation;
  TVpnTunnel = class;
  TVpnTunnelArray = Array of TVpnTunnel;
  TVpnTunnelikeNetworks = class;
  TVpnTunnelikeNetworksArray = Array of TVpnTunnelikeNetworks;
  TVpnTunnelAggregatedList = class;
  TVpnTunnelAggregatedListArray = Array of TVpnTunnelAggregatedList;
  TVpnTunnelAggregatedListitems = class;
  TVpnTunnelAggregatedListitemsArray = Array of TVpnTunnelAggregatedListitems;
  TVpnTunnelList = class;
  TVpnTunnelListArray = Array of TVpnTunnelList;
  TVpnTunnelListitems = class;
  TVpnTunnelListitemsArray = Array of TVpnTunnelListitems;
  TVpnTunnelsScopedList = class;
  TVpnTunnelsScopedListArray = Array of TVpnTunnelsScopedList;
  TVpnTunnelsScopedListvpnTunnels = class;
  TVpnTunnelsScopedListvpnTunnelsArray = Array of TVpnTunnelsScopedListvpnTunnels;
  TVpnTunnelsScopedListwarning = class;
  TVpnTunnelsScopedListwarningArray = Array of TVpnTunnelsScopedListwarning;
  TVpnTunnelsScopedListwarningdata = class;
  TVpnTunnelsScopedListwarningdataArray = Array of TVpnTunnelsScopedListwarningdata;
  TZone = class;
  TZoneArray = Array of TZone;
  TZonemaintenanceWindows = class;
  TZonemaintenanceWindowsArray = Array of TZonemaintenanceWindows;
  TZoneList = class;
  TZoneListArray = Array of TZoneList;
  TZoneListitems = class;
  TZoneListitemsArray = Array of TZoneListitems;
  
  { --------------------------------------------------------------------
    TAccessConfig
    --------------------------------------------------------------------}
  
  TAccessConfig = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fname : string;
    FnatIP : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetnatIP(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property name : string Index 8 Read Fname Write Setname;
    Property natIP : string Index 16 Read FnatIP Write SetnatIP;
    Property _type : string Index 24 Read F_type Write Set_type;
  end;
  TAccessConfigClass = Class of TAccessConfig;
  
  { --------------------------------------------------------------------
    TAddress
    --------------------------------------------------------------------}
  
  TAddress = Class(TGoogleBaseObject)
  Private
    Faddress : string;
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fregion : string;
    FselfLink : string;
    Fstatus : string;
    Fusers : TAddressusers;
  Protected
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TAddressusers); virtual;
  Public
  Published
    Property address : string Index 0 Read Faddress Write Setaddress;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property region : string Index 48 Read Fregion Write Setregion;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property status : string Index 64 Read Fstatus Write Setstatus;
    Property users : TAddressusers Index 72 Read Fusers Write Setusers;
  end;
  TAddressClass = Class of TAddress;
  
  { --------------------------------------------------------------------
    TAddressusers
    --------------------------------------------------------------------}
  
  TAddressusers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAddressusersClass = Class of TAddressusers;
  
  { --------------------------------------------------------------------
    TAddressAggregatedList
    --------------------------------------------------------------------}
  
  TAddressAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TAddressAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAddressAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TAddressAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TAddressAggregatedListClass = Class of TAddressAggregatedList;
  
  { --------------------------------------------------------------------
    TAddressAggregatedListitems
    --------------------------------------------------------------------}
  
  TAddressAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAddressAggregatedListitemsClass = Class of TAddressAggregatedListitems;
  
  { --------------------------------------------------------------------
    TAddressList
    --------------------------------------------------------------------}
  
  TAddressList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TAddressListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAddressListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TAddressListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TAddressListClass = Class of TAddressList;
  
  { --------------------------------------------------------------------
    TAddressListitems
    --------------------------------------------------------------------}
  
  TAddressListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAddressListitemsClass = Class of TAddressListitems;
  
  { --------------------------------------------------------------------
    TAddressesScopedList
    --------------------------------------------------------------------}
  
  TAddressesScopedList = Class(TGoogleBaseObject)
  Private
    Faddresses : TAddressesScopedListaddresses;
    Fwarning : TAddressesScopedListwarning;
  Protected
    //Property setters
    Procedure Setaddresses(AIndex : Integer; AValue : TAddressesScopedListaddresses); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TAddressesScopedListwarning); virtual;
  Public
  Published
    Property addresses : TAddressesScopedListaddresses Index 0 Read Faddresses Write Setaddresses;
    Property warning : TAddressesScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TAddressesScopedListClass = Class of TAddressesScopedList;
  
  { --------------------------------------------------------------------
    TAddressesScopedListaddresses
    --------------------------------------------------------------------}
  
  TAddressesScopedListaddresses = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAddressesScopedListaddressesClass = Class of TAddressesScopedListaddresses;
  
  { --------------------------------------------------------------------
    TAddressesScopedListwarning
    --------------------------------------------------------------------}
  
  TAddressesScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TAddressesScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TAddressesScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TAddressesScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TAddressesScopedListwarningClass = Class of TAddressesScopedListwarning;
  
  { --------------------------------------------------------------------
    TAddressesScopedListwarningdata
    --------------------------------------------------------------------}
  
  TAddressesScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TAddressesScopedListwarningdataClass = Class of TAddressesScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TAttachedDisk
    --------------------------------------------------------------------}
  
  TAttachedDisk = Class(TGoogleBaseObject)
  Private
    FautoDelete : boolean;
    Fboot : boolean;
    FdeviceName : string;
    Findex : integer;
    FinitializeParams : TAttachedDiskInitializeParams;
    F_interface : string;
    Fkind : string;
    Flicenses : TAttachedDisklicenses;
    Fmode : string;
    Fsource : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetautoDelete(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setboot(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdeviceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinitializeParams(AIndex : Integer; AValue : TAttachedDiskInitializeParams); virtual;
    Procedure Set_interface(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlicenses(AIndex : Integer; AValue : TAttachedDisklicenses); virtual;
    Procedure Setmode(AIndex : Integer; AValue : string); virtual;
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoDelete : boolean Index 0 Read FautoDelete Write SetautoDelete;
    Property boot : boolean Index 8 Read Fboot Write Setboot;
    Property deviceName : string Index 16 Read FdeviceName Write SetdeviceName;
    Property index : integer Index 24 Read Findex Write Setindex;
    Property initializeParams : TAttachedDiskInitializeParams Index 32 Read FinitializeParams Write SetinitializeParams;
    Property _interface : string Index 40 Read F_interface Write Set_interface;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property licenses : TAttachedDisklicenses Index 56 Read Flicenses Write Setlicenses;
    Property mode : string Index 64 Read Fmode Write Setmode;
    Property source : string Index 72 Read Fsource Write Setsource;
    Property _type : string Index 80 Read F_type Write Set_type;
  end;
  TAttachedDiskClass = Class of TAttachedDisk;
  
  { --------------------------------------------------------------------
    TAttachedDisklicenses
    --------------------------------------------------------------------}
  
  TAttachedDisklicenses = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAttachedDisklicensesClass = Class of TAttachedDisklicenses;
  
  { --------------------------------------------------------------------
    TAttachedDiskInitializeParams
    --------------------------------------------------------------------}
  
  TAttachedDiskInitializeParams = Class(TGoogleBaseObject)
  Private
    FdiskName : string;
    FdiskSizeGb : string;
    FdiskType : string;
    FsourceImage : string;
  Protected
    //Property setters
    Procedure SetdiskName(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiskSizeGb(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiskType(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceImage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property diskName : string Index 0 Read FdiskName Write SetdiskName;
    Property diskSizeGb : string Index 8 Read FdiskSizeGb Write SetdiskSizeGb;
    Property diskType : string Index 16 Read FdiskType Write SetdiskType;
    Property sourceImage : string Index 24 Read FsourceImage Write SetsourceImage;
  end;
  TAttachedDiskInitializeParamsClass = Class of TAttachedDiskInitializeParams;
  
  { --------------------------------------------------------------------
    TBackend
    --------------------------------------------------------------------}
  
  TBackend = Class(TGoogleBaseObject)
  Private
    FbalancingMode : string;
    FcapacityScaler : integer;
    Fdescription : string;
    Fgroup : string;
    FmaxRate : integer;
    FmaxRatePerInstance : integer;
    FmaxUtilization : integer;
  Protected
    //Property setters
    Procedure SetbalancingMode(AIndex : Integer; AValue : string); virtual;
    Procedure SetcapacityScaler(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setgroup(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxRate(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxRatePerInstance(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxUtilization(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property balancingMode : string Index 0 Read FbalancingMode Write SetbalancingMode;
    Property capacityScaler : integer Index 8 Read FcapacityScaler Write SetcapacityScaler;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property group : string Index 24 Read Fgroup Write Setgroup;
    Property maxRate : integer Index 32 Read FmaxRate Write SetmaxRate;
    Property maxRatePerInstance : integer Index 40 Read FmaxRatePerInstance Write SetmaxRatePerInstance;
    Property maxUtilization : integer Index 48 Read FmaxUtilization Write SetmaxUtilization;
  end;
  TBackendClass = Class of TBackend;
  
  { --------------------------------------------------------------------
    TBackendService
    --------------------------------------------------------------------}
  
  TBackendService = Class(TGoogleBaseObject)
  Private
    Fbackends : TBackendServicebackends;
    FcreationTimestamp : string;
    Fdescription : string;
    Ffingerprint : string;
    FhealthChecks : TBackendServicehealthChecks;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fport : integer;
    FportName : string;
    Fprotocol : string;
    FselfLink : string;
    FtimeoutSec : integer;
  Protected
    //Property setters
    Procedure Setbackends(AIndex : Integer; AValue : TBackendServicebackends); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure SethealthChecks(AIndex : Integer; AValue : TBackendServicehealthChecks); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setport(AIndex : Integer; AValue : integer); virtual;
    Procedure SetportName(AIndex : Integer; AValue : string); virtual;
    Procedure Setprotocol(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeoutSec(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property backends : TBackendServicebackends Index 0 Read Fbackends Write Setbackends;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property fingerprint : string Index 24 Read Ffingerprint Write Setfingerprint;
    Property healthChecks : TBackendServicehealthChecks Index 32 Read FhealthChecks Write SethealthChecks;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property port : integer Index 64 Read Fport Write Setport;
    Property portName : string Index 72 Read FportName Write SetportName;
    Property protocol : string Index 80 Read Fprotocol Write Setprotocol;
    Property selfLink : string Index 88 Read FselfLink Write SetselfLink;
    Property timeoutSec : integer Index 96 Read FtimeoutSec Write SettimeoutSec;
  end;
  TBackendServiceClass = Class of TBackendService;
  
  { --------------------------------------------------------------------
    TBackendServicebackends
    --------------------------------------------------------------------}
  
  TBackendServicebackends = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBackendServicebackendsClass = Class of TBackendServicebackends;
  
  { --------------------------------------------------------------------
    TBackendServicehealthChecks
    --------------------------------------------------------------------}
  
  TBackendServicehealthChecks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBackendServicehealthChecksClass = Class of TBackendServicehealthChecks;
  
  { --------------------------------------------------------------------
    TBackendServiceGroupHealth
    --------------------------------------------------------------------}
  
  TBackendServiceGroupHealth = Class(TGoogleBaseObject)
  Private
    FhealthStatus : TBackendServiceGroupHealthhealthStatus;
    Fkind : string;
  Protected
    //Property setters
    Procedure SethealthStatus(AIndex : Integer; AValue : TBackendServiceGroupHealthhealthStatus); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property healthStatus : TBackendServiceGroupHealthhealthStatus Index 0 Read FhealthStatus Write SethealthStatus;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TBackendServiceGroupHealthClass = Class of TBackendServiceGroupHealth;
  
  { --------------------------------------------------------------------
    TBackendServiceGroupHealthhealthStatus
    --------------------------------------------------------------------}
  
  TBackendServiceGroupHealthhealthStatus = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBackendServiceGroupHealthhealthStatusClass = Class of TBackendServiceGroupHealthhealthStatus;
  
  { --------------------------------------------------------------------
    TBackendServiceList
    --------------------------------------------------------------------}
  
  TBackendServiceList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TBackendServiceListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TBackendServiceListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TBackendServiceListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TBackendServiceListClass = Class of TBackendServiceList;
  
  { --------------------------------------------------------------------
    TBackendServiceListitems
    --------------------------------------------------------------------}
  
  TBackendServiceListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBackendServiceListitemsClass = Class of TBackendServiceListitems;
  
  { --------------------------------------------------------------------
    TDeprecationStatus
    --------------------------------------------------------------------}
  
  TDeprecationStatus = Class(TGoogleBaseObject)
  Private
    Fdeleted : string;
    Fdeprecated : string;
    Fobsolete : string;
    Freplacement : string;
    Fstate : string;
  Protected
    //Property setters
    Procedure Setdeleted(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : string); virtual;
    Procedure Setobsolete(AIndex : Integer; AValue : string); virtual;
    Procedure Setreplacement(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property deleted : string Index 0 Read Fdeleted Write Setdeleted;
    Property deprecated : string Index 8 Read Fdeprecated Write Setdeprecated;
    Property obsolete : string Index 16 Read Fobsolete Write Setobsolete;
    Property replacement : string Index 24 Read Freplacement Write Setreplacement;
    Property state : string Index 32 Read Fstate Write Setstate;
  end;
  TDeprecationStatusClass = Class of TDeprecationStatus;
  
  { --------------------------------------------------------------------
    TDisk
    --------------------------------------------------------------------}
  
  TDisk = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Flicenses : TDisklicenses;
    Fname : string;
    Foptions : string;
    FselfLink : string;
    FsizeGb : string;
    FsourceImage : string;
    FsourceImageId : string;
    FsourceSnapshot : string;
    FsourceSnapshotId : string;
    Fstatus : string;
    F_type : string;
    Fzone : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlicenses(AIndex : Integer; AValue : TDisklicenses); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setoptions(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsizeGb(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceImage(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceImageId(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceSnapshot(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceSnapshotId(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property licenses : TDisklicenses Index 32 Read Flicenses Write Setlicenses;
    Property name : string Index 40 Read Fname Write Setname;
    Property options : string Index 48 Read Foptions Write Setoptions;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property sizeGb : string Index 64 Read FsizeGb Write SetsizeGb;
    Property sourceImage : string Index 72 Read FsourceImage Write SetsourceImage;
    Property sourceImageId : string Index 80 Read FsourceImageId Write SetsourceImageId;
    Property sourceSnapshot : string Index 88 Read FsourceSnapshot Write SetsourceSnapshot;
    Property sourceSnapshotId : string Index 96 Read FsourceSnapshotId Write SetsourceSnapshotId;
    Property status : string Index 104 Read Fstatus Write Setstatus;
    Property _type : string Index 112 Read F_type Write Set_type;
    Property zone : string Index 120 Read Fzone Write Setzone;
  end;
  TDiskClass = Class of TDisk;
  
  { --------------------------------------------------------------------
    TDisklicenses
    --------------------------------------------------------------------}
  
  TDisklicenses = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDisklicensesClass = Class of TDisklicenses;
  
  { --------------------------------------------------------------------
    TDiskAggregatedList
    --------------------------------------------------------------------}
  
  TDiskAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TDiskAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TDiskAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TDiskAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TDiskAggregatedListClass = Class of TDiskAggregatedList;
  
  { --------------------------------------------------------------------
    TDiskAggregatedListitems
    --------------------------------------------------------------------}
  
  TDiskAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TDiskAggregatedListitemsClass = Class of TDiskAggregatedListitems;
  
  { --------------------------------------------------------------------
    TDiskList
    --------------------------------------------------------------------}
  
  TDiskList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TDiskListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TDiskListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TDiskListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TDiskListClass = Class of TDiskList;
  
  { --------------------------------------------------------------------
    TDiskListitems
    --------------------------------------------------------------------}
  
  TDiskListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDiskListitemsClass = Class of TDiskListitems;
  
  { --------------------------------------------------------------------
    TDiskMoveRequest
    --------------------------------------------------------------------}
  
  TDiskMoveRequest = Class(TGoogleBaseObject)
  Private
    FdestinationZone : string;
    FtargetDisk : string;
  Protected
    //Property setters
    Procedure SetdestinationZone(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetDisk(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property destinationZone : string Index 0 Read FdestinationZone Write SetdestinationZone;
    Property targetDisk : string Index 8 Read FtargetDisk Write SettargetDisk;
  end;
  TDiskMoveRequestClass = Class of TDiskMoveRequest;
  
  { --------------------------------------------------------------------
    TDiskType
    --------------------------------------------------------------------}
  
  TDiskType = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    FdefaultDiskSizeGb : string;
    Fdeprecated : TDeprecationStatus;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FselfLink : string;
    FvalidDiskSize : string;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultDiskSizeGb(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetvalidDiskSize(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property defaultDiskSizeGb : string Index 8 Read FdefaultDiskSizeGb Write SetdefaultDiskSizeGb;
    Property deprecated : TDeprecationStatus Index 16 Read Fdeprecated Write Setdeprecated;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property name : string Index 48 Read Fname Write Setname;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property validDiskSize : string Index 64 Read FvalidDiskSize Write SetvalidDiskSize;
    Property zone : string Index 72 Read Fzone Write Setzone;
  end;
  TDiskTypeClass = Class of TDiskType;
  
  { --------------------------------------------------------------------
    TDiskTypeAggregatedList
    --------------------------------------------------------------------}
  
  TDiskTypeAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TDiskTypeAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TDiskTypeAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TDiskTypeAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TDiskTypeAggregatedListClass = Class of TDiskTypeAggregatedList;
  
  { --------------------------------------------------------------------
    TDiskTypeAggregatedListitems
    --------------------------------------------------------------------}
  
  TDiskTypeAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TDiskTypeAggregatedListitemsClass = Class of TDiskTypeAggregatedListitems;
  
  { --------------------------------------------------------------------
    TDiskTypeList
    --------------------------------------------------------------------}
  
  TDiskTypeList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TDiskTypeListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TDiskTypeListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TDiskTypeListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TDiskTypeListClass = Class of TDiskTypeList;
  
  { --------------------------------------------------------------------
    TDiskTypeListitems
    --------------------------------------------------------------------}
  
  TDiskTypeListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDiskTypeListitemsClass = Class of TDiskTypeListitems;
  
  { --------------------------------------------------------------------
    TDiskTypesScopedList
    --------------------------------------------------------------------}
  
  TDiskTypesScopedList = Class(TGoogleBaseObject)
  Private
    FdiskTypes : TDiskTypesScopedListdiskTypes;
    Fwarning : TDiskTypesScopedListwarning;
  Protected
    //Property setters
    Procedure SetdiskTypes(AIndex : Integer; AValue : TDiskTypesScopedListdiskTypes); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TDiskTypesScopedListwarning); virtual;
  Public
  Published
    Property diskTypes : TDiskTypesScopedListdiskTypes Index 0 Read FdiskTypes Write SetdiskTypes;
    Property warning : TDiskTypesScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TDiskTypesScopedListClass = Class of TDiskTypesScopedList;
  
  { --------------------------------------------------------------------
    TDiskTypesScopedListdiskTypes
    --------------------------------------------------------------------}
  
  TDiskTypesScopedListdiskTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDiskTypesScopedListdiskTypesClass = Class of TDiskTypesScopedListdiskTypes;
  
  { --------------------------------------------------------------------
    TDiskTypesScopedListwarning
    --------------------------------------------------------------------}
  
  TDiskTypesScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TDiskTypesScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TDiskTypesScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TDiskTypesScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TDiskTypesScopedListwarningClass = Class of TDiskTypesScopedListwarning;
  
  { --------------------------------------------------------------------
    TDiskTypesScopedListwarningdata
    --------------------------------------------------------------------}
  
  TDiskTypesScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TDiskTypesScopedListwarningdataClass = Class of TDiskTypesScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TDisksScopedList
    --------------------------------------------------------------------}
  
  TDisksScopedList = Class(TGoogleBaseObject)
  Private
    Fdisks : TDisksScopedListdisks;
    Fwarning : TDisksScopedListwarning;
  Protected
    //Property setters
    Procedure Setdisks(AIndex : Integer; AValue : TDisksScopedListdisks); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TDisksScopedListwarning); virtual;
  Public
  Published
    Property disks : TDisksScopedListdisks Index 0 Read Fdisks Write Setdisks;
    Property warning : TDisksScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TDisksScopedListClass = Class of TDisksScopedList;
  
  { --------------------------------------------------------------------
    TDisksScopedListdisks
    --------------------------------------------------------------------}
  
  TDisksScopedListdisks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDisksScopedListdisksClass = Class of TDisksScopedListdisks;
  
  { --------------------------------------------------------------------
    TDisksScopedListwarning
    --------------------------------------------------------------------}
  
  TDisksScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TDisksScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TDisksScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TDisksScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TDisksScopedListwarningClass = Class of TDisksScopedListwarning;
  
  { --------------------------------------------------------------------
    TDisksScopedListwarningdata
    --------------------------------------------------------------------}
  
  TDisksScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TDisksScopedListwarningdataClass = Class of TDisksScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TFirewall
    --------------------------------------------------------------------}
  
  TFirewall = Class(TGoogleBaseObject)
  Private
    Fallowed : TFirewallallowed;
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fnetwork : string;
    FselfLink : string;
    FsourceRanges : TFirewallsourceRanges;
    FsourceTags : TFirewallsourceTags;
    FtargetTags : TFirewalltargetTags;
  Protected
    //Property setters
    Procedure Setallowed(AIndex : Integer; AValue : TFirewallallowed); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceRanges(AIndex : Integer; AValue : TFirewallsourceRanges); virtual;
    Procedure SetsourceTags(AIndex : Integer; AValue : TFirewallsourceTags); virtual;
    Procedure SettargetTags(AIndex : Integer; AValue : TFirewalltargetTags); virtual;
  Public
  Published
    Property allowed : TFirewallallowed Index 0 Read Fallowed Write Setallowed;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property network : string Index 48 Read Fnetwork Write Setnetwork;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property sourceRanges : TFirewallsourceRanges Index 64 Read FsourceRanges Write SetsourceRanges;
    Property sourceTags : TFirewallsourceTags Index 72 Read FsourceTags Write SetsourceTags;
    Property targetTags : TFirewalltargetTags Index 80 Read FtargetTags Write SettargetTags;
  end;
  TFirewallClass = Class of TFirewall;
  
  { --------------------------------------------------------------------
    TFirewallallowed
    --------------------------------------------------------------------}
  
  TFirewallallowed = Class(TGoogleBaseObject)
  Private
    FIPProtocol : string;
    Fports : TFirewallallowedports;
  Protected
    //Property setters
    Procedure SetIPProtocol(AIndex : Integer; AValue : string); virtual;
    Procedure Setports(AIndex : Integer; AValue : TFirewallallowedports); virtual;
  Public
  Published
    Property IPProtocol : string Index 0 Read FIPProtocol Write SetIPProtocol;
    Property ports : TFirewallallowedports Index 8 Read Fports Write Setports;
  end;
  TFirewallallowedClass = Class of TFirewallallowed;
  
  { --------------------------------------------------------------------
    TFirewallallowedports
    --------------------------------------------------------------------}
  
  TFirewallallowedports = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewallallowedportsClass = Class of TFirewallallowedports;
  
  { --------------------------------------------------------------------
    TFirewallsourceRanges
    --------------------------------------------------------------------}
  
  TFirewallsourceRanges = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewallsourceRangesClass = Class of TFirewallsourceRanges;
  
  { --------------------------------------------------------------------
    TFirewallsourceTags
    --------------------------------------------------------------------}
  
  TFirewallsourceTags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewallsourceTagsClass = Class of TFirewallsourceTags;
  
  { --------------------------------------------------------------------
    TFirewalltargetTags
    --------------------------------------------------------------------}
  
  TFirewalltargetTags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewalltargetTagsClass = Class of TFirewalltargetTags;
  
  { --------------------------------------------------------------------
    TFirewallList
    --------------------------------------------------------------------}
  
  TFirewallList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TFirewallListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TFirewallListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TFirewallListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TFirewallListClass = Class of TFirewallList;
  
  { --------------------------------------------------------------------
    TFirewallListitems
    --------------------------------------------------------------------}
  
  TFirewallListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewallListitemsClass = Class of TFirewallListitems;
  
  { --------------------------------------------------------------------
    TForwardingRule
    --------------------------------------------------------------------}
  
  TForwardingRule = Class(TGoogleBaseObject)
  Private
    FIPAddress : string;
    FIPProtocol : string;
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FportRange : string;
    Fregion : string;
    FselfLink : string;
    Ftarget : string;
  Protected
    //Property setters
    Procedure SetIPAddress(AIndex : Integer; AValue : string); virtual;
    Procedure SetIPProtocol(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetportRange(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settarget(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property IPAddress : string Index 0 Read FIPAddress Write SetIPAddress;
    Property IPProtocol : string Index 8 Read FIPProtocol Write SetIPProtocol;
    Property creationTimestamp : string Index 16 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property name : string Index 48 Read Fname Write Setname;
    Property portRange : string Index 56 Read FportRange Write SetportRange;
    Property region : string Index 64 Read Fregion Write Setregion;
    Property selfLink : string Index 72 Read FselfLink Write SetselfLink;
    Property target : string Index 80 Read Ftarget Write Settarget;
  end;
  TForwardingRuleClass = Class of TForwardingRule;
  
  { --------------------------------------------------------------------
    TForwardingRuleAggregatedList
    --------------------------------------------------------------------}
  
  TForwardingRuleAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TForwardingRuleAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TForwardingRuleAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TForwardingRuleAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TForwardingRuleAggregatedListClass = Class of TForwardingRuleAggregatedList;
  
  { --------------------------------------------------------------------
    TForwardingRuleAggregatedListitems
    --------------------------------------------------------------------}
  
  TForwardingRuleAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TForwardingRuleAggregatedListitemsClass = Class of TForwardingRuleAggregatedListitems;
  
  { --------------------------------------------------------------------
    TForwardingRuleList
    --------------------------------------------------------------------}
  
  TForwardingRuleList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TForwardingRuleListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TForwardingRuleListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TForwardingRuleListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TForwardingRuleListClass = Class of TForwardingRuleList;
  
  { --------------------------------------------------------------------
    TForwardingRuleListitems
    --------------------------------------------------------------------}
  
  TForwardingRuleListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TForwardingRuleListitemsClass = Class of TForwardingRuleListitems;
  
  { --------------------------------------------------------------------
    TForwardingRulesScopedList
    --------------------------------------------------------------------}
  
  TForwardingRulesScopedList = Class(TGoogleBaseObject)
  Private
    FforwardingRules : TForwardingRulesScopedListforwardingRules;
    Fwarning : TForwardingRulesScopedListwarning;
  Protected
    //Property setters
    Procedure SetforwardingRules(AIndex : Integer; AValue : TForwardingRulesScopedListforwardingRules); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TForwardingRulesScopedListwarning); virtual;
  Public
  Published
    Property forwardingRules : TForwardingRulesScopedListforwardingRules Index 0 Read FforwardingRules Write SetforwardingRules;
    Property warning : TForwardingRulesScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TForwardingRulesScopedListClass = Class of TForwardingRulesScopedList;
  
  { --------------------------------------------------------------------
    TForwardingRulesScopedListforwardingRules
    --------------------------------------------------------------------}
  
  TForwardingRulesScopedListforwardingRules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TForwardingRulesScopedListforwardingRulesClass = Class of TForwardingRulesScopedListforwardingRules;
  
  { --------------------------------------------------------------------
    TForwardingRulesScopedListwarning
    --------------------------------------------------------------------}
  
  TForwardingRulesScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TForwardingRulesScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TForwardingRulesScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TForwardingRulesScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TForwardingRulesScopedListwarningClass = Class of TForwardingRulesScopedListwarning;
  
  { --------------------------------------------------------------------
    TForwardingRulesScopedListwarningdata
    --------------------------------------------------------------------}
  
  TForwardingRulesScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TForwardingRulesScopedListwarningdataClass = Class of TForwardingRulesScopedListwarningdata;
  
  { --------------------------------------------------------------------
    THealthCheckReference
    --------------------------------------------------------------------}
  
  THealthCheckReference = Class(TGoogleBaseObject)
  Private
    FhealthCheck : string;
  Protected
    //Property setters
    Procedure SethealthCheck(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property healthCheck : string Index 0 Read FhealthCheck Write SethealthCheck;
  end;
  THealthCheckReferenceClass = Class of THealthCheckReference;
  
  { --------------------------------------------------------------------
    THealthStatus
    --------------------------------------------------------------------}
  
  THealthStatus = Class(TGoogleBaseObject)
  Private
    FhealthState : string;
    Finstance : string;
    FipAddress : string;
    Fport : integer;
  Protected
    //Property setters
    Procedure SethealthState(AIndex : Integer; AValue : string); virtual;
    Procedure Setinstance(AIndex : Integer; AValue : string); virtual;
    Procedure SetipAddress(AIndex : Integer; AValue : string); virtual;
    Procedure Setport(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property healthState : string Index 0 Read FhealthState Write SethealthState;
    Property instance : string Index 8 Read Finstance Write Setinstance;
    Property ipAddress : string Index 16 Read FipAddress Write SetipAddress;
    Property port : integer Index 24 Read Fport Write Setport;
  end;
  THealthStatusClass = Class of THealthStatus;
  
  { --------------------------------------------------------------------
    THostRule
    --------------------------------------------------------------------}
  
  THostRule = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fhosts : THostRulehosts;
    FpathMatcher : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Sethosts(AIndex : Integer; AValue : THostRulehosts); virtual;
    Procedure SetpathMatcher(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property hosts : THostRulehosts Index 8 Read Fhosts Write Sethosts;
    Property pathMatcher : string Index 16 Read FpathMatcher Write SetpathMatcher;
  end;
  THostRuleClass = Class of THostRule;
  
  { --------------------------------------------------------------------
    THostRulehosts
    --------------------------------------------------------------------}
  
  THostRulehosts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THostRulehostsClass = Class of THostRulehosts;
  
  { --------------------------------------------------------------------
    THttpHealthCheck
    --------------------------------------------------------------------}
  
  THttpHealthCheck = Class(TGoogleBaseObject)
  Private
    FcheckIntervalSec : integer;
    FcreationTimestamp : string;
    Fdescription : string;
    FhealthyThreshold : integer;
    Fhost : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fport : integer;
    FrequestPath : string;
    FselfLink : string;
    FtimeoutSec : integer;
    FunhealthyThreshold : integer;
  Protected
    //Property setters
    Procedure SetcheckIntervalSec(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SethealthyThreshold(AIndex : Integer; AValue : integer); virtual;
    Procedure Sethost(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setport(AIndex : Integer; AValue : integer); virtual;
    Procedure SetrequestPath(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeoutSec(AIndex : Integer; AValue : integer); virtual;
    Procedure SetunhealthyThreshold(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property checkIntervalSec : integer Index 0 Read FcheckIntervalSec Write SetcheckIntervalSec;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property healthyThreshold : integer Index 24 Read FhealthyThreshold Write SethealthyThreshold;
    Property host : string Index 32 Read Fhost Write Sethost;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property port : integer Index 64 Read Fport Write Setport;
    Property requestPath : string Index 72 Read FrequestPath Write SetrequestPath;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property timeoutSec : integer Index 88 Read FtimeoutSec Write SettimeoutSec;
    Property unhealthyThreshold : integer Index 96 Read FunhealthyThreshold Write SetunhealthyThreshold;
  end;
  THttpHealthCheckClass = Class of THttpHealthCheck;
  
  { --------------------------------------------------------------------
    THttpHealthCheckList
    --------------------------------------------------------------------}
  
  THttpHealthCheckList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : THttpHealthCheckListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : THttpHealthCheckListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : THttpHealthCheckListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  THttpHealthCheckListClass = Class of THttpHealthCheckList;
  
  { --------------------------------------------------------------------
    THttpHealthCheckListitems
    --------------------------------------------------------------------}
  
  THttpHealthCheckListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THttpHealthCheckListitemsClass = Class of THttpHealthCheckListitems;
  
  { --------------------------------------------------------------------
    TImage
    --------------------------------------------------------------------}
  
  TImage = Class(TGoogleBaseObject)
  Private
    FarchiveSizeBytes : string;
    FcreationTimestamp : string;
    Fdeprecated : TDeprecationStatus;
    Fdescription : string;
    FdiskSizeGb : string;
    Fid : string;
    Fkind : string;
    Flicenses : TImagelicenses;
    Fname : string;
    FrawDisk : TImagerawDisk;
    FselfLink : string;
    FsourceDisk : string;
    FsourceDiskId : string;
    FsourceType : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure SetarchiveSizeBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiskSizeGb(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlicenses(AIndex : Integer; AValue : TImagelicenses); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetrawDisk(AIndex : Integer; AValue : TImagerawDisk); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceDisk(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceDiskId(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceType(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property archiveSizeBytes : string Index 0 Read FarchiveSizeBytes Write SetarchiveSizeBytes;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property deprecated : TDeprecationStatus Index 16 Read Fdeprecated Write Setdeprecated;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property diskSizeGb : string Index 32 Read FdiskSizeGb Write SetdiskSizeGb;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property licenses : TImagelicenses Index 56 Read Flicenses Write Setlicenses;
    Property name : string Index 64 Read Fname Write Setname;
    Property rawDisk : TImagerawDisk Index 72 Read FrawDisk Write SetrawDisk;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property sourceDisk : string Index 88 Read FsourceDisk Write SetsourceDisk;
    Property sourceDiskId : string Index 96 Read FsourceDiskId Write SetsourceDiskId;
    Property sourceType : string Index 104 Read FsourceType Write SetsourceType;
    Property status : string Index 112 Read Fstatus Write Setstatus;
  end;
  TImageClass = Class of TImage;
  
  { --------------------------------------------------------------------
    TImagelicenses
    --------------------------------------------------------------------}
  
  TImagelicenses = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TImagelicensesClass = Class of TImagelicenses;
  
  { --------------------------------------------------------------------
    TImagerawDisk
    --------------------------------------------------------------------}
  
  TImagerawDisk = Class(TGoogleBaseObject)
  Private
    FcontainerType : string;
    Fsha1Checksum : string;
    Fsource : string;
  Protected
    //Property setters
    Procedure SetcontainerType(AIndex : Integer; AValue : string); virtual;
    Procedure Setsha1Checksum(AIndex : Integer; AValue : string); virtual;
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property containerType : string Index 0 Read FcontainerType Write SetcontainerType;
    Property sha1Checksum : string Index 8 Read Fsha1Checksum Write Setsha1Checksum;
    Property source : string Index 16 Read Fsource Write Setsource;
  end;
  TImagerawDiskClass = Class of TImagerawDisk;
  
  { --------------------------------------------------------------------
    TImageList
    --------------------------------------------------------------------}
  
  TImageList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TImageListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TImageListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TImageListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TImageListClass = Class of TImageList;
  
  { --------------------------------------------------------------------
    TImageListitems
    --------------------------------------------------------------------}
  
  TImageListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TImageListitemsClass = Class of TImageListitems;
  
  { --------------------------------------------------------------------
    TInstance
    --------------------------------------------------------------------}
  
  TInstance = Class(TGoogleBaseObject)
  Private
    FcanIpForward : boolean;
    FcpuPlatform : string;
    FcreationTimestamp : string;
    Fdescription : string;
    Fdisks : TInstancedisks;
    Fid : string;
    Fkind : string;
    FmachineType : string;
    Fmetadata : TMetadata;
    Fname : string;
    FnetworkInterfaces : TInstancenetworkInterfaces;
    Fscheduling : TScheduling;
    FselfLink : string;
    FserviceAccounts : TInstanceserviceAccounts;
    Fstatus : string;
    FstatusMessage : string;
    Ftags : TRoutetags;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetcanIpForward(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcpuPlatform(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setdisks(AIndex : Integer; AValue : TInstancedisks); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmachineType(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TMetadata); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetnetworkInterfaces(AIndex : Integer; AValue : TInstancenetworkInterfaces); virtual;
    Procedure Setscheduling(AIndex : Integer; AValue : TScheduling); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetserviceAccounts(AIndex : Integer; AValue : TInstanceserviceAccounts); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : string); virtual;
    Procedure Settags(AIndex : Integer; AValue : TRoutetags); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property canIpForward : boolean Index 0 Read FcanIpForward Write SetcanIpForward;
    Property cpuPlatform : string Index 8 Read FcpuPlatform Write SetcpuPlatform;
    Property creationTimestamp : string Index 16 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property disks : TInstancedisks Index 32 Read Fdisks Write Setdisks;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property machineType : string Index 56 Read FmachineType Write SetmachineType;
    Property metadata : TMetadata Index 64 Read Fmetadata Write Setmetadata;
    Property name : string Index 72 Read Fname Write Setname;
    Property networkInterfaces : TInstancenetworkInterfaces Index 80 Read FnetworkInterfaces Write SetnetworkInterfaces;
    Property scheduling : TScheduling Index 88 Read Fscheduling Write Setscheduling;
    Property selfLink : string Index 96 Read FselfLink Write SetselfLink;
    Property serviceAccounts : TInstanceserviceAccounts Index 104 Read FserviceAccounts Write SetserviceAccounts;
    Property status : string Index 112 Read Fstatus Write Setstatus;
    Property statusMessage : string Index 120 Read FstatusMessage Write SetstatusMessage;
    Property tags : TRoutetags Index 128 Read Ftags Write Settags;
    Property zone : string Index 136 Read Fzone Write Setzone;
  end;
  TInstanceClass = Class of TInstance;
  
  { --------------------------------------------------------------------
    TInstancedisks
    --------------------------------------------------------------------}
  
  TInstancedisks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstancedisksClass = Class of TInstancedisks;
  
  { --------------------------------------------------------------------
    TInstancenetworkInterfaces
    --------------------------------------------------------------------}
  
  TInstancenetworkInterfaces = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstancenetworkInterfacesClass = Class of TInstancenetworkInterfaces;
  
  { --------------------------------------------------------------------
    TInstanceserviceAccounts
    --------------------------------------------------------------------}
  
  TInstanceserviceAccounts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceserviceAccountsClass = Class of TInstanceserviceAccounts;
  
  { --------------------------------------------------------------------
    TInstanceAggregatedList
    --------------------------------------------------------------------}
  
  TInstanceAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TInstanceAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TInstanceAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TInstanceAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TInstanceAggregatedListClass = Class of TInstanceAggregatedList;
  
  { --------------------------------------------------------------------
    TInstanceAggregatedListitems
    --------------------------------------------------------------------}
  
  TInstanceAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TInstanceAggregatedListitemsClass = Class of TInstanceAggregatedListitems;
  
  { --------------------------------------------------------------------
    TInstanceList
    --------------------------------------------------------------------}
  
  TInstanceList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TInstanceListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TInstanceListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TInstanceListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TInstanceListClass = Class of TInstanceList;
  
  { --------------------------------------------------------------------
    TInstanceListitems
    --------------------------------------------------------------------}
  
  TInstanceListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceListitemsClass = Class of TInstanceListitems;
  
  { --------------------------------------------------------------------
    TInstanceMoveRequest
    --------------------------------------------------------------------}
  
  TInstanceMoveRequest = Class(TGoogleBaseObject)
  Private
    FdestinationZone : string;
    FtargetInstance : string;
  Protected
    //Property setters
    Procedure SetdestinationZone(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetInstance(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property destinationZone : string Index 0 Read FdestinationZone Write SetdestinationZone;
    Property targetInstance : string Index 8 Read FtargetInstance Write SettargetInstance;
  end;
  TInstanceMoveRequestClass = Class of TInstanceMoveRequest;
  
  { --------------------------------------------------------------------
    TInstanceProperties
    --------------------------------------------------------------------}
  
  TInstanceProperties = Class(TGoogleBaseObject)
  Private
    FcanIpForward : boolean;
    Fdescription : string;
    Fdisks : TInstancePropertiesdisks;
    FmachineType : string;
    Fmetadata : TMetadata;
    FnetworkInterfaces : TInstancePropertiesnetworkInterfaces;
    Fscheduling : TScheduling;
    FserviceAccounts : TInstancePropertiesserviceAccounts;
    Ftags : TRoutetags;
  Protected
    //Property setters
    Procedure SetcanIpForward(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setdisks(AIndex : Integer; AValue : TInstancePropertiesdisks); virtual;
    Procedure SetmachineType(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TMetadata); virtual;
    Procedure SetnetworkInterfaces(AIndex : Integer; AValue : TInstancePropertiesnetworkInterfaces); virtual;
    Procedure Setscheduling(AIndex : Integer; AValue : TScheduling); virtual;
    Procedure SetserviceAccounts(AIndex : Integer; AValue : TInstancePropertiesserviceAccounts); virtual;
    Procedure Settags(AIndex : Integer; AValue : TRoutetags); virtual;
  Public
  Published
    Property canIpForward : boolean Index 0 Read FcanIpForward Write SetcanIpForward;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property disks : TInstancePropertiesdisks Index 16 Read Fdisks Write Setdisks;
    Property machineType : string Index 24 Read FmachineType Write SetmachineType;
    Property metadata : TMetadata Index 32 Read Fmetadata Write Setmetadata;
    Property networkInterfaces : TInstancePropertiesnetworkInterfaces Index 40 Read FnetworkInterfaces Write SetnetworkInterfaces;
    Property scheduling : TScheduling Index 48 Read Fscheduling Write Setscheduling;
    Property serviceAccounts : TInstancePropertiesserviceAccounts Index 56 Read FserviceAccounts Write SetserviceAccounts;
    Property tags : TRoutetags Index 64 Read Ftags Write Settags;
  end;
  TInstancePropertiesClass = Class of TInstanceProperties;
  
  { --------------------------------------------------------------------
    TInstancePropertiesdisks
    --------------------------------------------------------------------}
  
  TInstancePropertiesdisks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstancePropertiesdisksClass = Class of TInstancePropertiesdisks;
  
  { --------------------------------------------------------------------
    TInstancePropertiesnetworkInterfaces
    --------------------------------------------------------------------}
  
  TInstancePropertiesnetworkInterfaces = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstancePropertiesnetworkInterfacesClass = Class of TInstancePropertiesnetworkInterfaces;
  
  { --------------------------------------------------------------------
    TInstancePropertiesserviceAccounts
    --------------------------------------------------------------------}
  
  TInstancePropertiesserviceAccounts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstancePropertiesserviceAccountsClass = Class of TInstancePropertiesserviceAccounts;
  
  { --------------------------------------------------------------------
    TInstanceReference
    --------------------------------------------------------------------}
  
  TInstanceReference = Class(TGoogleBaseObject)
  Private
    Finstance : string;
  Protected
    //Property setters
    Procedure Setinstance(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property instance : string Index 0 Read Finstance Write Setinstance;
  end;
  TInstanceReferenceClass = Class of TInstanceReference;
  
  { --------------------------------------------------------------------
    TInstanceTemplate
    --------------------------------------------------------------------}
  
  TInstanceTemplate = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fproperties : TInstanceProperties;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TInstanceProperties); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
    Property properties : TInstanceProperties Index 40 Read Fproperties Write Setproperties;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
  end;
  TInstanceTemplateClass = Class of TInstanceTemplate;
  
  { --------------------------------------------------------------------
    TInstanceTemplateList
    --------------------------------------------------------------------}
  
  TInstanceTemplateList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TInstanceTemplateListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TInstanceTemplateListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TInstanceTemplateListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TInstanceTemplateListClass = Class of TInstanceTemplateList;
  
  { --------------------------------------------------------------------
    TInstanceTemplateListitems
    --------------------------------------------------------------------}
  
  TInstanceTemplateListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceTemplateListitemsClass = Class of TInstanceTemplateListitems;
  
  { --------------------------------------------------------------------
    TInstancesScopedList
    --------------------------------------------------------------------}
  
  TInstancesScopedList = Class(TGoogleBaseObject)
  Private
    Finstances : TInstancesScopedListinstances;
    Fwarning : TInstancesScopedListwarning;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TInstancesScopedListinstances); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TInstancesScopedListwarning); virtual;
  Public
  Published
    Property instances : TInstancesScopedListinstances Index 0 Read Finstances Write Setinstances;
    Property warning : TInstancesScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TInstancesScopedListClass = Class of TInstancesScopedList;
  
  { --------------------------------------------------------------------
    TInstancesScopedListinstances
    --------------------------------------------------------------------}
  
  TInstancesScopedListinstances = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstancesScopedListinstancesClass = Class of TInstancesScopedListinstances;
  
  { --------------------------------------------------------------------
    TInstancesScopedListwarning
    --------------------------------------------------------------------}
  
  TInstancesScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TInstancesScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TInstancesScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TInstancesScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TInstancesScopedListwarningClass = Class of TInstancesScopedListwarning;
  
  { --------------------------------------------------------------------
    TInstancesScopedListwarningdata
    --------------------------------------------------------------------}
  
  TInstancesScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TInstancesScopedListwarningdataClass = Class of TInstancesScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TLicense
    --------------------------------------------------------------------}
  
  TLicense = Class(TGoogleBaseObject)
  Private
    FchargesUseFee : boolean;
    Fkind : string;
    Fname : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetchargesUseFee(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property chargesUseFee : boolean Index 0 Read FchargesUseFee Write SetchargesUseFee;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TLicenseClass = Class of TLicense;
  
  { --------------------------------------------------------------------
    TMachineType
    --------------------------------------------------------------------}
  
  TMachineType = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdeprecated : TDeprecationStatus;
    Fdescription : string;
    FguestCpus : integer;
    Fid : string;
    FimageSpaceGb : integer;
    Fkind : string;
    FmaximumPersistentDisks : integer;
    FmaximumPersistentDisksSizeGb : string;
    FmemoryMb : integer;
    Fname : string;
    FscratchDisks : TMachineTypescratchDisks;
    FselfLink : string;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetguestCpus(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetimageSpaceGb(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaximumPersistentDisks(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaximumPersistentDisksSizeGb(AIndex : Integer; AValue : string); virtual;
    Procedure SetmemoryMb(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetscratchDisks(AIndex : Integer; AValue : TMachineTypescratchDisks); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property deprecated : TDeprecationStatus Index 8 Read Fdeprecated Write Setdeprecated;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property guestCpus : integer Index 24 Read FguestCpus Write SetguestCpus;
    Property id : string Index 32 Read Fid Write Setid;
    Property imageSpaceGb : integer Index 40 Read FimageSpaceGb Write SetimageSpaceGb;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property maximumPersistentDisks : integer Index 56 Read FmaximumPersistentDisks Write SetmaximumPersistentDisks;
    Property maximumPersistentDisksSizeGb : string Index 64 Read FmaximumPersistentDisksSizeGb Write SetmaximumPersistentDisksSizeGb;
    Property memoryMb : integer Index 72 Read FmemoryMb Write SetmemoryMb;
    Property name : string Index 80 Read Fname Write Setname;
    Property scratchDisks : TMachineTypescratchDisks Index 88 Read FscratchDisks Write SetscratchDisks;
    Property selfLink : string Index 96 Read FselfLink Write SetselfLink;
    Property zone : string Index 104 Read Fzone Write Setzone;
  end;
  TMachineTypeClass = Class of TMachineType;
  
  { --------------------------------------------------------------------
    TMachineTypescratchDisks
    --------------------------------------------------------------------}
  
  TMachineTypescratchDisks = Class(TGoogleBaseObject)
  Private
    FdiskGb : integer;
  Protected
    //Property setters
    Procedure SetdiskGb(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property diskGb : integer Index 0 Read FdiskGb Write SetdiskGb;
  end;
  TMachineTypescratchDisksClass = Class of TMachineTypescratchDisks;
  
  { --------------------------------------------------------------------
    TMachineTypeAggregatedList
    --------------------------------------------------------------------}
  
  TMachineTypeAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TMachineTypeAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TMachineTypeAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TMachineTypeAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TMachineTypeAggregatedListClass = Class of TMachineTypeAggregatedList;
  
  { --------------------------------------------------------------------
    TMachineTypeAggregatedListitems
    --------------------------------------------------------------------}
  
  TMachineTypeAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMachineTypeAggregatedListitemsClass = Class of TMachineTypeAggregatedListitems;
  
  { --------------------------------------------------------------------
    TMachineTypeList
    --------------------------------------------------------------------}
  
  TMachineTypeList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TMachineTypeListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TMachineTypeListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TMachineTypeListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TMachineTypeListClass = Class of TMachineTypeList;
  
  { --------------------------------------------------------------------
    TMachineTypeListitems
    --------------------------------------------------------------------}
  
  TMachineTypeListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMachineTypeListitemsClass = Class of TMachineTypeListitems;
  
  { --------------------------------------------------------------------
    TMachineTypesScopedList
    --------------------------------------------------------------------}
  
  TMachineTypesScopedList = Class(TGoogleBaseObject)
  Private
    FmachineTypes : TMachineTypesScopedListmachineTypes;
    Fwarning : TMachineTypesScopedListwarning;
  Protected
    //Property setters
    Procedure SetmachineTypes(AIndex : Integer; AValue : TMachineTypesScopedListmachineTypes); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TMachineTypesScopedListwarning); virtual;
  Public
  Published
    Property machineTypes : TMachineTypesScopedListmachineTypes Index 0 Read FmachineTypes Write SetmachineTypes;
    Property warning : TMachineTypesScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TMachineTypesScopedListClass = Class of TMachineTypesScopedList;
  
  { --------------------------------------------------------------------
    TMachineTypesScopedListmachineTypes
    --------------------------------------------------------------------}
  
  TMachineTypesScopedListmachineTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMachineTypesScopedListmachineTypesClass = Class of TMachineTypesScopedListmachineTypes;
  
  { --------------------------------------------------------------------
    TMachineTypesScopedListwarning
    --------------------------------------------------------------------}
  
  TMachineTypesScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TMachineTypesScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TMachineTypesScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TMachineTypesScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TMachineTypesScopedListwarningClass = Class of TMachineTypesScopedListwarning;
  
  { --------------------------------------------------------------------
    TMachineTypesScopedListwarningdata
    --------------------------------------------------------------------}
  
  TMachineTypesScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TMachineTypesScopedListwarningdataClass = Class of TMachineTypesScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    Ffingerprint : string;
    Fitems : TMetadataitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TMetadataitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property fingerprint : string Index 0 Read Ffingerprint Write Setfingerprint;
    Property items : TMetadataitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TMetadataitems
    --------------------------------------------------------------------}
  
  TMetadataitems = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TMetadataitemsClass = Class of TMetadataitems;
  
  { --------------------------------------------------------------------
    TNetwork
    --------------------------------------------------------------------}
  
  TNetwork = Class(TGoogleBaseObject)
  Private
    FIPv4Range : string;
    FcreationTimestamp : string;
    Fdescription : string;
    FgatewayIPv4 : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetIPv4Range(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetgatewayIPv4(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property IPv4Range : string Index 0 Read FIPv4Range Write SetIPv4Range;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property gatewayIPv4 : string Index 24 Read FgatewayIPv4 Write SetgatewayIPv4;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property name : string Index 48 Read Fname Write Setname;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
  end;
  TNetworkClass = Class of TNetwork;
  
  { --------------------------------------------------------------------
    TNetworkInterface
    --------------------------------------------------------------------}
  
  TNetworkInterface = Class(TGoogleBaseObject)
  Private
    FaccessConfigs : TNetworkInterfaceaccessConfigs;
    Fname : string;
    Fnetwork : string;
    FnetworkIP : string;
  Protected
    //Property setters
    Procedure SetaccessConfigs(AIndex : Integer; AValue : TNetworkInterfaceaccessConfigs); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetnetworkIP(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accessConfigs : TNetworkInterfaceaccessConfigs Index 0 Read FaccessConfigs Write SetaccessConfigs;
    Property name : string Index 8 Read Fname Write Setname;
    Property network : string Index 16 Read Fnetwork Write Setnetwork;
    Property networkIP : string Index 24 Read FnetworkIP Write SetnetworkIP;
  end;
  TNetworkInterfaceClass = Class of TNetworkInterface;
  
  { --------------------------------------------------------------------
    TNetworkInterfaceaccessConfigs
    --------------------------------------------------------------------}
  
  TNetworkInterfaceaccessConfigs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TNetworkInterfaceaccessConfigsClass = Class of TNetworkInterfaceaccessConfigs;
  
  { --------------------------------------------------------------------
    TNetworkList
    --------------------------------------------------------------------}
  
  TNetworkList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TNetworkListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TNetworkListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TNetworkListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TNetworkListClass = Class of TNetworkList;
  
  { --------------------------------------------------------------------
    TNetworkListitems
    --------------------------------------------------------------------}
  
  TNetworkListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TNetworkListitemsClass = Class of TNetworkListitems;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FclientOperationId : string;
    FcreationTimestamp : string;
    FendTime : string;
    Ferror : TOperationerror;
    FhttpErrorMessage : string;
    FhttpErrorStatusCode : integer;
    Fid : string;
    FinsertTime : string;
    Fkind : string;
    Fname : string;
    FoperationType : string;
    Fprogress : integer;
    Fregion : string;
    FselfLink : string;
    FstartTime : string;
    Fstatus : string;
    FstatusMessage : string;
    FtargetId : string;
    FtargetLink : string;
    Fuser : string;
    Fwarnings : TOperationwarnings;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetclientOperationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : string); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : string); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TOperationwarnings); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property clientOperationId : string Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property endTime : string Index 16 Read FendTime Write SetendTime;
    Property error : TOperationerror Index 24 Read Ferror Write Seterror;
    Property httpErrorMessage : string Index 32 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 40 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : string Index 48 Read Fid Write Setid;
    Property insertTime : string Index 56 Read FinsertTime Write SetinsertTime;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property name : string Index 72 Read Fname Write Setname;
    Property operationType : string Index 80 Read FoperationType Write SetoperationType;
    Property progress : integer Index 88 Read Fprogress Write Setprogress;
    Property region : string Index 96 Read Fregion Write Setregion;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property startTime : string Index 112 Read FstartTime Write SetstartTime;
    Property status : string Index 120 Read Fstatus Write Setstatus;
    Property statusMessage : string Index 128 Read FstatusMessage Write SetstatusMessage;
    Property targetId : string Index 136 Read FtargetId Write SettargetId;
    Property targetLink : string Index 144 Read FtargetLink Write SettargetLink;
    Property user : string Index 152 Read Fuser Write Setuser;
    Property warnings : TOperationwarnings Index 160 Read Fwarnings Write Setwarnings;
    Property zone : string Index 168 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationerror
    --------------------------------------------------------------------}
  
  TOperationerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationerrorerrors;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TOperationerrorerrors); virtual;
  Public
  Published
    Property errors : TOperationerrorerrors Index 0 Read Ferrors Write Seterrors;
  end;
  TOperationerrorClass = Class of TOperationerror;
  
  { --------------------------------------------------------------------
    TOperationerrorerrors
    --------------------------------------------------------------------}
  
  TOperationerrorerrors = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Flocation : string;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property location : string Index 8 Read Flocation Write Setlocation;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationerrorerrorsClass = Class of TOperationerrorerrors;
  
  { --------------------------------------------------------------------
    TOperationwarnings
    --------------------------------------------------------------------}
  
  TOperationwarnings = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TOperationwarningsdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationwarningsdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TOperationwarningsdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationwarningsClass = Class of TOperationwarnings;
  
  { --------------------------------------------------------------------
    TOperationwarningsdata
    --------------------------------------------------------------------}
  
  TOperationwarningsdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TOperationwarningsdataClass = Class of TOperationwarningsdata;
  
  { --------------------------------------------------------------------
    TOperationAggregatedList
    --------------------------------------------------------------------}
  
  TOperationAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TOperationAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOperationAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TOperationAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TOperationAggregatedListClass = Class of TOperationAggregatedList;
  
  { --------------------------------------------------------------------
    TOperationAggregatedListitems
    --------------------------------------------------------------------}
  
  TOperationAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationAggregatedListitemsClass = Class of TOperationAggregatedListitems;
  
  { --------------------------------------------------------------------
    TOperationList
    --------------------------------------------------------------------}
  
  TOperationList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TOperationListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOperationListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TOperationListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TOperationListClass = Class of TOperationList;
  
  { --------------------------------------------------------------------
    TOperationListitems
    --------------------------------------------------------------------}
  
  TOperationListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOperationListitemsClass = Class of TOperationListitems;
  
  { --------------------------------------------------------------------
    TOperationsScopedList
    --------------------------------------------------------------------}
  
  TOperationsScopedList = Class(TGoogleBaseObject)
  Private
    Foperations : TOperationsScopedListoperations;
    Fwarning : TOperationsScopedListwarning;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; AValue : TOperationsScopedListoperations); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TOperationsScopedListwarning); virtual;
  Public
  Published
    Property operations : TOperationsScopedListoperations Index 0 Read Foperations Write Setoperations;
    Property warning : TOperationsScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TOperationsScopedListClass = Class of TOperationsScopedList;
  
  { --------------------------------------------------------------------
    TOperationsScopedListoperations
    --------------------------------------------------------------------}
  
  TOperationsScopedListoperations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOperationsScopedListoperationsClass = Class of TOperationsScopedListoperations;
  
  { --------------------------------------------------------------------
    TOperationsScopedListwarning
    --------------------------------------------------------------------}
  
  TOperationsScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TOperationsScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationsScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TOperationsScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationsScopedListwarningClass = Class of TOperationsScopedListwarning;
  
  { --------------------------------------------------------------------
    TOperationsScopedListwarningdata
    --------------------------------------------------------------------}
  
  TOperationsScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TOperationsScopedListwarningdataClass = Class of TOperationsScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TPathMatcher
    --------------------------------------------------------------------}
  
  TPathMatcher = Class(TGoogleBaseObject)
  Private
    FdefaultService : string;
    Fdescription : string;
    Fname : string;
    FpathRules : TPathMatcherpathRules;
  Protected
    //Property setters
    Procedure SetdefaultService(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpathRules(AIndex : Integer; AValue : TPathMatcherpathRules); virtual;
  Public
  Published
    Property defaultService : string Index 0 Read FdefaultService Write SetdefaultService;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property name : string Index 16 Read Fname Write Setname;
    Property pathRules : TPathMatcherpathRules Index 24 Read FpathRules Write SetpathRules;
  end;
  TPathMatcherClass = Class of TPathMatcher;
  
  { --------------------------------------------------------------------
    TPathMatcherpathRules
    --------------------------------------------------------------------}
  
  TPathMatcherpathRules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPathMatcherpathRulesClass = Class of TPathMatcherpathRules;
  
  { --------------------------------------------------------------------
    TPathRule
    --------------------------------------------------------------------}
  
  TPathRule = Class(TGoogleBaseObject)
  Private
    Fpaths : TPathRulepaths;
    Fservice : string;
  Protected
    //Property setters
    Procedure Setpaths(AIndex : Integer; AValue : TPathRulepaths); virtual;
    Procedure Setservice(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property paths : TPathRulepaths Index 0 Read Fpaths Write Setpaths;
    Property service : string Index 8 Read Fservice Write Setservice;
  end;
  TPathRuleClass = Class of TPathRule;
  
  { --------------------------------------------------------------------
    TPathRulepaths
    --------------------------------------------------------------------}
  
  TPathRulepaths = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPathRulepathsClass = Class of TPathRulepaths;
  
  { --------------------------------------------------------------------
    TProject
    --------------------------------------------------------------------}
  
  TProject = Class(TGoogleBaseObject)
  Private
    FcommonInstanceMetadata : TMetadata;
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fquotas : TProjectquotas;
    FselfLink : string;
    FusageExportLocation : TUsageExportLocation;
  Protected
    //Property setters
    Procedure SetcommonInstanceMetadata(AIndex : Integer; AValue : TMetadata); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setquotas(AIndex : Integer; AValue : TProjectquotas); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetusageExportLocation(AIndex : Integer; AValue : TUsageExportLocation); virtual;
  Public
  Published
    Property commonInstanceMetadata : TMetadata Index 0 Read FcommonInstanceMetadata Write SetcommonInstanceMetadata;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property quotas : TProjectquotas Index 48 Read Fquotas Write Setquotas;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property usageExportLocation : TUsageExportLocation Index 64 Read FusageExportLocation Write SetusageExportLocation;
  end;
  TProjectClass = Class of TProject;
  
  { --------------------------------------------------------------------
    TProjectquotas
    --------------------------------------------------------------------}
  
  TProjectquotas = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProjectquotasClass = Class of TProjectquotas;
  
  { --------------------------------------------------------------------
    TQuota
    --------------------------------------------------------------------}
  
  TQuota = Class(TGoogleBaseObject)
  Private
    Flimit : double;
    Fmetric : string;
    Fusage : double;
  Protected
    //Property setters
    Procedure Setlimit(AIndex : Integer; AValue : double); virtual;
    Procedure Setmetric(AIndex : Integer; AValue : string); virtual;
    Procedure Setusage(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property limit : double Index 0 Read Flimit Write Setlimit;
    Property metric : string Index 8 Read Fmetric Write Setmetric;
    Property usage : double Index 16 Read Fusage Write Setusage;
  end;
  TQuotaClass = Class of TQuota;
  
  { --------------------------------------------------------------------
    TRegion
    --------------------------------------------------------------------}
  
  TRegion = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdeprecated : TDeprecationStatus;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fquotas : TRegionquotas;
    FselfLink : string;
    Fstatus : string;
    Fzones : TRegionzones;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setquotas(AIndex : Integer; AValue : TRegionquotas); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setzones(AIndex : Integer; AValue : TRegionzones); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property deprecated : TDeprecationStatus Index 8 Read Fdeprecated Write Setdeprecated;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property quotas : TRegionquotas Index 48 Read Fquotas Write Setquotas;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property status : string Index 64 Read Fstatus Write Setstatus;
    Property zones : TRegionzones Index 72 Read Fzones Write Setzones;
  end;
  TRegionClass = Class of TRegion;
  
  { --------------------------------------------------------------------
    TRegionquotas
    --------------------------------------------------------------------}
  
  TRegionquotas = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRegionquotasClass = Class of TRegionquotas;
  
  { --------------------------------------------------------------------
    TRegionzones
    --------------------------------------------------------------------}
  
  TRegionzones = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRegionzonesClass = Class of TRegionzones;
  
  { --------------------------------------------------------------------
    TRegionList
    --------------------------------------------------------------------}
  
  TRegionList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TRegionListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TRegionListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TRegionListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TRegionListClass = Class of TRegionList;
  
  { --------------------------------------------------------------------
    TRegionListitems
    --------------------------------------------------------------------}
  
  TRegionListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRegionListitemsClass = Class of TRegionListitems;
  
  { --------------------------------------------------------------------
    TResourceGroupReference
    --------------------------------------------------------------------}
  
  TResourceGroupReference = Class(TGoogleBaseObject)
  Private
    Fgroup : string;
  Protected
    //Property setters
    Procedure Setgroup(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property group : string Index 0 Read Fgroup Write Setgroup;
  end;
  TResourceGroupReferenceClass = Class of TResourceGroupReference;
  
  { --------------------------------------------------------------------
    TRoute
    --------------------------------------------------------------------}
  
  TRoute = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    FdestRange : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fnetwork : string;
    FnextHopGateway : string;
    FnextHopInstance : string;
    FnextHopIp : string;
    FnextHopNetwork : string;
    FnextHopVpnTunnel : string;
    Fpriority : integer;
    FselfLink : string;
    Ftags : TRoutetags;
    Fwarnings : TRoutewarnings;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdestRange(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextHopGateway(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextHopInstance(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextHopIp(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextHopNetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextHopVpnTunnel(AIndex : Integer; AValue : string); virtual;
    Procedure Setpriority(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settags(AIndex : Integer; AValue : TRoutetags); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TRoutewarnings); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property destRange : string Index 16 Read FdestRange Write SetdestRange;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property network : string Index 48 Read Fnetwork Write Setnetwork;
    Property nextHopGateway : string Index 56 Read FnextHopGateway Write SetnextHopGateway;
    Property nextHopInstance : string Index 64 Read FnextHopInstance Write SetnextHopInstance;
    Property nextHopIp : string Index 72 Read FnextHopIp Write SetnextHopIp;
    Property nextHopNetwork : string Index 80 Read FnextHopNetwork Write SetnextHopNetwork;
    Property nextHopVpnTunnel : string Index 88 Read FnextHopVpnTunnel Write SetnextHopVpnTunnel;
    Property priority : integer Index 96 Read Fpriority Write Setpriority;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property tags : TRoutetags Index 112 Read Ftags Write Settags;
    Property warnings : TRoutewarnings Index 120 Read Fwarnings Write Setwarnings;
  end;
  TRouteClass = Class of TRoute;
  
  { --------------------------------------------------------------------
    TRoutetags
    --------------------------------------------------------------------}
  
  TRoutetags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRoutetagsClass = Class of TRoutetags;
  
  { --------------------------------------------------------------------
    TRoutewarnings
    --------------------------------------------------------------------}
  
  TRoutewarnings = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TRoutewarningsdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TRoutewarningsdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TRoutewarningsdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TRoutewarningsClass = Class of TRoutewarnings;
  
  { --------------------------------------------------------------------
    TRoutewarningsdata
    --------------------------------------------------------------------}
  
  TRoutewarningsdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TRoutewarningsdataClass = Class of TRoutewarningsdata;
  
  { --------------------------------------------------------------------
    TRouteList
    --------------------------------------------------------------------}
  
  TRouteList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TRouteListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TRouteListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TRouteListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TRouteListClass = Class of TRouteList;
  
  { --------------------------------------------------------------------
    TRouteListitems
    --------------------------------------------------------------------}
  
  TRouteListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRouteListitemsClass = Class of TRouteListitems;
  
  { --------------------------------------------------------------------
    TScheduling
    --------------------------------------------------------------------}
  
  TScheduling = Class(TGoogleBaseObject)
  Private
    FautomaticRestart : boolean;
    FonHostMaintenance : string;
  Protected
    //Property setters
    Procedure SetautomaticRestart(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetonHostMaintenance(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property automaticRestart : boolean Index 0 Read FautomaticRestart Write SetautomaticRestart;
    Property onHostMaintenance : string Index 8 Read FonHostMaintenance Write SetonHostMaintenance;
  end;
  TSchedulingClass = Class of TScheduling;
  
  { --------------------------------------------------------------------
    TSerialPortOutput
    --------------------------------------------------------------------}
  
  TSerialPortOutput = Class(TGoogleBaseObject)
  Private
    Fcontents : string;
    Fkind : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setcontents(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property contents : string Index 0 Read Fcontents Write Setcontents;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property selfLink : string Index 16 Read FselfLink Write SetselfLink;
  end;
  TSerialPortOutputClass = Class of TSerialPortOutput;
  
  { --------------------------------------------------------------------
    TServiceAccount
    --------------------------------------------------------------------}
  
  TServiceAccount = Class(TGoogleBaseObject)
  Private
    Femail : string;
    Fscopes : TServiceAccountscopes;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setscopes(AIndex : Integer; AValue : TServiceAccountscopes); virtual;
  Public
  Published
    Property email : string Index 0 Read Femail Write Setemail;
    Property scopes : TServiceAccountscopes Index 8 Read Fscopes Write Setscopes;
  end;
  TServiceAccountClass = Class of TServiceAccount;
  
  { --------------------------------------------------------------------
    TServiceAccountscopes
    --------------------------------------------------------------------}
  
  TServiceAccountscopes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TServiceAccountscopesClass = Class of TServiceAccountscopes;
  
  { --------------------------------------------------------------------
    TSnapshot
    --------------------------------------------------------------------}
  
  TSnapshot = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    FdiskSizeGb : string;
    Fid : string;
    Fkind : string;
    Flicenses : TSnapshotlicenses;
    Fname : string;
    FselfLink : string;
    FsourceDisk : string;
    FsourceDiskId : string;
    Fstatus : string;
    FstorageBytes : string;
    FstorageBytesStatus : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiskSizeGb(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlicenses(AIndex : Integer; AValue : TSnapshotlicenses); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceDisk(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceDiskId(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstorageBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SetstorageBytesStatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property diskSizeGb : string Index 16 Read FdiskSizeGb Write SetdiskSizeGb;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property licenses : TSnapshotlicenses Index 40 Read Flicenses Write Setlicenses;
    Property name : string Index 48 Read Fname Write Setname;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property sourceDisk : string Index 64 Read FsourceDisk Write SetsourceDisk;
    Property sourceDiskId : string Index 72 Read FsourceDiskId Write SetsourceDiskId;
    Property status : string Index 80 Read Fstatus Write Setstatus;
    Property storageBytes : string Index 88 Read FstorageBytes Write SetstorageBytes;
    Property storageBytesStatus : string Index 96 Read FstorageBytesStatus Write SetstorageBytesStatus;
  end;
  TSnapshotClass = Class of TSnapshot;
  
  { --------------------------------------------------------------------
    TSnapshotlicenses
    --------------------------------------------------------------------}
  
  TSnapshotlicenses = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSnapshotlicensesClass = Class of TSnapshotlicenses;
  
  { --------------------------------------------------------------------
    TSnapshotList
    --------------------------------------------------------------------}
  
  TSnapshotList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TSnapshotListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSnapshotListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TSnapshotListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TSnapshotListClass = Class of TSnapshotList;
  
  { --------------------------------------------------------------------
    TSnapshotListitems
    --------------------------------------------------------------------}
  
  TSnapshotListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSnapshotListitemsClass = Class of TSnapshotListitems;
  
  { --------------------------------------------------------------------
    TTags
    --------------------------------------------------------------------}
  
  TTags = Class(TGoogleBaseObject)
  Private
    Ffingerprint : string;
    Fitems : TTagsitems;
  Protected
    //Property setters
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTagsitems); virtual;
  Public
  Published
    Property fingerprint : string Index 0 Read Ffingerprint Write Setfingerprint;
    Property items : TTagsitems Index 8 Read Fitems Write Setitems;
  end;
  TTagsClass = Class of TTags;
  
  { --------------------------------------------------------------------
    TTagsitems
    --------------------------------------------------------------------}
  
  TTagsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTagsitemsClass = Class of TTagsitems;
  
  { --------------------------------------------------------------------
    TTargetHttpProxy
    --------------------------------------------------------------------}
  
  TTargetHttpProxy = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FselfLink : string;
    FurlMap : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SeturlMap(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
    Property urlMap : string Index 48 Read FurlMap Write SeturlMap;
  end;
  TTargetHttpProxyClass = Class of TTargetHttpProxy;
  
  { --------------------------------------------------------------------
    TTargetHttpProxyList
    --------------------------------------------------------------------}
  
  TTargetHttpProxyList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TTargetHttpProxyListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTargetHttpProxyListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TTargetHttpProxyListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TTargetHttpProxyListClass = Class of TTargetHttpProxyList;
  
  { --------------------------------------------------------------------
    TTargetHttpProxyListitems
    --------------------------------------------------------------------}
  
  TTargetHttpProxyListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetHttpProxyListitemsClass = Class of TTargetHttpProxyListitems;
  
  { --------------------------------------------------------------------
    TTargetInstance
    --------------------------------------------------------------------}
  
  TTargetInstance = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Finstance : string;
    Fkind : string;
    Fname : string;
    FnatPolicy : string;
    FselfLink : string;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinstance(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetnatPolicy(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property id : string Index 16 Read Fid Write Setid;
    Property instance : string Index 24 Read Finstance Write Setinstance;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property natPolicy : string Index 48 Read FnatPolicy Write SetnatPolicy;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property zone : string Index 64 Read Fzone Write Setzone;
  end;
  TTargetInstanceClass = Class of TTargetInstance;
  
  { --------------------------------------------------------------------
    TTargetInstanceAggregatedList
    --------------------------------------------------------------------}
  
  TTargetInstanceAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TTargetInstanceAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTargetInstanceAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TTargetInstanceAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TTargetInstanceAggregatedListClass = Class of TTargetInstanceAggregatedList;
  
  { --------------------------------------------------------------------
    TTargetInstanceAggregatedListitems
    --------------------------------------------------------------------}
  
  TTargetInstanceAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTargetInstanceAggregatedListitemsClass = Class of TTargetInstanceAggregatedListitems;
  
  { --------------------------------------------------------------------
    TTargetInstanceList
    --------------------------------------------------------------------}
  
  TTargetInstanceList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TTargetInstanceListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTargetInstanceListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TTargetInstanceListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TTargetInstanceListClass = Class of TTargetInstanceList;
  
  { --------------------------------------------------------------------
    TTargetInstanceListitems
    --------------------------------------------------------------------}
  
  TTargetInstanceListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetInstanceListitemsClass = Class of TTargetInstanceListitems;
  
  { --------------------------------------------------------------------
    TTargetInstancesScopedList
    --------------------------------------------------------------------}
  
  TTargetInstancesScopedList = Class(TGoogleBaseObject)
  Private
    FtargetInstances : TTargetInstancesScopedListtargetInstances;
    Fwarning : TTargetInstancesScopedListwarning;
  Protected
    //Property setters
    Procedure SettargetInstances(AIndex : Integer; AValue : TTargetInstancesScopedListtargetInstances); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TTargetInstancesScopedListwarning); virtual;
  Public
  Published
    Property targetInstances : TTargetInstancesScopedListtargetInstances Index 0 Read FtargetInstances Write SettargetInstances;
    Property warning : TTargetInstancesScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TTargetInstancesScopedListClass = Class of TTargetInstancesScopedList;
  
  { --------------------------------------------------------------------
    TTargetInstancesScopedListtargetInstances
    --------------------------------------------------------------------}
  
  TTargetInstancesScopedListtargetInstances = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetInstancesScopedListtargetInstancesClass = Class of TTargetInstancesScopedListtargetInstances;
  
  { --------------------------------------------------------------------
    TTargetInstancesScopedListwarning
    --------------------------------------------------------------------}
  
  TTargetInstancesScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TTargetInstancesScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TTargetInstancesScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TTargetInstancesScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TTargetInstancesScopedListwarningClass = Class of TTargetInstancesScopedListwarning;
  
  { --------------------------------------------------------------------
    TTargetInstancesScopedListwarningdata
    --------------------------------------------------------------------}
  
  TTargetInstancesScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TTargetInstancesScopedListwarningdataClass = Class of TTargetInstancesScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TTargetPool
    --------------------------------------------------------------------}
  
  TTargetPool = Class(TGoogleBaseObject)
  Private
    FbackupPool : string;
    FcreationTimestamp : string;
    Fdescription : string;
    FfailoverRatio : integer;
    FhealthChecks : TTargetPoolhealthChecks;
    Fid : string;
    Finstances : TTargetPoolinstances;
    Fkind : string;
    Fname : string;
    Fregion : string;
    FselfLink : string;
    FsessionAffinity : string;
  Protected
    //Property setters
    Procedure SetbackupPool(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetfailoverRatio(AIndex : Integer; AValue : integer); virtual;
    Procedure SethealthChecks(AIndex : Integer; AValue : TTargetPoolhealthChecks); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinstances(AIndex : Integer; AValue : TTargetPoolinstances); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsessionAffinity(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property backupPool : string Index 0 Read FbackupPool Write SetbackupPool;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property failoverRatio : integer Index 24 Read FfailoverRatio Write SetfailoverRatio;
    Property healthChecks : TTargetPoolhealthChecks Index 32 Read FhealthChecks Write SethealthChecks;
    Property id : string Index 40 Read Fid Write Setid;
    Property instances : TTargetPoolinstances Index 48 Read Finstances Write Setinstances;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property name : string Index 64 Read Fname Write Setname;
    Property region : string Index 72 Read Fregion Write Setregion;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property sessionAffinity : string Index 88 Read FsessionAffinity Write SetsessionAffinity;
  end;
  TTargetPoolClass = Class of TTargetPool;
  
  { --------------------------------------------------------------------
    TTargetPoolhealthChecks
    --------------------------------------------------------------------}
  
  TTargetPoolhealthChecks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolhealthChecksClass = Class of TTargetPoolhealthChecks;
  
  { --------------------------------------------------------------------
    TTargetPoolinstances
    --------------------------------------------------------------------}
  
  TTargetPoolinstances = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolinstancesClass = Class of TTargetPoolinstances;
  
  { --------------------------------------------------------------------
    TTargetPoolAggregatedList
    --------------------------------------------------------------------}
  
  TTargetPoolAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TTargetPoolAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTargetPoolAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TTargetPoolAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TTargetPoolAggregatedListClass = Class of TTargetPoolAggregatedList;
  
  { --------------------------------------------------------------------
    TTargetPoolAggregatedListitems
    --------------------------------------------------------------------}
  
  TTargetPoolAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTargetPoolAggregatedListitemsClass = Class of TTargetPoolAggregatedListitems;
  
  { --------------------------------------------------------------------
    TTargetPoolInstanceHealth
    --------------------------------------------------------------------}
  
  TTargetPoolInstanceHealth = Class(TGoogleBaseObject)
  Private
    FhealthStatus : TTargetPoolInstanceHealthhealthStatus;
    Fkind : string;
  Protected
    //Property setters
    Procedure SethealthStatus(AIndex : Integer; AValue : TTargetPoolInstanceHealthhealthStatus); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property healthStatus : TTargetPoolInstanceHealthhealthStatus Index 0 Read FhealthStatus Write SethealthStatus;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TTargetPoolInstanceHealthClass = Class of TTargetPoolInstanceHealth;
  
  { --------------------------------------------------------------------
    TTargetPoolInstanceHealthhealthStatus
    --------------------------------------------------------------------}
  
  TTargetPoolInstanceHealthhealthStatus = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolInstanceHealthhealthStatusClass = Class of TTargetPoolInstanceHealthhealthStatus;
  
  { --------------------------------------------------------------------
    TTargetPoolList
    --------------------------------------------------------------------}
  
  TTargetPoolList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TTargetPoolListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTargetPoolListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TTargetPoolListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TTargetPoolListClass = Class of TTargetPoolList;
  
  { --------------------------------------------------------------------
    TTargetPoolListitems
    --------------------------------------------------------------------}
  
  TTargetPoolListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolListitemsClass = Class of TTargetPoolListitems;
  
  { --------------------------------------------------------------------
    TTargetPoolsAddHealthCheckRequest
    --------------------------------------------------------------------}
  
  TTargetPoolsAddHealthCheckRequest = Class(TGoogleBaseObject)
  Private
    FhealthChecks : TTargetPoolsAddHealthCheckRequesthealthChecks;
  Protected
    //Property setters
    Procedure SethealthChecks(AIndex : Integer; AValue : TTargetPoolsAddHealthCheckRequesthealthChecks); virtual;
  Public
  Published
    Property healthChecks : TTargetPoolsAddHealthCheckRequesthealthChecks Index 0 Read FhealthChecks Write SethealthChecks;
  end;
  TTargetPoolsAddHealthCheckRequestClass = Class of TTargetPoolsAddHealthCheckRequest;
  
  { --------------------------------------------------------------------
    TTargetPoolsAddHealthCheckRequesthealthChecks
    --------------------------------------------------------------------}
  
  TTargetPoolsAddHealthCheckRequesthealthChecks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolsAddHealthCheckRequesthealthChecksClass = Class of TTargetPoolsAddHealthCheckRequesthealthChecks;
  
  { --------------------------------------------------------------------
    TTargetPoolsAddInstanceRequest
    --------------------------------------------------------------------}
  
  TTargetPoolsAddInstanceRequest = Class(TGoogleBaseObject)
  Private
    Finstances : TTargetPoolsAddInstanceRequestinstances;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TTargetPoolsAddInstanceRequestinstances); virtual;
  Public
  Published
    Property instances : TTargetPoolsAddInstanceRequestinstances Index 0 Read Finstances Write Setinstances;
  end;
  TTargetPoolsAddInstanceRequestClass = Class of TTargetPoolsAddInstanceRequest;
  
  { --------------------------------------------------------------------
    TTargetPoolsAddInstanceRequestinstances
    --------------------------------------------------------------------}
  
  TTargetPoolsAddInstanceRequestinstances = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolsAddInstanceRequestinstancesClass = Class of TTargetPoolsAddInstanceRequestinstances;
  
  { --------------------------------------------------------------------
    TTargetPoolsRemoveHealthCheckRequest
    --------------------------------------------------------------------}
  
  TTargetPoolsRemoveHealthCheckRequest = Class(TGoogleBaseObject)
  Private
    FhealthChecks : TTargetPoolsRemoveHealthCheckRequesthealthChecks;
  Protected
    //Property setters
    Procedure SethealthChecks(AIndex : Integer; AValue : TTargetPoolsRemoveHealthCheckRequesthealthChecks); virtual;
  Public
  Published
    Property healthChecks : TTargetPoolsRemoveHealthCheckRequesthealthChecks Index 0 Read FhealthChecks Write SethealthChecks;
  end;
  TTargetPoolsRemoveHealthCheckRequestClass = Class of TTargetPoolsRemoveHealthCheckRequest;
  
  { --------------------------------------------------------------------
    TTargetPoolsRemoveHealthCheckRequesthealthChecks
    --------------------------------------------------------------------}
  
  TTargetPoolsRemoveHealthCheckRequesthealthChecks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolsRemoveHealthCheckRequesthealthChecksClass = Class of TTargetPoolsRemoveHealthCheckRequesthealthChecks;
  
  { --------------------------------------------------------------------
    TTargetPoolsRemoveInstanceRequest
    --------------------------------------------------------------------}
  
  TTargetPoolsRemoveInstanceRequest = Class(TGoogleBaseObject)
  Private
    Finstances : TTargetPoolsRemoveInstanceRequestinstances;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TTargetPoolsRemoveInstanceRequestinstances); virtual;
  Public
  Published
    Property instances : TTargetPoolsRemoveInstanceRequestinstances Index 0 Read Finstances Write Setinstances;
  end;
  TTargetPoolsRemoveInstanceRequestClass = Class of TTargetPoolsRemoveInstanceRequest;
  
  { --------------------------------------------------------------------
    TTargetPoolsRemoveInstanceRequestinstances
    --------------------------------------------------------------------}
  
  TTargetPoolsRemoveInstanceRequestinstances = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolsRemoveInstanceRequestinstancesClass = Class of TTargetPoolsRemoveInstanceRequestinstances;
  
  { --------------------------------------------------------------------
    TTargetPoolsScopedList
    --------------------------------------------------------------------}
  
  TTargetPoolsScopedList = Class(TGoogleBaseObject)
  Private
    FtargetPools : TTargetPoolsScopedListtargetPools;
    Fwarning : TTargetPoolsScopedListwarning;
  Protected
    //Property setters
    Procedure SettargetPools(AIndex : Integer; AValue : TTargetPoolsScopedListtargetPools); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TTargetPoolsScopedListwarning); virtual;
  Public
  Published
    Property targetPools : TTargetPoolsScopedListtargetPools Index 0 Read FtargetPools Write SettargetPools;
    Property warning : TTargetPoolsScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TTargetPoolsScopedListClass = Class of TTargetPoolsScopedList;
  
  { --------------------------------------------------------------------
    TTargetPoolsScopedListtargetPools
    --------------------------------------------------------------------}
  
  TTargetPoolsScopedListtargetPools = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetPoolsScopedListtargetPoolsClass = Class of TTargetPoolsScopedListtargetPools;
  
  { --------------------------------------------------------------------
    TTargetPoolsScopedListwarning
    --------------------------------------------------------------------}
  
  TTargetPoolsScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TTargetPoolsScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TTargetPoolsScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TTargetPoolsScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TTargetPoolsScopedListwarningClass = Class of TTargetPoolsScopedListwarning;
  
  { --------------------------------------------------------------------
    TTargetPoolsScopedListwarningdata
    --------------------------------------------------------------------}
  
  TTargetPoolsScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TTargetPoolsScopedListwarningdataClass = Class of TTargetPoolsScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TTargetReference
    --------------------------------------------------------------------}
  
  TTargetReference = Class(TGoogleBaseObject)
  Private
    Ftarget : string;
  Protected
    //Property setters
    Procedure Settarget(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property target : string Index 0 Read Ftarget Write Settarget;
  end;
  TTargetReferenceClass = Class of TTargetReference;
  
  { --------------------------------------------------------------------
    TTargetVpnGateway
    --------------------------------------------------------------------}
  
  TTargetVpnGateway = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    FforwardingRules : TTargetVpnGatewayforwardingRules;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fnetwork : string;
    Fregion : string;
    FselfLink : string;
    Fstatus : string;
    Ftunnels : TTargetVpnGatewaytunnels;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetforwardingRules(AIndex : Integer; AValue : TTargetVpnGatewayforwardingRules); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Settunnels(AIndex : Integer; AValue : TTargetVpnGatewaytunnels); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property forwardingRules : TTargetVpnGatewayforwardingRules Index 16 Read FforwardingRules Write SetforwardingRules;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property network : string Index 48 Read Fnetwork Write Setnetwork;
    Property region : string Index 56 Read Fregion Write Setregion;
    Property selfLink : string Index 64 Read FselfLink Write SetselfLink;
    Property status : string Index 72 Read Fstatus Write Setstatus;
    Property tunnels : TTargetVpnGatewaytunnels Index 80 Read Ftunnels Write Settunnels;
  end;
  TTargetVpnGatewayClass = Class of TTargetVpnGateway;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewayforwardingRules
    --------------------------------------------------------------------}
  
  TTargetVpnGatewayforwardingRules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetVpnGatewayforwardingRulesClass = Class of TTargetVpnGatewayforwardingRules;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewaytunnels
    --------------------------------------------------------------------}
  
  TTargetVpnGatewaytunnels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetVpnGatewaytunnelsClass = Class of TTargetVpnGatewaytunnels;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewayAggregatedList
    --------------------------------------------------------------------}
  
  TTargetVpnGatewayAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TTargetVpnGatewayAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTargetVpnGatewayAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TTargetVpnGatewayAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TTargetVpnGatewayAggregatedListClass = Class of TTargetVpnGatewayAggregatedList;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewayAggregatedListitems
    --------------------------------------------------------------------}
  
  TTargetVpnGatewayAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTargetVpnGatewayAggregatedListitemsClass = Class of TTargetVpnGatewayAggregatedListitems;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewayList
    --------------------------------------------------------------------}
  
  TTargetVpnGatewayList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TTargetVpnGatewayListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTargetVpnGatewayListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TTargetVpnGatewayListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TTargetVpnGatewayListClass = Class of TTargetVpnGatewayList;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewayListitems
    --------------------------------------------------------------------}
  
  TTargetVpnGatewayListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetVpnGatewayListitemsClass = Class of TTargetVpnGatewayListitems;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewaysScopedList
    --------------------------------------------------------------------}
  
  TTargetVpnGatewaysScopedList = Class(TGoogleBaseObject)
  Private
    FtargetVpnGateways : TTargetVpnGatewaysScopedListtargetVpnGateways;
    Fwarning : TTargetVpnGatewaysScopedListwarning;
  Protected
    //Property setters
    Procedure SettargetVpnGateways(AIndex : Integer; AValue : TTargetVpnGatewaysScopedListtargetVpnGateways); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TTargetVpnGatewaysScopedListwarning); virtual;
  Public
  Published
    Property targetVpnGateways : TTargetVpnGatewaysScopedListtargetVpnGateways Index 0 Read FtargetVpnGateways Write SettargetVpnGateways;
    Property warning : TTargetVpnGatewaysScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TTargetVpnGatewaysScopedListClass = Class of TTargetVpnGatewaysScopedList;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewaysScopedListtargetVpnGateways
    --------------------------------------------------------------------}
  
  TTargetVpnGatewaysScopedListtargetVpnGateways = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTargetVpnGatewaysScopedListtargetVpnGatewaysClass = Class of TTargetVpnGatewaysScopedListtargetVpnGateways;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewaysScopedListwarning
    --------------------------------------------------------------------}
  
  TTargetVpnGatewaysScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TTargetVpnGatewaysScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TTargetVpnGatewaysScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TTargetVpnGatewaysScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TTargetVpnGatewaysScopedListwarningClass = Class of TTargetVpnGatewaysScopedListwarning;
  
  { --------------------------------------------------------------------
    TTargetVpnGatewaysScopedListwarningdata
    --------------------------------------------------------------------}
  
  TTargetVpnGatewaysScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TTargetVpnGatewaysScopedListwarningdataClass = Class of TTargetVpnGatewaysScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TTestFailure
    --------------------------------------------------------------------}
  
  TTestFailure = Class(TGoogleBaseObject)
  Private
    FactualService : string;
    FexpectedService : string;
    Fhost : string;
    Fpath : string;
  Protected
    //Property setters
    Procedure SetactualService(AIndex : Integer; AValue : string); virtual;
    Procedure SetexpectedService(AIndex : Integer; AValue : string); virtual;
    Procedure Sethost(AIndex : Integer; AValue : string); virtual;
    Procedure Setpath(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property actualService : string Index 0 Read FactualService Write SetactualService;
    Property expectedService : string Index 8 Read FexpectedService Write SetexpectedService;
    Property host : string Index 16 Read Fhost Write Sethost;
    Property path : string Index 24 Read Fpath Write Setpath;
  end;
  TTestFailureClass = Class of TTestFailure;
  
  { --------------------------------------------------------------------
    TUrlMap
    --------------------------------------------------------------------}
  
  TUrlMap = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    FdefaultService : string;
    Fdescription : string;
    Ffingerprint : string;
    FhostRules : TUrlMaphostRules;
    Fid : string;
    Fkind : string;
    Fname : string;
    FpathMatchers : TUrlMappathMatchers;
    FselfLink : string;
    Ftests : TUrlMaptests;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultService(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure SethostRules(AIndex : Integer; AValue : TUrlMaphostRules); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpathMatchers(AIndex : Integer; AValue : TUrlMappathMatchers); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settests(AIndex : Integer; AValue : TUrlMaptests); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property defaultService : string Index 8 Read FdefaultService Write SetdefaultService;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property fingerprint : string Index 24 Read Ffingerprint Write Setfingerprint;
    Property hostRules : TUrlMaphostRules Index 32 Read FhostRules Write SethostRules;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property pathMatchers : TUrlMappathMatchers Index 64 Read FpathMatchers Write SetpathMatchers;
    Property selfLink : string Index 72 Read FselfLink Write SetselfLink;
    Property tests : TUrlMaptests Index 80 Read Ftests Write Settests;
  end;
  TUrlMapClass = Class of TUrlMap;
  
  { --------------------------------------------------------------------
    TUrlMaphostRules
    --------------------------------------------------------------------}
  
  TUrlMaphostRules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUrlMaphostRulesClass = Class of TUrlMaphostRules;
  
  { --------------------------------------------------------------------
    TUrlMappathMatchers
    --------------------------------------------------------------------}
  
  TUrlMappathMatchers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUrlMappathMatchersClass = Class of TUrlMappathMatchers;
  
  { --------------------------------------------------------------------
    TUrlMaptests
    --------------------------------------------------------------------}
  
  TUrlMaptests = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUrlMaptestsClass = Class of TUrlMaptests;
  
  { --------------------------------------------------------------------
    TUrlMapList
    --------------------------------------------------------------------}
  
  TUrlMapList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TUrlMapListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TUrlMapListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TUrlMapListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TUrlMapListClass = Class of TUrlMapList;
  
  { --------------------------------------------------------------------
    TUrlMapListitems
    --------------------------------------------------------------------}
  
  TUrlMapListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUrlMapListitemsClass = Class of TUrlMapListitems;
  
  { --------------------------------------------------------------------
    TUrlMapReference
    --------------------------------------------------------------------}
  
  TUrlMapReference = Class(TGoogleBaseObject)
  Private
    FurlMap : string;
  Protected
    //Property setters
    Procedure SeturlMap(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property urlMap : string Index 0 Read FurlMap Write SeturlMap;
  end;
  TUrlMapReferenceClass = Class of TUrlMapReference;
  
  { --------------------------------------------------------------------
    TUrlMapTest
    --------------------------------------------------------------------}
  
  TUrlMapTest = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fhost : string;
    Fpath : string;
    Fservice : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Sethost(AIndex : Integer; AValue : string); virtual;
    Procedure Setpath(AIndex : Integer; AValue : string); virtual;
    Procedure Setservice(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property host : string Index 8 Read Fhost Write Sethost;
    Property path : string Index 16 Read Fpath Write Setpath;
    Property service : string Index 24 Read Fservice Write Setservice;
  end;
  TUrlMapTestClass = Class of TUrlMapTest;
  
  { --------------------------------------------------------------------
    TUrlMapValidationResult
    --------------------------------------------------------------------}
  
  TUrlMapValidationResult = Class(TGoogleBaseObject)
  Private
    FloadErrors : TUrlMapValidationResultloadErrors;
    FloadSucceeded : boolean;
    FtestFailures : TUrlMapValidationResulttestFailures;
    FtestPassed : boolean;
  Protected
    //Property setters
    Procedure SetloadErrors(AIndex : Integer; AValue : TUrlMapValidationResultloadErrors); virtual;
    Procedure SetloadSucceeded(AIndex : Integer; AValue : boolean); virtual;
    Procedure SettestFailures(AIndex : Integer; AValue : TUrlMapValidationResulttestFailures); virtual;
    Procedure SettestPassed(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property loadErrors : TUrlMapValidationResultloadErrors Index 0 Read FloadErrors Write SetloadErrors;
    Property loadSucceeded : boolean Index 8 Read FloadSucceeded Write SetloadSucceeded;
    Property testFailures : TUrlMapValidationResulttestFailures Index 16 Read FtestFailures Write SettestFailures;
    Property testPassed : boolean Index 24 Read FtestPassed Write SettestPassed;
  end;
  TUrlMapValidationResultClass = Class of TUrlMapValidationResult;
  
  { --------------------------------------------------------------------
    TUrlMapValidationResultloadErrors
    --------------------------------------------------------------------}
  
  TUrlMapValidationResultloadErrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUrlMapValidationResultloadErrorsClass = Class of TUrlMapValidationResultloadErrors;
  
  { --------------------------------------------------------------------
    TUrlMapValidationResulttestFailures
    --------------------------------------------------------------------}
  
  TUrlMapValidationResulttestFailures = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUrlMapValidationResulttestFailuresClass = Class of TUrlMapValidationResulttestFailures;
  
  { --------------------------------------------------------------------
    TUrlMapsValidateRequest
    --------------------------------------------------------------------}
  
  TUrlMapsValidateRequest = Class(TGoogleBaseObject)
  Private
    Fresource : TUrlMap;
  Protected
    //Property setters
    Procedure Setresource(AIndex : Integer; AValue : TUrlMap); virtual;
  Public
  Published
    Property resource : TUrlMap Index 0 Read Fresource Write Setresource;
  end;
  TUrlMapsValidateRequestClass = Class of TUrlMapsValidateRequest;
  
  { --------------------------------------------------------------------
    TUrlMapsValidateResponse
    --------------------------------------------------------------------}
  
  TUrlMapsValidateResponse = Class(TGoogleBaseObject)
  Private
    Fresult : TUrlMapValidationResult;
  Protected
    //Property setters
    Procedure Setresult(AIndex : Integer; AValue : TUrlMapValidationResult); virtual;
  Public
  Published
    Property result : TUrlMapValidationResult Index 0 Read Fresult Write Setresult;
  end;
  TUrlMapsValidateResponseClass = Class of TUrlMapsValidateResponse;
  
  { --------------------------------------------------------------------
    TUsageExportLocation
    --------------------------------------------------------------------}
  
  TUsageExportLocation = Class(TGoogleBaseObject)
  Private
    FbucketName : string;
    FreportNamePrefix : string;
  Protected
    //Property setters
    Procedure SetbucketName(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportNamePrefix(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bucketName : string Index 0 Read FbucketName Write SetbucketName;
    Property reportNamePrefix : string Index 8 Read FreportNamePrefix Write SetreportNamePrefix;
  end;
  TUsageExportLocationClass = Class of TUsageExportLocation;
  
  { --------------------------------------------------------------------
    TVpnTunnel
    --------------------------------------------------------------------}
  
  TVpnTunnel = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    FdetailedStatus : string;
    Fid : string;
    FikeNetworks : TVpnTunnelikeNetworks;
    FikeVersion : integer;
    Fkind : string;
    Fname : string;
    FpeerIp : string;
    Fregion : string;
    FselfLink : string;
    FsharedSecret : string;
    FsharedSecretHash : string;
    Fstatus : string;
    FtargetVpnGateway : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdetailedStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetikeNetworks(AIndex : Integer; AValue : TVpnTunnelikeNetworks); virtual;
    Procedure SetikeVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpeerIp(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsharedSecret(AIndex : Integer; AValue : string); virtual;
    Procedure SetsharedSecretHash(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetVpnGateway(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property detailedStatus : string Index 16 Read FdetailedStatus Write SetdetailedStatus;
    Property id : string Index 24 Read Fid Write Setid;
    Property ikeNetworks : TVpnTunnelikeNetworks Index 32 Read FikeNetworks Write SetikeNetworks;
    Property ikeVersion : integer Index 40 Read FikeVersion Write SetikeVersion;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property peerIp : string Index 64 Read FpeerIp Write SetpeerIp;
    Property region : string Index 72 Read Fregion Write Setregion;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property sharedSecret : string Index 88 Read FsharedSecret Write SetsharedSecret;
    Property sharedSecretHash : string Index 96 Read FsharedSecretHash Write SetsharedSecretHash;
    Property status : string Index 104 Read Fstatus Write Setstatus;
    Property targetVpnGateway : string Index 112 Read FtargetVpnGateway Write SettargetVpnGateway;
  end;
  TVpnTunnelClass = Class of TVpnTunnel;
  
  { --------------------------------------------------------------------
    TVpnTunnelikeNetworks
    --------------------------------------------------------------------}
  
  TVpnTunnelikeNetworks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVpnTunnelikeNetworksClass = Class of TVpnTunnelikeNetworks;
  
  { --------------------------------------------------------------------
    TVpnTunnelAggregatedList
    --------------------------------------------------------------------}
  
  TVpnTunnelAggregatedList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TVpnTunnelAggregatedListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TVpnTunnelAggregatedListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TVpnTunnelAggregatedListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TVpnTunnelAggregatedListClass = Class of TVpnTunnelAggregatedList;
  
  { --------------------------------------------------------------------
    TVpnTunnelAggregatedListitems
    --------------------------------------------------------------------}
  
  TVpnTunnelAggregatedListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TVpnTunnelAggregatedListitemsClass = Class of TVpnTunnelAggregatedListitems;
  
  { --------------------------------------------------------------------
    TVpnTunnelList
    --------------------------------------------------------------------}
  
  TVpnTunnelList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TVpnTunnelListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TVpnTunnelListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TVpnTunnelListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TVpnTunnelListClass = Class of TVpnTunnelList;
  
  { --------------------------------------------------------------------
    TVpnTunnelListitems
    --------------------------------------------------------------------}
  
  TVpnTunnelListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVpnTunnelListitemsClass = Class of TVpnTunnelListitems;
  
  { --------------------------------------------------------------------
    TVpnTunnelsScopedList
    --------------------------------------------------------------------}
  
  TVpnTunnelsScopedList = Class(TGoogleBaseObject)
  Private
    FvpnTunnels : TVpnTunnelsScopedListvpnTunnels;
    Fwarning : TVpnTunnelsScopedListwarning;
  Protected
    //Property setters
    Procedure SetvpnTunnels(AIndex : Integer; AValue : TVpnTunnelsScopedListvpnTunnels); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TVpnTunnelsScopedListwarning); virtual;
  Public
  Published
    Property vpnTunnels : TVpnTunnelsScopedListvpnTunnels Index 0 Read FvpnTunnels Write SetvpnTunnels;
    Property warning : TVpnTunnelsScopedListwarning Index 8 Read Fwarning Write Setwarning;
  end;
  TVpnTunnelsScopedListClass = Class of TVpnTunnelsScopedList;
  
  { --------------------------------------------------------------------
    TVpnTunnelsScopedListvpnTunnels
    --------------------------------------------------------------------}
  
  TVpnTunnelsScopedListvpnTunnels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVpnTunnelsScopedListvpnTunnelsClass = Class of TVpnTunnelsScopedListvpnTunnels;
  
  { --------------------------------------------------------------------
    TVpnTunnelsScopedListwarning
    --------------------------------------------------------------------}
  
  TVpnTunnelsScopedListwarning = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TVpnTunnelsScopedListwarningdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TVpnTunnelsScopedListwarningdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TVpnTunnelsScopedListwarningdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TVpnTunnelsScopedListwarningClass = Class of TVpnTunnelsScopedListwarning;
  
  { --------------------------------------------------------------------
    TVpnTunnelsScopedListwarningdata
    --------------------------------------------------------------------}
  
  TVpnTunnelsScopedListwarningdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TVpnTunnelsScopedListwarningdataClass = Class of TVpnTunnelsScopedListwarningdata;
  
  { --------------------------------------------------------------------
    TZone
    --------------------------------------------------------------------}
  
  TZone = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdeprecated : TDeprecationStatus;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    FmaintenanceWindows : TZonemaintenanceWindows;
    Fname : string;
    Fregion : string;
    FselfLink : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaintenanceWindows(AIndex : Integer; AValue : TZonemaintenanceWindows); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property deprecated : TDeprecationStatus Index 8 Read Fdeprecated Write Setdeprecated;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property maintenanceWindows : TZonemaintenanceWindows Index 40 Read FmaintenanceWindows Write SetmaintenanceWindows;
    Property name : string Index 48 Read Fname Write Setname;
    Property region : string Index 56 Read Fregion Write Setregion;
    Property selfLink : string Index 64 Read FselfLink Write SetselfLink;
    Property status : string Index 72 Read Fstatus Write Setstatus;
  end;
  TZoneClass = Class of TZone;
  
  { --------------------------------------------------------------------
    TZonemaintenanceWindows
    --------------------------------------------------------------------}
  
  TZonemaintenanceWindows = Class(TGoogleBaseObject)
  Private
    FbeginTime : string;
    Fdescription : string;
    FendTime : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetbeginTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property beginTime : string Index 0 Read FbeginTime Write SetbeginTime;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property endTime : string Index 16 Read FendTime Write SetendTime;
    Property name : string Index 24 Read Fname Write Setname;
  end;
  TZonemaintenanceWindowsClass = Class of TZonemaintenanceWindows;
  
  { --------------------------------------------------------------------
    TZoneList
    --------------------------------------------------------------------}
  
  TZoneList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TZoneListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TZoneListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TZoneListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TZoneListClass = Class of TZoneList;
  
  { --------------------------------------------------------------------
    TZoneListitems
    --------------------------------------------------------------------}
  
  TZoneListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TZoneListitemsClass = Class of TZoneListitems;
  
  { --------------------------------------------------------------------
    TAddressesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAddressesResource, method AggregatedList
  
  TAddressesAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TAddressesResource, method List
  
  TAddressesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TAddressesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TAddressAggregatedList;
    Function AggregatedList(project: string; AQuery : TAddressesaggregatedListOptions) : TAddressAggregatedList;
    Function Delete(address: string; project: string; region: string) : TOperation;
    Function Get(address: string; project: string; region: string) : TAddress;
    Function Insert(project: string; region: string; aAddress : TAddress) : TOperation;
    Function List(project: string; region: string; AQuery : string  = '') : TAddressList;
    Function List(project: string; region: string; AQuery : TAddresseslistOptions) : TAddressList;
  end;
  
  
  { --------------------------------------------------------------------
    TBackendServicesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBackendServicesResource, method List
  
  TBackendServicesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TBackendServicesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(backendService: string; project: string) : TOperation;
    Function Get(backendService: string; project: string) : TBackendService;
    Function GetHealth(backendService: string; project: string; aResourceGroupReference : TResourceGroupReference) : TBackendServiceGroupHealth;
    Function Insert(project: string; aBackendService : TBackendService) : TOperation;
    Function List(project: string; AQuery : string  = '') : TBackendServiceList;
    Function List(project: string; AQuery : TBackendServiceslistOptions) : TBackendServiceList;
    Function Patch(backendService: string; project: string; aBackendService : TBackendService) : TOperation;
    Function Update(backendService: string; project: string; aBackendService : TBackendService) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TDiskTypesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDiskTypesResource, method AggregatedList
  
  TDiskTypesAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TDiskTypesResource, method List
  
  TDiskTypesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TDiskTypesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TDiskTypeAggregatedList;
    Function AggregatedList(project: string; AQuery : TDiskTypesaggregatedListOptions) : TDiskTypeAggregatedList;
    Function Get(diskType: string; project: string; zone: string) : TDiskType;
    Function List(project: string; zone: string; AQuery : string  = '') : TDiskTypeList;
    Function List(project: string; zone: string; AQuery : TDiskTypeslistOptions) : TDiskTypeList;
  end;
  
  
  { --------------------------------------------------------------------
    TDisksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDisksResource, method AggregatedList
  
  TDisksAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TDisksResource, method Insert
  
  TDisksInsertOptions = Record
    sourceImage : string;
  end;
  
  
  //Optional query Options for TDisksResource, method List
  
  TDisksListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TDisksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TDiskAggregatedList;
    Function AggregatedList(project: string; AQuery : TDisksaggregatedListOptions) : TDiskAggregatedList;
    Function CreateSnapshot(disk: string; project: string; zone: string; aSnapshot : TSnapshot) : TOperation;
    Function Delete(disk: string; project: string; zone: string) : TOperation;
    Function Get(disk: string; project: string; zone: string) : TDisk;
    Function Insert(project: string; zone: string; aDisk : TDisk; AQuery : string  = '') : TOperation;
    Function Insert(project: string; zone: string; aDisk : TDisk; AQuery : TDisksinsertOptions) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TDiskList;
    Function List(project: string; zone: string; AQuery : TDiskslistOptions) : TDiskList;
  end;
  
  
  { --------------------------------------------------------------------
    TFirewallsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TFirewallsResource, method List
  
  TFirewallsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TFirewallsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(firewall: string; project: string) : TOperation;
    Function Get(firewall: string; project: string) : TFirewall;
    Function Insert(project: string; aFirewall : TFirewall) : TOperation;
    Function List(project: string; AQuery : string  = '') : TFirewallList;
    Function List(project: string; AQuery : TFirewallslistOptions) : TFirewallList;
    Function Patch(firewall: string; project: string; aFirewall : TFirewall) : TOperation;
    Function Update(firewall: string; project: string; aFirewall : TFirewall) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TForwardingRulesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TForwardingRulesResource, method AggregatedList
  
  TForwardingRulesAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TForwardingRulesResource, method List
  
  TForwardingRulesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TForwardingRulesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TForwardingRuleAggregatedList;
    Function AggregatedList(project: string; AQuery : TForwardingRulesaggregatedListOptions) : TForwardingRuleAggregatedList;
    Function Delete(forwardingRule: string; project: string; region: string) : TOperation;
    Function Get(forwardingRule: string; project: string; region: string) : TForwardingRule;
    Function Insert(project: string; region: string; aForwardingRule : TForwardingRule) : TOperation;
    Function List(project: string; region: string; AQuery : string  = '') : TForwardingRuleList;
    Function List(project: string; region: string; AQuery : TForwardingRuleslistOptions) : TForwardingRuleList;
    Function SetTarget(forwardingRule: string; project: string; region: string; aTargetReference : TTargetReference) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TGlobalAddressesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGlobalAddressesResource, method List
  
  TGlobalAddressesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TGlobalAddressesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(address: string; project: string) : TOperation;
    Function Get(address: string; project: string) : TAddress;
    Function Insert(project: string; aAddress : TAddress) : TOperation;
    Function List(project: string; AQuery : string  = '') : TAddressList;
    Function List(project: string; AQuery : TGlobalAddresseslistOptions) : TAddressList;
  end;
  
  
  { --------------------------------------------------------------------
    TGlobalForwardingRulesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGlobalForwardingRulesResource, method List
  
  TGlobalForwardingRulesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TGlobalForwardingRulesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(forwardingRule: string; project: string) : TOperation;
    Function Get(forwardingRule: string; project: string) : TForwardingRule;
    Function Insert(project: string; aForwardingRule : TForwardingRule) : TOperation;
    Function List(project: string; AQuery : string  = '') : TForwardingRuleList;
    Function List(project: string; AQuery : TGlobalForwardingRuleslistOptions) : TForwardingRuleList;
    Function SetTarget(forwardingRule: string; project: string; aTargetReference : TTargetReference) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TGlobalOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGlobalOperationsResource, method AggregatedList
  
  TGlobalOperationsAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TGlobalOperationsResource, method List
  
  TGlobalOperationsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TGlobalOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TOperationAggregatedList;
    Function AggregatedList(project: string; AQuery : TGlobalOperationsaggregatedListOptions) : TOperationAggregatedList;
    Procedure Delete(operation: string; project: string);
    Function Get(operation: string; project: string) : TOperation;
    Function List(project: string; AQuery : string  = '') : TOperationList;
    Function List(project: string; AQuery : TGlobalOperationslistOptions) : TOperationList;
  end;
  
  
  { --------------------------------------------------------------------
    THttpHealthChecksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for THttpHealthChecksResource, method List
  
  THttpHealthChecksListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  THttpHealthChecksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(httpHealthCheck: string; project: string) : TOperation;
    Function Get(httpHealthCheck: string; project: string) : THttpHealthCheck;
    Function Insert(project: string; aHttpHealthCheck : THttpHealthCheck) : TOperation;
    Function List(project: string; AQuery : string  = '') : THttpHealthCheckList;
    Function List(project: string; AQuery : THttpHealthCheckslistOptions) : THttpHealthCheckList;
    Function Patch(httpHealthCheck: string; project: string; aHttpHealthCheck : THttpHealthCheck) : TOperation;
    Function Update(httpHealthCheck: string; project: string; aHttpHealthCheck : THttpHealthCheck) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TImagesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TImagesResource, method List
  
  TImagesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TImagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(image: string; project: string) : TOperation;
    Function Deprecate(image: string; project: string; aDeprecationStatus : TDeprecationStatus) : TOperation;
    Function Get(image: string; project: string) : TImage;
    Function Insert(project: string; aImage : TImage) : TOperation;
    Function List(project: string; AQuery : string  = '') : TImageList;
    Function List(project: string; AQuery : TImageslistOptions) : TImageList;
  end;
  
  
  { --------------------------------------------------------------------
    TInstanceTemplatesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TInstanceTemplatesResource, method List
  
  TInstanceTemplatesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TInstanceTemplatesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(instanceTemplate: string; project: string) : TOperation;
    Function Get(instanceTemplate: string; project: string) : TInstanceTemplate;
    Function Insert(project: string; aInstanceTemplate : TInstanceTemplate) : TOperation;
    Function List(project: string; AQuery : string  = '') : TInstanceTemplateList;
    Function List(project: string; AQuery : TInstanceTemplateslistOptions) : TInstanceTemplateList;
  end;
  
  
  { --------------------------------------------------------------------
    TInstancesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TInstancesResource, method AddAccessConfig
  
  TInstancesAddAccessConfigOptions = Record
    networkInterface : string;
  end;
  
  
  //Optional query Options for TInstancesResource, method AggregatedList
  
  TInstancesAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TInstancesResource, method DeleteAccessConfig
  
  TInstancesDeleteAccessConfigOptions = Record
    accessConfig : string;
    networkInterface : string;
  end;
  
  
  //Optional query Options for TInstancesResource, method DetachDisk
  
  TInstancesDetachDiskOptions = Record
    deviceName : string;
  end;
  
  
  //Optional query Options for TInstancesResource, method GetSerialPortOutput
  
  TInstancesGetSerialPortOutputOptions = Record
    port : integer;
  end;
  
  
  //Optional query Options for TInstancesResource, method List
  
  TInstancesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TInstancesResource, method SetDiskAutoDelete
  
  TInstancesSetDiskAutoDeleteOptions = Record
    autoDelete : boolean;
    deviceName : string;
  end;
  
  TInstancesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AddAccessConfig(instance: string; project: string; zone: string; aAccessConfig : TAccessConfig; AQuery : string  = '') : TOperation;
    Function AddAccessConfig(instance: string; project: string; zone: string; aAccessConfig : TAccessConfig; AQuery : TInstancesaddAccessConfigOptions) : TOperation;
    Function AggregatedList(project: string; AQuery : string  = '') : TInstanceAggregatedList;
    Function AggregatedList(project: string; AQuery : TInstancesaggregatedListOptions) : TInstanceAggregatedList;
    Function AttachDisk(instance: string; project: string; zone: string; aAttachedDisk : TAttachedDisk) : TOperation;
    Function Delete(instance: string; project: string; zone: string) : TOperation;
    Function DeleteAccessConfig(instance: string; project: string; zone: string; AQuery : string  = '') : TOperation;
    Function DeleteAccessConfig(instance: string; project: string; zone: string; AQuery : TInstancesdeleteAccessConfigOptions) : TOperation;
    Function DetachDisk(instance: string; project: string; zone: string; AQuery : string  = '') : TOperation;
    Function DetachDisk(instance: string; project: string; zone: string; AQuery : TInstancesdetachDiskOptions) : TOperation;
    Function Get(instance: string; project: string; zone: string) : TInstance;
    Function GetSerialPortOutput(instance: string; project: string; zone: string; AQuery : string  = '') : TSerialPortOutput;
    Function GetSerialPortOutput(instance: string; project: string; zone: string; AQuery : TInstancesgetSerialPortOutputOptions) : TSerialPortOutput;
    Function Insert(project: string; zone: string; aInstance : TInstance) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TInstanceList;
    Function List(project: string; zone: string; AQuery : TInstanceslistOptions) : TInstanceList;
    Function Reset(instance: string; project: string; zone: string) : TOperation;
    Function SetDiskAutoDelete(instance: string; project: string; zone: string; AQuery : string  = '') : TOperation;
    Function SetDiskAutoDelete(instance: string; project: string; zone: string; AQuery : TInstancessetDiskAutoDeleteOptions) : TOperation;
    Function SetMetadata(instance: string; project: string; zone: string; aMetadata : TMetadata) : TOperation;
    Function SetScheduling(instance: string; project: string; zone: string; aScheduling : TScheduling) : TOperation;
    Function SetTags(instance: string; project: string; zone: string; aTags : TTags) : TOperation;
    Function Start(instance: string; project: string; zone: string) : TOperation;
    Function Stop(instance: string; project: string; zone: string) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TLicensesResource
    --------------------------------------------------------------------}
  
  TLicensesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(license: string; project: string) : TLicense;
  end;
  
  
  { --------------------------------------------------------------------
    TMachineTypesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMachineTypesResource, method AggregatedList
  
  TMachineTypesAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TMachineTypesResource, method List
  
  TMachineTypesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TMachineTypesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TMachineTypeAggregatedList;
    Function AggregatedList(project: string; AQuery : TMachineTypesaggregatedListOptions) : TMachineTypeAggregatedList;
    Function Get(machineType: string; project: string; zone: string) : TMachineType;
    Function List(project: string; zone: string; AQuery : string  = '') : TMachineTypeList;
    Function List(project: string; zone: string; AQuery : TMachineTypeslistOptions) : TMachineTypeList;
  end;
  
  
  { --------------------------------------------------------------------
    TNetworksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TNetworksResource, method List
  
  TNetworksListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TNetworksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(network: string; project: string) : TOperation;
    Function Get(network: string; project: string) : TNetwork;
    Function Insert(project: string; aNetwork : TNetwork) : TOperation;
    Function List(project: string; AQuery : string  = '') : TNetworkList;
    Function List(project: string; AQuery : TNetworkslistOptions) : TNetworkList;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(project: string) : TProject;
    Function MoveDisk(project: string; aDiskMoveRequest : TDiskMoveRequest) : TOperation;
    Function MoveInstance(project: string; aInstanceMoveRequest : TInstanceMoveRequest) : TOperation;
    Function SetCommonInstanceMetadata(project: string; aMetadata : TMetadata) : TOperation;
    Function SetUsageExportBucket(project: string; aUsageExportLocation : TUsageExportLocation) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TRegionOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRegionOperationsResource, method List
  
  TRegionOperationsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TRegionOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(operation: string; project: string; region: string);
    Function Get(operation: string; project: string; region: string) : TOperation;
    Function List(project: string; region: string; AQuery : string  = '') : TOperationList;
    Function List(project: string; region: string; AQuery : TRegionOperationslistOptions) : TOperationList;
  end;
  
  
  { --------------------------------------------------------------------
    TRegionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRegionsResource, method List
  
  TRegionsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TRegionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(project: string; region: string) : TRegion;
    Function List(project: string; AQuery : string  = '') : TRegionList;
    Function List(project: string; AQuery : TRegionslistOptions) : TRegionList;
  end;
  
  
  { --------------------------------------------------------------------
    TRoutesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRoutesResource, method List
  
  TRoutesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TRoutesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(project: string; route: string) : TOperation;
    Function Get(project: string; route: string) : TRoute;
    Function Insert(project: string; aRoute : TRoute) : TOperation;
    Function List(project: string; AQuery : string  = '') : TRouteList;
    Function List(project: string; AQuery : TRouteslistOptions) : TRouteList;
  end;
  
  
  { --------------------------------------------------------------------
    TSnapshotsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSnapshotsResource, method List
  
  TSnapshotsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TSnapshotsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(project: string; snapshot: string) : TOperation;
    Function Get(project: string; snapshot: string) : TSnapshot;
    Function List(project: string; AQuery : string  = '') : TSnapshotList;
    Function List(project: string; AQuery : TSnapshotslistOptions) : TSnapshotList;
  end;
  
  
  { --------------------------------------------------------------------
    TTargetHttpProxiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTargetHttpProxiesResource, method List
  
  TTargetHttpProxiesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TTargetHttpProxiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(project: string; targetHttpProxy: string) : TOperation;
    Function Get(project: string; targetHttpProxy: string) : TTargetHttpProxy;
    Function Insert(project: string; aTargetHttpProxy : TTargetHttpProxy) : TOperation;
    Function List(project: string; AQuery : string  = '') : TTargetHttpProxyList;
    Function List(project: string; AQuery : TTargetHttpProxieslistOptions) : TTargetHttpProxyList;
    Function SetUrlMap(project: string; targetHttpProxy: string; aUrlMapReference : TUrlMapReference) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TTargetInstancesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTargetInstancesResource, method AggregatedList
  
  TTargetInstancesAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TTargetInstancesResource, method List
  
  TTargetInstancesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TTargetInstancesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TTargetInstanceAggregatedList;
    Function AggregatedList(project: string; AQuery : TTargetInstancesaggregatedListOptions) : TTargetInstanceAggregatedList;
    Function Delete(project: string; targetInstance: string; zone: string) : TOperation;
    Function Get(project: string; targetInstance: string; zone: string) : TTargetInstance;
    Function Insert(project: string; zone: string; aTargetInstance : TTargetInstance) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TTargetInstanceList;
    Function List(project: string; zone: string; AQuery : TTargetInstanceslistOptions) : TTargetInstanceList;
  end;
  
  
  { --------------------------------------------------------------------
    TTargetPoolsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTargetPoolsResource, method AggregatedList
  
  TTargetPoolsAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TTargetPoolsResource, method List
  
  TTargetPoolsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TTargetPoolsResource, method SetBackup
  
  TTargetPoolsSetBackupOptions = Record
    failoverRatio : integer;
  end;
  
  TTargetPoolsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AddHealthCheck(project: string; region: string; targetPool: string; aTargetPoolsAddHealthCheckRequest : TTargetPoolsAddHealthCheckRequest) : TOperation;
    Function AddInstance(project: string; region: string; targetPool: string; aTargetPoolsAddInstanceRequest : TTargetPoolsAddInstanceRequest) : TOperation;
    Function AggregatedList(project: string; AQuery : string  = '') : TTargetPoolAggregatedList;
    Function AggregatedList(project: string; AQuery : TTargetPoolsaggregatedListOptions) : TTargetPoolAggregatedList;
    Function Delete(project: string; region: string; targetPool: string) : TOperation;
    Function Get(project: string; region: string; targetPool: string) : TTargetPool;
    Function GetHealth(project: string; region: string; targetPool: string; aInstanceReference : TInstanceReference) : TTargetPoolInstanceHealth;
    Function Insert(project: string; region: string; aTargetPool : TTargetPool) : TOperation;
    Function List(project: string; region: string; AQuery : string  = '') : TTargetPoolList;
    Function List(project: string; region: string; AQuery : TTargetPoolslistOptions) : TTargetPoolList;
    Function RemoveHealthCheck(project: string; region: string; targetPool: string; aTargetPoolsRemoveHealthCheckRequest : TTargetPoolsRemoveHealthCheckRequest) : TOperation;
    Function RemoveInstance(project: string; region: string; targetPool: string; aTargetPoolsRemoveInstanceRequest : TTargetPoolsRemoveInstanceRequest) : TOperation;
    Function SetBackup(project: string; region: string; targetPool: string; aTargetReference : TTargetReference; AQuery : string  = '') : TOperation;
    Function SetBackup(project: string; region: string; targetPool: string; aTargetReference : TTargetReference; AQuery : TTargetPoolssetBackupOptions) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TTargetVpnGatewaysResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTargetVpnGatewaysResource, method AggregatedList
  
  TTargetVpnGatewaysAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TTargetVpnGatewaysResource, method List
  
  TTargetVpnGatewaysListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TTargetVpnGatewaysResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TTargetVpnGatewayAggregatedList;
    Function AggregatedList(project: string; AQuery : TTargetVpnGatewaysaggregatedListOptions) : TTargetVpnGatewayAggregatedList;
    Function Delete(project: string; region: string; targetVpnGateway: string) : TOperation;
    Function Get(project: string; region: string; targetVpnGateway: string) : TTargetVpnGateway;
    Function Insert(project: string; region: string; aTargetVpnGateway : TTargetVpnGateway) : TOperation;
    Function List(project: string; region: string; AQuery : string  = '') : TTargetVpnGatewayList;
    Function List(project: string; region: string; AQuery : TTargetVpnGatewayslistOptions) : TTargetVpnGatewayList;
  end;
  
  
  { --------------------------------------------------------------------
    TUrlMapsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUrlMapsResource, method List
  
  TUrlMapsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TUrlMapsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(project: string; urlMap: string) : TOperation;
    Function Get(project: string; urlMap: string) : TUrlMap;
    Function Insert(project: string; aUrlMap : TUrlMap) : TOperation;
    Function List(project: string; AQuery : string  = '') : TUrlMapList;
    Function List(project: string; AQuery : TUrlMapslistOptions) : TUrlMapList;
    Function Patch(project: string; urlMap: string; aUrlMap : TUrlMap) : TOperation;
    Function Update(project: string; urlMap: string; aUrlMap : TUrlMap) : TOperation;
    Function Validate(project: string; urlMap: string; aUrlMapsValidateRequest : TUrlMapsValidateRequest) : TUrlMapsValidateResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TVpnTunnelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVpnTunnelsResource, method AggregatedList
  
  TVpnTunnelsAggregatedListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TVpnTunnelsResource, method List
  
  TVpnTunnelsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TVpnTunnelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AggregatedList(project: string; AQuery : string  = '') : TVpnTunnelAggregatedList;
    Function AggregatedList(project: string; AQuery : TVpnTunnelsaggregatedListOptions) : TVpnTunnelAggregatedList;
    Function Delete(project: string; region: string; vpnTunnel: string) : TOperation;
    Function Get(project: string; region: string; vpnTunnel: string) : TVpnTunnel;
    Function Insert(project: string; region: string; aVpnTunnel : TVpnTunnel) : TOperation;
    Function List(project: string; region: string; AQuery : string  = '') : TVpnTunnelList;
    Function List(project: string; region: string; AQuery : TVpnTunnelslistOptions) : TVpnTunnelList;
  end;
  
  
  { --------------------------------------------------------------------
    TZoneOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TZoneOperationsResource, method List
  
  TZoneOperationsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TZoneOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(operation: string; project: string; zone: string);
    Function Get(operation: string; project: string; zone: string) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TOperationList;
    Function List(project: string; zone: string; AQuery : TZoneOperationslistOptions) : TOperationList;
  end;
  
  
  { --------------------------------------------------------------------
    TZonesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TZonesResource, method List
  
  TZonesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TZonesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(project: string; zone: string) : TZone;
    Function List(project: string; AQuery : string  = '') : TZoneList;
    Function List(project: string; AQuery : TZoneslistOptions) : TZoneList;
  end;
  
  
  { --------------------------------------------------------------------
    TComputeAPI
    --------------------------------------------------------------------}
  
  TComputeAPI = Class(TGoogleAPI)
  Private
    FAddressesInstance : TAddressesResource;
    FBackendServicesInstance : TBackendServicesResource;
    FDiskTypesInstance : TDiskTypesResource;
    FDisksInstance : TDisksResource;
    FFirewallsInstance : TFirewallsResource;
    FForwardingRulesInstance : TForwardingRulesResource;
    FGlobalAddressesInstance : TGlobalAddressesResource;
    FGlobalForwardingRulesInstance : TGlobalForwardingRulesResource;
    FGlobalOperationsInstance : TGlobalOperationsResource;
    FHttpHealthChecksInstance : THttpHealthChecksResource;
    FImagesInstance : TImagesResource;
    FInstanceTemplatesInstance : TInstanceTemplatesResource;
    FInstancesInstance : TInstancesResource;
    FLicensesInstance : TLicensesResource;
    FMachineTypesInstance : TMachineTypesResource;
    FNetworksInstance : TNetworksResource;
    FProjectsInstance : TProjectsResource;
    FRegionOperationsInstance : TRegionOperationsResource;
    FRegionsInstance : TRegionsResource;
    FRoutesInstance : TRoutesResource;
    FSnapshotsInstance : TSnapshotsResource;
    FTargetHttpProxiesInstance : TTargetHttpProxiesResource;
    FTargetInstancesInstance : TTargetInstancesResource;
    FTargetPoolsInstance : TTargetPoolsResource;
    FTargetVpnGatewaysInstance : TTargetVpnGatewaysResource;
    FUrlMapsInstance : TUrlMapsResource;
    FVpnTunnelsInstance : TVpnTunnelsResource;
    FZoneOperationsInstance : TZoneOperationsResource;
    FZonesInstance : TZonesResource;
    Function GetAddressesInstance : TAddressesResource;virtual;
    Function GetBackendServicesInstance : TBackendServicesResource;virtual;
    Function GetDiskTypesInstance : TDiskTypesResource;virtual;
    Function GetDisksInstance : TDisksResource;virtual;
    Function GetFirewallsInstance : TFirewallsResource;virtual;
    Function GetForwardingRulesInstance : TForwardingRulesResource;virtual;
    Function GetGlobalAddressesInstance : TGlobalAddressesResource;virtual;
    Function GetGlobalForwardingRulesInstance : TGlobalForwardingRulesResource;virtual;
    Function GetGlobalOperationsInstance : TGlobalOperationsResource;virtual;
    Function GetHttpHealthChecksInstance : THttpHealthChecksResource;virtual;
    Function GetImagesInstance : TImagesResource;virtual;
    Function GetInstanceTemplatesInstance : TInstanceTemplatesResource;virtual;
    Function GetInstancesInstance : TInstancesResource;virtual;
    Function GetLicensesInstance : TLicensesResource;virtual;
    Function GetMachineTypesInstance : TMachineTypesResource;virtual;
    Function GetNetworksInstance : TNetworksResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
    Function GetRegionOperationsInstance : TRegionOperationsResource;virtual;
    Function GetRegionsInstance : TRegionsResource;virtual;
    Function GetRoutesInstance : TRoutesResource;virtual;
    Function GetSnapshotsInstance : TSnapshotsResource;virtual;
    Function GetTargetHttpProxiesInstance : TTargetHttpProxiesResource;virtual;
    Function GetTargetInstancesInstance : TTargetInstancesResource;virtual;
    Function GetTargetPoolsInstance : TTargetPoolsResource;virtual;
    Function GetTargetVpnGatewaysInstance : TTargetVpnGatewaysResource;virtual;
    Function GetUrlMapsInstance : TUrlMapsResource;virtual;
    Function GetVpnTunnelsInstance : TVpnTunnelsResource;virtual;
    Function GetZoneOperationsInstance : TZoneOperationsResource;virtual;
    Function GetZonesInstance : TZonesResource;virtual;
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
    Function CreateAddressesResource(AOwner : TComponent) : TAddressesResource;virtual;overload;
    Function CreateAddressesResource : TAddressesResource;virtual;overload;
    Function CreateBackendServicesResource(AOwner : TComponent) : TBackendServicesResource;virtual;overload;
    Function CreateBackendServicesResource : TBackendServicesResource;virtual;overload;
    Function CreateDiskTypesResource(AOwner : TComponent) : TDiskTypesResource;virtual;overload;
    Function CreateDiskTypesResource : TDiskTypesResource;virtual;overload;
    Function CreateDisksResource(AOwner : TComponent) : TDisksResource;virtual;overload;
    Function CreateDisksResource : TDisksResource;virtual;overload;
    Function CreateFirewallsResource(AOwner : TComponent) : TFirewallsResource;virtual;overload;
    Function CreateFirewallsResource : TFirewallsResource;virtual;overload;
    Function CreateForwardingRulesResource(AOwner : TComponent) : TForwardingRulesResource;virtual;overload;
    Function CreateForwardingRulesResource : TForwardingRulesResource;virtual;overload;
    Function CreateGlobalAddressesResource(AOwner : TComponent) : TGlobalAddressesResource;virtual;overload;
    Function CreateGlobalAddressesResource : TGlobalAddressesResource;virtual;overload;
    Function CreateGlobalForwardingRulesResource(AOwner : TComponent) : TGlobalForwardingRulesResource;virtual;overload;
    Function CreateGlobalForwardingRulesResource : TGlobalForwardingRulesResource;virtual;overload;
    Function CreateGlobalOperationsResource(AOwner : TComponent) : TGlobalOperationsResource;virtual;overload;
    Function CreateGlobalOperationsResource : TGlobalOperationsResource;virtual;overload;
    Function CreateHttpHealthChecksResource(AOwner : TComponent) : THttpHealthChecksResource;virtual;overload;
    Function CreateHttpHealthChecksResource : THttpHealthChecksResource;virtual;overload;
    Function CreateImagesResource(AOwner : TComponent) : TImagesResource;virtual;overload;
    Function CreateImagesResource : TImagesResource;virtual;overload;
    Function CreateInstanceTemplatesResource(AOwner : TComponent) : TInstanceTemplatesResource;virtual;overload;
    Function CreateInstanceTemplatesResource : TInstanceTemplatesResource;virtual;overload;
    Function CreateInstancesResource(AOwner : TComponent) : TInstancesResource;virtual;overload;
    Function CreateInstancesResource : TInstancesResource;virtual;overload;
    Function CreateLicensesResource(AOwner : TComponent) : TLicensesResource;virtual;overload;
    Function CreateLicensesResource : TLicensesResource;virtual;overload;
    Function CreateMachineTypesResource(AOwner : TComponent) : TMachineTypesResource;virtual;overload;
    Function CreateMachineTypesResource : TMachineTypesResource;virtual;overload;
    Function CreateNetworksResource(AOwner : TComponent) : TNetworksResource;virtual;overload;
    Function CreateNetworksResource : TNetworksResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    Function CreateRegionOperationsResource(AOwner : TComponent) : TRegionOperationsResource;virtual;overload;
    Function CreateRegionOperationsResource : TRegionOperationsResource;virtual;overload;
    Function CreateRegionsResource(AOwner : TComponent) : TRegionsResource;virtual;overload;
    Function CreateRegionsResource : TRegionsResource;virtual;overload;
    Function CreateRoutesResource(AOwner : TComponent) : TRoutesResource;virtual;overload;
    Function CreateRoutesResource : TRoutesResource;virtual;overload;
    Function CreateSnapshotsResource(AOwner : TComponent) : TSnapshotsResource;virtual;overload;
    Function CreateSnapshotsResource : TSnapshotsResource;virtual;overload;
    Function CreateTargetHttpProxiesResource(AOwner : TComponent) : TTargetHttpProxiesResource;virtual;overload;
    Function CreateTargetHttpProxiesResource : TTargetHttpProxiesResource;virtual;overload;
    Function CreateTargetInstancesResource(AOwner : TComponent) : TTargetInstancesResource;virtual;overload;
    Function CreateTargetInstancesResource : TTargetInstancesResource;virtual;overload;
    Function CreateTargetPoolsResource(AOwner : TComponent) : TTargetPoolsResource;virtual;overload;
    Function CreateTargetPoolsResource : TTargetPoolsResource;virtual;overload;
    Function CreateTargetVpnGatewaysResource(AOwner : TComponent) : TTargetVpnGatewaysResource;virtual;overload;
    Function CreateTargetVpnGatewaysResource : TTargetVpnGatewaysResource;virtual;overload;
    Function CreateUrlMapsResource(AOwner : TComponent) : TUrlMapsResource;virtual;overload;
    Function CreateUrlMapsResource : TUrlMapsResource;virtual;overload;
    Function CreateVpnTunnelsResource(AOwner : TComponent) : TVpnTunnelsResource;virtual;overload;
    Function CreateVpnTunnelsResource : TVpnTunnelsResource;virtual;overload;
    Function CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;virtual;overload;
    Function CreateZoneOperationsResource : TZoneOperationsResource;virtual;overload;
    Function CreateZonesResource(AOwner : TComponent) : TZonesResource;virtual;overload;
    Function CreateZonesResource : TZonesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AddressesResource : TAddressesResource Read GetAddressesInstance;
    Property BackendServicesResource : TBackendServicesResource Read GetBackendServicesInstance;
    Property DiskTypesResource : TDiskTypesResource Read GetDiskTypesInstance;
    Property DisksResource : TDisksResource Read GetDisksInstance;
    Property FirewallsResource : TFirewallsResource Read GetFirewallsInstance;
    Property ForwardingRulesResource : TForwardingRulesResource Read GetForwardingRulesInstance;
    Property GlobalAddressesResource : TGlobalAddressesResource Read GetGlobalAddressesInstance;
    Property GlobalForwardingRulesResource : TGlobalForwardingRulesResource Read GetGlobalForwardingRulesInstance;
    Property GlobalOperationsResource : TGlobalOperationsResource Read GetGlobalOperationsInstance;
    Property HttpHealthChecksResource : THttpHealthChecksResource Read GetHttpHealthChecksInstance;
    Property ImagesResource : TImagesResource Read GetImagesInstance;
    Property InstanceTemplatesResource : TInstanceTemplatesResource Read GetInstanceTemplatesInstance;
    Property InstancesResource : TInstancesResource Read GetInstancesInstance;
    Property LicensesResource : TLicensesResource Read GetLicensesInstance;
    Property MachineTypesResource : TMachineTypesResource Read GetMachineTypesInstance;
    Property NetworksResource : TNetworksResource Read GetNetworksInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
    Property RegionOperationsResource : TRegionOperationsResource Read GetRegionOperationsInstance;
    Property RegionsResource : TRegionsResource Read GetRegionsInstance;
    Property RoutesResource : TRoutesResource Read GetRoutesInstance;
    Property SnapshotsResource : TSnapshotsResource Read GetSnapshotsInstance;
    Property TargetHttpProxiesResource : TTargetHttpProxiesResource Read GetTargetHttpProxiesInstance;
    Property TargetInstancesResource : TTargetInstancesResource Read GetTargetInstancesInstance;
    Property TargetPoolsResource : TTargetPoolsResource Read GetTargetPoolsInstance;
    Property TargetVpnGatewaysResource : TTargetVpnGatewaysResource Read GetTargetVpnGatewaysInstance;
    Property UrlMapsResource : TUrlMapsResource Read GetUrlMapsInstance;
    Property VpnTunnelsResource : TVpnTunnelsResource Read GetVpnTunnelsInstance;
    Property ZoneOperationsResource : TZoneOperationsResource Read GetZoneOperationsInstance;
    Property ZonesResource : TZonesResource Read GetZonesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccessConfig
  --------------------------------------------------------------------}


Procedure TAccessConfig.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccessConfig.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccessConfig.SetnatIP(AIndex : Integer; AValue : string); 

begin
  If (FnatIP=AValue) then exit;
  FnatIP:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccessConfig.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAccessConfig.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAddress
  --------------------------------------------------------------------}


Procedure TAddress.Setaddress(AIndex : Integer; AValue : string); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setusers(AIndex : Integer; AValue : TAddressusers); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddressusers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAddressAggregatedList
  --------------------------------------------------------------------}


Procedure TAddressAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressAggregatedList.Setitems(AIndex : Integer; AValue : TAddressAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddressAggregatedListitems
  --------------------------------------------------------------------}


Class Function TAddressAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAddressList
  --------------------------------------------------------------------}


Procedure TAddressList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressList.Setitems(AIndex : Integer; AValue : TAddressListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddressListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAddressesScopedList
  --------------------------------------------------------------------}


Procedure TAddressesScopedList.Setaddresses(AIndex : Integer; AValue : TAddressesScopedListaddresses); 

begin
  If (Faddresses=AValue) then exit;
  Faddresses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressesScopedList.Setwarning(AIndex : Integer; AValue : TAddressesScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddressesScopedListaddresses
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAddressesScopedListwarning
  --------------------------------------------------------------------}


Procedure TAddressesScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressesScopedListwarning.Setdata(AIndex : Integer; AValue : TAddressesScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressesScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddressesScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TAddressesScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddressesScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAttachedDisk
  --------------------------------------------------------------------}


Procedure TAttachedDisk.SetautoDelete(AIndex : Integer; AValue : boolean); 

begin
  If (FautoDelete=AValue) then exit;
  FautoDelete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.Setboot(AIndex : Integer; AValue : boolean); 

begin
  If (Fboot=AValue) then exit;
  Fboot:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.SetdeviceName(AIndex : Integer; AValue : string); 

begin
  If (FdeviceName=AValue) then exit;
  FdeviceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.Setindex(AIndex : Integer; AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.SetinitializeParams(AIndex : Integer; AValue : TAttachedDiskInitializeParams); 

begin
  If (FinitializeParams=AValue) then exit;
  FinitializeParams:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.Set_interface(AIndex : Integer; AValue : string); 

begin
  If (F_interface=AValue) then exit;
  F_interface:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.Setlicenses(AIndex : Integer; AValue : TAttachedDisklicenses); 

begin
  If (Flicenses=AValue) then exit;
  Flicenses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.Setmode(AIndex : Integer; AValue : string); 

begin
  If (Fmode=AValue) then exit;
  Fmode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDisk.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAttachedDisk.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_interface' : Result:='interface';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAttachedDisklicenses
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAttachedDiskInitializeParams
  --------------------------------------------------------------------}


Procedure TAttachedDiskInitializeParams.SetdiskName(AIndex : Integer; AValue : string); 

begin
  If (FdiskName=AValue) then exit;
  FdiskName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDiskInitializeParams.SetdiskSizeGb(AIndex : Integer; AValue : string); 

begin
  If (FdiskSizeGb=AValue) then exit;
  FdiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDiskInitializeParams.SetdiskType(AIndex : Integer; AValue : string); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachedDiskInitializeParams.SetsourceImage(AIndex : Integer; AValue : string); 

begin
  If (FsourceImage=AValue) then exit;
  FsourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackend
  --------------------------------------------------------------------}


Procedure TBackend.SetbalancingMode(AIndex : Integer; AValue : string); 

begin
  If (FbalancingMode=AValue) then exit;
  FbalancingMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackend.SetcapacityScaler(AIndex : Integer; AValue : integer); 

begin
  If (FcapacityScaler=AValue) then exit;
  FcapacityScaler:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackend.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackend.Setgroup(AIndex : Integer; AValue : string); 

begin
  If (Fgroup=AValue) then exit;
  Fgroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackend.SetmaxRate(AIndex : Integer; AValue : integer); 

begin
  If (FmaxRate=AValue) then exit;
  FmaxRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackend.SetmaxRatePerInstance(AIndex : Integer; AValue : integer); 

begin
  If (FmaxRatePerInstance=AValue) then exit;
  FmaxRatePerInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackend.SetmaxUtilization(AIndex : Integer; AValue : integer); 

begin
  If (FmaxUtilization=AValue) then exit;
  FmaxUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackendService
  --------------------------------------------------------------------}


Procedure TBackendService.Setbackends(AIndex : Integer; AValue : TBackendServicebackends); 

begin
  If (Fbackends=AValue) then exit;
  Fbackends:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.SethealthChecks(AIndex : Integer; AValue : TBackendServicehealthChecks); 

begin
  If (FhealthChecks=AValue) then exit;
  FhealthChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.Setport(AIndex : Integer; AValue : integer); 

begin
  If (Fport=AValue) then exit;
  Fport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.SetportName(AIndex : Integer; AValue : string); 

begin
  If (FportName=AValue) then exit;
  FportName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.Setprotocol(AIndex : Integer; AValue : string); 

begin
  If (Fprotocol=AValue) then exit;
  Fprotocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendService.SettimeoutSec(AIndex : Integer; AValue : integer); 

begin
  If (FtimeoutSec=AValue) then exit;
  FtimeoutSec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackendServicebackends
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBackendServicehealthChecks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBackendServiceGroupHealth
  --------------------------------------------------------------------}


Procedure TBackendServiceGroupHealth.SethealthStatus(AIndex : Integer; AValue : TBackendServiceGroupHealthhealthStatus); 

begin
  If (FhealthStatus=AValue) then exit;
  FhealthStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendServiceGroupHealth.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackendServiceGroupHealthhealthStatus
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBackendServiceList
  --------------------------------------------------------------------}


Procedure TBackendServiceList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendServiceList.Setitems(AIndex : Integer; AValue : TBackendServiceListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendServiceList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendServiceList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackendServiceList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackendServiceListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDeprecationStatus
  --------------------------------------------------------------------}


Procedure TDeprecationStatus.Setdeleted(AIndex : Integer; AValue : string); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setdeprecated(AIndex : Integer; AValue : string); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setobsolete(AIndex : Integer; AValue : string); 

begin
  If (Fobsolete=AValue) then exit;
  Fobsolete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setreplacement(AIndex : Integer; AValue : string); 

begin
  If (Freplacement=AValue) then exit;
  Freplacement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setstate(AIndex : Integer; AValue : string); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDisk
  --------------------------------------------------------------------}


Procedure TDisk.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Setlicenses(AIndex : Integer; AValue : TDisklicenses); 

begin
  If (Flicenses=AValue) then exit;
  Flicenses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Setoptions(AIndex : Integer; AValue : string); 

begin
  If (Foptions=AValue) then exit;
  Foptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetsizeGb(AIndex : Integer; AValue : string); 

begin
  If (FsizeGb=AValue) then exit;
  FsizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetsourceImage(AIndex : Integer; AValue : string); 

begin
  If (FsourceImage=AValue) then exit;
  FsourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetsourceImageId(AIndex : Integer; AValue : string); 

begin
  If (FsourceImageId=AValue) then exit;
  FsourceImageId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetsourceSnapshot(AIndex : Integer; AValue : string); 

begin
  If (FsourceSnapshot=AValue) then exit;
  FsourceSnapshot:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetsourceSnapshotId(AIndex : Integer; AValue : string); 

begin
  If (FsourceSnapshotId=AValue) then exit;
  FsourceSnapshotId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDisk.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDisklicenses
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDiskAggregatedList
  --------------------------------------------------------------------}


Procedure TDiskAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskAggregatedList.Setitems(AIndex : Integer; AValue : TDiskAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskAggregatedListitems
  --------------------------------------------------------------------}


Class Function TDiskAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TDiskList
  --------------------------------------------------------------------}


Procedure TDiskList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskList.Setitems(AIndex : Integer; AValue : TDiskListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDiskMoveRequest
  --------------------------------------------------------------------}


Procedure TDiskMoveRequest.SetdestinationZone(AIndex : Integer; AValue : string); 

begin
  If (FdestinationZone=AValue) then exit;
  FdestinationZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskMoveRequest.SettargetDisk(AIndex : Integer; AValue : string); 

begin
  If (FtargetDisk=AValue) then exit;
  FtargetDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskType
  --------------------------------------------------------------------}


Procedure TDiskType.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.SetdefaultDiskSizeGb(AIndex : Integer; AValue : string); 

begin
  If (FdefaultDiskSizeGb=AValue) then exit;
  FdefaultDiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.SetvalidDiskSize(AIndex : Integer; AValue : string); 

begin
  If (FvalidDiskSize=AValue) then exit;
  FvalidDiskSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskType.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskTypeAggregatedList
  --------------------------------------------------------------------}


Procedure TDiskTypeAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypeAggregatedList.Setitems(AIndex : Integer; AValue : TDiskTypeAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypeAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypeAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypeAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskTypeAggregatedListitems
  --------------------------------------------------------------------}


Class Function TDiskTypeAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TDiskTypeList
  --------------------------------------------------------------------}


Procedure TDiskTypeList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypeList.Setitems(AIndex : Integer; AValue : TDiskTypeListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypeList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypeList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypeList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskTypeListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDiskTypesScopedList
  --------------------------------------------------------------------}


Procedure TDiskTypesScopedList.SetdiskTypes(AIndex : Integer; AValue : TDiskTypesScopedListdiskTypes); 

begin
  If (FdiskTypes=AValue) then exit;
  FdiskTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypesScopedList.Setwarning(AIndex : Integer; AValue : TDiskTypesScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskTypesScopedListdiskTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDiskTypesScopedListwarning
  --------------------------------------------------------------------}


Procedure TDiskTypesScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypesScopedListwarning.Setdata(AIndex : Integer; AValue : TDiskTypesScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypesScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskTypesScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TDiskTypesScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskTypesScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDisksScopedList
  --------------------------------------------------------------------}


Procedure TDisksScopedList.Setdisks(AIndex : Integer; AValue : TDisksScopedListdisks); 

begin
  If (Fdisks=AValue) then exit;
  Fdisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisksScopedList.Setwarning(AIndex : Integer; AValue : TDisksScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDisksScopedListdisks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDisksScopedListwarning
  --------------------------------------------------------------------}


Procedure TDisksScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisksScopedListwarning.Setdata(AIndex : Integer; AValue : TDisksScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisksScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDisksScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TDisksScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisksScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFirewall
  --------------------------------------------------------------------}


Procedure TFirewall.Setallowed(AIndex : Integer; AValue : TFirewallallowed); 

begin
  If (Fallowed=AValue) then exit;
  Fallowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.SetsourceRanges(AIndex : Integer; AValue : TFirewallsourceRanges); 

begin
  If (FsourceRanges=AValue) then exit;
  FsourceRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.SetsourceTags(AIndex : Integer; AValue : TFirewallsourceTags); 

begin
  If (FsourceTags=AValue) then exit;
  FsourceTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewall.SettargetTags(AIndex : Integer; AValue : TFirewalltargetTags); 

begin
  If (FtargetTags=AValue) then exit;
  FtargetTags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFirewallallowed
  --------------------------------------------------------------------}


Procedure TFirewallallowed.SetIPProtocol(AIndex : Integer; AValue : string); 

begin
  If (FIPProtocol=AValue) then exit;
  FIPProtocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallallowed.Setports(AIndex : Integer; AValue : TFirewallallowedports); 

begin
  If (Fports=AValue) then exit;
  Fports:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFirewallallowedports
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFirewallsourceRanges
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFirewallsourceTags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFirewalltargetTags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFirewallList
  --------------------------------------------------------------------}


Procedure TFirewallList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallList.Setitems(AIndex : Integer; AValue : TFirewallListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFirewallListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TForwardingRule
  --------------------------------------------------------------------}


Procedure TForwardingRule.SetIPAddress(AIndex : Integer; AValue : string); 

begin
  If (FIPAddress=AValue) then exit;
  FIPAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.SetIPProtocol(AIndex : Integer; AValue : string); 

begin
  If (FIPProtocol=AValue) then exit;
  FIPProtocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.SetportRange(AIndex : Integer; AValue : string); 

begin
  If (FportRange=AValue) then exit;
  FportRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRule.Settarget(AIndex : Integer; AValue : string); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TForwardingRuleAggregatedList
  --------------------------------------------------------------------}


Procedure TForwardingRuleAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRuleAggregatedList.Setitems(AIndex : Integer; AValue : TForwardingRuleAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRuleAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRuleAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRuleAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TForwardingRuleAggregatedListitems
  --------------------------------------------------------------------}


Class Function TForwardingRuleAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TForwardingRuleList
  --------------------------------------------------------------------}


Procedure TForwardingRuleList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRuleList.Setitems(AIndex : Integer; AValue : TForwardingRuleListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRuleList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRuleList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRuleList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TForwardingRuleListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TForwardingRulesScopedList
  --------------------------------------------------------------------}


Procedure TForwardingRulesScopedList.SetforwardingRules(AIndex : Integer; AValue : TForwardingRulesScopedListforwardingRules); 

begin
  If (FforwardingRules=AValue) then exit;
  FforwardingRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRulesScopedList.Setwarning(AIndex : Integer; AValue : TForwardingRulesScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TForwardingRulesScopedListforwardingRules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TForwardingRulesScopedListwarning
  --------------------------------------------------------------------}


Procedure TForwardingRulesScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRulesScopedListwarning.Setdata(AIndex : Integer; AValue : TForwardingRulesScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRulesScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TForwardingRulesScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TForwardingRulesScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForwardingRulesScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THealthCheckReference
  --------------------------------------------------------------------}


Procedure THealthCheckReference.SethealthCheck(AIndex : Integer; AValue : string); 

begin
  If (FhealthCheck=AValue) then exit;
  FhealthCheck:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THealthStatus
  --------------------------------------------------------------------}


Procedure THealthStatus.SethealthState(AIndex : Integer; AValue : string); 

begin
  If (FhealthState=AValue) then exit;
  FhealthState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthStatus.Setinstance(AIndex : Integer; AValue : string); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthStatus.SetipAddress(AIndex : Integer; AValue : string); 

begin
  If (FipAddress=AValue) then exit;
  FipAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthStatus.Setport(AIndex : Integer; AValue : integer); 

begin
  If (Fport=AValue) then exit;
  Fport:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THostRule
  --------------------------------------------------------------------}


Procedure THostRule.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THostRule.Sethosts(AIndex : Integer; AValue : THostRulehosts); 

begin
  If (Fhosts=AValue) then exit;
  Fhosts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THostRule.SetpathMatcher(AIndex : Integer; AValue : string); 

begin
  If (FpathMatcher=AValue) then exit;
  FpathMatcher:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THostRulehosts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  THttpHealthCheck
  --------------------------------------------------------------------}


Procedure THttpHealthCheck.SetcheckIntervalSec(AIndex : Integer; AValue : integer); 

begin
  If (FcheckIntervalSec=AValue) then exit;
  FcheckIntervalSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.SethealthyThreshold(AIndex : Integer; AValue : integer); 

begin
  If (FhealthyThreshold=AValue) then exit;
  FhealthyThreshold:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.Sethost(AIndex : Integer; AValue : string); 

begin
  If (Fhost=AValue) then exit;
  Fhost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.Setport(AIndex : Integer; AValue : integer); 

begin
  If (Fport=AValue) then exit;
  Fport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.SetrequestPath(AIndex : Integer; AValue : string); 

begin
  If (FrequestPath=AValue) then exit;
  FrequestPath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.SettimeoutSec(AIndex : Integer; AValue : integer); 

begin
  If (FtimeoutSec=AValue) then exit;
  FtimeoutSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheck.SetunhealthyThreshold(AIndex : Integer; AValue : integer); 

begin
  If (FunhealthyThreshold=AValue) then exit;
  FunhealthyThreshold:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THttpHealthCheckList
  --------------------------------------------------------------------}


Procedure THttpHealthCheckList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheckList.Setitems(AIndex : Integer; AValue : THttpHealthCheckListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheckList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheckList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpHealthCheckList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THttpHealthCheckListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImage
  --------------------------------------------------------------------}


Procedure TImage.SetarchiveSizeBytes(AIndex : Integer; AValue : string); 

begin
  If (FarchiveSizeBytes=AValue) then exit;
  FarchiveSizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetdiskSizeGb(AIndex : Integer; AValue : string); 

begin
  If (FdiskSizeGb=AValue) then exit;
  FdiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setlicenses(AIndex : Integer; AValue : TImagelicenses); 

begin
  If (Flicenses=AValue) then exit;
  Flicenses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetrawDisk(AIndex : Integer; AValue : TImagerawDisk); 

begin
  If (FrawDisk=AValue) then exit;
  FrawDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetsourceDisk(AIndex : Integer; AValue : string); 

begin
  If (FsourceDisk=AValue) then exit;
  FsourceDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetsourceDiskId(AIndex : Integer; AValue : string); 

begin
  If (FsourceDiskId=AValue) then exit;
  FsourceDiskId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetsourceType(AIndex : Integer; AValue : string); 

begin
  If (FsourceType=AValue) then exit;
  FsourceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImagelicenses
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImagerawDisk
  --------------------------------------------------------------------}


Procedure TImagerawDisk.SetcontainerType(AIndex : Integer; AValue : string); 

begin
  If (FcontainerType=AValue) then exit;
  FcontainerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImagerawDisk.Setsha1Checksum(AIndex : Integer; AValue : string); 

begin
  If (Fsha1Checksum=AValue) then exit;
  Fsha1Checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImagerawDisk.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImageList
  --------------------------------------------------------------------}


Procedure TImageList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageList.Setitems(AIndex : Integer; AValue : TImageListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImageListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstance
  --------------------------------------------------------------------}


Procedure TInstance.SetcanIpForward(AIndex : Integer; AValue : boolean); 

begin
  If (FcanIpForward=AValue) then exit;
  FcanIpForward:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetcpuPlatform(AIndex : Integer; AValue : string); 

begin
  If (FcpuPlatform=AValue) then exit;
  FcpuPlatform:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setdisks(AIndex : Integer; AValue : TInstancedisks); 

begin
  If (Fdisks=AValue) then exit;
  Fdisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetmachineType(AIndex : Integer; AValue : string); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setmetadata(AIndex : Integer; AValue : TMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetnetworkInterfaces(AIndex : Integer; AValue : TInstancenetworkInterfaces); 

begin
  If (FnetworkInterfaces=AValue) then exit;
  FnetworkInterfaces:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setscheduling(AIndex : Integer; AValue : TScheduling); 

begin
  If (Fscheduling=AValue) then exit;
  Fscheduling:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetserviceAccounts(AIndex : Integer; AValue : TInstanceserviceAccounts); 

begin
  If (FserviceAccounts=AValue) then exit;
  FserviceAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetstatusMessage(AIndex : Integer; AValue : string); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Settags(AIndex : Integer; AValue : TRoutetags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstancedisks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstancenetworkInterfaces
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceserviceAccounts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceAggregatedList
  --------------------------------------------------------------------}


Procedure TInstanceAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceAggregatedList.Setitems(AIndex : Integer; AValue : TInstanceAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceAggregatedListitems
  --------------------------------------------------------------------}


Class Function TInstanceAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInstanceList
  --------------------------------------------------------------------}


Procedure TInstanceList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceList.Setitems(AIndex : Integer; AValue : TInstanceListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceMoveRequest
  --------------------------------------------------------------------}


Procedure TInstanceMoveRequest.SetdestinationZone(AIndex : Integer; AValue : string); 

begin
  If (FdestinationZone=AValue) then exit;
  FdestinationZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceMoveRequest.SettargetInstance(AIndex : Integer; AValue : string); 

begin
  If (FtargetInstance=AValue) then exit;
  FtargetInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceProperties
  --------------------------------------------------------------------}


Procedure TInstanceProperties.SetcanIpForward(AIndex : Integer; AValue : boolean); 

begin
  If (FcanIpForward=AValue) then exit;
  FcanIpForward:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceProperties.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceProperties.Setdisks(AIndex : Integer; AValue : TInstancePropertiesdisks); 

begin
  If (Fdisks=AValue) then exit;
  Fdisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceProperties.SetmachineType(AIndex : Integer; AValue : string); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceProperties.Setmetadata(AIndex : Integer; AValue : TMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceProperties.SetnetworkInterfaces(AIndex : Integer; AValue : TInstancePropertiesnetworkInterfaces); 

begin
  If (FnetworkInterfaces=AValue) then exit;
  FnetworkInterfaces:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceProperties.Setscheduling(AIndex : Integer; AValue : TScheduling); 

begin
  If (Fscheduling=AValue) then exit;
  Fscheduling:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceProperties.SetserviceAccounts(AIndex : Integer; AValue : TInstancePropertiesserviceAccounts); 

begin
  If (FserviceAccounts=AValue) then exit;
  FserviceAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceProperties.Settags(AIndex : Integer; AValue : TRoutetags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstancePropertiesdisks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstancePropertiesnetworkInterfaces
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstancePropertiesserviceAccounts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceReference
  --------------------------------------------------------------------}


Procedure TInstanceReference.Setinstance(AIndex : Integer; AValue : string); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceTemplate
  --------------------------------------------------------------------}


Procedure TInstanceTemplate.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplate.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplate.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplate.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplate.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplate.Setproperties(AIndex : Integer; AValue : TInstanceProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplate.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceTemplateList
  --------------------------------------------------------------------}


Procedure TInstanceTemplateList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplateList.Setitems(AIndex : Integer; AValue : TInstanceTemplateListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplateList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplateList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceTemplateList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceTemplateListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstancesScopedList
  --------------------------------------------------------------------}


Procedure TInstancesScopedList.Setinstances(AIndex : Integer; AValue : TInstancesScopedListinstances); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstancesScopedList.Setwarning(AIndex : Integer; AValue : TInstancesScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstancesScopedListinstances
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstancesScopedListwarning
  --------------------------------------------------------------------}


Procedure TInstancesScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstancesScopedListwarning.Setdata(AIndex : Integer; AValue : TInstancesScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstancesScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstancesScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TInstancesScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstancesScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLicense
  --------------------------------------------------------------------}


Procedure TLicense.SetchargesUseFee(AIndex : Integer; AValue : boolean); 

begin
  If (FchargesUseFee=AValue) then exit;
  FchargesUseFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicense.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicense.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicense.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMachineType
  --------------------------------------------------------------------}


Procedure TMachineType.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.SetguestCpus(AIndex : Integer; AValue : integer); 

begin
  If (FguestCpus=AValue) then exit;
  FguestCpus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.SetimageSpaceGb(AIndex : Integer; AValue : integer); 

begin
  If (FimageSpaceGb=AValue) then exit;
  FimageSpaceGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.SetmaximumPersistentDisks(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumPersistentDisks=AValue) then exit;
  FmaximumPersistentDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.SetmaximumPersistentDisksSizeGb(AIndex : Integer; AValue : string); 

begin
  If (FmaximumPersistentDisksSizeGb=AValue) then exit;
  FmaximumPersistentDisksSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.SetmemoryMb(AIndex : Integer; AValue : integer); 

begin
  If (FmemoryMb=AValue) then exit;
  FmemoryMb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.SetscratchDisks(AIndex : Integer; AValue : TMachineTypescratchDisks); 

begin
  If (FscratchDisks=AValue) then exit;
  FscratchDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineType.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMachineTypescratchDisks
  --------------------------------------------------------------------}


Procedure TMachineTypescratchDisks.SetdiskGb(AIndex : Integer; AValue : integer); 

begin
  If (FdiskGb=AValue) then exit;
  FdiskGb:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMachineTypeAggregatedList
  --------------------------------------------------------------------}


Procedure TMachineTypeAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypeAggregatedList.Setitems(AIndex : Integer; AValue : TMachineTypeAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypeAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypeAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypeAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMachineTypeAggregatedListitems
  --------------------------------------------------------------------}


Class Function TMachineTypeAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMachineTypeList
  --------------------------------------------------------------------}


Procedure TMachineTypeList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypeList.Setitems(AIndex : Integer; AValue : TMachineTypeListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypeList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypeList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypeList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMachineTypeListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMachineTypesScopedList
  --------------------------------------------------------------------}


Procedure TMachineTypesScopedList.SetmachineTypes(AIndex : Integer; AValue : TMachineTypesScopedListmachineTypes); 

begin
  If (FmachineTypes=AValue) then exit;
  FmachineTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypesScopedList.Setwarning(AIndex : Integer; AValue : TMachineTypesScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMachineTypesScopedListmachineTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMachineTypesScopedListwarning
  --------------------------------------------------------------------}


Procedure TMachineTypesScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypesScopedListwarning.Setdata(AIndex : Integer; AValue : TMachineTypesScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypesScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMachineTypesScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TMachineTypesScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMachineTypesScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



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


Procedure TMetadataitems.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataitems.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetwork
  --------------------------------------------------------------------}


Procedure TNetwork.SetIPv4Range(AIndex : Integer; AValue : string); 

begin
  If (FIPv4Range=AValue) then exit;
  FIPv4Range:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.SetgatewayIPv4(AIndex : Integer; AValue : string); 

begin
  If (FgatewayIPv4=AValue) then exit;
  FgatewayIPv4:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetworkInterface
  --------------------------------------------------------------------}


Procedure TNetworkInterface.SetaccessConfigs(AIndex : Integer; AValue : TNetworkInterfaceaccessConfigs); 

begin
  If (FaccessConfigs=AValue) then exit;
  FaccessConfigs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.SetnetworkIP(AIndex : Integer; AValue : string); 

begin
  If (FnetworkIP=AValue) then exit;
  FnetworkIP:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetworkInterfaceaccessConfigs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TNetworkList
  --------------------------------------------------------------------}


Procedure TNetworkList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkList.Setitems(AIndex : Integer; AValue : TNetworkListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetworkListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; AValue : string); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; AValue : TOperationerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; AValue : string); 

begin
  If (FhttpErrorMessage=AValue) then exit;
  FhttpErrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorStatusCode(AIndex : Integer; AValue : integer); 

begin
  If (FhttpErrorStatusCode=AValue) then exit;
  FhttpErrorStatusCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; AValue : string); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : string); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setprogress(AIndex : Integer; AValue : integer); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; AValue : string); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; AValue : string); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; AValue : string); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : string); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; AValue : string); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; AValue : TOperationwarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationerror
  --------------------------------------------------------------------}


Procedure TOperationerror.Seterrors(AIndex : Integer; AValue : TOperationerrorerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationerrorerrors
  --------------------------------------------------------------------}


Procedure TOperationerrorerrors.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationerrorerrors.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationerrorerrors.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationwarnings
  --------------------------------------------------------------------}


Procedure TOperationwarnings.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarnings.Setdata(AIndex : Integer; AValue : TOperationwarningsdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarnings.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationwarningsdata
  --------------------------------------------------------------------}


Procedure TOperationwarningsdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarningsdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationAggregatedList
  --------------------------------------------------------------------}


Procedure TOperationAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationAggregatedList.Setitems(AIndex : Integer; AValue : TOperationAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationAggregatedListitems
  --------------------------------------------------------------------}


Class Function TOperationAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperationList
  --------------------------------------------------------------------}


Procedure TOperationList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setitems(AIndex : Integer; AValue : TOperationListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOperationsScopedList
  --------------------------------------------------------------------}


Procedure TOperationsScopedList.Setoperations(AIndex : Integer; AValue : TOperationsScopedListoperations); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsScopedList.Setwarning(AIndex : Integer; AValue : TOperationsScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationsScopedListoperations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOperationsScopedListwarning
  --------------------------------------------------------------------}


Procedure TOperationsScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsScopedListwarning.Setdata(AIndex : Integer; AValue : TOperationsScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationsScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TOperationsScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPathMatcher
  --------------------------------------------------------------------}


Procedure TPathMatcher.SetdefaultService(AIndex : Integer; AValue : string); 

begin
  If (FdefaultService=AValue) then exit;
  FdefaultService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathMatcher.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathMatcher.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathMatcher.SetpathRules(AIndex : Integer; AValue : TPathMatcherpathRules); 

begin
  If (FpathRules=AValue) then exit;
  FpathRules:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPathMatcherpathRules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPathRule
  --------------------------------------------------------------------}


Procedure TPathRule.Setpaths(AIndex : Integer; AValue : TPathRulepaths); 

begin
  If (Fpaths=AValue) then exit;
  Fpaths:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathRule.Setservice(AIndex : Integer; AValue : string); 

begin
  If (Fservice=AValue) then exit;
  Fservice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPathRulepaths
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProject
  --------------------------------------------------------------------}


Procedure TProject.SetcommonInstanceMetadata(AIndex : Integer; AValue : TMetadata); 

begin
  If (FcommonInstanceMetadata=AValue) then exit;
  FcommonInstanceMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setquotas(AIndex : Integer; AValue : TProjectquotas); 

begin
  If (Fquotas=AValue) then exit;
  Fquotas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetusageExportLocation(AIndex : Integer; AValue : TUsageExportLocation); 

begin
  If (FusageExportLocation=AValue) then exit;
  FusageExportLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectquotas
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TQuota
  --------------------------------------------------------------------}


Procedure TQuota.Setlimit(AIndex : Integer; AValue : double); 

begin
  If (Flimit=AValue) then exit;
  Flimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuota.Setmetric(AIndex : Integer; AValue : string); 

begin
  If (Fmetric=AValue) then exit;
  Fmetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuota.Setusage(AIndex : Integer; AValue : double); 

begin
  If (Fusage=AValue) then exit;
  Fusage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRegion
  --------------------------------------------------------------------}


Procedure TRegion.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setquotas(AIndex : Integer; AValue : TRegionquotas); 

begin
  If (Fquotas=AValue) then exit;
  Fquotas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegion.Setzones(AIndex : Integer; AValue : TRegionzones); 

begin
  If (Fzones=AValue) then exit;
  Fzones:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRegionquotas
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRegionzones
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRegionList
  --------------------------------------------------------------------}


Procedure TRegionList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegionList.Setitems(AIndex : Integer; AValue : TRegionListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegionList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegionList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRegionList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRegionListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TResourceGroupReference
  --------------------------------------------------------------------}


Procedure TResourceGroupReference.Setgroup(AIndex : Integer; AValue : string); 

begin
  If (Fgroup=AValue) then exit;
  Fgroup:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoute
  --------------------------------------------------------------------}


Procedure TRoute.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.SetdestRange(AIndex : Integer; AValue : string); 

begin
  If (FdestRange=AValue) then exit;
  FdestRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.SetnextHopGateway(AIndex : Integer; AValue : string); 

begin
  If (FnextHopGateway=AValue) then exit;
  FnextHopGateway:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.SetnextHopInstance(AIndex : Integer; AValue : string); 

begin
  If (FnextHopInstance=AValue) then exit;
  FnextHopInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.SetnextHopIp(AIndex : Integer; AValue : string); 

begin
  If (FnextHopIp=AValue) then exit;
  FnextHopIp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.SetnextHopNetwork(AIndex : Integer; AValue : string); 

begin
  If (FnextHopNetwork=AValue) then exit;
  FnextHopNetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.SetnextHopVpnTunnel(AIndex : Integer; AValue : string); 

begin
  If (FnextHopVpnTunnel=AValue) then exit;
  FnextHopVpnTunnel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.Setpriority(AIndex : Integer; AValue : integer); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.Settags(AIndex : Integer; AValue : TRoutetags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoute.Setwarnings(AIndex : Integer; AValue : TRoutewarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoutetags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRoutewarnings
  --------------------------------------------------------------------}


Procedure TRoutewarnings.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoutewarnings.Setdata(AIndex : Integer; AValue : TRoutewarningsdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoutewarnings.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoutewarningsdata
  --------------------------------------------------------------------}


Procedure TRoutewarningsdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoutewarningsdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRouteList
  --------------------------------------------------------------------}


Procedure TRouteList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRouteList.Setitems(AIndex : Integer; AValue : TRouteListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRouteList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRouteList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRouteList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRouteListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TScheduling
  --------------------------------------------------------------------}


Procedure TScheduling.SetautomaticRestart(AIndex : Integer; AValue : boolean); 

begin
  If (FautomaticRestart=AValue) then exit;
  FautomaticRestart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScheduling.SetonHostMaintenance(AIndex : Integer; AValue : string); 

begin
  If (FonHostMaintenance=AValue) then exit;
  FonHostMaintenance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSerialPortOutput
  --------------------------------------------------------------------}


Procedure TSerialPortOutput.Setcontents(AIndex : Integer; AValue : string); 

begin
  If (Fcontents=AValue) then exit;
  Fcontents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSerialPortOutput.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSerialPortOutput.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TServiceAccount
  --------------------------------------------------------------------}


Procedure TServiceAccount.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.Setscopes(AIndex : Integer; AValue : TServiceAccountscopes); 

begin
  If (Fscopes=AValue) then exit;
  Fscopes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TServiceAccountscopes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSnapshot
  --------------------------------------------------------------------}


Procedure TSnapshot.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetdiskSizeGb(AIndex : Integer; AValue : string); 

begin
  If (FdiskSizeGb=AValue) then exit;
  FdiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setlicenses(AIndex : Integer; AValue : TSnapshotlicenses); 

begin
  If (Flicenses=AValue) then exit;
  Flicenses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetsourceDisk(AIndex : Integer; AValue : string); 

begin
  If (FsourceDisk=AValue) then exit;
  FsourceDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetsourceDiskId(AIndex : Integer; AValue : string); 

begin
  If (FsourceDiskId=AValue) then exit;
  FsourceDiskId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetstorageBytes(AIndex : Integer; AValue : string); 

begin
  If (FstorageBytes=AValue) then exit;
  FstorageBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetstorageBytesStatus(AIndex : Integer; AValue : string); 

begin
  If (FstorageBytesStatus=AValue) then exit;
  FstorageBytesStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSnapshotlicenses
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSnapshotList
  --------------------------------------------------------------------}


Procedure TSnapshotList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotList.Setitems(AIndex : Integer; AValue : TSnapshotListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSnapshotListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTags
  --------------------------------------------------------------------}


Procedure TTags.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTags.Setitems(AIndex : Integer; AValue : TTagsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTagsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetHttpProxy
  --------------------------------------------------------------------}


Procedure TTargetHttpProxy.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxy.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxy.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxy.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxy.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxy.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxy.SeturlMap(AIndex : Integer; AValue : string); 

begin
  If (FurlMap=AValue) then exit;
  FurlMap:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetHttpProxyList
  --------------------------------------------------------------------}


Procedure TTargetHttpProxyList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxyList.Setitems(AIndex : Integer; AValue : TTargetHttpProxyListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxyList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxyList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetHttpProxyList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetHttpProxyListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetInstance
  --------------------------------------------------------------------}


Procedure TTargetInstance.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstance.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstance.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstance.Setinstance(AIndex : Integer; AValue : string); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstance.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstance.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstance.SetnatPolicy(AIndex : Integer; AValue : string); 

begin
  If (FnatPolicy=AValue) then exit;
  FnatPolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstance.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstance.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetInstanceAggregatedList
  --------------------------------------------------------------------}


Procedure TTargetInstanceAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstanceAggregatedList.Setitems(AIndex : Integer; AValue : TTargetInstanceAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstanceAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstanceAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstanceAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetInstanceAggregatedListitems
  --------------------------------------------------------------------}


Class Function TTargetInstanceAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTargetInstanceList
  --------------------------------------------------------------------}


Procedure TTargetInstanceList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstanceList.Setitems(AIndex : Integer; AValue : TTargetInstanceListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstanceList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstanceList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstanceList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetInstanceListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetInstancesScopedList
  --------------------------------------------------------------------}


Procedure TTargetInstancesScopedList.SettargetInstances(AIndex : Integer; AValue : TTargetInstancesScopedListtargetInstances); 

begin
  If (FtargetInstances=AValue) then exit;
  FtargetInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstancesScopedList.Setwarning(AIndex : Integer; AValue : TTargetInstancesScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetInstancesScopedListtargetInstances
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetInstancesScopedListwarning
  --------------------------------------------------------------------}


Procedure TTargetInstancesScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstancesScopedListwarning.Setdata(AIndex : Integer; AValue : TTargetInstancesScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstancesScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetInstancesScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TTargetInstancesScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetInstancesScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPool
  --------------------------------------------------------------------}


Procedure TTargetPool.SetbackupPool(AIndex : Integer; AValue : string); 

begin
  If (FbackupPool=AValue) then exit;
  FbackupPool:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.SetfailoverRatio(AIndex : Integer; AValue : integer); 

begin
  If (FfailoverRatio=AValue) then exit;
  FfailoverRatio:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.SethealthChecks(AIndex : Integer; AValue : TTargetPoolhealthChecks); 

begin
  If (FhealthChecks=AValue) then exit;
  FhealthChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.Setinstances(AIndex : Integer; AValue : TTargetPoolinstances); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPool.SetsessionAffinity(AIndex : Integer; AValue : string); 

begin
  If (FsessionAffinity=AValue) then exit;
  FsessionAffinity:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolhealthChecks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolinstances
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolAggregatedList
  --------------------------------------------------------------------}


Procedure TTargetPoolAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolAggregatedList.Setitems(AIndex : Integer; AValue : TTargetPoolAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolAggregatedListitems
  --------------------------------------------------------------------}


Class Function TTargetPoolAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTargetPoolInstanceHealth
  --------------------------------------------------------------------}


Procedure TTargetPoolInstanceHealth.SethealthStatus(AIndex : Integer; AValue : TTargetPoolInstanceHealthhealthStatus); 

begin
  If (FhealthStatus=AValue) then exit;
  FhealthStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolInstanceHealth.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolInstanceHealthhealthStatus
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolList
  --------------------------------------------------------------------}


Procedure TTargetPoolList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolList.Setitems(AIndex : Integer; AValue : TTargetPoolListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolsAddHealthCheckRequest
  --------------------------------------------------------------------}


Procedure TTargetPoolsAddHealthCheckRequest.SethealthChecks(AIndex : Integer; AValue : TTargetPoolsAddHealthCheckRequesthealthChecks); 

begin
  If (FhealthChecks=AValue) then exit;
  FhealthChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolsAddHealthCheckRequesthealthChecks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolsAddInstanceRequest
  --------------------------------------------------------------------}


Procedure TTargetPoolsAddInstanceRequest.Setinstances(AIndex : Integer; AValue : TTargetPoolsAddInstanceRequestinstances); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolsAddInstanceRequestinstances
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolsRemoveHealthCheckRequest
  --------------------------------------------------------------------}


Procedure TTargetPoolsRemoveHealthCheckRequest.SethealthChecks(AIndex : Integer; AValue : TTargetPoolsRemoveHealthCheckRequesthealthChecks); 

begin
  If (FhealthChecks=AValue) then exit;
  FhealthChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolsRemoveHealthCheckRequesthealthChecks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolsRemoveInstanceRequest
  --------------------------------------------------------------------}


Procedure TTargetPoolsRemoveInstanceRequest.Setinstances(AIndex : Integer; AValue : TTargetPoolsRemoveInstanceRequestinstances); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolsRemoveInstanceRequestinstances
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolsScopedList
  --------------------------------------------------------------------}


Procedure TTargetPoolsScopedList.SettargetPools(AIndex : Integer; AValue : TTargetPoolsScopedListtargetPools); 

begin
  If (FtargetPools=AValue) then exit;
  FtargetPools:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolsScopedList.Setwarning(AIndex : Integer; AValue : TTargetPoolsScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolsScopedListtargetPools
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetPoolsScopedListwarning
  --------------------------------------------------------------------}


Procedure TTargetPoolsScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolsScopedListwarning.Setdata(AIndex : Integer; AValue : TTargetPoolsScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolsScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetPoolsScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TTargetPoolsScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetPoolsScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetReference
  --------------------------------------------------------------------}


Procedure TTargetReference.Settarget(AIndex : Integer; AValue : string); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetVpnGateway
  --------------------------------------------------------------------}


Procedure TTargetVpnGateway.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.SetforwardingRules(AIndex : Integer; AValue : TTargetVpnGatewayforwardingRules); 

begin
  If (FforwardingRules=AValue) then exit;
  FforwardingRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGateway.Settunnels(AIndex : Integer; AValue : TTargetVpnGatewaytunnels); 

begin
  If (Ftunnels=AValue) then exit;
  Ftunnels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetVpnGatewayforwardingRules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetVpnGatewaytunnels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetVpnGatewayAggregatedList
  --------------------------------------------------------------------}


Procedure TTargetVpnGatewayAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewayAggregatedList.Setitems(AIndex : Integer; AValue : TTargetVpnGatewayAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewayAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewayAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewayAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetVpnGatewayAggregatedListitems
  --------------------------------------------------------------------}


Class Function TTargetVpnGatewayAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTargetVpnGatewayList
  --------------------------------------------------------------------}


Procedure TTargetVpnGatewayList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewayList.Setitems(AIndex : Integer; AValue : TTargetVpnGatewayListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewayList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewayList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewayList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetVpnGatewayListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetVpnGatewaysScopedList
  --------------------------------------------------------------------}


Procedure TTargetVpnGatewaysScopedList.SettargetVpnGateways(AIndex : Integer; AValue : TTargetVpnGatewaysScopedListtargetVpnGateways); 

begin
  If (FtargetVpnGateways=AValue) then exit;
  FtargetVpnGateways:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewaysScopedList.Setwarning(AIndex : Integer; AValue : TTargetVpnGatewaysScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetVpnGatewaysScopedListtargetVpnGateways
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTargetVpnGatewaysScopedListwarning
  --------------------------------------------------------------------}


Procedure TTargetVpnGatewaysScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewaysScopedListwarning.Setdata(AIndex : Integer; AValue : TTargetVpnGatewaysScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewaysScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetVpnGatewaysScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TTargetVpnGatewaysScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetVpnGatewaysScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestFailure
  --------------------------------------------------------------------}


Procedure TTestFailure.SetactualService(AIndex : Integer; AValue : string); 

begin
  If (FactualService=AValue) then exit;
  FactualService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestFailure.SetexpectedService(AIndex : Integer; AValue : string); 

begin
  If (FexpectedService=AValue) then exit;
  FexpectedService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestFailure.Sethost(AIndex : Integer; AValue : string); 

begin
  If (Fhost=AValue) then exit;
  Fhost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestFailure.Setpath(AIndex : Integer; AValue : string); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlMap
  --------------------------------------------------------------------}


Procedure TUrlMap.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SetdefaultService(AIndex : Integer; AValue : string); 

begin
  If (FdefaultService=AValue) then exit;
  FdefaultService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SethostRules(AIndex : Integer; AValue : TUrlMaphostRules); 

begin
  If (FhostRules=AValue) then exit;
  FhostRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SetpathMatchers(AIndex : Integer; AValue : TUrlMappathMatchers); 

begin
  If (FpathMatchers=AValue) then exit;
  FpathMatchers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.Settests(AIndex : Integer; AValue : TUrlMaptests); 

begin
  If (Ftests=AValue) then exit;
  Ftests:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlMaphostRules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUrlMappathMatchers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUrlMaptests
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUrlMapList
  --------------------------------------------------------------------}


Procedure TUrlMapList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapList.Setitems(AIndex : Integer; AValue : TUrlMapListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlMapListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUrlMapReference
  --------------------------------------------------------------------}


Procedure TUrlMapReference.SeturlMap(AIndex : Integer; AValue : string); 

begin
  If (FurlMap=AValue) then exit;
  FurlMap:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlMapTest
  --------------------------------------------------------------------}


Procedure TUrlMapTest.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapTest.Sethost(AIndex : Integer; AValue : string); 

begin
  If (Fhost=AValue) then exit;
  Fhost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapTest.Setpath(AIndex : Integer; AValue : string); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapTest.Setservice(AIndex : Integer; AValue : string); 

begin
  If (Fservice=AValue) then exit;
  Fservice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlMapValidationResult
  --------------------------------------------------------------------}


Procedure TUrlMapValidationResult.SetloadErrors(AIndex : Integer; AValue : TUrlMapValidationResultloadErrors); 

begin
  If (FloadErrors=AValue) then exit;
  FloadErrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapValidationResult.SetloadSucceeded(AIndex : Integer; AValue : boolean); 

begin
  If (FloadSucceeded=AValue) then exit;
  FloadSucceeded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapValidationResult.SettestFailures(AIndex : Integer; AValue : TUrlMapValidationResulttestFailures); 

begin
  If (FtestFailures=AValue) then exit;
  FtestFailures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMapValidationResult.SettestPassed(AIndex : Integer; AValue : boolean); 

begin
  If (FtestPassed=AValue) then exit;
  FtestPassed:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlMapValidationResultloadErrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUrlMapValidationResulttestFailures
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUrlMapsValidateRequest
  --------------------------------------------------------------------}


Procedure TUrlMapsValidateRequest.Setresource(AIndex : Integer; AValue : TUrlMap); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlMapsValidateResponse
  --------------------------------------------------------------------}


Procedure TUrlMapsValidateResponse.Setresult(AIndex : Integer; AValue : TUrlMapValidationResult); 

begin
  If (Fresult=AValue) then exit;
  Fresult:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsageExportLocation
  --------------------------------------------------------------------}


Procedure TUsageExportLocation.SetbucketName(AIndex : Integer; AValue : string); 

begin
  If (FbucketName=AValue) then exit;
  FbucketName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageExportLocation.SetreportNamePrefix(AIndex : Integer; AValue : string); 

begin
  If (FreportNamePrefix=AValue) then exit;
  FreportNamePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVpnTunnel
  --------------------------------------------------------------------}


Procedure TVpnTunnel.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.SetdetailedStatus(AIndex : Integer; AValue : string); 

begin
  If (FdetailedStatus=AValue) then exit;
  FdetailedStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.SetikeNetworks(AIndex : Integer; AValue : TVpnTunnelikeNetworks); 

begin
  If (FikeNetworks=AValue) then exit;
  FikeNetworks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.SetikeVersion(AIndex : Integer; AValue : integer); 

begin
  If (FikeVersion=AValue) then exit;
  FikeVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.SetpeerIp(AIndex : Integer; AValue : string); 

begin
  If (FpeerIp=AValue) then exit;
  FpeerIp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.SetsharedSecret(AIndex : Integer; AValue : string); 

begin
  If (FsharedSecret=AValue) then exit;
  FsharedSecret:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.SetsharedSecretHash(AIndex : Integer; AValue : string); 

begin
  If (FsharedSecretHash=AValue) then exit;
  FsharedSecretHash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnel.SettargetVpnGateway(AIndex : Integer; AValue : string); 

begin
  If (FtargetVpnGateway=AValue) then exit;
  FtargetVpnGateway:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVpnTunnelikeNetworks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVpnTunnelAggregatedList
  --------------------------------------------------------------------}


Procedure TVpnTunnelAggregatedList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelAggregatedList.Setitems(AIndex : Integer; AValue : TVpnTunnelAggregatedListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelAggregatedList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelAggregatedList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelAggregatedList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVpnTunnelAggregatedListitems
  --------------------------------------------------------------------}


Class Function TVpnTunnelAggregatedListitems.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TVpnTunnelList
  --------------------------------------------------------------------}


Procedure TVpnTunnelList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelList.Setitems(AIndex : Integer; AValue : TVpnTunnelListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVpnTunnelListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVpnTunnelsScopedList
  --------------------------------------------------------------------}


Procedure TVpnTunnelsScopedList.SetvpnTunnels(AIndex : Integer; AValue : TVpnTunnelsScopedListvpnTunnels); 

begin
  If (FvpnTunnels=AValue) then exit;
  FvpnTunnels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelsScopedList.Setwarning(AIndex : Integer; AValue : TVpnTunnelsScopedListwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVpnTunnelsScopedListvpnTunnels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVpnTunnelsScopedListwarning
  --------------------------------------------------------------------}


Procedure TVpnTunnelsScopedListwarning.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelsScopedListwarning.Setdata(AIndex : Integer; AValue : TVpnTunnelsScopedListwarningdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelsScopedListwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVpnTunnelsScopedListwarningdata
  --------------------------------------------------------------------}


Procedure TVpnTunnelsScopedListwarningdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVpnTunnelsScopedListwarningdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZone
  --------------------------------------------------------------------}


Procedure TZone.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.SetmaintenanceWindows(AIndex : Integer; AValue : TZonemaintenanceWindows); 

begin
  If (FmaintenanceWindows=AValue) then exit;
  FmaintenanceWindows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZonemaintenanceWindows
  --------------------------------------------------------------------}


Procedure TZonemaintenanceWindows.SetbeginTime(AIndex : Integer; AValue : string); 

begin
  If (FbeginTime=AValue) then exit;
  FbeginTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZonemaintenanceWindows.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZonemaintenanceWindows.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZonemaintenanceWindows.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneList
  --------------------------------------------------------------------}


Procedure TZoneList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.Setitems(AIndex : Integer; AValue : TZoneListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAddressesResource
  --------------------------------------------------------------------}


Class Function TAddressesResource.ResourceName : String;

begin
  Result:='addresses';
end;

Class Function TAddressesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TAddressesResource.AggregatedList(project: string; AQuery : string = '') : TAddressAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/addresses';
  _Methodid   = 'compute.addresses.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAddressAggregatedList) as TAddressAggregatedList;
end;


Function TAddressesResource.AggregatedList(project: string; AQuery : TAddressesaggregatedListOptions) : TAddressAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TAddressesResource.Delete(address: string; project: string; region: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/regions/{region}/addresses/{address}';
  _Methodid   = 'compute.addresses.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['address',address,'project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TAddressesResource.Get(address: string; project: string; region: string) : TAddress;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/addresses/{address}';
  _Methodid   = 'compute.addresses.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['address',address,'project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAddress) as TAddress;
end;

Function TAddressesResource.Insert(project: string; region: string; aAddress : TAddress) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/addresses';
  _Methodid   = 'compute.addresses.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAddress,TOperation) as TOperation;
end;

Function TAddressesResource.List(project: string; region: string; AQuery : string = '') : TAddressList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/addresses';
  _Methodid   = 'compute.addresses.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAddressList) as TAddressList;
end;


Function TAddressesResource.List(project: string; region: string; AQuery : TAddresseslistOptions) : TAddressList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,region,_Q);
end;



{ --------------------------------------------------------------------
  TBackendServicesResource
  --------------------------------------------------------------------}


Class Function TBackendServicesResource.ResourceName : String;

begin
  Result:='backendServices';
end;

Class Function TBackendServicesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TBackendServicesResource.Delete(backendService: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/backendServices/{backendService}';
  _Methodid   = 'compute.backendServices.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['backendService',backendService,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TBackendServicesResource.Get(backendService: string; project: string) : TBackendService;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/backendServices/{backendService}';
  _Methodid   = 'compute.backendServices.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['backendService',backendService,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBackendService) as TBackendService;
end;

Function TBackendServicesResource.GetHealth(backendService: string; project: string; aResourceGroupReference : TResourceGroupReference) : TBackendServiceGroupHealth;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/backendServices/{backendService}/getHealth';
  _Methodid   = 'compute.backendServices.getHealth';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['backendService',backendService,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aResourceGroupReference,TBackendServiceGroupHealth) as TBackendServiceGroupHealth;
end;

Function TBackendServicesResource.Insert(project: string; aBackendService : TBackendService) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/backendServices';
  _Methodid   = 'compute.backendServices.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBackendService,TOperation) as TOperation;
end;

Function TBackendServicesResource.List(project: string; AQuery : string = '') : TBackendServiceList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/backendServices';
  _Methodid   = 'compute.backendServices.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBackendServiceList) as TBackendServiceList;
end;


Function TBackendServicesResource.List(project: string; AQuery : TBackendServiceslistOptions) : TBackendServiceList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TBackendServicesResource.Patch(backendService: string; project: string; aBackendService : TBackendService) : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{project}/global/backendServices/{backendService}';
  _Methodid   = 'compute.backendServices.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['backendService',backendService,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBackendService,TOperation) as TOperation;
end;

Function TBackendServicesResource.Update(backendService: string; project: string; aBackendService : TBackendService) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{project}/global/backendServices/{backendService}';
  _Methodid   = 'compute.backendServices.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['backendService',backendService,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBackendService,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TDiskTypesResource
  --------------------------------------------------------------------}


Class Function TDiskTypesResource.ResourceName : String;

begin
  Result:='diskTypes';
end;

Class Function TDiskTypesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TDiskTypesResource.AggregatedList(project: string; AQuery : string = '') : TDiskTypeAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/diskTypes';
  _Methodid   = 'compute.diskTypes.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDiskTypeAggregatedList) as TDiskTypeAggregatedList;
end;


Function TDiskTypesResource.AggregatedList(project: string; AQuery : TDiskTypesaggregatedListOptions) : TDiskTypeAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TDiskTypesResource.Get(diskType: string; project: string; zone: string) : TDiskType;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/diskTypes/{diskType}';
  _Methodid   = 'compute.diskTypes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['diskType',diskType,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDiskType) as TDiskType;
end;

Function TDiskTypesResource.List(project: string; zone: string; AQuery : string = '') : TDiskTypeList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/diskTypes';
  _Methodid   = 'compute.diskTypes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDiskTypeList) as TDiskTypeList;
end;


Function TDiskTypesResource.List(project: string; zone: string; AQuery : TDiskTypeslistOptions) : TDiskTypeList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;



{ --------------------------------------------------------------------
  TDisksResource
  --------------------------------------------------------------------}


Class Function TDisksResource.ResourceName : String;

begin
  Result:='disks';
end;

Class Function TDisksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TDisksResource.AggregatedList(project: string; AQuery : string = '') : TDiskAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/disks';
  _Methodid   = 'compute.disks.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDiskAggregatedList) as TDiskAggregatedList;
end;


Function TDisksResource.AggregatedList(project: string; AQuery : TDisksaggregatedListOptions) : TDiskAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TDisksResource.CreateSnapshot(disk: string; project: string; zone: string; aSnapshot : TSnapshot) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/disks/{disk}/createSnapshot';
  _Methodid   = 'compute.disks.createSnapshot';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['disk',disk,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSnapshot,TOperation) as TOperation;
end;

Function TDisksResource.Delete(disk: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/zones/{zone}/disks/{disk}';
  _Methodid   = 'compute.disks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['disk',disk,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TDisksResource.Get(disk: string; project: string; zone: string) : TDisk;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/disks/{disk}';
  _Methodid   = 'compute.disks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['disk',disk,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDisk) as TDisk;
end;

Function TDisksResource.Insert(project: string; zone: string; aDisk : TDisk; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/disks';
  _Methodid   = 'compute.disks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aDisk,TOperation) as TOperation;
end;


Function TDisksResource.Insert(project: string; zone: string; aDisk : TDisk; AQuery : TDisksinsertOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'sourceImage',AQuery.sourceImage);
  Result:=Insert(project,zone,aDisk,_Q);
end;

Function TDisksResource.List(project: string; zone: string; AQuery : string = '') : TDiskList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/disks';
  _Methodid   = 'compute.disks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDiskList) as TDiskList;
end;


Function TDisksResource.List(project: string; zone: string; AQuery : TDiskslistOptions) : TDiskList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;



{ --------------------------------------------------------------------
  TFirewallsResource
  --------------------------------------------------------------------}


Class Function TFirewallsResource.ResourceName : String;

begin
  Result:='firewalls';
end;

Class Function TFirewallsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TFirewallsResource.Delete(firewall: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/firewalls/{firewall}';
  _Methodid   = 'compute.firewalls.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['firewall',firewall,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TFirewallsResource.Get(firewall: string; project: string) : TFirewall;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/firewalls/{firewall}';
  _Methodid   = 'compute.firewalls.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['firewall',firewall,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFirewall) as TFirewall;
end;

Function TFirewallsResource.Insert(project: string; aFirewall : TFirewall) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/firewalls';
  _Methodid   = 'compute.firewalls.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFirewall,TOperation) as TOperation;
end;

Function TFirewallsResource.List(project: string; AQuery : string = '') : TFirewallList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/firewalls';
  _Methodid   = 'compute.firewalls.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFirewallList) as TFirewallList;
end;


Function TFirewallsResource.List(project: string; AQuery : TFirewallslistOptions) : TFirewallList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TFirewallsResource.Patch(firewall: string; project: string; aFirewall : TFirewall) : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{project}/global/firewalls/{firewall}';
  _Methodid   = 'compute.firewalls.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['firewall',firewall,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFirewall,TOperation) as TOperation;
end;

Function TFirewallsResource.Update(firewall: string; project: string; aFirewall : TFirewall) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{project}/global/firewalls/{firewall}';
  _Methodid   = 'compute.firewalls.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['firewall',firewall,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFirewall,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TForwardingRulesResource
  --------------------------------------------------------------------}


Class Function TForwardingRulesResource.ResourceName : String;

begin
  Result:='forwardingRules';
end;

Class Function TForwardingRulesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TForwardingRulesResource.AggregatedList(project: string; AQuery : string = '') : TForwardingRuleAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/forwardingRules';
  _Methodid   = 'compute.forwardingRules.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TForwardingRuleAggregatedList) as TForwardingRuleAggregatedList;
end;


Function TForwardingRulesResource.AggregatedList(project: string; AQuery : TForwardingRulesaggregatedListOptions) : TForwardingRuleAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TForwardingRulesResource.Delete(forwardingRule: string; project: string; region: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/regions/{region}/forwardingRules/{forwardingRule}';
  _Methodid   = 'compute.forwardingRules.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['forwardingRule',forwardingRule,'project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TForwardingRulesResource.Get(forwardingRule: string; project: string; region: string) : TForwardingRule;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/forwardingRules/{forwardingRule}';
  _Methodid   = 'compute.forwardingRules.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['forwardingRule',forwardingRule,'project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TForwardingRule) as TForwardingRule;
end;

Function TForwardingRulesResource.Insert(project: string; region: string; aForwardingRule : TForwardingRule) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/forwardingRules';
  _Methodid   = 'compute.forwardingRules.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aForwardingRule,TOperation) as TOperation;
end;

Function TForwardingRulesResource.List(project: string; region: string; AQuery : string = '') : TForwardingRuleList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/forwardingRules';
  _Methodid   = 'compute.forwardingRules.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TForwardingRuleList) as TForwardingRuleList;
end;


Function TForwardingRulesResource.List(project: string; region: string; AQuery : TForwardingRuleslistOptions) : TForwardingRuleList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,region,_Q);
end;

Function TForwardingRulesResource.SetTarget(forwardingRule: string; project: string; region: string; aTargetReference : TTargetReference) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/forwardingRules/{forwardingRule}/setTarget';
  _Methodid   = 'compute.forwardingRules.setTarget';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['forwardingRule',forwardingRule,'project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetReference,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TGlobalAddressesResource
  --------------------------------------------------------------------}


Class Function TGlobalAddressesResource.ResourceName : String;

begin
  Result:='globalAddresses';
end;

Class Function TGlobalAddressesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TGlobalAddressesResource.Delete(address: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/addresses/{address}';
  _Methodid   = 'compute.globalAddresses.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['address',address,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TGlobalAddressesResource.Get(address: string; project: string) : TAddress;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/addresses/{address}';
  _Methodid   = 'compute.globalAddresses.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['address',address,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAddress) as TAddress;
end;

Function TGlobalAddressesResource.Insert(project: string; aAddress : TAddress) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/addresses';
  _Methodid   = 'compute.globalAddresses.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAddress,TOperation) as TOperation;
end;

Function TGlobalAddressesResource.List(project: string; AQuery : string = '') : TAddressList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/addresses';
  _Methodid   = 'compute.globalAddresses.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAddressList) as TAddressList;
end;


Function TGlobalAddressesResource.List(project: string; AQuery : TGlobalAddresseslistOptions) : TAddressList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TGlobalForwardingRulesResource
  --------------------------------------------------------------------}


Class Function TGlobalForwardingRulesResource.ResourceName : String;

begin
  Result:='globalForwardingRules';
end;

Class Function TGlobalForwardingRulesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TGlobalForwardingRulesResource.Delete(forwardingRule: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/forwardingRules/{forwardingRule}';
  _Methodid   = 'compute.globalForwardingRules.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['forwardingRule',forwardingRule,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TGlobalForwardingRulesResource.Get(forwardingRule: string; project: string) : TForwardingRule;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/forwardingRules/{forwardingRule}';
  _Methodid   = 'compute.globalForwardingRules.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['forwardingRule',forwardingRule,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TForwardingRule) as TForwardingRule;
end;

Function TGlobalForwardingRulesResource.Insert(project: string; aForwardingRule : TForwardingRule) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/forwardingRules';
  _Methodid   = 'compute.globalForwardingRules.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aForwardingRule,TOperation) as TOperation;
end;

Function TGlobalForwardingRulesResource.List(project: string; AQuery : string = '') : TForwardingRuleList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/forwardingRules';
  _Methodid   = 'compute.globalForwardingRules.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TForwardingRuleList) as TForwardingRuleList;
end;


Function TGlobalForwardingRulesResource.List(project: string; AQuery : TGlobalForwardingRuleslistOptions) : TForwardingRuleList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TGlobalForwardingRulesResource.SetTarget(forwardingRule: string; project: string; aTargetReference : TTargetReference) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/forwardingRules/{forwardingRule}/setTarget';
  _Methodid   = 'compute.globalForwardingRules.setTarget';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['forwardingRule',forwardingRule,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetReference,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TGlobalOperationsResource
  --------------------------------------------------------------------}


Class Function TGlobalOperationsResource.ResourceName : String;

begin
  Result:='globalOperations';
end;

Class Function TGlobalOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TGlobalOperationsResource.AggregatedList(project: string; AQuery : string = '') : TOperationAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/operations';
  _Methodid   = 'compute.globalOperations.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationAggregatedList) as TOperationAggregatedList;
end;


Function TGlobalOperationsResource.AggregatedList(project: string; AQuery : TGlobalOperationsaggregatedListOptions) : TOperationAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Procedure TGlobalOperationsResource.Delete(operation: string; project: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/operations/{operation}';
  _Methodid   = 'compute.globalOperations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TGlobalOperationsResource.Get(operation: string; project: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/operations/{operation}';
  _Methodid   = 'compute.globalOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TGlobalOperationsResource.List(project: string; AQuery : string = '') : TOperationList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/operations';
  _Methodid   = 'compute.globalOperations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationList) as TOperationList;
end;


Function TGlobalOperationsResource.List(project: string; AQuery : TGlobalOperationslistOptions) : TOperationList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  THttpHealthChecksResource
  --------------------------------------------------------------------}


Class Function THttpHealthChecksResource.ResourceName : String;

begin
  Result:='httpHealthChecks';
end;

Class Function THttpHealthChecksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function THttpHealthChecksResource.Delete(httpHealthCheck: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/httpHealthChecks/{httpHealthCheck}';
  _Methodid   = 'compute.httpHealthChecks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['httpHealthCheck',httpHealthCheck,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function THttpHealthChecksResource.Get(httpHealthCheck: string; project: string) : THttpHealthCheck;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/httpHealthChecks/{httpHealthCheck}';
  _Methodid   = 'compute.httpHealthChecks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['httpHealthCheck',httpHealthCheck,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,THttpHealthCheck) as THttpHealthCheck;
end;

Function THttpHealthChecksResource.Insert(project: string; aHttpHealthCheck : THttpHealthCheck) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/httpHealthChecks';
  _Methodid   = 'compute.httpHealthChecks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aHttpHealthCheck,TOperation) as TOperation;
end;

Function THttpHealthChecksResource.List(project: string; AQuery : string = '') : THttpHealthCheckList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/httpHealthChecks';
  _Methodid   = 'compute.httpHealthChecks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,THttpHealthCheckList) as THttpHealthCheckList;
end;


Function THttpHealthChecksResource.List(project: string; AQuery : THttpHealthCheckslistOptions) : THttpHealthCheckList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function THttpHealthChecksResource.Patch(httpHealthCheck: string; project: string; aHttpHealthCheck : THttpHealthCheck) : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{project}/global/httpHealthChecks/{httpHealthCheck}';
  _Methodid   = 'compute.httpHealthChecks.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['httpHealthCheck',httpHealthCheck,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aHttpHealthCheck,TOperation) as TOperation;
end;

Function THttpHealthChecksResource.Update(httpHealthCheck: string; project: string; aHttpHealthCheck : THttpHealthCheck) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{project}/global/httpHealthChecks/{httpHealthCheck}';
  _Methodid   = 'compute.httpHealthChecks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['httpHealthCheck',httpHealthCheck,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aHttpHealthCheck,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TImagesResource
  --------------------------------------------------------------------}


Class Function TImagesResource.ResourceName : String;

begin
  Result:='images';
end;

Class Function TImagesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TImagesResource.Delete(image: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/images/{image}';
  _Methodid   = 'compute.images.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['image',image,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TImagesResource.Deprecate(image: string; project: string; aDeprecationStatus : TDeprecationStatus) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/images/{image}/deprecate';
  _Methodid   = 'compute.images.deprecate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['image',image,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDeprecationStatus,TOperation) as TOperation;
end;

Function TImagesResource.Get(image: string; project: string) : TImage;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/images/{image}';
  _Methodid   = 'compute.images.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['image',image,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TImage) as TImage;
end;

Function TImagesResource.Insert(project: string; aImage : TImage) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/images';
  _Methodid   = 'compute.images.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aImage,TOperation) as TOperation;
end;

Function TImagesResource.List(project: string; AQuery : string = '') : TImageList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/images';
  _Methodid   = 'compute.images.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TImageList) as TImageList;
end;


Function TImagesResource.List(project: string; AQuery : TImageslistOptions) : TImageList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TInstanceTemplatesResource
  --------------------------------------------------------------------}


Class Function TInstanceTemplatesResource.ResourceName : String;

begin
  Result:='instanceTemplates';
end;

Class Function TInstanceTemplatesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TInstanceTemplatesResource.Delete(instanceTemplate: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/instanceTemplates/{instanceTemplate}';
  _Methodid   = 'compute.instanceTemplates.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceTemplate',instanceTemplate,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstanceTemplatesResource.Get(instanceTemplate: string; project: string) : TInstanceTemplate;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/instanceTemplates/{instanceTemplate}';
  _Methodid   = 'compute.instanceTemplates.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceTemplate',instanceTemplate,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInstanceTemplate) as TInstanceTemplate;
end;

Function TInstanceTemplatesResource.Insert(project: string; aInstanceTemplate : TInstanceTemplate) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/instanceTemplates';
  _Methodid   = 'compute.instanceTemplates.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceTemplate,TOperation) as TOperation;
end;

Function TInstanceTemplatesResource.List(project: string; AQuery : string = '') : TInstanceTemplateList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/instanceTemplates';
  _Methodid   = 'compute.instanceTemplates.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInstanceTemplateList) as TInstanceTemplateList;
end;


Function TInstanceTemplatesResource.List(project: string; AQuery : TInstanceTemplateslistOptions) : TInstanceTemplateList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TInstancesResource
  --------------------------------------------------------------------}


Class Function TInstancesResource.ResourceName : String;

begin
  Result:='instances';
end;

Class Function TInstancesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TInstancesResource.AddAccessConfig(instance: string; project: string; zone: string; aAccessConfig : TAccessConfig; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/addAccessConfig';
  _Methodid   = 'compute.instances.addAccessConfig';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAccessConfig,TOperation) as TOperation;
end;


Function TInstancesResource.AddAccessConfig(instance: string; project: string; zone: string; aAccessConfig : TAccessConfig; AQuery : TInstancesaddAccessConfigOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'networkInterface',AQuery.networkInterface);
  Result:=AddAccessConfig(instance,project,zone,aAccessConfig,_Q);
end;

Function TInstancesResource.AggregatedList(project: string; AQuery : string = '') : TInstanceAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/instances';
  _Methodid   = 'compute.instances.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInstanceAggregatedList) as TInstanceAggregatedList;
end;


Function TInstancesResource.AggregatedList(project: string; AQuery : TInstancesaggregatedListOptions) : TInstanceAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TInstancesResource.AttachDisk(instance: string; project: string; zone: string; aAttachedDisk : TAttachedDisk) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/attachDisk';
  _Methodid   = 'compute.instances.attachDisk';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAttachedDisk,TOperation) as TOperation;
end;

Function TInstancesResource.Delete(instance: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/zones/{zone}/instances/{instance}';
  _Methodid   = 'compute.instances.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.DeleteAccessConfig(instance: string; project: string; zone: string; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/deleteAccessConfig';
  _Methodid   = 'compute.instances.deleteAccessConfig';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperation) as TOperation;
end;


Function TInstancesResource.DeleteAccessConfig(instance: string; project: string; zone: string; AQuery : TInstancesdeleteAccessConfigOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'accessConfig',AQuery.accessConfig);
  AddToQuery(_Q,'networkInterface',AQuery.networkInterface);
  Result:=DeleteAccessConfig(instance,project,zone,_Q);
end;

Function TInstancesResource.DetachDisk(instance: string; project: string; zone: string; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/detachDisk';
  _Methodid   = 'compute.instances.detachDisk';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperation) as TOperation;
end;


Function TInstancesResource.DetachDisk(instance: string; project: string; zone: string; AQuery : TInstancesdetachDiskOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'deviceName',AQuery.deviceName);
  Result:=DetachDisk(instance,project,zone,_Q);
end;

Function TInstancesResource.Get(instance: string; project: string; zone: string) : TInstance;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/instances/{instance}';
  _Methodid   = 'compute.instances.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInstance) as TInstance;
end;

Function TInstancesResource.GetSerialPortOutput(instance: string; project: string; zone: string; AQuery : string = '') : TSerialPortOutput;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/instances/{instance}/serialPort';
  _Methodid   = 'compute.instances.getSerialPortOutput';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSerialPortOutput) as TSerialPortOutput;
end;


Function TInstancesResource.GetSerialPortOutput(instance: string; project: string; zone: string; AQuery : TInstancesgetSerialPortOutputOptions) : TSerialPortOutput;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'port',AQuery.port);
  Result:=GetSerialPortOutput(instance,project,zone,_Q);
end;

Function TInstancesResource.Insert(project: string; zone: string; aInstance : TInstance) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances';
  _Methodid   = 'compute.instances.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstance,TOperation) as TOperation;
end;

Function TInstancesResource.List(project: string; zone: string; AQuery : string = '') : TInstanceList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/instances';
  _Methodid   = 'compute.instances.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInstanceList) as TInstanceList;
end;


Function TInstancesResource.List(project: string; zone: string; AQuery : TInstanceslistOptions) : TInstanceList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;

Function TInstancesResource.Reset(instance: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/reset';
  _Methodid   = 'compute.instances.reset';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.SetDiskAutoDelete(instance: string; project: string; zone: string; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/setDiskAutoDelete';
  _Methodid   = 'compute.instances.setDiskAutoDelete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperation) as TOperation;
end;


Function TInstancesResource.SetDiskAutoDelete(instance: string; project: string; zone: string; AQuery : TInstancessetDiskAutoDeleteOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'autoDelete',AQuery.autoDelete);
  AddToQuery(_Q,'deviceName',AQuery.deviceName);
  Result:=SetDiskAutoDelete(instance,project,zone,_Q);
end;

Function TInstancesResource.SetMetadata(instance: string; project: string; zone: string; aMetadata : TMetadata) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/setMetadata';
  _Methodid   = 'compute.instances.setMetadata';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aMetadata,TOperation) as TOperation;
end;

Function TInstancesResource.SetScheduling(instance: string; project: string; zone: string; aScheduling : TScheduling) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/setScheduling';
  _Methodid   = 'compute.instances.setScheduling';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aScheduling,TOperation) as TOperation;
end;

Function TInstancesResource.SetTags(instance: string; project: string; zone: string; aTags : TTags) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/setTags';
  _Methodid   = 'compute.instances.setTags';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTags,TOperation) as TOperation;
end;

Function TInstancesResource.Start(instance: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/start';
  _Methodid   = 'compute.instances.start';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.Stop(instance: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instances/{instance}/stop';
  _Methodid   = 'compute.instances.stop';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TLicensesResource
  --------------------------------------------------------------------}


Class Function TLicensesResource.ResourceName : String;

begin
  Result:='licenses';
end;

Class Function TLicensesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TLicensesResource.Get(license: string; project: string) : TLicense;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/licenses/{license}';
  _Methodid   = 'compute.licenses.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['license',license,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLicense) as TLicense;
end;



{ --------------------------------------------------------------------
  TMachineTypesResource
  --------------------------------------------------------------------}


Class Function TMachineTypesResource.ResourceName : String;

begin
  Result:='machineTypes';
end;

Class Function TMachineTypesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TMachineTypesResource.AggregatedList(project: string; AQuery : string = '') : TMachineTypeAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/machineTypes';
  _Methodid   = 'compute.machineTypes.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TMachineTypeAggregatedList) as TMachineTypeAggregatedList;
end;


Function TMachineTypesResource.AggregatedList(project: string; AQuery : TMachineTypesaggregatedListOptions) : TMachineTypeAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TMachineTypesResource.Get(machineType: string; project: string; zone: string) : TMachineType;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/machineTypes/{machineType}';
  _Methodid   = 'compute.machineTypes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['machineType',machineType,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMachineType) as TMachineType;
end;

Function TMachineTypesResource.List(project: string; zone: string; AQuery : string = '') : TMachineTypeList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/machineTypes';
  _Methodid   = 'compute.machineTypes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TMachineTypeList) as TMachineTypeList;
end;


Function TMachineTypesResource.List(project: string; zone: string; AQuery : TMachineTypeslistOptions) : TMachineTypeList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;



{ --------------------------------------------------------------------
  TNetworksResource
  --------------------------------------------------------------------}


Class Function TNetworksResource.ResourceName : String;

begin
  Result:='networks';
end;

Class Function TNetworksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TNetworksResource.Delete(network: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/networks/{network}';
  _Methodid   = 'compute.networks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['network',network,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TNetworksResource.Get(network: string; project: string) : TNetwork;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/networks/{network}';
  _Methodid   = 'compute.networks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['network',network,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TNetwork) as TNetwork;
end;

Function TNetworksResource.Insert(project: string; aNetwork : TNetwork) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/networks';
  _Methodid   = 'compute.networks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aNetwork,TOperation) as TOperation;
end;

Function TNetworksResource.List(project: string; AQuery : string = '') : TNetworkList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/networks';
  _Methodid   = 'compute.networks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TNetworkList) as TNetworkList;
end;


Function TNetworksResource.List(project: string; AQuery : TNetworkslistOptions) : TNetworkList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
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
  Result:=TcomputeAPI;
end;

Function TProjectsResource.Get(project: string) : TProject;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}';
  _Methodid   = 'compute.projects.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProject) as TProject;
end;

Function TProjectsResource.MoveDisk(project: string; aDiskMoveRequest : TDiskMoveRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/moveDisk';
  _Methodid   = 'compute.projects.moveDisk';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDiskMoveRequest,TOperation) as TOperation;
end;

Function TProjectsResource.MoveInstance(project: string; aInstanceMoveRequest : TInstanceMoveRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/moveInstance';
  _Methodid   = 'compute.projects.moveInstance';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceMoveRequest,TOperation) as TOperation;
end;

Function TProjectsResource.SetCommonInstanceMetadata(project: string; aMetadata : TMetadata) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/setCommonInstanceMetadata';
  _Methodid   = 'compute.projects.setCommonInstanceMetadata';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aMetadata,TOperation) as TOperation;
end;

Function TProjectsResource.SetUsageExportBucket(project: string; aUsageExportLocation : TUsageExportLocation) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/setUsageExportBucket';
  _Methodid   = 'compute.projects.setUsageExportBucket';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUsageExportLocation,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TRegionOperationsResource
  --------------------------------------------------------------------}


Class Function TRegionOperationsResource.ResourceName : String;

begin
  Result:='regionOperations';
end;

Class Function TRegionOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Procedure TRegionOperationsResource.Delete(operation: string; project: string; region: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/regions/{region}/operations/{operation}';
  _Methodid   = 'compute.regionOperations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'region',region]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TRegionOperationsResource.Get(operation: string; project: string; region: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/operations/{operation}';
  _Methodid   = 'compute.regionOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TRegionOperationsResource.List(project: string; region: string; AQuery : string = '') : TOperationList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/operations';
  _Methodid   = 'compute.regionOperations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationList) as TOperationList;
end;


Function TRegionOperationsResource.List(project: string; region: string; AQuery : TRegionOperationslistOptions) : TOperationList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,region,_Q);
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
  Result:=TcomputeAPI;
end;

Function TRegionsResource.Get(project: string; region: string) : TRegion;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}';
  _Methodid   = 'compute.regions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRegion) as TRegion;
end;

Function TRegionsResource.List(project: string; AQuery : string = '') : TRegionList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions';
  _Methodid   = 'compute.regions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRegionList) as TRegionList;
end;


Function TRegionsResource.List(project: string; AQuery : TRegionslistOptions) : TRegionList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TRoutesResource
  --------------------------------------------------------------------}


Class Function TRoutesResource.ResourceName : String;

begin
  Result:='routes';
end;

Class Function TRoutesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TRoutesResource.Delete(project: string; route: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/routes/{route}';
  _Methodid   = 'compute.routes.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'route',route]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TRoutesResource.Get(project: string; route: string) : TRoute;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/routes/{route}';
  _Methodid   = 'compute.routes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'route',route]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRoute) as TRoute;
end;

Function TRoutesResource.Insert(project: string; aRoute : TRoute) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/routes';
  _Methodid   = 'compute.routes.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRoute,TOperation) as TOperation;
end;

Function TRoutesResource.List(project: string; AQuery : string = '') : TRouteList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/routes';
  _Methodid   = 'compute.routes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRouteList) as TRouteList;
end;


Function TRoutesResource.List(project: string; AQuery : TRouteslistOptions) : TRouteList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TSnapshotsResource
  --------------------------------------------------------------------}


Class Function TSnapshotsResource.ResourceName : String;

begin
  Result:='snapshots';
end;

Class Function TSnapshotsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TSnapshotsResource.Delete(project: string; snapshot: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/snapshots/{snapshot}';
  _Methodid   = 'compute.snapshots.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'snapshot',snapshot]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TSnapshotsResource.Get(project: string; snapshot: string) : TSnapshot;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/snapshots/{snapshot}';
  _Methodid   = 'compute.snapshots.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'snapshot',snapshot]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSnapshot) as TSnapshot;
end;

Function TSnapshotsResource.List(project: string; AQuery : string = '') : TSnapshotList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/snapshots';
  _Methodid   = 'compute.snapshots.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSnapshotList) as TSnapshotList;
end;


Function TSnapshotsResource.List(project: string; AQuery : TSnapshotslistOptions) : TSnapshotList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TTargetHttpProxiesResource
  --------------------------------------------------------------------}


Class Function TTargetHttpProxiesResource.ResourceName : String;

begin
  Result:='targetHttpProxies';
end;

Class Function TTargetHttpProxiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TTargetHttpProxiesResource.Delete(project: string; targetHttpProxy: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/targetHttpProxies/{targetHttpProxy}';
  _Methodid   = 'compute.targetHttpProxies.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'targetHttpProxy',targetHttpProxy]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TTargetHttpProxiesResource.Get(project: string; targetHttpProxy: string) : TTargetHttpProxy;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/targetHttpProxies/{targetHttpProxy}';
  _Methodid   = 'compute.targetHttpProxies.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'targetHttpProxy',targetHttpProxy]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTargetHttpProxy) as TTargetHttpProxy;
end;

Function TTargetHttpProxiesResource.Insert(project: string; aTargetHttpProxy : TTargetHttpProxy) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/targetHttpProxies';
  _Methodid   = 'compute.targetHttpProxies.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetHttpProxy,TOperation) as TOperation;
end;

Function TTargetHttpProxiesResource.List(project: string; AQuery : string = '') : TTargetHttpProxyList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/targetHttpProxies';
  _Methodid   = 'compute.targetHttpProxies.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTargetHttpProxyList) as TTargetHttpProxyList;
end;


Function TTargetHttpProxiesResource.List(project: string; AQuery : TTargetHttpProxieslistOptions) : TTargetHttpProxyList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TTargetHttpProxiesResource.SetUrlMap(project: string; targetHttpProxy: string; aUrlMapReference : TUrlMapReference) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/targetHttpProxies/{targetHttpProxy}/setUrlMap';
  _Methodid   = 'compute.targetHttpProxies.setUrlMap';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'targetHttpProxy',targetHttpProxy]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUrlMapReference,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TTargetInstancesResource
  --------------------------------------------------------------------}


Class Function TTargetInstancesResource.ResourceName : String;

begin
  Result:='targetInstances';
end;

Class Function TTargetInstancesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TTargetInstancesResource.AggregatedList(project: string; AQuery : string = '') : TTargetInstanceAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/targetInstances';
  _Methodid   = 'compute.targetInstances.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTargetInstanceAggregatedList) as TTargetInstanceAggregatedList;
end;


Function TTargetInstancesResource.AggregatedList(project: string; AQuery : TTargetInstancesaggregatedListOptions) : TTargetInstanceAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TTargetInstancesResource.Delete(project: string; targetInstance: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/zones/{zone}/targetInstances/{targetInstance}';
  _Methodid   = 'compute.targetInstances.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'targetInstance',targetInstance,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TTargetInstancesResource.Get(project: string; targetInstance: string; zone: string) : TTargetInstance;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/targetInstances/{targetInstance}';
  _Methodid   = 'compute.targetInstances.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'targetInstance',targetInstance,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTargetInstance) as TTargetInstance;
end;

Function TTargetInstancesResource.Insert(project: string; zone: string; aTargetInstance : TTargetInstance) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/targetInstances';
  _Methodid   = 'compute.targetInstances.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetInstance,TOperation) as TOperation;
end;

Function TTargetInstancesResource.List(project: string; zone: string; AQuery : string = '') : TTargetInstanceList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/targetInstances';
  _Methodid   = 'compute.targetInstances.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTargetInstanceList) as TTargetInstanceList;
end;


Function TTargetInstancesResource.List(project: string; zone: string; AQuery : TTargetInstanceslistOptions) : TTargetInstanceList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;



{ --------------------------------------------------------------------
  TTargetPoolsResource
  --------------------------------------------------------------------}


Class Function TTargetPoolsResource.ResourceName : String;

begin
  Result:='targetPools';
end;

Class Function TTargetPoolsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TTargetPoolsResource.AddHealthCheck(project: string; region: string; targetPool: string; aTargetPoolsAddHealthCheckRequest : TTargetPoolsAddHealthCheckRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/targetPools/{targetPool}/addHealthCheck';
  _Methodid   = 'compute.targetPools.addHealthCheck';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetPool',targetPool]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetPoolsAddHealthCheckRequest,TOperation) as TOperation;
end;

Function TTargetPoolsResource.AddInstance(project: string; region: string; targetPool: string; aTargetPoolsAddInstanceRequest : TTargetPoolsAddInstanceRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/targetPools/{targetPool}/addInstance';
  _Methodid   = 'compute.targetPools.addInstance';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetPool',targetPool]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetPoolsAddInstanceRequest,TOperation) as TOperation;
end;

Function TTargetPoolsResource.AggregatedList(project: string; AQuery : string = '') : TTargetPoolAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/targetPools';
  _Methodid   = 'compute.targetPools.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTargetPoolAggregatedList) as TTargetPoolAggregatedList;
end;


Function TTargetPoolsResource.AggregatedList(project: string; AQuery : TTargetPoolsaggregatedListOptions) : TTargetPoolAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TTargetPoolsResource.Delete(project: string; region: string; targetPool: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/regions/{region}/targetPools/{targetPool}';
  _Methodid   = 'compute.targetPools.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetPool',targetPool]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TTargetPoolsResource.Get(project: string; region: string; targetPool: string) : TTargetPool;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/targetPools/{targetPool}';
  _Methodid   = 'compute.targetPools.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetPool',targetPool]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTargetPool) as TTargetPool;
end;

Function TTargetPoolsResource.GetHealth(project: string; region: string; targetPool: string; aInstanceReference : TInstanceReference) : TTargetPoolInstanceHealth;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/targetPools/{targetPool}/getHealth';
  _Methodid   = 'compute.targetPools.getHealth';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetPool',targetPool]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceReference,TTargetPoolInstanceHealth) as TTargetPoolInstanceHealth;
end;

Function TTargetPoolsResource.Insert(project: string; region: string; aTargetPool : TTargetPool) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/targetPools';
  _Methodid   = 'compute.targetPools.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetPool,TOperation) as TOperation;
end;

Function TTargetPoolsResource.List(project: string; region: string; AQuery : string = '') : TTargetPoolList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/targetPools';
  _Methodid   = 'compute.targetPools.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTargetPoolList) as TTargetPoolList;
end;


Function TTargetPoolsResource.List(project: string; region: string; AQuery : TTargetPoolslistOptions) : TTargetPoolList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,region,_Q);
end;

Function TTargetPoolsResource.RemoveHealthCheck(project: string; region: string; targetPool: string; aTargetPoolsRemoveHealthCheckRequest : TTargetPoolsRemoveHealthCheckRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/targetPools/{targetPool}/removeHealthCheck';
  _Methodid   = 'compute.targetPools.removeHealthCheck';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetPool',targetPool]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetPoolsRemoveHealthCheckRequest,TOperation) as TOperation;
end;

Function TTargetPoolsResource.RemoveInstance(project: string; region: string; targetPool: string; aTargetPoolsRemoveInstanceRequest : TTargetPoolsRemoveInstanceRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/targetPools/{targetPool}/removeInstance';
  _Methodid   = 'compute.targetPools.removeInstance';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetPool',targetPool]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetPoolsRemoveInstanceRequest,TOperation) as TOperation;
end;

Function TTargetPoolsResource.SetBackup(project: string; region: string; targetPool: string; aTargetReference : TTargetReference; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/targetPools/{targetPool}/setBackup';
  _Methodid   = 'compute.targetPools.setBackup';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetPool',targetPool]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTargetReference,TOperation) as TOperation;
end;


Function TTargetPoolsResource.SetBackup(project: string; region: string; targetPool: string; aTargetReference : TTargetReference; AQuery : TTargetPoolssetBackupOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'failoverRatio',AQuery.failoverRatio);
  Result:=SetBackup(project,region,targetPool,aTargetReference,_Q);
end;



{ --------------------------------------------------------------------
  TTargetVpnGatewaysResource
  --------------------------------------------------------------------}


Class Function TTargetVpnGatewaysResource.ResourceName : String;

begin
  Result:='targetVpnGateways';
end;

Class Function TTargetVpnGatewaysResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TTargetVpnGatewaysResource.AggregatedList(project: string; AQuery : string = '') : TTargetVpnGatewayAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/targetVpnGateways';
  _Methodid   = 'compute.targetVpnGateways.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTargetVpnGatewayAggregatedList) as TTargetVpnGatewayAggregatedList;
end;


Function TTargetVpnGatewaysResource.AggregatedList(project: string; AQuery : TTargetVpnGatewaysaggregatedListOptions) : TTargetVpnGatewayAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TTargetVpnGatewaysResource.Delete(project: string; region: string; targetVpnGateway: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/regions/{region}/targetVpnGateways/{targetVpnGateway}';
  _Methodid   = 'compute.targetVpnGateways.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetVpnGateway',targetVpnGateway]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TTargetVpnGatewaysResource.Get(project: string; region: string; targetVpnGateway: string) : TTargetVpnGateway;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/targetVpnGateways/{targetVpnGateway}';
  _Methodid   = 'compute.targetVpnGateways.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'targetVpnGateway',targetVpnGateway]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTargetVpnGateway) as TTargetVpnGateway;
end;

Function TTargetVpnGatewaysResource.Insert(project: string; region: string; aTargetVpnGateway : TTargetVpnGateway) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/targetVpnGateways';
  _Methodid   = 'compute.targetVpnGateways.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTargetVpnGateway,TOperation) as TOperation;
end;

Function TTargetVpnGatewaysResource.List(project: string; region: string; AQuery : string = '') : TTargetVpnGatewayList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/targetVpnGateways';
  _Methodid   = 'compute.targetVpnGateways.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTargetVpnGatewayList) as TTargetVpnGatewayList;
end;


Function TTargetVpnGatewaysResource.List(project: string; region: string; AQuery : TTargetVpnGatewayslistOptions) : TTargetVpnGatewayList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,region,_Q);
end;



{ --------------------------------------------------------------------
  TUrlMapsResource
  --------------------------------------------------------------------}


Class Function TUrlMapsResource.ResourceName : String;

begin
  Result:='urlMaps';
end;

Class Function TUrlMapsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TUrlMapsResource.Delete(project: string; urlMap: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/urlMaps/{urlMap}';
  _Methodid   = 'compute.urlMaps.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'urlMap',urlMap]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TUrlMapsResource.Get(project: string; urlMap: string) : TUrlMap;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/urlMaps/{urlMap}';
  _Methodid   = 'compute.urlMaps.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'urlMap',urlMap]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUrlMap) as TUrlMap;
end;

Function TUrlMapsResource.Insert(project: string; aUrlMap : TUrlMap) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/urlMaps';
  _Methodid   = 'compute.urlMaps.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUrlMap,TOperation) as TOperation;
end;

Function TUrlMapsResource.List(project: string; AQuery : string = '') : TUrlMapList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/urlMaps';
  _Methodid   = 'compute.urlMaps.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUrlMapList) as TUrlMapList;
end;


Function TUrlMapsResource.List(project: string; AQuery : TUrlMapslistOptions) : TUrlMapList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TUrlMapsResource.Patch(project: string; urlMap: string; aUrlMap : TUrlMap) : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{project}/global/urlMaps/{urlMap}';
  _Methodid   = 'compute.urlMaps.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'urlMap',urlMap]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUrlMap,TOperation) as TOperation;
end;

Function TUrlMapsResource.Update(project: string; urlMap: string; aUrlMap : TUrlMap) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{project}/global/urlMaps/{urlMap}';
  _Methodid   = 'compute.urlMaps.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'urlMap',urlMap]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUrlMap,TOperation) as TOperation;
end;

Function TUrlMapsResource.Validate(project: string; urlMap: string; aUrlMapsValidateRequest : TUrlMapsValidateRequest) : TUrlMapsValidateResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/urlMaps/{urlMap}/validate';
  _Methodid   = 'compute.urlMaps.validate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'urlMap',urlMap]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUrlMapsValidateRequest,TUrlMapsValidateResponse) as TUrlMapsValidateResponse;
end;



{ --------------------------------------------------------------------
  TVpnTunnelsResource
  --------------------------------------------------------------------}


Class Function TVpnTunnelsResource.ResourceName : String;

begin
  Result:='vpnTunnels';
end;

Class Function TVpnTunnelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TVpnTunnelsResource.AggregatedList(project: string; AQuery : string = '') : TVpnTunnelAggregatedList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/aggregated/vpnTunnels';
  _Methodid   = 'compute.vpnTunnels.aggregatedList';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVpnTunnelAggregatedList) as TVpnTunnelAggregatedList;
end;


Function TVpnTunnelsResource.AggregatedList(project: string; AQuery : TVpnTunnelsaggregatedListOptions) : TVpnTunnelAggregatedList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=AggregatedList(project,_Q);
end;

Function TVpnTunnelsResource.Delete(project: string; region: string; vpnTunnel: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/regions/{region}/vpnTunnels/{vpnTunnel}';
  _Methodid   = 'compute.vpnTunnels.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'vpnTunnel',vpnTunnel]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TVpnTunnelsResource.Get(project: string; region: string; vpnTunnel: string) : TVpnTunnel;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/vpnTunnels/{vpnTunnel}';
  _Methodid   = 'compute.vpnTunnels.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region,'vpnTunnel',vpnTunnel]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TVpnTunnel) as TVpnTunnel;
end;

Function TVpnTunnelsResource.Insert(project: string; region: string; aVpnTunnel : TVpnTunnel) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/regions/{region}/vpnTunnels';
  _Methodid   = 'compute.vpnTunnels.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aVpnTunnel,TOperation) as TOperation;
end;

Function TVpnTunnelsResource.List(project: string; region: string; AQuery : string = '') : TVpnTunnelList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/regions/{region}/vpnTunnels';
  _Methodid   = 'compute.vpnTunnels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVpnTunnelList) as TVpnTunnelList;
end;


Function TVpnTunnelsResource.List(project: string; region: string; AQuery : TVpnTunnelslistOptions) : TVpnTunnelList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,region,_Q);
end;



{ --------------------------------------------------------------------
  TZoneOperationsResource
  --------------------------------------------------------------------}


Class Function TZoneOperationsResource.ResourceName : String;

begin
  Result:='zoneOperations';
end;

Class Function TZoneOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Procedure TZoneOperationsResource.Delete(operation: string; project: string; zone: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'compute.zoneOperations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'zone',zone]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TZoneOperationsResource.Get(operation: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'compute.zoneOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TZoneOperationsResource.List(project: string; zone: string; AQuery : string = '') : TOperationList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations';
  _Methodid   = 'compute.zoneOperations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationList) as TOperationList;
end;


Function TZoneOperationsResource.List(project: string; zone: string; AQuery : TZoneOperationslistOptions) : TOperationList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;



{ --------------------------------------------------------------------
  TZonesResource
  --------------------------------------------------------------------}


Class Function TZonesResource.ResourceName : String;

begin
  Result:='zones';
end;

Class Function TZonesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeAPI;
end;

Function TZonesResource.Get(project: string; zone: string) : TZone;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}';
  _Methodid   = 'compute.zones.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TZone) as TZone;
end;

Function TZonesResource.List(project: string; AQuery : string = '') : TZoneList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones';
  _Methodid   = 'compute.zones.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TZoneList) as TZoneList;
end;


Function TZonesResource.List(project: string; AQuery : TZoneslistOptions) : TZoneList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TComputeAPI
  --------------------------------------------------------------------}

Class Function TComputeAPI.APIName : String;

begin
  Result:='compute';
end;

Class Function TComputeAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TComputeAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TComputeAPI.APIID : String;

begin
  Result:='compute:v1';
end;

Class Function TComputeAPI.APITitle : String;

begin
  Result:='Compute Engine API';
end;

Class Function TComputeAPI.APIDescription : String;

begin
  Result:='API for the Google Compute Engine service.';
end;

Class Function TComputeAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TComputeAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TComputeAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/compute_engine-16.png';
end;

Class Function TComputeAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/compute_engine-32.png';
end;

Class Function TComputeAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/compute/docs/reference/latest/';
end;

Class Function TComputeAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TComputeAPI.APIbasePath : string;

begin
  Result:='/compute/v1/projects/';
end;

Class Function TComputeAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/compute/v1/projects/';
end;

Class Function TComputeAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TComputeAPI.APIservicePath : string;

begin
  Result:='compute/v1/projects/';
end;

Class Function TComputeAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TComputeAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,6);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/compute';
  Result[1].Description:='View and manage your Google Compute Engine resources';
  Result[2].Name:='https://www.googleapis.com/auth/compute.readonly';
  Result[2].Description:='View your Google Compute Engine resources';
  Result[3].Name:='https://www.googleapis.com/auth/devstorage.full_control';
  Result[3].Description:='Manage your data and permissions in Google Cloud Storage';
  Result[4].Name:='https://www.googleapis.com/auth/devstorage.read_only';
  Result[4].Description:='View your data in Google Cloud Storage';
  Result[5].Name:='https://www.googleapis.com/auth/devstorage.read_write';
  Result[5].Description:='Manage your data in Google Cloud Storage';
  
end;

Class Function TComputeAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TComputeAPI.RegisterAPIResources;

begin
  TAccessConfig.RegisterObject;
  TAddress.RegisterObject;
  TAddressusers.RegisterObject;
  TAddressAggregatedList.RegisterObject;
  TAddressAggregatedListitems.RegisterObject;
  TAddressList.RegisterObject;
  TAddressListitems.RegisterObject;
  TAddressesScopedList.RegisterObject;
  TAddressesScopedListaddresses.RegisterObject;
  TAddressesScopedListwarning.RegisterObject;
  TAddressesScopedListwarningdata.RegisterObject;
  TAttachedDisk.RegisterObject;
  TAttachedDisklicenses.RegisterObject;
  TAttachedDiskInitializeParams.RegisterObject;
  TBackend.RegisterObject;
  TBackendService.RegisterObject;
  TBackendServicebackends.RegisterObject;
  TBackendServicehealthChecks.RegisterObject;
  TBackendServiceGroupHealth.RegisterObject;
  TBackendServiceGroupHealthhealthStatus.RegisterObject;
  TBackendServiceList.RegisterObject;
  TBackendServiceListitems.RegisterObject;
  TDeprecationStatus.RegisterObject;
  TDisk.RegisterObject;
  TDisklicenses.RegisterObject;
  TDiskAggregatedList.RegisterObject;
  TDiskAggregatedListitems.RegisterObject;
  TDiskList.RegisterObject;
  TDiskListitems.RegisterObject;
  TDiskMoveRequest.RegisterObject;
  TDiskType.RegisterObject;
  TDiskTypeAggregatedList.RegisterObject;
  TDiskTypeAggregatedListitems.RegisterObject;
  TDiskTypeList.RegisterObject;
  TDiskTypeListitems.RegisterObject;
  TDiskTypesScopedList.RegisterObject;
  TDiskTypesScopedListdiskTypes.RegisterObject;
  TDiskTypesScopedListwarning.RegisterObject;
  TDiskTypesScopedListwarningdata.RegisterObject;
  TDisksScopedList.RegisterObject;
  TDisksScopedListdisks.RegisterObject;
  TDisksScopedListwarning.RegisterObject;
  TDisksScopedListwarningdata.RegisterObject;
  TFirewall.RegisterObject;
  TFirewallallowed.RegisterObject;
  TFirewallallowedports.RegisterObject;
  TFirewallsourceRanges.RegisterObject;
  TFirewallsourceTags.RegisterObject;
  TFirewalltargetTags.RegisterObject;
  TFirewallList.RegisterObject;
  TFirewallListitems.RegisterObject;
  TForwardingRule.RegisterObject;
  TForwardingRuleAggregatedList.RegisterObject;
  TForwardingRuleAggregatedListitems.RegisterObject;
  TForwardingRuleList.RegisterObject;
  TForwardingRuleListitems.RegisterObject;
  TForwardingRulesScopedList.RegisterObject;
  TForwardingRulesScopedListforwardingRules.RegisterObject;
  TForwardingRulesScopedListwarning.RegisterObject;
  TForwardingRulesScopedListwarningdata.RegisterObject;
  THealthCheckReference.RegisterObject;
  THealthStatus.RegisterObject;
  THostRule.RegisterObject;
  THostRulehosts.RegisterObject;
  THttpHealthCheck.RegisterObject;
  THttpHealthCheckList.RegisterObject;
  THttpHealthCheckListitems.RegisterObject;
  TImage.RegisterObject;
  TImagelicenses.RegisterObject;
  TImagerawDisk.RegisterObject;
  TImageList.RegisterObject;
  TImageListitems.RegisterObject;
  TInstance.RegisterObject;
  TInstancedisks.RegisterObject;
  TInstancenetworkInterfaces.RegisterObject;
  TInstanceserviceAccounts.RegisterObject;
  TInstanceAggregatedList.RegisterObject;
  TInstanceAggregatedListitems.RegisterObject;
  TInstanceList.RegisterObject;
  TInstanceListitems.RegisterObject;
  TInstanceMoveRequest.RegisterObject;
  TInstanceProperties.RegisterObject;
  TInstancePropertiesdisks.RegisterObject;
  TInstancePropertiesnetworkInterfaces.RegisterObject;
  TInstancePropertiesserviceAccounts.RegisterObject;
  TInstanceReference.RegisterObject;
  TInstanceTemplate.RegisterObject;
  TInstanceTemplateList.RegisterObject;
  TInstanceTemplateListitems.RegisterObject;
  TInstancesScopedList.RegisterObject;
  TInstancesScopedListinstances.RegisterObject;
  TInstancesScopedListwarning.RegisterObject;
  TInstancesScopedListwarningdata.RegisterObject;
  TLicense.RegisterObject;
  TMachineType.RegisterObject;
  TMachineTypescratchDisks.RegisterObject;
  TMachineTypeAggregatedList.RegisterObject;
  TMachineTypeAggregatedListitems.RegisterObject;
  TMachineTypeList.RegisterObject;
  TMachineTypeListitems.RegisterObject;
  TMachineTypesScopedList.RegisterObject;
  TMachineTypesScopedListmachineTypes.RegisterObject;
  TMachineTypesScopedListwarning.RegisterObject;
  TMachineTypesScopedListwarningdata.RegisterObject;
  TMetadata.RegisterObject;
  TMetadataitems.RegisterObject;
  TNetwork.RegisterObject;
  TNetworkInterface.RegisterObject;
  TNetworkInterfaceaccessConfigs.RegisterObject;
  TNetworkList.RegisterObject;
  TNetworkListitems.RegisterObject;
  TOperation.RegisterObject;
  TOperationerror.RegisterObject;
  TOperationerrorerrors.RegisterObject;
  TOperationwarnings.RegisterObject;
  TOperationwarningsdata.RegisterObject;
  TOperationAggregatedList.RegisterObject;
  TOperationAggregatedListitems.RegisterObject;
  TOperationList.RegisterObject;
  TOperationListitems.RegisterObject;
  TOperationsScopedList.RegisterObject;
  TOperationsScopedListoperations.RegisterObject;
  TOperationsScopedListwarning.RegisterObject;
  TOperationsScopedListwarningdata.RegisterObject;
  TPathMatcher.RegisterObject;
  TPathMatcherpathRules.RegisterObject;
  TPathRule.RegisterObject;
  TPathRulepaths.RegisterObject;
  TProject.RegisterObject;
  TProjectquotas.RegisterObject;
  TQuota.RegisterObject;
  TRegion.RegisterObject;
  TRegionquotas.RegisterObject;
  TRegionzones.RegisterObject;
  TRegionList.RegisterObject;
  TRegionListitems.RegisterObject;
  TResourceGroupReference.RegisterObject;
  TRoute.RegisterObject;
  TRoutetags.RegisterObject;
  TRoutewarnings.RegisterObject;
  TRoutewarningsdata.RegisterObject;
  TRouteList.RegisterObject;
  TRouteListitems.RegisterObject;
  TScheduling.RegisterObject;
  TSerialPortOutput.RegisterObject;
  TServiceAccount.RegisterObject;
  TServiceAccountscopes.RegisterObject;
  TSnapshot.RegisterObject;
  TSnapshotlicenses.RegisterObject;
  TSnapshotList.RegisterObject;
  TSnapshotListitems.RegisterObject;
  TTags.RegisterObject;
  TTagsitems.RegisterObject;
  TTargetHttpProxy.RegisterObject;
  TTargetHttpProxyList.RegisterObject;
  TTargetHttpProxyListitems.RegisterObject;
  TTargetInstance.RegisterObject;
  TTargetInstanceAggregatedList.RegisterObject;
  TTargetInstanceAggregatedListitems.RegisterObject;
  TTargetInstanceList.RegisterObject;
  TTargetInstanceListitems.RegisterObject;
  TTargetInstancesScopedList.RegisterObject;
  TTargetInstancesScopedListtargetInstances.RegisterObject;
  TTargetInstancesScopedListwarning.RegisterObject;
  TTargetInstancesScopedListwarningdata.RegisterObject;
  TTargetPool.RegisterObject;
  TTargetPoolhealthChecks.RegisterObject;
  TTargetPoolinstances.RegisterObject;
  TTargetPoolAggregatedList.RegisterObject;
  TTargetPoolAggregatedListitems.RegisterObject;
  TTargetPoolInstanceHealth.RegisterObject;
  TTargetPoolInstanceHealthhealthStatus.RegisterObject;
  TTargetPoolList.RegisterObject;
  TTargetPoolListitems.RegisterObject;
  TTargetPoolsAddHealthCheckRequest.RegisterObject;
  TTargetPoolsAddHealthCheckRequesthealthChecks.RegisterObject;
  TTargetPoolsAddInstanceRequest.RegisterObject;
  TTargetPoolsAddInstanceRequestinstances.RegisterObject;
  TTargetPoolsRemoveHealthCheckRequest.RegisterObject;
  TTargetPoolsRemoveHealthCheckRequesthealthChecks.RegisterObject;
  TTargetPoolsRemoveInstanceRequest.RegisterObject;
  TTargetPoolsRemoveInstanceRequestinstances.RegisterObject;
  TTargetPoolsScopedList.RegisterObject;
  TTargetPoolsScopedListtargetPools.RegisterObject;
  TTargetPoolsScopedListwarning.RegisterObject;
  TTargetPoolsScopedListwarningdata.RegisterObject;
  TTargetReference.RegisterObject;
  TTargetVpnGateway.RegisterObject;
  TTargetVpnGatewayforwardingRules.RegisterObject;
  TTargetVpnGatewaytunnels.RegisterObject;
  TTargetVpnGatewayAggregatedList.RegisterObject;
  TTargetVpnGatewayAggregatedListitems.RegisterObject;
  TTargetVpnGatewayList.RegisterObject;
  TTargetVpnGatewayListitems.RegisterObject;
  TTargetVpnGatewaysScopedList.RegisterObject;
  TTargetVpnGatewaysScopedListtargetVpnGateways.RegisterObject;
  TTargetVpnGatewaysScopedListwarning.RegisterObject;
  TTargetVpnGatewaysScopedListwarningdata.RegisterObject;
  TTestFailure.RegisterObject;
  TUrlMap.RegisterObject;
  TUrlMaphostRules.RegisterObject;
  TUrlMappathMatchers.RegisterObject;
  TUrlMaptests.RegisterObject;
  TUrlMapList.RegisterObject;
  TUrlMapListitems.RegisterObject;
  TUrlMapReference.RegisterObject;
  TUrlMapTest.RegisterObject;
  TUrlMapValidationResult.RegisterObject;
  TUrlMapValidationResultloadErrors.RegisterObject;
  TUrlMapValidationResulttestFailures.RegisterObject;
  TUrlMapsValidateRequest.RegisterObject;
  TUrlMapsValidateResponse.RegisterObject;
  TUsageExportLocation.RegisterObject;
  TVpnTunnel.RegisterObject;
  TVpnTunnelikeNetworks.RegisterObject;
  TVpnTunnelAggregatedList.RegisterObject;
  TVpnTunnelAggregatedListitems.RegisterObject;
  TVpnTunnelList.RegisterObject;
  TVpnTunnelListitems.RegisterObject;
  TVpnTunnelsScopedList.RegisterObject;
  TVpnTunnelsScopedListvpnTunnels.RegisterObject;
  TVpnTunnelsScopedListwarning.RegisterObject;
  TVpnTunnelsScopedListwarningdata.RegisterObject;
  TZone.RegisterObject;
  TZonemaintenanceWindows.RegisterObject;
  TZoneList.RegisterObject;
  TZoneListitems.RegisterObject;
end;


Function TComputeAPI.GetAddressesInstance : TAddressesResource;

begin
  if (FAddressesInstance=Nil) then
    FAddressesInstance:=CreateAddressesResource;
  Result:=FAddressesInstance;
end;

Function TComputeAPI.CreateAddressesResource : TAddressesResource;

begin
  Result:=CreateAddressesResource(Self);
end;


Function TComputeAPI.CreateAddressesResource(AOwner : TComponent) : TAddressesResource;

begin
  Result:=TAddressesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetBackendServicesInstance : TBackendServicesResource;

begin
  if (FBackendServicesInstance=Nil) then
    FBackendServicesInstance:=CreateBackendServicesResource;
  Result:=FBackendServicesInstance;
end;

Function TComputeAPI.CreateBackendServicesResource : TBackendServicesResource;

begin
  Result:=CreateBackendServicesResource(Self);
end;


Function TComputeAPI.CreateBackendServicesResource(AOwner : TComponent) : TBackendServicesResource;

begin
  Result:=TBackendServicesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetDiskTypesInstance : TDiskTypesResource;

begin
  if (FDiskTypesInstance=Nil) then
    FDiskTypesInstance:=CreateDiskTypesResource;
  Result:=FDiskTypesInstance;
end;

Function TComputeAPI.CreateDiskTypesResource : TDiskTypesResource;

begin
  Result:=CreateDiskTypesResource(Self);
end;


Function TComputeAPI.CreateDiskTypesResource(AOwner : TComponent) : TDiskTypesResource;

begin
  Result:=TDiskTypesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetDisksInstance : TDisksResource;

begin
  if (FDisksInstance=Nil) then
    FDisksInstance:=CreateDisksResource;
  Result:=FDisksInstance;
end;

Function TComputeAPI.CreateDisksResource : TDisksResource;

begin
  Result:=CreateDisksResource(Self);
end;


Function TComputeAPI.CreateDisksResource(AOwner : TComponent) : TDisksResource;

begin
  Result:=TDisksResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetFirewallsInstance : TFirewallsResource;

begin
  if (FFirewallsInstance=Nil) then
    FFirewallsInstance:=CreateFirewallsResource;
  Result:=FFirewallsInstance;
end;

Function TComputeAPI.CreateFirewallsResource : TFirewallsResource;

begin
  Result:=CreateFirewallsResource(Self);
end;


Function TComputeAPI.CreateFirewallsResource(AOwner : TComponent) : TFirewallsResource;

begin
  Result:=TFirewallsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetForwardingRulesInstance : TForwardingRulesResource;

begin
  if (FForwardingRulesInstance=Nil) then
    FForwardingRulesInstance:=CreateForwardingRulesResource;
  Result:=FForwardingRulesInstance;
end;

Function TComputeAPI.CreateForwardingRulesResource : TForwardingRulesResource;

begin
  Result:=CreateForwardingRulesResource(Self);
end;


Function TComputeAPI.CreateForwardingRulesResource(AOwner : TComponent) : TForwardingRulesResource;

begin
  Result:=TForwardingRulesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetGlobalAddressesInstance : TGlobalAddressesResource;

begin
  if (FGlobalAddressesInstance=Nil) then
    FGlobalAddressesInstance:=CreateGlobalAddressesResource;
  Result:=FGlobalAddressesInstance;
end;

Function TComputeAPI.CreateGlobalAddressesResource : TGlobalAddressesResource;

begin
  Result:=CreateGlobalAddressesResource(Self);
end;


Function TComputeAPI.CreateGlobalAddressesResource(AOwner : TComponent) : TGlobalAddressesResource;

begin
  Result:=TGlobalAddressesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetGlobalForwardingRulesInstance : TGlobalForwardingRulesResource;

begin
  if (FGlobalForwardingRulesInstance=Nil) then
    FGlobalForwardingRulesInstance:=CreateGlobalForwardingRulesResource;
  Result:=FGlobalForwardingRulesInstance;
end;

Function TComputeAPI.CreateGlobalForwardingRulesResource : TGlobalForwardingRulesResource;

begin
  Result:=CreateGlobalForwardingRulesResource(Self);
end;


Function TComputeAPI.CreateGlobalForwardingRulesResource(AOwner : TComponent) : TGlobalForwardingRulesResource;

begin
  Result:=TGlobalForwardingRulesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetGlobalOperationsInstance : TGlobalOperationsResource;

begin
  if (FGlobalOperationsInstance=Nil) then
    FGlobalOperationsInstance:=CreateGlobalOperationsResource;
  Result:=FGlobalOperationsInstance;
end;

Function TComputeAPI.CreateGlobalOperationsResource : TGlobalOperationsResource;

begin
  Result:=CreateGlobalOperationsResource(Self);
end;


Function TComputeAPI.CreateGlobalOperationsResource(AOwner : TComponent) : TGlobalOperationsResource;

begin
  Result:=TGlobalOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetHttpHealthChecksInstance : THttpHealthChecksResource;

begin
  if (FHttpHealthChecksInstance=Nil) then
    FHttpHealthChecksInstance:=CreateHttpHealthChecksResource;
  Result:=FHttpHealthChecksInstance;
end;

Function TComputeAPI.CreateHttpHealthChecksResource : THttpHealthChecksResource;

begin
  Result:=CreateHttpHealthChecksResource(Self);
end;


Function TComputeAPI.CreateHttpHealthChecksResource(AOwner : TComponent) : THttpHealthChecksResource;

begin
  Result:=THttpHealthChecksResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetImagesInstance : TImagesResource;

begin
  if (FImagesInstance=Nil) then
    FImagesInstance:=CreateImagesResource;
  Result:=FImagesInstance;
end;

Function TComputeAPI.CreateImagesResource : TImagesResource;

begin
  Result:=CreateImagesResource(Self);
end;


Function TComputeAPI.CreateImagesResource(AOwner : TComponent) : TImagesResource;

begin
  Result:=TImagesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetInstanceTemplatesInstance : TInstanceTemplatesResource;

begin
  if (FInstanceTemplatesInstance=Nil) then
    FInstanceTemplatesInstance:=CreateInstanceTemplatesResource;
  Result:=FInstanceTemplatesInstance;
end;

Function TComputeAPI.CreateInstanceTemplatesResource : TInstanceTemplatesResource;

begin
  Result:=CreateInstanceTemplatesResource(Self);
end;


Function TComputeAPI.CreateInstanceTemplatesResource(AOwner : TComponent) : TInstanceTemplatesResource;

begin
  Result:=TInstanceTemplatesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetInstancesInstance : TInstancesResource;

begin
  if (FInstancesInstance=Nil) then
    FInstancesInstance:=CreateInstancesResource;
  Result:=FInstancesInstance;
end;

Function TComputeAPI.CreateInstancesResource : TInstancesResource;

begin
  Result:=CreateInstancesResource(Self);
end;


Function TComputeAPI.CreateInstancesResource(AOwner : TComponent) : TInstancesResource;

begin
  Result:=TInstancesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetLicensesInstance : TLicensesResource;

begin
  if (FLicensesInstance=Nil) then
    FLicensesInstance:=CreateLicensesResource;
  Result:=FLicensesInstance;
end;

Function TComputeAPI.CreateLicensesResource : TLicensesResource;

begin
  Result:=CreateLicensesResource(Self);
end;


Function TComputeAPI.CreateLicensesResource(AOwner : TComponent) : TLicensesResource;

begin
  Result:=TLicensesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetMachineTypesInstance : TMachineTypesResource;

begin
  if (FMachineTypesInstance=Nil) then
    FMachineTypesInstance:=CreateMachineTypesResource;
  Result:=FMachineTypesInstance;
end;

Function TComputeAPI.CreateMachineTypesResource : TMachineTypesResource;

begin
  Result:=CreateMachineTypesResource(Self);
end;


Function TComputeAPI.CreateMachineTypesResource(AOwner : TComponent) : TMachineTypesResource;

begin
  Result:=TMachineTypesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetNetworksInstance : TNetworksResource;

begin
  if (FNetworksInstance=Nil) then
    FNetworksInstance:=CreateNetworksResource;
  Result:=FNetworksInstance;
end;

Function TComputeAPI.CreateNetworksResource : TNetworksResource;

begin
  Result:=CreateNetworksResource(Self);
end;


Function TComputeAPI.CreateNetworksResource(AOwner : TComponent) : TNetworksResource;

begin
  Result:=TNetworksResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TComputeAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TComputeAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetRegionOperationsInstance : TRegionOperationsResource;

begin
  if (FRegionOperationsInstance=Nil) then
    FRegionOperationsInstance:=CreateRegionOperationsResource;
  Result:=FRegionOperationsInstance;
end;

Function TComputeAPI.CreateRegionOperationsResource : TRegionOperationsResource;

begin
  Result:=CreateRegionOperationsResource(Self);
end;


Function TComputeAPI.CreateRegionOperationsResource(AOwner : TComponent) : TRegionOperationsResource;

begin
  Result:=TRegionOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetRegionsInstance : TRegionsResource;

begin
  if (FRegionsInstance=Nil) then
    FRegionsInstance:=CreateRegionsResource;
  Result:=FRegionsInstance;
end;

Function TComputeAPI.CreateRegionsResource : TRegionsResource;

begin
  Result:=CreateRegionsResource(Self);
end;


Function TComputeAPI.CreateRegionsResource(AOwner : TComponent) : TRegionsResource;

begin
  Result:=TRegionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetRoutesInstance : TRoutesResource;

begin
  if (FRoutesInstance=Nil) then
    FRoutesInstance:=CreateRoutesResource;
  Result:=FRoutesInstance;
end;

Function TComputeAPI.CreateRoutesResource : TRoutesResource;

begin
  Result:=CreateRoutesResource(Self);
end;


Function TComputeAPI.CreateRoutesResource(AOwner : TComponent) : TRoutesResource;

begin
  Result:=TRoutesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetSnapshotsInstance : TSnapshotsResource;

begin
  if (FSnapshotsInstance=Nil) then
    FSnapshotsInstance:=CreateSnapshotsResource;
  Result:=FSnapshotsInstance;
end;

Function TComputeAPI.CreateSnapshotsResource : TSnapshotsResource;

begin
  Result:=CreateSnapshotsResource(Self);
end;


Function TComputeAPI.CreateSnapshotsResource(AOwner : TComponent) : TSnapshotsResource;

begin
  Result:=TSnapshotsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetTargetHttpProxiesInstance : TTargetHttpProxiesResource;

begin
  if (FTargetHttpProxiesInstance=Nil) then
    FTargetHttpProxiesInstance:=CreateTargetHttpProxiesResource;
  Result:=FTargetHttpProxiesInstance;
end;

Function TComputeAPI.CreateTargetHttpProxiesResource : TTargetHttpProxiesResource;

begin
  Result:=CreateTargetHttpProxiesResource(Self);
end;


Function TComputeAPI.CreateTargetHttpProxiesResource(AOwner : TComponent) : TTargetHttpProxiesResource;

begin
  Result:=TTargetHttpProxiesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetTargetInstancesInstance : TTargetInstancesResource;

begin
  if (FTargetInstancesInstance=Nil) then
    FTargetInstancesInstance:=CreateTargetInstancesResource;
  Result:=FTargetInstancesInstance;
end;

Function TComputeAPI.CreateTargetInstancesResource : TTargetInstancesResource;

begin
  Result:=CreateTargetInstancesResource(Self);
end;


Function TComputeAPI.CreateTargetInstancesResource(AOwner : TComponent) : TTargetInstancesResource;

begin
  Result:=TTargetInstancesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetTargetPoolsInstance : TTargetPoolsResource;

begin
  if (FTargetPoolsInstance=Nil) then
    FTargetPoolsInstance:=CreateTargetPoolsResource;
  Result:=FTargetPoolsInstance;
end;

Function TComputeAPI.CreateTargetPoolsResource : TTargetPoolsResource;

begin
  Result:=CreateTargetPoolsResource(Self);
end;


Function TComputeAPI.CreateTargetPoolsResource(AOwner : TComponent) : TTargetPoolsResource;

begin
  Result:=TTargetPoolsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetTargetVpnGatewaysInstance : TTargetVpnGatewaysResource;

begin
  if (FTargetVpnGatewaysInstance=Nil) then
    FTargetVpnGatewaysInstance:=CreateTargetVpnGatewaysResource;
  Result:=FTargetVpnGatewaysInstance;
end;

Function TComputeAPI.CreateTargetVpnGatewaysResource : TTargetVpnGatewaysResource;

begin
  Result:=CreateTargetVpnGatewaysResource(Self);
end;


Function TComputeAPI.CreateTargetVpnGatewaysResource(AOwner : TComponent) : TTargetVpnGatewaysResource;

begin
  Result:=TTargetVpnGatewaysResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetUrlMapsInstance : TUrlMapsResource;

begin
  if (FUrlMapsInstance=Nil) then
    FUrlMapsInstance:=CreateUrlMapsResource;
  Result:=FUrlMapsInstance;
end;

Function TComputeAPI.CreateUrlMapsResource : TUrlMapsResource;

begin
  Result:=CreateUrlMapsResource(Self);
end;


Function TComputeAPI.CreateUrlMapsResource(AOwner : TComponent) : TUrlMapsResource;

begin
  Result:=TUrlMapsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetVpnTunnelsInstance : TVpnTunnelsResource;

begin
  if (FVpnTunnelsInstance=Nil) then
    FVpnTunnelsInstance:=CreateVpnTunnelsResource;
  Result:=FVpnTunnelsInstance;
end;

Function TComputeAPI.CreateVpnTunnelsResource : TVpnTunnelsResource;

begin
  Result:=CreateVpnTunnelsResource(Self);
end;


Function TComputeAPI.CreateVpnTunnelsResource(AOwner : TComponent) : TVpnTunnelsResource;

begin
  Result:=TVpnTunnelsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetZoneOperationsInstance : TZoneOperationsResource;

begin
  if (FZoneOperationsInstance=Nil) then
    FZoneOperationsInstance:=CreateZoneOperationsResource;
  Result:=FZoneOperationsInstance;
end;

Function TComputeAPI.CreateZoneOperationsResource : TZoneOperationsResource;

begin
  Result:=CreateZoneOperationsResource(Self);
end;


Function TComputeAPI.CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;

begin
  Result:=TZoneOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeAPI.GetZonesInstance : TZonesResource;

begin
  if (FZonesInstance=Nil) then
    FZonesInstance:=CreateZonesResource;
  Result:=FZonesInstance;
end;

Function TComputeAPI.CreateZonesResource : TZonesResource;

begin
  Result:=CreateZonesResource(Self);
end;


Function TComputeAPI.CreateZonesResource(AOwner : TComponent) : TZonesResource;

begin
  Result:=TZonesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TComputeAPI.RegisterAPI;
end.
