unit googlemapsengine;
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
  TAcquisitionTime = class;
  TAcquisitionTimeArray = Array of TAcquisitionTime;
  TAsset = class;
  TAssetArray = Array of TAsset;
  TAssetbbox = class;
  TAssetbboxArray = Array of TAssetbbox;
  TAssettags = class;
  TAssettagsArray = Array of TAssettags;
  TAssetsListResponse = class;
  TAssetsListResponseArray = Array of TAssetsListResponse;
  TAssetsListResponseassets = class;
  TAssetsListResponseassetsArray = Array of TAssetsListResponseassets;
  TBorder = class;
  TBorderArray = Array of TBorder;
  TColor = class;
  TColorArray = Array of TColor;
  TDatasource = class;
  TDatasourceArray = Array of TDatasource;
  TDatasources = class;
  TDatasourcesArray = Array of TDatasources;
  TDisplayRule = class;
  TDisplayRuleArray = Array of TDisplayRule;
  TDisplayRulefilters = class;
  TDisplayRulefiltersArray = Array of TDisplayRulefilters;
  TFeature = class;
  TFeatureArray = Array of TFeature;
  TFeatureInfo = class;
  TFeatureInfoArray = Array of TFeatureInfo;
  TFeaturesBatchDeleteRequest = class;
  TFeaturesBatchDeleteRequestArray = Array of TFeaturesBatchDeleteRequest;
  TFeaturesBatchDeleteRequestgx_ids = class;
  TFeaturesBatchDeleteRequestgx_idsArray = Array of TFeaturesBatchDeleteRequestgx_ids;
  TFeaturesBatchDeleteRequestprimaryKeys = class;
  TFeaturesBatchDeleteRequestprimaryKeysArray = Array of TFeaturesBatchDeleteRequestprimaryKeys;
  TFeaturesBatchInsertRequest = class;
  TFeaturesBatchInsertRequestArray = Array of TFeaturesBatchInsertRequest;
  TFeaturesBatchInsertRequestfeatures = class;
  TFeaturesBatchInsertRequestfeaturesArray = Array of TFeaturesBatchInsertRequestfeatures;
  TFeaturesBatchPatchRequest = class;
  TFeaturesBatchPatchRequestArray = Array of TFeaturesBatchPatchRequest;
  TFeaturesBatchPatchRequestfeatures = class;
  TFeaturesBatchPatchRequestfeaturesArray = Array of TFeaturesBatchPatchRequestfeatures;
  TFeaturesListResponse = class;
  TFeaturesListResponseArray = Array of TFeaturesListResponse;
  TFeaturesListResponsefeatures = class;
  TFeaturesListResponsefeaturesArray = Array of TFeaturesListResponsefeatures;
  TFile = class;
  TFileArray = Array of TFile;
  TFilter = class;
  TFilterArray = Array of TFilter;
  TGeoJsonGeometry = class;
  TGeoJsonGeometryArray = Array of TGeoJsonGeometry;
  TGeoJsonGeometryCollection = class;
  TGeoJsonGeometryCollectionArray = Array of TGeoJsonGeometryCollection;
  TGeoJsonGeometryCollectiongeometries = class;
  TGeoJsonGeometryCollectiongeometriesArray = Array of TGeoJsonGeometryCollectiongeometries;
  TGeoJsonLineString = class;
  TGeoJsonLineStringArray = Array of TGeoJsonLineString;
  TGeoJsonLineStringcoordinates = class;
  TGeoJsonLineStringcoordinatesArray = Array of TGeoJsonLineStringcoordinates;
  TGeoJsonMultiLineString = class;
  TGeoJsonMultiLineStringArray = Array of TGeoJsonMultiLineString;
  TGeoJsonMultiLineStringcoordinates = class;
  TGeoJsonMultiLineStringcoordinatesArray = Array of TGeoJsonMultiLineStringcoordinates;
  TGeoJsonMultiPoint = class;
  TGeoJsonMultiPointArray = Array of TGeoJsonMultiPoint;
  TGeoJsonMultiPointcoordinates = class;
  TGeoJsonMultiPointcoordinatesArray = Array of TGeoJsonMultiPointcoordinates;
  TGeoJsonMultiPolygon = class;
  TGeoJsonMultiPolygonArray = Array of TGeoJsonMultiPolygon;
  TGeoJsonMultiPolygoncoordinates = class;
  TGeoJsonMultiPolygoncoordinatesArray = Array of TGeoJsonMultiPolygoncoordinates;
  TGeoJsonPoint = class;
  TGeoJsonPointArray = Array of TGeoJsonPoint;
  TGeoJsonPolygon = class;
  TGeoJsonPolygonArray = Array of TGeoJsonPolygon;
  TGeoJsonPolygoncoordinates = class;
  TGeoJsonPolygoncoordinatesArray = Array of TGeoJsonPolygoncoordinates;
  TGeoJsonPosition = class;
  TGeoJsonPositionArray = Array of TGeoJsonPosition;
  TGeoJsonProperties = class;
  TGeoJsonPropertiesArray = Array of TGeoJsonProperties;
  TIcon = class;
  TIconArray = Array of TIcon;
  TIconStyle = class;
  TIconStyleArray = Array of TIconStyle;
  TIconsListResponse = class;
  TIconsListResponseArray = Array of TIconsListResponse;
  TIconsListResponseicons = class;
  TIconsListResponseiconsArray = Array of TIconsListResponseicons;
  TLabelStyle = class;
  TLabelStyleArray = Array of TLabelStyle;
  TLatLngBox = class;
  TLatLngBoxArray = Array of TLatLngBox;
  TLayer = class;
  TLayerArray = Array of TLayer;
  TLayerbbox = class;
  TLayerbboxArray = Array of TLayerbbox;
  TLayersListResponse = class;
  TLayersListResponseArray = Array of TLayersListResponse;
  TLayersListResponselayers = class;
  TLayersListResponselayersArray = Array of TLayersListResponselayers;
  TLineStyle = class;
  TLineStyleArray = Array of TLineStyle;
  TLineStyledash = class;
  TLineStyledashArray = Array of TLineStyledash;
  TLineStylestroke = class;
  TLineStylestrokeArray = Array of TLineStylestroke;
  TMap = class;
  TMapArray = Array of TMap;
  TMapbbox = class;
  TMapbboxArray = Array of TMapbbox;
  TMapversions = class;
  TMapversionsArray = Array of TMapversions;
  TMapContents = class;
  TMapContentsArray = Array of TMapContents;
  TMapFolder = class;
  TMapFolderArray = Array of TMapFolder;
  TMapFoldercontents = class;
  TMapFoldercontentsArray = Array of TMapFoldercontents;
  TMapFolderdefaultViewport = class;
  TMapFolderdefaultViewportArray = Array of TMapFolderdefaultViewport;
  TMapItem = class;
  TMapItemArray = Array of TMapItem;
  TMapKmlLink = class;
  TMapKmlLinkArray = Array of TMapKmlLink;
  TMapKmlLinkdefaultViewport = class;
  TMapKmlLinkdefaultViewportArray = Array of TMapKmlLinkdefaultViewport;
  TMapLayer = class;
  TMapLayerArray = Array of TMapLayer;
  TMapLayerdefaultViewport = class;
  TMapLayerdefaultViewportArray = Array of TMapLayerdefaultViewport;
  TMapsListResponse = class;
  TMapsListResponseArray = Array of TMapsListResponse;
  TMapsListResponsemaps = class;
  TMapsListResponsemapsArray = Array of TMapsListResponsemaps;
  TParent = class;
  TParentArray = Array of TParent;
  TParentsListResponse = class;
  TParentsListResponseArray = Array of TParentsListResponse;
  TParentsListResponseparents = class;
  TParentsListResponseparentsArray = Array of TParentsListResponseparents;
  TPermission = class;
  TPermissionArray = Array of TPermission;
  TPermissionsBatchDeleteRequest = class;
  TPermissionsBatchDeleteRequestArray = Array of TPermissionsBatchDeleteRequest;
  TPermissionsBatchDeleteRequestids = class;
  TPermissionsBatchDeleteRequestidsArray = Array of TPermissionsBatchDeleteRequestids;
  TPermissionsBatchDeleteResponse = class;
  TPermissionsBatchDeleteResponseArray = Array of TPermissionsBatchDeleteResponse;
  TPermissionsBatchUpdateRequest = class;
  TPermissionsBatchUpdateRequestArray = Array of TPermissionsBatchUpdateRequest;
  TPermissionsBatchUpdateRequestpermissions = class;
  TPermissionsBatchUpdateRequestpermissionsArray = Array of TPermissionsBatchUpdateRequestpermissions;
  TPermissionsBatchUpdateResponse = class;
  TPermissionsBatchUpdateResponseArray = Array of TPermissionsBatchUpdateResponse;
  TPermissionsListResponse = class;
  TPermissionsListResponseArray = Array of TPermissionsListResponse;
  TPermissionsListResponsepermissions = class;
  TPermissionsListResponsepermissionsArray = Array of TPermissionsListResponsepermissions;
  TPointStyle = class;
  TPointStyleArray = Array of TPointStyle;
  TPolygonStyle = class;
  TPolygonStyleArray = Array of TPolygonStyle;
  TProcessResponse = class;
  TProcessResponseArray = Array of TProcessResponse;
  TProject = class;
  TProjectArray = Array of TProject;
  TProjectsListResponse = class;
  TProjectsListResponseArray = Array of TProjectsListResponse;
  TProjectsListResponseprojects = class;
  TProjectsListResponseprojectsArray = Array of TProjectsListResponseprojects;
  TPublishResponse = class;
  TPublishResponseArray = Array of TPublishResponse;
  TPublishedLayer = class;
  TPublishedLayerArray = Array of TPublishedLayer;
  TPublishedLayersListResponse = class;
  TPublishedLayersListResponseArray = Array of TPublishedLayersListResponse;
  TPublishedLayersListResponselayers = class;
  TPublishedLayersListResponselayersArray = Array of TPublishedLayersListResponselayers;
  TPublishedMap = class;
  TPublishedMapArray = Array of TPublishedMap;
  TPublishedMapsListResponse = class;
  TPublishedMapsListResponseArray = Array of TPublishedMapsListResponse;
  TPublishedMapsListResponsemaps = class;
  TPublishedMapsListResponsemapsArray = Array of TPublishedMapsListResponsemaps;
  TRaster = class;
  TRasterArray = Array of TRaster;
  TRasterbbox = class;
  TRasterbboxArray = Array of TRasterbbox;
  TRasterfiles = class;
  TRasterfilesArray = Array of TRasterfiles;
  TRasterCollection = class;
  TRasterCollectionArray = Array of TRasterCollection;
  TRasterCollectionbbox = class;
  TRasterCollectionbboxArray = Array of TRasterCollectionbbox;
  TRasterCollectionsListResponse = class;
  TRasterCollectionsListResponseArray = Array of TRasterCollectionsListResponse;
  TRasterCollectionsListResponserasterCollections = class;
  TRasterCollectionsListResponserasterCollectionsArray = Array of TRasterCollectionsListResponserasterCollections;
  TRasterCollectionsRaster = class;
  TRasterCollectionsRasterArray = Array of TRasterCollectionsRaster;
  TRasterCollectionsRasterbbox = class;
  TRasterCollectionsRasterbboxArray = Array of TRasterCollectionsRasterbbox;
  TRasterCollectionsRastertags = class;
  TRasterCollectionsRastertagsArray = Array of TRasterCollectionsRastertags;
  TRasterCollectionsRasterBatchDeleteRequest = class;
  TRasterCollectionsRasterBatchDeleteRequestArray = Array of TRasterCollectionsRasterBatchDeleteRequest;
  TRasterCollectionsRasterBatchDeleteRequestids = class;
  TRasterCollectionsRasterBatchDeleteRequestidsArray = Array of TRasterCollectionsRasterBatchDeleteRequestids;
  TRasterCollectionsRastersBatchDeleteResponse = class;
  TRasterCollectionsRastersBatchDeleteResponseArray = Array of TRasterCollectionsRastersBatchDeleteResponse;
  TRasterCollectionsRastersBatchInsertRequest = class;
  TRasterCollectionsRastersBatchInsertRequestArray = Array of TRasterCollectionsRastersBatchInsertRequest;
  TRasterCollectionsRastersBatchInsertRequestids = class;
  TRasterCollectionsRastersBatchInsertRequestidsArray = Array of TRasterCollectionsRastersBatchInsertRequestids;
  TRasterCollectionsRastersBatchInsertResponse = class;
  TRasterCollectionsRastersBatchInsertResponseArray = Array of TRasterCollectionsRastersBatchInsertResponse;
  TRasterCollectionsRastersListResponse = class;
  TRasterCollectionsRastersListResponseArray = Array of TRasterCollectionsRastersListResponse;
  TRasterCollectionsRastersListResponserasters = class;
  TRasterCollectionsRastersListResponserastersArray = Array of TRasterCollectionsRastersListResponserasters;
  TRastersListResponse = class;
  TRastersListResponseArray = Array of TRastersListResponse;
  TRastersListResponserasters = class;
  TRastersListResponserastersArray = Array of TRastersListResponserasters;
  TScaledShape = class;
  TScaledShapeArray = Array of TScaledShape;
  TScalingFunction = class;
  TScalingFunctionArray = Array of TScalingFunction;
  TSchema = class;
  TSchemaArray = Array of TSchema;
  TSchemacolumns = class;
  TSchemacolumnsArray = Array of TSchemacolumns;
  TSizeRange = class;
  TSizeRangeArray = Array of TSizeRange;
  TTable = class;
  TTableArray = Array of TTable;
  TTablebbox = class;
  TTablebboxArray = Array of TTablebbox;
  TTablefiles = class;
  TTablefilesArray = Array of TTablefiles;
  TTableColumn = class;
  TTableColumnArray = Array of TTableColumn;
  TTablesListResponse = class;
  TTablesListResponseArray = Array of TTablesListResponse;
  TTablesListResponsetables = class;
  TTablesListResponsetablesArray = Array of TTablesListResponsetables;
  TTags = class;
  TTagsArray = Array of TTags;
  TValueRange = class;
  TValueRangeArray = Array of TValueRange;
  TVectorStyle = class;
  TVectorStyleArray = Array of TVectorStyle;
  TVectorStyledisplayRules = class;
  TVectorStyledisplayRulesArray = Array of TVectorStyledisplayRules;
  TZoomLevels = class;
  TZoomLevelsArray = Array of TZoomLevels;
  
  { --------------------------------------------------------------------
    TAcquisitionTime
    --------------------------------------------------------------------}
  
  TAcquisitionTime = Class(TGoogleBaseObject)
  Private
    F_end : TDatetime;
    Fprecision : string;
    Fstart : TDatetime;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setprecision(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property _end : TDatetime Index 0 Read F_end Write Set_end;
    Property precision : string Index 8 Read Fprecision Write Setprecision;
    Property start : TDatetime Index 16 Read Fstart Write Setstart;
  end;
  TAcquisitionTimeClass = Class of TAcquisitionTime;
  
  { --------------------------------------------------------------------
    TAsset
    --------------------------------------------------------------------}
  
  TAsset = Class(TGoogleBaseObject)
  Private
    Fbbox : TAssetbbox;
    FcreationTime : TDatetime;
    FcreatorEmail : string;
    Fdescription : string;
    Fetag : string;
    Fid : string;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : string;
    Fname : string;
    FprojectId : string;
    Fresource : string;
    Ftags : TAssettags;
    F_type : string;
    FwritersCanEditPermissions : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TAssetbbox); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure Setresource(AIndex : Integer; AValue : string); virtual;
    Procedure Settags(AIndex : Integer; AValue : TAssettags); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bbox : TAssetbbox Index 0 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 8 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : string Index 16 Read FcreatorEmail Write SetcreatorEmail;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property etag : string Index 32 Read Fetag Write Setetag;
    Property id : string Index 40 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 48 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : string Index 56 Read FlastModifierEmail Write SetlastModifierEmail;
    Property name : string Index 64 Read Fname Write Setname;
    Property projectId : string Index 72 Read FprojectId Write SetprojectId;
    Property resource : string Index 80 Read Fresource Write Setresource;
    Property tags : TAssettags Index 88 Read Ftags Write Settags;
    Property _type : string Index 96 Read F_type Write Set_type;
    Property writersCanEditPermissions : boolean Index 104 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TAssetClass = Class of TAsset;
  
  { --------------------------------------------------------------------
    TAssetbbox
    --------------------------------------------------------------------}
  
  TAssetbbox = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAssetbboxClass = Class of TAssetbbox;
  
  { --------------------------------------------------------------------
    TAssettags
    --------------------------------------------------------------------}
  
  TAssettags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAssettagsClass = Class of TAssettags;
  
  { --------------------------------------------------------------------
    TAssetsListResponse
    --------------------------------------------------------------------}
  
  TAssetsListResponse = Class(TGoogleBaseObject)
  Private
    Fassets : TAssetsListResponseassets;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setassets(AIndex : Integer; AValue : TAssetsListResponseassets); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property assets : TAssetsListResponseassets Index 0 Read Fassets Write Setassets;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TAssetsListResponseClass = Class of TAssetsListResponse;
  
  { --------------------------------------------------------------------
    TAssetsListResponseassets
    --------------------------------------------------------------------}
  
  TAssetsListResponseassets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAssetsListResponseassetsClass = Class of TAssetsListResponseassets;
  
  { --------------------------------------------------------------------
    TBorder
    --------------------------------------------------------------------}
  
  TBorder = Class(TGoogleBaseObject)
  Private
    Fcolor : string;
    Fopacity : double;
    Fwidth : double;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : string); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property color : string Index 0 Read Fcolor Write Setcolor;
    Property opacity : double Index 8 Read Fopacity Write Setopacity;
    Property width : double Index 16 Read Fwidth Write Setwidth;
  end;
  TBorderClass = Class of TBorder;
  
  { --------------------------------------------------------------------
    TColor
    --------------------------------------------------------------------}
  
  TColor = Class(TGoogleBaseObject)
  Private
    Fcolor : string;
    Fopacity : double;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : string); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property color : string Index 0 Read Fcolor Write Setcolor;
    Property opacity : double Index 8 Read Fopacity Write Setopacity;
  end;
  TColorClass = Class of TColor;
  
  { --------------------------------------------------------------------
    TDatasource
    --------------------------------------------------------------------}
  
  TDatasource = Class(TGoogleBaseObject)
  Private
    Fid : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
  end;
  TDatasourceClass = Class of TDatasource;
  
  { --------------------------------------------------------------------
    TDatasources
    --------------------------------------------------------------------}
  
  TDatasources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatasourcesClass = Class of TDatasources;
  
  { --------------------------------------------------------------------
    TDisplayRule
    --------------------------------------------------------------------}
  
  TDisplayRule = Class(TGoogleBaseObject)
  Private
    Ffilters : TDisplayRulefilters;
    FlineOptions : TLineStyle;
    Fname : string;
    FpointOptions : TPointStyle;
    FpolygonOptions : TPolygonStyle;
    FzoomLevels : TZoomLevels;
  Protected
    //Property setters
    Procedure Setfilters(AIndex : Integer; AValue : TDisplayRulefilters); virtual;
    Procedure SetlineOptions(AIndex : Integer; AValue : TLineStyle); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpointOptions(AIndex : Integer; AValue : TPointStyle); virtual;
    Procedure SetpolygonOptions(AIndex : Integer; AValue : TPolygonStyle); virtual;
    Procedure SetzoomLevels(AIndex : Integer; AValue : TZoomLevels); virtual;
  Public
  Published
    Property filters : TDisplayRulefilters Index 0 Read Ffilters Write Setfilters;
    Property lineOptions : TLineStyle Index 8 Read FlineOptions Write SetlineOptions;
    Property name : string Index 16 Read Fname Write Setname;
    Property pointOptions : TPointStyle Index 24 Read FpointOptions Write SetpointOptions;
    Property polygonOptions : TPolygonStyle Index 32 Read FpolygonOptions Write SetpolygonOptions;
    Property zoomLevels : TZoomLevels Index 40 Read FzoomLevels Write SetzoomLevels;
  end;
  TDisplayRuleClass = Class of TDisplayRule;
  
  { --------------------------------------------------------------------
    TDisplayRulefilters
    --------------------------------------------------------------------}
  
  TDisplayRulefilters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDisplayRulefiltersClass = Class of TDisplayRulefilters;
  
  { --------------------------------------------------------------------
    TFeature
    --------------------------------------------------------------------}
  
  TFeature = Class(TGoogleBaseObject)
  Private
    Fgeometry : TGeoJsonGeometry;
    Fproperties : TGeoJsonProperties;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setgeometry(AIndex : Integer; AValue : TGeoJsonGeometry); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TGeoJsonProperties); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property geometry : TGeoJsonGeometry Index 0 Read Fgeometry Write Setgeometry;
    Property properties : TGeoJsonProperties Index 8 Read Fproperties Write Setproperties;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TFeatureClass = Class of TFeature;
  
  { --------------------------------------------------------------------
    TFeatureInfo
    --------------------------------------------------------------------}
  
  TFeatureInfo = Class(TGoogleBaseObject)
  Private
    Fcontent : string;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property content : string Index 0 Read Fcontent Write Setcontent;
  end;
  TFeatureInfoClass = Class of TFeatureInfo;
  
  { --------------------------------------------------------------------
    TFeaturesBatchDeleteRequest
    --------------------------------------------------------------------}
  
  TFeaturesBatchDeleteRequest = Class(TGoogleBaseObject)
  Private
    Fgx_ids : TFeaturesBatchDeleteRequestgx_ids;
    FprimaryKeys : TFeaturesBatchDeleteRequestprimaryKeys;
  Protected
    //Property setters
    Procedure Setgx_ids(AIndex : Integer; AValue : TFeaturesBatchDeleteRequestgx_ids); virtual;
    Procedure SetprimaryKeys(AIndex : Integer; AValue : TFeaturesBatchDeleteRequestprimaryKeys); virtual;
  Public
  Published
    Property gx_ids : TFeaturesBatchDeleteRequestgx_ids Index 0 Read Fgx_ids Write Setgx_ids;
    Property primaryKeys : TFeaturesBatchDeleteRequestprimaryKeys Index 8 Read FprimaryKeys Write SetprimaryKeys;
  end;
  TFeaturesBatchDeleteRequestClass = Class of TFeaturesBatchDeleteRequest;
  
  { --------------------------------------------------------------------
    TFeaturesBatchDeleteRequestgx_ids
    --------------------------------------------------------------------}
  
  TFeaturesBatchDeleteRequestgx_ids = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFeaturesBatchDeleteRequestgx_idsClass = Class of TFeaturesBatchDeleteRequestgx_ids;
  
  { --------------------------------------------------------------------
    TFeaturesBatchDeleteRequestprimaryKeys
    --------------------------------------------------------------------}
  
  TFeaturesBatchDeleteRequestprimaryKeys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFeaturesBatchDeleteRequestprimaryKeysClass = Class of TFeaturesBatchDeleteRequestprimaryKeys;
  
  { --------------------------------------------------------------------
    TFeaturesBatchInsertRequest
    --------------------------------------------------------------------}
  
  TFeaturesBatchInsertRequest = Class(TGoogleBaseObject)
  Private
    Ffeatures : TFeaturesBatchInsertRequestfeatures;
    FnormalizeGeometries : boolean;
  Protected
    //Property setters
    Procedure Setfeatures(AIndex : Integer; AValue : TFeaturesBatchInsertRequestfeatures); virtual;
    Procedure SetnormalizeGeometries(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property features : TFeaturesBatchInsertRequestfeatures Index 0 Read Ffeatures Write Setfeatures;
    Property normalizeGeometries : boolean Index 8 Read FnormalizeGeometries Write SetnormalizeGeometries;
  end;
  TFeaturesBatchInsertRequestClass = Class of TFeaturesBatchInsertRequest;
  
  { --------------------------------------------------------------------
    TFeaturesBatchInsertRequestfeatures
    --------------------------------------------------------------------}
  
  TFeaturesBatchInsertRequestfeatures = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFeaturesBatchInsertRequestfeaturesClass = Class of TFeaturesBatchInsertRequestfeatures;
  
  { --------------------------------------------------------------------
    TFeaturesBatchPatchRequest
    --------------------------------------------------------------------}
  
  TFeaturesBatchPatchRequest = Class(TGoogleBaseObject)
  Private
    Ffeatures : TFeaturesBatchPatchRequestfeatures;
    FnormalizeGeometries : boolean;
  Protected
    //Property setters
    Procedure Setfeatures(AIndex : Integer; AValue : TFeaturesBatchPatchRequestfeatures); virtual;
    Procedure SetnormalizeGeometries(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property features : TFeaturesBatchPatchRequestfeatures Index 0 Read Ffeatures Write Setfeatures;
    Property normalizeGeometries : boolean Index 8 Read FnormalizeGeometries Write SetnormalizeGeometries;
  end;
  TFeaturesBatchPatchRequestClass = Class of TFeaturesBatchPatchRequest;
  
  { --------------------------------------------------------------------
    TFeaturesBatchPatchRequestfeatures
    --------------------------------------------------------------------}
  
  TFeaturesBatchPatchRequestfeatures = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFeaturesBatchPatchRequestfeaturesClass = Class of TFeaturesBatchPatchRequestfeatures;
  
  { --------------------------------------------------------------------
    TFeaturesListResponse
    --------------------------------------------------------------------}
  
  TFeaturesListResponse = Class(TGoogleBaseObject)
  Private
    FallowedQueriesPerSecond : double;
    Ffeatures : TFeaturesListResponsefeatures;
    FnextPageToken : string;
    Fschema : TSchema;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetallowedQueriesPerSecond(AIndex : Integer; AValue : double); virtual;
    Procedure Setfeatures(AIndex : Integer; AValue : TFeaturesListResponsefeatures); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setschema(AIndex : Integer; AValue : TSchema); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property allowedQueriesPerSecond : double Index 0 Read FallowedQueriesPerSecond Write SetallowedQueriesPerSecond;
    Property features : TFeaturesListResponsefeatures Index 8 Read Ffeatures Write Setfeatures;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property schema : TSchema Index 24 Read Fschema Write Setschema;
    Property _type : string Index 32 Read F_type Write Set_type;
  end;
  TFeaturesListResponseClass = Class of TFeaturesListResponse;
  
  { --------------------------------------------------------------------
    TFeaturesListResponsefeatures
    --------------------------------------------------------------------}
  
  TFeaturesListResponsefeatures = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFeaturesListResponsefeaturesClass = Class of TFeaturesListResponsefeatures;
  
  { --------------------------------------------------------------------
    TFile
    --------------------------------------------------------------------}
  
  TFile = Class(TGoogleBaseObject)
  Private
    Ffilename : string;
    Fsize : string;
    FuploadStatus : string;
  Protected
    //Property setters
    Procedure Setfilename(AIndex : Integer; AValue : string); virtual;
    Procedure Setsize(AIndex : Integer; AValue : string); virtual;
    Procedure SetuploadStatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property filename : string Index 0 Read Ffilename Write Setfilename;
    Property size : string Index 8 Read Fsize Write Setsize;
    Property uploadStatus : string Index 16 Read FuploadStatus Write SetuploadStatus;
  end;
  TFileClass = Class of TFile;
  
  { --------------------------------------------------------------------
    TFilter
    --------------------------------------------------------------------}
  
  TFilter = Class(TGoogleBaseObject)
  Private
    Fcolumn : string;
    F_operator : string;
    Fvalue : TJSONSchema;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolumn(AIndex : Integer; AValue : string); virtual;
    Procedure Set_operator(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : TJSONSchema); virtual;
  Public
  Published
    Property column : string Index 0 Read Fcolumn Write Setcolumn;
    Property _operator : string Index 8 Read F_operator Write Set_operator;
    Property value : TJSONSchema Index 16 Read Fvalue Write Setvalue;
  end;
  TFilterClass = Class of TFilter;
  
  { --------------------------------------------------------------------
    TGeoJsonGeometry
    --------------------------------------------------------------------}
  
  TGeoJsonGeometry = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeoJsonGeometryClass = Class of TGeoJsonGeometry;
  
  { --------------------------------------------------------------------
    TGeoJsonGeometryCollection
    --------------------------------------------------------------------}
  
  TGeoJsonGeometryCollection = Class(TGoogleBaseObject)
  Private
    Fgeometries : TGeoJsonGeometryCollectiongeometries;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setgeometries(AIndex : Integer; AValue : TGeoJsonGeometryCollectiongeometries); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property geometries : TGeoJsonGeometryCollectiongeometries Index 0 Read Fgeometries Write Setgeometries;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonGeometryCollectionClass = Class of TGeoJsonGeometryCollection;
  
  { --------------------------------------------------------------------
    TGeoJsonGeometryCollectiongeometries
    --------------------------------------------------------------------}
  
  TGeoJsonGeometryCollectiongeometries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeoJsonGeometryCollectiongeometriesClass = Class of TGeoJsonGeometryCollectiongeometries;
  
  { --------------------------------------------------------------------
    TGeoJsonLineString
    --------------------------------------------------------------------}
  
  TGeoJsonLineString = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonLineStringcoordinates;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonLineStringcoordinates); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TGeoJsonLineStringcoordinates Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonLineStringClass = Class of TGeoJsonLineString;
  
  { --------------------------------------------------------------------
    TGeoJsonLineStringcoordinates
    --------------------------------------------------------------------}
  
  TGeoJsonLineStringcoordinates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeoJsonLineStringcoordinatesClass = Class of TGeoJsonLineStringcoordinates;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiLineString
    --------------------------------------------------------------------}
  
  TGeoJsonMultiLineString = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonMultiLineStringcoordinates;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiLineStringcoordinates); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TGeoJsonMultiLineStringcoordinates Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonMultiLineStringClass = Class of TGeoJsonMultiLineString;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiLineStringcoordinates
    --------------------------------------------------------------------}
  
  TGeoJsonMultiLineStringcoordinates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeoJsonMultiLineStringcoordinatesClass = Class of TGeoJsonMultiLineStringcoordinates;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiPoint
    --------------------------------------------------------------------}
  
  TGeoJsonMultiPoint = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonMultiPointcoordinates;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiPointcoordinates); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TGeoJsonMultiPointcoordinates Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonMultiPointClass = Class of TGeoJsonMultiPoint;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiPointcoordinates
    --------------------------------------------------------------------}
  
  TGeoJsonMultiPointcoordinates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeoJsonMultiPointcoordinatesClass = Class of TGeoJsonMultiPointcoordinates;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiPolygon
    --------------------------------------------------------------------}
  
  TGeoJsonMultiPolygon = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonMultiPolygoncoordinates;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiPolygoncoordinates); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TGeoJsonMultiPolygoncoordinates Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonMultiPolygonClass = Class of TGeoJsonMultiPolygon;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiPolygoncoordinates
    --------------------------------------------------------------------}
  
  TGeoJsonMultiPolygoncoordinates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeoJsonMultiPolygoncoordinatesClass = Class of TGeoJsonMultiPolygoncoordinates;
  
  { --------------------------------------------------------------------
    TGeoJsonPoint
    --------------------------------------------------------------------}
  
  TGeoJsonPoint = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonPosition;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonPosition); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TGeoJsonPosition Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonPointClass = Class of TGeoJsonPoint;
  
  { --------------------------------------------------------------------
    TGeoJsonPolygon
    --------------------------------------------------------------------}
  
  TGeoJsonPolygon = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonPolygoncoordinates;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonPolygoncoordinates); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TGeoJsonPolygoncoordinates Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonPolygonClass = Class of TGeoJsonPolygon;
  
  { --------------------------------------------------------------------
    TGeoJsonPolygoncoordinates
    --------------------------------------------------------------------}
  
  TGeoJsonPolygoncoordinates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeoJsonPolygoncoordinatesClass = Class of TGeoJsonPolygoncoordinates;
  
  { --------------------------------------------------------------------
    TGeoJsonPosition
    --------------------------------------------------------------------}
  
  TGeoJsonPosition = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeoJsonPositionClass = Class of TGeoJsonPosition;
  
  { --------------------------------------------------------------------
    TGeoJsonProperties
    --------------------------------------------------------------------}
  
  TGeoJsonProperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TGeoJsonPropertiesClass = Class of TGeoJsonProperties;
  
  { --------------------------------------------------------------------
    TIcon
    --------------------------------------------------------------------}
  
  TIcon = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fid : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property id : string Index 8 Read Fid Write Setid;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TIconClass = Class of TIcon;
  
  { --------------------------------------------------------------------
    TIconStyle
    --------------------------------------------------------------------}
  
  TIconStyle = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fname : string;
    FscaledShape : TScaledShape;
    FscalingFunction : TScalingFunction;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetscaledShape(AIndex : Integer; AValue : TScaledShape); virtual;
    Procedure SetscalingFunction(AIndex : Integer; AValue : TScalingFunction); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property name : string Index 8 Read Fname Write Setname;
    Property scaledShape : TScaledShape Index 16 Read FscaledShape Write SetscaledShape;
    Property scalingFunction : TScalingFunction Index 24 Read FscalingFunction Write SetscalingFunction;
  end;
  TIconStyleClass = Class of TIconStyle;
  
  { --------------------------------------------------------------------
    TIconsListResponse
    --------------------------------------------------------------------}
  
  TIconsListResponse = Class(TGoogleBaseObject)
  Private
    Ficons : TIconsListResponseicons;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Seticons(AIndex : Integer; AValue : TIconsListResponseicons); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property icons : TIconsListResponseicons Index 0 Read Ficons Write Seticons;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TIconsListResponseClass = Class of TIconsListResponse;
  
  { --------------------------------------------------------------------
    TIconsListResponseicons
    --------------------------------------------------------------------}
  
  TIconsListResponseicons = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TIconsListResponseiconsClass = Class of TIconsListResponseicons;
  
  { --------------------------------------------------------------------
    TLabelStyle
    --------------------------------------------------------------------}
  
  TLabelStyle = Class(TGoogleBaseObject)
  Private
    Fcolor : string;
    Fcolumn : string;
    FfontStyle : string;
    FfontWeight : string;
    Fopacity : double;
    Foutline : TColor;
    Fsize : double;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : string); virtual;
    Procedure Setcolumn(AIndex : Integer; AValue : string); virtual;
    Procedure SetfontStyle(AIndex : Integer; AValue : string); virtual;
    Procedure SetfontWeight(AIndex : Integer; AValue : string); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
    Procedure Setoutline(AIndex : Integer; AValue : TColor); virtual;
    Procedure Setsize(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property color : string Index 0 Read Fcolor Write Setcolor;
    Property column : string Index 8 Read Fcolumn Write Setcolumn;
    Property fontStyle : string Index 16 Read FfontStyle Write SetfontStyle;
    Property fontWeight : string Index 24 Read FfontWeight Write SetfontWeight;
    Property opacity : double Index 32 Read Fopacity Write Setopacity;
    Property outline : TColor Index 40 Read Foutline Write Setoutline;
    Property size : double Index 48 Read Fsize Write Setsize;
  end;
  TLabelStyleClass = Class of TLabelStyle;
  
  { --------------------------------------------------------------------
    TLatLngBox
    --------------------------------------------------------------------}
  
  TLatLngBox = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLatLngBoxClass = Class of TLatLngBox;
  
  { --------------------------------------------------------------------
    TLayer
    --------------------------------------------------------------------}
  
  TLayer = Class(TGoogleBaseObject)
  Private
    Fbbox : TLayerbbox;
    FcreationTime : TDatetime;
    FcreatorEmail : string;
    FdatasourceType : string;
    Fdatasources : TDatasources;
    Fdescription : string;
    FdraftAccessList : string;
    Fetag : string;
    Fid : string;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : string;
    FlayerType : string;
    Fname : string;
    FprocessingStatus : string;
    FprojectId : string;
    FpublishedAccessList : string;
    FpublishingStatus : string;
    Fstyle : TVectorStyle;
    Ftags : TAssettags;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TLayerbbox); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : string); virtual;
    Procedure SetdatasourceType(AIndex : Integer; AValue : string); virtual;
    Procedure Setdatasources(AIndex : Integer; AValue : TDatasources); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : string); virtual;
    Procedure SetlayerType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublishedAccessList(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublishingStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setstyle(AIndex : Integer; AValue : TVectorStyle); virtual;
    Procedure Settags(AIndex : Integer; AValue : TAssettags); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bbox : TLayerbbox Index 0 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 8 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : string Index 16 Read FcreatorEmail Write SetcreatorEmail;
    Property datasourceType : string Index 24 Read FdatasourceType Write SetdatasourceType;
    Property datasources : TDatasources Index 32 Read Fdatasources Write Setdatasources;
    Property description : string Index 40 Read Fdescription Write Setdescription;
    Property draftAccessList : string Index 48 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : string Index 56 Read Fetag Write Setetag;
    Property id : string Index 64 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 72 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : string Index 80 Read FlastModifierEmail Write SetlastModifierEmail;
    Property layerType : string Index 88 Read FlayerType Write SetlayerType;
    Property name : string Index 96 Read Fname Write Setname;
    Property processingStatus : string Index 104 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : string Index 112 Read FprojectId Write SetprojectId;
    Property publishedAccessList : string Index 120 Read FpublishedAccessList Write SetpublishedAccessList;
    Property publishingStatus : string Index 128 Read FpublishingStatus Write SetpublishingStatus;
    Property style : TVectorStyle Index 136 Read Fstyle Write Setstyle;
    Property tags : TAssettags Index 144 Read Ftags Write Settags;
    Property writersCanEditPermissions : boolean Index 152 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TLayerClass = Class of TLayer;
  
  { --------------------------------------------------------------------
    TLayerbbox
    --------------------------------------------------------------------}
  
  TLayerbbox = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLayerbboxClass = Class of TLayerbbox;
  
  { --------------------------------------------------------------------
    TLayersListResponse
    --------------------------------------------------------------------}
  
  TLayersListResponse = Class(TGoogleBaseObject)
  Private
    Flayers : TLayersListResponselayers;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setlayers(AIndex : Integer; AValue : TLayersListResponselayers); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property layers : TLayersListResponselayers Index 0 Read Flayers Write Setlayers;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TLayersListResponseClass = Class of TLayersListResponse;
  
  { --------------------------------------------------------------------
    TLayersListResponselayers
    --------------------------------------------------------------------}
  
  TLayersListResponselayers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLayersListResponselayersClass = Class of TLayersListResponselayers;
  
  { --------------------------------------------------------------------
    TLineStyle
    --------------------------------------------------------------------}
  
  TLineStyle = Class(TGoogleBaseObject)
  Private
    Fborder : TBorder;
    Fdash : TLineStyledash;
    F_label : TLabelStyle;
    Fstroke : TLineStylestroke;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setborder(AIndex : Integer; AValue : TBorder); virtual;
    Procedure Setdash(AIndex : Integer; AValue : TLineStyledash); virtual;
    Procedure Set_label(AIndex : Integer; AValue : TLabelStyle); virtual;
    Procedure Setstroke(AIndex : Integer; AValue : TLineStylestroke); virtual;
  Public
  Published
    Property border : TBorder Index 0 Read Fborder Write Setborder;
    Property dash : TLineStyledash Index 8 Read Fdash Write Setdash;
    Property _label : TLabelStyle Index 16 Read F_label Write Set_label;
    Property stroke : TLineStylestroke Index 24 Read Fstroke Write Setstroke;
  end;
  TLineStyleClass = Class of TLineStyle;
  
  { --------------------------------------------------------------------
    TLineStyledash
    --------------------------------------------------------------------}
  
  TLineStyledash = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLineStyledashClass = Class of TLineStyledash;
  
  { --------------------------------------------------------------------
    TLineStylestroke
    --------------------------------------------------------------------}
  
  TLineStylestroke = Class(TGoogleBaseObject)
  Private
    Fcolor : string;
    Fopacity : double;
    Fwidth : double;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : string); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property color : string Index 0 Read Fcolor Write Setcolor;
    Property opacity : double Index 8 Read Fopacity Write Setopacity;
    Property width : double Index 16 Read Fwidth Write Setwidth;
  end;
  TLineStylestrokeClass = Class of TLineStylestroke;
  
  { --------------------------------------------------------------------
    TMap
    --------------------------------------------------------------------}
  
  TMap = Class(TGoogleBaseObject)
  Private
    Fbbox : TMapbbox;
    Fcontents : TMapContents;
    FcreationTime : TDatetime;
    FcreatorEmail : string;
    FdefaultViewport : TLatLngBox;
    Fdescription : string;
    FdraftAccessList : string;
    Fetag : string;
    Fid : string;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : string;
    Fname : string;
    FprocessingStatus : string;
    FprojectId : string;
    FpublishedAccessList : string;
    FpublishingStatus : string;
    Ftags : TAssettags;
    Fversions : TMapversions;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TMapbbox); virtual;
    Procedure Setcontents(AIndex : Integer; AValue : TMapContents); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TLatLngBox); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublishedAccessList(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublishingStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Settags(AIndex : Integer; AValue : TAssettags); virtual;
    Procedure Setversions(AIndex : Integer; AValue : TMapversions); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bbox : TMapbbox Index 0 Read Fbbox Write Setbbox;
    Property contents : TMapContents Index 8 Read Fcontents Write Setcontents;
    Property creationTime : TDatetime Index 16 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : string Index 24 Read FcreatorEmail Write SetcreatorEmail;
    Property defaultViewport : TLatLngBox Index 32 Read FdefaultViewport Write SetdefaultViewport;
    Property description : string Index 40 Read Fdescription Write Setdescription;
    Property draftAccessList : string Index 48 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : string Index 56 Read Fetag Write Setetag;
    Property id : string Index 64 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 72 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : string Index 80 Read FlastModifierEmail Write SetlastModifierEmail;
    Property name : string Index 88 Read Fname Write Setname;
    Property processingStatus : string Index 96 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : string Index 104 Read FprojectId Write SetprojectId;
    Property publishedAccessList : string Index 112 Read FpublishedAccessList Write SetpublishedAccessList;
    Property publishingStatus : string Index 120 Read FpublishingStatus Write SetpublishingStatus;
    Property tags : TAssettags Index 128 Read Ftags Write Settags;
    Property versions : TMapversions Index 136 Read Fversions Write Setversions;
    Property writersCanEditPermissions : boolean Index 144 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TMapClass = Class of TMap;
  
  { --------------------------------------------------------------------
    TMapbbox
    --------------------------------------------------------------------}
  
  TMapbbox = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapbboxClass = Class of TMapbbox;
  
  { --------------------------------------------------------------------
    TMapversions
    --------------------------------------------------------------------}
  
  TMapversions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapversionsClass = Class of TMapversions;
  
  { --------------------------------------------------------------------
    TMapContents
    --------------------------------------------------------------------}
  
  TMapContents = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapContentsClass = Class of TMapContents;
  
  { --------------------------------------------------------------------
    TMapFolder
    --------------------------------------------------------------------}
  
  TMapFolder = Class(TGoogleBaseObject)
  Private
    Fcontents : TMapFoldercontents;
    FdefaultViewport : TMapFolderdefaultViewport;
    Fexpandable : boolean;
    Fkey : string;
    Fname : string;
    F_type : string;
    Fvisibility : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcontents(AIndex : Integer; AValue : TMapFoldercontents); virtual;
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TMapFolderdefaultViewport); virtual;
    Procedure Setexpandable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property contents : TMapFoldercontents Index 0 Read Fcontents Write Setcontents;
    Property defaultViewport : TMapFolderdefaultViewport Index 8 Read FdefaultViewport Write SetdefaultViewport;
    Property expandable : boolean Index 16 Read Fexpandable Write Setexpandable;
    Property key : string Index 24 Read Fkey Write Setkey;
    Property name : string Index 32 Read Fname Write Setname;
    Property _type : string Index 40 Read F_type Write Set_type;
    Property visibility : string Index 48 Read Fvisibility Write Setvisibility;
  end;
  TMapFolderClass = Class of TMapFolder;
  
  { --------------------------------------------------------------------
    TMapFoldercontents
    --------------------------------------------------------------------}
  
  TMapFoldercontents = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapFoldercontentsClass = Class of TMapFoldercontents;
  
  { --------------------------------------------------------------------
    TMapFolderdefaultViewport
    --------------------------------------------------------------------}
  
  TMapFolderdefaultViewport = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapFolderdefaultViewportClass = Class of TMapFolderdefaultViewport;
  
  { --------------------------------------------------------------------
    TMapItem
    --------------------------------------------------------------------}
  
  TMapItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapItemClass = Class of TMapItem;
  
  { --------------------------------------------------------------------
    TMapKmlLink
    --------------------------------------------------------------------}
  
  TMapKmlLink = Class(TGoogleBaseObject)
  Private
    FdefaultViewport : TMapKmlLinkdefaultViewport;
    FkmlUrl : string;
    Fname : string;
    F_type : string;
    Fvisibility : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TMapKmlLinkdefaultViewport); virtual;
    Procedure SetkmlUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property defaultViewport : TMapKmlLinkdefaultViewport Index 0 Read FdefaultViewport Write SetdefaultViewport;
    Property kmlUrl : string Index 8 Read FkmlUrl Write SetkmlUrl;
    Property name : string Index 16 Read Fname Write Setname;
    Property _type : string Index 24 Read F_type Write Set_type;
    Property visibility : string Index 32 Read Fvisibility Write Setvisibility;
  end;
  TMapKmlLinkClass = Class of TMapKmlLink;
  
  { --------------------------------------------------------------------
    TMapKmlLinkdefaultViewport
    --------------------------------------------------------------------}
  
  TMapKmlLinkdefaultViewport = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapKmlLinkdefaultViewportClass = Class of TMapKmlLinkdefaultViewport;
  
  { --------------------------------------------------------------------
    TMapLayer
    --------------------------------------------------------------------}
  
  TMapLayer = Class(TGoogleBaseObject)
  Private
    FdefaultViewport : TMapLayerdefaultViewport;
    Fid : string;
    Fkey : string;
    Fname : string;
    F_type : string;
    Fvisibility : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TMapLayerdefaultViewport); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property defaultViewport : TMapLayerdefaultViewport Index 0 Read FdefaultViewport Write SetdefaultViewport;
    Property id : string Index 8 Read Fid Write Setid;
    Property key : string Index 16 Read Fkey Write Setkey;
    Property name : string Index 24 Read Fname Write Setname;
    Property _type : string Index 32 Read F_type Write Set_type;
    Property visibility : string Index 40 Read Fvisibility Write Setvisibility;
  end;
  TMapLayerClass = Class of TMapLayer;
  
  { --------------------------------------------------------------------
    TMapLayerdefaultViewport
    --------------------------------------------------------------------}
  
  TMapLayerdefaultViewport = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapLayerdefaultViewportClass = Class of TMapLayerdefaultViewport;
  
  { --------------------------------------------------------------------
    TMapsListResponse
    --------------------------------------------------------------------}
  
  TMapsListResponse = Class(TGoogleBaseObject)
  Private
    Fmaps : TMapsListResponsemaps;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setmaps(AIndex : Integer; AValue : TMapsListResponsemaps); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property maps : TMapsListResponsemaps Index 0 Read Fmaps Write Setmaps;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TMapsListResponseClass = Class of TMapsListResponse;
  
  { --------------------------------------------------------------------
    TMapsListResponsemaps
    --------------------------------------------------------------------}
  
  TMapsListResponsemaps = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapsListResponsemapsClass = Class of TMapsListResponsemaps;
  
  { --------------------------------------------------------------------
    TParent
    --------------------------------------------------------------------}
  
  TParent = Class(TGoogleBaseObject)
  Private
    Fid : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
  end;
  TParentClass = Class of TParent;
  
  { --------------------------------------------------------------------
    TParentsListResponse
    --------------------------------------------------------------------}
  
  TParentsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Fparents : TParentsListResponseparents;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setparents(AIndex : Integer; AValue : TParentsListResponseparents); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property parents : TParentsListResponseparents Index 8 Read Fparents Write Setparents;
  end;
  TParentsListResponseClass = Class of TParentsListResponse;
  
  { --------------------------------------------------------------------
    TParentsListResponseparents
    --------------------------------------------------------------------}
  
  TParentsListResponseparents = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParentsListResponseparentsClass = Class of TParentsListResponseparents;
  
  { --------------------------------------------------------------------
    TPermission
    --------------------------------------------------------------------}
  
  TPermission = Class(TGoogleBaseObject)
  Private
    Fdiscoverable : boolean;
    Fid : string;
    Frole : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdiscoverable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setrole(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property discoverable : boolean Index 0 Read Fdiscoverable Write Setdiscoverable;
    Property id : string Index 8 Read Fid Write Setid;
    Property role : string Index 16 Read Frole Write Setrole;
    Property _type : string Index 24 Read F_type Write Set_type;
  end;
  TPermissionClass = Class of TPermission;
  
  { --------------------------------------------------------------------
    TPermissionsBatchDeleteRequest
    --------------------------------------------------------------------}
  
  TPermissionsBatchDeleteRequest = Class(TGoogleBaseObject)
  Private
    Fids : TPermissionsBatchDeleteRequestids;
  Protected
    //Property setters
    Procedure Setids(AIndex : Integer; AValue : TPermissionsBatchDeleteRequestids); virtual;
  Public
  Published
    Property ids : TPermissionsBatchDeleteRequestids Index 0 Read Fids Write Setids;
  end;
  TPermissionsBatchDeleteRequestClass = Class of TPermissionsBatchDeleteRequest;
  
  { --------------------------------------------------------------------
    TPermissionsBatchDeleteRequestids
    --------------------------------------------------------------------}
  
  TPermissionsBatchDeleteRequestids = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionsBatchDeleteRequestidsClass = Class of TPermissionsBatchDeleteRequestids;
  
  { --------------------------------------------------------------------
    TPermissionsBatchDeleteResponse
    --------------------------------------------------------------------}
  
  TPermissionsBatchDeleteResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionsBatchDeleteResponseClass = Class of TPermissionsBatchDeleteResponse;
  
  { --------------------------------------------------------------------
    TPermissionsBatchUpdateRequest
    --------------------------------------------------------------------}
  
  TPermissionsBatchUpdateRequest = Class(TGoogleBaseObject)
  Private
    Fpermissions : TPermissionsBatchUpdateRequestpermissions;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; AValue : TPermissionsBatchUpdateRequestpermissions); virtual;
  Public
  Published
    Property permissions : TPermissionsBatchUpdateRequestpermissions Index 0 Read Fpermissions Write Setpermissions;
  end;
  TPermissionsBatchUpdateRequestClass = Class of TPermissionsBatchUpdateRequest;
  
  { --------------------------------------------------------------------
    TPermissionsBatchUpdateRequestpermissions
    --------------------------------------------------------------------}
  
  TPermissionsBatchUpdateRequestpermissions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionsBatchUpdateRequestpermissionsClass = Class of TPermissionsBatchUpdateRequestpermissions;
  
  { --------------------------------------------------------------------
    TPermissionsBatchUpdateResponse
    --------------------------------------------------------------------}
  
  TPermissionsBatchUpdateResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionsBatchUpdateResponseClass = Class of TPermissionsBatchUpdateResponse;
  
  { --------------------------------------------------------------------
    TPermissionsListResponse
    --------------------------------------------------------------------}
  
  TPermissionsListResponse = Class(TGoogleBaseObject)
  Private
    Fpermissions : TPermissionsListResponsepermissions;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; AValue : TPermissionsListResponsepermissions); virtual;
  Public
  Published
    Property permissions : TPermissionsListResponsepermissions Index 0 Read Fpermissions Write Setpermissions;
  end;
  TPermissionsListResponseClass = Class of TPermissionsListResponse;
  
  { --------------------------------------------------------------------
    TPermissionsListResponsepermissions
    --------------------------------------------------------------------}
  
  TPermissionsListResponsepermissions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionsListResponsepermissionsClass = Class of TPermissionsListResponsepermissions;
  
  { --------------------------------------------------------------------
    TPointStyle
    --------------------------------------------------------------------}
  
  TPointStyle = Class(TGoogleBaseObject)
  Private
    Ficon : TIconStyle;
    F_label : TLabelStyle;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Seticon(AIndex : Integer; AValue : TIconStyle); virtual;
    Procedure Set_label(AIndex : Integer; AValue : TLabelStyle); virtual;
  Public
  Published
    Property icon : TIconStyle Index 0 Read Ficon Write Seticon;
    Property _label : TLabelStyle Index 8 Read F_label Write Set_label;
  end;
  TPointStyleClass = Class of TPointStyle;
  
  { --------------------------------------------------------------------
    TPolygonStyle
    --------------------------------------------------------------------}
  
  TPolygonStyle = Class(TGoogleBaseObject)
  Private
    Ffill : TColor;
    F_label : TLabelStyle;
    Fstroke : TBorder;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setfill(AIndex : Integer; AValue : TColor); virtual;
    Procedure Set_label(AIndex : Integer; AValue : TLabelStyle); virtual;
    Procedure Setstroke(AIndex : Integer; AValue : TBorder); virtual;
  Public
  Published
    Property fill : TColor Index 0 Read Ffill Write Setfill;
    Property _label : TLabelStyle Index 8 Read F_label Write Set_label;
    Property stroke : TBorder Index 16 Read Fstroke Write Setstroke;
  end;
  TPolygonStyleClass = Class of TPolygonStyle;
  
  { --------------------------------------------------------------------
    TProcessResponse
    --------------------------------------------------------------------}
  
  TProcessResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProcessResponseClass = Class of TProcessResponse;
  
  { --------------------------------------------------------------------
    TProject
    --------------------------------------------------------------------}
  
  TProject = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property name : string Index 8 Read Fname Write Setname;
  end;
  TProjectClass = Class of TProject;
  
  { --------------------------------------------------------------------
    TProjectsListResponse
    --------------------------------------------------------------------}
  
  TProjectsListResponse = Class(TGoogleBaseObject)
  Private
    Fprojects : TProjectsListResponseprojects;
  Protected
    //Property setters
    Procedure Setprojects(AIndex : Integer; AValue : TProjectsListResponseprojects); virtual;
  Public
  Published
    Property projects : TProjectsListResponseprojects Index 0 Read Fprojects Write Setprojects;
  end;
  TProjectsListResponseClass = Class of TProjectsListResponse;
  
  { --------------------------------------------------------------------
    TProjectsListResponseprojects
    --------------------------------------------------------------------}
  
  TProjectsListResponseprojects = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProjectsListResponseprojectsClass = Class of TProjectsListResponseprojects;
  
  { --------------------------------------------------------------------
    TPublishResponse
    --------------------------------------------------------------------}
  
  TPublishResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPublishResponseClass = Class of TPublishResponse;
  
  { --------------------------------------------------------------------
    TPublishedLayer
    --------------------------------------------------------------------}
  
  TPublishedLayer = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fid : string;
    FlayerType : string;
    Fname : string;
    FprojectId : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlayerType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property id : string Index 8 Read Fid Write Setid;
    Property layerType : string Index 16 Read FlayerType Write SetlayerType;
    Property name : string Index 24 Read Fname Write Setname;
    Property projectId : string Index 32 Read FprojectId Write SetprojectId;
  end;
  TPublishedLayerClass = Class of TPublishedLayer;
  
  { --------------------------------------------------------------------
    TPublishedLayersListResponse
    --------------------------------------------------------------------}
  
  TPublishedLayersListResponse = Class(TGoogleBaseObject)
  Private
    Flayers : TPublishedLayersListResponselayers;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setlayers(AIndex : Integer; AValue : TPublishedLayersListResponselayers); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property layers : TPublishedLayersListResponselayers Index 0 Read Flayers Write Setlayers;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TPublishedLayersListResponseClass = Class of TPublishedLayersListResponse;
  
  { --------------------------------------------------------------------
    TPublishedLayersListResponselayers
    --------------------------------------------------------------------}
  
  TPublishedLayersListResponselayers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPublishedLayersListResponselayersClass = Class of TPublishedLayersListResponselayers;
  
  { --------------------------------------------------------------------
    TPublishedMap
    --------------------------------------------------------------------}
  
  TPublishedMap = Class(TGoogleBaseObject)
  Private
    Fcontents : TMapContents;
    FdefaultViewport : TLatLngBox;
    Fdescription : string;
    Fid : string;
    Fname : string;
    FprojectId : string;
  Protected
    //Property setters
    Procedure Setcontents(AIndex : Integer; AValue : TMapContents); virtual;
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TLatLngBox); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property contents : TMapContents Index 0 Read Fcontents Write Setcontents;
    Property defaultViewport : TLatLngBox Index 8 Read FdefaultViewport Write SetdefaultViewport;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property name : string Index 32 Read Fname Write Setname;
    Property projectId : string Index 40 Read FprojectId Write SetprojectId;
  end;
  TPublishedMapClass = Class of TPublishedMap;
  
  { --------------------------------------------------------------------
    TPublishedMapsListResponse
    --------------------------------------------------------------------}
  
  TPublishedMapsListResponse = Class(TGoogleBaseObject)
  Private
    Fmaps : TPublishedMapsListResponsemaps;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setmaps(AIndex : Integer; AValue : TPublishedMapsListResponsemaps); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property maps : TPublishedMapsListResponsemaps Index 0 Read Fmaps Write Setmaps;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TPublishedMapsListResponseClass = Class of TPublishedMapsListResponse;
  
  { --------------------------------------------------------------------
    TPublishedMapsListResponsemaps
    --------------------------------------------------------------------}
  
  TPublishedMapsListResponsemaps = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPublishedMapsListResponsemapsClass = Class of TPublishedMapsListResponsemaps;
  
  { --------------------------------------------------------------------
    TRaster
    --------------------------------------------------------------------}
  
  TRaster = Class(TGoogleBaseObject)
  Private
    FacquisitionTime : TAcquisitionTime;
    Fattribution : string;
    Fbbox : TRasterbbox;
    FcreationTime : TDatetime;
    FcreatorEmail : string;
    Fdescription : string;
    FdraftAccessList : string;
    Fetag : string;
    Ffiles : TRasterfiles;
    Fid : string;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : string;
    FmaskType : string;
    Fname : string;
    FprocessingStatus : string;
    FprojectId : string;
    FrasterType : string;
    Ftags : TAssettags;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure SetacquisitionTime(AIndex : Integer; AValue : TAcquisitionTime); virtual;
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Setbbox(AIndex : Integer; AValue : TRasterbbox); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setfiles(AIndex : Integer; AValue : TRasterfiles); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaskType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetrasterType(AIndex : Integer; AValue : string); virtual;
    Procedure Settags(AIndex : Integer; AValue : TAssettags); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property acquisitionTime : TAcquisitionTime Index 0 Read FacquisitionTime Write SetacquisitionTime;
    Property attribution : string Index 8 Read Fattribution Write Setattribution;
    Property bbox : TRasterbbox Index 16 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 24 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : string Index 32 Read FcreatorEmail Write SetcreatorEmail;
    Property description : string Index 40 Read Fdescription Write Setdescription;
    Property draftAccessList : string Index 48 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : string Index 56 Read Fetag Write Setetag;
    Property files : TRasterfiles Index 64 Read Ffiles Write Setfiles;
    Property id : string Index 72 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 80 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : string Index 88 Read FlastModifierEmail Write SetlastModifierEmail;
    Property maskType : string Index 96 Read FmaskType Write SetmaskType;
    Property name : string Index 104 Read Fname Write Setname;
    Property processingStatus : string Index 112 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : string Index 120 Read FprojectId Write SetprojectId;
    Property rasterType : string Index 128 Read FrasterType Write SetrasterType;
    Property tags : TAssettags Index 136 Read Ftags Write Settags;
    Property writersCanEditPermissions : boolean Index 144 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TRasterClass = Class of TRaster;
  
  { --------------------------------------------------------------------
    TRasterbbox
    --------------------------------------------------------------------}
  
  TRasterbbox = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterbboxClass = Class of TRasterbbox;
  
  { --------------------------------------------------------------------
    TRasterfiles
    --------------------------------------------------------------------}
  
  TRasterfiles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterfilesClass = Class of TRasterfiles;
  
  { --------------------------------------------------------------------
    TRasterCollection
    --------------------------------------------------------------------}
  
  TRasterCollection = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    Fbbox : TRasterCollectionbbox;
    FcreationTime : TDatetime;
    FcreatorEmail : string;
    Fdescription : string;
    FdraftAccessList : string;
    Fetag : string;
    Fid : string;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : string;
    Fmosaic : boolean;
    Fname : string;
    FprocessingStatus : string;
    FprojectId : string;
    FrasterType : string;
    Ftags : TAssettags;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure Setbbox(AIndex : Integer; AValue : TRasterCollectionbbox); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setmosaic(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetrasterType(AIndex : Integer; AValue : string); virtual;
    Procedure Settags(AIndex : Integer; AValue : TAssettags); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property bbox : TRasterCollectionbbox Index 8 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 16 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : string Index 24 Read FcreatorEmail Write SetcreatorEmail;
    Property description : string Index 32 Read Fdescription Write Setdescription;
    Property draftAccessList : string Index 40 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : string Index 48 Read Fetag Write Setetag;
    Property id : string Index 56 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 64 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : string Index 72 Read FlastModifierEmail Write SetlastModifierEmail;
    Property mosaic : boolean Index 80 Read Fmosaic Write Setmosaic;
    Property name : string Index 88 Read Fname Write Setname;
    Property processingStatus : string Index 96 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : string Index 104 Read FprojectId Write SetprojectId;
    Property rasterType : string Index 112 Read FrasterType Write SetrasterType;
    Property tags : TAssettags Index 120 Read Ftags Write Settags;
    Property writersCanEditPermissions : boolean Index 128 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TRasterCollectionClass = Class of TRasterCollection;
  
  { --------------------------------------------------------------------
    TRasterCollectionbbox
    --------------------------------------------------------------------}
  
  TRasterCollectionbbox = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionbboxClass = Class of TRasterCollectionbbox;
  
  { --------------------------------------------------------------------
    TRasterCollectionsListResponse
    --------------------------------------------------------------------}
  
  TRasterCollectionsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    FrasterCollections : TRasterCollectionsListResponserasterCollections;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetrasterCollections(AIndex : Integer; AValue : TRasterCollectionsListResponserasterCollections); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property rasterCollections : TRasterCollectionsListResponserasterCollections Index 8 Read FrasterCollections Write SetrasterCollections;
  end;
  TRasterCollectionsListResponseClass = Class of TRasterCollectionsListResponse;
  
  { --------------------------------------------------------------------
    TRasterCollectionsListResponserasterCollections
    --------------------------------------------------------------------}
  
  TRasterCollectionsListResponserasterCollections = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionsListResponserasterCollectionsClass = Class of TRasterCollectionsListResponserasterCollections;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRaster
    --------------------------------------------------------------------}
  
  TRasterCollectionsRaster = Class(TGoogleBaseObject)
  Private
    Fbbox : TRasterCollectionsRasterbbox;
    FcreationTime : TDatetime;
    Fdescription : string;
    Fid : string;
    FlastModifiedTime : TDatetime;
    Fname : string;
    FprojectId : string;
    FrasterType : string;
    Ftags : TRasterCollectionsRastertags;
  Protected
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TRasterCollectionsRasterbbox); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetrasterType(AIndex : Integer; AValue : string); virtual;
    Procedure Settags(AIndex : Integer; AValue : TRasterCollectionsRastertags); virtual;
  Public
  Published
    Property bbox : TRasterCollectionsRasterbbox Index 0 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 8 Read FcreationTime Write SetcreationTime;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 32 Read FlastModifiedTime Write SetlastModifiedTime;
    Property name : string Index 40 Read Fname Write Setname;
    Property projectId : string Index 48 Read FprojectId Write SetprojectId;
    Property rasterType : string Index 56 Read FrasterType Write SetrasterType;
    Property tags : TRasterCollectionsRastertags Index 64 Read Ftags Write Settags;
  end;
  TRasterCollectionsRasterClass = Class of TRasterCollectionsRaster;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRasterbbox
    --------------------------------------------------------------------}
  
  TRasterCollectionsRasterbbox = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionsRasterbboxClass = Class of TRasterCollectionsRasterbbox;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRastertags
    --------------------------------------------------------------------}
  
  TRasterCollectionsRastertags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionsRastertagsClass = Class of TRasterCollectionsRastertags;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRasterBatchDeleteRequest
    --------------------------------------------------------------------}
  
  TRasterCollectionsRasterBatchDeleteRequest = Class(TGoogleBaseObject)
  Private
    Fids : TRasterCollectionsRasterBatchDeleteRequestids;
  Protected
    //Property setters
    Procedure Setids(AIndex : Integer; AValue : TRasterCollectionsRasterBatchDeleteRequestids); virtual;
  Public
  Published
    Property ids : TRasterCollectionsRasterBatchDeleteRequestids Index 0 Read Fids Write Setids;
  end;
  TRasterCollectionsRasterBatchDeleteRequestClass = Class of TRasterCollectionsRasterBatchDeleteRequest;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRasterBatchDeleteRequestids
    --------------------------------------------------------------------}
  
  TRasterCollectionsRasterBatchDeleteRequestids = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionsRasterBatchDeleteRequestidsClass = Class of TRasterCollectionsRasterBatchDeleteRequestids;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRastersBatchDeleteResponse
    --------------------------------------------------------------------}
  
  TRasterCollectionsRastersBatchDeleteResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionsRastersBatchDeleteResponseClass = Class of TRasterCollectionsRastersBatchDeleteResponse;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRastersBatchInsertRequest
    --------------------------------------------------------------------}
  
  TRasterCollectionsRastersBatchInsertRequest = Class(TGoogleBaseObject)
  Private
    Fids : TRasterCollectionsRastersBatchInsertRequestids;
  Protected
    //Property setters
    Procedure Setids(AIndex : Integer; AValue : TRasterCollectionsRastersBatchInsertRequestids); virtual;
  Public
  Published
    Property ids : TRasterCollectionsRastersBatchInsertRequestids Index 0 Read Fids Write Setids;
  end;
  TRasterCollectionsRastersBatchInsertRequestClass = Class of TRasterCollectionsRastersBatchInsertRequest;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRastersBatchInsertRequestids
    --------------------------------------------------------------------}
  
  TRasterCollectionsRastersBatchInsertRequestids = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionsRastersBatchInsertRequestidsClass = Class of TRasterCollectionsRastersBatchInsertRequestids;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRastersBatchInsertResponse
    --------------------------------------------------------------------}
  
  TRasterCollectionsRastersBatchInsertResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionsRastersBatchInsertResponseClass = Class of TRasterCollectionsRastersBatchInsertResponse;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRastersListResponse
    --------------------------------------------------------------------}
  
  TRasterCollectionsRastersListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Frasters : TRasterCollectionsRastersListResponserasters;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setrasters(AIndex : Integer; AValue : TRasterCollectionsRastersListResponserasters); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property rasters : TRasterCollectionsRastersListResponserasters Index 8 Read Frasters Write Setrasters;
  end;
  TRasterCollectionsRastersListResponseClass = Class of TRasterCollectionsRastersListResponse;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRastersListResponserasters
    --------------------------------------------------------------------}
  
  TRasterCollectionsRastersListResponserasters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRasterCollectionsRastersListResponserastersClass = Class of TRasterCollectionsRastersListResponserasters;
  
  { --------------------------------------------------------------------
    TRastersListResponse
    --------------------------------------------------------------------}
  
  TRastersListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Frasters : TRastersListResponserasters;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setrasters(AIndex : Integer; AValue : TRastersListResponserasters); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property rasters : TRastersListResponserasters Index 8 Read Frasters Write Setrasters;
  end;
  TRastersListResponseClass = Class of TRastersListResponse;
  
  { --------------------------------------------------------------------
    TRastersListResponserasters
    --------------------------------------------------------------------}
  
  TRastersListResponserasters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRastersListResponserastersClass = Class of TRastersListResponserasters;
  
  { --------------------------------------------------------------------
    TScaledShape
    --------------------------------------------------------------------}
  
  TScaledShape = Class(TGoogleBaseObject)
  Private
    Fborder : TBorder;
    Ffill : TColor;
    Fshape : string;
  Protected
    //Property setters
    Procedure Setborder(AIndex : Integer; AValue : TBorder); virtual;
    Procedure Setfill(AIndex : Integer; AValue : TColor); virtual;
    Procedure Setshape(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property border : TBorder Index 0 Read Fborder Write Setborder;
    Property fill : TColor Index 8 Read Ffill Write Setfill;
    Property shape : string Index 16 Read Fshape Write Setshape;
  end;
  TScaledShapeClass = Class of TScaledShape;
  
  { --------------------------------------------------------------------
    TScalingFunction
    --------------------------------------------------------------------}
  
  TScalingFunction = Class(TGoogleBaseObject)
  Private
    Fcolumn : string;
    FscalingType : string;
    FsizeRange : TSizeRange;
    FvalueRange : TValueRange;
  Protected
    //Property setters
    Procedure Setcolumn(AIndex : Integer; AValue : string); virtual;
    Procedure SetscalingType(AIndex : Integer; AValue : string); virtual;
    Procedure SetsizeRange(AIndex : Integer; AValue : TSizeRange); virtual;
    Procedure SetvalueRange(AIndex : Integer; AValue : TValueRange); virtual;
  Public
  Published
    Property column : string Index 0 Read Fcolumn Write Setcolumn;
    Property scalingType : string Index 8 Read FscalingType Write SetscalingType;
    Property sizeRange : TSizeRange Index 16 Read FsizeRange Write SetsizeRange;
    Property valueRange : TValueRange Index 24 Read FvalueRange Write SetvalueRange;
  end;
  TScalingFunctionClass = Class of TScalingFunction;
  
  { --------------------------------------------------------------------
    TSchema
    --------------------------------------------------------------------}
  
  TSchema = Class(TGoogleBaseObject)
  Private
    Fcolumns : TSchemacolumns;
    FprimaryGeometry : string;
    FprimaryKey : string;
  Protected
    //Property setters
    Procedure Setcolumns(AIndex : Integer; AValue : TSchemacolumns); virtual;
    Procedure SetprimaryGeometry(AIndex : Integer; AValue : string); virtual;
    Procedure SetprimaryKey(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property columns : TSchemacolumns Index 0 Read Fcolumns Write Setcolumns;
    Property primaryGeometry : string Index 8 Read FprimaryGeometry Write SetprimaryGeometry;
    Property primaryKey : string Index 16 Read FprimaryKey Write SetprimaryKey;
  end;
  TSchemaClass = Class of TSchema;
  
  { --------------------------------------------------------------------
    TSchemacolumns
    --------------------------------------------------------------------}
  
  TSchemacolumns = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSchemacolumnsClass = Class of TSchemacolumns;
  
  { --------------------------------------------------------------------
    TSizeRange
    --------------------------------------------------------------------}
  
  TSizeRange = Class(TGoogleBaseObject)
  Private
    Fmax : double;
    Fmin : double;
  Protected
    //Property setters
    Procedure Setmax(AIndex : Integer; AValue : double); virtual;
    Procedure Setmin(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property max : double Index 0 Read Fmax Write Setmax;
    Property min : double Index 8 Read Fmin Write Setmin;
  end;
  TSizeRangeClass = Class of TSizeRange;
  
  { --------------------------------------------------------------------
    TTable
    --------------------------------------------------------------------}
  
  TTable = Class(TGoogleBaseObject)
  Private
    Fbbox : TTablebbox;
    FcreationTime : TDatetime;
    FcreatorEmail : string;
    Fdescription : string;
    FdraftAccessList : string;
    Fetag : string;
    Ffiles : TTablefiles;
    Fid : string;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : string;
    Fname : string;
    FprocessingStatus : string;
    FprojectId : string;
    FpublishedAccessList : string;
    Fschema : TSchema;
    FsourceEncoding : string;
    Ftags : TAssettags;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TTablebbox); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setfiles(AIndex : Integer; AValue : TTablefiles); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublishedAccessList(AIndex : Integer; AValue : string); virtual;
    Procedure Setschema(AIndex : Integer; AValue : TSchema); virtual;
    Procedure SetsourceEncoding(AIndex : Integer; AValue : string); virtual;
    Procedure Settags(AIndex : Integer; AValue : TAssettags); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bbox : TTablebbox Index 0 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 8 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : string Index 16 Read FcreatorEmail Write SetcreatorEmail;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property draftAccessList : string Index 32 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : string Index 40 Read Fetag Write Setetag;
    Property files : TTablefiles Index 48 Read Ffiles Write Setfiles;
    Property id : string Index 56 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 64 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : string Index 72 Read FlastModifierEmail Write SetlastModifierEmail;
    Property name : string Index 80 Read Fname Write Setname;
    Property processingStatus : string Index 88 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : string Index 96 Read FprojectId Write SetprojectId;
    Property publishedAccessList : string Index 104 Read FpublishedAccessList Write SetpublishedAccessList;
    Property schema : TSchema Index 112 Read Fschema Write Setschema;
    Property sourceEncoding : string Index 120 Read FsourceEncoding Write SetsourceEncoding;
    Property tags : TAssettags Index 128 Read Ftags Write Settags;
    Property writersCanEditPermissions : boolean Index 136 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TTableClass = Class of TTable;
  
  { --------------------------------------------------------------------
    TTablebbox
    --------------------------------------------------------------------}
  
  TTablebbox = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTablebboxClass = Class of TTablebbox;
  
  { --------------------------------------------------------------------
    TTablefiles
    --------------------------------------------------------------------}
  
  TTablefiles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTablefilesClass = Class of TTablefiles;
  
  { --------------------------------------------------------------------
    TTableColumn
    --------------------------------------------------------------------}
  
  TTableColumn = Class(TGoogleBaseObject)
  Private
    Fname : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TTableColumnClass = Class of TTableColumn;
  
  { --------------------------------------------------------------------
    TTablesListResponse
    --------------------------------------------------------------------}
  
  TTablesListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Ftables : TTablesListResponsetables;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Settables(AIndex : Integer; AValue : TTablesListResponsetables); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property tables : TTablesListResponsetables Index 8 Read Ftables Write Settables;
  end;
  TTablesListResponseClass = Class of TTablesListResponse;
  
  { --------------------------------------------------------------------
    TTablesListResponsetables
    --------------------------------------------------------------------}
  
  TTablesListResponsetables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTablesListResponsetablesClass = Class of TTablesListResponsetables;
  
  { --------------------------------------------------------------------
    TTags
    --------------------------------------------------------------------}
  
  TTags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTagsClass = Class of TTags;
  
  { --------------------------------------------------------------------
    TValueRange
    --------------------------------------------------------------------}
  
  TValueRange = Class(TGoogleBaseObject)
  Private
    Fmax : double;
    Fmin : double;
  Protected
    //Property setters
    Procedure Setmax(AIndex : Integer; AValue : double); virtual;
    Procedure Setmin(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property max : double Index 0 Read Fmax Write Setmax;
    Property min : double Index 8 Read Fmin Write Setmin;
  end;
  TValueRangeClass = Class of TValueRange;
  
  { --------------------------------------------------------------------
    TVectorStyle
    --------------------------------------------------------------------}
  
  TVectorStyle = Class(TGoogleBaseObject)
  Private
    FdisplayRules : TVectorStyledisplayRules;
    FfeatureInfo : TFeatureInfo;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdisplayRules(AIndex : Integer; AValue : TVectorStyledisplayRules); virtual;
    Procedure SetfeatureInfo(AIndex : Integer; AValue : TFeatureInfo); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayRules : TVectorStyledisplayRules Index 0 Read FdisplayRules Write SetdisplayRules;
    Property featureInfo : TFeatureInfo Index 8 Read FfeatureInfo Write SetfeatureInfo;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TVectorStyleClass = Class of TVectorStyle;
  
  { --------------------------------------------------------------------
    TVectorStyledisplayRules
    --------------------------------------------------------------------}
  
  TVectorStyledisplayRules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVectorStyledisplayRulesClass = Class of TVectorStyledisplayRules;
  
  { --------------------------------------------------------------------
    TZoomLevels
    --------------------------------------------------------------------}
  
  TZoomLevels = Class(TGoogleBaseObject)
  Private
    Fmax : integer;
    Fmin : integer;
  Protected
    //Property setters
    Procedure Setmax(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmin(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property max : integer Index 0 Read Fmax Write Setmax;
    Property min : integer Index 8 Read Fmin Write Setmin;
  end;
  TZoomLevelsClass = Class of TZoomLevels;
  
  { --------------------------------------------------------------------
    TAssetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAssetsResource, method List
  
  TAssetsListOptions = Record
    bbox : string;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : string;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : string;
    projectId : string;
    role : string;
    search : string;
    tags : string;
    _type : string;
  end;
  
  TAssetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string) : TAsset;
    Function List(AQuery : string  = '') : TAssetsListResponse;
    Function List(AQuery : TAssetslistOptions) : TAssetsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TLayersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLayersResource, method Create
  
  TLayersCreateOptions = Record
    process : boolean;
  end;
  
  
  //Optional query Options for TLayersResource, method Get
  
  TLayersGetOptions = Record
    version : string;
  end;
  
  
  //Optional query Options for TLayersResource, method List
  
  TLayersListOptions = Record
    bbox : string;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : string;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : string;
    processingStatus : string;
    projectId : string;
    role : string;
    search : string;
    tags : string;
  end;
  
  
  //Optional query Options for TLayersResource, method ListPublished
  
  TLayersListPublishedOptions = Record
    maxResults : integer;
    pageToken : string;
    projectId : string;
  end;
  
  
  //Optional query Options for TLayersResource, method Publish
  
  TLayersPublishOptions = Record
    force : boolean;
  end;
  
  TLayersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CancelProcessing(id: string) : TProcessResponse;
    Function Create(aLayer : TLayer; AQuery : string  = '') : TLayer;overload;
    Function Create(aLayer : TLayer; AQuery : TLayerscreateOptions) : TLayer;overload;
    Procedure Delete(id: string);
    Function Get(id: string; AQuery : string  = '') : TLayer;
    Function Get(id: string; AQuery : TLayersgetOptions) : TLayer;
    Function GetPublished(id: string) : TPublishedLayer;
    Function List(AQuery : string  = '') : TLayersListResponse;
    Function List(AQuery : TLayerslistOptions) : TLayersListResponse;
    Function ListPublished(AQuery : string  = '') : TPublishedLayersListResponse;
    Function ListPublished(AQuery : TLayerslistPublishedOptions) : TPublishedLayersListResponse;
    Procedure Patch(id: string; aLayer : TLayer);
    Function Process(id: string) : TProcessResponse;
    Function Publish(id: string; AQuery : string  = '') : TPublishResponse;
    Function Publish(id: string; AQuery : TLayerspublishOptions) : TPublishResponse;
    Function Unpublish(id: string) : TPublishResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TMapsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMapsResource, method Get
  
  TMapsGetOptions = Record
    version : string;
  end;
  
  
  //Optional query Options for TMapsResource, method List
  
  TMapsListOptions = Record
    bbox : string;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : string;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : string;
    processingStatus : string;
    projectId : string;
    role : string;
    search : string;
    tags : string;
  end;
  
  
  //Optional query Options for TMapsResource, method ListPublished
  
  TMapsListPublishedOptions = Record
    maxResults : integer;
    pageToken : string;
    projectId : string;
  end;
  
  
  //Optional query Options for TMapsResource, method Publish
  
  TMapsPublishOptions = Record
    force : boolean;
  end;
  
  TMapsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aMap : TMap) : TMap;overload;
    Procedure Delete(id: string);
    Function Get(id: string; AQuery : string  = '') : TMap;
    Function Get(id: string; AQuery : TMapsgetOptions) : TMap;
    Function GetPublished(id: string) : TPublishedMap;
    Function List(AQuery : string  = '') : TMapsListResponse;
    Function List(AQuery : TMapslistOptions) : TMapsListResponse;
    Function ListPublished(AQuery : string  = '') : TPublishedMapsListResponse;
    Function ListPublished(AQuery : TMapslistPublishedOptions) : TPublishedMapsListResponse;
    Procedure Patch(id: string; aMap : TMap);
    Function Publish(id: string; AQuery : string  = '') : TPublishResponse;
    Function Publish(id: string; AQuery : TMapspublishOptions) : TPublishResponse;
    Function Unpublish(id: string) : TPublishResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List : TProjectsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TRasterCollectionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRasterCollectionsResource, method List
  
  TRasterCollectionsListOptions = Record
    bbox : string;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : string;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : string;
    processingStatus : string;
    projectId : string;
    role : string;
    search : string;
    tags : string;
  end;
  
  TRasterCollectionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CancelProcessing(id: string) : TProcessResponse;
    Function Create(aRasterCollection : TRasterCollection) : TRasterCollection;overload;
    Procedure Delete(id: string);
    Function Get(id: string) : TRasterCollection;
    Function List(AQuery : string  = '') : TRasterCollectionsListResponse;
    Function List(AQuery : TRasterCollectionslistOptions) : TRasterCollectionsListResponse;
    Procedure Patch(id: string; aRasterCollection : TRasterCollection);
    Function Process(id: string) : TProcessResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TRastersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRastersResource, method List
  
  TRastersListOptions = Record
    bbox : string;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : string;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : string;
    processingStatus : string;
    projectId : string;
    role : string;
    search : string;
    tags : string;
  end;
  
  TRastersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string);
    Function Get(id: string) : TRaster;
    Function List(AQuery : string  = '') : TRastersListResponse;
    Function List(AQuery : TRasterslistOptions) : TRastersListResponse;
    Procedure Patch(id: string; aRaster : TRaster);
    Function Process(id: string) : TProcessResponse;
    Function Upload(aRaster : TRaster) : TRaster;
  end;
  
  
  { --------------------------------------------------------------------
    TTablesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTablesResource, method Get
  
  TTablesGetOptions = Record
    version : string;
  end;
  
  
  //Optional query Options for TTablesResource, method List
  
  TTablesListOptions = Record
    bbox : string;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : string;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : string;
    processingStatus : string;
    projectId : string;
    role : string;
    search : string;
    tags : string;
  end;
  
  TTablesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aTable : TTable) : TTable;overload;
    Procedure Delete(id: string);
    Function Get(id: string; AQuery : string  = '') : TTable;
    Function Get(id: string; AQuery : TTablesgetOptions) : TTable;
    Function List(AQuery : string  = '') : TTablesListResponse;
    Function List(AQuery : TTableslistOptions) : TTablesListResponse;
    Procedure Patch(id: string; aTable : TTable);
    Function Process(id: string) : TProcessResponse;
    Function Upload(aTable : TTable) : TTable;
  end;
  
  
  { --------------------------------------------------------------------
    TMapsengineAPI
    --------------------------------------------------------------------}
  
  TMapsengineAPI = Class(TGoogleAPI)
  Private
    FAssetsInstance : TAssetsResource;
    FLayersInstance : TLayersResource;
    FMapsInstance : TMapsResource;
    FProjectsInstance : TProjectsResource;
    FRasterCollectionsInstance : TRasterCollectionsResource;
    FRastersInstance : TRastersResource;
    FTablesInstance : TTablesResource;
    Function GetAssetsInstance : TAssetsResource;virtual;
    Function GetLayersInstance : TLayersResource;virtual;
    Function GetMapsInstance : TMapsResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
    Function GetRasterCollectionsInstance : TRasterCollectionsResource;virtual;
    Function GetRastersInstance : TRastersResource;virtual;
    Function GetTablesInstance : TTablesResource;virtual;
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
    Function CreateAssetsResource(AOwner : TComponent) : TAssetsResource;virtual;overload;
    Function CreateAssetsResource : TAssetsResource;virtual;overload;
    Function CreateLayersResource(AOwner : TComponent) : TLayersResource;virtual;overload;
    Function CreateLayersResource : TLayersResource;virtual;overload;
    Function CreateMapsResource(AOwner : TComponent) : TMapsResource;virtual;overload;
    Function CreateMapsResource : TMapsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    Function CreateRasterCollectionsResource(AOwner : TComponent) : TRasterCollectionsResource;virtual;overload;
    Function CreateRasterCollectionsResource : TRasterCollectionsResource;virtual;overload;
    Function CreateRastersResource(AOwner : TComponent) : TRastersResource;virtual;overload;
    Function CreateRastersResource : TRastersResource;virtual;overload;
    Function CreateTablesResource(AOwner : TComponent) : TTablesResource;virtual;overload;
    Function CreateTablesResource : TTablesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AssetsResource : TAssetsResource Read GetAssetsInstance;
    Property LayersResource : TLayersResource Read GetLayersInstance;
    Property MapsResource : TMapsResource Read GetMapsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
    Property RasterCollectionsResource : TRasterCollectionsResource Read GetRasterCollectionsInstance;
    Property RastersResource : TRastersResource Read GetRastersInstance;
    Property TablesResource : TTablesResource Read GetTablesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAcquisitionTime
  --------------------------------------------------------------------}


Procedure TAcquisitionTime.Set_end(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcquisitionTime.Setprecision(AIndex : Integer; AValue : string); 

begin
  If (Fprecision=AValue) then exit;
  Fprecision:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcquisitionTime.Setstart(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAcquisitionTime.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAsset
  --------------------------------------------------------------------}


Procedure TAsset.Setbbox(AIndex : Integer; AValue : TAssetbbox); 

begin
  If (Fbbox=AValue) then exit;
  Fbbox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.SetcreationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.SetcreatorEmail(AIndex : Integer; AValue : string); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.SetlastModifierEmail(AIndex : Integer; AValue : string); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setresource(AIndex : Integer; AValue : string); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Settags(AIndex : Integer; AValue : TAssettags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); 

begin
  If (FwritersCanEditPermissions=AValue) then exit;
  FwritersCanEditPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAsset.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAssetbbox
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAssettags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAssetsListResponse
  --------------------------------------------------------------------}


Procedure TAssetsListResponse.Setassets(AIndex : Integer; AValue : TAssetsListResponseassets); 

begin
  If (Fassets=AValue) then exit;
  Fassets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssetsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAssetsListResponseassets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBorder
  --------------------------------------------------------------------}


Procedure TBorder.Setcolor(AIndex : Integer; AValue : string); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBorder.Setopacity(AIndex : Integer; AValue : double); 

begin
  If (Fopacity=AValue) then exit;
  Fopacity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBorder.Setwidth(AIndex : Integer; AValue : double); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColor
  --------------------------------------------------------------------}


Procedure TColor.Setcolor(AIndex : Integer; AValue : string); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColor.Setopacity(AIndex : Integer; AValue : double); 

begin
  If (Fopacity=AValue) then exit;
  Fopacity:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatasource
  --------------------------------------------------------------------}


Procedure TDatasource.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatasources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDisplayRule
  --------------------------------------------------------------------}


Procedure TDisplayRule.Setfilters(AIndex : Integer; AValue : TDisplayRulefilters); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisplayRule.SetlineOptions(AIndex : Integer; AValue : TLineStyle); 

begin
  If (FlineOptions=AValue) then exit;
  FlineOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisplayRule.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisplayRule.SetpointOptions(AIndex : Integer; AValue : TPointStyle); 

begin
  If (FpointOptions=AValue) then exit;
  FpointOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisplayRule.SetpolygonOptions(AIndex : Integer; AValue : TPolygonStyle); 

begin
  If (FpolygonOptions=AValue) then exit;
  FpolygonOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisplayRule.SetzoomLevels(AIndex : Integer; AValue : TZoomLevels); 

begin
  If (FzoomLevels=AValue) then exit;
  FzoomLevels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDisplayRulefilters
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFeature
  --------------------------------------------------------------------}


Procedure TFeature.Setgeometry(AIndex : Integer; AValue : TGeoJsonGeometry); 

begin
  If (Fgeometry=AValue) then exit;
  Fgeometry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeature.Setproperties(AIndex : Integer; AValue : TGeoJsonProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeature.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFeature.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFeatureInfo
  --------------------------------------------------------------------}


Procedure TFeatureInfo.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFeaturesBatchDeleteRequest
  --------------------------------------------------------------------}


Procedure TFeaturesBatchDeleteRequest.Setgx_ids(AIndex : Integer; AValue : TFeaturesBatchDeleteRequestgx_ids); 

begin
  If (Fgx_ids=AValue) then exit;
  Fgx_ids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesBatchDeleteRequest.SetprimaryKeys(AIndex : Integer; AValue : TFeaturesBatchDeleteRequestprimaryKeys); 

begin
  If (FprimaryKeys=AValue) then exit;
  FprimaryKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFeaturesBatchDeleteRequestgx_ids
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFeaturesBatchDeleteRequestprimaryKeys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFeaturesBatchInsertRequest
  --------------------------------------------------------------------}


Procedure TFeaturesBatchInsertRequest.Setfeatures(AIndex : Integer; AValue : TFeaturesBatchInsertRequestfeatures); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesBatchInsertRequest.SetnormalizeGeometries(AIndex : Integer; AValue : boolean); 

begin
  If (FnormalizeGeometries=AValue) then exit;
  FnormalizeGeometries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFeaturesBatchInsertRequestfeatures
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFeaturesBatchPatchRequest
  --------------------------------------------------------------------}


Procedure TFeaturesBatchPatchRequest.Setfeatures(AIndex : Integer; AValue : TFeaturesBatchPatchRequestfeatures); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesBatchPatchRequest.SetnormalizeGeometries(AIndex : Integer; AValue : boolean); 

begin
  If (FnormalizeGeometries=AValue) then exit;
  FnormalizeGeometries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFeaturesBatchPatchRequestfeatures
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFeaturesListResponse
  --------------------------------------------------------------------}


Procedure TFeaturesListResponse.SetallowedQueriesPerSecond(AIndex : Integer; AValue : double); 

begin
  If (FallowedQueriesPerSecond=AValue) then exit;
  FallowedQueriesPerSecond:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesListResponse.Setfeatures(AIndex : Integer; AValue : TFeaturesListResponsefeatures); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesListResponse.Setschema(AIndex : Integer; AValue : TSchema); 

begin
  If (Fschema=AValue) then exit;
  Fschema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesListResponse.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFeaturesListResponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFeaturesListResponsefeatures
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFile
  --------------------------------------------------------------------}


Procedure TFile.Setfilename(AIndex : Integer; AValue : string); 

begin
  If (Ffilename=AValue) then exit;
  Ffilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setsize(AIndex : Integer; AValue : string); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetuploadStatus(AIndex : Integer; AValue : string); 

begin
  If (FuploadStatus=AValue) then exit;
  FuploadStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilter
  --------------------------------------------------------------------}


Procedure TFilter.Setcolumn(AIndex : Integer; AValue : string); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Set_operator(AIndex : Integer; AValue : string); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Setvalue(AIndex : Integer; AValue : TJSONSchema); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoJsonGeometry
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeoJsonGeometryCollection
  --------------------------------------------------------------------}


Procedure TGeoJsonGeometryCollection.Setgeometries(AIndex : Integer; AValue : TGeoJsonGeometryCollectiongeometries); 

begin
  If (Fgeometries=AValue) then exit;
  Fgeometries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonGeometryCollection.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGeoJsonGeometryCollection.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoJsonGeometryCollectiongeometries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeoJsonLineString
  --------------------------------------------------------------------}


Procedure TGeoJsonLineString.Setcoordinates(AIndex : Integer; AValue : TGeoJsonLineStringcoordinates); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonLineString.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGeoJsonLineString.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoJsonLineStringcoordinates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeoJsonMultiLineString
  --------------------------------------------------------------------}


Procedure TGeoJsonMultiLineString.Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiLineStringcoordinates); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonMultiLineString.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGeoJsonMultiLineString.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoJsonMultiLineStringcoordinates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeoJsonMultiPoint
  --------------------------------------------------------------------}


Procedure TGeoJsonMultiPoint.Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiPointcoordinates); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonMultiPoint.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGeoJsonMultiPoint.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoJsonMultiPointcoordinates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeoJsonMultiPolygon
  --------------------------------------------------------------------}


Procedure TGeoJsonMultiPolygon.Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiPolygoncoordinates); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonMultiPolygon.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGeoJsonMultiPolygon.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoJsonMultiPolygoncoordinates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeoJsonPoint
  --------------------------------------------------------------------}


Procedure TGeoJsonPoint.Setcoordinates(AIndex : Integer; AValue : TGeoJsonPosition); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonPoint.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGeoJsonPoint.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoJsonPolygon
  --------------------------------------------------------------------}


Procedure TGeoJsonPolygon.Setcoordinates(AIndex : Integer; AValue : TGeoJsonPolygoncoordinates); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonPolygon.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGeoJsonPolygon.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGeoJsonPolygoncoordinates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeoJsonPosition
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeoJsonProperties
  --------------------------------------------------------------------}


Class Function TGeoJsonProperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TIcon
  --------------------------------------------------------------------}


Procedure TIcon.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIcon.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIcon.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIconStyle
  --------------------------------------------------------------------}


Procedure TIconStyle.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIconStyle.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIconStyle.SetscaledShape(AIndex : Integer; AValue : TScaledShape); 

begin
  If (FscaledShape=AValue) then exit;
  FscaledShape:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIconStyle.SetscalingFunction(AIndex : Integer; AValue : TScalingFunction); 

begin
  If (FscalingFunction=AValue) then exit;
  FscalingFunction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIconsListResponse
  --------------------------------------------------------------------}


Procedure TIconsListResponse.Seticons(AIndex : Integer; AValue : TIconsListResponseicons); 

begin
  If (Ficons=AValue) then exit;
  Ficons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIconsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIconsListResponseicons
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLabelStyle
  --------------------------------------------------------------------}


Procedure TLabelStyle.Setcolor(AIndex : Integer; AValue : string); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.Setcolumn(AIndex : Integer; AValue : string); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.SetfontStyle(AIndex : Integer; AValue : string); 

begin
  If (FfontStyle=AValue) then exit;
  FfontStyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.SetfontWeight(AIndex : Integer; AValue : string); 

begin
  If (FfontWeight=AValue) then exit;
  FfontWeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.Setopacity(AIndex : Integer; AValue : double); 

begin
  If (Fopacity=AValue) then exit;
  Fopacity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.Setoutline(AIndex : Integer; AValue : TColor); 

begin
  If (Foutline=AValue) then exit;
  Foutline:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.Setsize(AIndex : Integer; AValue : double); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLatLngBox
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLayer
  --------------------------------------------------------------------}


Procedure TLayer.Setbbox(AIndex : Integer; AValue : TLayerbbox); 

begin
  If (Fbbox=AValue) then exit;
  Fbbox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetcreationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetcreatorEmail(AIndex : Integer; AValue : string); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetdatasourceType(AIndex : Integer; AValue : string); 

begin
  If (FdatasourceType=AValue) then exit;
  FdatasourceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setdatasources(AIndex : Integer; AValue : TDatasources); 

begin
  If (Fdatasources=AValue) then exit;
  Fdatasources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetdraftAccessList(AIndex : Integer; AValue : string); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetlastModifierEmail(AIndex : Integer; AValue : string); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetlayerType(AIndex : Integer; AValue : string); 

begin
  If (FlayerType=AValue) then exit;
  FlayerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetprocessingStatus(AIndex : Integer; AValue : string); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetpublishedAccessList(AIndex : Integer; AValue : string); 

begin
  If (FpublishedAccessList=AValue) then exit;
  FpublishedAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetpublishingStatus(AIndex : Integer; AValue : string); 

begin
  If (FpublishingStatus=AValue) then exit;
  FpublishingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setstyle(AIndex : Integer; AValue : TVectorStyle); 

begin
  If (Fstyle=AValue) then exit;
  Fstyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Settags(AIndex : Integer; AValue : TAssettags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); 

begin
  If (FwritersCanEditPermissions=AValue) then exit;
  FwritersCanEditPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLayerbbox
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLayersListResponse
  --------------------------------------------------------------------}


Procedure TLayersListResponse.Setlayers(AIndex : Integer; AValue : TLayersListResponselayers); 

begin
  If (Flayers=AValue) then exit;
  Flayers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLayersListResponselayers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLineStyle
  --------------------------------------------------------------------}


Procedure TLineStyle.Setborder(AIndex : Integer; AValue : TBorder); 

begin
  If (Fborder=AValue) then exit;
  Fborder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyle.Setdash(AIndex : Integer; AValue : TLineStyledash); 

begin
  If (Fdash=AValue) then exit;
  Fdash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyle.Set_label(AIndex : Integer; AValue : TLabelStyle); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyle.Setstroke(AIndex : Integer; AValue : TLineStylestroke); 

begin
  If (Fstroke=AValue) then exit;
  Fstroke:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TLineStyle.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TLineStyledash
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLineStylestroke
  --------------------------------------------------------------------}


Procedure TLineStylestroke.Setcolor(AIndex : Integer; AValue : string); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStylestroke.Setopacity(AIndex : Integer; AValue : double); 

begin
  If (Fopacity=AValue) then exit;
  Fopacity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStylestroke.Setwidth(AIndex : Integer; AValue : double); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMap
  --------------------------------------------------------------------}


Procedure TMap.Setbbox(AIndex : Integer; AValue : TMapbbox); 

begin
  If (Fbbox=AValue) then exit;
  Fbbox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setcontents(AIndex : Integer; AValue : TMapContents); 

begin
  If (Fcontents=AValue) then exit;
  Fcontents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetcreationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetcreatorEmail(AIndex : Integer; AValue : string); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetdefaultViewport(AIndex : Integer; AValue : TLatLngBox); 

begin
  If (FdefaultViewport=AValue) then exit;
  FdefaultViewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetdraftAccessList(AIndex : Integer; AValue : string); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetlastModifierEmail(AIndex : Integer; AValue : string); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetprocessingStatus(AIndex : Integer; AValue : string); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetpublishedAccessList(AIndex : Integer; AValue : string); 

begin
  If (FpublishedAccessList=AValue) then exit;
  FpublishedAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetpublishingStatus(AIndex : Integer; AValue : string); 

begin
  If (FpublishingStatus=AValue) then exit;
  FpublishingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Settags(AIndex : Integer; AValue : TAssettags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setversions(AIndex : Integer; AValue : TMapversions); 

begin
  If (Fversions=AValue) then exit;
  Fversions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); 

begin
  If (FwritersCanEditPermissions=AValue) then exit;
  FwritersCanEditPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMapbbox
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapversions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapContents
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapFolder
  --------------------------------------------------------------------}


Procedure TMapFolder.Setcontents(AIndex : Integer; AValue : TMapFoldercontents); 

begin
  If (Fcontents=AValue) then exit;
  Fcontents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.SetdefaultViewport(AIndex : Integer; AValue : TMapFolderdefaultViewport); 

begin
  If (FdefaultViewport=AValue) then exit;
  FdefaultViewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.Setexpandable(AIndex : Integer; AValue : boolean); 

begin
  If (Fexpandable=AValue) then exit;
  Fexpandable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.Setvisibility(AIndex : Integer; AValue : string); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMapFolder.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TMapFoldercontents
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapFolderdefaultViewport
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapItem
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapKmlLink
  --------------------------------------------------------------------}


Procedure TMapKmlLink.SetdefaultViewport(AIndex : Integer; AValue : TMapKmlLinkdefaultViewport); 

begin
  If (FdefaultViewport=AValue) then exit;
  FdefaultViewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapKmlLink.SetkmlUrl(AIndex : Integer; AValue : string); 

begin
  If (FkmlUrl=AValue) then exit;
  FkmlUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapKmlLink.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapKmlLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapKmlLink.Setvisibility(AIndex : Integer; AValue : string); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMapKmlLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TMapKmlLinkdefaultViewport
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapLayer
  --------------------------------------------------------------------}


Procedure TMapLayer.SetdefaultViewport(AIndex : Integer; AValue : TMapLayerdefaultViewport); 

begin
  If (FdefaultViewport=AValue) then exit;
  FdefaultViewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Setvisibility(AIndex : Integer; AValue : string); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMapLayer.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TMapLayerdefaultViewport
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapsListResponse
  --------------------------------------------------------------------}


Procedure TMapsListResponse.Setmaps(AIndex : Integer; AValue : TMapsListResponsemaps); 

begin
  If (Fmaps=AValue) then exit;
  Fmaps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMapsListResponsemaps
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParent
  --------------------------------------------------------------------}


Procedure TParent.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParentsListResponse
  --------------------------------------------------------------------}


Procedure TParentsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentsListResponse.Setparents(AIndex : Integer; AValue : TParentsListResponseparents); 

begin
  If (Fparents=AValue) then exit;
  Fparents:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParentsListResponseparents
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermission
  --------------------------------------------------------------------}


Procedure TPermission.Setdiscoverable(AIndex : Integer; AValue : boolean); 

begin
  If (Fdiscoverable=AValue) then exit;
  Fdiscoverable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setrole(AIndex : Integer; AValue : string); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPermission.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPermissionsBatchDeleteRequest
  --------------------------------------------------------------------}


Procedure TPermissionsBatchDeleteRequest.Setids(AIndex : Integer; AValue : TPermissionsBatchDeleteRequestids); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermissionsBatchDeleteRequestids
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermissionsBatchDeleteResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermissionsBatchUpdateRequest
  --------------------------------------------------------------------}


Procedure TPermissionsBatchUpdateRequest.Setpermissions(AIndex : Integer; AValue : TPermissionsBatchUpdateRequestpermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermissionsBatchUpdateRequestpermissions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermissionsBatchUpdateResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermissionsListResponse
  --------------------------------------------------------------------}


Procedure TPermissionsListResponse.Setpermissions(AIndex : Integer; AValue : TPermissionsListResponsepermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermissionsListResponsepermissions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPointStyle
  --------------------------------------------------------------------}


Procedure TPointStyle.Seticon(AIndex : Integer; AValue : TIconStyle); 

begin
  If (Ficon=AValue) then exit;
  Ficon:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPointStyle.Set_label(AIndex : Integer; AValue : TLabelStyle); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPointStyle.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPolygonStyle
  --------------------------------------------------------------------}


Procedure TPolygonStyle.Setfill(AIndex : Integer; AValue : TColor); 

begin
  If (Ffill=AValue) then exit;
  Ffill:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.Set_label(AIndex : Integer; AValue : TLabelStyle); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.Setstroke(AIndex : Integer; AValue : TBorder); 

begin
  If (Fstroke=AValue) then exit;
  Fstroke:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPolygonStyle.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProcessResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProject
  --------------------------------------------------------------------}


Procedure TProject.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsListResponse
  --------------------------------------------------------------------}


Procedure TProjectsListResponse.Setprojects(AIndex : Integer; AValue : TProjectsListResponseprojects); 

begin
  If (Fprojects=AValue) then exit;
  Fprojects:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsListResponseprojects
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPublishResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPublishedLayer
  --------------------------------------------------------------------}


Procedure TPublishedLayer.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayer.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayer.SetlayerType(AIndex : Integer; AValue : string); 

begin
  If (FlayerType=AValue) then exit;
  FlayerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayer.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayer.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishedLayersListResponse
  --------------------------------------------------------------------}


Procedure TPublishedLayersListResponse.Setlayers(AIndex : Integer; AValue : TPublishedLayersListResponselayers); 

begin
  If (Flayers=AValue) then exit;
  Flayers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayersListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishedLayersListResponselayers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPublishedMap
  --------------------------------------------------------------------}


Procedure TPublishedMap.Setcontents(AIndex : Integer; AValue : TMapContents); 

begin
  If (Fcontents=AValue) then exit;
  Fcontents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMap.SetdefaultViewport(AIndex : Integer; AValue : TLatLngBox); 

begin
  If (FdefaultViewport=AValue) then exit;
  FdefaultViewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMap.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMap.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMap.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMap.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishedMapsListResponse
  --------------------------------------------------------------------}


Procedure TPublishedMapsListResponse.Setmaps(AIndex : Integer; AValue : TPublishedMapsListResponsemaps); 

begin
  If (Fmaps=AValue) then exit;
  Fmaps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMapsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishedMapsListResponsemaps
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRaster
  --------------------------------------------------------------------}


Procedure TRaster.SetacquisitionTime(AIndex : Integer; AValue : TAcquisitionTime); 

begin
  If (FacquisitionTime=AValue) then exit;
  FacquisitionTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setbbox(AIndex : Integer; AValue : TRasterbbox); 

begin
  If (Fbbox=AValue) then exit;
  Fbbox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetcreationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetcreatorEmail(AIndex : Integer; AValue : string); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetdraftAccessList(AIndex : Integer; AValue : string); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setfiles(AIndex : Integer; AValue : TRasterfiles); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetlastModifierEmail(AIndex : Integer; AValue : string); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetmaskType(AIndex : Integer; AValue : string); 

begin
  If (FmaskType=AValue) then exit;
  FmaskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetprocessingStatus(AIndex : Integer; AValue : string); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetrasterType(AIndex : Integer; AValue : string); 

begin
  If (FrasterType=AValue) then exit;
  FrasterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Settags(AIndex : Integer; AValue : TAssettags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); 

begin
  If (FwritersCanEditPermissions=AValue) then exit;
  FwritersCanEditPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterbbox
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterfiles
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollection
  --------------------------------------------------------------------}


Procedure TRasterCollection.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setbbox(AIndex : Integer; AValue : TRasterCollectionbbox); 

begin
  If (Fbbox=AValue) then exit;
  Fbbox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetcreationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetcreatorEmail(AIndex : Integer; AValue : string); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetdraftAccessList(AIndex : Integer; AValue : string); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetlastModifierEmail(AIndex : Integer; AValue : string); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setmosaic(AIndex : Integer; AValue : boolean); 

begin
  If (Fmosaic=AValue) then exit;
  Fmosaic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetprocessingStatus(AIndex : Integer; AValue : string); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetrasterType(AIndex : Integer; AValue : string); 

begin
  If (FrasterType=AValue) then exit;
  FrasterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Settags(AIndex : Integer; AValue : TAssettags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); 

begin
  If (FwritersCanEditPermissions=AValue) then exit;
  FwritersCanEditPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionbbox
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsListResponse
  --------------------------------------------------------------------}


Procedure TRasterCollectionsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsListResponse.SetrasterCollections(AIndex : Integer; AValue : TRasterCollectionsListResponserasterCollections); 

begin
  If (FrasterCollections=AValue) then exit;
  FrasterCollections:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsListResponserasterCollections
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRaster
  --------------------------------------------------------------------}


Procedure TRasterCollectionsRaster.Setbbox(AIndex : Integer; AValue : TRasterCollectionsRasterbbox); 

begin
  If (Fbbox=AValue) then exit;
  Fbbox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.SetcreationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.SetrasterType(AIndex : Integer; AValue : string); 

begin
  If (FrasterType=AValue) then exit;
  FrasterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.Settags(AIndex : Integer; AValue : TRasterCollectionsRastertags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsRasterbbox
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRastertags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRasterBatchDeleteRequest
  --------------------------------------------------------------------}


Procedure TRasterCollectionsRasterBatchDeleteRequest.Setids(AIndex : Integer; AValue : TRasterCollectionsRasterBatchDeleteRequestids); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsRasterBatchDeleteRequestids
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRastersBatchDeleteResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRastersBatchInsertRequest
  --------------------------------------------------------------------}


Procedure TRasterCollectionsRastersBatchInsertRequest.Setids(AIndex : Integer; AValue : TRasterCollectionsRastersBatchInsertRequestids); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsRastersBatchInsertRequestids
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRastersBatchInsertResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRastersListResponse
  --------------------------------------------------------------------}


Procedure TRasterCollectionsRastersListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRastersListResponse.Setrasters(AIndex : Integer; AValue : TRasterCollectionsRastersListResponserasters); 

begin
  If (Frasters=AValue) then exit;
  Frasters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsRastersListResponserasters
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRastersListResponse
  --------------------------------------------------------------------}


Procedure TRastersListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRastersListResponse.Setrasters(AIndex : Integer; AValue : TRastersListResponserasters); 

begin
  If (Frasters=AValue) then exit;
  Frasters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRastersListResponserasters
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TScaledShape
  --------------------------------------------------------------------}


Procedure TScaledShape.Setborder(AIndex : Integer; AValue : TBorder); 

begin
  If (Fborder=AValue) then exit;
  Fborder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScaledShape.Setfill(AIndex : Integer; AValue : TColor); 

begin
  If (Ffill=AValue) then exit;
  Ffill:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScaledShape.Setshape(AIndex : Integer; AValue : string); 

begin
  If (Fshape=AValue) then exit;
  Fshape:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TScalingFunction
  --------------------------------------------------------------------}


Procedure TScalingFunction.Setcolumn(AIndex : Integer; AValue : string); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScalingFunction.SetscalingType(AIndex : Integer; AValue : string); 

begin
  If (FscalingType=AValue) then exit;
  FscalingType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScalingFunction.SetsizeRange(AIndex : Integer; AValue : TSizeRange); 

begin
  If (FsizeRange=AValue) then exit;
  FsizeRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScalingFunction.SetvalueRange(AIndex : Integer; AValue : TValueRange); 

begin
  If (FvalueRange=AValue) then exit;
  FvalueRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSchema
  --------------------------------------------------------------------}


Procedure TSchema.Setcolumns(AIndex : Integer; AValue : TSchemacolumns); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchema.SetprimaryGeometry(AIndex : Integer; AValue : string); 

begin
  If (FprimaryGeometry=AValue) then exit;
  FprimaryGeometry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchema.SetprimaryKey(AIndex : Integer; AValue : string); 

begin
  If (FprimaryKey=AValue) then exit;
  FprimaryKey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSchemacolumns
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSizeRange
  --------------------------------------------------------------------}


Procedure TSizeRange.Setmax(AIndex : Integer; AValue : double); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSizeRange.Setmin(AIndex : Integer; AValue : double); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTable
  --------------------------------------------------------------------}


Procedure TTable.Setbbox(AIndex : Integer; AValue : TTablebbox); 

begin
  If (Fbbox=AValue) then exit;
  Fbbox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetcreationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetcreatorEmail(AIndex : Integer; AValue : string); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetdraftAccessList(AIndex : Integer; AValue : string); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setfiles(AIndex : Integer; AValue : TTablefiles); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetlastModifierEmail(AIndex : Integer; AValue : string); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetprocessingStatus(AIndex : Integer; AValue : string); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetpublishedAccessList(AIndex : Integer; AValue : string); 

begin
  If (FpublishedAccessList=AValue) then exit;
  FpublishedAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setschema(AIndex : Integer; AValue : TSchema); 

begin
  If (Fschema=AValue) then exit;
  Fschema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetsourceEncoding(AIndex : Integer; AValue : string); 

begin
  If (FsourceEncoding=AValue) then exit;
  FsourceEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Settags(AIndex : Integer; AValue : TAssettags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); 

begin
  If (FwritersCanEditPermissions=AValue) then exit;
  FwritersCanEditPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTablebbox
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTablefiles
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTableColumn
  --------------------------------------------------------------------}


Procedure TTableColumn.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableColumn.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTableColumn.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TTablesListResponse
  --------------------------------------------------------------------}


Procedure TTablesListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTablesListResponse.Settables(AIndex : Integer; AValue : TTablesListResponsetables); 

begin
  If (Ftables=AValue) then exit;
  Ftables:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTablesListResponsetables
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TValueRange
  --------------------------------------------------------------------}


Procedure TValueRange.Setmax(AIndex : Integer; AValue : double); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValueRange.Setmin(AIndex : Integer; AValue : double); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVectorStyle
  --------------------------------------------------------------------}


Procedure TVectorStyle.SetdisplayRules(AIndex : Integer; AValue : TVectorStyledisplayRules); 

begin
  If (FdisplayRules=AValue) then exit;
  FdisplayRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVectorStyle.SetfeatureInfo(AIndex : Integer; AValue : TFeatureInfo); 

begin
  If (FfeatureInfo=AValue) then exit;
  FfeatureInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVectorStyle.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVectorStyle.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVectorStyledisplayRules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TZoomLevels
  --------------------------------------------------------------------}


Procedure TZoomLevels.Setmax(AIndex : Integer; AValue : integer); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoomLevels.Setmin(AIndex : Integer; AValue : integer); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAssetsResource
  --------------------------------------------------------------------}


Class Function TAssetsResource.ResourceName : String;

begin
  Result:='assets';
end;

Class Function TAssetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmapsengineAPI;
end;

Function TAssetsResource.Get(id: string) : TAsset;

Const
  _HTTPMethod = 'GET';
  _Path       = 'assets/{id}';
  _Methodid   = 'mapsengine.assets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAsset) as TAsset;
end;

Function TAssetsResource.List(AQuery : string = '') : TAssetsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'assets';
  _Methodid   = 'mapsengine.assets.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAssetsListResponse) as TAssetsListResponse;
end;


Function TAssetsResource.List(AQuery : TAssetslistOptions) : TAssetsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'bbox',AQuery.bbox);
  AddToQuery(_Q,'createdAfter',AQuery.createdAfter);
  AddToQuery(_Q,'createdBefore',AQuery.createdBefore);
  AddToQuery(_Q,'creatorEmail',AQuery.creatorEmail);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'modifiedAfter',AQuery.modifiedAfter);
  AddToQuery(_Q,'modifiedBefore',AQuery.modifiedBefore);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  AddToQuery(_Q,'role',AQuery.role);
  AddToQuery(_Q,'search',AQuery.search);
  AddToQuery(_Q,'tags',AQuery.tags);
  AddToQuery(_Q,'type',AQuery._type);
  Result:=List(_Q);
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
  Result:=TmapsengineAPI;
end;

Function TLayersResource.CancelProcessing(id: string) : TProcessResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'layers/{id}/cancelProcessing';
  _Methodid   = 'mapsengine.layers.cancelProcessing';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProcessResponse) as TProcessResponse;
end;

Function TLayersResource.Create(aLayer : TLayer; AQuery : string = '') : TLayer;

Const
  _HTTPMethod = 'POST';
  _Path       = 'layers';
  _Methodid   = 'mapsengine.layers.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aLayer,TLayer) as TLayer;
end;


Function TLayersResource.Create(aLayer : TLayer; AQuery : TLayerscreateOptions) : TLayer;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'process',AQuery.process);
  Result:=Create(aLayer,_Q);
end;

Procedure TLayersResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'layers/{id}';
  _Methodid   = 'mapsengine.layers.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TLayersResource.Get(id: string; AQuery : string = '') : TLayer;

Const
  _HTTPMethod = 'GET';
  _Path       = 'layers/{id}';
  _Methodid   = 'mapsengine.layers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLayer) as TLayer;
end;


Function TLayersResource.Get(id: string; AQuery : TLayersgetOptions) : TLayer;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'version',AQuery.version);
  Result:=Get(id,_Q);
end;

Function TLayersResource.GetPublished(id: string) : TPublishedLayer;

Const
  _HTTPMethod = 'GET';
  _Path       = 'layers/{id}/published';
  _Methodid   = 'mapsengine.layers.getPublished';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPublishedLayer) as TPublishedLayer;
end;

Function TLayersResource.List(AQuery : string = '') : TLayersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'layers';
  _Methodid   = 'mapsengine.layers.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TLayersListResponse) as TLayersListResponse;
end;


Function TLayersResource.List(AQuery : TLayerslistOptions) : TLayersListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'bbox',AQuery.bbox);
  AddToQuery(_Q,'createdAfter',AQuery.createdAfter);
  AddToQuery(_Q,'createdBefore',AQuery.createdBefore);
  AddToQuery(_Q,'creatorEmail',AQuery.creatorEmail);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'modifiedAfter',AQuery.modifiedAfter);
  AddToQuery(_Q,'modifiedBefore',AQuery.modifiedBefore);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'processingStatus',AQuery.processingStatus);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  AddToQuery(_Q,'role',AQuery.role);
  AddToQuery(_Q,'search',AQuery.search);
  AddToQuery(_Q,'tags',AQuery.tags);
  Result:=List(_Q);
end;

Function TLayersResource.ListPublished(AQuery : string = '') : TPublishedLayersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'layers/published';
  _Methodid   = 'mapsengine.layers.listPublished';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TPublishedLayersListResponse) as TPublishedLayersListResponse;
end;


Function TLayersResource.ListPublished(AQuery : TLayerslistPublishedOptions) : TPublishedLayersListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=ListPublished(_Q);
end;

Procedure TLayersResource.Patch(id: string; aLayer : TLayer);

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'layers/{id}';
  _Methodid   = 'mapsengine.layers.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',aLayer,Nil);
end;

Function TLayersResource.Process(id: string) : TProcessResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'layers/{id}/process';
  _Methodid   = 'mapsengine.layers.process';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProcessResponse) as TProcessResponse;
end;

Function TLayersResource.Publish(id: string; AQuery : string = '') : TPublishResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'layers/{id}/publish';
  _Methodid   = 'mapsengine.layers.publish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPublishResponse) as TPublishResponse;
end;


Function TLayersResource.Publish(id: string; AQuery : TLayerspublishOptions) : TPublishResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'force',AQuery.force);
  Result:=Publish(id,_Q);
end;

Function TLayersResource.Unpublish(id: string) : TPublishResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'layers/{id}/unpublish';
  _Methodid   = 'mapsengine.layers.unpublish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPublishResponse) as TPublishResponse;
end;



{ --------------------------------------------------------------------
  TMapsResource
  --------------------------------------------------------------------}


Class Function TMapsResource.ResourceName : String;

begin
  Result:='maps';
end;

Class Function TMapsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmapsengineAPI;
end;

Function TMapsResource.Create(aMap : TMap) : TMap;

Const
  _HTTPMethod = 'POST';
  _Path       = 'maps';
  _Methodid   = 'mapsengine.maps.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aMap,TMap) as TMap;
end;

Procedure TMapsResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'maps/{id}';
  _Methodid   = 'mapsengine.maps.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TMapsResource.Get(id: string; AQuery : string = '') : TMap;

Const
  _HTTPMethod = 'GET';
  _Path       = 'maps/{id}';
  _Methodid   = 'mapsengine.maps.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TMap) as TMap;
end;


Function TMapsResource.Get(id: string; AQuery : TMapsgetOptions) : TMap;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'version',AQuery.version);
  Result:=Get(id,_Q);
end;

Function TMapsResource.GetPublished(id: string) : TPublishedMap;

Const
  _HTTPMethod = 'GET';
  _Path       = 'maps/{id}/published';
  _Methodid   = 'mapsengine.maps.getPublished';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPublishedMap) as TPublishedMap;
end;

Function TMapsResource.List(AQuery : string = '') : TMapsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'maps';
  _Methodid   = 'mapsengine.maps.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TMapsListResponse) as TMapsListResponse;
end;


Function TMapsResource.List(AQuery : TMapslistOptions) : TMapsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'bbox',AQuery.bbox);
  AddToQuery(_Q,'createdAfter',AQuery.createdAfter);
  AddToQuery(_Q,'createdBefore',AQuery.createdBefore);
  AddToQuery(_Q,'creatorEmail',AQuery.creatorEmail);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'modifiedAfter',AQuery.modifiedAfter);
  AddToQuery(_Q,'modifiedBefore',AQuery.modifiedBefore);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'processingStatus',AQuery.processingStatus);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  AddToQuery(_Q,'role',AQuery.role);
  AddToQuery(_Q,'search',AQuery.search);
  AddToQuery(_Q,'tags',AQuery.tags);
  Result:=List(_Q);
end;

Function TMapsResource.ListPublished(AQuery : string = '') : TPublishedMapsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'maps/published';
  _Methodid   = 'mapsengine.maps.listPublished';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TPublishedMapsListResponse) as TPublishedMapsListResponse;
end;


Function TMapsResource.ListPublished(AQuery : TMapslistPublishedOptions) : TPublishedMapsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=ListPublished(_Q);
end;

Procedure TMapsResource.Patch(id: string; aMap : TMap);

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'maps/{id}';
  _Methodid   = 'mapsengine.maps.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',aMap,Nil);
end;

Function TMapsResource.Publish(id: string; AQuery : string = '') : TPublishResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'maps/{id}/publish';
  _Methodid   = 'mapsengine.maps.publish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPublishResponse) as TPublishResponse;
end;


Function TMapsResource.Publish(id: string; AQuery : TMapspublishOptions) : TPublishResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'force',AQuery.force);
  Result:=Publish(id,_Q);
end;

Function TMapsResource.Unpublish(id: string) : TPublishResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'maps/{id}/unpublish';
  _Methodid   = 'mapsengine.maps.unpublish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPublishResponse) as TPublishResponse;
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
  Result:=TmapsengineAPI;
end;

Function TProjectsResource.List : TProjectsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects';
  _Methodid   = 'mapsengine.projects.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TProjectsListResponse) as TProjectsListResponse;
end;



{ --------------------------------------------------------------------
  TRasterCollectionsResource
  --------------------------------------------------------------------}


Class Function TRasterCollectionsResource.ResourceName : String;

begin
  Result:='rasterCollections';
end;

Class Function TRasterCollectionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmapsengineAPI;
end;

Function TRasterCollectionsResource.CancelProcessing(id: string) : TProcessResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rasterCollections/{id}/cancelProcessing';
  _Methodid   = 'mapsengine.rasterCollections.cancelProcessing';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProcessResponse) as TProcessResponse;
end;

Function TRasterCollectionsResource.Create(aRasterCollection : TRasterCollection) : TRasterCollection;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rasterCollections';
  _Methodid   = 'mapsengine.rasterCollections.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aRasterCollection,TRasterCollection) as TRasterCollection;
end;

Procedure TRasterCollectionsResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'rasterCollections/{id}';
  _Methodid   = 'mapsengine.rasterCollections.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TRasterCollectionsResource.Get(id: string) : TRasterCollection;

Const
  _HTTPMethod = 'GET';
  _Path       = 'rasterCollections/{id}';
  _Methodid   = 'mapsengine.rasterCollections.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRasterCollection) as TRasterCollection;
end;

Function TRasterCollectionsResource.List(AQuery : string = '') : TRasterCollectionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'rasterCollections';
  _Methodid   = 'mapsengine.rasterCollections.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TRasterCollectionsListResponse) as TRasterCollectionsListResponse;
end;


Function TRasterCollectionsResource.List(AQuery : TRasterCollectionslistOptions) : TRasterCollectionsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'bbox',AQuery.bbox);
  AddToQuery(_Q,'createdAfter',AQuery.createdAfter);
  AddToQuery(_Q,'createdBefore',AQuery.createdBefore);
  AddToQuery(_Q,'creatorEmail',AQuery.creatorEmail);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'modifiedAfter',AQuery.modifiedAfter);
  AddToQuery(_Q,'modifiedBefore',AQuery.modifiedBefore);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'processingStatus',AQuery.processingStatus);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  AddToQuery(_Q,'role',AQuery.role);
  AddToQuery(_Q,'search',AQuery.search);
  AddToQuery(_Q,'tags',AQuery.tags);
  Result:=List(_Q);
end;

Procedure TRasterCollectionsResource.Patch(id: string; aRasterCollection : TRasterCollection);

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'rasterCollections/{id}';
  _Methodid   = 'mapsengine.rasterCollections.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',aRasterCollection,Nil);
end;

Function TRasterCollectionsResource.Process(id: string) : TProcessResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rasterCollections/{id}/process';
  _Methodid   = 'mapsengine.rasterCollections.process';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProcessResponse) as TProcessResponse;
end;



{ --------------------------------------------------------------------
  TRastersResource
  --------------------------------------------------------------------}


Class Function TRastersResource.ResourceName : String;

begin
  Result:='rasters';
end;

Class Function TRastersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmapsengineAPI;
end;

Procedure TRastersResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'rasters/{id}';
  _Methodid   = 'mapsengine.rasters.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TRastersResource.Get(id: string) : TRaster;

Const
  _HTTPMethod = 'GET';
  _Path       = 'rasters/{id}';
  _Methodid   = 'mapsengine.rasters.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRaster) as TRaster;
end;

Function TRastersResource.List(AQuery : string = '') : TRastersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'rasters';
  _Methodid   = 'mapsengine.rasters.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TRastersListResponse) as TRastersListResponse;
end;


Function TRastersResource.List(AQuery : TRasterslistOptions) : TRastersListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'bbox',AQuery.bbox);
  AddToQuery(_Q,'createdAfter',AQuery.createdAfter);
  AddToQuery(_Q,'createdBefore',AQuery.createdBefore);
  AddToQuery(_Q,'creatorEmail',AQuery.creatorEmail);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'modifiedAfter',AQuery.modifiedAfter);
  AddToQuery(_Q,'modifiedBefore',AQuery.modifiedBefore);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'processingStatus',AQuery.processingStatus);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  AddToQuery(_Q,'role',AQuery.role);
  AddToQuery(_Q,'search',AQuery.search);
  AddToQuery(_Q,'tags',AQuery.tags);
  Result:=List(_Q);
end;

Procedure TRastersResource.Patch(id: string; aRaster : TRaster);

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'rasters/{id}';
  _Methodid   = 'mapsengine.rasters.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',aRaster,Nil);
end;

Function TRastersResource.Process(id: string) : TProcessResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rasters/{id}/process';
  _Methodid   = 'mapsengine.rasters.process';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProcessResponse) as TProcessResponse;
end;

Function TRastersResource.Upload(aRaster : TRaster) : TRaster;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rasters/upload';
  _Methodid   = 'mapsengine.rasters.upload';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aRaster,TRaster) as TRaster;
end;



{ --------------------------------------------------------------------
  TTablesResource
  --------------------------------------------------------------------}


Class Function TTablesResource.ResourceName : String;

begin
  Result:='tables';
end;

Class Function TTablesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmapsengineAPI;
end;

Function TTablesResource.Create(aTable : TTable) : TTable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables';
  _Methodid   = 'mapsengine.tables.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aTable,TTable) as TTable;
end;

Procedure TTablesResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'tables/{id}';
  _Methodid   = 'mapsengine.tables.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTablesResource.Get(id: string; AQuery : string = '') : TTable;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{id}';
  _Methodid   = 'mapsengine.tables.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTable) as TTable;
end;


Function TTablesResource.Get(id: string; AQuery : TTablesgetOptions) : TTable;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'version',AQuery.version);
  Result:=Get(id,_Q);
end;

Function TTablesResource.List(AQuery : string = '') : TTablesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables';
  _Methodid   = 'mapsengine.tables.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTablesListResponse) as TTablesListResponse;
end;


Function TTablesResource.List(AQuery : TTableslistOptions) : TTablesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'bbox',AQuery.bbox);
  AddToQuery(_Q,'createdAfter',AQuery.createdAfter);
  AddToQuery(_Q,'createdBefore',AQuery.createdBefore);
  AddToQuery(_Q,'creatorEmail',AQuery.creatorEmail);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'modifiedAfter',AQuery.modifiedAfter);
  AddToQuery(_Q,'modifiedBefore',AQuery.modifiedBefore);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'processingStatus',AQuery.processingStatus);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  AddToQuery(_Q,'role',AQuery.role);
  AddToQuery(_Q,'search',AQuery.search);
  AddToQuery(_Q,'tags',AQuery.tags);
  Result:=List(_Q);
end;

Procedure TTablesResource.Patch(id: string; aTable : TTable);

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'tables/{id}';
  _Methodid   = 'mapsengine.tables.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',aTable,Nil);
end;

Function TTablesResource.Process(id: string) : TProcessResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/{id}/process';
  _Methodid   = 'mapsengine.tables.process';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProcessResponse) as TProcessResponse;
end;

Function TTablesResource.Upload(aTable : TTable) : TTable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/upload';
  _Methodid   = 'mapsengine.tables.upload';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aTable,TTable) as TTable;
end;



{ --------------------------------------------------------------------
  TMapsengineAPI
  --------------------------------------------------------------------}

Class Function TMapsengineAPI.APIName : String;

begin
  Result:='mapsengine';
end;

Class Function TMapsengineAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TMapsengineAPI.APIRevision : String;

begin
  Result:='20150414';
end;

Class Function TMapsengineAPI.APIID : String;

begin
  Result:='mapsengine:v1';
end;

Class Function TMapsengineAPI.APITitle : String;

begin
  Result:='Google Maps Engine API';
end;

Class Function TMapsengineAPI.APIDescription : String;

begin
  Result:='The Google Maps Engine API allows developers to store and query geospatial vector and raster data.';
end;

Class Function TMapsengineAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TMapsengineAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TMapsengineAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/maps_engine-16.png';
end;

Class Function TMapsengineAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/maps_engine-32.png';
end;

Class Function TMapsengineAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/maps-engine/';
end;

Class Function TMapsengineAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TMapsengineAPI.APIbasePath : string;

begin
  Result:='/mapsengine/v1/';
end;

Class Function TMapsengineAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/mapsengine/v1/';
end;

Class Function TMapsengineAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TMapsengineAPI.APIservicePath : string;

begin
  Result:='mapsengine/v1/';
end;

Class Function TMapsengineAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TMapsengineAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/mapsengine';
  Result[0].Description:='View and manage your Google My Maps data';
  Result[1].Name:='https://www.googleapis.com/auth/mapsengine.readonly';
  Result[1].Description:='View your Google My Maps data';
  
end;

Class Function TMapsengineAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TMapsengineAPI.RegisterAPIResources;

begin
  TAcquisitionTime.RegisterObject;
  TAsset.RegisterObject;
  TAssetbbox.RegisterObject;
  TAssettags.RegisterObject;
  TAssetsListResponse.RegisterObject;
  TAssetsListResponseassets.RegisterObject;
  TBorder.RegisterObject;
  TColor.RegisterObject;
  TDatasource.RegisterObject;
  TDatasources.RegisterObject;
  TDisplayRule.RegisterObject;
  TDisplayRulefilters.RegisterObject;
  TFeature.RegisterObject;
  TFeatureInfo.RegisterObject;
  TFeaturesBatchDeleteRequest.RegisterObject;
  TFeaturesBatchDeleteRequestgx_ids.RegisterObject;
  TFeaturesBatchDeleteRequestprimaryKeys.RegisterObject;
  TFeaturesBatchInsertRequest.RegisterObject;
  TFeaturesBatchInsertRequestfeatures.RegisterObject;
  TFeaturesBatchPatchRequest.RegisterObject;
  TFeaturesBatchPatchRequestfeatures.RegisterObject;
  TFeaturesListResponse.RegisterObject;
  TFeaturesListResponsefeatures.RegisterObject;
  TFile.RegisterObject;
  TFilter.RegisterObject;
  TGeoJsonGeometry.RegisterObject;
  TGeoJsonGeometryCollection.RegisterObject;
  TGeoJsonGeometryCollectiongeometries.RegisterObject;
  TGeoJsonLineString.RegisterObject;
  TGeoJsonLineStringcoordinates.RegisterObject;
  TGeoJsonMultiLineString.RegisterObject;
  TGeoJsonMultiLineStringcoordinates.RegisterObject;
  TGeoJsonMultiPoint.RegisterObject;
  TGeoJsonMultiPointcoordinates.RegisterObject;
  TGeoJsonMultiPolygon.RegisterObject;
  TGeoJsonMultiPolygoncoordinates.RegisterObject;
  TGeoJsonPoint.RegisterObject;
  TGeoJsonPolygon.RegisterObject;
  TGeoJsonPolygoncoordinates.RegisterObject;
  TGeoJsonPosition.RegisterObject;
  TGeoJsonProperties.RegisterObject;
  TIcon.RegisterObject;
  TIconStyle.RegisterObject;
  TIconsListResponse.RegisterObject;
  TIconsListResponseicons.RegisterObject;
  TLabelStyle.RegisterObject;
  TLatLngBox.RegisterObject;
  TLayer.RegisterObject;
  TLayerbbox.RegisterObject;
  TLayersListResponse.RegisterObject;
  TLayersListResponselayers.RegisterObject;
  TLineStyle.RegisterObject;
  TLineStyledash.RegisterObject;
  TLineStylestroke.RegisterObject;
  TMap.RegisterObject;
  TMapbbox.RegisterObject;
  TMapversions.RegisterObject;
  TMapContents.RegisterObject;
  TMapFolder.RegisterObject;
  TMapFoldercontents.RegisterObject;
  TMapFolderdefaultViewport.RegisterObject;
  TMapItem.RegisterObject;
  TMapKmlLink.RegisterObject;
  TMapKmlLinkdefaultViewport.RegisterObject;
  TMapLayer.RegisterObject;
  TMapLayerdefaultViewport.RegisterObject;
  TMapsListResponse.RegisterObject;
  TMapsListResponsemaps.RegisterObject;
  TParent.RegisterObject;
  TParentsListResponse.RegisterObject;
  TParentsListResponseparents.RegisterObject;
  TPermission.RegisterObject;
  TPermissionsBatchDeleteRequest.RegisterObject;
  TPermissionsBatchDeleteRequestids.RegisterObject;
  TPermissionsBatchDeleteResponse.RegisterObject;
  TPermissionsBatchUpdateRequest.RegisterObject;
  TPermissionsBatchUpdateRequestpermissions.RegisterObject;
  TPermissionsBatchUpdateResponse.RegisterObject;
  TPermissionsListResponse.RegisterObject;
  TPermissionsListResponsepermissions.RegisterObject;
  TPointStyle.RegisterObject;
  TPolygonStyle.RegisterObject;
  TProcessResponse.RegisterObject;
  TProject.RegisterObject;
  TProjectsListResponse.RegisterObject;
  TProjectsListResponseprojects.RegisterObject;
  TPublishResponse.RegisterObject;
  TPublishedLayer.RegisterObject;
  TPublishedLayersListResponse.RegisterObject;
  TPublishedLayersListResponselayers.RegisterObject;
  TPublishedMap.RegisterObject;
  TPublishedMapsListResponse.RegisterObject;
  TPublishedMapsListResponsemaps.RegisterObject;
  TRaster.RegisterObject;
  TRasterbbox.RegisterObject;
  TRasterfiles.RegisterObject;
  TRasterCollection.RegisterObject;
  TRasterCollectionbbox.RegisterObject;
  TRasterCollectionsListResponse.RegisterObject;
  TRasterCollectionsListResponserasterCollections.RegisterObject;
  TRasterCollectionsRaster.RegisterObject;
  TRasterCollectionsRasterbbox.RegisterObject;
  TRasterCollectionsRastertags.RegisterObject;
  TRasterCollectionsRasterBatchDeleteRequest.RegisterObject;
  TRasterCollectionsRasterBatchDeleteRequestids.RegisterObject;
  TRasterCollectionsRastersBatchDeleteResponse.RegisterObject;
  TRasterCollectionsRastersBatchInsertRequest.RegisterObject;
  TRasterCollectionsRastersBatchInsertRequestids.RegisterObject;
  TRasterCollectionsRastersBatchInsertResponse.RegisterObject;
  TRasterCollectionsRastersListResponse.RegisterObject;
  TRasterCollectionsRastersListResponserasters.RegisterObject;
  TRastersListResponse.RegisterObject;
  TRastersListResponserasters.RegisterObject;
  TScaledShape.RegisterObject;
  TScalingFunction.RegisterObject;
  TSchema.RegisterObject;
  TSchemacolumns.RegisterObject;
  TSizeRange.RegisterObject;
  TTable.RegisterObject;
  TTablebbox.RegisterObject;
  TTablefiles.RegisterObject;
  TTableColumn.RegisterObject;
  TTablesListResponse.RegisterObject;
  TTablesListResponsetables.RegisterObject;
  TTags.RegisterObject;
  TValueRange.RegisterObject;
  TVectorStyle.RegisterObject;
  TVectorStyledisplayRules.RegisterObject;
  TZoomLevels.RegisterObject;
end;


Function TMapsengineAPI.GetAssetsInstance : TAssetsResource;

begin
  if (FAssetsInstance=Nil) then
    FAssetsInstance:=CreateAssetsResource;
  Result:=FAssetsInstance;
end;

Function TMapsengineAPI.CreateAssetsResource : TAssetsResource;

begin
  Result:=CreateAssetsResource(Self);
end;


Function TMapsengineAPI.CreateAssetsResource(AOwner : TComponent) : TAssetsResource;

begin
  Result:=TAssetsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMapsengineAPI.GetLayersInstance : TLayersResource;

begin
  if (FLayersInstance=Nil) then
    FLayersInstance:=CreateLayersResource;
  Result:=FLayersInstance;
end;

Function TMapsengineAPI.CreateLayersResource : TLayersResource;

begin
  Result:=CreateLayersResource(Self);
end;


Function TMapsengineAPI.CreateLayersResource(AOwner : TComponent) : TLayersResource;

begin
  Result:=TLayersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMapsengineAPI.GetMapsInstance : TMapsResource;

begin
  if (FMapsInstance=Nil) then
    FMapsInstance:=CreateMapsResource;
  Result:=FMapsInstance;
end;

Function TMapsengineAPI.CreateMapsResource : TMapsResource;

begin
  Result:=CreateMapsResource(Self);
end;


Function TMapsengineAPI.CreateMapsResource(AOwner : TComponent) : TMapsResource;

begin
  Result:=TMapsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMapsengineAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TMapsengineAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TMapsengineAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMapsengineAPI.GetRasterCollectionsInstance : TRasterCollectionsResource;

begin
  if (FRasterCollectionsInstance=Nil) then
    FRasterCollectionsInstance:=CreateRasterCollectionsResource;
  Result:=FRasterCollectionsInstance;
end;

Function TMapsengineAPI.CreateRasterCollectionsResource : TRasterCollectionsResource;

begin
  Result:=CreateRasterCollectionsResource(Self);
end;


Function TMapsengineAPI.CreateRasterCollectionsResource(AOwner : TComponent) : TRasterCollectionsResource;

begin
  Result:=TRasterCollectionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMapsengineAPI.GetRastersInstance : TRastersResource;

begin
  if (FRastersInstance=Nil) then
    FRastersInstance:=CreateRastersResource;
  Result:=FRastersInstance;
end;

Function TMapsengineAPI.CreateRastersResource : TRastersResource;

begin
  Result:=CreateRastersResource(Self);
end;


Function TMapsengineAPI.CreateRastersResource(AOwner : TComponent) : TRastersResource;

begin
  Result:=TRastersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMapsengineAPI.GetTablesInstance : TTablesResource;

begin
  if (FTablesInstance=Nil) then
    FTablesInstance:=CreateTablesResource;
  Result:=FTablesInstance;
end;

Function TMapsengineAPI.CreateTablesResource : TTablesResource;

begin
  Result:=CreateTablesResource(Self);
end;


Function TMapsengineAPI.CreateTablesResource(AOwner : TComponent) : TTablesResource;

begin
  Result:=TTablesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TMapsengineAPI.RegisterAPI;
end.
