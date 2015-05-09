unit googlemapsengine;
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
//Generated on: 9-5-15 13:22:56
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAcquisitionTime = class;
  TAsset = class;
  TAssetsListResponse = class;
  TBorder = class;
  TColor = class;
  TDatasource = class;
  TDisplayRule = class;
  TFeature = class;
  TFeatureInfo = class;
  TFeaturesBatchDeleteRequest = class;
  TFeaturesBatchInsertRequest = class;
  TFeaturesBatchPatchRequest = class;
  TFeaturesListResponse = class;
  TFile = class;
  TFilter = class;
  TGeoJsonGeometry = class;
  TGeoJsonGeometryCollection = class;
  TGeoJsonLineString = class;
  TGeoJsonMultiLineString = class;
  TGeoJsonMultiPoint = class;
  TGeoJsonMultiPolygon = class;
  TGeoJsonPoint = class;
  TGeoJsonPolygon = class;
  TGeoJsonProperties = class;
  TIcon = class;
  TIconStyle = class;
  TIconsListResponse = class;
  TLabelStyle = class;
  TLayer = class;
  TLayersListResponse = class;
  TLineStyle = class;
  TMap = class;
  TMapFolder = class;
  TMapItem = class;
  TMapKmlLink = class;
  TMapLayer = class;
  TMapsListResponse = class;
  TParent = class;
  TParentsListResponse = class;
  TPermission = class;
  TPermissionsBatchDeleteRequest = class;
  TPermissionsBatchDeleteResponse = class;
  TPermissionsBatchUpdateRequest = class;
  TPermissionsBatchUpdateResponse = class;
  TPermissionsListResponse = class;
  TPointStyle = class;
  TPolygonStyle = class;
  TProcessResponse = class;
  TProject = class;
  TProjectsListResponse = class;
  TPublishResponse = class;
  TPublishedLayer = class;
  TPublishedLayersListResponse = class;
  TPublishedMap = class;
  TPublishedMapsListResponse = class;
  TRaster = class;
  TRasterCollection = class;
  TRasterCollectionsListResponse = class;
  TRasterCollectionsRaster = class;
  TRasterCollectionsRasterBatchDeleteRequest = class;
  TRasterCollectionsRastersBatchDeleteResponse = class;
  TRasterCollectionsRastersBatchInsertRequest = class;
  TRasterCollectionsRastersBatchInsertResponse = class;
  TRasterCollectionsRastersListResponse = class;
  TRastersListResponse = class;
  TScaledShape = class;
  TScalingFunction = class;
  TSchema = class;
  TSizeRange = class;
  TTable = class;
  TTableColumn = class;
  TTablesListResponse = class;
  TValueRange = class;
  TVectorStyle = class;
  TZoomLevels = class;
  TAcquisitionTimeArray = Array of TAcquisitionTime;
  TAssetArray = Array of TAsset;
  TAssetsListResponseArray = Array of TAssetsListResponse;
  TBorderArray = Array of TBorder;
  TColorArray = Array of TColor;
  TDatasourceArray = Array of TDatasource;
  TDatasources = Array of TDatasource;
  TDisplayRuleArray = Array of TDisplayRule;
  TFeatureArray = Array of TFeature;
  TFeatureInfoArray = Array of TFeatureInfo;
  TFeaturesBatchDeleteRequestArray = Array of TFeaturesBatchDeleteRequest;
  TFeaturesBatchInsertRequestArray = Array of TFeaturesBatchInsertRequest;
  TFeaturesBatchPatchRequestArray = Array of TFeaturesBatchPatchRequest;
  TFeaturesListResponseArray = Array of TFeaturesListResponse;
  TFileArray = Array of TFile;
  TFilterArray = Array of TFilter;
  TGeoJsonGeometryArray = Array of TGeoJsonGeometry;
  TGeoJsonGeometryCollectionArray = Array of TGeoJsonGeometryCollection;
  TGeoJsonLineStringArray = Array of TGeoJsonLineString;
  TGeoJsonMultiLineStringArray = Array of TGeoJsonMultiLineString;
  TGeoJsonMultiPointArray = Array of TGeoJsonMultiPoint;
  TGeoJsonMultiPolygonArray = Array of TGeoJsonMultiPolygon;
  TGeoJsonPointArray = Array of TGeoJsonPoint;
  TGeoJsonPolygonArray = Array of TGeoJsonPolygon;
  TGeoJsonPosition = Array of double;
  TGeoJsonPropertiesArray = Array of TGeoJsonProperties;
  TIconArray = Array of TIcon;
  TIconStyleArray = Array of TIconStyle;
  TIconsListResponseArray = Array of TIconsListResponse;
  TLabelStyleArray = Array of TLabelStyle;
  TLatLngBox = Array of double;
  TLayerArray = Array of TLayer;
  TLayersListResponseArray = Array of TLayersListResponse;
  TLineStyleArray = Array of TLineStyle;
  TMapArray = Array of TMap;
  TMapContents = Array of TMapItem;
  TMapFolderArray = Array of TMapFolder;
  TMapItemArray = Array of TMapItem;
  TMapKmlLinkArray = Array of TMapKmlLink;
  TMapLayerArray = Array of TMapLayer;
  TMapsListResponseArray = Array of TMapsListResponse;
  TParentArray = Array of TParent;
  TParentsListResponseArray = Array of TParentsListResponse;
  TPermissionArray = Array of TPermission;
  TPermissionsBatchDeleteRequestArray = Array of TPermissionsBatchDeleteRequest;
  TPermissionsBatchDeleteResponseArray = Array of TPermissionsBatchDeleteResponse;
  TPermissionsBatchUpdateRequestArray = Array of TPermissionsBatchUpdateRequest;
  TPermissionsBatchUpdateResponseArray = Array of TPermissionsBatchUpdateResponse;
  TPermissionsListResponseArray = Array of TPermissionsListResponse;
  TPointStyleArray = Array of TPointStyle;
  TPolygonStyleArray = Array of TPolygonStyle;
  TProcessResponseArray = Array of TProcessResponse;
  TProjectArray = Array of TProject;
  TProjectsListResponseArray = Array of TProjectsListResponse;
  TPublishResponseArray = Array of TPublishResponse;
  TPublishedLayerArray = Array of TPublishedLayer;
  TPublishedLayersListResponseArray = Array of TPublishedLayersListResponse;
  TPublishedMapArray = Array of TPublishedMap;
  TPublishedMapsListResponseArray = Array of TPublishedMapsListResponse;
  TRasterArray = Array of TRaster;
  TRasterCollectionArray = Array of TRasterCollection;
  TRasterCollectionsListResponseArray = Array of TRasterCollectionsListResponse;
  TRasterCollectionsRasterArray = Array of TRasterCollectionsRaster;
  TRasterCollectionsRasterBatchDeleteRequestArray = Array of TRasterCollectionsRasterBatchDeleteRequest;
  TRasterCollectionsRastersBatchDeleteResponseArray = Array of TRasterCollectionsRastersBatchDeleteResponse;
  TRasterCollectionsRastersBatchInsertRequestArray = Array of TRasterCollectionsRastersBatchInsertRequest;
  TRasterCollectionsRastersBatchInsertResponseArray = Array of TRasterCollectionsRastersBatchInsertResponse;
  TRasterCollectionsRastersListResponseArray = Array of TRasterCollectionsRastersListResponse;
  TRastersListResponseArray = Array of TRastersListResponse;
  TScaledShapeArray = Array of TScaledShape;
  TScalingFunctionArray = Array of TScalingFunction;
  TSchemaArray = Array of TSchema;
  TSizeRangeArray = Array of TSizeRange;
  TTableArray = Array of TTable;
  TTableColumnArray = Array of TTableColumn;
  TTablesListResponseArray = Array of TTablesListResponse;
  TTags = Array of String;
  TValueRangeArray = Array of TValueRange;
  TVectorStyleArray = Array of TVectorStyle;
  TZoomLevelsArray = Array of TZoomLevels;
  //Anonymous types, using auto-generated names
  TLineStyleTypestroke = class;
  TAssetsListResponseTypeassetsArray = Array of TAsset;
  TDisplayRuleTypefiltersArray = Array of TFilter;
  TFeaturesBatchInsertRequestTypefeaturesArray = Array of TFeature;
  TFeaturesBatchPatchRequestTypefeaturesArray = Array of TFeature;
  TFeaturesListResponseTypefeaturesArray = Array of TFeature;
  TGeoJsonGeometryCollectionTypegeometriesArray = Array of TGeoJsonGeometry;
  TGeoJsonLineStringTypecoordinatesArray = Array of TGeoJsonPosition;
  TGeoJsonMultiLineStringTypecoordinatesItemArray = Array of TGeoJsonPosition;
  TGeoJsonMultiLineStringTypecoordinatesArray = Array of TGeoJsonMultiLineStringTypecoordinatesItemArray;
  TGeoJsonMultiPointTypecoordinatesArray = Array of TGeoJsonPosition;
  TGeoJsonMultiPolygonTypecoordinatesItemItemArray = Array of TGeoJsonPosition;
  TGeoJsonMultiPolygonTypecoordinatesItemArray = Array of TGeoJsonMultiPolygonTypecoordinatesItemItemArray;
  TGeoJsonMultiPolygonTypecoordinatesArray = Array of TGeoJsonMultiPolygonTypecoordinatesItemArray;
  TGeoJsonPolygonTypecoordinatesItemArray = Array of TGeoJsonPosition;
  TGeoJsonPolygonTypecoordinatesArray = Array of TGeoJsonPolygonTypecoordinatesItemArray;
  TIconsListResponseTypeiconsArray = Array of TIcon;
  TLayersListResponseTypelayersArray = Array of TLayer;
  TMapFolderTypecontentsArray = Array of TMapItem;
  TMapsListResponseTypemapsArray = Array of TMap;
  TParentsListResponseTypeparentsArray = Array of TParent;
  TPermissionsBatchUpdateRequestTypepermissionsArray = Array of TPermission;
  TPermissionsListResponseTypepermissionsArray = Array of TPermission;
  TProjectsListResponseTypeprojectsArray = Array of TProject;
  TPublishedLayersListResponseTypelayersArray = Array of TPublishedLayer;
  TPublishedMapsListResponseTypemapsArray = Array of TPublishedMap;
  TRasterTypefilesArray = Array of TFile;
  TRasterCollectionsListResponseTyperasterCollectionsArray = Array of TRasterCollection;
  TRasterCollectionsRastersListResponseTyperastersArray = Array of TRasterCollectionsRaster;
  TRastersListResponseTyperastersArray = Array of TRaster;
  TSchemaTypecolumnsArray = Array of TTableColumn;
  TTableTypefilesArray = Array of TFile;
  TTablesListResponseTypetablesArray = Array of TTable;
  TVectorStyleTypedisplayRulesArray = Array of TDisplayRule;
  
  { --------------------------------------------------------------------
    TAcquisitionTime
    --------------------------------------------------------------------}
  
  TAcquisitionTime = Class(TGoogleBaseObject)
  Private
    F_end : TDatetime;
    Fprecision : String;
    Fstart : TDatetime;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setprecision(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property _end : TDatetime Index 0 Read F_end Write Set_end;
    Property precision : String Index 8 Read Fprecision Write Setprecision;
    Property start : TDatetime Index 16 Read Fstart Write Setstart;
  end;
  TAcquisitionTimeClass = Class of TAcquisitionTime;
  
  { --------------------------------------------------------------------
    TAsset
    --------------------------------------------------------------------}
  
  TAsset = Class(TGoogleBaseObject)
  Private
    Fbbox : TdoubleArray;
    FcreationTime : TDatetime;
    FcreatorEmail : String;
    Fdescription : String;
    Fetag : String;
    Fid : String;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : String;
    Fname : String;
    FprojectId : String;
    Fresource : String;
    Ftags : TStringArray;
    F_type : String;
    FwritersCanEditPermissions : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure Setresource(AIndex : Integer; AValue : String); virtual;
    Procedure Settags(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bbox : TdoubleArray Index 0 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 8 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : String Index 16 Read FcreatorEmail Write SetcreatorEmail;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property etag : String Index 32 Read Fetag Write Setetag;
    Property id : String Index 40 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 48 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : String Index 56 Read FlastModifierEmail Write SetlastModifierEmail;
    Property name : String Index 64 Read Fname Write Setname;
    Property projectId : String Index 72 Read FprojectId Write SetprojectId;
    Property resource : String Index 80 Read Fresource Write Setresource;
    Property tags : TStringArray Index 88 Read Ftags Write Settags;
    Property _type : String Index 96 Read F_type Write Set_type;
    Property writersCanEditPermissions : boolean Index 104 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TAssetClass = Class of TAsset;
  
  { --------------------------------------------------------------------
    TAssetsListResponse
    --------------------------------------------------------------------}
  
  TAssetsListResponse = Class(TGoogleBaseObject)
  Private
    Fassets : TAssetsListResponseTypeassetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setassets(AIndex : Integer; AValue : TAssetsListResponseTypeassetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property assets : TAssetsListResponseTypeassetsArray Index 0 Read Fassets Write Setassets;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TAssetsListResponseClass = Class of TAssetsListResponse;
  
  { --------------------------------------------------------------------
    TBorder
    --------------------------------------------------------------------}
  
  TBorder = Class(TGoogleBaseObject)
  Private
    Fcolor : String;
    Fopacity : double;
    Fwidth : double;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : String); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property color : String Index 0 Read Fcolor Write Setcolor;
    Property opacity : double Index 8 Read Fopacity Write Setopacity;
    Property width : double Index 16 Read Fwidth Write Setwidth;
  end;
  TBorderClass = Class of TBorder;
  
  { --------------------------------------------------------------------
    TColor
    --------------------------------------------------------------------}
  
  TColor = Class(TGoogleBaseObject)
  Private
    Fcolor : String;
    Fopacity : double;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : String); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property color : String Index 0 Read Fcolor Write Setcolor;
    Property opacity : double Index 8 Read Fopacity Write Setopacity;
  end;
  TColorClass = Class of TColor;
  
  { --------------------------------------------------------------------
    TDatasource
    --------------------------------------------------------------------}
  
  TDatasource = Class(TGoogleBaseObject)
  Private
    Fid : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
  end;
  TDatasourceClass = Class of TDatasource;
  
  { --------------------------------------------------------------------
    TDisplayRule
    --------------------------------------------------------------------}
  
  TDisplayRule = Class(TGoogleBaseObject)
  Private
    Ffilters : TDisplayRuleTypefiltersArray;
    FlineOptions : TLineStyle;
    Fname : String;
    FpointOptions : TPointStyle;
    FpolygonOptions : TPolygonStyle;
    FzoomLevels : TZoomLevels;
  Protected
    //Property setters
    Procedure Setfilters(AIndex : Integer; AValue : TDisplayRuleTypefiltersArray); virtual;
    Procedure SetlineOptions(AIndex : Integer; AValue : TLineStyle); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpointOptions(AIndex : Integer; AValue : TPointStyle); virtual;
    Procedure SetpolygonOptions(AIndex : Integer; AValue : TPolygonStyle); virtual;
    Procedure SetzoomLevels(AIndex : Integer; AValue : TZoomLevels); virtual;
  Public
  Published
    Property filters : TDisplayRuleTypefiltersArray Index 0 Read Ffilters Write Setfilters;
    Property lineOptions : TLineStyle Index 8 Read FlineOptions Write SetlineOptions;
    Property name : String Index 16 Read Fname Write Setname;
    Property pointOptions : TPointStyle Index 24 Read FpointOptions Write SetpointOptions;
    Property polygonOptions : TPolygonStyle Index 32 Read FpolygonOptions Write SetpolygonOptions;
    Property zoomLevels : TZoomLevels Index 40 Read FzoomLevels Write SetzoomLevels;
  end;
  TDisplayRuleClass = Class of TDisplayRule;
  
  { --------------------------------------------------------------------
    TFeature
    --------------------------------------------------------------------}
  
  TFeature = Class(TGoogleBaseObject)
  Private
    Fgeometry : TGeoJsonGeometry;
    Fproperties : TGeoJsonProperties;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setgeometry(AIndex : Integer; AValue : TGeoJsonGeometry); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TGeoJsonProperties); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property geometry : TGeoJsonGeometry Index 0 Read Fgeometry Write Setgeometry;
    Property properties : TGeoJsonProperties Index 8 Read Fproperties Write Setproperties;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TFeatureClass = Class of TFeature;
  
  { --------------------------------------------------------------------
    TFeatureInfo
    --------------------------------------------------------------------}
  
  TFeatureInfo = Class(TGoogleBaseObject)
  Private
    Fcontent : String;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property content : String Index 0 Read Fcontent Write Setcontent;
  end;
  TFeatureInfoClass = Class of TFeatureInfo;
  
  { --------------------------------------------------------------------
    TFeaturesBatchDeleteRequest
    --------------------------------------------------------------------}
  
  TFeaturesBatchDeleteRequest = Class(TGoogleBaseObject)
  Private
    Fgx_ids : TStringArray;
    FprimaryKeys : TStringArray;
  Protected
    //Property setters
    Procedure Setgx_ids(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetprimaryKeys(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property gx_ids : TStringArray Index 0 Read Fgx_ids Write Setgx_ids;
    Property primaryKeys : TStringArray Index 8 Read FprimaryKeys Write SetprimaryKeys;
  end;
  TFeaturesBatchDeleteRequestClass = Class of TFeaturesBatchDeleteRequest;
  
  { --------------------------------------------------------------------
    TFeaturesBatchInsertRequest
    --------------------------------------------------------------------}
  
  TFeaturesBatchInsertRequest = Class(TGoogleBaseObject)
  Private
    Ffeatures : TFeaturesBatchInsertRequestTypefeaturesArray;
    FnormalizeGeometries : boolean;
  Protected
    //Property setters
    Procedure Setfeatures(AIndex : Integer; AValue : TFeaturesBatchInsertRequestTypefeaturesArray); virtual;
    Procedure SetnormalizeGeometries(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property features : TFeaturesBatchInsertRequestTypefeaturesArray Index 0 Read Ffeatures Write Setfeatures;
    Property normalizeGeometries : boolean Index 8 Read FnormalizeGeometries Write SetnormalizeGeometries;
  end;
  TFeaturesBatchInsertRequestClass = Class of TFeaturesBatchInsertRequest;
  
  { --------------------------------------------------------------------
    TFeaturesBatchPatchRequest
    --------------------------------------------------------------------}
  
  TFeaturesBatchPatchRequest = Class(TGoogleBaseObject)
  Private
    Ffeatures : TFeaturesBatchPatchRequestTypefeaturesArray;
    FnormalizeGeometries : boolean;
  Protected
    //Property setters
    Procedure Setfeatures(AIndex : Integer; AValue : TFeaturesBatchPatchRequestTypefeaturesArray); virtual;
    Procedure SetnormalizeGeometries(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property features : TFeaturesBatchPatchRequestTypefeaturesArray Index 0 Read Ffeatures Write Setfeatures;
    Property normalizeGeometries : boolean Index 8 Read FnormalizeGeometries Write SetnormalizeGeometries;
  end;
  TFeaturesBatchPatchRequestClass = Class of TFeaturesBatchPatchRequest;
  
  { --------------------------------------------------------------------
    TFeaturesListResponse
    --------------------------------------------------------------------}
  
  TFeaturesListResponse = Class(TGoogleBaseObject)
  Private
    FallowedQueriesPerSecond : double;
    Ffeatures : TFeaturesListResponseTypefeaturesArray;
    FnextPageToken : String;
    Fschema : TSchema;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetallowedQueriesPerSecond(AIndex : Integer; AValue : double); virtual;
    Procedure Setfeatures(AIndex : Integer; AValue : TFeaturesListResponseTypefeaturesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setschema(AIndex : Integer; AValue : TSchema); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property allowedQueriesPerSecond : double Index 0 Read FallowedQueriesPerSecond Write SetallowedQueriesPerSecond;
    Property features : TFeaturesListResponseTypefeaturesArray Index 8 Read Ffeatures Write Setfeatures;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property schema : TSchema Index 24 Read Fschema Write Setschema;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TFeaturesListResponseClass = Class of TFeaturesListResponse;
  
  { --------------------------------------------------------------------
    TFile
    --------------------------------------------------------------------}
  
  TFile = Class(TGoogleBaseObject)
  Private
    Ffilename : String;
    Fsize : String;
    FuploadStatus : String;
  Protected
    //Property setters
    Procedure Setfilename(AIndex : Integer; AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; AValue : String); virtual;
    Procedure SetuploadStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property filename : String Index 0 Read Ffilename Write Setfilename;
    Property size : String Index 8 Read Fsize Write Setsize;
    Property uploadStatus : String Index 16 Read FuploadStatus Write SetuploadStatus;
  end;
  TFileClass = Class of TFile;
  
  { --------------------------------------------------------------------
    TFilter
    --------------------------------------------------------------------}
  
  TFilter = Class(TGoogleBaseObject)
  Private
    Fcolumn : String;
    F_operator : String;
    Fvalue : TJSONSchema;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolumn(AIndex : Integer; AValue : String); virtual;
    Procedure Set_operator(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : TJSONSchema); virtual;
  Public
  Published
    Property column : String Index 0 Read Fcolumn Write Setcolumn;
    Property _operator : String Index 8 Read F_operator Write Set_operator;
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
    Fgeometries : TGeoJsonGeometryCollectionTypegeometriesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setgeometries(AIndex : Integer; AValue : TGeoJsonGeometryCollectionTypegeometriesArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property geometries : TGeoJsonGeometryCollectionTypegeometriesArray Index 0 Read Fgeometries Write Setgeometries;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonGeometryCollectionClass = Class of TGeoJsonGeometryCollection;
  
  { --------------------------------------------------------------------
    TGeoJsonLineString
    --------------------------------------------------------------------}
  
  TGeoJsonLineString = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonLineStringTypecoordinatesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonLineStringTypecoordinatesArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TGeoJsonLineStringTypecoordinatesArray Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonLineStringClass = Class of TGeoJsonLineString;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiLineString
    --------------------------------------------------------------------}
  
  TGeoJsonMultiLineString = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonMultiLineStringTypecoordinatesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiLineStringTypecoordinatesArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TGeoJsonMultiLineStringTypecoordinatesArray Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonMultiLineStringClass = Class of TGeoJsonMultiLineString;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiPoint
    --------------------------------------------------------------------}
  
  TGeoJsonMultiPoint = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonMultiPointTypecoordinatesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiPointTypecoordinatesArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TGeoJsonMultiPointTypecoordinatesArray Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonMultiPointClass = Class of TGeoJsonMultiPoint;
  
  { --------------------------------------------------------------------
    TGeoJsonMultiPolygon
    --------------------------------------------------------------------}
  
  TGeoJsonMultiPolygon = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonMultiPolygonTypecoordinatesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiPolygonTypecoordinatesArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TGeoJsonMultiPolygonTypecoordinatesArray Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonMultiPolygonClass = Class of TGeoJsonMultiPolygon;
  
  { --------------------------------------------------------------------
    TGeoJsonPoint
    --------------------------------------------------------------------}
  
  TGeoJsonPoint = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonPosition;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonPosition); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TGeoJsonPosition Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonPointClass = Class of TGeoJsonPoint;
  
  { --------------------------------------------------------------------
    TGeoJsonPolygon
    --------------------------------------------------------------------}
  
  TGeoJsonPolygon = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TGeoJsonPolygonTypecoordinatesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TGeoJsonPolygonTypecoordinatesArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TGeoJsonPolygonTypecoordinatesArray Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TGeoJsonPolygonClass = Class of TGeoJsonPolygon;
  
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
    Fdescription : String;
    Fid : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property id : String Index 8 Read Fid Write Setid;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TIconClass = Class of TIcon;
  
  { --------------------------------------------------------------------
    TIconStyle
    --------------------------------------------------------------------}
  
  TIconStyle = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : String;
    FscaledShape : TScaledShape;
    FscalingFunction : TScalingFunction;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetscaledShape(AIndex : Integer; AValue : TScaledShape); virtual;
    Procedure SetscalingFunction(AIndex : Integer; AValue : TScalingFunction); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
    Property scaledShape : TScaledShape Index 16 Read FscaledShape Write SetscaledShape;
    Property scalingFunction : TScalingFunction Index 24 Read FscalingFunction Write SetscalingFunction;
  end;
  TIconStyleClass = Class of TIconStyle;
  
  { --------------------------------------------------------------------
    TIconsListResponse
    --------------------------------------------------------------------}
  
  TIconsListResponse = Class(TGoogleBaseObject)
  Private
    Ficons : TIconsListResponseTypeiconsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Seticons(AIndex : Integer; AValue : TIconsListResponseTypeiconsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property icons : TIconsListResponseTypeiconsArray Index 0 Read Ficons Write Seticons;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TIconsListResponseClass = Class of TIconsListResponse;
  
  { --------------------------------------------------------------------
    TLabelStyle
    --------------------------------------------------------------------}
  
  TLabelStyle = Class(TGoogleBaseObject)
  Private
    Fcolor : String;
    Fcolumn : String;
    FfontStyle : String;
    FfontWeight : String;
    Fopacity : double;
    Foutline : TColor;
    Fsize : double;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : String); virtual;
    Procedure Setcolumn(AIndex : Integer; AValue : String); virtual;
    Procedure SetfontStyle(AIndex : Integer; AValue : String); virtual;
    Procedure SetfontWeight(AIndex : Integer; AValue : String); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
    Procedure Setoutline(AIndex : Integer; AValue : TColor); virtual;
    Procedure Setsize(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property color : String Index 0 Read Fcolor Write Setcolor;
    Property column : String Index 8 Read Fcolumn Write Setcolumn;
    Property fontStyle : String Index 16 Read FfontStyle Write SetfontStyle;
    Property fontWeight : String Index 24 Read FfontWeight Write SetfontWeight;
    Property opacity : double Index 32 Read Fopacity Write Setopacity;
    Property outline : TColor Index 40 Read Foutline Write Setoutline;
    Property size : double Index 48 Read Fsize Write Setsize;
  end;
  TLabelStyleClass = Class of TLabelStyle;
  
  { --------------------------------------------------------------------
    TLayer
    --------------------------------------------------------------------}
  
  TLayer = Class(TGoogleBaseObject)
  Private
    Fbbox : TdoubleArray;
    FcreationTime : TDatetime;
    FcreatorEmail : String;
    FdatasourceType : String;
    Fdatasources : TDatasources;
    Fdescription : String;
    FdraftAccessList : String;
    Fetag : String;
    Fid : String;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : String;
    FlayerType : String;
    Fname : String;
    FprocessingStatus : String;
    FprojectId : String;
    FpublishedAccessList : String;
    FpublishingStatus : String;
    Fstyle : TVectorStyle;
    Ftags : TTags;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetdatasourceType(AIndex : Integer; AValue : String); virtual;
    Procedure Setdatasources(AIndex : Integer; AValue : TDatasources); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetlayerType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAccessList(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setstyle(AIndex : Integer; AValue : TVectorStyle); virtual;
    Procedure Settags(AIndex : Integer; AValue : TTags); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bbox : TdoubleArray Index 0 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 8 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : String Index 16 Read FcreatorEmail Write SetcreatorEmail;
    Property datasourceType : String Index 24 Read FdatasourceType Write SetdatasourceType;
    Property datasources : TDatasources Index 32 Read Fdatasources Write Setdatasources;
    Property description : String Index 40 Read Fdescription Write Setdescription;
    Property draftAccessList : String Index 48 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : String Index 56 Read Fetag Write Setetag;
    Property id : String Index 64 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 72 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : String Index 80 Read FlastModifierEmail Write SetlastModifierEmail;
    Property layerType : String Index 88 Read FlayerType Write SetlayerType;
    Property name : String Index 96 Read Fname Write Setname;
    Property processingStatus : String Index 104 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : String Index 112 Read FprojectId Write SetprojectId;
    Property publishedAccessList : String Index 120 Read FpublishedAccessList Write SetpublishedAccessList;
    Property publishingStatus : String Index 128 Read FpublishingStatus Write SetpublishingStatus;
    Property style : TVectorStyle Index 136 Read Fstyle Write Setstyle;
    Property tags : TTags Index 144 Read Ftags Write Settags;
    Property writersCanEditPermissions : boolean Index 152 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TLayerClass = Class of TLayer;
  
  { --------------------------------------------------------------------
    TLayersListResponse
    --------------------------------------------------------------------}
  
  TLayersListResponse = Class(TGoogleBaseObject)
  Private
    Flayers : TLayersListResponseTypelayersArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setlayers(AIndex : Integer; AValue : TLayersListResponseTypelayersArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property layers : TLayersListResponseTypelayersArray Index 0 Read Flayers Write Setlayers;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TLayersListResponseClass = Class of TLayersListResponse;
  
  { --------------------------------------------------------------------
    TLineStyleTypestroke
    --------------------------------------------------------------------}
  
  TLineStyleTypestroke = Class(TGoogleBaseObject)
  Private
    Fcolor : String;
    Fopacity : double;
    Fwidth : double;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : String); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property color : String Index 0 Read Fcolor Write Setcolor;
    Property opacity : double Index 8 Read Fopacity Write Setopacity;
    Property width : double Index 16 Read Fwidth Write Setwidth;
  end;
  TLineStyleTypestrokeClass = Class of TLineStyleTypestroke;
  
  { --------------------------------------------------------------------
    TLineStyle
    --------------------------------------------------------------------}
  
  TLineStyle = Class(TGoogleBaseObject)
  Private
    Fborder : TBorder;
    Fdash : TdoubleArray;
    F_label : TLabelStyle;
    Fstroke : TLineStyleTypestroke;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setborder(AIndex : Integer; AValue : TBorder); virtual;
    Procedure Setdash(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure Set_label(AIndex : Integer; AValue : TLabelStyle); virtual;
    Procedure Setstroke(AIndex : Integer; AValue : TLineStyleTypestroke); virtual;
  Public
  Published
    Property border : TBorder Index 0 Read Fborder Write Setborder;
    Property dash : TdoubleArray Index 8 Read Fdash Write Setdash;
    Property _label : TLabelStyle Index 16 Read F_label Write Set_label;
    Property stroke : TLineStyleTypestroke Index 24 Read Fstroke Write Setstroke;
  end;
  TLineStyleClass = Class of TLineStyle;
  
  { --------------------------------------------------------------------
    TMap
    --------------------------------------------------------------------}
  
  TMap = Class(TGoogleBaseObject)
  Private
    Fbbox : TdoubleArray;
    Fcontents : TMapContents;
    FcreationTime : TDatetime;
    FcreatorEmail : String;
    FdefaultViewport : TLatLngBox;
    Fdescription : String;
    FdraftAccessList : String;
    Fetag : String;
    Fid : String;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : String;
    Fname : String;
    FprocessingStatus : String;
    FprojectId : String;
    FpublishedAccessList : String;
    FpublishingStatus : String;
    Ftags : TTags;
    Fversions : TStringArray;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure Setcontents(AIndex : Integer; AValue : TMapContents); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TLatLngBox); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAccessList(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure Settags(AIndex : Integer; AValue : TTags); virtual;
    Procedure Setversions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bbox : TdoubleArray Index 0 Read Fbbox Write Setbbox;
    Property contents : TMapContents Index 8 Read Fcontents Write Setcontents;
    Property creationTime : TDatetime Index 16 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : String Index 24 Read FcreatorEmail Write SetcreatorEmail;
    Property defaultViewport : TLatLngBox Index 32 Read FdefaultViewport Write SetdefaultViewport;
    Property description : String Index 40 Read Fdescription Write Setdescription;
    Property draftAccessList : String Index 48 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : String Index 56 Read Fetag Write Setetag;
    Property id : String Index 64 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 72 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : String Index 80 Read FlastModifierEmail Write SetlastModifierEmail;
    Property name : String Index 88 Read Fname Write Setname;
    Property processingStatus : String Index 96 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : String Index 104 Read FprojectId Write SetprojectId;
    Property publishedAccessList : String Index 112 Read FpublishedAccessList Write SetpublishedAccessList;
    Property publishingStatus : String Index 120 Read FpublishingStatus Write SetpublishingStatus;
    Property tags : TTags Index 128 Read Ftags Write Settags;
    Property versions : TStringArray Index 136 Read Fversions Write Setversions;
    Property writersCanEditPermissions : boolean Index 144 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TMapClass = Class of TMap;
  
  { --------------------------------------------------------------------
    TMapFolder
    --------------------------------------------------------------------}
  
  TMapFolder = Class(TGoogleBaseObject)
  Private
    Fcontents : TMapFolderTypecontentsArray;
    FdefaultViewport : TdoubleArray;
    Fexpandable : boolean;
    Fkey : String;
    Fname : String;
    F_type : String;
    Fvisibility : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcontents(AIndex : Integer; AValue : TMapFolderTypecontentsArray); virtual;
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure Setexpandable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property contents : TMapFolderTypecontentsArray Index 0 Read Fcontents Write Setcontents;
    Property defaultViewport : TdoubleArray Index 8 Read FdefaultViewport Write SetdefaultViewport;
    Property expandable : boolean Index 16 Read Fexpandable Write Setexpandable;
    Property key : String Index 24 Read Fkey Write Setkey;
    Property name : String Index 32 Read Fname Write Setname;
    Property _type : String Index 40 Read F_type Write Set_type;
    Property visibility : String Index 48 Read Fvisibility Write Setvisibility;
  end;
  TMapFolderClass = Class of TMapFolder;
  
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
    FdefaultViewport : TdoubleArray;
    FkmlUrl : String;
    Fname : String;
    F_type : String;
    Fvisibility : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure SetkmlUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property defaultViewport : TdoubleArray Index 0 Read FdefaultViewport Write SetdefaultViewport;
    Property kmlUrl : String Index 8 Read FkmlUrl Write SetkmlUrl;
    Property name : String Index 16 Read Fname Write Setname;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property visibility : String Index 32 Read Fvisibility Write Setvisibility;
  end;
  TMapKmlLinkClass = Class of TMapKmlLink;
  
  { --------------------------------------------------------------------
    TMapLayer
    --------------------------------------------------------------------}
  
  TMapLayer = Class(TGoogleBaseObject)
  Private
    FdefaultViewport : TdoubleArray;
    Fid : String;
    Fkey : String;
    Fname : String;
    F_type : String;
    Fvisibility : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property defaultViewport : TdoubleArray Index 0 Read FdefaultViewport Write SetdefaultViewport;
    Property id : String Index 8 Read Fid Write Setid;
    Property key : String Index 16 Read Fkey Write Setkey;
    Property name : String Index 24 Read Fname Write Setname;
    Property _type : String Index 32 Read F_type Write Set_type;
    Property visibility : String Index 40 Read Fvisibility Write Setvisibility;
  end;
  TMapLayerClass = Class of TMapLayer;
  
  { --------------------------------------------------------------------
    TMapsListResponse
    --------------------------------------------------------------------}
  
  TMapsListResponse = Class(TGoogleBaseObject)
  Private
    Fmaps : TMapsListResponseTypemapsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setmaps(AIndex : Integer; AValue : TMapsListResponseTypemapsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property maps : TMapsListResponseTypemapsArray Index 0 Read Fmaps Write Setmaps;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TMapsListResponseClass = Class of TMapsListResponse;
  
  { --------------------------------------------------------------------
    TParent
    --------------------------------------------------------------------}
  
  TParent = Class(TGoogleBaseObject)
  Private
    Fid : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
  end;
  TParentClass = Class of TParent;
  
  { --------------------------------------------------------------------
    TParentsListResponse
    --------------------------------------------------------------------}
  
  TParentsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fparents : TParentsListResponseTypeparentsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setparents(AIndex : Integer; AValue : TParentsListResponseTypeparentsArray); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property parents : TParentsListResponseTypeparentsArray Index 8 Read Fparents Write Setparents;
  end;
  TParentsListResponseClass = Class of TParentsListResponse;
  
  { --------------------------------------------------------------------
    TPermission
    --------------------------------------------------------------------}
  
  TPermission = Class(TGoogleBaseObject)
  Private
    Fdiscoverable : boolean;
    Fid : String;
    Frole : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdiscoverable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setrole(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property discoverable : boolean Index 0 Read Fdiscoverable Write Setdiscoverable;
    Property id : String Index 8 Read Fid Write Setid;
    Property role : String Index 16 Read Frole Write Setrole;
    Property _type : String Index 24 Read F_type Write Set_type;
  end;
  TPermissionClass = Class of TPermission;
  
  { --------------------------------------------------------------------
    TPermissionsBatchDeleteRequest
    --------------------------------------------------------------------}
  
  TPermissionsBatchDeleteRequest = Class(TGoogleBaseObject)
  Private
    Fids : TStringArray;
  Protected
    //Property setters
    Procedure Setids(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property ids : TStringArray Index 0 Read Fids Write Setids;
  end;
  TPermissionsBatchDeleteRequestClass = Class of TPermissionsBatchDeleteRequest;
  
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
    Fpermissions : TPermissionsBatchUpdateRequestTypepermissionsArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; AValue : TPermissionsBatchUpdateRequestTypepermissionsArray); virtual;
  Public
  Published
    Property permissions : TPermissionsBatchUpdateRequestTypepermissionsArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TPermissionsBatchUpdateRequestClass = Class of TPermissionsBatchUpdateRequest;
  
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
    Fpermissions : TPermissionsListResponseTypepermissionsArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; AValue : TPermissionsListResponseTypepermissionsArray); virtual;
  Public
  Published
    Property permissions : TPermissionsListResponseTypepermissionsArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TPermissionsListResponseClass = Class of TPermissionsListResponse;
  
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
    Fid : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TProjectClass = Class of TProject;
  
  { --------------------------------------------------------------------
    TProjectsListResponse
    --------------------------------------------------------------------}
  
  TProjectsListResponse = Class(TGoogleBaseObject)
  Private
    Fprojects : TProjectsListResponseTypeprojectsArray;
  Protected
    //Property setters
    Procedure Setprojects(AIndex : Integer; AValue : TProjectsListResponseTypeprojectsArray); virtual;
  Public
  Published
    Property projects : TProjectsListResponseTypeprojectsArray Index 0 Read Fprojects Write Setprojects;
  end;
  TProjectsListResponseClass = Class of TProjectsListResponse;
  
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
    Fdescription : String;
    Fid : String;
    FlayerType : String;
    Fname : String;
    FprojectId : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlayerType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property id : String Index 8 Read Fid Write Setid;
    Property layerType : String Index 16 Read FlayerType Write SetlayerType;
    Property name : String Index 24 Read Fname Write Setname;
    Property projectId : String Index 32 Read FprojectId Write SetprojectId;
  end;
  TPublishedLayerClass = Class of TPublishedLayer;
  
  { --------------------------------------------------------------------
    TPublishedLayersListResponse
    --------------------------------------------------------------------}
  
  TPublishedLayersListResponse = Class(TGoogleBaseObject)
  Private
    Flayers : TPublishedLayersListResponseTypelayersArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setlayers(AIndex : Integer; AValue : TPublishedLayersListResponseTypelayersArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property layers : TPublishedLayersListResponseTypelayersArray Index 0 Read Flayers Write Setlayers;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TPublishedLayersListResponseClass = Class of TPublishedLayersListResponse;
  
  { --------------------------------------------------------------------
    TPublishedMap
    --------------------------------------------------------------------}
  
  TPublishedMap = Class(TGoogleBaseObject)
  Private
    Fcontents : TMapContents;
    FdefaultViewport : TLatLngBox;
    Fdescription : String;
    Fid : String;
    Fname : String;
    FprojectId : String;
  Protected
    //Property setters
    Procedure Setcontents(AIndex : Integer; AValue : TMapContents); virtual;
    Procedure SetdefaultViewport(AIndex : Integer; AValue : TLatLngBox); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property contents : TMapContents Index 0 Read Fcontents Write Setcontents;
    Property defaultViewport : TLatLngBox Index 8 Read FdefaultViewport Write SetdefaultViewport;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property id : String Index 24 Read Fid Write Setid;
    Property name : String Index 32 Read Fname Write Setname;
    Property projectId : String Index 40 Read FprojectId Write SetprojectId;
  end;
  TPublishedMapClass = Class of TPublishedMap;
  
  { --------------------------------------------------------------------
    TPublishedMapsListResponse
    --------------------------------------------------------------------}
  
  TPublishedMapsListResponse = Class(TGoogleBaseObject)
  Private
    Fmaps : TPublishedMapsListResponseTypemapsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setmaps(AIndex : Integer; AValue : TPublishedMapsListResponseTypemapsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property maps : TPublishedMapsListResponseTypemapsArray Index 0 Read Fmaps Write Setmaps;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TPublishedMapsListResponseClass = Class of TPublishedMapsListResponse;
  
  { --------------------------------------------------------------------
    TRaster
    --------------------------------------------------------------------}
  
  TRaster = Class(TGoogleBaseObject)
  Private
    FacquisitionTime : TAcquisitionTime;
    Fattribution : String;
    Fbbox : TdoubleArray;
    FcreationTime : TDatetime;
    FcreatorEmail : String;
    Fdescription : String;
    FdraftAccessList : String;
    Fetag : String;
    Ffiles : TRasterTypefilesArray;
    Fid : String;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : String;
    FmaskType : String;
    Fname : String;
    FprocessingStatus : String;
    FprojectId : String;
    FrasterType : String;
    Ftags : TTags;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure SetacquisitionTime(AIndex : Integer; AValue : TAcquisitionTime); virtual;
    Procedure Setattribution(AIndex : Integer; AValue : String); virtual;
    Procedure Setbbox(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setfiles(AIndex : Integer; AValue : TRasterTypefilesArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaskType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrasterType(AIndex : Integer; AValue : String); virtual;
    Procedure Settags(AIndex : Integer; AValue : TTags); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property acquisitionTime : TAcquisitionTime Index 0 Read FacquisitionTime Write SetacquisitionTime;
    Property attribution : String Index 8 Read Fattribution Write Setattribution;
    Property bbox : TdoubleArray Index 16 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 24 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : String Index 32 Read FcreatorEmail Write SetcreatorEmail;
    Property description : String Index 40 Read Fdescription Write Setdescription;
    Property draftAccessList : String Index 48 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : String Index 56 Read Fetag Write Setetag;
    Property files : TRasterTypefilesArray Index 64 Read Ffiles Write Setfiles;
    Property id : String Index 72 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 80 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : String Index 88 Read FlastModifierEmail Write SetlastModifierEmail;
    Property maskType : String Index 96 Read FmaskType Write SetmaskType;
    Property name : String Index 104 Read Fname Write Setname;
    Property processingStatus : String Index 112 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : String Index 120 Read FprojectId Write SetprojectId;
    Property rasterType : String Index 128 Read FrasterType Write SetrasterType;
    Property tags : TTags Index 136 Read Ftags Write Settags;
    Property writersCanEditPermissions : boolean Index 144 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TRasterClass = Class of TRaster;
  
  { --------------------------------------------------------------------
    TRasterCollection
    --------------------------------------------------------------------}
  
  TRasterCollection = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    Fbbox : TdoubleArray;
    FcreationTime : TDatetime;
    FcreatorEmail : String;
    Fdescription : String;
    FdraftAccessList : String;
    Fetag : String;
    Fid : String;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : String;
    Fmosaic : boolean;
    Fname : String;
    FprocessingStatus : String;
    FprojectId : String;
    FrasterType : String;
    Ftags : TTags;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : String); virtual;
    Procedure Setbbox(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setmosaic(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrasterType(AIndex : Integer; AValue : String); virtual;
    Procedure Settags(AIndex : Integer; AValue : TTags); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property bbox : TdoubleArray Index 8 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 16 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : String Index 24 Read FcreatorEmail Write SetcreatorEmail;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property draftAccessList : String Index 40 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : String Index 48 Read Fetag Write Setetag;
    Property id : String Index 56 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 64 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : String Index 72 Read FlastModifierEmail Write SetlastModifierEmail;
    Property mosaic : boolean Index 80 Read Fmosaic Write Setmosaic;
    Property name : String Index 88 Read Fname Write Setname;
    Property processingStatus : String Index 96 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : String Index 104 Read FprojectId Write SetprojectId;
    Property rasterType : String Index 112 Read FrasterType Write SetrasterType;
    Property tags : TTags Index 120 Read Ftags Write Settags;
    Property writersCanEditPermissions : boolean Index 128 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TRasterCollectionClass = Class of TRasterCollection;
  
  { --------------------------------------------------------------------
    TRasterCollectionsListResponse
    --------------------------------------------------------------------}
  
  TRasterCollectionsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FrasterCollections : TRasterCollectionsListResponseTyperasterCollectionsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetrasterCollections(AIndex : Integer; AValue : TRasterCollectionsListResponseTyperasterCollectionsArray); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property rasterCollections : TRasterCollectionsListResponseTyperasterCollectionsArray Index 8 Read FrasterCollections Write SetrasterCollections;
  end;
  TRasterCollectionsListResponseClass = Class of TRasterCollectionsListResponse;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRaster
    --------------------------------------------------------------------}
  
  TRasterCollectionsRaster = Class(TGoogleBaseObject)
  Private
    Fbbox : TdoubleArray;
    FcreationTime : TDatetime;
    Fdescription : String;
    Fid : String;
    FlastModifiedTime : TDatetime;
    Fname : String;
    FprojectId : String;
    FrasterType : String;
    Ftags : TStringArray;
  Protected
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrasterType(AIndex : Integer; AValue : String); virtual;
    Procedure Settags(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property bbox : TdoubleArray Index 0 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 8 Read FcreationTime Write SetcreationTime;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property id : String Index 24 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 32 Read FlastModifiedTime Write SetlastModifiedTime;
    Property name : String Index 40 Read Fname Write Setname;
    Property projectId : String Index 48 Read FprojectId Write SetprojectId;
    Property rasterType : String Index 56 Read FrasterType Write SetrasterType;
    Property tags : TStringArray Index 64 Read Ftags Write Settags;
  end;
  TRasterCollectionsRasterClass = Class of TRasterCollectionsRaster;
  
  { --------------------------------------------------------------------
    TRasterCollectionsRasterBatchDeleteRequest
    --------------------------------------------------------------------}
  
  TRasterCollectionsRasterBatchDeleteRequest = Class(TGoogleBaseObject)
  Private
    Fids : TStringArray;
  Protected
    //Property setters
    Procedure Setids(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property ids : TStringArray Index 0 Read Fids Write Setids;
  end;
  TRasterCollectionsRasterBatchDeleteRequestClass = Class of TRasterCollectionsRasterBatchDeleteRequest;
  
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
    Fids : TStringArray;
  Protected
    //Property setters
    Procedure Setids(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property ids : TStringArray Index 0 Read Fids Write Setids;
  end;
  TRasterCollectionsRastersBatchInsertRequestClass = Class of TRasterCollectionsRastersBatchInsertRequest;
  
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
    FnextPageToken : String;
    Frasters : TRasterCollectionsRastersListResponseTyperastersArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setrasters(AIndex : Integer; AValue : TRasterCollectionsRastersListResponseTyperastersArray); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property rasters : TRasterCollectionsRastersListResponseTyperastersArray Index 8 Read Frasters Write Setrasters;
  end;
  TRasterCollectionsRastersListResponseClass = Class of TRasterCollectionsRastersListResponse;
  
  { --------------------------------------------------------------------
    TRastersListResponse
    --------------------------------------------------------------------}
  
  TRastersListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Frasters : TRastersListResponseTyperastersArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setrasters(AIndex : Integer; AValue : TRastersListResponseTyperastersArray); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property rasters : TRastersListResponseTyperastersArray Index 8 Read Frasters Write Setrasters;
  end;
  TRastersListResponseClass = Class of TRastersListResponse;
  
  { --------------------------------------------------------------------
    TScaledShape
    --------------------------------------------------------------------}
  
  TScaledShape = Class(TGoogleBaseObject)
  Private
    Fborder : TBorder;
    Ffill : TColor;
    Fshape : String;
  Protected
    //Property setters
    Procedure Setborder(AIndex : Integer; AValue : TBorder); virtual;
    Procedure Setfill(AIndex : Integer; AValue : TColor); virtual;
    Procedure Setshape(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property border : TBorder Index 0 Read Fborder Write Setborder;
    Property fill : TColor Index 8 Read Ffill Write Setfill;
    Property shape : String Index 16 Read Fshape Write Setshape;
  end;
  TScaledShapeClass = Class of TScaledShape;
  
  { --------------------------------------------------------------------
    TScalingFunction
    --------------------------------------------------------------------}
  
  TScalingFunction = Class(TGoogleBaseObject)
  Private
    Fcolumn : String;
    FscalingType : String;
    FsizeRange : TSizeRange;
    FvalueRange : TValueRange;
  Protected
    //Property setters
    Procedure Setcolumn(AIndex : Integer; AValue : String); virtual;
    Procedure SetscalingType(AIndex : Integer; AValue : String); virtual;
    Procedure SetsizeRange(AIndex : Integer; AValue : TSizeRange); virtual;
    Procedure SetvalueRange(AIndex : Integer; AValue : TValueRange); virtual;
  Public
  Published
    Property column : String Index 0 Read Fcolumn Write Setcolumn;
    Property scalingType : String Index 8 Read FscalingType Write SetscalingType;
    Property sizeRange : TSizeRange Index 16 Read FsizeRange Write SetsizeRange;
    Property valueRange : TValueRange Index 24 Read FvalueRange Write SetvalueRange;
  end;
  TScalingFunctionClass = Class of TScalingFunction;
  
  { --------------------------------------------------------------------
    TSchema
    --------------------------------------------------------------------}
  
  TSchema = Class(TGoogleBaseObject)
  Private
    Fcolumns : TSchemaTypecolumnsArray;
    FprimaryGeometry : String;
    FprimaryKey : String;
  Protected
    //Property setters
    Procedure Setcolumns(AIndex : Integer; AValue : TSchemaTypecolumnsArray); virtual;
    Procedure SetprimaryGeometry(AIndex : Integer; AValue : String); virtual;
    Procedure SetprimaryKey(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property columns : TSchemaTypecolumnsArray Index 0 Read Fcolumns Write Setcolumns;
    Property primaryGeometry : String Index 8 Read FprimaryGeometry Write SetprimaryGeometry;
    Property primaryKey : String Index 16 Read FprimaryKey Write SetprimaryKey;
  end;
  TSchemaClass = Class of TSchema;
  
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
    Fbbox : TdoubleArray;
    FcreationTime : TDatetime;
    FcreatorEmail : String;
    Fdescription : String;
    FdraftAccessList : String;
    Fetag : String;
    Ffiles : TTableTypefilesArray;
    Fid : String;
    FlastModifiedTime : TDatetime;
    FlastModifierEmail : String;
    Fname : String;
    FprocessingStatus : String;
    FprojectId : String;
    FpublishedAccessList : String;
    Fschema : TSchema;
    FsourceEncoding : String;
    Ftags : TTags;
    FwritersCanEditPermissions : boolean;
  Protected
    //Property setters
    Procedure Setbbox(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetcreatorEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdraftAccessList(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setfiles(AIndex : Integer; AValue : TTableTypefilesArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastModifierEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublishedAccessList(AIndex : Integer; AValue : String); virtual;
    Procedure Setschema(AIndex : Integer; AValue : TSchema); virtual;
    Procedure SetsourceEncoding(AIndex : Integer; AValue : String); virtual;
    Procedure Settags(AIndex : Integer; AValue : TTags); virtual;
    Procedure SetwritersCanEditPermissions(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bbox : TdoubleArray Index 0 Read Fbbox Write Setbbox;
    Property creationTime : TDatetime Index 8 Read FcreationTime Write SetcreationTime;
    Property creatorEmail : String Index 16 Read FcreatorEmail Write SetcreatorEmail;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property draftAccessList : String Index 32 Read FdraftAccessList Write SetdraftAccessList;
    Property etag : String Index 40 Read Fetag Write Setetag;
    Property files : TTableTypefilesArray Index 48 Read Ffiles Write Setfiles;
    Property id : String Index 56 Read Fid Write Setid;
    Property lastModifiedTime : TDatetime Index 64 Read FlastModifiedTime Write SetlastModifiedTime;
    Property lastModifierEmail : String Index 72 Read FlastModifierEmail Write SetlastModifierEmail;
    Property name : String Index 80 Read Fname Write Setname;
    Property processingStatus : String Index 88 Read FprocessingStatus Write SetprocessingStatus;
    Property projectId : String Index 96 Read FprojectId Write SetprojectId;
    Property publishedAccessList : String Index 104 Read FpublishedAccessList Write SetpublishedAccessList;
    Property schema : TSchema Index 112 Read Fschema Write Setschema;
    Property sourceEncoding : String Index 120 Read FsourceEncoding Write SetsourceEncoding;
    Property tags : TTags Index 128 Read Ftags Write Settags;
    Property writersCanEditPermissions : boolean Index 136 Read FwritersCanEditPermissions Write SetwritersCanEditPermissions;
  end;
  TTableClass = Class of TTable;
  
  { --------------------------------------------------------------------
    TTableColumn
    --------------------------------------------------------------------}
  
  TTableColumn = Class(TGoogleBaseObject)
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
  TTableColumnClass = Class of TTableColumn;
  
  { --------------------------------------------------------------------
    TTablesListResponse
    --------------------------------------------------------------------}
  
  TTablesListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Ftables : TTablesListResponseTypetablesArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Settables(AIndex : Integer; AValue : TTablesListResponseTypetablesArray); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property tables : TTablesListResponseTypetablesArray Index 8 Read Ftables Write Settables;
  end;
  TTablesListResponseClass = Class of TTablesListResponse;
  
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
    FdisplayRules : TVectorStyleTypedisplayRulesArray;
    FfeatureInfo : TFeatureInfo;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdisplayRules(AIndex : Integer; AValue : TVectorStyleTypedisplayRulesArray); virtual;
    Procedure SetfeatureInfo(AIndex : Integer; AValue : TFeatureInfo); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayRules : TVectorStyleTypedisplayRulesArray Index 0 Read FdisplayRules Write SetdisplayRules;
    Property featureInfo : TFeatureInfo Index 8 Read FfeatureInfo Write SetfeatureInfo;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TVectorStyleClass = Class of TVectorStyle;
  
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
    bbox : String;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : String;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : String;
    projectId : String;
    role : String;
    search : String;
    tags : String;
    _type : String;
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
    version : String;
  end;
  
  
  //Optional query Options for TLayersResource, method List
  
  TLayersListOptions = Record
    bbox : String;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : String;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : String;
    processingStatus : String;
    projectId : String;
    role : String;
    search : String;
    tags : String;
  end;
  
  
  //Optional query Options for TLayersResource, method ListPublished
  
  TLayersListPublishedOptions = Record
    maxResults : integer;
    pageToken : String;
    projectId : String;
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
    version : String;
  end;
  
  
  //Optional query Options for TMapsResource, method List
  
  TMapsListOptions = Record
    bbox : String;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : String;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : String;
    processingStatus : String;
    projectId : String;
    role : String;
    search : String;
    tags : String;
  end;
  
  
  //Optional query Options for TMapsResource, method ListPublished
  
  TMapsListPublishedOptions = Record
    maxResults : integer;
    pageToken : String;
    projectId : String;
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
    bbox : String;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : String;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : String;
    processingStatus : String;
    projectId : String;
    role : String;
    search : String;
    tags : String;
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
    bbox : String;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : String;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : String;
    processingStatus : String;
    projectId : String;
    role : String;
    search : String;
    tags : String;
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
    version : String;
  end;
  
  
  //Optional query Options for TTablesResource, method List
  
  TTablesListOptions = Record
    bbox : String;
    createdAfter : TDatetime;
    createdBefore : TDatetime;
    creatorEmail : String;
    maxResults : integer;
    modifiedAfter : TDatetime;
    modifiedBefore : TDatetime;
    pageToken : String;
    processingStatus : String;
    projectId : String;
    role : String;
    search : String;
    tags : String;
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



Procedure TAcquisitionTime.Setprecision(AIndex : Integer; AValue : String); 

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


Procedure TAsset.Setbbox(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TAsset.SetcreatorEmail(AIndex : Integer; AValue : String); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setid(AIndex : Integer; AValue : String); 

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



Procedure TAsset.SetlastModifierEmail(AIndex : Integer; AValue : String); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Setresource(AIndex : Integer; AValue : String); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Settags(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAsset.Set_type(AIndex : Integer; AValue : String); 

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
  TAssetsListResponse
  --------------------------------------------------------------------}


Procedure TAssetsListResponse.Setassets(AIndex : Integer; AValue : TAssetsListResponseTypeassetsArray); 

begin
  If (Fassets=AValue) then exit;
  Fassets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssetsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBorder
  --------------------------------------------------------------------}


Procedure TBorder.Setcolor(AIndex : Integer; AValue : String); 

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


Procedure TColor.Setcolor(AIndex : Integer; AValue : String); 

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


Procedure TDatasource.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDisplayRule
  --------------------------------------------------------------------}


Procedure TDisplayRule.Setfilters(AIndex : Integer; AValue : TDisplayRuleTypefiltersArray); 

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



Procedure TDisplayRule.Setname(AIndex : Integer; AValue : String); 

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



Procedure TFeature.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TFeatureInfo.Setcontent(AIndex : Integer; AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFeaturesBatchDeleteRequest
  --------------------------------------------------------------------}


Procedure TFeaturesBatchDeleteRequest.Setgx_ids(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fgx_ids=AValue) then exit;
  Fgx_ids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesBatchDeleteRequest.SetprimaryKeys(AIndex : Integer; AValue : TStringArray); 

begin
  If (FprimaryKeys=AValue) then exit;
  FprimaryKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFeaturesBatchInsertRequest
  --------------------------------------------------------------------}


Procedure TFeaturesBatchInsertRequest.Setfeatures(AIndex : Integer; AValue : TFeaturesBatchInsertRequestTypefeaturesArray); 

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
  TFeaturesBatchPatchRequest
  --------------------------------------------------------------------}


Procedure TFeaturesBatchPatchRequest.Setfeatures(AIndex : Integer; AValue : TFeaturesBatchPatchRequestTypefeaturesArray); 

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
  TFeaturesListResponse
  --------------------------------------------------------------------}


Procedure TFeaturesListResponse.SetallowedQueriesPerSecond(AIndex : Integer; AValue : double); 

begin
  If (FallowedQueriesPerSecond=AValue) then exit;
  FallowedQueriesPerSecond:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesListResponse.Setfeatures(AIndex : Integer; AValue : TFeaturesListResponseTypefeaturesArray); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeaturesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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



Procedure TFeaturesListResponse.Set_type(AIndex : Integer; AValue : String); 

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
  TFile
  --------------------------------------------------------------------}


Procedure TFile.Setfilename(AIndex : Integer; AValue : String); 

begin
  If (Ffilename=AValue) then exit;
  Ffilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setsize(AIndex : Integer; AValue : String); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetuploadStatus(AIndex : Integer; AValue : String); 

begin
  If (FuploadStatus=AValue) then exit;
  FuploadStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilter
  --------------------------------------------------------------------}


Procedure TFilter.Setcolumn(AIndex : Integer; AValue : String); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Set_operator(AIndex : Integer; AValue : String); 

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


Procedure TGeoJsonGeometryCollection.Setgeometries(AIndex : Integer; AValue : TGeoJsonGeometryCollectionTypegeometriesArray); 

begin
  If (Fgeometries=AValue) then exit;
  Fgeometries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonGeometryCollection.Set_type(AIndex : Integer; AValue : String); 

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
  TGeoJsonLineString
  --------------------------------------------------------------------}


Procedure TGeoJsonLineString.Setcoordinates(AIndex : Integer; AValue : TGeoJsonLineStringTypecoordinatesArray); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonLineString.Set_type(AIndex : Integer; AValue : String); 

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
  TGeoJsonMultiLineString
  --------------------------------------------------------------------}


Procedure TGeoJsonMultiLineString.Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiLineStringTypecoordinatesArray); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonMultiLineString.Set_type(AIndex : Integer; AValue : String); 

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
  TGeoJsonMultiPoint
  --------------------------------------------------------------------}


Procedure TGeoJsonMultiPoint.Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiPointTypecoordinatesArray); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonMultiPoint.Set_type(AIndex : Integer; AValue : String); 

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
  TGeoJsonMultiPolygon
  --------------------------------------------------------------------}


Procedure TGeoJsonMultiPolygon.Setcoordinates(AIndex : Integer; AValue : TGeoJsonMultiPolygonTypecoordinatesArray); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonMultiPolygon.Set_type(AIndex : Integer; AValue : String); 

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
  TGeoJsonPoint
  --------------------------------------------------------------------}


Procedure TGeoJsonPoint.Setcoordinates(AIndex : Integer; AValue : TGeoJsonPosition); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonPoint.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TGeoJsonPolygon.Setcoordinates(AIndex : Integer; AValue : TGeoJsonPolygonTypecoordinatesArray); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoJsonPolygon.Set_type(AIndex : Integer; AValue : String); 

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
  TGeoJsonProperties
  --------------------------------------------------------------------}


Class Function TGeoJsonProperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TIcon
  --------------------------------------------------------------------}


Procedure TIcon.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIcon.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIcon.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIconStyle
  --------------------------------------------------------------------}


Procedure TIconStyle.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIconStyle.Setname(AIndex : Integer; AValue : String); 

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


Procedure TIconsListResponse.Seticons(AIndex : Integer; AValue : TIconsListResponseTypeiconsArray); 

begin
  If (Ficons=AValue) then exit;
  Ficons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIconsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLabelStyle
  --------------------------------------------------------------------}


Procedure TLabelStyle.Setcolor(AIndex : Integer; AValue : String); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.Setcolumn(AIndex : Integer; AValue : String); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.SetfontStyle(AIndex : Integer; AValue : String); 

begin
  If (FfontStyle=AValue) then exit;
  FfontStyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelStyle.SetfontWeight(AIndex : Integer; AValue : String); 

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
  TLayer
  --------------------------------------------------------------------}


Procedure TLayer.Setbbox(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TLayer.SetcreatorEmail(AIndex : Integer; AValue : String); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetdatasourceType(AIndex : Integer; AValue : String); 

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



Procedure TLayer.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetdraftAccessList(AIndex : Integer; AValue : String); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setid(AIndex : Integer; AValue : String); 

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



Procedure TLayer.SetlastModifierEmail(AIndex : Integer; AValue : String); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetlayerType(AIndex : Integer; AValue : String); 

begin
  If (FlayerType=AValue) then exit;
  FlayerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetprocessingStatus(AIndex : Integer; AValue : String); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetpublishedAccessList(AIndex : Integer; AValue : String); 

begin
  If (FpublishedAccessList=AValue) then exit;
  FpublishedAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayer.SetpublishingStatus(AIndex : Integer; AValue : String); 

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



Procedure TLayer.Settags(AIndex : Integer; AValue : TTags); 

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
  TLayersListResponse
  --------------------------------------------------------------------}


Procedure TLayersListResponse.Setlayers(AIndex : Integer; AValue : TLayersListResponseTypelayersArray); 

begin
  If (Flayers=AValue) then exit;
  Flayers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLayersListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLineStyleTypestroke
  --------------------------------------------------------------------}


Procedure TLineStyleTypestroke.Setcolor(AIndex : Integer; AValue : String); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyleTypestroke.Setopacity(AIndex : Integer; AValue : double); 

begin
  If (Fopacity=AValue) then exit;
  Fopacity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyleTypestroke.Setwidth(AIndex : Integer; AValue : double); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLineStyle
  --------------------------------------------------------------------}


Procedure TLineStyle.Setborder(AIndex : Integer; AValue : TBorder); 

begin
  If (Fborder=AValue) then exit;
  Fborder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyle.Setdash(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TLineStyle.Setstroke(AIndex : Integer; AValue : TLineStyleTypestroke); 

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
  TMap
  --------------------------------------------------------------------}


Procedure TMap.Setbbox(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TMap.SetcreatorEmail(AIndex : Integer; AValue : String); 

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



Procedure TMap.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetdraftAccessList(AIndex : Integer; AValue : String); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setid(AIndex : Integer; AValue : String); 

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



Procedure TMap.SetlastModifierEmail(AIndex : Integer; AValue : String); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetprocessingStatus(AIndex : Integer; AValue : String); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetpublishedAccessList(AIndex : Integer; AValue : String); 

begin
  If (FpublishedAccessList=AValue) then exit;
  FpublishedAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.SetpublishingStatus(AIndex : Integer; AValue : String); 

begin
  If (FpublishingStatus=AValue) then exit;
  FpublishingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Settags(AIndex : Integer; AValue : TTags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMap.Setversions(AIndex : Integer; AValue : TStringArray); 

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
  TMapFolder
  --------------------------------------------------------------------}


Procedure TMapFolder.Setcontents(AIndex : Integer; AValue : TMapFolderTypecontentsArray); 

begin
  If (Fcontents=AValue) then exit;
  Fcontents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.SetdefaultViewport(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TMapFolder.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapFolder.Setvisibility(AIndex : Integer; AValue : String); 

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
  TMapItem
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapKmlLink
  --------------------------------------------------------------------}


Procedure TMapKmlLink.SetdefaultViewport(AIndex : Integer; AValue : TdoubleArray); 

begin
  If (FdefaultViewport=AValue) then exit;
  FdefaultViewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapKmlLink.SetkmlUrl(AIndex : Integer; AValue : String); 

begin
  If (FkmlUrl=AValue) then exit;
  FkmlUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapKmlLink.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapKmlLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapKmlLink.Setvisibility(AIndex : Integer; AValue : String); 

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
  TMapLayer
  --------------------------------------------------------------------}


Procedure TMapLayer.SetdefaultViewport(AIndex : Integer; AValue : TdoubleArray); 

begin
  If (FdefaultViewport=AValue) then exit;
  FdefaultViewport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapLayer.Setvisibility(AIndex : Integer; AValue : String); 

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
  TMapsListResponse
  --------------------------------------------------------------------}


Procedure TMapsListResponse.Setmaps(AIndex : Integer; AValue : TMapsListResponseTypemapsArray); 

begin
  If (Fmaps=AValue) then exit;
  Fmaps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParent
  --------------------------------------------------------------------}


Procedure TParent.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParentsListResponse
  --------------------------------------------------------------------}


Procedure TParentsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentsListResponse.Setparents(AIndex : Integer; AValue : TParentsListResponseTypeparentsArray); 

begin
  If (Fparents=AValue) then exit;
  Fparents:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermission
  --------------------------------------------------------------------}


Procedure TPermission.Setdiscoverable(AIndex : Integer; AValue : boolean); 

begin
  If (Fdiscoverable=AValue) then exit;
  Fdiscoverable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setrole(AIndex : Integer; AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TPermissionsBatchDeleteRequest.Setids(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermissionsBatchDeleteResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermissionsBatchUpdateRequest
  --------------------------------------------------------------------}


Procedure TPermissionsBatchUpdateRequest.Setpermissions(AIndex : Integer; AValue : TPermissionsBatchUpdateRequestTypepermissionsArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermissionsBatchUpdateResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermissionsListResponse
  --------------------------------------------------------------------}


Procedure TPermissionsListResponse.Setpermissions(AIndex : Integer; AValue : TPermissionsListResponseTypepermissionsArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





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


Procedure TProject.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsListResponse
  --------------------------------------------------------------------}


Procedure TProjectsListResponse.Setprojects(AIndex : Integer; AValue : TProjectsListResponseTypeprojectsArray); 

begin
  If (Fprojects=AValue) then exit;
  Fprojects:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPublishedLayer
  --------------------------------------------------------------------}


Procedure TPublishedLayer.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayer.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayer.SetlayerType(AIndex : Integer; AValue : String); 

begin
  If (FlayerType=AValue) then exit;
  FlayerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayer.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayer.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishedLayersListResponse
  --------------------------------------------------------------------}


Procedure TPublishedLayersListResponse.Setlayers(AIndex : Integer; AValue : TPublishedLayersListResponseTypelayersArray); 

begin
  If (Flayers=AValue) then exit;
  Flayers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedLayersListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





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



Procedure TPublishedMap.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMap.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMap.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMap.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishedMapsListResponse
  --------------------------------------------------------------------}


Procedure TPublishedMapsListResponse.Setmaps(AIndex : Integer; AValue : TPublishedMapsListResponseTypemapsArray); 

begin
  If (Fmaps=AValue) then exit;
  Fmaps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishedMapsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRaster
  --------------------------------------------------------------------}


Procedure TRaster.SetacquisitionTime(AIndex : Integer; AValue : TAcquisitionTime); 

begin
  If (FacquisitionTime=AValue) then exit;
  FacquisitionTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setattribution(AIndex : Integer; AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setbbox(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TRaster.SetcreatorEmail(AIndex : Integer; AValue : String); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetdraftAccessList(AIndex : Integer; AValue : String); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setfiles(AIndex : Integer; AValue : TRasterTypefilesArray); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setid(AIndex : Integer; AValue : String); 

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



Procedure TRaster.SetlastModifierEmail(AIndex : Integer; AValue : String); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetmaskType(AIndex : Integer; AValue : String); 

begin
  If (FmaskType=AValue) then exit;
  FmaskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetprocessingStatus(AIndex : Integer; AValue : String); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.SetrasterType(AIndex : Integer; AValue : String); 

begin
  If (FrasterType=AValue) then exit;
  FrasterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRaster.Settags(AIndex : Integer; AValue : TTags); 

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
  TRasterCollection
  --------------------------------------------------------------------}


Procedure TRasterCollection.Setattribution(AIndex : Integer; AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setbbox(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TRasterCollection.SetcreatorEmail(AIndex : Integer; AValue : String); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetdraftAccessList(AIndex : Integer; AValue : String); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Setid(AIndex : Integer; AValue : String); 

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



Procedure TRasterCollection.SetlastModifierEmail(AIndex : Integer; AValue : String); 

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



Procedure TRasterCollection.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetprocessingStatus(AIndex : Integer; AValue : String); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.SetrasterType(AIndex : Integer; AValue : String); 

begin
  If (FrasterType=AValue) then exit;
  FrasterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollection.Settags(AIndex : Integer; AValue : TTags); 

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
  TRasterCollectionsListResponse
  --------------------------------------------------------------------}


Procedure TRasterCollectionsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsListResponse.SetrasterCollections(AIndex : Integer; AValue : TRasterCollectionsListResponseTyperasterCollectionsArray); 

begin
  If (FrasterCollections=AValue) then exit;
  FrasterCollections:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsRaster
  --------------------------------------------------------------------}


Procedure TRasterCollectionsRaster.Setbbox(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TRasterCollectionsRaster.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.Setid(AIndex : Integer; AValue : String); 

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



Procedure TRasterCollectionsRaster.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.SetrasterType(AIndex : Integer; AValue : String); 

begin
  If (FrasterType=AValue) then exit;
  FrasterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRaster.Settags(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsRasterBatchDeleteRequest
  --------------------------------------------------------------------}


Procedure TRasterCollectionsRasterBatchDeleteRequest.Setids(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsRastersBatchDeleteResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRastersBatchInsertRequest
  --------------------------------------------------------------------}


Procedure TRasterCollectionsRastersBatchInsertRequest.Setids(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRasterCollectionsRastersBatchInsertResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRasterCollectionsRastersListResponse
  --------------------------------------------------------------------}


Procedure TRasterCollectionsRastersListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRasterCollectionsRastersListResponse.Setrasters(AIndex : Integer; AValue : TRasterCollectionsRastersListResponseTyperastersArray); 

begin
  If (Frasters=AValue) then exit;
  Frasters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRastersListResponse
  --------------------------------------------------------------------}


Procedure TRastersListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRastersListResponse.Setrasters(AIndex : Integer; AValue : TRastersListResponseTyperastersArray); 

begin
  If (Frasters=AValue) then exit;
  Frasters:=AValue;
  MarkPropertyChanged(AIndex);
end;





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



Procedure TScaledShape.Setshape(AIndex : Integer; AValue : String); 

begin
  If (Fshape=AValue) then exit;
  Fshape:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TScalingFunction
  --------------------------------------------------------------------}


Procedure TScalingFunction.Setcolumn(AIndex : Integer; AValue : String); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScalingFunction.SetscalingType(AIndex : Integer; AValue : String); 

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


Procedure TSchema.Setcolumns(AIndex : Integer; AValue : TSchemaTypecolumnsArray); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchema.SetprimaryGeometry(AIndex : Integer; AValue : String); 

begin
  If (FprimaryGeometry=AValue) then exit;
  FprimaryGeometry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchema.SetprimaryKey(AIndex : Integer; AValue : String); 

begin
  If (FprimaryKey=AValue) then exit;
  FprimaryKey:=AValue;
  MarkPropertyChanged(AIndex);
end;





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


Procedure TTable.Setbbox(AIndex : Integer; AValue : TdoubleArray); 

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



Procedure TTable.SetcreatorEmail(AIndex : Integer; AValue : String); 

begin
  If (FcreatorEmail=AValue) then exit;
  FcreatorEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetdraftAccessList(AIndex : Integer; AValue : String); 

begin
  If (FdraftAccessList=AValue) then exit;
  FdraftAccessList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setfiles(AIndex : Integer; AValue : TTableTypefilesArray); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setid(AIndex : Integer; AValue : String); 

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



Procedure TTable.SetlastModifierEmail(AIndex : Integer; AValue : String); 

begin
  If (FlastModifierEmail=AValue) then exit;
  FlastModifierEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetprocessingStatus(AIndex : Integer; AValue : String); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetpublishedAccessList(AIndex : Integer; AValue : String); 

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



Procedure TTable.SetsourceEncoding(AIndex : Integer; AValue : String); 

begin
  If (FsourceEncoding=AValue) then exit;
  FsourceEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Settags(AIndex : Integer; AValue : TTags); 

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
  TTableColumn
  --------------------------------------------------------------------}


Procedure TTableColumn.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableColumn.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TTablesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTablesListResponse.Settables(AIndex : Integer; AValue : TTablesListResponseTypetablesArray); 

begin
  If (Ftables=AValue) then exit;
  Ftables:=AValue;
  MarkPropertyChanged(AIndex);
end;





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


Procedure TVectorStyle.SetdisplayRules(AIndex : Integer; AValue : TVectorStyleTypedisplayRulesArray); 

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



Procedure TVectorStyle.Set_type(AIndex : Integer; AValue : String); 

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
  TAssetsListResponse.RegisterObject;
  TBorder.RegisterObject;
  TColor.RegisterObject;
  TDatasource.RegisterObject;
  TDisplayRule.RegisterObject;
  TFeature.RegisterObject;
  TFeatureInfo.RegisterObject;
  TFeaturesBatchDeleteRequest.RegisterObject;
  TFeaturesBatchInsertRequest.RegisterObject;
  TFeaturesBatchPatchRequest.RegisterObject;
  TFeaturesListResponse.RegisterObject;
  TFile.RegisterObject;
  TFilter.RegisterObject;
  TGeoJsonGeometry.RegisterObject;
  TGeoJsonGeometryCollection.RegisterObject;
  TGeoJsonLineString.RegisterObject;
  TGeoJsonMultiLineString.RegisterObject;
  TGeoJsonMultiPoint.RegisterObject;
  TGeoJsonMultiPolygon.RegisterObject;
  TGeoJsonPoint.RegisterObject;
  TGeoJsonPolygon.RegisterObject;
  TGeoJsonProperties.RegisterObject;
  TIcon.RegisterObject;
  TIconStyle.RegisterObject;
  TIconsListResponse.RegisterObject;
  TLabelStyle.RegisterObject;
  TLayer.RegisterObject;
  TLayersListResponse.RegisterObject;
  TLineStyleTypestroke.RegisterObject;
  TLineStyle.RegisterObject;
  TMap.RegisterObject;
  TMapFolder.RegisterObject;
  TMapItem.RegisterObject;
  TMapKmlLink.RegisterObject;
  TMapLayer.RegisterObject;
  TMapsListResponse.RegisterObject;
  TParent.RegisterObject;
  TParentsListResponse.RegisterObject;
  TPermission.RegisterObject;
  TPermissionsBatchDeleteRequest.RegisterObject;
  TPermissionsBatchDeleteResponse.RegisterObject;
  TPermissionsBatchUpdateRequest.RegisterObject;
  TPermissionsBatchUpdateResponse.RegisterObject;
  TPermissionsListResponse.RegisterObject;
  TPointStyle.RegisterObject;
  TPolygonStyle.RegisterObject;
  TProcessResponse.RegisterObject;
  TProject.RegisterObject;
  TProjectsListResponse.RegisterObject;
  TPublishResponse.RegisterObject;
  TPublishedLayer.RegisterObject;
  TPublishedLayersListResponse.RegisterObject;
  TPublishedMap.RegisterObject;
  TPublishedMapsListResponse.RegisterObject;
  TRaster.RegisterObject;
  TRasterCollection.RegisterObject;
  TRasterCollectionsListResponse.RegisterObject;
  TRasterCollectionsRaster.RegisterObject;
  TRasterCollectionsRasterBatchDeleteRequest.RegisterObject;
  TRasterCollectionsRastersBatchDeleteResponse.RegisterObject;
  TRasterCollectionsRastersBatchInsertRequest.RegisterObject;
  TRasterCollectionsRastersBatchInsertResponse.RegisterObject;
  TRasterCollectionsRastersListResponse.RegisterObject;
  TRastersListResponse.RegisterObject;
  TScaledShape.RegisterObject;
  TScalingFunction.RegisterObject;
  TSchema.RegisterObject;
  TSizeRange.RegisterObject;
  TTable.RegisterObject;
  TTableColumn.RegisterObject;
  TTablesListResponse.RegisterObject;
  TValueRange.RegisterObject;
  TVectorStyle.RegisterObject;
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
