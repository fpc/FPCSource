unit googlefusiontables;
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
//Generated on: 9-5-15 13:22:54
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TBucket = class;
  TColumn = class;
  TColumnList = class;
  TGeometry = class;
  TImport = class;
  TLine = class;
  TLineStyle = class;
  TPoint = class;
  TPointStyle = class;
  TPolygon = class;
  TPolygonStyle = class;
  TSqlresponse = class;
  TStyleFunction = class;
  TStyleSetting = class;
  TStyleSettingList = class;
  TTable = class;
  TTableList = class;
  TTask = class;
  TTaskList = class;
  TTemplate = class;
  TTemplateList = class;
  TBucketArray = Array of TBucket;
  TColumnArray = Array of TColumn;
  TColumnListArray = Array of TColumnList;
  TGeometryArray = Array of TGeometry;
  TImportArray = Array of TImport;
  TLineArray = Array of TLine;
  TLineStyleArray = Array of TLineStyle;
  TPointArray = Array of TPoint;
  TPointStyleArray = Array of TPointStyle;
  TPolygonArray = Array of TPolygon;
  TPolygonStyleArray = Array of TPolygonStyle;
  TSqlresponseArray = Array of TSqlresponse;
  TStyleFunctionArray = Array of TStyleFunction;
  TStyleSettingArray = Array of TStyleSetting;
  TStyleSettingListArray = Array of TStyleSettingList;
  TTableArray = Array of TTable;
  TTableListArray = Array of TTableList;
  TTaskArray = Array of TTask;
  TTaskListArray = Array of TTaskList;
  TTemplateArray = Array of TTemplate;
  TTemplateListArray = Array of TTemplateList;
  //Anonymous types, using auto-generated names
  TColumnTypebaseColumn = class;
  TStyleFunctionTypegradientTypecolorsItem = class;
  TStyleFunctionTypegradient = class;
  TColumnListTypeitemsArray = Array of TColumn;
  TLineTypecoordinatesArray = Array of TdoubleArray;
  TPolygonTypecoordinatesItemArray = Array of TdoubleArray;
  TPolygonTypecoordinatesArray = Array of TPolygonTypecoordinatesItemArray;
  TSqlresponseTyperowsArray = Array of TTJSONSchemaArray;
  TStyleFunctionTypebucketsArray = Array of TBucket;
  TStyleFunctionTypegradientTypecolorsArray = Array of TStyleFunctionTypegradientTypecolorsItem;
  TStyleSettingListTypeitemsArray = Array of TStyleSetting;
  TTableTypecolumnsArray = Array of TColumn;
  TTableListTypeitemsArray = Array of TTable;
  TTaskListTypeitemsArray = Array of TTask;
  TTemplateListTypeitemsArray = Array of TTemplate;
  
  { --------------------------------------------------------------------
    TBucket
    --------------------------------------------------------------------}
  
  TBucket = Class(TGoogleBaseObject)
  Private
    Fcolor : String;
    Ficon : String;
    Fmax : double;
    Fmin : double;
    Fopacity : double;
    Fweight : integer;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : String); virtual;
    Procedure Seticon(AIndex : Integer; AValue : String); virtual;
    Procedure Setmax(AIndex : Integer; AValue : double); virtual;
    Procedure Setmin(AIndex : Integer; AValue : double); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
    Procedure Setweight(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property color : String Index 0 Read Fcolor Write Setcolor;
    Property icon : String Index 8 Read Ficon Write Seticon;
    Property max : double Index 16 Read Fmax Write Setmax;
    Property min : double Index 24 Read Fmin Write Setmin;
    Property opacity : double Index 32 Read Fopacity Write Setopacity;
    Property weight : integer Index 40 Read Fweight Write Setweight;
  end;
  TBucketClass = Class of TBucket;
  
  { --------------------------------------------------------------------
    TColumnTypebaseColumn
    --------------------------------------------------------------------}
  
  TColumnTypebaseColumn = Class(TGoogleBaseObject)
  Private
    FcolumnId : integer;
    FtableIndex : integer;
  Protected
    //Property setters
    Procedure SetcolumnId(AIndex : Integer; AValue : integer); virtual;
    Procedure SettableIndex(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property columnId : integer Index 0 Read FcolumnId Write SetcolumnId;
    Property tableIndex : integer Index 8 Read FtableIndex Write SettableIndex;
  end;
  TColumnTypebaseColumnClass = Class of TColumnTypebaseColumn;
  
  { --------------------------------------------------------------------
    TColumn
    --------------------------------------------------------------------}
  
  TColumn = Class(TGoogleBaseObject)
  Private
    FbaseColumn : TColumnTypebaseColumn;
    FcolumnId : integer;
    FcolumnJsonSchema : String;
    FcolumnPropertiesJson : String;
    Fdescription : String;
    FformatPattern : String;
    FgraphPredicate : String;
    Fkind : String;
    Fname : String;
    F_type : String;
    FvalidValues : TStringArray;
    FvalidateData : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetbaseColumn(AIndex : Integer; AValue : TColumnTypebaseColumn); virtual;
    Procedure SetcolumnId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcolumnJsonSchema(AIndex : Integer; AValue : String); virtual;
    Procedure SetcolumnPropertiesJson(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetformatPattern(AIndex : Integer; AValue : String); virtual;
    Procedure SetgraphPredicate(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure SetvalidValues(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetvalidateData(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property baseColumn : TColumnTypebaseColumn Index 0 Read FbaseColumn Write SetbaseColumn;
    Property columnId : integer Index 8 Read FcolumnId Write SetcolumnId;
    Property columnJsonSchema : String Index 16 Read FcolumnJsonSchema Write SetcolumnJsonSchema;
    Property columnPropertiesJson : String Index 24 Read FcolumnPropertiesJson Write SetcolumnPropertiesJson;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property formatPattern : String Index 40 Read FformatPattern Write SetformatPattern;
    Property graphPredicate : String Index 48 Read FgraphPredicate Write SetgraphPredicate;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property name : String Index 64 Read Fname Write Setname;
    Property _type : String Index 72 Read F_type Write Set_type;
    Property validValues : TStringArray Index 80 Read FvalidValues Write SetvalidValues;
    Property validateData : boolean Index 88 Read FvalidateData Write SetvalidateData;
  end;
  TColumnClass = Class of TColumn;
  
  { --------------------------------------------------------------------
    TColumnList
    --------------------------------------------------------------------}
  
  TColumnList = Class(TGoogleBaseObject)
  Private
    Fitems : TColumnListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TColumnListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TColumnListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TColumnListClass = Class of TColumnList;
  
  { --------------------------------------------------------------------
    TGeometry
    --------------------------------------------------------------------}
  
  TGeometry = Class(TGoogleBaseObject)
  Private
    Fgeometries : TTJSONSchemaArray;
    Fgeometry : TJSONSchema;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setgeometries(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    Procedure Setgeometry(AIndex : Integer; AValue : TJSONSchema); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property geometries : TTJSONSchemaArray Index 0 Read Fgeometries Write Setgeometries;
    Property geometry : TJSONSchema Index 8 Read Fgeometry Write Setgeometry;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TGeometryClass = Class of TGeometry;
  
  { --------------------------------------------------------------------
    TImport
    --------------------------------------------------------------------}
  
  TImport = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnumRowsReceived : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumRowsReceived(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property numRowsReceived : String Index 8 Read FnumRowsReceived Write SetnumRowsReceived;
  end;
  TImportClass = Class of TImport;
  
  { --------------------------------------------------------------------
    TLine
    --------------------------------------------------------------------}
  
  TLine = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TLineTypecoordinatesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TLineTypecoordinatesArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TLineTypecoordinatesArray Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TLineClass = Class of TLine;
  
  { --------------------------------------------------------------------
    TLineStyle
    --------------------------------------------------------------------}
  
  TLineStyle = Class(TGoogleBaseObject)
  Private
    FstrokeColor : String;
    FstrokeColorStyler : TStyleFunction;
    FstrokeOpacity : double;
    FstrokeWeight : integer;
    FstrokeWeightStyler : TStyleFunction;
  Protected
    //Property setters
    Procedure SetstrokeColor(AIndex : Integer; AValue : String); virtual;
    Procedure SetstrokeColorStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
    Procedure SetstrokeOpacity(AIndex : Integer; AValue : double); virtual;
    Procedure SetstrokeWeight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstrokeWeightStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
  Public
  Published
    Property strokeColor : String Index 0 Read FstrokeColor Write SetstrokeColor;
    Property strokeColorStyler : TStyleFunction Index 8 Read FstrokeColorStyler Write SetstrokeColorStyler;
    Property strokeOpacity : double Index 16 Read FstrokeOpacity Write SetstrokeOpacity;
    Property strokeWeight : integer Index 24 Read FstrokeWeight Write SetstrokeWeight;
    Property strokeWeightStyler : TStyleFunction Index 32 Read FstrokeWeightStyler Write SetstrokeWeightStyler;
  end;
  TLineStyleClass = Class of TLineStyle;
  
  { --------------------------------------------------------------------
    TPoint
    --------------------------------------------------------------------}
  
  TPoint = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TdoubleArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TdoubleArray Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TPointClass = Class of TPoint;
  
  { --------------------------------------------------------------------
    TPointStyle
    --------------------------------------------------------------------}
  
  TPointStyle = Class(TGoogleBaseObject)
  Private
    FiconName : String;
    FiconStyler : TStyleFunction;
  Protected
    //Property setters
    Procedure SeticonName(AIndex : Integer; AValue : String); virtual;
    Procedure SeticonStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
  Public
  Published
    Property iconName : String Index 0 Read FiconName Write SeticonName;
    Property iconStyler : TStyleFunction Index 8 Read FiconStyler Write SeticonStyler;
  end;
  TPointStyleClass = Class of TPointStyle;
  
  { --------------------------------------------------------------------
    TPolygon
    --------------------------------------------------------------------}
  
  TPolygon = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TPolygonTypecoordinatesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TPolygonTypecoordinatesArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coordinates : TPolygonTypecoordinatesArray Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TPolygonClass = Class of TPolygon;
  
  { --------------------------------------------------------------------
    TPolygonStyle
    --------------------------------------------------------------------}
  
  TPolygonStyle = Class(TGoogleBaseObject)
  Private
    FfillColor : String;
    FfillColorStyler : TStyleFunction;
    FfillOpacity : double;
    FstrokeColor : String;
    FstrokeColorStyler : TStyleFunction;
    FstrokeOpacity : double;
    FstrokeWeight : integer;
    FstrokeWeightStyler : TStyleFunction;
  Protected
    //Property setters
    Procedure SetfillColor(AIndex : Integer; AValue : String); virtual;
    Procedure SetfillColorStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
    Procedure SetfillOpacity(AIndex : Integer; AValue : double); virtual;
    Procedure SetstrokeColor(AIndex : Integer; AValue : String); virtual;
    Procedure SetstrokeColorStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
    Procedure SetstrokeOpacity(AIndex : Integer; AValue : double); virtual;
    Procedure SetstrokeWeight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstrokeWeightStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
  Public
  Published
    Property fillColor : String Index 0 Read FfillColor Write SetfillColor;
    Property fillColorStyler : TStyleFunction Index 8 Read FfillColorStyler Write SetfillColorStyler;
    Property fillOpacity : double Index 16 Read FfillOpacity Write SetfillOpacity;
    Property strokeColor : String Index 24 Read FstrokeColor Write SetstrokeColor;
    Property strokeColorStyler : TStyleFunction Index 32 Read FstrokeColorStyler Write SetstrokeColorStyler;
    Property strokeOpacity : double Index 40 Read FstrokeOpacity Write SetstrokeOpacity;
    Property strokeWeight : integer Index 48 Read FstrokeWeight Write SetstrokeWeight;
    Property strokeWeightStyler : TStyleFunction Index 56 Read FstrokeWeightStyler Write SetstrokeWeightStyler;
  end;
  TPolygonStyleClass = Class of TPolygonStyle;
  
  { --------------------------------------------------------------------
    TSqlresponse
    --------------------------------------------------------------------}
  
  TSqlresponse = Class(TGoogleBaseObject)
  Private
    Fcolumns : TStringArray;
    Fkind : String;
    Frows : TSqlresponseTyperowsArray;
  Protected
    //Property setters
    Procedure Setcolumns(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TSqlresponseTyperowsArray); virtual;
  Public
  Published
    Property columns : TStringArray Index 0 Read Fcolumns Write Setcolumns;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property rows : TSqlresponseTyperowsArray Index 16 Read Frows Write Setrows;
  end;
  TSqlresponseClass = Class of TSqlresponse;
  
  { --------------------------------------------------------------------
    TStyleFunctionTypegradientTypecolorsItem
    --------------------------------------------------------------------}
  
  TStyleFunctionTypegradientTypecolorsItem = Class(TGoogleBaseObject)
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
  TStyleFunctionTypegradientTypecolorsItemClass = Class of TStyleFunctionTypegradientTypecolorsItem;
  
  { --------------------------------------------------------------------
    TStyleFunctionTypegradient
    --------------------------------------------------------------------}
  
  TStyleFunctionTypegradient = Class(TGoogleBaseObject)
  Private
    Fcolors : TStyleFunctionTypegradientTypecolorsArray;
    Fmax : double;
    Fmin : double;
  Protected
    //Property setters
    Procedure Setcolors(AIndex : Integer; AValue : TStyleFunctionTypegradientTypecolorsArray); virtual;
    Procedure Setmax(AIndex : Integer; AValue : double); virtual;
    Procedure Setmin(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property colors : TStyleFunctionTypegradientTypecolorsArray Index 0 Read Fcolors Write Setcolors;
    Property max : double Index 8 Read Fmax Write Setmax;
    Property min : double Index 16 Read Fmin Write Setmin;
  end;
  TStyleFunctionTypegradientClass = Class of TStyleFunctionTypegradient;
  
  { --------------------------------------------------------------------
    TStyleFunction
    --------------------------------------------------------------------}
  
  TStyleFunction = Class(TGoogleBaseObject)
  Private
    Fbuckets : TStyleFunctionTypebucketsArray;
    FcolumnName : String;
    Fgradient : TStyleFunctionTypegradient;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setbuckets(AIndex : Integer; AValue : TStyleFunctionTypebucketsArray); virtual;
    Procedure SetcolumnName(AIndex : Integer; AValue : String); virtual;
    Procedure Setgradient(AIndex : Integer; AValue : TStyleFunctionTypegradient); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property buckets : TStyleFunctionTypebucketsArray Index 0 Read Fbuckets Write Setbuckets;
    Property columnName : String Index 8 Read FcolumnName Write SetcolumnName;
    Property gradient : TStyleFunctionTypegradient Index 16 Read Fgradient Write Setgradient;
    Property kind : String Index 24 Read Fkind Write Setkind;
  end;
  TStyleFunctionClass = Class of TStyleFunction;
  
  { --------------------------------------------------------------------
    TStyleSetting
    --------------------------------------------------------------------}
  
  TStyleSetting = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FmarkerOptions : TPointStyle;
    Fname : String;
    FpolygonOptions : TPolygonStyle;
    FpolylineOptions : TLineStyle;
    FstyleId : integer;
    FtableId : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmarkerOptions(AIndex : Integer; AValue : TPointStyle); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpolygonOptions(AIndex : Integer; AValue : TPolygonStyle); virtual;
    Procedure SetpolylineOptions(AIndex : Integer; AValue : TLineStyle); virtual;
    Procedure SetstyleId(AIndex : Integer; AValue : integer); virtual;
    Procedure SettableId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property markerOptions : TPointStyle Index 8 Read FmarkerOptions Write SetmarkerOptions;
    Property name : String Index 16 Read Fname Write Setname;
    Property polygonOptions : TPolygonStyle Index 24 Read FpolygonOptions Write SetpolygonOptions;
    Property polylineOptions : TLineStyle Index 32 Read FpolylineOptions Write SetpolylineOptions;
    Property styleId : integer Index 40 Read FstyleId Write SetstyleId;
    Property tableId : String Index 48 Read FtableId Write SettableId;
  end;
  TStyleSettingClass = Class of TStyleSetting;
  
  { --------------------------------------------------------------------
    TStyleSettingList
    --------------------------------------------------------------------}
  
  TStyleSettingList = Class(TGoogleBaseObject)
  Private
    Fitems : TStyleSettingListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TStyleSettingListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TStyleSettingListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TStyleSettingListClass = Class of TStyleSettingList;
  
  { --------------------------------------------------------------------
    TTable
    --------------------------------------------------------------------}
  
  TTable = Class(TGoogleBaseObject)
  Private
    Fattribution : String;
    FattributionLink : String;
    FbaseTableIds : TStringArray;
    FcolumnPropertiesJsonSchema : String;
    Fcolumns : TTableTypecolumnsArray;
    Fdescription : String;
    FisExportable : boolean;
    Fkind : String;
    Fname : String;
    Fsql : String;
    FtableId : String;
    FtablePropertiesJson : String;
    FtablePropertiesJsonSchema : String;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : String); virtual;
    Procedure SetattributionLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetbaseTableIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetcolumnPropertiesJsonSchema(AIndex : Integer; AValue : String); virtual;
    Procedure Setcolumns(AIndex : Integer; AValue : TTableTypecolumnsArray); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetisExportable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setsql(AIndex : Integer; AValue : String); virtual;
    Procedure SettableId(AIndex : Integer; AValue : String); virtual;
    Procedure SettablePropertiesJson(AIndex : Integer; AValue : String); virtual;
    Procedure SettablePropertiesJsonSchema(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property attribution : String Index 0 Read Fattribution Write Setattribution;
    Property attributionLink : String Index 8 Read FattributionLink Write SetattributionLink;
    Property baseTableIds : TStringArray Index 16 Read FbaseTableIds Write SetbaseTableIds;
    Property columnPropertiesJsonSchema : String Index 24 Read FcolumnPropertiesJsonSchema Write SetcolumnPropertiesJsonSchema;
    Property columns : TTableTypecolumnsArray Index 32 Read Fcolumns Write Setcolumns;
    Property description : String Index 40 Read Fdescription Write Setdescription;
    Property isExportable : boolean Index 48 Read FisExportable Write SetisExportable;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property name : String Index 64 Read Fname Write Setname;
    Property sql : String Index 72 Read Fsql Write Setsql;
    Property tableId : String Index 80 Read FtableId Write SettableId;
    Property tablePropertiesJson : String Index 88 Read FtablePropertiesJson Write SettablePropertiesJson;
    Property tablePropertiesJsonSchema : String Index 96 Read FtablePropertiesJsonSchema Write SettablePropertiesJsonSchema;
  end;
  TTableClass = Class of TTable;
  
  { --------------------------------------------------------------------
    TTableList
    --------------------------------------------------------------------}
  
  TTableList = Class(TGoogleBaseObject)
  Private
    Fitems : TTableListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTableListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TTableListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TTableListClass = Class of TTableList;
  
  { --------------------------------------------------------------------
    TTask
    --------------------------------------------------------------------}
  
  TTask = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fprogress : String;
    Fstarted : boolean;
    FtaskId : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : String); virtual;
    Procedure Setstarted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SettaskId(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property progress : String Index 8 Read Fprogress Write Setprogress;
    Property started : boolean Index 16 Read Fstarted Write Setstarted;
    Property taskId : String Index 24 Read FtaskId Write SettaskId;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TTaskClass = Class of TTask;
  
  { --------------------------------------------------------------------
    TTaskList
    --------------------------------------------------------------------}
  
  TTaskList = Class(TGoogleBaseObject)
  Private
    Fitems : TTaskListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTaskListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TTaskListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TTaskListClass = Class of TTaskList;
  
  { --------------------------------------------------------------------
    TTemplate
    --------------------------------------------------------------------}
  
  TTemplate = Class(TGoogleBaseObject)
  Private
    FautomaticColumnNames : TStringArray;
    Fbody : String;
    Fkind : String;
    Fname : String;
    FtableId : String;
    FtemplateId : integer;
  Protected
    //Property setters
    Procedure SetautomaticColumnNames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setbody(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SettableId(AIndex : Integer; AValue : String); virtual;
    Procedure SettemplateId(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property automaticColumnNames : TStringArray Index 0 Read FautomaticColumnNames Write SetautomaticColumnNames;
    Property body : String Index 8 Read Fbody Write Setbody;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
    Property tableId : String Index 32 Read FtableId Write SettableId;
    Property templateId : integer Index 40 Read FtemplateId Write SettemplateId;
  end;
  TTemplateClass = Class of TTemplate;
  
  { --------------------------------------------------------------------
    TTemplateList
    --------------------------------------------------------------------}
  
  TTemplateList = Class(TGoogleBaseObject)
  Private
    Fitems : TTemplateListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTemplateListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TTemplateListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TTemplateListClass = Class of TTemplateList;
  
  { --------------------------------------------------------------------
    TColumnResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TColumnResource, method List
  
  TColumnListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TColumnResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(columnId: string; tableId: string);
    Function Get(columnId: string; tableId: string) : TColumn;
    Function Insert(tableId: string; aColumn : TColumn) : TColumn;
    Function List(tableId: string; AQuery : string  = '') : TColumnList;
    Function List(tableId: string; AQuery : TColumnlistOptions) : TColumnList;
    Function Patch(columnId: string; tableId: string; aColumn : TColumn) : TColumn;
    Function Update(columnId: string; tableId: string; aColumn : TColumn) : TColumn;
  end;
  
  
  { --------------------------------------------------------------------
    TQueryResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TQueryResource, method Sql
  
  TQuerySqlOptions = Record
    hdrs : boolean;
    sql : String;
    typed : boolean;
  end;
  
  
  //Optional query Options for TQueryResource, method SqlGet
  
  TQuerySqlGetOptions = Record
    hdrs : boolean;
    sql : String;
    typed : boolean;
  end;
  
  TQueryResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Sql(AQuery : string  = '') : TSqlresponse;
    Function Sql(AQuery : TQuerysqlOptions) : TSqlresponse;
    Function SqlGet(AQuery : string  = '') : TSqlresponse;
    Function SqlGet(AQuery : TQuerysqlGetOptions) : TSqlresponse;
  end;
  
  
  { --------------------------------------------------------------------
    TStyleResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TStyleResource, method List
  
  TStyleListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TStyleResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(styleId: integer; tableId: string);
    Function Get(styleId: integer; tableId: string) : TStyleSetting;
    Function Insert(tableId: string; aStyleSetting : TStyleSetting) : TStyleSetting;
    Function List(tableId: string; AQuery : string  = '') : TStyleSettingList;
    Function List(tableId: string; AQuery : TStylelistOptions) : TStyleSettingList;
    Function Patch(styleId: integer; tableId: string; aStyleSetting : TStyleSetting) : TStyleSetting;
    Function Update(styleId: integer; tableId: string; aStyleSetting : TStyleSetting) : TStyleSetting;
  end;
  
  
  { --------------------------------------------------------------------
    TTableResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTableResource, method Copy
  
  TTableCopyOptions = Record
    copyPresentation : boolean;
  end;
  
  
  //Optional query Options for TTableResource, method ImportRows
  
  TTableImportRowsOptions = Record
    delimiter : String;
    encoding : String;
    endLine : integer;
    isStrict : boolean;
    startLine : integer;
  end;
  
  
  //Optional query Options for TTableResource, method ImportTable
  
  TTableImportTableOptions = Record
    delimiter : String;
    encoding : String;
    _name : String;
  end;
  
  
  //Optional query Options for TTableResource, method List
  
  TTableListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TTableResource, method Patch
  
  TTablePatchOptions = Record
    replaceViewDefinition : boolean;
  end;
  
  
  //Optional query Options for TTableResource, method ReplaceRows
  
  TTableReplaceRowsOptions = Record
    delimiter : String;
    encoding : String;
    endLine : integer;
    isStrict : boolean;
    startLine : integer;
  end;
  
  
  //Optional query Options for TTableResource, method Update
  
  TTableUpdateOptions = Record
    replaceViewDefinition : boolean;
  end;
  
  TTableResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Copy(tableId: string; AQuery : string  = '') : TTable;
    Function Copy(tableId: string; AQuery : TTablecopyOptions) : TTable;
    Procedure Delete(tableId: string);
    Function Get(tableId: string) : TTable;
    Function ImportRows(tableId: string; AQuery : string  = '') : TImport;
    Function ImportRows(tableId: string; AQuery : TTableimportRowsOptions) : TImport;
    Function ImportTable(AQuery : string  = '') : TTable;
    Function ImportTable(AQuery : TTableimportTableOptions) : TTable;
    Function Insert(aTable : TTable) : TTable;
    Function List(AQuery : string  = '') : TTableList;
    Function List(AQuery : TTablelistOptions) : TTableList;
    Function Patch(tableId: string; aTable : TTable; AQuery : string  = '') : TTable;
    Function Patch(tableId: string; aTable : TTable; AQuery : TTablepatchOptions) : TTable;
    Function ReplaceRows(tableId: string; AQuery : string  = '') : TTask;
    Function ReplaceRows(tableId: string; AQuery : TTablereplaceRowsOptions) : TTask;
    Function Update(tableId: string; aTable : TTable; AQuery : string  = '') : TTable;
    Function Update(tableId: string; aTable : TTable; AQuery : TTableupdateOptions) : TTable;
  end;
  
  
  { --------------------------------------------------------------------
    TTaskResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTaskResource, method List
  
  TTaskListOptions = Record
    maxResults : integer;
    pageToken : String;
    startIndex : integer;
  end;
  
  TTaskResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(tableId: string; taskId: string);
    Function Get(tableId: string; taskId: string) : TTask;
    Function List(tableId: string; AQuery : string  = '') : TTaskList;
    Function List(tableId: string; AQuery : TTasklistOptions) : TTaskList;
  end;
  
  
  { --------------------------------------------------------------------
    TTemplateResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTemplateResource, method List
  
  TTemplateListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TTemplateResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(tableId: string; templateId: integer);
    Function Get(tableId: string; templateId: integer) : TTemplate;
    Function Insert(tableId: string; aTemplate : TTemplate) : TTemplate;
    Function List(tableId: string; AQuery : string  = '') : TTemplateList;
    Function List(tableId: string; AQuery : TTemplatelistOptions) : TTemplateList;
    Function Patch(tableId: string; templateId: integer; aTemplate : TTemplate) : TTemplate;
    Function Update(tableId: string; templateId: integer; aTemplate : TTemplate) : TTemplate;
  end;
  
  
  { --------------------------------------------------------------------
    TFusiontablesAPI
    --------------------------------------------------------------------}
  
  TFusiontablesAPI = Class(TGoogleAPI)
  Private
    FColumnInstance : TColumnResource;
    FQueryInstance : TQueryResource;
    FStyleInstance : TStyleResource;
    FTableInstance : TTableResource;
    FTaskInstance : TTaskResource;
    FTemplateInstance : TTemplateResource;
    Function GetColumnInstance : TColumnResource;virtual;
    Function GetQueryInstance : TQueryResource;virtual;
    Function GetStyleInstance : TStyleResource;virtual;
    Function GetTableInstance : TTableResource;virtual;
    Function GetTaskInstance : TTaskResource;virtual;
    Function GetTemplateInstance : TTemplateResource;virtual;
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
    Function CreateColumnResource(AOwner : TComponent) : TColumnResource;virtual;overload;
    Function CreateColumnResource : TColumnResource;virtual;overload;
    Function CreateQueryResource(AOwner : TComponent) : TQueryResource;virtual;overload;
    Function CreateQueryResource : TQueryResource;virtual;overload;
    Function CreateStyleResource(AOwner : TComponent) : TStyleResource;virtual;overload;
    Function CreateStyleResource : TStyleResource;virtual;overload;
    Function CreateTableResource(AOwner : TComponent) : TTableResource;virtual;overload;
    Function CreateTableResource : TTableResource;virtual;overload;
    Function CreateTaskResource(AOwner : TComponent) : TTaskResource;virtual;overload;
    Function CreateTaskResource : TTaskResource;virtual;overload;
    Function CreateTemplateResource(AOwner : TComponent) : TTemplateResource;virtual;overload;
    Function CreateTemplateResource : TTemplateResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ColumnResource : TColumnResource Read GetColumnInstance;
    Property QueryResource : TQueryResource Read GetQueryInstance;
    Property StyleResource : TStyleResource Read GetStyleInstance;
    Property TableResource : TTableResource Read GetTableInstance;
    Property TaskResource : TTaskResource Read GetTaskInstance;
    Property TemplateResource : TTemplateResource Read GetTemplateInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TBucket
  --------------------------------------------------------------------}


Procedure TBucket.Setcolor(AIndex : Integer; AValue : String); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Seticon(AIndex : Integer; AValue : String); 

begin
  If (Ficon=AValue) then exit;
  Ficon:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setmax(AIndex : Integer; AValue : double); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setmin(AIndex : Integer; AValue : double); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setopacity(AIndex : Integer; AValue : double); 

begin
  If (Fopacity=AValue) then exit;
  Fopacity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setweight(AIndex : Integer; AValue : integer); 

begin
  If (Fweight=AValue) then exit;
  Fweight:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColumnTypebaseColumn
  --------------------------------------------------------------------}


Procedure TColumnTypebaseColumn.SetcolumnId(AIndex : Integer; AValue : integer); 

begin
  If (FcolumnId=AValue) then exit;
  FcolumnId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumnTypebaseColumn.SettableIndex(AIndex : Integer; AValue : integer); 

begin
  If (FtableIndex=AValue) then exit;
  FtableIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColumn
  --------------------------------------------------------------------}


Procedure TColumn.SetbaseColumn(AIndex : Integer; AValue : TColumnTypebaseColumn); 

begin
  If (FbaseColumn=AValue) then exit;
  FbaseColumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetcolumnId(AIndex : Integer; AValue : integer); 

begin
  If (FcolumnId=AValue) then exit;
  FcolumnId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetcolumnJsonSchema(AIndex : Integer; AValue : String); 

begin
  If (FcolumnJsonSchema=AValue) then exit;
  FcolumnJsonSchema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetcolumnPropertiesJson(AIndex : Integer; AValue : String); 

begin
  If (FcolumnPropertiesJson=AValue) then exit;
  FcolumnPropertiesJson:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetformatPattern(AIndex : Integer; AValue : String); 

begin
  If (FformatPattern=AValue) then exit;
  FformatPattern:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetgraphPredicate(AIndex : Integer; AValue : String); 

begin
  If (FgraphPredicate=AValue) then exit;
  FgraphPredicate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetvalidValues(AIndex : Integer; AValue : TStringArray); 

begin
  If (FvalidValues=AValue) then exit;
  FvalidValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetvalidateData(AIndex : Integer; AValue : boolean); 

begin
  If (FvalidateData=AValue) then exit;
  FvalidateData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TColumn.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TColumnList
  --------------------------------------------------------------------}


Procedure TColumnList.Setitems(AIndex : Integer; AValue : TColumnListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumnList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumnList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumnList.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeometry
  --------------------------------------------------------------------}


Procedure TGeometry.Setgeometries(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (Fgeometries=AValue) then exit;
  Fgeometries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeometry.Setgeometry(AIndex : Integer; AValue : TJSONSchema); 

begin
  If (Fgeometry=AValue) then exit;
  Fgeometry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeometry.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGeometry.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TImport
  --------------------------------------------------------------------}


Procedure TImport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImport.SetnumRowsReceived(AIndex : Integer; AValue : String); 

begin
  If (FnumRowsReceived=AValue) then exit;
  FnumRowsReceived:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLine
  --------------------------------------------------------------------}


Procedure TLine.Setcoordinates(AIndex : Integer; AValue : TLineTypecoordinatesArray); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLine.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TLine.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TLineStyle
  --------------------------------------------------------------------}


Procedure TLineStyle.SetstrokeColor(AIndex : Integer; AValue : String); 

begin
  If (FstrokeColor=AValue) then exit;
  FstrokeColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyle.SetstrokeColorStyler(AIndex : Integer; AValue : TStyleFunction); 

begin
  If (FstrokeColorStyler=AValue) then exit;
  FstrokeColorStyler:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyle.SetstrokeOpacity(AIndex : Integer; AValue : double); 

begin
  If (FstrokeOpacity=AValue) then exit;
  FstrokeOpacity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyle.SetstrokeWeight(AIndex : Integer; AValue : integer); 

begin
  If (FstrokeWeight=AValue) then exit;
  FstrokeWeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLineStyle.SetstrokeWeightStyler(AIndex : Integer; AValue : TStyleFunction); 

begin
  If (FstrokeWeightStyler=AValue) then exit;
  FstrokeWeightStyler:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPoint
  --------------------------------------------------------------------}


Procedure TPoint.Setcoordinates(AIndex : Integer; AValue : TdoubleArray); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPoint.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPointStyle
  --------------------------------------------------------------------}


Procedure TPointStyle.SeticonName(AIndex : Integer; AValue : String); 

begin
  If (FiconName=AValue) then exit;
  FiconName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPointStyle.SeticonStyler(AIndex : Integer; AValue : TStyleFunction); 

begin
  If (FiconStyler=AValue) then exit;
  FiconStyler:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPolygon
  --------------------------------------------------------------------}


Procedure TPolygon.Setcoordinates(AIndex : Integer; AValue : TPolygonTypecoordinatesArray); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygon.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPolygon.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPolygonStyle
  --------------------------------------------------------------------}


Procedure TPolygonStyle.SetfillColor(AIndex : Integer; AValue : String); 

begin
  If (FfillColor=AValue) then exit;
  FfillColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.SetfillColorStyler(AIndex : Integer; AValue : TStyleFunction); 

begin
  If (FfillColorStyler=AValue) then exit;
  FfillColorStyler:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.SetfillOpacity(AIndex : Integer; AValue : double); 

begin
  If (FfillOpacity=AValue) then exit;
  FfillOpacity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.SetstrokeColor(AIndex : Integer; AValue : String); 

begin
  If (FstrokeColor=AValue) then exit;
  FstrokeColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.SetstrokeColorStyler(AIndex : Integer; AValue : TStyleFunction); 

begin
  If (FstrokeColorStyler=AValue) then exit;
  FstrokeColorStyler:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.SetstrokeOpacity(AIndex : Integer; AValue : double); 

begin
  If (FstrokeOpacity=AValue) then exit;
  FstrokeOpacity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.SetstrokeWeight(AIndex : Integer; AValue : integer); 

begin
  If (FstrokeWeight=AValue) then exit;
  FstrokeWeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygonStyle.SetstrokeWeightStyler(AIndex : Integer; AValue : TStyleFunction); 

begin
  If (FstrokeWeightStyler=AValue) then exit;
  FstrokeWeightStyler:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSqlresponse
  --------------------------------------------------------------------}


Procedure TSqlresponse.Setcolumns(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSqlresponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSqlresponse.Setrows(AIndex : Integer; AValue : TSqlresponseTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleFunctionTypegradientTypecolorsItem
  --------------------------------------------------------------------}


Procedure TStyleFunctionTypegradientTypecolorsItem.Setcolor(AIndex : Integer; AValue : String); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunctionTypegradientTypecolorsItem.Setopacity(AIndex : Integer; AValue : double); 

begin
  If (Fopacity=AValue) then exit;
  Fopacity:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleFunctionTypegradient
  --------------------------------------------------------------------}


Procedure TStyleFunctionTypegradient.Setcolors(AIndex : Integer; AValue : TStyleFunctionTypegradientTypecolorsArray); 

begin
  If (Fcolors=AValue) then exit;
  Fcolors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunctionTypegradient.Setmax(AIndex : Integer; AValue : double); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunctionTypegradient.Setmin(AIndex : Integer; AValue : double); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleFunction
  --------------------------------------------------------------------}


Procedure TStyleFunction.Setbuckets(AIndex : Integer; AValue : TStyleFunctionTypebucketsArray); 

begin
  If (Fbuckets=AValue) then exit;
  Fbuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunction.SetcolumnName(AIndex : Integer; AValue : String); 

begin
  If (FcolumnName=AValue) then exit;
  FcolumnName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunction.Setgradient(AIndex : Integer; AValue : TStyleFunctionTypegradient); 

begin
  If (Fgradient=AValue) then exit;
  Fgradient:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunction.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleSetting
  --------------------------------------------------------------------}


Procedure TStyleSetting.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSetting.SetmarkerOptions(AIndex : Integer; AValue : TPointStyle); 

begin
  If (FmarkerOptions=AValue) then exit;
  FmarkerOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSetting.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSetting.SetpolygonOptions(AIndex : Integer; AValue : TPolygonStyle); 

begin
  If (FpolygonOptions=AValue) then exit;
  FpolygonOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSetting.SetpolylineOptions(AIndex : Integer; AValue : TLineStyle); 

begin
  If (FpolylineOptions=AValue) then exit;
  FpolylineOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSetting.SetstyleId(AIndex : Integer; AValue : integer); 

begin
  If (FstyleId=AValue) then exit;
  FstyleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSetting.SettableId(AIndex : Integer; AValue : String); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleSettingList
  --------------------------------------------------------------------}


Procedure TStyleSettingList.Setitems(AIndex : Integer; AValue : TStyleSettingListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSettingList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSettingList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSettingList.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTable
  --------------------------------------------------------------------}


Procedure TTable.Setattribution(AIndex : Integer; AValue : String); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetattributionLink(AIndex : Integer; AValue : String); 

begin
  If (FattributionLink=AValue) then exit;
  FattributionLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetbaseTableIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FbaseTableIds=AValue) then exit;
  FbaseTableIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetcolumnPropertiesJsonSchema(AIndex : Integer; AValue : String); 

begin
  If (FcolumnPropertiesJsonSchema=AValue) then exit;
  FcolumnPropertiesJsonSchema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setcolumns(AIndex : Integer; AValue : TTableTypecolumnsArray); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetisExportable(AIndex : Integer; AValue : boolean); 

begin
  If (FisExportable=AValue) then exit;
  FisExportable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setsql(AIndex : Integer; AValue : String); 

begin
  If (Fsql=AValue) then exit;
  Fsql:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SettableId(AIndex : Integer; AValue : String); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SettablePropertiesJson(AIndex : Integer; AValue : String); 

begin
  If (FtablePropertiesJson=AValue) then exit;
  FtablePropertiesJson:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SettablePropertiesJsonSchema(AIndex : Integer; AValue : String); 

begin
  If (FtablePropertiesJsonSchema=AValue) then exit;
  FtablePropertiesJsonSchema:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTableList
  --------------------------------------------------------------------}


Procedure TTableList.Setitems(AIndex : Integer; AValue : TTableListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTask
  --------------------------------------------------------------------}


Procedure TTask.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setprogress(AIndex : Integer; AValue : String); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setstarted(AIndex : Integer; AValue : boolean); 

begin
  If (Fstarted=AValue) then exit;
  Fstarted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SettaskId(AIndex : Integer; AValue : String); 

begin
  If (FtaskId=AValue) then exit;
  FtaskId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTask.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TTaskList
  --------------------------------------------------------------------}


Procedure TTaskList.Setitems(AIndex : Integer; AValue : TTaskListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTemplate
  --------------------------------------------------------------------}


Procedure TTemplate.SetautomaticColumnNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FautomaticColumnNames=AValue) then exit;
  FautomaticColumnNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setbody(AIndex : Integer; AValue : String); 

begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.SettableId(AIndex : Integer; AValue : String); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.SettemplateId(AIndex : Integer; AValue : integer); 

begin
  If (FtemplateId=AValue) then exit;
  FtemplateId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTemplateList
  --------------------------------------------------------------------}


Procedure TTemplateList.Setitems(AIndex : Integer; AValue : TTemplateListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplateList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplateList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplateList.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColumnResource
  --------------------------------------------------------------------}


Class Function TColumnResource.ResourceName : String;

begin
  Result:='column';
end;

Class Function TColumnResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfusiontablesAPI;
end;

Procedure TColumnResource.Delete(columnId: string; tableId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'tables/{tableId}/columns/{columnId}';
  _Methodid   = 'fusiontables.column.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['columnId',columnId,'tableId',tableId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TColumnResource.Get(columnId: string; tableId: string) : TColumn;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}/columns/{columnId}';
  _Methodid   = 'fusiontables.column.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['columnId',columnId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TColumn) as TColumn;
end;

Function TColumnResource.Insert(tableId: string; aColumn : TColumn) : TColumn;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/{tableId}/columns';
  _Methodid   = 'fusiontables.column.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aColumn,TColumn) as TColumn;
end;

Function TColumnResource.List(tableId: string; AQuery : string = '') : TColumnList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}/columns';
  _Methodid   = 'fusiontables.column.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TColumnList) as TColumnList;
end;


Function TColumnResource.List(tableId: string; AQuery : TColumnlistOptions) : TColumnList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(tableId,_Q);
end;

Function TColumnResource.Patch(columnId: string; tableId: string; aColumn : TColumn) : TColumn;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'tables/{tableId}/columns/{columnId}';
  _Methodid   = 'fusiontables.column.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['columnId',columnId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aColumn,TColumn) as TColumn;
end;

Function TColumnResource.Update(columnId: string; tableId: string; aColumn : TColumn) : TColumn;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'tables/{tableId}/columns/{columnId}';
  _Methodid   = 'fusiontables.column.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['columnId',columnId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aColumn,TColumn) as TColumn;
end;



{ --------------------------------------------------------------------
  TQueryResource
  --------------------------------------------------------------------}


Class Function TQueryResource.ResourceName : String;

begin
  Result:='query';
end;

Class Function TQueryResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfusiontablesAPI;
end;

Function TQueryResource.Sql(AQuery : string = '') : TSqlresponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'query';
  _Methodid   = 'fusiontables.query.sql';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSqlresponse) as TSqlresponse;
end;


Function TQueryResource.Sql(AQuery : TQuerysqlOptions) : TSqlresponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'hdrs',AQuery.hdrs);
  AddToQuery(_Q,'sql',AQuery.sql);
  AddToQuery(_Q,'typed',AQuery.typed);
  Result:=Sql(_Q);
end;

Function TQueryResource.SqlGet(AQuery : string = '') : TSqlresponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'query';
  _Methodid   = 'fusiontables.query.sqlGet';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSqlresponse) as TSqlresponse;
end;


Function TQueryResource.SqlGet(AQuery : TQuerysqlGetOptions) : TSqlresponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'hdrs',AQuery.hdrs);
  AddToQuery(_Q,'sql',AQuery.sql);
  AddToQuery(_Q,'typed',AQuery.typed);
  Result:=SqlGet(_Q);
end;



{ --------------------------------------------------------------------
  TStyleResource
  --------------------------------------------------------------------}


Class Function TStyleResource.ResourceName : String;

begin
  Result:='style';
end;

Class Function TStyleResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfusiontablesAPI;
end;

Procedure TStyleResource.Delete(styleId: integer; tableId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'tables/{tableId}/styles/{styleId}';
  _Methodid   = 'fusiontables.style.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['styleId',styleId,'tableId',tableId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TStyleResource.Get(styleId: integer; tableId: string) : TStyleSetting;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}/styles/{styleId}';
  _Methodid   = 'fusiontables.style.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['styleId',styleId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TStyleSetting) as TStyleSetting;
end;

Function TStyleResource.Insert(tableId: string; aStyleSetting : TStyleSetting) : TStyleSetting;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/{tableId}/styles';
  _Methodid   = 'fusiontables.style.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aStyleSetting,TStyleSetting) as TStyleSetting;
end;

Function TStyleResource.List(tableId: string; AQuery : string = '') : TStyleSettingList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}/styles';
  _Methodid   = 'fusiontables.style.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TStyleSettingList) as TStyleSettingList;
end;


Function TStyleResource.List(tableId: string; AQuery : TStylelistOptions) : TStyleSettingList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(tableId,_Q);
end;

Function TStyleResource.Patch(styleId: integer; tableId: string; aStyleSetting : TStyleSetting) : TStyleSetting;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'tables/{tableId}/styles/{styleId}';
  _Methodid   = 'fusiontables.style.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['styleId',styleId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aStyleSetting,TStyleSetting) as TStyleSetting;
end;

Function TStyleResource.Update(styleId: integer; tableId: string; aStyleSetting : TStyleSetting) : TStyleSetting;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'tables/{tableId}/styles/{styleId}';
  _Methodid   = 'fusiontables.style.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['styleId',styleId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aStyleSetting,TStyleSetting) as TStyleSetting;
end;



{ --------------------------------------------------------------------
  TTableResource
  --------------------------------------------------------------------}


Class Function TTableResource.ResourceName : String;

begin
  Result:='table';
end;

Class Function TTableResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfusiontablesAPI;
end;

Function TTableResource.Copy(tableId: string; AQuery : string = '') : TTable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/{tableId}/copy';
  _Methodid   = 'fusiontables.table.copy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTable) as TTable;
end;


Function TTableResource.Copy(tableId: string; AQuery : TTablecopyOptions) : TTable;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'copyPresentation',AQuery.copyPresentation);
  Result:=Copy(tableId,_Q);
end;

Procedure TTableResource.Delete(tableId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'tables/{tableId}';
  _Methodid   = 'fusiontables.table.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTableResource.Get(tableId: string) : TTable;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}';
  _Methodid   = 'fusiontables.table.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTable) as TTable;
end;

Function TTableResource.ImportRows(tableId: string; AQuery : string = '') : TImport;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/{tableId}/import';
  _Methodid   = 'fusiontables.table.importRows';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TImport) as TImport;
end;


Function TTableResource.ImportRows(tableId: string; AQuery : TTableimportRowsOptions) : TImport;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'delimiter',AQuery.delimiter);
  AddToQuery(_Q,'encoding',AQuery.encoding);
  AddToQuery(_Q,'endLine',AQuery.endLine);
  AddToQuery(_Q,'isStrict',AQuery.isStrict);
  AddToQuery(_Q,'startLine',AQuery.startLine);
  Result:=ImportRows(tableId,_Q);
end;

Function TTableResource.ImportTable(AQuery : string = '') : TTable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/import';
  _Methodid   = 'fusiontables.table.importTable';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTable) as TTable;
end;


Function TTableResource.ImportTable(AQuery : TTableimportTableOptions) : TTable;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'delimiter',AQuery.delimiter);
  AddToQuery(_Q,'encoding',AQuery.encoding);
  AddToQuery(_Q,'name',AQuery._name);
  Result:=ImportTable(_Q);
end;

Function TTableResource.Insert(aTable : TTable) : TTable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables';
  _Methodid   = 'fusiontables.table.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aTable,TTable) as TTable;
end;

Function TTableResource.List(AQuery : string = '') : TTableList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables';
  _Methodid   = 'fusiontables.table.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTableList) as TTableList;
end;


Function TTableResource.List(AQuery : TTablelistOptions) : TTableList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;

Function TTableResource.Patch(tableId: string; aTable : TTable; AQuery : string = '') : TTable;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'tables/{tableId}';
  _Methodid   = 'fusiontables.table.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTable,TTable) as TTable;
end;


Function TTableResource.Patch(tableId: string; aTable : TTable; AQuery : TTablepatchOptions) : TTable;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'replaceViewDefinition',AQuery.replaceViewDefinition);
  Result:=Patch(tableId,aTable,_Q);
end;

Function TTableResource.ReplaceRows(tableId: string; AQuery : string = '') : TTask;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/{tableId}/replace';
  _Methodid   = 'fusiontables.table.replaceRows';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTask) as TTask;
end;


Function TTableResource.ReplaceRows(tableId: string; AQuery : TTablereplaceRowsOptions) : TTask;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'delimiter',AQuery.delimiter);
  AddToQuery(_Q,'encoding',AQuery.encoding);
  AddToQuery(_Q,'endLine',AQuery.endLine);
  AddToQuery(_Q,'isStrict',AQuery.isStrict);
  AddToQuery(_Q,'startLine',AQuery.startLine);
  Result:=ReplaceRows(tableId,_Q);
end;

Function TTableResource.Update(tableId: string; aTable : TTable; AQuery : string = '') : TTable;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'tables/{tableId}';
  _Methodid   = 'fusiontables.table.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTable,TTable) as TTable;
end;


Function TTableResource.Update(tableId: string; aTable : TTable; AQuery : TTableupdateOptions) : TTable;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'replaceViewDefinition',AQuery.replaceViewDefinition);
  Result:=Update(tableId,aTable,_Q);
end;



{ --------------------------------------------------------------------
  TTaskResource
  --------------------------------------------------------------------}


Class Function TTaskResource.ResourceName : String;

begin
  Result:='task';
end;

Class Function TTaskResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfusiontablesAPI;
end;

Procedure TTaskResource.Delete(tableId: string; taskId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'tables/{tableId}/tasks/{taskId}';
  _Methodid   = 'fusiontables.task.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId,'taskId',taskId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTaskResource.Get(tableId: string; taskId: string) : TTask;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}/tasks/{taskId}';
  _Methodid   = 'fusiontables.task.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId,'taskId',taskId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTask) as TTask;
end;

Function TTaskResource.List(tableId: string; AQuery : string = '') : TTaskList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}/tasks';
  _Methodid   = 'fusiontables.task.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTaskList) as TTaskList;
end;


Function TTaskResource.List(tableId: string; AQuery : TTasklistOptions) : TTaskList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=List(tableId,_Q);
end;



{ --------------------------------------------------------------------
  TTemplateResource
  --------------------------------------------------------------------}


Class Function TTemplateResource.ResourceName : String;

begin
  Result:='template';
end;

Class Function TTemplateResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfusiontablesAPI;
end;

Procedure TTemplateResource.Delete(tableId: string; templateId: integer);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'tables/{tableId}/templates/{templateId}';
  _Methodid   = 'fusiontables.template.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId,'templateId',templateId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTemplateResource.Get(tableId: string; templateId: integer) : TTemplate;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}/templates/{templateId}';
  _Methodid   = 'fusiontables.template.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId,'templateId',templateId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTemplate) as TTemplate;
end;

Function TTemplateResource.Insert(tableId: string; aTemplate : TTemplate) : TTemplate;

Const
  _HTTPMethod = 'POST';
  _Path       = 'tables/{tableId}/templates';
  _Methodid   = 'fusiontables.template.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTemplate,TTemplate) as TTemplate;
end;

Function TTemplateResource.List(tableId: string; AQuery : string = '') : TTemplateList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'tables/{tableId}/templates';
  _Methodid   = 'fusiontables.template.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTemplateList) as TTemplateList;
end;


Function TTemplateResource.List(tableId: string; AQuery : TTemplatelistOptions) : TTemplateList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(tableId,_Q);
end;

Function TTemplateResource.Patch(tableId: string; templateId: integer; aTemplate : TTemplate) : TTemplate;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'tables/{tableId}/templates/{templateId}';
  _Methodid   = 'fusiontables.template.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId,'templateId',templateId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTemplate,TTemplate) as TTemplate;
end;

Function TTemplateResource.Update(tableId: string; templateId: integer; aTemplate : TTemplate) : TTemplate;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'tables/{tableId}/templates/{templateId}';
  _Methodid   = 'fusiontables.template.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tableId',tableId,'templateId',templateId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTemplate,TTemplate) as TTemplate;
end;



{ --------------------------------------------------------------------
  TFusiontablesAPI
  --------------------------------------------------------------------}

Class Function TFusiontablesAPI.APIName : String;

begin
  Result:='fusiontables';
end;

Class Function TFusiontablesAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TFusiontablesAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TFusiontablesAPI.APIID : String;

begin
  Result:='fusiontables:v2';
end;

Class Function TFusiontablesAPI.APITitle : String;

begin
  Result:='Fusion Tables API';
end;

Class Function TFusiontablesAPI.APIDescription : String;

begin
  Result:='API for working with Fusion Tables data.';
end;

Class Function TFusiontablesAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TFusiontablesAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TFusiontablesAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TFusiontablesAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TFusiontablesAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/fusiontables';
end;

Class Function TFusiontablesAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TFusiontablesAPI.APIbasePath : string;

begin
  Result:='/fusiontables/v2/';
end;

Class Function TFusiontablesAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/fusiontables/v2/';
end;

Class Function TFusiontablesAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TFusiontablesAPI.APIservicePath : string;

begin
  Result:='fusiontables/v2/';
end;

Class Function TFusiontablesAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TFusiontablesAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/fusiontables';
  Result[0].Description:='Manage your Fusion Tables';
  Result[1].Name:='https://www.googleapis.com/auth/fusiontables.readonly';
  Result[1].Description:='View your Fusion Tables';
  
end;

Class Function TFusiontablesAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TFusiontablesAPI.RegisterAPIResources;

begin
  TBucket.RegisterObject;
  TColumnTypebaseColumn.RegisterObject;
  TColumn.RegisterObject;
  TColumnList.RegisterObject;
  TGeometry.RegisterObject;
  TImport.RegisterObject;
  TLine.RegisterObject;
  TLineStyle.RegisterObject;
  TPoint.RegisterObject;
  TPointStyle.RegisterObject;
  TPolygon.RegisterObject;
  TPolygonStyle.RegisterObject;
  TSqlresponse.RegisterObject;
  TStyleFunctionTypegradientTypecolorsItem.RegisterObject;
  TStyleFunctionTypegradient.RegisterObject;
  TStyleFunction.RegisterObject;
  TStyleSetting.RegisterObject;
  TStyleSettingList.RegisterObject;
  TTable.RegisterObject;
  TTableList.RegisterObject;
  TTask.RegisterObject;
  TTaskList.RegisterObject;
  TTemplate.RegisterObject;
  TTemplateList.RegisterObject;
end;


Function TFusiontablesAPI.GetColumnInstance : TColumnResource;

begin
  if (FColumnInstance=Nil) then
    FColumnInstance:=CreateColumnResource;
  Result:=FColumnInstance;
end;

Function TFusiontablesAPI.CreateColumnResource : TColumnResource;

begin
  Result:=CreateColumnResource(Self);
end;


Function TFusiontablesAPI.CreateColumnResource(AOwner : TComponent) : TColumnResource;

begin
  Result:=TColumnResource.Create(AOwner);
  Result.API:=Self;
end;



Function TFusiontablesAPI.GetQueryInstance : TQueryResource;

begin
  if (FQueryInstance=Nil) then
    FQueryInstance:=CreateQueryResource;
  Result:=FQueryInstance;
end;

Function TFusiontablesAPI.CreateQueryResource : TQueryResource;

begin
  Result:=CreateQueryResource(Self);
end;


Function TFusiontablesAPI.CreateQueryResource(AOwner : TComponent) : TQueryResource;

begin
  Result:=TQueryResource.Create(AOwner);
  Result.API:=Self;
end;



Function TFusiontablesAPI.GetStyleInstance : TStyleResource;

begin
  if (FStyleInstance=Nil) then
    FStyleInstance:=CreateStyleResource;
  Result:=FStyleInstance;
end;

Function TFusiontablesAPI.CreateStyleResource : TStyleResource;

begin
  Result:=CreateStyleResource(Self);
end;


Function TFusiontablesAPI.CreateStyleResource(AOwner : TComponent) : TStyleResource;

begin
  Result:=TStyleResource.Create(AOwner);
  Result.API:=Self;
end;



Function TFusiontablesAPI.GetTableInstance : TTableResource;

begin
  if (FTableInstance=Nil) then
    FTableInstance:=CreateTableResource;
  Result:=FTableInstance;
end;

Function TFusiontablesAPI.CreateTableResource : TTableResource;

begin
  Result:=CreateTableResource(Self);
end;


Function TFusiontablesAPI.CreateTableResource(AOwner : TComponent) : TTableResource;

begin
  Result:=TTableResource.Create(AOwner);
  Result.API:=Self;
end;



Function TFusiontablesAPI.GetTaskInstance : TTaskResource;

begin
  if (FTaskInstance=Nil) then
    FTaskInstance:=CreateTaskResource;
  Result:=FTaskInstance;
end;

Function TFusiontablesAPI.CreateTaskResource : TTaskResource;

begin
  Result:=CreateTaskResource(Self);
end;


Function TFusiontablesAPI.CreateTaskResource(AOwner : TComponent) : TTaskResource;

begin
  Result:=TTaskResource.Create(AOwner);
  Result.API:=Self;
end;



Function TFusiontablesAPI.GetTemplateInstance : TTemplateResource;

begin
  if (FTemplateInstance=Nil) then
    FTemplateInstance:=CreateTemplateResource;
  Result:=FTemplateInstance;
end;

Function TFusiontablesAPI.CreateTemplateResource : TTemplateResource;

begin
  Result:=CreateTemplateResource(Self);
end;


Function TFusiontablesAPI.CreateTemplateResource(AOwner : TComponent) : TTemplateResource;

begin
  Result:=TTemplateResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TFusiontablesAPI.RegisterAPI;
end.
