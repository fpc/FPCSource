unit googlefusiontables;
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
  TBucket = class;
  TBucketArray = Array of TBucket;
  TColumn = class;
  TColumnArray = Array of TColumn;
  TColumnbaseColumn = class;
  TColumnbaseColumnArray = Array of TColumnbaseColumn;
  TColumnvalidValues = class;
  TColumnvalidValuesArray = Array of TColumnvalidValues;
  TColumnList = class;
  TColumnListArray = Array of TColumnList;
  TColumnListitems = class;
  TColumnListitemsArray = Array of TColumnListitems;
  TGeometry = class;
  TGeometryArray = Array of TGeometry;
  TGeometrygeometries = class;
  TGeometrygeometriesArray = Array of TGeometrygeometries;
  TImport = class;
  TImportArray = Array of TImport;
  TLine = class;
  TLineArray = Array of TLine;
  TLinecoordinates = class;
  TLinecoordinatesArray = Array of TLinecoordinates;
  TLineStyle = class;
  TLineStyleArray = Array of TLineStyle;
  TPoint = class;
  TPointArray = Array of TPoint;
  TPointcoordinates = class;
  TPointcoordinatesArray = Array of TPointcoordinates;
  TPointStyle = class;
  TPointStyleArray = Array of TPointStyle;
  TPolygon = class;
  TPolygonArray = Array of TPolygon;
  TPolygoncoordinates = class;
  TPolygoncoordinatesArray = Array of TPolygoncoordinates;
  TPolygonStyle = class;
  TPolygonStyleArray = Array of TPolygonStyle;
  TSqlresponse = class;
  TSqlresponseArray = Array of TSqlresponse;
  TSqlresponsecolumns = class;
  TSqlresponsecolumnsArray = Array of TSqlresponsecolumns;
  TSqlresponserows = class;
  TSqlresponserowsArray = Array of TSqlresponserows;
  TStyleFunction = class;
  TStyleFunctionArray = Array of TStyleFunction;
  TStyleFunctionbuckets = class;
  TStyleFunctionbucketsArray = Array of TStyleFunctionbuckets;
  TStyleFunctiongradient = class;
  TStyleFunctiongradientArray = Array of TStyleFunctiongradient;
  TStyleFunctiongradientcolors = class;
  TStyleFunctiongradientcolorsArray = Array of TStyleFunctiongradientcolors;
  TStyleSetting = class;
  TStyleSettingArray = Array of TStyleSetting;
  TStyleSettingList = class;
  TStyleSettingListArray = Array of TStyleSettingList;
  TStyleSettingListitems = class;
  TStyleSettingListitemsArray = Array of TStyleSettingListitems;
  TTable = class;
  TTableArray = Array of TTable;
  TTablebaseTableIds = class;
  TTablebaseTableIdsArray = Array of TTablebaseTableIds;
  TTablecolumns = class;
  TTablecolumnsArray = Array of TTablecolumns;
  TTableList = class;
  TTableListArray = Array of TTableList;
  TTableListitems = class;
  TTableListitemsArray = Array of TTableListitems;
  TTask = class;
  TTaskArray = Array of TTask;
  TTaskList = class;
  TTaskListArray = Array of TTaskList;
  TTaskListitems = class;
  TTaskListitemsArray = Array of TTaskListitems;
  TTemplate = class;
  TTemplateArray = Array of TTemplate;
  TTemplateautomaticColumnNames = class;
  TTemplateautomaticColumnNamesArray = Array of TTemplateautomaticColumnNames;
  TTemplateList = class;
  TTemplateListArray = Array of TTemplateList;
  TTemplateListitems = class;
  TTemplateListitemsArray = Array of TTemplateListitems;
  
  { --------------------------------------------------------------------
    TBucket
    --------------------------------------------------------------------}
  
  TBucket = Class(TGoogleBaseObject)
  Private
    Fcolor : string;
    Ficon : string;
    Fmax : double;
    Fmin : double;
    Fopacity : double;
    Fweight : integer;
  Protected
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : string); virtual;
    Procedure Seticon(AIndex : Integer; AValue : string); virtual;
    Procedure Setmax(AIndex : Integer; AValue : double); virtual;
    Procedure Setmin(AIndex : Integer; AValue : double); virtual;
    Procedure Setopacity(AIndex : Integer; AValue : double); virtual;
    Procedure Setweight(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property color : string Index 0 Read Fcolor Write Setcolor;
    Property icon : string Index 8 Read Ficon Write Seticon;
    Property max : double Index 16 Read Fmax Write Setmax;
    Property min : double Index 24 Read Fmin Write Setmin;
    Property opacity : double Index 32 Read Fopacity Write Setopacity;
    Property weight : integer Index 40 Read Fweight Write Setweight;
  end;
  TBucketClass = Class of TBucket;
  
  { --------------------------------------------------------------------
    TColumn
    --------------------------------------------------------------------}
  
  TColumn = Class(TGoogleBaseObject)
  Private
    FbaseColumn : TColumnbaseColumn;
    FcolumnId : integer;
    FcolumnJsonSchema : string;
    FcolumnPropertiesJson : string;
    Fdescription : string;
    FformatPattern : string;
    FgraphPredicate : string;
    Fkind : string;
    Fname : string;
    F_type : string;
    FvalidValues : TColumnvalidValues;
    FvalidateData : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetbaseColumn(AIndex : Integer; AValue : TColumnbaseColumn); virtual;
    Procedure SetcolumnId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcolumnJsonSchema(AIndex : Integer; AValue : string); virtual;
    Procedure SetcolumnPropertiesJson(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetformatPattern(AIndex : Integer; AValue : string); virtual;
    Procedure SetgraphPredicate(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure SetvalidValues(AIndex : Integer; AValue : TColumnvalidValues); virtual;
    Procedure SetvalidateData(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property baseColumn : TColumnbaseColumn Index 0 Read FbaseColumn Write SetbaseColumn;
    Property columnId : integer Index 8 Read FcolumnId Write SetcolumnId;
    Property columnJsonSchema : string Index 16 Read FcolumnJsonSchema Write SetcolumnJsonSchema;
    Property columnPropertiesJson : string Index 24 Read FcolumnPropertiesJson Write SetcolumnPropertiesJson;
    Property description : string Index 32 Read Fdescription Write Setdescription;
    Property formatPattern : string Index 40 Read FformatPattern Write SetformatPattern;
    Property graphPredicate : string Index 48 Read FgraphPredicate Write SetgraphPredicate;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property name : string Index 64 Read Fname Write Setname;
    Property _type : string Index 72 Read F_type Write Set_type;
    Property validValues : TColumnvalidValues Index 80 Read FvalidValues Write SetvalidValues;
    Property validateData : boolean Index 88 Read FvalidateData Write SetvalidateData;
  end;
  TColumnClass = Class of TColumn;
  
  { --------------------------------------------------------------------
    TColumnbaseColumn
    --------------------------------------------------------------------}
  
  TColumnbaseColumn = Class(TGoogleBaseObject)
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
  TColumnbaseColumnClass = Class of TColumnbaseColumn;
  
  { --------------------------------------------------------------------
    TColumnvalidValues
    --------------------------------------------------------------------}
  
  TColumnvalidValues = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TColumnvalidValuesClass = Class of TColumnvalidValues;
  
  { --------------------------------------------------------------------
    TColumnList
    --------------------------------------------------------------------}
  
  TColumnList = Class(TGoogleBaseObject)
  Private
    Fitems : TColumnListitems;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TColumnListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TColumnListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TColumnListClass = Class of TColumnList;
  
  { --------------------------------------------------------------------
    TColumnListitems
    --------------------------------------------------------------------}
  
  TColumnListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TColumnListitemsClass = Class of TColumnListitems;
  
  { --------------------------------------------------------------------
    TGeometry
    --------------------------------------------------------------------}
  
  TGeometry = Class(TGoogleBaseObject)
  Private
    Fgeometries : TGeometrygeometries;
    Fgeometry : TJSONSchema;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setgeometries(AIndex : Integer; AValue : TGeometrygeometries); virtual;
    Procedure Setgeometry(AIndex : Integer; AValue : TJSONSchema); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property geometries : TGeometrygeometries Index 0 Read Fgeometries Write Setgeometries;
    Property geometry : TJSONSchema Index 8 Read Fgeometry Write Setgeometry;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TGeometryClass = Class of TGeometry;
  
  { --------------------------------------------------------------------
    TGeometrygeometries
    --------------------------------------------------------------------}
  
  TGeometrygeometries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeometrygeometriesClass = Class of TGeometrygeometries;
  
  { --------------------------------------------------------------------
    TImport
    --------------------------------------------------------------------}
  
  TImport = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnumRowsReceived : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumRowsReceived(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property numRowsReceived : string Index 8 Read FnumRowsReceived Write SetnumRowsReceived;
  end;
  TImportClass = Class of TImport;
  
  { --------------------------------------------------------------------
    TLine
    --------------------------------------------------------------------}
  
  TLine = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TLinecoordinates;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TLinecoordinates); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TLinecoordinates Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TLineClass = Class of TLine;
  
  { --------------------------------------------------------------------
    TLinecoordinates
    --------------------------------------------------------------------}
  
  TLinecoordinates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLinecoordinatesClass = Class of TLinecoordinates;
  
  { --------------------------------------------------------------------
    TLineStyle
    --------------------------------------------------------------------}
  
  TLineStyle = Class(TGoogleBaseObject)
  Private
    FstrokeColor : string;
    FstrokeColorStyler : TStyleFunction;
    FstrokeOpacity : double;
    FstrokeWeight : integer;
    FstrokeWeightStyler : TStyleFunction;
  Protected
    //Property setters
    Procedure SetstrokeColor(AIndex : Integer; AValue : string); virtual;
    Procedure SetstrokeColorStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
    Procedure SetstrokeOpacity(AIndex : Integer; AValue : double); virtual;
    Procedure SetstrokeWeight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstrokeWeightStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
  Public
  Published
    Property strokeColor : string Index 0 Read FstrokeColor Write SetstrokeColor;
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
    Fcoordinates : TPointcoordinates;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TPointcoordinates); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TPointcoordinates Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TPointClass = Class of TPoint;
  
  { --------------------------------------------------------------------
    TPointcoordinates
    --------------------------------------------------------------------}
  
  TPointcoordinates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPointcoordinatesClass = Class of TPointcoordinates;
  
  { --------------------------------------------------------------------
    TPointStyle
    --------------------------------------------------------------------}
  
  TPointStyle = Class(TGoogleBaseObject)
  Private
    FiconName : string;
    FiconStyler : TStyleFunction;
  Protected
    //Property setters
    Procedure SeticonName(AIndex : Integer; AValue : string); virtual;
    Procedure SeticonStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
  Public
  Published
    Property iconName : string Index 0 Read FiconName Write SeticonName;
    Property iconStyler : TStyleFunction Index 8 Read FiconStyler Write SeticonStyler;
  end;
  TPointStyleClass = Class of TPointStyle;
  
  { --------------------------------------------------------------------
    TPolygon
    --------------------------------------------------------------------}
  
  TPolygon = Class(TGoogleBaseObject)
  Private
    Fcoordinates : TPolygoncoordinates;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcoordinates(AIndex : Integer; AValue : TPolygoncoordinates); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coordinates : TPolygoncoordinates Index 0 Read Fcoordinates Write Setcoordinates;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TPolygonClass = Class of TPolygon;
  
  { --------------------------------------------------------------------
    TPolygoncoordinates
    --------------------------------------------------------------------}
  
  TPolygoncoordinates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPolygoncoordinatesClass = Class of TPolygoncoordinates;
  
  { --------------------------------------------------------------------
    TPolygonStyle
    --------------------------------------------------------------------}
  
  TPolygonStyle = Class(TGoogleBaseObject)
  Private
    FfillColor : string;
    FfillColorStyler : TStyleFunction;
    FfillOpacity : double;
    FstrokeColor : string;
    FstrokeColorStyler : TStyleFunction;
    FstrokeOpacity : double;
    FstrokeWeight : integer;
    FstrokeWeightStyler : TStyleFunction;
  Protected
    //Property setters
    Procedure SetfillColor(AIndex : Integer; AValue : string); virtual;
    Procedure SetfillColorStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
    Procedure SetfillOpacity(AIndex : Integer; AValue : double); virtual;
    Procedure SetstrokeColor(AIndex : Integer; AValue : string); virtual;
    Procedure SetstrokeColorStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
    Procedure SetstrokeOpacity(AIndex : Integer; AValue : double); virtual;
    Procedure SetstrokeWeight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstrokeWeightStyler(AIndex : Integer; AValue : TStyleFunction); virtual;
  Public
  Published
    Property fillColor : string Index 0 Read FfillColor Write SetfillColor;
    Property fillColorStyler : TStyleFunction Index 8 Read FfillColorStyler Write SetfillColorStyler;
    Property fillOpacity : double Index 16 Read FfillOpacity Write SetfillOpacity;
    Property strokeColor : string Index 24 Read FstrokeColor Write SetstrokeColor;
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
    Fcolumns : TSqlresponsecolumns;
    Fkind : string;
    Frows : TSqlresponserows;
  Protected
    //Property setters
    Procedure Setcolumns(AIndex : Integer; AValue : TSqlresponsecolumns); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TSqlresponserows); virtual;
  Public
  Published
    Property columns : TSqlresponsecolumns Index 0 Read Fcolumns Write Setcolumns;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property rows : TSqlresponserows Index 16 Read Frows Write Setrows;
  end;
  TSqlresponseClass = Class of TSqlresponse;
  
  { --------------------------------------------------------------------
    TSqlresponsecolumns
    --------------------------------------------------------------------}
  
  TSqlresponsecolumns = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSqlresponsecolumnsClass = Class of TSqlresponsecolumns;
  
  { --------------------------------------------------------------------
    TSqlresponserows
    --------------------------------------------------------------------}
  
  TSqlresponserows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSqlresponserowsClass = Class of TSqlresponserows;
  
  { --------------------------------------------------------------------
    TStyleFunction
    --------------------------------------------------------------------}
  
  TStyleFunction = Class(TGoogleBaseObject)
  Private
    Fbuckets : TStyleFunctionbuckets;
    FcolumnName : string;
    Fgradient : TStyleFunctiongradient;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setbuckets(AIndex : Integer; AValue : TStyleFunctionbuckets); virtual;
    Procedure SetcolumnName(AIndex : Integer; AValue : string); virtual;
    Procedure Setgradient(AIndex : Integer; AValue : TStyleFunctiongradient); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property buckets : TStyleFunctionbuckets Index 0 Read Fbuckets Write Setbuckets;
    Property columnName : string Index 8 Read FcolumnName Write SetcolumnName;
    Property gradient : TStyleFunctiongradient Index 16 Read Fgradient Write Setgradient;
    Property kind : string Index 24 Read Fkind Write Setkind;
  end;
  TStyleFunctionClass = Class of TStyleFunction;
  
  { --------------------------------------------------------------------
    TStyleFunctionbuckets
    --------------------------------------------------------------------}
  
  TStyleFunctionbuckets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStyleFunctionbucketsClass = Class of TStyleFunctionbuckets;
  
  { --------------------------------------------------------------------
    TStyleFunctiongradient
    --------------------------------------------------------------------}
  
  TStyleFunctiongradient = Class(TGoogleBaseObject)
  Private
    Fcolors : TStyleFunctiongradientcolors;
    Fmax : double;
    Fmin : double;
  Protected
    //Property setters
    Procedure Setcolors(AIndex : Integer; AValue : TStyleFunctiongradientcolors); virtual;
    Procedure Setmax(AIndex : Integer; AValue : double); virtual;
    Procedure Setmin(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property colors : TStyleFunctiongradientcolors Index 0 Read Fcolors Write Setcolors;
    Property max : double Index 8 Read Fmax Write Setmax;
    Property min : double Index 16 Read Fmin Write Setmin;
  end;
  TStyleFunctiongradientClass = Class of TStyleFunctiongradient;
  
  { --------------------------------------------------------------------
    TStyleFunctiongradientcolors
    --------------------------------------------------------------------}
  
  TStyleFunctiongradientcolors = Class(TGoogleBaseObject)
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
  TStyleFunctiongradientcolorsClass = Class of TStyleFunctiongradientcolors;
  
  { --------------------------------------------------------------------
    TStyleSetting
    --------------------------------------------------------------------}
  
  TStyleSetting = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FmarkerOptions : TPointStyle;
    Fname : string;
    FpolygonOptions : TPolygonStyle;
    FpolylineOptions : TLineStyle;
    FstyleId : integer;
    FtableId : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmarkerOptions(AIndex : Integer; AValue : TPointStyle); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpolygonOptions(AIndex : Integer; AValue : TPolygonStyle); virtual;
    Procedure SetpolylineOptions(AIndex : Integer; AValue : TLineStyle); virtual;
    Procedure SetstyleId(AIndex : Integer; AValue : integer); virtual;
    Procedure SettableId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property markerOptions : TPointStyle Index 8 Read FmarkerOptions Write SetmarkerOptions;
    Property name : string Index 16 Read Fname Write Setname;
    Property polygonOptions : TPolygonStyle Index 24 Read FpolygonOptions Write SetpolygonOptions;
    Property polylineOptions : TLineStyle Index 32 Read FpolylineOptions Write SetpolylineOptions;
    Property styleId : integer Index 40 Read FstyleId Write SetstyleId;
    Property tableId : string Index 48 Read FtableId Write SettableId;
  end;
  TStyleSettingClass = Class of TStyleSetting;
  
  { --------------------------------------------------------------------
    TStyleSettingList
    --------------------------------------------------------------------}
  
  TStyleSettingList = Class(TGoogleBaseObject)
  Private
    Fitems : TStyleSettingListitems;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TStyleSettingListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TStyleSettingListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TStyleSettingListClass = Class of TStyleSettingList;
  
  { --------------------------------------------------------------------
    TStyleSettingListitems
    --------------------------------------------------------------------}
  
  TStyleSettingListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStyleSettingListitemsClass = Class of TStyleSettingListitems;
  
  { --------------------------------------------------------------------
    TTable
    --------------------------------------------------------------------}
  
  TTable = Class(TGoogleBaseObject)
  Private
    Fattribution : string;
    FattributionLink : string;
    FbaseTableIds : TTablebaseTableIds;
    FcolumnPropertiesJsonSchema : string;
    Fcolumns : TTablecolumns;
    Fdescription : string;
    FisExportable : boolean;
    Fkind : string;
    Fname : string;
    Fsql : string;
    FtableId : string;
    FtablePropertiesJson : string;
    FtablePropertiesJsonSchema : string;
  Protected
    //Property setters
    Procedure Setattribution(AIndex : Integer; AValue : string); virtual;
    Procedure SetattributionLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetbaseTableIds(AIndex : Integer; AValue : TTablebaseTableIds); virtual;
    Procedure SetcolumnPropertiesJsonSchema(AIndex : Integer; AValue : string); virtual;
    Procedure Setcolumns(AIndex : Integer; AValue : TTablecolumns); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetisExportable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setsql(AIndex : Integer; AValue : string); virtual;
    Procedure SettableId(AIndex : Integer; AValue : string); virtual;
    Procedure SettablePropertiesJson(AIndex : Integer; AValue : string); virtual;
    Procedure SettablePropertiesJsonSchema(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attribution : string Index 0 Read Fattribution Write Setattribution;
    Property attributionLink : string Index 8 Read FattributionLink Write SetattributionLink;
    Property baseTableIds : TTablebaseTableIds Index 16 Read FbaseTableIds Write SetbaseTableIds;
    Property columnPropertiesJsonSchema : string Index 24 Read FcolumnPropertiesJsonSchema Write SetcolumnPropertiesJsonSchema;
    Property columns : TTablecolumns Index 32 Read Fcolumns Write Setcolumns;
    Property description : string Index 40 Read Fdescription Write Setdescription;
    Property isExportable : boolean Index 48 Read FisExportable Write SetisExportable;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property name : string Index 64 Read Fname Write Setname;
    Property sql : string Index 72 Read Fsql Write Setsql;
    Property tableId : string Index 80 Read FtableId Write SettableId;
    Property tablePropertiesJson : string Index 88 Read FtablePropertiesJson Write SettablePropertiesJson;
    Property tablePropertiesJsonSchema : string Index 96 Read FtablePropertiesJsonSchema Write SettablePropertiesJsonSchema;
  end;
  TTableClass = Class of TTable;
  
  { --------------------------------------------------------------------
    TTablebaseTableIds
    --------------------------------------------------------------------}
  
  TTablebaseTableIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTablebaseTableIdsClass = Class of TTablebaseTableIds;
  
  { --------------------------------------------------------------------
    TTablecolumns
    --------------------------------------------------------------------}
  
  TTablecolumns = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTablecolumnsClass = Class of TTablecolumns;
  
  { --------------------------------------------------------------------
    TTableList
    --------------------------------------------------------------------}
  
  TTableList = Class(TGoogleBaseObject)
  Private
    Fitems : TTableListitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTableListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TTableListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TTableListClass = Class of TTableList;
  
  { --------------------------------------------------------------------
    TTableListitems
    --------------------------------------------------------------------}
  
  TTableListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTableListitemsClass = Class of TTableListitems;
  
  { --------------------------------------------------------------------
    TTask
    --------------------------------------------------------------------}
  
  TTask = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fprogress : string;
    Fstarted : boolean;
    FtaskId : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : string); virtual;
    Procedure Setstarted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SettaskId(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property progress : string Index 8 Read Fprogress Write Setprogress;
    Property started : boolean Index 16 Read Fstarted Write Setstarted;
    Property taskId : string Index 24 Read FtaskId Write SettaskId;
    Property _type : string Index 32 Read F_type Write Set_type;
  end;
  TTaskClass = Class of TTask;
  
  { --------------------------------------------------------------------
    TTaskList
    --------------------------------------------------------------------}
  
  TTaskList = Class(TGoogleBaseObject)
  Private
    Fitems : TTaskListitems;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTaskListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TTaskListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TTaskListClass = Class of TTaskList;
  
  { --------------------------------------------------------------------
    TTaskListitems
    --------------------------------------------------------------------}
  
  TTaskListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTaskListitemsClass = Class of TTaskListitems;
  
  { --------------------------------------------------------------------
    TTemplate
    --------------------------------------------------------------------}
  
  TTemplate = Class(TGoogleBaseObject)
  Private
    FautomaticColumnNames : TTemplateautomaticColumnNames;
    Fbody : string;
    Fkind : string;
    Fname : string;
    FtableId : string;
    FtemplateId : integer;
  Protected
    //Property setters
    Procedure SetautomaticColumnNames(AIndex : Integer; AValue : TTemplateautomaticColumnNames); virtual;
    Procedure Setbody(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SettableId(AIndex : Integer; AValue : string); virtual;
    Procedure SettemplateId(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property automaticColumnNames : TTemplateautomaticColumnNames Index 0 Read FautomaticColumnNames Write SetautomaticColumnNames;
    Property body : string Index 8 Read Fbody Write Setbody;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property name : string Index 24 Read Fname Write Setname;
    Property tableId : string Index 32 Read FtableId Write SettableId;
    Property templateId : integer Index 40 Read FtemplateId Write SettemplateId;
  end;
  TTemplateClass = Class of TTemplate;
  
  { --------------------------------------------------------------------
    TTemplateautomaticColumnNames
    --------------------------------------------------------------------}
  
  TTemplateautomaticColumnNames = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTemplateautomaticColumnNamesClass = Class of TTemplateautomaticColumnNames;
  
  { --------------------------------------------------------------------
    TTemplateList
    --------------------------------------------------------------------}
  
  TTemplateList = Class(TGoogleBaseObject)
  Private
    Fitems : TTemplateListitems;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTemplateListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TTemplateListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 24 Read FtotalItems Write SettotalItems;
  end;
  TTemplateListClass = Class of TTemplateList;
  
  { --------------------------------------------------------------------
    TTemplateListitems
    --------------------------------------------------------------------}
  
  TTemplateListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTemplateListitemsClass = Class of TTemplateListitems;
  
  { --------------------------------------------------------------------
    TColumnResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TColumnResource, method List
  
  TColumnListOptions = Record
    maxResults : integer;
    pageToken : string;
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
    sql : string;
    typed : boolean;
  end;
  
  
  //Optional query Options for TQueryResource, method SqlGet
  
  TQuerySqlGetOptions = Record
    hdrs : boolean;
    sql : string;
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
    pageToken : string;
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
    delimiter : string;
    encoding : string;
    endLine : integer;
    isStrict : boolean;
    startLine : integer;
  end;
  
  
  //Optional query Options for TTableResource, method ImportTable
  
  TTableImportTableOptions = Record
    delimiter : string;
    encoding : string;
    _name : string;
  end;
  
  
  //Optional query Options for TTableResource, method List
  
  TTableListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TTableResource, method Patch
  
  TTablePatchOptions = Record
    replaceViewDefinition : boolean;
  end;
  
  
  //Optional query Options for TTableResource, method ReplaceRows
  
  TTableReplaceRowsOptions = Record
    delimiter : string;
    encoding : string;
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
    pageToken : string;
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
    pageToken : string;
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


Procedure TBucket.Setcolor(AIndex : Integer; AValue : string); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Seticon(AIndex : Integer; AValue : string); 

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
  TColumn
  --------------------------------------------------------------------}


Procedure TColumn.SetbaseColumn(AIndex : Integer; AValue : TColumnbaseColumn); 

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



Procedure TColumn.SetcolumnJsonSchema(AIndex : Integer; AValue : string); 

begin
  If (FcolumnJsonSchema=AValue) then exit;
  FcolumnJsonSchema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetcolumnPropertiesJson(AIndex : Integer; AValue : string); 

begin
  If (FcolumnPropertiesJson=AValue) then exit;
  FcolumnPropertiesJson:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetformatPattern(AIndex : Integer; AValue : string); 

begin
  If (FformatPattern=AValue) then exit;
  FformatPattern:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetgraphPredicate(AIndex : Integer; AValue : string); 

begin
  If (FgraphPredicate=AValue) then exit;
  FgraphPredicate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.SetvalidValues(AIndex : Integer; AValue : TColumnvalidValues); 

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
  TColumnbaseColumn
  --------------------------------------------------------------------}


Procedure TColumnbaseColumn.SetcolumnId(AIndex : Integer; AValue : integer); 

begin
  If (FcolumnId=AValue) then exit;
  FcolumnId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumnbaseColumn.SettableIndex(AIndex : Integer; AValue : integer); 

begin
  If (FtableIndex=AValue) then exit;
  FtableIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColumnvalidValues
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TColumnList
  --------------------------------------------------------------------}


Procedure TColumnList.Setitems(AIndex : Integer; AValue : TColumnListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumnList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumnList.SetnextPageToken(AIndex : Integer; AValue : string); 

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
  TColumnListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeometry
  --------------------------------------------------------------------}


Procedure TGeometry.Setgeometries(AIndex : Integer; AValue : TGeometrygeometries); 

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



Procedure TGeometry.Set_type(AIndex : Integer; AValue : string); 

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
  TGeometrygeometries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImport
  --------------------------------------------------------------------}


Procedure TImport.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImport.SetnumRowsReceived(AIndex : Integer; AValue : string); 

begin
  If (FnumRowsReceived=AValue) then exit;
  FnumRowsReceived:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLine
  --------------------------------------------------------------------}


Procedure TLine.Setcoordinates(AIndex : Integer; AValue : TLinecoordinates); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLine.Set_type(AIndex : Integer; AValue : string); 

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
  TLinecoordinates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLineStyle
  --------------------------------------------------------------------}


Procedure TLineStyle.SetstrokeColor(AIndex : Integer; AValue : string); 

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


Procedure TPoint.Setcoordinates(AIndex : Integer; AValue : TPointcoordinates); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.Set_type(AIndex : Integer; AValue : string); 

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
  TPointcoordinates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPointStyle
  --------------------------------------------------------------------}


Procedure TPointStyle.SeticonName(AIndex : Integer; AValue : string); 

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


Procedure TPolygon.Setcoordinates(AIndex : Integer; AValue : TPolygoncoordinates); 

begin
  If (Fcoordinates=AValue) then exit;
  Fcoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolygon.Set_type(AIndex : Integer; AValue : string); 

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
  TPolygoncoordinates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPolygonStyle
  --------------------------------------------------------------------}


Procedure TPolygonStyle.SetfillColor(AIndex : Integer; AValue : string); 

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



Procedure TPolygonStyle.SetstrokeColor(AIndex : Integer; AValue : string); 

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


Procedure TSqlresponse.Setcolumns(AIndex : Integer; AValue : TSqlresponsecolumns); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSqlresponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSqlresponse.Setrows(AIndex : Integer; AValue : TSqlresponserows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSqlresponsecolumns
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSqlresponserows
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TStyleFunction
  --------------------------------------------------------------------}


Procedure TStyleFunction.Setbuckets(AIndex : Integer; AValue : TStyleFunctionbuckets); 

begin
  If (Fbuckets=AValue) then exit;
  Fbuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunction.SetcolumnName(AIndex : Integer; AValue : string); 

begin
  If (FcolumnName=AValue) then exit;
  FcolumnName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunction.Setgradient(AIndex : Integer; AValue : TStyleFunctiongradient); 

begin
  If (Fgradient=AValue) then exit;
  Fgradient:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunction.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleFunctionbuckets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TStyleFunctiongradient
  --------------------------------------------------------------------}


Procedure TStyleFunctiongradient.Setcolors(AIndex : Integer; AValue : TStyleFunctiongradientcolors); 

begin
  If (Fcolors=AValue) then exit;
  Fcolors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunctiongradient.Setmax(AIndex : Integer; AValue : double); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunctiongradient.Setmin(AIndex : Integer; AValue : double); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleFunctiongradientcolors
  --------------------------------------------------------------------}


Procedure TStyleFunctiongradientcolors.Setcolor(AIndex : Integer; AValue : string); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleFunctiongradientcolors.Setopacity(AIndex : Integer; AValue : double); 

begin
  If (Fopacity=AValue) then exit;
  Fopacity:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleSetting
  --------------------------------------------------------------------}


Procedure TStyleSetting.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TStyleSetting.Setname(AIndex : Integer; AValue : string); 

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



Procedure TStyleSetting.SettableId(AIndex : Integer; AValue : string); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStyleSettingList
  --------------------------------------------------------------------}


Procedure TStyleSettingList.Setitems(AIndex : Integer; AValue : TStyleSettingListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSettingList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStyleSettingList.SetnextPageToken(AIndex : Integer; AValue : string); 

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
  TStyleSettingListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTable
  --------------------------------------------------------------------}


Procedure TTable.Setattribution(AIndex : Integer; AValue : string); 

begin
  If (Fattribution=AValue) then exit;
  Fattribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetattributionLink(AIndex : Integer; AValue : string); 

begin
  If (FattributionLink=AValue) then exit;
  FattributionLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetbaseTableIds(AIndex : Integer; AValue : TTablebaseTableIds); 

begin
  If (FbaseTableIds=AValue) then exit;
  FbaseTableIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetcolumnPropertiesJsonSchema(AIndex : Integer; AValue : string); 

begin
  If (FcolumnPropertiesJsonSchema=AValue) then exit;
  FcolumnPropertiesJsonSchema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setcolumns(AIndex : Integer; AValue : TTablecolumns); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setdescription(AIndex : Integer; AValue : string); 

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



Procedure TTable.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setsql(AIndex : Integer; AValue : string); 

begin
  If (Fsql=AValue) then exit;
  Fsql:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SettableId(AIndex : Integer; AValue : string); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SettablePropertiesJson(AIndex : Integer; AValue : string); 

begin
  If (FtablePropertiesJson=AValue) then exit;
  FtablePropertiesJson:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SettablePropertiesJsonSchema(AIndex : Integer; AValue : string); 

begin
  If (FtablePropertiesJsonSchema=AValue) then exit;
  FtablePropertiesJsonSchema:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTablebaseTableIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTablecolumns
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTableList
  --------------------------------------------------------------------}


Procedure TTableList.Setitems(AIndex : Integer; AValue : TTableListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTableListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTask
  --------------------------------------------------------------------}


Procedure TTask.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setprogress(AIndex : Integer; AValue : string); 

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



Procedure TTask.SettaskId(AIndex : Integer; AValue : string); 

begin
  If (FtaskId=AValue) then exit;
  FtaskId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TTaskList.Setitems(AIndex : Integer; AValue : TTaskListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.SetnextPageToken(AIndex : Integer; AValue : string); 

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
  TTaskListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTemplate
  --------------------------------------------------------------------}


Procedure TTemplate.SetautomaticColumnNames(AIndex : Integer; AValue : TTemplateautomaticColumnNames); 

begin
  If (FautomaticColumnNames=AValue) then exit;
  FautomaticColumnNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setbody(AIndex : Integer; AValue : string); 

begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.SettableId(AIndex : Integer; AValue : string); 

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
  TTemplateautomaticColumnNames
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTemplateList
  --------------------------------------------------------------------}


Procedure TTemplateList.Setitems(AIndex : Integer; AValue : TTemplateListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplateList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplateList.SetnextPageToken(AIndex : Integer; AValue : string); 

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
  TTemplateListitems
  --------------------------------------------------------------------}




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
  TColumn.RegisterObject;
  TColumnbaseColumn.RegisterObject;
  TColumnvalidValues.RegisterObject;
  TColumnList.RegisterObject;
  TColumnListitems.RegisterObject;
  TGeometry.RegisterObject;
  TGeometrygeometries.RegisterObject;
  TImport.RegisterObject;
  TLine.RegisterObject;
  TLinecoordinates.RegisterObject;
  TLineStyle.RegisterObject;
  TPoint.RegisterObject;
  TPointcoordinates.RegisterObject;
  TPointStyle.RegisterObject;
  TPolygon.RegisterObject;
  TPolygoncoordinates.RegisterObject;
  TPolygonStyle.RegisterObject;
  TSqlresponse.RegisterObject;
  TSqlresponsecolumns.RegisterObject;
  TSqlresponserows.RegisterObject;
  TStyleFunction.RegisterObject;
  TStyleFunctionbuckets.RegisterObject;
  TStyleFunctiongradient.RegisterObject;
  TStyleFunctiongradientcolors.RegisterObject;
  TStyleSetting.RegisterObject;
  TStyleSettingList.RegisterObject;
  TStyleSettingListitems.RegisterObject;
  TTable.RegisterObject;
  TTablebaseTableIds.RegisterObject;
  TTablecolumns.RegisterObject;
  TTableList.RegisterObject;
  TTableListitems.RegisterObject;
  TTask.RegisterObject;
  TTaskList.RegisterObject;
  TTaskListitems.RegisterObject;
  TTemplate.RegisterObject;
  TTemplateautomaticColumnNames.RegisterObject;
  TTemplateList.RegisterObject;
  TTemplateListitems.RegisterObject;
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
