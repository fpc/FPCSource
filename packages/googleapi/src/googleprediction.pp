unit googleprediction;
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
//Generated on: 16-5-15 08:53:06
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAnalyze = Class;
  TInput = Class;
  TInsert = Class;
  TInsert2 = Class;
  TList = Class;
  TOutput = Class;
  TUpdate = Class;
  TAnalyzeArray = Array of TAnalyze;
  TInputArray = Array of TInput;
  TInsertArray = Array of TInsert;
  TInsert2Array = Array of TInsert2;
  TListArray = Array of TList;
  TOutputArray = Array of TOutput;
  TUpdateArray = Array of TUpdate;
  //Anonymous types, using auto-generated names
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem = Class;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical = Class;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric = Class;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypetext = Class;
  TAnalyzeTypedataDescriptionTypefeaturesItem = Class;
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric = Class;
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem = Class;
  TAnalyzeTypedataDescriptionTypeoutputFeature = Class;
  TAnalyzeTypedataDescription = Class;
  TAnalyzeTypeerrorsItem = Class;
  TAnalyzeTypemodelDescriptionTypeconfusionMatrix = Class;
  TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals = Class;
  TAnalyzeTypemodelDescription = Class;
  TInputTypeinput = Class;
  TInsertTypetrainingInstancesItem = Class;
  TInsertTypeutilityItem = Class;
  TInsert2TypemodelInfo = Class;
  TOutputTypeoutputMultiItem = Class;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesArray = Array of TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem;
  TAnalyzeTypedataDescriptionTypefeaturesArray = Array of TAnalyzeTypedataDescriptionTypefeaturesItem;
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextArray = Array of TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem;
  TAnalyzeTypeerrorsArray = Array of TAnalyzeTypeerrorsItem;
  TInsertTypetrainingInstancesArray = Array of TInsertTypetrainingInstancesItem;
  TInsertTypeutilityArray = Array of TInsertTypeutilityItem;
  TListTypeitemsArray = Array of TInsert2;
  TOutputTypeoutputMultiArray = Array of TOutputTypeoutputMultiItem;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItemClass = Class of TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    Fvalues : TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesArray;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalues(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property values : TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesArray Index 8 Read Fvalues Write Setvalues;
  end;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalClass = Class of TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    Fmean : String;
    Fvariance : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Setmean(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariance(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property mean : String Index 8 Read Fmean Write Setmean;
    Property variance : String Index 16 Read Fvariance Write Setvariance;
  end;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypenumericClass = Class of TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescriptionTypefeaturesItemTypetext
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescriptionTypefeaturesItemTypetext = Class(TGoogleBaseObject)
  Private
    Fcount : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
  end;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypetextClass = Class of TAnalyzeTypedataDescriptionTypefeaturesItemTypetext;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescriptionTypefeaturesItem
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescriptionTypefeaturesItem = Class(TGoogleBaseObject)
  Private
    Fcategorical : TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical;
    Findex : String;
    Fnumeric : TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric;
    Ftext : TAnalyzeTypedataDescriptionTypefeaturesItemTypetext;
  Protected
    //Property setters
    Procedure Setcategorical(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical); virtual;
    Procedure Setindex(AIndex : Integer; AValue : String); virtual;
    Procedure Setnumeric(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric); virtual;
    Procedure Settext(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesItemTypetext); virtual;
  Public
  Published
    Property categorical : TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical Index 0 Read Fcategorical Write Setcategorical;
    Property index : String Index 8 Read Findex Write Setindex;
    Property numeric : TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric Index 16 Read Fnumeric Write Setnumeric;
    Property text : TAnalyzeTypedataDescriptionTypefeaturesItemTypetext Index 24 Read Ftext Write Settext;
  end;
  TAnalyzeTypedataDescriptionTypefeaturesItemClass = Class of TAnalyzeTypedataDescriptionTypefeaturesItem;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    Fmean : String;
    Fvariance : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Setmean(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariance(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property mean : String Index 8 Read Fmean Write Setmean;
    Property variance : String Index 16 Read Fvariance Write Setvariance;
  end;
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumericClass = Class of TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItemClass = Class of TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescriptionTypeoutputFeature
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescriptionTypeoutputFeature = Class(TGoogleBaseObject)
  Private
    Fnumeric : TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric;
    Ftext : TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextArray;
  Protected
    //Property setters
    Procedure Setnumeric(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric); virtual;
    Procedure Settext(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property numeric : TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric Index 0 Read Fnumeric Write Setnumeric;
    Property text : TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextArray Index 8 Read Ftext Write Settext;
  end;
  TAnalyzeTypedataDescriptionTypeoutputFeatureClass = Class of TAnalyzeTypedataDescriptionTypeoutputFeature;
  
  { --------------------------------------------------------------------
    TAnalyzeTypedataDescription
    --------------------------------------------------------------------}
  
  TAnalyzeTypedataDescription = Class(TGoogleBaseObject)
  Private
    Ffeatures : TAnalyzeTypedataDescriptionTypefeaturesArray;
    FoutputFeature : TAnalyzeTypedataDescriptionTypeoutputFeature;
  Protected
    //Property setters
    Procedure Setfeatures(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesArray); virtual;
    Procedure SetoutputFeature(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypeoutputFeature); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property features : TAnalyzeTypedataDescriptionTypefeaturesArray Index 0 Read Ffeatures Write Setfeatures;
    Property outputFeature : TAnalyzeTypedataDescriptionTypeoutputFeature Index 8 Read FoutputFeature Write SetoutputFeature;
  end;
  TAnalyzeTypedataDescriptionClass = Class of TAnalyzeTypedataDescription;
  
  { --------------------------------------------------------------------
    TAnalyzeTypeerrorsItem
    --------------------------------------------------------------------}
  
  TAnalyzeTypeerrorsItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnalyzeTypeerrorsItemClass = Class of TAnalyzeTypeerrorsItem;
  
  { --------------------------------------------------------------------
    TAnalyzeTypemodelDescriptionTypeconfusionMatrix
    --------------------------------------------------------------------}
  
  TAnalyzeTypemodelDescriptionTypeconfusionMatrix = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnalyzeTypemodelDescriptionTypeconfusionMatrixClass = Class of TAnalyzeTypemodelDescriptionTypeconfusionMatrix;
  
  { --------------------------------------------------------------------
    TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals
    --------------------------------------------------------------------}
  
  TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotalsClass = Class of TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals;
  
  { --------------------------------------------------------------------
    TAnalyzeTypemodelDescription
    --------------------------------------------------------------------}
  
  TAnalyzeTypemodelDescription = Class(TGoogleBaseObject)
  Private
    FconfusionMatrix : TAnalyzeTypemodelDescriptionTypeconfusionMatrix;
    FconfusionMatrixRowTotals : TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals;
    Fmodelinfo : TInsert2;
  Protected
    //Property setters
    Procedure SetconfusionMatrix(AIndex : Integer; AValue : TAnalyzeTypemodelDescriptionTypeconfusionMatrix); virtual;
    Procedure SetconfusionMatrixRowTotals(AIndex : Integer; AValue : TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals); virtual;
    Procedure Setmodelinfo(AIndex : Integer; AValue : TInsert2); virtual;
  Public
  Published
    Property confusionMatrix : TAnalyzeTypemodelDescriptionTypeconfusionMatrix Index 0 Read FconfusionMatrix Write SetconfusionMatrix;
    Property confusionMatrixRowTotals : TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals Index 8 Read FconfusionMatrixRowTotals Write SetconfusionMatrixRowTotals;
    Property modelinfo : TInsert2 Index 16 Read Fmodelinfo Write Setmodelinfo;
  end;
  TAnalyzeTypemodelDescriptionClass = Class of TAnalyzeTypemodelDescription;
  
  { --------------------------------------------------------------------
    TAnalyze
    --------------------------------------------------------------------}
  
  TAnalyze = Class(TGoogleBaseObject)
  Private
    FdataDescription : TAnalyzeTypedataDescription;
    Ferrors : TAnalyzeTypeerrorsArray;
    Fid : String;
    Fkind : String;
    FmodelDescription : TAnalyzeTypemodelDescription;
    FselfLink : String;
  Protected
    //Property setters
    Procedure SetdataDescription(AIndex : Integer; AValue : TAnalyzeTypedataDescription); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TAnalyzeTypeerrorsArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodelDescription(AIndex : Integer; AValue : TAnalyzeTypemodelDescription); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dataDescription : TAnalyzeTypedataDescription Index 0 Read FdataDescription Write SetdataDescription;
    Property errors : TAnalyzeTypeerrorsArray Index 8 Read Ferrors Write Seterrors;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property modelDescription : TAnalyzeTypemodelDescription Index 32 Read FmodelDescription Write SetmodelDescription;
    Property selfLink : String Index 40 Read FselfLink Write SetselfLink;
  end;
  TAnalyzeClass = Class of TAnalyze;
  
  { --------------------------------------------------------------------
    TInputTypeinput
    --------------------------------------------------------------------}
  
  TInputTypeinput = Class(TGoogleBaseObject)
  Private
    FcsvInstance : TTJSONSchemaArray;
  Protected
    //Property setters
    Procedure SetcsvInstance(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property csvInstance : TTJSONSchemaArray Index 0 Read FcsvInstance Write SetcsvInstance;
  end;
  TInputTypeinputClass = Class of TInputTypeinput;
  
  { --------------------------------------------------------------------
    TInput
    --------------------------------------------------------------------}
  
  TInput = Class(TGoogleBaseObject)
  Private
    Finput : TInputTypeinput;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; AValue : TInputTypeinput); virtual;
  Public
  Published
    Property input : TInputTypeinput Index 0 Read Finput Write Setinput;
  end;
  TInputClass = Class of TInput;
  
  { --------------------------------------------------------------------
    TInsertTypetrainingInstancesItem
    --------------------------------------------------------------------}
  
  TInsertTypetrainingInstancesItem = Class(TGoogleBaseObject)
  Private
    FcsvInstance : TTJSONSchemaArray;
    Foutput : String;
  Protected
    //Property setters
    Procedure SetcsvInstance(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    Procedure Setoutput(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property csvInstance : TTJSONSchemaArray Index 0 Read FcsvInstance Write SetcsvInstance;
    Property output : String Index 8 Read Foutput Write Setoutput;
  end;
  TInsertTypetrainingInstancesItemClass = Class of TInsertTypetrainingInstancesItem;
  
  { --------------------------------------------------------------------
    TInsertTypeutilityItem
    --------------------------------------------------------------------}
  
  TInsertTypeutilityItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TInsertTypeutilityItemClass = Class of TInsertTypeutilityItem;
  
  { --------------------------------------------------------------------
    TInsert
    --------------------------------------------------------------------}
  
  TInsert = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FmodelType : String;
    FsourceModel : String;
    FstorageDataLocation : String;
    FstoragePMMLLocation : String;
    FstoragePMMLModelLocation : String;
    FtrainingInstances : TInsertTypetrainingInstancesArray;
    Futility : TInsertTypeutilityArray;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodelType(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceModel(AIndex : Integer; AValue : String); virtual;
    Procedure SetstorageDataLocation(AIndex : Integer; AValue : String); virtual;
    Procedure SetstoragePMMLLocation(AIndex : Integer; AValue : String); virtual;
    Procedure SetstoragePMMLModelLocation(AIndex : Integer; AValue : String); virtual;
    Procedure SettrainingInstances(AIndex : Integer; AValue : TInsertTypetrainingInstancesArray); virtual;
    Procedure Setutility(AIndex : Integer; AValue : TInsertTypeutilityArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property modelType : String Index 8 Read FmodelType Write SetmodelType;
    Property sourceModel : String Index 16 Read FsourceModel Write SetsourceModel;
    Property storageDataLocation : String Index 24 Read FstorageDataLocation Write SetstorageDataLocation;
    Property storagePMMLLocation : String Index 32 Read FstoragePMMLLocation Write SetstoragePMMLLocation;
    Property storagePMMLModelLocation : String Index 40 Read FstoragePMMLModelLocation Write SetstoragePMMLModelLocation;
    Property trainingInstances : TInsertTypetrainingInstancesArray Index 48 Read FtrainingInstances Write SettrainingInstances;
    Property utility : TInsertTypeutilityArray Index 56 Read Futility Write Setutility;
  end;
  TInsertClass = Class of TInsert;
  
  { --------------------------------------------------------------------
    TInsert2TypemodelInfo
    --------------------------------------------------------------------}
  
  TInsert2TypemodelInfo = Class(TGoogleBaseObject)
  Private
    FclassWeightedAccuracy : String;
    FclassificationAccuracy : String;
    FmeanSquaredError : String;
    FmodelType : String;
    FnumberInstances : String;
    FnumberLabels : String;
  Protected
    //Property setters
    Procedure SetclassWeightedAccuracy(AIndex : Integer; AValue : String); virtual;
    Procedure SetclassificationAccuracy(AIndex : Integer; AValue : String); virtual;
    Procedure SetmeanSquaredError(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodelType(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumberInstances(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumberLabels(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property classWeightedAccuracy : String Index 0 Read FclassWeightedAccuracy Write SetclassWeightedAccuracy;
    Property classificationAccuracy : String Index 8 Read FclassificationAccuracy Write SetclassificationAccuracy;
    Property meanSquaredError : String Index 16 Read FmeanSquaredError Write SetmeanSquaredError;
    Property modelType : String Index 24 Read FmodelType Write SetmodelType;
    Property numberInstances : String Index 32 Read FnumberInstances Write SetnumberInstances;
    Property numberLabels : String Index 40 Read FnumberLabels Write SetnumberLabels;
  end;
  TInsert2TypemodelInfoClass = Class of TInsert2TypemodelInfo;
  
  { --------------------------------------------------------------------
    TInsert2
    --------------------------------------------------------------------}
  
  TInsert2 = Class(TGoogleBaseObject)
  Private
    Fcreated : TDatetime;
    Fid : String;
    Fkind : String;
    FmodelInfo : TInsert2TypemodelInfo;
    FmodelType : String;
    FselfLink : String;
    FstorageDataLocation : String;
    FstoragePMMLLocation : String;
    FstoragePMMLModelLocation : String;
    FtrainingComplete : TDatetime;
    FtrainingStatus : String;
  Protected
    //Property setters
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodelInfo(AIndex : Integer; AValue : TInsert2TypemodelInfo); virtual;
    Procedure SetmodelType(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstorageDataLocation(AIndex : Integer; AValue : String); virtual;
    Procedure SetstoragePMMLLocation(AIndex : Integer; AValue : String); virtual;
    Procedure SetstoragePMMLModelLocation(AIndex : Integer; AValue : String); virtual;
    Procedure SettrainingComplete(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SettrainingStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property created : TDatetime Index 0 Read Fcreated Write Setcreated;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property modelInfo : TInsert2TypemodelInfo Index 24 Read FmodelInfo Write SetmodelInfo;
    Property modelType : String Index 32 Read FmodelType Write SetmodelType;
    Property selfLink : String Index 40 Read FselfLink Write SetselfLink;
    Property storageDataLocation : String Index 48 Read FstorageDataLocation Write SetstorageDataLocation;
    Property storagePMMLLocation : String Index 56 Read FstoragePMMLLocation Write SetstoragePMMLLocation;
    Property storagePMMLModelLocation : String Index 64 Read FstoragePMMLModelLocation Write SetstoragePMMLModelLocation;
    Property trainingComplete : TDatetime Index 72 Read FtrainingComplete Write SettrainingComplete;
    Property trainingStatus : String Index 80 Read FtrainingStatus Write SettrainingStatus;
  end;
  TInsert2Class = Class of TInsert2;
  
  { --------------------------------------------------------------------
    TList
    --------------------------------------------------------------------}
  
  TList = Class(TGoogleBaseObject)
  Private
    Fitems : TListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 24 Read FselfLink Write SetselfLink;
  end;
  TListClass = Class of TList;
  
  { --------------------------------------------------------------------
    TOutputTypeoutputMultiItem
    --------------------------------------------------------------------}
  
  TOutputTypeoutputMultiItem = Class(TGoogleBaseObject)
  Private
    F_label : String;
    Fscore : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
    Procedure Setscore(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _label : String Index 0 Read F_label Write Set_label;
    Property score : String Index 8 Read Fscore Write Setscore;
  end;
  TOutputTypeoutputMultiItemClass = Class of TOutputTypeoutputMultiItem;
  
  { --------------------------------------------------------------------
    TOutput
    --------------------------------------------------------------------}
  
  TOutput = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    FoutputLabel : String;
    FoutputMulti : TOutputTypeoutputMultiArray;
    FoutputValue : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetoutputLabel(AIndex : Integer; AValue : String); virtual;
    Procedure SetoutputMulti(AIndex : Integer; AValue : TOutputTypeoutputMultiArray); virtual;
    Procedure SetoutputValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property outputLabel : String Index 16 Read FoutputLabel Write SetoutputLabel;
    Property outputMulti : TOutputTypeoutputMultiArray Index 24 Read FoutputMulti Write SetoutputMulti;
    Property outputValue : String Index 32 Read FoutputValue Write SetoutputValue;
    Property selfLink : String Index 40 Read FselfLink Write SetselfLink;
  end;
  TOutputClass = Class of TOutput;
  
  { --------------------------------------------------------------------
    TUpdate
    --------------------------------------------------------------------}
  
  TUpdate = Class(TGoogleBaseObject)
  Private
    FcsvInstance : TTJSONSchemaArray;
    Foutput : String;
  Protected
    //Property setters
    Procedure SetcsvInstance(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    Procedure Setoutput(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property csvInstance : TTJSONSchemaArray Index 0 Read FcsvInstance Write SetcsvInstance;
    Property output : String Index 8 Read Foutput Write Setoutput;
  end;
  TUpdateClass = Class of TUpdate;
  
  { --------------------------------------------------------------------
    THostedmodelsResource
    --------------------------------------------------------------------}
  
  THostedmodelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Predict(hostedModelName: string; project: string; aInput : TInput) : TOutput;
  end;
  
  
  { --------------------------------------------------------------------
    TTrainedmodelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTrainedmodelsResource, method List
  
  TTrainedmodelsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TTrainedmodelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Analyze(id: string; project: string) : TAnalyze;
    Procedure Delete(id: string; project: string);
    Function Get(id: string; project: string) : TInsert2;
    Function Insert(project: string; aInsert : TInsert) : TInsert2;
    Function List(project: string; AQuery : string  = '') : TList;
    Function List(project: string; AQuery : TTrainedmodelslistOptions) : TList;
    Function Predict(id: string; project: string; aInput : TInput) : TOutput;
    Function Update(id: string; project: string; aUpdate : TUpdate) : TInsert2;
  end;
  
  
  { --------------------------------------------------------------------
    TPredictionAPI
    --------------------------------------------------------------------}
  
  TPredictionAPI = Class(TGoogleAPI)
  Private
    FHostedmodelsInstance : THostedmodelsResource;
    FTrainedmodelsInstance : TTrainedmodelsResource;
    Function GetHostedmodelsInstance : THostedmodelsResource;virtual;
    Function GetTrainedmodelsInstance : TTrainedmodelsResource;virtual;
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
    Function CreateHostedmodelsResource(AOwner : TComponent) : THostedmodelsResource;virtual;overload;
    Function CreateHostedmodelsResource : THostedmodelsResource;virtual;overload;
    Function CreateTrainedmodelsResource(AOwner : TComponent) : TTrainedmodelsResource;virtual;overload;
    Function CreateTrainedmodelsResource : TTrainedmodelsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property HostedmodelsResource : THostedmodelsResource Read GetHostedmodelsInstance;
    Property TrainedmodelsResource : TTrainedmodelsResource Read GetTrainedmodelsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical.Setvalues(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric.Setmean(AIndex : Integer; AValue : String); 

begin
  If (Fmean=AValue) then exit;
  Fmean:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric.Setvariance(AIndex : Integer; AValue : String); 

begin
  If (Fvariance=AValue) then exit;
  Fvariance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzeTypedataDescriptionTypefeaturesItemTypetext
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescriptionTypefeaturesItemTypetext.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzeTypedataDescriptionTypefeaturesItem
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescriptionTypefeaturesItem.Setcategorical(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical); 

begin
  If (Fcategorical=AValue) then exit;
  Fcategorical:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypefeaturesItem.Setindex(AIndex : Integer; AValue : String); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypefeaturesItem.Setnumeric(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric); 

begin
  If (Fnumeric=AValue) then exit;
  Fnumeric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypefeaturesItem.Settext(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesItemTypetext); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric.Setmean(AIndex : Integer; AValue : String); 

begin
  If (Fmean=AValue) then exit;
  Fmean:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric.Setvariance(AIndex : Integer; AValue : String); 

begin
  If (Fvariance=AValue) then exit;
  Fvariance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzeTypedataDescriptionTypeoutputFeature
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescriptionTypeoutputFeature.Setnumeric(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric); 

begin
  If (Fnumeric=AValue) then exit;
  Fnumeric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescriptionTypeoutputFeature.Settext(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextArray); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnalyzeTypedataDescriptionTypeoutputFeature.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'text' : SetLength(Ftext,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnalyzeTypedataDescription
  --------------------------------------------------------------------}


Procedure TAnalyzeTypedataDescription.Setfeatures(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypefeaturesArray); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypedataDescription.SetoutputFeature(AIndex : Integer; AValue : TAnalyzeTypedataDescriptionTypeoutputFeature); 

begin
  If (FoutputFeature=AValue) then exit;
  FoutputFeature:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnalyzeTypedataDescription.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'features' : SetLength(Ffeatures,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnalyzeTypeerrorsItem
  --------------------------------------------------------------------}


Class Function TAnalyzeTypeerrorsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAnalyzeTypemodelDescriptionTypeconfusionMatrix
  --------------------------------------------------------------------}


Class Function TAnalyzeTypemodelDescriptionTypeconfusionMatrix.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals
  --------------------------------------------------------------------}


Class Function TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAnalyzeTypemodelDescription
  --------------------------------------------------------------------}


Procedure TAnalyzeTypemodelDescription.SetconfusionMatrix(AIndex : Integer; AValue : TAnalyzeTypemodelDescriptionTypeconfusionMatrix); 

begin
  If (FconfusionMatrix=AValue) then exit;
  FconfusionMatrix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypemodelDescription.SetconfusionMatrixRowTotals(AIndex : Integer; AValue : TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals); 

begin
  If (FconfusionMatrixRowTotals=AValue) then exit;
  FconfusionMatrixRowTotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzeTypemodelDescription.Setmodelinfo(AIndex : Integer; AValue : TInsert2); 

begin
  If (Fmodelinfo=AValue) then exit;
  Fmodelinfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyze
  --------------------------------------------------------------------}


Procedure TAnalyze.SetdataDescription(AIndex : Integer; AValue : TAnalyzeTypedataDescription); 

begin
  If (FdataDescription=AValue) then exit;
  FdataDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.Seterrors(AIndex : Integer; AValue : TAnalyzeTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.SetmodelDescription(AIndex : Integer; AValue : TAnalyzeTypemodelDescription); 

begin
  If (FmodelDescription=AValue) then exit;
  FmodelDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnalyze.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInputTypeinput
  --------------------------------------------------------------------}


Procedure TInputTypeinput.SetcsvInstance(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (FcsvInstance=AValue) then exit;
  FcsvInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInputTypeinput.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'csvinstance' : SetLength(FcsvInstance,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInput
  --------------------------------------------------------------------}


Procedure TInput.Setinput(AIndex : Integer; AValue : TInputTypeinput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInsertTypetrainingInstancesItem
  --------------------------------------------------------------------}


Procedure TInsertTypetrainingInstancesItem.SetcsvInstance(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (FcsvInstance=AValue) then exit;
  FcsvInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsertTypetrainingInstancesItem.Setoutput(AIndex : Integer; AValue : String); 

begin
  If (Foutput=AValue) then exit;
  Foutput:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInsertTypetrainingInstancesItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'csvinstance' : SetLength(FcsvInstance,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInsertTypeutilityItem
  --------------------------------------------------------------------}


Class Function TInsertTypeutilityItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInsert
  --------------------------------------------------------------------}


Procedure TInsert.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetmodelType(AIndex : Integer; AValue : String); 

begin
  If (FmodelType=AValue) then exit;
  FmodelType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetsourceModel(AIndex : Integer; AValue : String); 

begin
  If (FsourceModel=AValue) then exit;
  FsourceModel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetstorageDataLocation(AIndex : Integer; AValue : String); 

begin
  If (FstorageDataLocation=AValue) then exit;
  FstorageDataLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetstoragePMMLLocation(AIndex : Integer; AValue : String); 

begin
  If (FstoragePMMLLocation=AValue) then exit;
  FstoragePMMLLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetstoragePMMLModelLocation(AIndex : Integer; AValue : String); 

begin
  If (FstoragePMMLModelLocation=AValue) then exit;
  FstoragePMMLModelLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SettrainingInstances(AIndex : Integer; AValue : TInsertTypetrainingInstancesArray); 

begin
  If (FtrainingInstances=AValue) then exit;
  FtrainingInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.Setutility(AIndex : Integer; AValue : TInsertTypeutilityArray); 

begin
  If (Futility=AValue) then exit;
  Futility:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInsert.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'traininginstances' : SetLength(FtrainingInstances,ALength);
  'utility' : SetLength(Futility,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInsert2TypemodelInfo
  --------------------------------------------------------------------}


Procedure TInsert2TypemodelInfo.SetclassWeightedAccuracy(AIndex : Integer; AValue : String); 

begin
  If (FclassWeightedAccuracy=AValue) then exit;
  FclassWeightedAccuracy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2TypemodelInfo.SetclassificationAccuracy(AIndex : Integer; AValue : String); 

begin
  If (FclassificationAccuracy=AValue) then exit;
  FclassificationAccuracy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2TypemodelInfo.SetmeanSquaredError(AIndex : Integer; AValue : String); 

begin
  If (FmeanSquaredError=AValue) then exit;
  FmeanSquaredError:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2TypemodelInfo.SetmodelType(AIndex : Integer; AValue : String); 

begin
  If (FmodelType=AValue) then exit;
  FmodelType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2TypemodelInfo.SetnumberInstances(AIndex : Integer; AValue : String); 

begin
  If (FnumberInstances=AValue) then exit;
  FnumberInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2TypemodelInfo.SetnumberLabels(AIndex : Integer; AValue : String); 

begin
  If (FnumberLabels=AValue) then exit;
  FnumberLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInsert2
  --------------------------------------------------------------------}


Procedure TInsert2.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetmodelInfo(AIndex : Integer; AValue : TInsert2TypemodelInfo); 

begin
  If (FmodelInfo=AValue) then exit;
  FmodelInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetmodelType(AIndex : Integer; AValue : String); 

begin
  If (FmodelType=AValue) then exit;
  FmodelType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetstorageDataLocation(AIndex : Integer; AValue : String); 

begin
  If (FstorageDataLocation=AValue) then exit;
  FstorageDataLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetstoragePMMLLocation(AIndex : Integer; AValue : String); 

begin
  If (FstoragePMMLLocation=AValue) then exit;
  FstoragePMMLLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetstoragePMMLModelLocation(AIndex : Integer; AValue : String); 

begin
  If (FstoragePMMLModelLocation=AValue) then exit;
  FstoragePMMLModelLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SettrainingComplete(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtrainingComplete=AValue) then exit;
  FtrainingComplete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SettrainingStatus(AIndex : Integer; AValue : String); 

begin
  If (FtrainingStatus=AValue) then exit;
  FtrainingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TList
  --------------------------------------------------------------------}


Procedure TList.Setitems(AIndex : Integer; AValue : TListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOutputTypeoutputMultiItem
  --------------------------------------------------------------------}


Procedure TOutputTypeoutputMultiItem.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutputTypeoutputMultiItem.Setscore(AIndex : Integer; AValue : String); 

begin
  If (Fscore=AValue) then exit;
  Fscore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TOutputTypeoutputMultiItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TOutput
  --------------------------------------------------------------------}


Procedure TOutput.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.SetoutputLabel(AIndex : Integer; AValue : String); 

begin
  If (FoutputLabel=AValue) then exit;
  FoutputLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.SetoutputMulti(AIndex : Integer; AValue : TOutputTypeoutputMultiArray); 

begin
  If (FoutputMulti=AValue) then exit;
  FoutputMulti:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.SetoutputValue(AIndex : Integer; AValue : String); 

begin
  If (FoutputValue=AValue) then exit;
  FoutputValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOutput.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'outputmulti' : SetLength(FoutputMulti,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUpdate
  --------------------------------------------------------------------}


Procedure TUpdate.SetcsvInstance(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (FcsvInstance=AValue) then exit;
  FcsvInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdate.Setoutput(AIndex : Integer; AValue : String); 

begin
  If (Foutput=AValue) then exit;
  Foutput:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUpdate.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'csvinstance' : SetLength(FcsvInstance,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  THostedmodelsResource
  --------------------------------------------------------------------}


Class Function THostedmodelsResource.ResourceName : String;

begin
  Result:='hostedmodels';
end;

Class Function THostedmodelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpredictionAPI;
end;

Function THostedmodelsResource.Predict(hostedModelName: string; project: string; aInput : TInput) : TOutput;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/hostedmodels/{hostedModelName}/predict';
  _Methodid   = 'prediction.hostedmodels.predict';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['hostedModelName',hostedModelName,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInput,TOutput) as TOutput;
end;



{ --------------------------------------------------------------------
  TTrainedmodelsResource
  --------------------------------------------------------------------}


Class Function TTrainedmodelsResource.ResourceName : String;

begin
  Result:='trainedmodels';
end;

Class Function TTrainedmodelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpredictionAPI;
end;

Function TTrainedmodelsResource.Analyze(id: string; project: string) : TAnalyze;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/trainedmodels/{id}/analyze';
  _Methodid   = 'prediction.trainedmodels.analyze';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAnalyze) as TAnalyze;
end;

Procedure TTrainedmodelsResource.Delete(id: string; project: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/trainedmodels/{id}';
  _Methodid   = 'prediction.trainedmodels.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'project',project]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTrainedmodelsResource.Get(id: string; project: string) : TInsert2;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/trainedmodels/{id}';
  _Methodid   = 'prediction.trainedmodels.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInsert2) as TInsert2;
end;

Function TTrainedmodelsResource.Insert(project: string; aInsert : TInsert) : TInsert2;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/trainedmodels';
  _Methodid   = 'prediction.trainedmodels.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInsert,TInsert2) as TInsert2;
end;

Function TTrainedmodelsResource.List(project: string; AQuery : string = '') : TList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/trainedmodels/list';
  _Methodid   = 'prediction.trainedmodels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TList) as TList;
end;


Function TTrainedmodelsResource.List(project: string; AQuery : TTrainedmodelslistOptions) : TList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TTrainedmodelsResource.Predict(id: string; project: string; aInput : TInput) : TOutput;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/trainedmodels/{id}/predict';
  _Methodid   = 'prediction.trainedmodels.predict';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInput,TOutput) as TOutput;
end;

Function TTrainedmodelsResource.Update(id: string; project: string; aUpdate : TUpdate) : TInsert2;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{project}/trainedmodels/{id}';
  _Methodid   = 'prediction.trainedmodels.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUpdate,TInsert2) as TInsert2;
end;



{ --------------------------------------------------------------------
  TPredictionAPI
  --------------------------------------------------------------------}

Class Function TPredictionAPI.APIName : String;

begin
  Result:='prediction';
end;

Class Function TPredictionAPI.APIVersion : String;

begin
  Result:='v1.6';
end;

Class Function TPredictionAPI.APIRevision : String;

begin
  Result:='20140522';
end;

Class Function TPredictionAPI.APIID : String;

begin
  Result:='prediction:v1.6';
end;

Class Function TPredictionAPI.APITitle : String;

begin
  Result:='Prediction API';
end;

Class Function TPredictionAPI.APIDescription : String;

begin
  Result:='Lets you access a cloud hosted machine learning service that makes it easy to build smart apps';
end;

Class Function TPredictionAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TPredictionAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TPredictionAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/feature/predictionapi-16.png';
end;

Class Function TPredictionAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/feature/predictionapi-32.png';
end;

Class Function TPredictionAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/prediction/docs/developer-guide';
end;

Class Function TPredictionAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TPredictionAPI.APIbasePath : string;

begin
  Result:='/prediction/v1.6/projects/';
end;

Class Function TPredictionAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/prediction/v1.6/projects/';
end;

Class Function TPredictionAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPredictionAPI.APIservicePath : string;

begin
  Result:='prediction/v1.6/projects/';
end;

Class Function TPredictionAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TPredictionAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/devstorage.full_control';
  Result[0].Description:='Manage your data and permissions in Google Cloud Storage';
  Result[1].Name:='https://www.googleapis.com/auth/devstorage.read_only';
  Result[1].Description:='View your data in Google Cloud Storage';
  Result[2].Name:='https://www.googleapis.com/auth/devstorage.read_write';
  Result[2].Description:='Manage your data in Google Cloud Storage';
  Result[3].Name:='https://www.googleapis.com/auth/prediction';
  Result[3].Description:='Manage your data in the Google Prediction API';
  
end;

Class Function TPredictionAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TPredictionAPI.RegisterAPIResources;

begin
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategoricalTypevaluesItem.RegisterObject;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypecategorical.RegisterObject;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypenumeric.RegisterObject;
  TAnalyzeTypedataDescriptionTypefeaturesItemTypetext.RegisterObject;
  TAnalyzeTypedataDescriptionTypefeaturesItem.RegisterObject;
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypenumeric.RegisterObject;
  TAnalyzeTypedataDescriptionTypeoutputFeatureTypetextItem.RegisterObject;
  TAnalyzeTypedataDescriptionTypeoutputFeature.RegisterObject;
  TAnalyzeTypedataDescription.RegisterObject;
  TAnalyzeTypeerrorsItem.RegisterObject;
  TAnalyzeTypemodelDescriptionTypeconfusionMatrix.RegisterObject;
  TAnalyzeTypemodelDescriptionTypeconfusionMatrixRowTotals.RegisterObject;
  TAnalyzeTypemodelDescription.RegisterObject;
  TAnalyze.RegisterObject;
  TInputTypeinput.RegisterObject;
  TInput.RegisterObject;
  TInsertTypetrainingInstancesItem.RegisterObject;
  TInsertTypeutilityItem.RegisterObject;
  TInsert.RegisterObject;
  TInsert2TypemodelInfo.RegisterObject;
  TInsert2.RegisterObject;
  TList.RegisterObject;
  TOutputTypeoutputMultiItem.RegisterObject;
  TOutput.RegisterObject;
  TUpdate.RegisterObject;
end;


Function TPredictionAPI.GetHostedmodelsInstance : THostedmodelsResource;

begin
  if (FHostedmodelsInstance=Nil) then
    FHostedmodelsInstance:=CreateHostedmodelsResource;
  Result:=FHostedmodelsInstance;
end;

Function TPredictionAPI.CreateHostedmodelsResource : THostedmodelsResource;

begin
  Result:=CreateHostedmodelsResource(Self);
end;


Function TPredictionAPI.CreateHostedmodelsResource(AOwner : TComponent) : THostedmodelsResource;

begin
  Result:=THostedmodelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPredictionAPI.GetTrainedmodelsInstance : TTrainedmodelsResource;

begin
  if (FTrainedmodelsInstance=Nil) then
    FTrainedmodelsInstance:=CreateTrainedmodelsResource;
  Result:=FTrainedmodelsInstance;
end;

Function TPredictionAPI.CreateTrainedmodelsResource : TTrainedmodelsResource;

begin
  Result:=CreateTrainedmodelsResource(Self);
end;


Function TPredictionAPI.CreateTrainedmodelsResource(AOwner : TComponent) : TTrainedmodelsResource;

begin
  Result:=TTrainedmodelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TPredictionAPI.RegisterAPI;
end.
