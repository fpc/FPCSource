unit googleprediction;
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
  TAnalyze = class;
  TAnalyzeArray = Array of TAnalyze;
  TAnalyzedataDescription = class;
  TAnalyzedataDescriptionArray = Array of TAnalyzedataDescription;
  TAnalyzedataDescriptionfeatures = class;
  TAnalyzedataDescriptionfeaturesArray = Array of TAnalyzedataDescriptionfeatures;
  TAnalyzedataDescriptionfeaturescategorical = class;
  TAnalyzedataDescriptionfeaturescategoricalArray = Array of TAnalyzedataDescriptionfeaturescategorical;
  TAnalyzedataDescriptionfeaturescategoricalvalues = class;
  TAnalyzedataDescriptionfeaturescategoricalvaluesArray = Array of TAnalyzedataDescriptionfeaturescategoricalvalues;
  TAnalyzedataDescriptionfeaturesnumeric = class;
  TAnalyzedataDescriptionfeaturesnumericArray = Array of TAnalyzedataDescriptionfeaturesnumeric;
  TAnalyzedataDescriptionfeaturestext = class;
  TAnalyzedataDescriptionfeaturestextArray = Array of TAnalyzedataDescriptionfeaturestext;
  TAnalyzedataDescriptionoutputFeature = class;
  TAnalyzedataDescriptionoutputFeatureArray = Array of TAnalyzedataDescriptionoutputFeature;
  TAnalyzedataDescriptionoutputFeaturenumeric = class;
  TAnalyzedataDescriptionoutputFeaturenumericArray = Array of TAnalyzedataDescriptionoutputFeaturenumeric;
  TAnalyzedataDescriptionoutputFeaturetext = class;
  TAnalyzedataDescriptionoutputFeaturetextArray = Array of TAnalyzedataDescriptionoutputFeaturetext;
  TAnalyzeerrors = class;
  TAnalyzeerrorsArray = Array of TAnalyzeerrors;
  TAnalyzemodelDescription = class;
  TAnalyzemodelDescriptionArray = Array of TAnalyzemodelDescription;
  TAnalyzemodelDescriptionconfusionMatrix = class;
  TAnalyzemodelDescriptionconfusionMatrixArray = Array of TAnalyzemodelDescriptionconfusionMatrix;
  TAnalyzemodelDescriptionconfusionMatrixRowTotals = class;
  TAnalyzemodelDescriptionconfusionMatrixRowTotalsArray = Array of TAnalyzemodelDescriptionconfusionMatrixRowTotals;
  TInput = class;
  TInputArray = Array of TInput;
  TInputinput = class;
  TInputinputArray = Array of TInputinput;
  TInputinputcsvInstance = class;
  TInputinputcsvInstanceArray = Array of TInputinputcsvInstance;
  TInsert = class;
  TInsertArray = Array of TInsert;
  TInserttrainingInstances = class;
  TInserttrainingInstancesArray = Array of TInserttrainingInstances;
  TInserttrainingInstancescsvInstance = class;
  TInserttrainingInstancescsvInstanceArray = Array of TInserttrainingInstancescsvInstance;
  TInsertutility = class;
  TInsertutilityArray = Array of TInsertutility;
  TInsert2 = class;
  TInsert2Array = Array of TInsert2;
  TInsert2modelInfo = class;
  TInsert2modelInfoArray = Array of TInsert2modelInfo;
  TList = class;
  TListArray = Array of TList;
  TListitems = class;
  TListitemsArray = Array of TListitems;
  TOutput = class;
  TOutputArray = Array of TOutput;
  TOutputoutputMulti = class;
  TOutputoutputMultiArray = Array of TOutputoutputMulti;
  TUpdate = class;
  TUpdateArray = Array of TUpdate;
  TUpdatecsvInstance = class;
  TUpdatecsvInstanceArray = Array of TUpdatecsvInstance;
  
  { --------------------------------------------------------------------
    TAnalyze
    --------------------------------------------------------------------}
  
  TAnalyze = Class(TGoogleBaseObject)
  Private
    FdataDescription : TAnalyzedataDescription;
    Ferrors : TAnalyzeerrors;
    Fid : string;
    Fkind : string;
    FmodelDescription : TAnalyzemodelDescription;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetdataDescription(AIndex : Integer; AValue : TAnalyzedataDescription); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TAnalyzeerrors); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodelDescription(AIndex : Integer; AValue : TAnalyzemodelDescription); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dataDescription : TAnalyzedataDescription Index 0 Read FdataDescription Write SetdataDescription;
    Property errors : TAnalyzeerrors Index 8 Read Ferrors Write Seterrors;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property modelDescription : TAnalyzemodelDescription Index 32 Read FmodelDescription Write SetmodelDescription;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
  end;
  TAnalyzeClass = Class of TAnalyze;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescription
    --------------------------------------------------------------------}
  
  TAnalyzedataDescription = Class(TGoogleBaseObject)
  Private
    Ffeatures : TAnalyzedataDescriptionfeatures;
    FoutputFeature : TAnalyzedataDescriptionoutputFeature;
  Protected
    //Property setters
    Procedure Setfeatures(AIndex : Integer; AValue : TAnalyzedataDescriptionfeatures); virtual;
    Procedure SetoutputFeature(AIndex : Integer; AValue : TAnalyzedataDescriptionoutputFeature); virtual;
  Public
  Published
    Property features : TAnalyzedataDescriptionfeatures Index 0 Read Ffeatures Write Setfeatures;
    Property outputFeature : TAnalyzedataDescriptionoutputFeature Index 8 Read FoutputFeature Write SetoutputFeature;
  end;
  TAnalyzedataDescriptionClass = Class of TAnalyzedataDescription;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescriptionfeatures
    --------------------------------------------------------------------}
  
  TAnalyzedataDescriptionfeatures = Class(TGoogleBaseObject)
  Private
    Fcategorical : TAnalyzedataDescriptionfeaturescategorical;
    Findex : string;
    Fnumeric : TAnalyzedataDescriptionfeaturesnumeric;
    Ftext : TAnalyzedataDescriptionfeaturestext;
  Protected
    //Property setters
    Procedure Setcategorical(AIndex : Integer; AValue : TAnalyzedataDescriptionfeaturescategorical); virtual;
    Procedure Setindex(AIndex : Integer; AValue : string); virtual;
    Procedure Setnumeric(AIndex : Integer; AValue : TAnalyzedataDescriptionfeaturesnumeric); virtual;
    Procedure Settext(AIndex : Integer; AValue : TAnalyzedataDescriptionfeaturestext); virtual;
  Public
  Published
    Property categorical : TAnalyzedataDescriptionfeaturescategorical Index 0 Read Fcategorical Write Setcategorical;
    Property index : string Index 8 Read Findex Write Setindex;
    Property numeric : TAnalyzedataDescriptionfeaturesnumeric Index 16 Read Fnumeric Write Setnumeric;
    Property text : TAnalyzedataDescriptionfeaturestext Index 24 Read Ftext Write Settext;
  end;
  TAnalyzedataDescriptionfeaturesClass = Class of TAnalyzedataDescriptionfeatures;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescriptionfeaturescategorical
    --------------------------------------------------------------------}
  
  TAnalyzedataDescriptionfeaturescategorical = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    Fvalues : TAnalyzedataDescriptionfeaturescategoricalvalues;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalues(AIndex : Integer; AValue : TAnalyzedataDescriptionfeaturescategoricalvalues); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property values : TAnalyzedataDescriptionfeaturescategoricalvalues Index 8 Read Fvalues Write Setvalues;
  end;
  TAnalyzedataDescriptionfeaturescategoricalClass = Class of TAnalyzedataDescriptionfeaturescategorical;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescriptionfeaturescategoricalvalues
    --------------------------------------------------------------------}
  
  TAnalyzedataDescriptionfeaturescategoricalvalues = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TAnalyzedataDescriptionfeaturescategoricalvaluesClass = Class of TAnalyzedataDescriptionfeaturescategoricalvalues;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescriptionfeaturesnumeric
    --------------------------------------------------------------------}
  
  TAnalyzedataDescriptionfeaturesnumeric = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    Fmean : string;
    Fvariance : string;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure Setmean(AIndex : Integer; AValue : string); virtual;
    Procedure Setvariance(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property mean : string Index 8 Read Fmean Write Setmean;
    Property variance : string Index 16 Read Fvariance Write Setvariance;
  end;
  TAnalyzedataDescriptionfeaturesnumericClass = Class of TAnalyzedataDescriptionfeaturesnumeric;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescriptionfeaturestext
    --------------------------------------------------------------------}
  
  TAnalyzedataDescriptionfeaturestext = Class(TGoogleBaseObject)
  Private
    Fcount : string;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
  end;
  TAnalyzedataDescriptionfeaturestextClass = Class of TAnalyzedataDescriptionfeaturestext;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescriptionoutputFeature
    --------------------------------------------------------------------}
  
  TAnalyzedataDescriptionoutputFeature = Class(TGoogleBaseObject)
  Private
    Fnumeric : TAnalyzedataDescriptionoutputFeaturenumeric;
    Ftext : TAnalyzedataDescriptionoutputFeaturetext;
  Protected
    //Property setters
    Procedure Setnumeric(AIndex : Integer; AValue : TAnalyzedataDescriptionoutputFeaturenumeric); virtual;
    Procedure Settext(AIndex : Integer; AValue : TAnalyzedataDescriptionoutputFeaturetext); virtual;
  Public
  Published
    Property numeric : TAnalyzedataDescriptionoutputFeaturenumeric Index 0 Read Fnumeric Write Setnumeric;
    Property text : TAnalyzedataDescriptionoutputFeaturetext Index 8 Read Ftext Write Settext;
  end;
  TAnalyzedataDescriptionoutputFeatureClass = Class of TAnalyzedataDescriptionoutputFeature;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescriptionoutputFeaturenumeric
    --------------------------------------------------------------------}
  
  TAnalyzedataDescriptionoutputFeaturenumeric = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    Fmean : string;
    Fvariance : string;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure Setmean(AIndex : Integer; AValue : string); virtual;
    Procedure Setvariance(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property mean : string Index 8 Read Fmean Write Setmean;
    Property variance : string Index 16 Read Fvariance Write Setvariance;
  end;
  TAnalyzedataDescriptionoutputFeaturenumericClass = Class of TAnalyzedataDescriptionoutputFeaturenumeric;
  
  { --------------------------------------------------------------------
    TAnalyzedataDescriptionoutputFeaturetext
    --------------------------------------------------------------------}
  
  TAnalyzedataDescriptionoutputFeaturetext = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TAnalyzedataDescriptionoutputFeaturetextClass = Class of TAnalyzedataDescriptionoutputFeaturetext;
  
  { --------------------------------------------------------------------
    TAnalyzeerrors
    --------------------------------------------------------------------}
  
  TAnalyzeerrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnalyzeerrorsClass = Class of TAnalyzeerrors;
  
  { --------------------------------------------------------------------
    TAnalyzemodelDescription
    --------------------------------------------------------------------}
  
  TAnalyzemodelDescription = Class(TGoogleBaseObject)
  Private
    FconfusionMatrix : TAnalyzemodelDescriptionconfusionMatrix;
    FconfusionMatrixRowTotals : TAnalyzemodelDescriptionconfusionMatrixRowTotals;
    Fmodelinfo : TInsert2;
  Protected
    //Property setters
    Procedure SetconfusionMatrix(AIndex : Integer; AValue : TAnalyzemodelDescriptionconfusionMatrix); virtual;
    Procedure SetconfusionMatrixRowTotals(AIndex : Integer; AValue : TAnalyzemodelDescriptionconfusionMatrixRowTotals); virtual;
    Procedure Setmodelinfo(AIndex : Integer; AValue : TInsert2); virtual;
  Public
  Published
    Property confusionMatrix : TAnalyzemodelDescriptionconfusionMatrix Index 0 Read FconfusionMatrix Write SetconfusionMatrix;
    Property confusionMatrixRowTotals : TAnalyzemodelDescriptionconfusionMatrixRowTotals Index 8 Read FconfusionMatrixRowTotals Write SetconfusionMatrixRowTotals;
    Property modelinfo : TInsert2 Index 16 Read Fmodelinfo Write Setmodelinfo;
  end;
  TAnalyzemodelDescriptionClass = Class of TAnalyzemodelDescription;
  
  { --------------------------------------------------------------------
    TAnalyzemodelDescriptionconfusionMatrix
    --------------------------------------------------------------------}
  
  TAnalyzemodelDescriptionconfusionMatrix = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnalyzemodelDescriptionconfusionMatrixClass = Class of TAnalyzemodelDescriptionconfusionMatrix;
  
  { --------------------------------------------------------------------
    TAnalyzemodelDescriptionconfusionMatrixRowTotals
    --------------------------------------------------------------------}
  
  TAnalyzemodelDescriptionconfusionMatrixRowTotals = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnalyzemodelDescriptionconfusionMatrixRowTotalsClass = Class of TAnalyzemodelDescriptionconfusionMatrixRowTotals;
  
  { --------------------------------------------------------------------
    TInput
    --------------------------------------------------------------------}
  
  TInput = Class(TGoogleBaseObject)
  Private
    Finput : TInputinput;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; AValue : TInputinput); virtual;
  Public
  Published
    Property input : TInputinput Index 0 Read Finput Write Setinput;
  end;
  TInputClass = Class of TInput;
  
  { --------------------------------------------------------------------
    TInputinput
    --------------------------------------------------------------------}
  
  TInputinput = Class(TGoogleBaseObject)
  Private
    FcsvInstance : TInputinputcsvInstance;
  Protected
    //Property setters
    Procedure SetcsvInstance(AIndex : Integer; AValue : TInputinputcsvInstance); virtual;
  Public
  Published
    Property csvInstance : TInputinputcsvInstance Index 0 Read FcsvInstance Write SetcsvInstance;
  end;
  TInputinputClass = Class of TInputinput;
  
  { --------------------------------------------------------------------
    TInputinputcsvInstance
    --------------------------------------------------------------------}
  
  TInputinputcsvInstance = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInputinputcsvInstanceClass = Class of TInputinputcsvInstance;
  
  { --------------------------------------------------------------------
    TInsert
    --------------------------------------------------------------------}
  
  TInsert = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FmodelType : string;
    FsourceModel : string;
    FstorageDataLocation : string;
    FstoragePMMLLocation : string;
    FstoragePMMLModelLocation : string;
    FtrainingInstances : TInserttrainingInstances;
    Futility : TInsertutility;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodelType(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceModel(AIndex : Integer; AValue : string); virtual;
    Procedure SetstorageDataLocation(AIndex : Integer; AValue : string); virtual;
    Procedure SetstoragePMMLLocation(AIndex : Integer; AValue : string); virtual;
    Procedure SetstoragePMMLModelLocation(AIndex : Integer; AValue : string); virtual;
    Procedure SettrainingInstances(AIndex : Integer; AValue : TInserttrainingInstances); virtual;
    Procedure Setutility(AIndex : Integer; AValue : TInsertutility); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property modelType : string Index 8 Read FmodelType Write SetmodelType;
    Property sourceModel : string Index 16 Read FsourceModel Write SetsourceModel;
    Property storageDataLocation : string Index 24 Read FstorageDataLocation Write SetstorageDataLocation;
    Property storagePMMLLocation : string Index 32 Read FstoragePMMLLocation Write SetstoragePMMLLocation;
    Property storagePMMLModelLocation : string Index 40 Read FstoragePMMLModelLocation Write SetstoragePMMLModelLocation;
    Property trainingInstances : TInserttrainingInstances Index 48 Read FtrainingInstances Write SettrainingInstances;
    Property utility : TInsertutility Index 56 Read Futility Write Setutility;
  end;
  TInsertClass = Class of TInsert;
  
  { --------------------------------------------------------------------
    TInserttrainingInstances
    --------------------------------------------------------------------}
  
  TInserttrainingInstances = Class(TGoogleBaseObject)
  Private
    FcsvInstance : TInserttrainingInstancescsvInstance;
    Foutput : string;
  Protected
    //Property setters
    Procedure SetcsvInstance(AIndex : Integer; AValue : TInserttrainingInstancescsvInstance); virtual;
    Procedure Setoutput(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property csvInstance : TInserttrainingInstancescsvInstance Index 0 Read FcsvInstance Write SetcsvInstance;
    Property output : string Index 8 Read Foutput Write Setoutput;
  end;
  TInserttrainingInstancesClass = Class of TInserttrainingInstances;
  
  { --------------------------------------------------------------------
    TInserttrainingInstancescsvInstance
    --------------------------------------------------------------------}
  
  TInserttrainingInstancescsvInstance = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInserttrainingInstancescsvInstanceClass = Class of TInserttrainingInstancescsvInstance;
  
  { --------------------------------------------------------------------
    TInsertutility
    --------------------------------------------------------------------}
  
  TInsertutility = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInsertutilityClass = Class of TInsertutility;
  
  { --------------------------------------------------------------------
    TInsert2
    --------------------------------------------------------------------}
  
  TInsert2 = Class(TGoogleBaseObject)
  Private
    Fcreated : TDatetime;
    Fid : string;
    Fkind : string;
    FmodelInfo : TInsert2modelInfo;
    FmodelType : string;
    FselfLink : string;
    FstorageDataLocation : string;
    FstoragePMMLLocation : string;
    FstoragePMMLModelLocation : string;
    FtrainingComplete : TDatetime;
    FtrainingStatus : string;
  Protected
    //Property setters
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodelInfo(AIndex : Integer; AValue : TInsert2modelInfo); virtual;
    Procedure SetmodelType(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstorageDataLocation(AIndex : Integer; AValue : string); virtual;
    Procedure SetstoragePMMLLocation(AIndex : Integer; AValue : string); virtual;
    Procedure SetstoragePMMLModelLocation(AIndex : Integer; AValue : string); virtual;
    Procedure SettrainingComplete(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SettrainingStatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property created : TDatetime Index 0 Read Fcreated Write Setcreated;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property modelInfo : TInsert2modelInfo Index 24 Read FmodelInfo Write SetmodelInfo;
    Property modelType : string Index 32 Read FmodelType Write SetmodelType;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
    Property storageDataLocation : string Index 48 Read FstorageDataLocation Write SetstorageDataLocation;
    Property storagePMMLLocation : string Index 56 Read FstoragePMMLLocation Write SetstoragePMMLLocation;
    Property storagePMMLModelLocation : string Index 64 Read FstoragePMMLModelLocation Write SetstoragePMMLModelLocation;
    Property trainingComplete : TDatetime Index 72 Read FtrainingComplete Write SettrainingComplete;
    Property trainingStatus : string Index 80 Read FtrainingStatus Write SettrainingStatus;
  end;
  TInsert2Class = Class of TInsert2;
  
  { --------------------------------------------------------------------
    TInsert2modelInfo
    --------------------------------------------------------------------}
  
  TInsert2modelInfo = Class(TGoogleBaseObject)
  Private
    FclassWeightedAccuracy : string;
    FclassificationAccuracy : string;
    FmeanSquaredError : string;
    FmodelType : string;
    FnumberInstances : string;
    FnumberLabels : string;
  Protected
    //Property setters
    Procedure SetclassWeightedAccuracy(AIndex : Integer; AValue : string); virtual;
    Procedure SetclassificationAccuracy(AIndex : Integer; AValue : string); virtual;
    Procedure SetmeanSquaredError(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodelType(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumberInstances(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumberLabels(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property classWeightedAccuracy : string Index 0 Read FclassWeightedAccuracy Write SetclassWeightedAccuracy;
    Property classificationAccuracy : string Index 8 Read FclassificationAccuracy Write SetclassificationAccuracy;
    Property meanSquaredError : string Index 16 Read FmeanSquaredError Write SetmeanSquaredError;
    Property modelType : string Index 24 Read FmodelType Write SetmodelType;
    Property numberInstances : string Index 32 Read FnumberInstances Write SetnumberInstances;
    Property numberLabels : string Index 40 Read FnumberLabels Write SetnumberLabels;
  end;
  TInsert2modelInfoClass = Class of TInsert2modelInfo;
  
  { --------------------------------------------------------------------
    TList
    --------------------------------------------------------------------}
  
  TList = Class(TGoogleBaseObject)
  Private
    Fitems : TListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TListClass = Class of TList;
  
  { --------------------------------------------------------------------
    TListitems
    --------------------------------------------------------------------}
  
  TListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListitemsClass = Class of TListitems;
  
  { --------------------------------------------------------------------
    TOutput
    --------------------------------------------------------------------}
  
  TOutput = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    FoutputLabel : string;
    FoutputMulti : TOutputoutputMulti;
    FoutputValue : double;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetoutputLabel(AIndex : Integer; AValue : string); virtual;
    Procedure SetoutputMulti(AIndex : Integer; AValue : TOutputoutputMulti); virtual;
    Procedure SetoutputValue(AIndex : Integer; AValue : double); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property outputLabel : string Index 16 Read FoutputLabel Write SetoutputLabel;
    Property outputMulti : TOutputoutputMulti Index 24 Read FoutputMulti Write SetoutputMulti;
    Property outputValue : double Index 32 Read FoutputValue Write SetoutputValue;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
  end;
  TOutputClass = Class of TOutput;
  
  { --------------------------------------------------------------------
    TOutputoutputMulti
    --------------------------------------------------------------------}
  
  TOutputoutputMulti = Class(TGoogleBaseObject)
  Private
    F_label : string;
    Fscore : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_label(AIndex : Integer; AValue : string); virtual;
    Procedure Setscore(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _label : string Index 0 Read F_label Write Set_label;
    Property score : string Index 8 Read Fscore Write Setscore;
  end;
  TOutputoutputMultiClass = Class of TOutputoutputMulti;
  
  { --------------------------------------------------------------------
    TUpdate
    --------------------------------------------------------------------}
  
  TUpdate = Class(TGoogleBaseObject)
  Private
    FcsvInstance : TUpdatecsvInstance;
    Foutput : string;
  Protected
    //Property setters
    Procedure SetcsvInstance(AIndex : Integer; AValue : TUpdatecsvInstance); virtual;
    Procedure Setoutput(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property csvInstance : TUpdatecsvInstance Index 0 Read FcsvInstance Write SetcsvInstance;
    Property output : string Index 8 Read Foutput Write Setoutput;
  end;
  TUpdateClass = Class of TUpdate;
  
  { --------------------------------------------------------------------
    TUpdatecsvInstance
    --------------------------------------------------------------------}
  
  TUpdatecsvInstance = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUpdatecsvInstanceClass = Class of TUpdatecsvInstance;
  
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
    pageToken : string;
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
  TAnalyze
  --------------------------------------------------------------------}


Procedure TAnalyze.SetdataDescription(AIndex : Integer; AValue : TAnalyzedataDescription); 

begin
  If (FdataDescription=AValue) then exit;
  FdataDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.Seterrors(AIndex : Integer; AValue : TAnalyzeerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.SetmodelDescription(AIndex : Integer; AValue : TAnalyzemodelDescription); 

begin
  If (FmodelDescription=AValue) then exit;
  FmodelDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyze.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescription
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescription.Setfeatures(AIndex : Integer; AValue : TAnalyzedataDescriptionfeatures); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescription.SetoutputFeature(AIndex : Integer; AValue : TAnalyzedataDescriptionoutputFeature); 

begin
  If (FoutputFeature=AValue) then exit;
  FoutputFeature:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescriptionfeatures
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescriptionfeatures.Setcategorical(AIndex : Integer; AValue : TAnalyzedataDescriptionfeaturescategorical); 

begin
  If (Fcategorical=AValue) then exit;
  Fcategorical:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionfeatures.Setindex(AIndex : Integer; AValue : string); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionfeatures.Setnumeric(AIndex : Integer; AValue : TAnalyzedataDescriptionfeaturesnumeric); 

begin
  If (Fnumeric=AValue) then exit;
  Fnumeric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionfeatures.Settext(AIndex : Integer; AValue : TAnalyzedataDescriptionfeaturestext); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescriptionfeaturescategorical
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescriptionfeaturescategorical.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionfeaturescategorical.Setvalues(AIndex : Integer; AValue : TAnalyzedataDescriptionfeaturescategoricalvalues); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescriptionfeaturescategoricalvalues
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescriptionfeaturescategoricalvalues.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionfeaturescategoricalvalues.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescriptionfeaturesnumeric
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescriptionfeaturesnumeric.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionfeaturesnumeric.Setmean(AIndex : Integer; AValue : string); 

begin
  If (Fmean=AValue) then exit;
  Fmean:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionfeaturesnumeric.Setvariance(AIndex : Integer; AValue : string); 

begin
  If (Fvariance=AValue) then exit;
  Fvariance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescriptionfeaturestext
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescriptionfeaturestext.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescriptionoutputFeature
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescriptionoutputFeature.Setnumeric(AIndex : Integer; AValue : TAnalyzedataDescriptionoutputFeaturenumeric); 

begin
  If (Fnumeric=AValue) then exit;
  Fnumeric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionoutputFeature.Settext(AIndex : Integer; AValue : TAnalyzedataDescriptionoutputFeaturetext); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescriptionoutputFeaturenumeric
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescriptionoutputFeaturenumeric.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionoutputFeaturenumeric.Setmean(AIndex : Integer; AValue : string); 

begin
  If (Fmean=AValue) then exit;
  Fmean:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionoutputFeaturenumeric.Setvariance(AIndex : Integer; AValue : string); 

begin
  If (Fvariance=AValue) then exit;
  Fvariance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzedataDescriptionoutputFeaturetext
  --------------------------------------------------------------------}


Procedure TAnalyzedataDescriptionoutputFeaturetext.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzedataDescriptionoutputFeaturetext.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzeerrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAnalyzemodelDescription
  --------------------------------------------------------------------}


Procedure TAnalyzemodelDescription.SetconfusionMatrix(AIndex : Integer; AValue : TAnalyzemodelDescriptionconfusionMatrix); 

begin
  If (FconfusionMatrix=AValue) then exit;
  FconfusionMatrix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzemodelDescription.SetconfusionMatrixRowTotals(AIndex : Integer; AValue : TAnalyzemodelDescriptionconfusionMatrixRowTotals); 

begin
  If (FconfusionMatrixRowTotals=AValue) then exit;
  FconfusionMatrixRowTotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyzemodelDescription.Setmodelinfo(AIndex : Integer; AValue : TInsert2); 

begin
  If (Fmodelinfo=AValue) then exit;
  Fmodelinfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyzemodelDescriptionconfusionMatrix
  --------------------------------------------------------------------}


Class Function TAnalyzemodelDescriptionconfusionMatrix.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAnalyzemodelDescriptionconfusionMatrixRowTotals
  --------------------------------------------------------------------}


Class Function TAnalyzemodelDescriptionconfusionMatrixRowTotals.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInput
  --------------------------------------------------------------------}


Procedure TInput.Setinput(AIndex : Integer; AValue : TInputinput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInputinput
  --------------------------------------------------------------------}


Procedure TInputinput.SetcsvInstance(AIndex : Integer; AValue : TInputinputcsvInstance); 

begin
  If (FcsvInstance=AValue) then exit;
  FcsvInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInputinputcsvInstance
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInsert
  --------------------------------------------------------------------}


Procedure TInsert.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetmodelType(AIndex : Integer; AValue : string); 

begin
  If (FmodelType=AValue) then exit;
  FmodelType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetsourceModel(AIndex : Integer; AValue : string); 

begin
  If (FsourceModel=AValue) then exit;
  FsourceModel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetstorageDataLocation(AIndex : Integer; AValue : string); 

begin
  If (FstorageDataLocation=AValue) then exit;
  FstorageDataLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetstoragePMMLLocation(AIndex : Integer; AValue : string); 

begin
  If (FstoragePMMLLocation=AValue) then exit;
  FstoragePMMLLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SetstoragePMMLModelLocation(AIndex : Integer; AValue : string); 

begin
  If (FstoragePMMLModelLocation=AValue) then exit;
  FstoragePMMLModelLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.SettrainingInstances(AIndex : Integer; AValue : TInserttrainingInstances); 

begin
  If (FtrainingInstances=AValue) then exit;
  FtrainingInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert.Setutility(AIndex : Integer; AValue : TInsertutility); 

begin
  If (Futility=AValue) then exit;
  Futility:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInserttrainingInstances
  --------------------------------------------------------------------}


Procedure TInserttrainingInstances.SetcsvInstance(AIndex : Integer; AValue : TInserttrainingInstancescsvInstance); 

begin
  If (FcsvInstance=AValue) then exit;
  FcsvInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInserttrainingInstances.Setoutput(AIndex : Integer; AValue : string); 

begin
  If (Foutput=AValue) then exit;
  Foutput:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInserttrainingInstancescsvInstance
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInsertutility
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInsert2
  --------------------------------------------------------------------}


Procedure TInsert2.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetmodelInfo(AIndex : Integer; AValue : TInsert2modelInfo); 

begin
  If (FmodelInfo=AValue) then exit;
  FmodelInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetmodelType(AIndex : Integer; AValue : string); 

begin
  If (FmodelType=AValue) then exit;
  FmodelType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetstorageDataLocation(AIndex : Integer; AValue : string); 

begin
  If (FstorageDataLocation=AValue) then exit;
  FstorageDataLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetstoragePMMLLocation(AIndex : Integer; AValue : string); 

begin
  If (FstoragePMMLLocation=AValue) then exit;
  FstoragePMMLLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2.SetstoragePMMLModelLocation(AIndex : Integer; AValue : string); 

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



Procedure TInsert2.SettrainingStatus(AIndex : Integer; AValue : string); 

begin
  If (FtrainingStatus=AValue) then exit;
  FtrainingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInsert2modelInfo
  --------------------------------------------------------------------}


Procedure TInsert2modelInfo.SetclassWeightedAccuracy(AIndex : Integer; AValue : string); 

begin
  If (FclassWeightedAccuracy=AValue) then exit;
  FclassWeightedAccuracy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2modelInfo.SetclassificationAccuracy(AIndex : Integer; AValue : string); 

begin
  If (FclassificationAccuracy=AValue) then exit;
  FclassificationAccuracy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2modelInfo.SetmeanSquaredError(AIndex : Integer; AValue : string); 

begin
  If (FmeanSquaredError=AValue) then exit;
  FmeanSquaredError:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2modelInfo.SetmodelType(AIndex : Integer; AValue : string); 

begin
  If (FmodelType=AValue) then exit;
  FmodelType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2modelInfo.SetnumberInstances(AIndex : Integer; AValue : string); 

begin
  If (FnumberInstances=AValue) then exit;
  FnumberInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsert2modelInfo.SetnumberLabels(AIndex : Integer; AValue : string); 

begin
  If (FnumberLabels=AValue) then exit;
  FnumberLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TList
  --------------------------------------------------------------------}


Procedure TList.Setitems(AIndex : Integer; AValue : TListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOutput
  --------------------------------------------------------------------}


Procedure TOutput.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.SetoutputLabel(AIndex : Integer; AValue : string); 

begin
  If (FoutputLabel=AValue) then exit;
  FoutputLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.SetoutputMulti(AIndex : Integer; AValue : TOutputoutputMulti); 

begin
  If (FoutputMulti=AValue) then exit;
  FoutputMulti:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.SetoutputValue(AIndex : Integer; AValue : double); 

begin
  If (FoutputValue=AValue) then exit;
  FoutputValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutput.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOutputoutputMulti
  --------------------------------------------------------------------}


Procedure TOutputoutputMulti.Set_label(AIndex : Integer; AValue : string); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutputoutputMulti.Setscore(AIndex : Integer; AValue : string); 

begin
  If (Fscore=AValue) then exit;
  Fscore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TOutputoutputMulti.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TUpdate
  --------------------------------------------------------------------}


Procedure TUpdate.SetcsvInstance(AIndex : Integer; AValue : TUpdatecsvInstance); 

begin
  If (FcsvInstance=AValue) then exit;
  FcsvInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdate.Setoutput(AIndex : Integer; AValue : string); 

begin
  If (Foutput=AValue) then exit;
  Foutput:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdatecsvInstance
  --------------------------------------------------------------------}




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
  Result:='https://www.googleapis.com/';
end;

Class Function TPredictionAPI.APIbasePath : string;

begin
  Result:='/prediction/v1.6/projects/';
end;

Class Function TPredictionAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/prediction/v1.6/projects/';
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
  TAnalyze.RegisterObject;
  TAnalyzedataDescription.RegisterObject;
  TAnalyzedataDescriptionfeatures.RegisterObject;
  TAnalyzedataDescriptionfeaturescategorical.RegisterObject;
  TAnalyzedataDescriptionfeaturescategoricalvalues.RegisterObject;
  TAnalyzedataDescriptionfeaturesnumeric.RegisterObject;
  TAnalyzedataDescriptionfeaturestext.RegisterObject;
  TAnalyzedataDescriptionoutputFeature.RegisterObject;
  TAnalyzedataDescriptionoutputFeaturenumeric.RegisterObject;
  TAnalyzedataDescriptionoutputFeaturetext.RegisterObject;
  TAnalyzeerrors.RegisterObject;
  TAnalyzemodelDescription.RegisterObject;
  TAnalyzemodelDescriptionconfusionMatrix.RegisterObject;
  TAnalyzemodelDescriptionconfusionMatrixRowTotals.RegisterObject;
  TInput.RegisterObject;
  TInputinput.RegisterObject;
  TInputinputcsvInstance.RegisterObject;
  TInsert.RegisterObject;
  TInserttrainingInstances.RegisterObject;
  TInserttrainingInstancescsvInstance.RegisterObject;
  TInsertutility.RegisterObject;
  TInsert2.RegisterObject;
  TInsert2modelInfo.RegisterObject;
  TList.RegisterObject;
  TListitems.RegisterObject;
  TOutput.RegisterObject;
  TOutputoutputMulti.RegisterObject;
  TUpdate.RegisterObject;
  TUpdatecsvInstance.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TPredictionAPI.RegisterAPI;
end.
