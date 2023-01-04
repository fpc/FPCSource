unit googlevision;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TImageSource = Class;
  TAnnotateImageRequest = Class;
  TAnnotateImageResponse = Class;
  TLatLongRect = Class;
  TStatus = Class;
  TFaceAnnotation = Class;
  TVertex = Class;
  TColorInfo = Class;
  TBoundingPoly = Class;
  TLandmark = Class;
  TImageContext = Class;
  TBatchAnnotateImagesRequest = Class;
  TEntityAnnotation = Class;
  TProperty = Class;
  TColor = Class;
  TLocationInfo = Class;
  TSafeSearchAnnotation = Class;
  TImage = Class;
  TDominantColorsAnnotation = Class;
  TFeature = Class;
  TBatchAnnotateImagesResponse = Class;
  TImageProperties = Class;
  TLatLng = Class;
  TPosition = Class;
  TImageSourceArray = Array of TImageSource;
  TAnnotateImageRequestArray = Array of TAnnotateImageRequest;
  TAnnotateImageResponseArray = Array of TAnnotateImageResponse;
  TLatLongRectArray = Array of TLatLongRect;
  TStatusArray = Array of TStatus;
  TFaceAnnotationArray = Array of TFaceAnnotation;
  TVertexArray = Array of TVertex;
  TColorInfoArray = Array of TColorInfo;
  TBoundingPolyArray = Array of TBoundingPoly;
  TLandmarkArray = Array of TLandmark;
  TImageContextArray = Array of TImageContext;
  TBatchAnnotateImagesRequestArray = Array of TBatchAnnotateImagesRequest;
  TEntityAnnotationArray = Array of TEntityAnnotation;
  TPropertyArray = Array of TProperty;
  TColorArray = Array of TColor;
  TLocationInfoArray = Array of TLocationInfo;
  TSafeSearchAnnotationArray = Array of TSafeSearchAnnotation;
  TImageArray = Array of TImage;
  TDominantColorsAnnotationArray = Array of TDominantColorsAnnotation;
  TFeatureArray = Array of TFeature;
  TBatchAnnotateImagesResponseArray = Array of TBatchAnnotateImagesResponse;
  TImagePropertiesArray = Array of TImageProperties;
  TLatLngArray = Array of TLatLng;
  TPositionArray = Array of TPosition;
  //Anonymous types, using auto-generated names
  TStatusTypedetailsItem = Class;
  TAnnotateImageRequestTypefeaturesArray = Array of TFeature;
  TAnnotateImageResponseTypelabelAnnotationsArray = Array of TEntityAnnotation;
  TAnnotateImageResponseTypelandmarkAnnotationsArray = Array of TEntityAnnotation;
  TAnnotateImageResponseTypetextAnnotationsArray = Array of TEntityAnnotation;
  TAnnotateImageResponseTypelogoAnnotationsArray = Array of TEntityAnnotation;
  TAnnotateImageResponseTypefaceAnnotationsArray = Array of TFaceAnnotation;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TFaceAnnotationTypelandmarksArray = Array of TLandmark;
  TBoundingPolyTypeverticesArray = Array of TVertex;
  TBatchAnnotateImagesRequestTyperequestsArray = Array of TAnnotateImageRequest;
  TEntityAnnotationTypepropertiesArray = Array of TProperty;
  TEntityAnnotationTypelocationsArray = Array of TLocationInfo;
  TDominantColorsAnnotationTypecolorsArray = Array of TColorInfo;
  TBatchAnnotateImagesResponseTyperesponsesArray = Array of TAnnotateImageResponse;
  
  { --------------------------------------------------------------------
    TImageSource
    --------------------------------------------------------------------}
  
  TImageSource = Class(TGoogleBaseObject)
  Private
    FgcsImageUri : String;
  Protected
    //Property setters
    Procedure SetgcsImageUri(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property gcsImageUri : String Index 0 Read FgcsImageUri Write SetgcsImageUri;
  end;
  TImageSourceClass = Class of TImageSource;
  
  { --------------------------------------------------------------------
    TAnnotateImageRequest
    --------------------------------------------------------------------}
  
  TAnnotateImageRequest = Class(TGoogleBaseObject)
  Private
    Fimage : TImage;
    FimageContext : TImageContext;
    Ffeatures : TAnnotateImageRequestTypefeaturesArray;
  Protected
    //Property setters
    Procedure Setimage(AIndex : Integer; const AValue : TImage); virtual;
    Procedure SetimageContext(AIndex : Integer; const AValue : TImageContext); virtual;
    Procedure Setfeatures(AIndex : Integer; const AValue : TAnnotateImageRequestTypefeaturesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property image : TImage Index 0 Read Fimage Write Setimage;
    Property imageContext : TImageContext Index 8 Read FimageContext Write SetimageContext;
    Property features : TAnnotateImageRequestTypefeaturesArray Index 16 Read Ffeatures Write Setfeatures;
  end;
  TAnnotateImageRequestClass = Class of TAnnotateImageRequest;
  
  { --------------------------------------------------------------------
    TAnnotateImageResponse
    --------------------------------------------------------------------}
  
  TAnnotateImageResponse = Class(TGoogleBaseObject)
  Private
    FlabelAnnotations : TAnnotateImageResponseTypelabelAnnotationsArray;
    FlandmarkAnnotations : TAnnotateImageResponseTypelandmarkAnnotationsArray;
    FsafeSearchAnnotation : TSafeSearchAnnotation;
    FimagePropertiesAnnotation : TImageProperties;
    FtextAnnotations : TAnnotateImageResponseTypetextAnnotationsArray;
    FlogoAnnotations : TAnnotateImageResponseTypelogoAnnotationsArray;
    FfaceAnnotations : TAnnotateImageResponseTypefaceAnnotationsArray;
    Ferror : TStatus;
  Protected
    //Property setters
    Procedure SetlabelAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypelabelAnnotationsArray); virtual;
    Procedure SetlandmarkAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypelandmarkAnnotationsArray); virtual;
    Procedure SetsafeSearchAnnotation(AIndex : Integer; const AValue : TSafeSearchAnnotation); virtual;
    Procedure SetimagePropertiesAnnotation(AIndex : Integer; const AValue : TImageProperties); virtual;
    Procedure SettextAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypetextAnnotationsArray); virtual;
    Procedure SetlogoAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypelogoAnnotationsArray); virtual;
    Procedure SetfaceAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypefaceAnnotationsArray); virtual;
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property labelAnnotations : TAnnotateImageResponseTypelabelAnnotationsArray Index 0 Read FlabelAnnotations Write SetlabelAnnotations;
    Property landmarkAnnotations : TAnnotateImageResponseTypelandmarkAnnotationsArray Index 8 Read FlandmarkAnnotations Write SetlandmarkAnnotations;
    Property safeSearchAnnotation : TSafeSearchAnnotation Index 16 Read FsafeSearchAnnotation Write SetsafeSearchAnnotation;
    Property imagePropertiesAnnotation : TImageProperties Index 24 Read FimagePropertiesAnnotation Write SetimagePropertiesAnnotation;
    Property textAnnotations : TAnnotateImageResponseTypetextAnnotationsArray Index 32 Read FtextAnnotations Write SettextAnnotations;
    Property logoAnnotations : TAnnotateImageResponseTypelogoAnnotationsArray Index 40 Read FlogoAnnotations Write SetlogoAnnotations;
    Property faceAnnotations : TAnnotateImageResponseTypefaceAnnotationsArray Index 48 Read FfaceAnnotations Write SetfaceAnnotations;
    Property error : TStatus Index 56 Read Ferror Write Seterror;
  end;
  TAnnotateImageResponseClass = Class of TAnnotateImageResponse;
  
  { --------------------------------------------------------------------
    TLatLongRect
    --------------------------------------------------------------------}
  
  TLatLongRect = Class(TGoogleBaseObject)
  Private
    FmaxLatLng : TLatLng;
    FminLatLng : TLatLng;
  Protected
    //Property setters
    Procedure SetmaxLatLng(AIndex : Integer; const AValue : TLatLng); virtual;
    Procedure SetminLatLng(AIndex : Integer; const AValue : TLatLng); virtual;
  Public
  Published
    Property maxLatLng : TLatLng Index 0 Read FmaxLatLng Write SetmaxLatLng;
    Property minLatLng : TLatLng Index 8 Read FminLatLng Write SetminLatLng;
  end;
  TLatLongRectClass = Class of TLatLongRect;
  
  { --------------------------------------------------------------------
    TStatusTypedetailsItem
    --------------------------------------------------------------------}
  
  TStatusTypedetailsItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TStatusTypedetailsItemClass = Class of TStatusTypedetailsItem;
  
  { --------------------------------------------------------------------
    TStatus
    --------------------------------------------------------------------}
  
  TStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fdetails : TStatusTypedetailsArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property details : TStatusTypedetailsArray Index 8 Read Fdetails Write Setdetails;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TFaceAnnotation
    --------------------------------------------------------------------}
  
  TFaceAnnotation = Class(TGoogleBaseObject)
  Private
    FtiltAngle : integer;
    FunderExposedLikelihood : String;
    FfdBoundingPoly : TBoundingPoly;
    FlandmarkingConfidence : integer;
    FjoyLikelihood : String;
    FdetectionConfidence : integer;
    FsurpriseLikelihood : String;
    FangerLikelihood : String;
    FheadwearLikelihood : String;
    FpanAngle : integer;
    FboundingPoly : TBoundingPoly;
    Flandmarks : TFaceAnnotationTypelandmarksArray;
    FblurredLikelihood : String;
    FrollAngle : integer;
    FsorrowLikelihood : String;
  Protected
    //Property setters
    Procedure SettiltAngle(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetunderExposedLikelihood(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfdBoundingPoly(AIndex : Integer; const AValue : TBoundingPoly); virtual;
    Procedure SetlandmarkingConfidence(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetjoyLikelihood(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdetectionConfidence(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetsurpriseLikelihood(AIndex : Integer; const AValue : String); virtual;
    Procedure SetangerLikelihood(AIndex : Integer; const AValue : String); virtual;
    Procedure SetheadwearLikelihood(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpanAngle(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetboundingPoly(AIndex : Integer; const AValue : TBoundingPoly); virtual;
    Procedure Setlandmarks(AIndex : Integer; const AValue : TFaceAnnotationTypelandmarksArray); virtual;
    Procedure SetblurredLikelihood(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrollAngle(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetsorrowLikelihood(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property tiltAngle : integer Index 0 Read FtiltAngle Write SettiltAngle;
    Property underExposedLikelihood : String Index 8 Read FunderExposedLikelihood Write SetunderExposedLikelihood;
    Property fdBoundingPoly : TBoundingPoly Index 16 Read FfdBoundingPoly Write SetfdBoundingPoly;
    Property landmarkingConfidence : integer Index 24 Read FlandmarkingConfidence Write SetlandmarkingConfidence;
    Property joyLikelihood : String Index 32 Read FjoyLikelihood Write SetjoyLikelihood;
    Property detectionConfidence : integer Index 40 Read FdetectionConfidence Write SetdetectionConfidence;
    Property surpriseLikelihood : String Index 48 Read FsurpriseLikelihood Write SetsurpriseLikelihood;
    Property angerLikelihood : String Index 56 Read FangerLikelihood Write SetangerLikelihood;
    Property headwearLikelihood : String Index 64 Read FheadwearLikelihood Write SetheadwearLikelihood;
    Property panAngle : integer Index 72 Read FpanAngle Write SetpanAngle;
    Property boundingPoly : TBoundingPoly Index 80 Read FboundingPoly Write SetboundingPoly;
    Property landmarks : TFaceAnnotationTypelandmarksArray Index 88 Read Flandmarks Write Setlandmarks;
    Property blurredLikelihood : String Index 96 Read FblurredLikelihood Write SetblurredLikelihood;
    Property rollAngle : integer Index 104 Read FrollAngle Write SetrollAngle;
    Property sorrowLikelihood : String Index 112 Read FsorrowLikelihood Write SetsorrowLikelihood;
  end;
  TFaceAnnotationClass = Class of TFaceAnnotation;
  
  { --------------------------------------------------------------------
    TVertex
    --------------------------------------------------------------------}
  
  TVertex = Class(TGoogleBaseObject)
  Private
    Fy : integer;
    Fx : integer;
  Protected
    //Property setters
    Procedure Sety(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setx(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property y : integer Index 0 Read Fy Write Sety;
    Property x : integer Index 8 Read Fx Write Setx;
  end;
  TVertexClass = Class of TVertex;
  
  { --------------------------------------------------------------------
    TColorInfo
    --------------------------------------------------------------------}
  
  TColorInfo = Class(TGoogleBaseObject)
  Private
    FpixelFraction : integer;
    Fcolor : TColor;
    Fscore : integer;
  Protected
    //Property setters
    Procedure SetpixelFraction(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setcolor(AIndex : Integer; const AValue : TColor); virtual;
    Procedure Setscore(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property pixelFraction : integer Index 0 Read FpixelFraction Write SetpixelFraction;
    Property color : TColor Index 8 Read Fcolor Write Setcolor;
    Property score : integer Index 16 Read Fscore Write Setscore;
  end;
  TColorInfoClass = Class of TColorInfo;
  
  { --------------------------------------------------------------------
    TBoundingPoly
    --------------------------------------------------------------------}
  
  TBoundingPoly = Class(TGoogleBaseObject)
  Private
    Fvertices : TBoundingPolyTypeverticesArray;
  Protected
    //Property setters
    Procedure Setvertices(AIndex : Integer; const AValue : TBoundingPolyTypeverticesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property vertices : TBoundingPolyTypeverticesArray Index 0 Read Fvertices Write Setvertices;
  end;
  TBoundingPolyClass = Class of TBoundingPoly;
  
  { --------------------------------------------------------------------
    TLandmark
    --------------------------------------------------------------------}
  
  TLandmark = Class(TGoogleBaseObject)
  Private
    Fposition : TPosition;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setposition(AIndex : Integer; const AValue : TPosition); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property position : TPosition Index 0 Read Fposition Write Setposition;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TLandmarkClass = Class of TLandmark;
  
  { --------------------------------------------------------------------
    TImageContext
    --------------------------------------------------------------------}
  
  TImageContext = Class(TGoogleBaseObject)
  Private
    FlatLongRect : TLatLongRect;
    FlanguageHints : TStringArray;
  Protected
    //Property setters
    Procedure SetlatLongRect(AIndex : Integer; const AValue : TLatLongRect); virtual;
    Procedure SetlanguageHints(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property latLongRect : TLatLongRect Index 0 Read FlatLongRect Write SetlatLongRect;
    Property languageHints : TStringArray Index 8 Read FlanguageHints Write SetlanguageHints;
  end;
  TImageContextClass = Class of TImageContext;
  
  { --------------------------------------------------------------------
    TBatchAnnotateImagesRequest
    --------------------------------------------------------------------}
  
  TBatchAnnotateImagesRequest = Class(TGoogleBaseObject)
  Private
    Frequests : TBatchAnnotateImagesRequestTyperequestsArray;
  Protected
    //Property setters
    Procedure Setrequests(AIndex : Integer; const AValue : TBatchAnnotateImagesRequestTyperequestsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property requests : TBatchAnnotateImagesRequestTyperequestsArray Index 0 Read Frequests Write Setrequests;
  end;
  TBatchAnnotateImagesRequestClass = Class of TBatchAnnotateImagesRequest;
  
  { --------------------------------------------------------------------
    TEntityAnnotation
    --------------------------------------------------------------------}
  
  TEntityAnnotation = Class(TGoogleBaseObject)
  Private
    Fmid : String;
    Fdescription : String;
    Ftopicality : integer;
    Flocale : String;
    Fproperties : TEntityAnnotationTypepropertiesArray;
    Fscore : integer;
    FboundingPoly : TBoundingPoly;
    Flocations : TEntityAnnotationTypelocationsArray;
    Fconfidence : integer;
  Protected
    //Property setters
    Procedure Setmid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Settopicality(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setlocale(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TEntityAnnotationTypepropertiesArray); virtual;
    Procedure Setscore(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetboundingPoly(AIndex : Integer; const AValue : TBoundingPoly); virtual;
    Procedure Setlocations(AIndex : Integer; const AValue : TEntityAnnotationTypelocationsArray); virtual;
    Procedure Setconfidence(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property mid : String Index 0 Read Fmid Write Setmid;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property topicality : integer Index 16 Read Ftopicality Write Settopicality;
    Property locale : String Index 24 Read Flocale Write Setlocale;
    Property properties : TEntityAnnotationTypepropertiesArray Index 32 Read Fproperties Write Setproperties;
    Property score : integer Index 40 Read Fscore Write Setscore;
    Property boundingPoly : TBoundingPoly Index 48 Read FboundingPoly Write SetboundingPoly;
    Property locations : TEntityAnnotationTypelocationsArray Index 56 Read Flocations Write Setlocations;
    Property confidence : integer Index 64 Read Fconfidence Write Setconfidence;
  end;
  TEntityAnnotationClass = Class of TEntityAnnotation;
  
  { --------------------------------------------------------------------
    TProperty
    --------------------------------------------------------------------}
  
  TProperty = Class(TGoogleBaseObject)
  Private
    Fvalue : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property value : String Index 0 Read Fvalue Write Setvalue;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TPropertyClass = Class of TProperty;
  
  { --------------------------------------------------------------------
    TColor
    --------------------------------------------------------------------}
  
  TColor = Class(TGoogleBaseObject)
  Private
    Fgreen : integer;
    Fblue : integer;
    Fred : integer;
    Falpha : integer;
  Protected
    //Property setters
    Procedure Setgreen(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setblue(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setred(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setalpha(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property green : integer Index 0 Read Fgreen Write Setgreen;
    Property blue : integer Index 8 Read Fblue Write Setblue;
    Property red : integer Index 16 Read Fred Write Setred;
    Property alpha : integer Index 24 Read Falpha Write Setalpha;
  end;
  TColorClass = Class of TColor;
  
  { --------------------------------------------------------------------
    TLocationInfo
    --------------------------------------------------------------------}
  
  TLocationInfo = Class(TGoogleBaseObject)
  Private
    FlatLng : TLatLng;
  Protected
    //Property setters
    Procedure SetlatLng(AIndex : Integer; const AValue : TLatLng); virtual;
  Public
  Published
    Property latLng : TLatLng Index 0 Read FlatLng Write SetlatLng;
  end;
  TLocationInfoClass = Class of TLocationInfo;
  
  { --------------------------------------------------------------------
    TSafeSearchAnnotation
    --------------------------------------------------------------------}
  
  TSafeSearchAnnotation = Class(TGoogleBaseObject)
  Private
    Fmedical : String;
    Fspoof : String;
    Fviolence : String;
    Fadult : String;
  Protected
    //Property setters
    Procedure Setmedical(AIndex : Integer; const AValue : String); virtual;
    Procedure Setspoof(AIndex : Integer; const AValue : String); virtual;
    Procedure Setviolence(AIndex : Integer; const AValue : String); virtual;
    Procedure Setadult(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property medical : String Index 0 Read Fmedical Write Setmedical;
    Property spoof : String Index 8 Read Fspoof Write Setspoof;
    Property violence : String Index 16 Read Fviolence Write Setviolence;
    Property adult : String Index 24 Read Fadult Write Setadult;
  end;
  TSafeSearchAnnotationClass = Class of TSafeSearchAnnotation;
  
  { --------------------------------------------------------------------
    TImage
    --------------------------------------------------------------------}
  
  TImage = Class(TGoogleBaseObject)
  Private
    Fsource : TImageSource;
    Fcontent : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TImageSource); virtual;
    Procedure Setcontent(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TImageSource Index 0 Read Fsource Write Setsource;
    Property content : String Index 8 Read Fcontent Write Setcontent;
  end;
  TImageClass = Class of TImage;
  
  { --------------------------------------------------------------------
    TDominantColorsAnnotation
    --------------------------------------------------------------------}
  
  TDominantColorsAnnotation = Class(TGoogleBaseObject)
  Private
    Fcolors : TDominantColorsAnnotationTypecolorsArray;
  Protected
    //Property setters
    Procedure Setcolors(AIndex : Integer; const AValue : TDominantColorsAnnotationTypecolorsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property colors : TDominantColorsAnnotationTypecolorsArray Index 0 Read Fcolors Write Setcolors;
  end;
  TDominantColorsAnnotationClass = Class of TDominantColorsAnnotation;
  
  { --------------------------------------------------------------------
    TFeature
    --------------------------------------------------------------------}
  
  TFeature = Class(TGoogleBaseObject)
  Private
    F_type : String;
    FmaxResults : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxResults(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property maxResults : integer Index 8 Read FmaxResults Write SetmaxResults;
  end;
  TFeatureClass = Class of TFeature;
  
  { --------------------------------------------------------------------
    TBatchAnnotateImagesResponse
    --------------------------------------------------------------------}
  
  TBatchAnnotateImagesResponse = Class(TGoogleBaseObject)
  Private
    Fresponses : TBatchAnnotateImagesResponseTyperesponsesArray;
  Protected
    //Property setters
    Procedure Setresponses(AIndex : Integer; const AValue : TBatchAnnotateImagesResponseTyperesponsesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property responses : TBatchAnnotateImagesResponseTyperesponsesArray Index 0 Read Fresponses Write Setresponses;
  end;
  TBatchAnnotateImagesResponseClass = Class of TBatchAnnotateImagesResponse;
  
  { --------------------------------------------------------------------
    TImageProperties
    --------------------------------------------------------------------}
  
  TImageProperties = Class(TGoogleBaseObject)
  Private
    FdominantColors : TDominantColorsAnnotation;
  Protected
    //Property setters
    Procedure SetdominantColors(AIndex : Integer; const AValue : TDominantColorsAnnotation); virtual;
  Public
  Published
    Property dominantColors : TDominantColorsAnnotation Index 0 Read FdominantColors Write SetdominantColors;
  end;
  TImagePropertiesClass = Class of TImageProperties;
  
  { --------------------------------------------------------------------
    TLatLng
    --------------------------------------------------------------------}
  
  TLatLng = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TLatLngClass = Class of TLatLng;
  
  { --------------------------------------------------------------------
    TPosition
    --------------------------------------------------------------------}
  
  TPosition = Class(TGoogleBaseObject)
  Private
    Fy : integer;
    Fx : integer;
    Fz : integer;
  Protected
    //Property setters
    Procedure Sety(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setx(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setz(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property y : integer Index 0 Read Fy Write Sety;
    Property x : integer Index 8 Read Fx Write Setx;
    Property z : integer Index 16 Read Fz Write Setz;
  end;
  TPositionClass = Class of TPosition;
  
  { --------------------------------------------------------------------
    TImagesResource
    --------------------------------------------------------------------}
  
  TImagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Annotate(aBatchAnnotateImagesRequest : TBatchAnnotateImagesRequest) : TBatchAnnotateImagesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TVisionAPI
    --------------------------------------------------------------------}
  
  TVisionAPI = Class(TGoogleAPI)
  Private
    FImagesInstance : TImagesResource;
    Function GetImagesInstance : TImagesResource;virtual;
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
    Function CreateImagesResource(AOwner : TComponent) : TImagesResource;virtual;overload;
    Function CreateImagesResource : TImagesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ImagesResource : TImagesResource Read GetImagesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TImageSource
  --------------------------------------------------------------------}


Procedure TImageSource.SetgcsImageUri(AIndex : Integer; const AValue : String); 

begin
  If (FgcsImageUri=AValue) then exit;
  FgcsImageUri:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotateImageRequest
  --------------------------------------------------------------------}


Procedure TAnnotateImageRequest.Setimage(AIndex : Integer; const AValue : TImage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageRequest.SetimageContext(AIndex : Integer; const AValue : TImageContext); 

begin
  If (FimageContext=AValue) then exit;
  FimageContext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageRequest.Setfeatures(AIndex : Integer; const AValue : TAnnotateImageRequestTypefeaturesArray); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnnotateImageRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'features' : SetLength(Ffeatures,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnnotateImageResponse
  --------------------------------------------------------------------}


Procedure TAnnotateImageResponse.SetlabelAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypelabelAnnotationsArray); 

begin
  If (FlabelAnnotations=AValue) then exit;
  FlabelAnnotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageResponse.SetlandmarkAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypelandmarkAnnotationsArray); 

begin
  If (FlandmarkAnnotations=AValue) then exit;
  FlandmarkAnnotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageResponse.SetsafeSearchAnnotation(AIndex : Integer; const AValue : TSafeSearchAnnotation); 

begin
  If (FsafeSearchAnnotation=AValue) then exit;
  FsafeSearchAnnotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageResponse.SetimagePropertiesAnnotation(AIndex : Integer; const AValue : TImageProperties); 

begin
  If (FimagePropertiesAnnotation=AValue) then exit;
  FimagePropertiesAnnotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageResponse.SettextAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypetextAnnotationsArray); 

begin
  If (FtextAnnotations=AValue) then exit;
  FtextAnnotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageResponse.SetlogoAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypelogoAnnotationsArray); 

begin
  If (FlogoAnnotations=AValue) then exit;
  FlogoAnnotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageResponse.SetfaceAnnotations(AIndex : Integer; const AValue : TAnnotateImageResponseTypefaceAnnotationsArray); 

begin
  If (FfaceAnnotations=AValue) then exit;
  FfaceAnnotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotateImageResponse.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnnotateImageResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labelannotations' : SetLength(FlabelAnnotations,ALength);
  'landmarkannotations' : SetLength(FlandmarkAnnotations,ALength);
  'textannotations' : SetLength(FtextAnnotations,ALength);
  'logoannotations' : SetLength(FlogoAnnotations,ALength);
  'faceannotations' : SetLength(FfaceAnnotations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLatLongRect
  --------------------------------------------------------------------}


Procedure TLatLongRect.SetmaxLatLng(AIndex : Integer; const AValue : TLatLng); 

begin
  If (FmaxLatLng=AValue) then exit;
  FmaxLatLng:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLatLongRect.SetminLatLng(AIndex : Integer; const AValue : TLatLng); 

begin
  If (FminLatLng=AValue) then exit;
  FminLatLng:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStatusTypedetailsItem
  --------------------------------------------------------------------}


Class Function TStatusTypedetailsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStatus
  --------------------------------------------------------------------}


Procedure TStatus.Setcode(AIndex : Integer; const AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'details' : SetLength(Fdetails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFaceAnnotation
  --------------------------------------------------------------------}


Procedure TFaceAnnotation.SettiltAngle(AIndex : Integer; const AValue : integer); 

begin
  If (FtiltAngle=AValue) then exit;
  FtiltAngle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetunderExposedLikelihood(AIndex : Integer; const AValue : String); 

begin
  If (FunderExposedLikelihood=AValue) then exit;
  FunderExposedLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetfdBoundingPoly(AIndex : Integer; const AValue : TBoundingPoly); 

begin
  If (FfdBoundingPoly=AValue) then exit;
  FfdBoundingPoly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetlandmarkingConfidence(AIndex : Integer; const AValue : integer); 

begin
  If (FlandmarkingConfidence=AValue) then exit;
  FlandmarkingConfidence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetjoyLikelihood(AIndex : Integer; const AValue : String); 

begin
  If (FjoyLikelihood=AValue) then exit;
  FjoyLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetdetectionConfidence(AIndex : Integer; const AValue : integer); 

begin
  If (FdetectionConfidence=AValue) then exit;
  FdetectionConfidence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetsurpriseLikelihood(AIndex : Integer; const AValue : String); 

begin
  If (FsurpriseLikelihood=AValue) then exit;
  FsurpriseLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetangerLikelihood(AIndex : Integer; const AValue : String); 

begin
  If (FangerLikelihood=AValue) then exit;
  FangerLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetheadwearLikelihood(AIndex : Integer; const AValue : String); 

begin
  If (FheadwearLikelihood=AValue) then exit;
  FheadwearLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetpanAngle(AIndex : Integer; const AValue : integer); 

begin
  If (FpanAngle=AValue) then exit;
  FpanAngle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetboundingPoly(AIndex : Integer; const AValue : TBoundingPoly); 

begin
  If (FboundingPoly=AValue) then exit;
  FboundingPoly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.Setlandmarks(AIndex : Integer; const AValue : TFaceAnnotationTypelandmarksArray); 

begin
  If (Flandmarks=AValue) then exit;
  Flandmarks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetblurredLikelihood(AIndex : Integer; const AValue : String); 

begin
  If (FblurredLikelihood=AValue) then exit;
  FblurredLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetrollAngle(AIndex : Integer; const AValue : integer); 

begin
  If (FrollAngle=AValue) then exit;
  FrollAngle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFaceAnnotation.SetsorrowLikelihood(AIndex : Integer; const AValue : String); 

begin
  If (FsorrowLikelihood=AValue) then exit;
  FsorrowLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFaceAnnotation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'landmarks' : SetLength(Flandmarks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVertex
  --------------------------------------------------------------------}


Procedure TVertex.Sety(AIndex : Integer; const AValue : integer); 

begin
  If (Fy=AValue) then exit;
  Fy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVertex.Setx(AIndex : Integer; const AValue : integer); 

begin
  If (Fx=AValue) then exit;
  Fx:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColorInfo
  --------------------------------------------------------------------}


Procedure TColorInfo.SetpixelFraction(AIndex : Integer; const AValue : integer); 

begin
  If (FpixelFraction=AValue) then exit;
  FpixelFraction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColorInfo.Setcolor(AIndex : Integer; const AValue : TColor); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColorInfo.Setscore(AIndex : Integer; const AValue : integer); 

begin
  If (Fscore=AValue) then exit;
  Fscore:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBoundingPoly
  --------------------------------------------------------------------}


Procedure TBoundingPoly.Setvertices(AIndex : Integer; const AValue : TBoundingPolyTypeverticesArray); 

begin
  If (Fvertices=AValue) then exit;
  Fvertices:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBoundingPoly.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'vertices' : SetLength(Fvertices,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLandmark
  --------------------------------------------------------------------}


Procedure TLandmark.Setposition(AIndex : Integer; const AValue : TPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLandmark.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TLandmark.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TImageContext
  --------------------------------------------------------------------}


Procedure TImageContext.SetlatLongRect(AIndex : Integer; const AValue : TLatLongRect); 

begin
  If (FlatLongRect=AValue) then exit;
  FlatLongRect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageContext.SetlanguageHints(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FlanguageHints=AValue) then exit;
  FlanguageHints:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TImageContext.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'languagehints' : SetLength(FlanguageHints,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBatchAnnotateImagesRequest
  --------------------------------------------------------------------}


Procedure TBatchAnnotateImagesRequest.Setrequests(AIndex : Integer; const AValue : TBatchAnnotateImagesRequestTyperequestsArray); 

begin
  If (Frequests=AValue) then exit;
  Frequests:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchAnnotateImagesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'requests' : SetLength(Frequests,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEntityAnnotation
  --------------------------------------------------------------------}


Procedure TEntityAnnotation.Setmid(AIndex : Integer; const AValue : String); 

begin
  If (Fmid=AValue) then exit;
  Fmid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAnnotation.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAnnotation.Settopicality(AIndex : Integer; const AValue : integer); 

begin
  If (Ftopicality=AValue) then exit;
  Ftopicality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAnnotation.Setlocale(AIndex : Integer; const AValue : String); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAnnotation.Setproperties(AIndex : Integer; const AValue : TEntityAnnotationTypepropertiesArray); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAnnotation.Setscore(AIndex : Integer; const AValue : integer); 

begin
  If (Fscore=AValue) then exit;
  Fscore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAnnotation.SetboundingPoly(AIndex : Integer; const AValue : TBoundingPoly); 

begin
  If (FboundingPoly=AValue) then exit;
  FboundingPoly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAnnotation.Setlocations(AIndex : Integer; const AValue : TEntityAnnotationTypelocationsArray); 

begin
  If (Flocations=AValue) then exit;
  Flocations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAnnotation.Setconfidence(AIndex : Integer; const AValue : integer); 

begin
  If (Fconfidence=AValue) then exit;
  Fconfidence:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEntityAnnotation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'properties' : SetLength(Fproperties,ALength);
  'locations' : SetLength(Flocations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProperty
  --------------------------------------------------------------------}


Procedure TProperty.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColor
  --------------------------------------------------------------------}


Procedure TColor.Setgreen(AIndex : Integer; const AValue : integer); 

begin
  If (Fgreen=AValue) then exit;
  Fgreen:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColor.Setblue(AIndex : Integer; const AValue : integer); 

begin
  If (Fblue=AValue) then exit;
  Fblue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColor.Setred(AIndex : Integer; const AValue : integer); 

begin
  If (Fred=AValue) then exit;
  Fred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColor.Setalpha(AIndex : Integer; const AValue : integer); 

begin
  If (Falpha=AValue) then exit;
  Falpha:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocationInfo
  --------------------------------------------------------------------}


Procedure TLocationInfo.SetlatLng(AIndex : Integer; const AValue : TLatLng); 

begin
  If (FlatLng=AValue) then exit;
  FlatLng:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSafeSearchAnnotation
  --------------------------------------------------------------------}


Procedure TSafeSearchAnnotation.Setmedical(AIndex : Integer; const AValue : String); 

begin
  If (Fmedical=AValue) then exit;
  Fmedical:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSafeSearchAnnotation.Setspoof(AIndex : Integer; const AValue : String); 

begin
  If (Fspoof=AValue) then exit;
  Fspoof:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSafeSearchAnnotation.Setviolence(AIndex : Integer; const AValue : String); 

begin
  If (Fviolence=AValue) then exit;
  Fviolence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSafeSearchAnnotation.Setadult(AIndex : Integer; const AValue : String); 

begin
  If (Fadult=AValue) then exit;
  Fadult:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImage
  --------------------------------------------------------------------}


Procedure TImage.Setsource(AIndex : Integer; const AValue : TImageSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setcontent(AIndex : Integer; const AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDominantColorsAnnotation
  --------------------------------------------------------------------}


Procedure TDominantColorsAnnotation.Setcolors(AIndex : Integer; const AValue : TDominantColorsAnnotationTypecolorsArray); 

begin
  If (Fcolors=AValue) then exit;
  Fcolors:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDominantColorsAnnotation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'colors' : SetLength(Fcolors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFeature
  --------------------------------------------------------------------}


Procedure TFeature.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFeature.SetmaxResults(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxResults=AValue) then exit;
  FmaxResults:=AValue;
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
  TBatchAnnotateImagesResponse
  --------------------------------------------------------------------}


Procedure TBatchAnnotateImagesResponse.Setresponses(AIndex : Integer; const AValue : TBatchAnnotateImagesResponseTyperesponsesArray); 

begin
  If (Fresponses=AValue) then exit;
  Fresponses:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchAnnotateImagesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'responses' : SetLength(Fresponses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TImageProperties
  --------------------------------------------------------------------}


Procedure TImageProperties.SetdominantColors(AIndex : Integer; const AValue : TDominantColorsAnnotation); 

begin
  If (FdominantColors=AValue) then exit;
  FdominantColors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLatLng
  --------------------------------------------------------------------}


Procedure TLatLng.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLatLng.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPosition
  --------------------------------------------------------------------}


Procedure TPosition.Sety(AIndex : Integer; const AValue : integer); 

begin
  If (Fy=AValue) then exit;
  Fy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.Setx(AIndex : Integer; const AValue : integer); 

begin
  If (Fx=AValue) then exit;
  Fx:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.Setz(AIndex : Integer; const AValue : integer); 

begin
  If (Fz=AValue) then exit;
  Fz:=AValue;
  MarkPropertyChanged(AIndex);
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
  Result:=TvisionAPI;
end;

Function TImagesResource.Annotate(aBatchAnnotateImagesRequest : TBatchAnnotateImagesRequest) : TBatchAnnotateImagesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/images:annotate';
  _Methodid   = 'vision.images.annotate';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aBatchAnnotateImagesRequest,TBatchAnnotateImagesResponse) as TBatchAnnotateImagesResponse;
end;



{ --------------------------------------------------------------------
  TVisionAPI
  --------------------------------------------------------------------}

Class Function TVisionAPI.APIName : String;

begin
  Result:='vision';
end;

Class Function TVisionAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TVisionAPI.APIRevision : String;

begin
  Result:='20160519';
end;

Class Function TVisionAPI.APIID : String;

begin
  Result:='vision:v1';
end;

Class Function TVisionAPI.APITitle : String;

begin
  Result:='Google Cloud Vision API';
end;

Class Function TVisionAPI.APIDescription : String;

begin
  Result:='Integrates Google Vision features, including image labeling, face, logo, and landmark detection, optical character recognition (OCR), and detection of explicit content, into applications.';
end;

Class Function TVisionAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TVisionAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TVisionAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TVisionAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TVisionAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/vision/';
end;

Class Function TVisionAPI.APIrootUrl : string;

begin
  Result:='https://vision.googleapis.com/';
end;

Class Function TVisionAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TVisionAPI.APIbaseURL : String;

begin
  Result:='https://vision.googleapis.com/';
end;

Class Function TVisionAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TVisionAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TVisionAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TVisionAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TVisionAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TVisionAPI.RegisterAPIResources;

begin
  TImageSource.RegisterObject;
  TAnnotateImageRequest.RegisterObject;
  TAnnotateImageResponse.RegisterObject;
  TLatLongRect.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TFaceAnnotation.RegisterObject;
  TVertex.RegisterObject;
  TColorInfo.RegisterObject;
  TBoundingPoly.RegisterObject;
  TLandmark.RegisterObject;
  TImageContext.RegisterObject;
  TBatchAnnotateImagesRequest.RegisterObject;
  TEntityAnnotation.RegisterObject;
  TProperty.RegisterObject;
  TColor.RegisterObject;
  TLocationInfo.RegisterObject;
  TSafeSearchAnnotation.RegisterObject;
  TImage.RegisterObject;
  TDominantColorsAnnotation.RegisterObject;
  TFeature.RegisterObject;
  TBatchAnnotateImagesResponse.RegisterObject;
  TImageProperties.RegisterObject;
  TLatLng.RegisterObject;
  TPosition.RegisterObject;
end;


Function TVisionAPI.GetImagesInstance : TImagesResource;

begin
  if (FImagesInstance=Nil) then
    FImagesInstance:=CreateImagesResource;
  Result:=FImagesInstance;
end;

Function TVisionAPI.CreateImagesResource : TImagesResource;

begin
  Result:=CreateImagesResource(Self);
end;


Function TVisionAPI.CreateImagesResource(AOwner : TComponent) : TImagesResource;

begin
  Result:=TImagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TVisionAPI.RegisterAPI;
end.
