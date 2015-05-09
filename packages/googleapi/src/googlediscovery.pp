unit googlediscovery;
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
  TDirectoryList = class;
  TJsonSchema = class;
  TRestDescription = class;
  TRestMethod = class;
  TRestResource = class;
  TDirectoryListArray = Array of TDirectoryList;
  TJsonSchemaArray = Array of TJsonSchema;
  TRestDescriptionArray = Array of TRestDescription;
  TRestMethodArray = Array of TRestMethod;
  TRestResourceArray = Array of TRestResource;
  //Anonymous types, using auto-generated names
  TDirectoryListTypeitemsItemTypeicons = class;
  TDirectoryListTypeitemsItem = class;
  TJsonSchemaTypeannotations = class;
  TJsonSchemaTypeproperties = class;
  TJsonSchemaTypevariantTypemapItem = class;
  TJsonSchemaTypevariant = class;
  TRestDescriptionTypeauthTypeoauth2Typescopes = class;
  TRestDescriptionTypeauthTypeoauth2 = class;
  TRestDescriptionTypeauth = class;
  TRestDescriptionTypeicons = class;
  TRestDescriptionTypemethods = class;
  TRestDescriptionTypeparameters = class;
  TRestDescriptionTyperesources = class;
  TRestDescriptionTypeschemas = class;
  TRestMethodTypemediaUploadTypeprotocolsTyperesumable = class;
  TRestMethodTypemediaUploadTypeprotocolsTypesimple = class;
  TRestMethodTypemediaUploadTypeprotocols = class;
  TRestMethodTypemediaUpload = class;
  TRestMethodTypeparameters = class;
  TRestMethodTyperequest = class;
  TRestMethodTyperesponse = class;
  TRestResourceTypemethods = class;
  TRestResourceTyperesources = class;
  TDirectoryListTypeitemsArray = Array of TDirectoryListTypeitemsItem;
  TJsonSchemaTypevariantTypemapArray = Array of TJsonSchemaTypevariantTypemapItem;
  
  { --------------------------------------------------------------------
    TDirectoryListTypeitemsItemTypeicons
    --------------------------------------------------------------------}
  
  TDirectoryListTypeitemsItemTypeicons = Class(TGoogleBaseObject)
  Private
    Fx16 : String;
    Fx32 : String;
  Protected
    //Property setters
    Procedure Setx16(AIndex : Integer; AValue : String); virtual;
    Procedure Setx32(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property x16 : String Index 0 Read Fx16 Write Setx16;
    Property x32 : String Index 8 Read Fx32 Write Setx32;
  end;
  TDirectoryListTypeitemsItemTypeiconsClass = Class of TDirectoryListTypeitemsItemTypeicons;
  
  { --------------------------------------------------------------------
    TDirectoryListTypeitemsItem
    --------------------------------------------------------------------}
  
  TDirectoryListTypeitemsItem = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    FdiscoveryLink : String;
    FdiscoveryRestUrl : String;
    FdocumentationLink : String;
    Ficons : TDirectoryListTypeitemsItemTypeicons;
    Fid : String;
    Fkind : String;
    Flabels : TStringArray;
    Fname : String;
    Fpreferred : boolean;
    Ftitle : String;
    Fversion : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdiscoveryLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetdiscoveryRestUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetdocumentationLink(AIndex : Integer; AValue : String); virtual;
    Procedure Seticons(AIndex : Integer; AValue : TDirectoryListTypeitemsItemTypeicons); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setpreferred(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property discoveryLink : String Index 8 Read FdiscoveryLink Write SetdiscoveryLink;
    Property discoveryRestUrl : String Index 16 Read FdiscoveryRestUrl Write SetdiscoveryRestUrl;
    Property documentationLink : String Index 24 Read FdocumentationLink Write SetdocumentationLink;
    Property icons : TDirectoryListTypeitemsItemTypeicons Index 32 Read Ficons Write Seticons;
    Property id : String Index 40 Read Fid Write Setid;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property labels : TStringArray Index 56 Read Flabels Write Setlabels;
    Property name : String Index 64 Read Fname Write Setname;
    Property preferred : boolean Index 72 Read Fpreferred Write Setpreferred;
    Property title : String Index 80 Read Ftitle Write Settitle;
    Property version : String Index 88 Read Fversion Write Setversion;
  end;
  TDirectoryListTypeitemsItemClass = Class of TDirectoryListTypeitemsItem;
  
  { --------------------------------------------------------------------
    TDirectoryList
    --------------------------------------------------------------------}
  
  TDirectoryList = Class(TGoogleBaseObject)
  Private
    FdiscoveryVersion : String;
    Fitems : TDirectoryListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetdiscoveryVersion(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TDirectoryListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property discoveryVersion : String Index 0 Read FdiscoveryVersion Write SetdiscoveryVersion;
    Property items : TDirectoryListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TDirectoryListClass = Class of TDirectoryList;
  
  { --------------------------------------------------------------------
    TJsonSchemaTypeannotations
    --------------------------------------------------------------------}
  
  TJsonSchemaTypeannotations = Class(TGoogleBaseObject)
  Private
    Frequired : TStringArray;
  Protected
    //Property setters
    Procedure Setrequired(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property required : TStringArray Index 0 Read Frequired Write Setrequired;
  end;
  TJsonSchemaTypeannotationsClass = Class of TJsonSchemaTypeannotations;
  
  { --------------------------------------------------------------------
    TJsonSchemaTypeproperties
    --------------------------------------------------------------------}
  
  TJsonSchemaTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TJsonSchemaTypepropertiesClass = Class of TJsonSchemaTypeproperties;
  
  { --------------------------------------------------------------------
    TJsonSchemaTypevariantTypemapItem
    --------------------------------------------------------------------}
  
  TJsonSchemaTypevariantTypemapItem = Class(TGoogleBaseObject)
  Private
    Fref : String;
    Ftype_value : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setref(AIndex : Integer; AValue : String); virtual;
    Procedure Settype_value(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ref : String Index 0 Read Fref Write Setref;
    Property type_value : String Index 8 Read Ftype_value Write Settype_value;
  end;
  TJsonSchemaTypevariantTypemapItemClass = Class of TJsonSchemaTypevariantTypemapItem;
  
  { --------------------------------------------------------------------
    TJsonSchemaTypevariant
    --------------------------------------------------------------------}
  
  TJsonSchemaTypevariant = Class(TGoogleBaseObject)
  Private
    Fdiscriminant : String;
    Fmap : TJsonSchemaTypevariantTypemapArray;
  Protected
    //Property setters
    Procedure Setdiscriminant(AIndex : Integer; AValue : String); virtual;
    Procedure Setmap(AIndex : Integer; AValue : TJsonSchemaTypevariantTypemapArray); virtual;
  Public
  Published
    Property discriminant : String Index 0 Read Fdiscriminant Write Setdiscriminant;
    Property map : TJsonSchemaTypevariantTypemapArray Index 8 Read Fmap Write Setmap;
  end;
  TJsonSchemaTypevariantClass = Class of TJsonSchemaTypevariant;
  
  { --------------------------------------------------------------------
    TJsonSchema
    --------------------------------------------------------------------}
  
  TJsonSchema = Class(TGoogleBaseObject)
  Private
    Fref : String;
    FadditionalProperties : TJsonSchema;
    Fannotations : TJsonSchemaTypeannotations;
    Fdefault : String;
    Fdescription : String;
    Fenum : TStringArray;
    FenumDescriptions : TStringArray;
    Fformat : String;
    Fid : String;
    Fitems : TJsonSchema;
    Flocation : String;
    Fmaximum : String;
    Fminimum : String;
    Fpattern : String;
    Fproperties : TJsonSchemaTypeproperties;
    FreadOnly : boolean;
    Frepeated : boolean;
    Frequired : boolean;
    F_type : String;
    Fvariant : TJsonSchemaTypevariant;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setref(AIndex : Integer; AValue : String); virtual;
    Procedure SetadditionalProperties(AIndex : Integer; AValue : TJsonSchema); virtual;
    Procedure Setannotations(AIndex : Integer; AValue : TJsonSchemaTypeannotations); virtual;
    Procedure Setdefault(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setenum(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetenumDescriptions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TJsonSchema); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setmaximum(AIndex : Integer; AValue : String); virtual;
    Procedure Setminimum(AIndex : Integer; AValue : String); virtual;
    Procedure Setpattern(AIndex : Integer; AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TJsonSchemaTypeproperties); virtual;
    Procedure SetreadOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setrepeated(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setrequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : TJsonSchemaTypevariant); virtual;
  Public
  Published
    Property ref : String Index 0 Read Fref Write Setref;
    Property additionalProperties : TJsonSchema Index 8 Read FadditionalProperties Write SetadditionalProperties;
    Property annotations : TJsonSchemaTypeannotations Index 16 Read Fannotations Write Setannotations;
    Property default : String Index 24 Read Fdefault Write Setdefault;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property enum : TStringArray Index 40 Read Fenum Write Setenum;
    Property enumDescriptions : TStringArray Index 48 Read FenumDescriptions Write SetenumDescriptions;
    Property format : String Index 56 Read Fformat Write Setformat;
    Property id : String Index 64 Read Fid Write Setid;
    Property items : TJsonSchema Index 72 Read Fitems Write Setitems;
    Property location : String Index 80 Read Flocation Write Setlocation;
    Property maximum : String Index 88 Read Fmaximum Write Setmaximum;
    Property minimum : String Index 96 Read Fminimum Write Setminimum;
    Property pattern : String Index 104 Read Fpattern Write Setpattern;
    Property properties : TJsonSchemaTypeproperties Index 112 Read Fproperties Write Setproperties;
    Property readOnly : boolean Index 120 Read FreadOnly Write SetreadOnly;
    Property repeated : boolean Index 128 Read Frepeated Write Setrepeated;
    Property required : boolean Index 136 Read Frequired Write Setrequired;
    Property _type : String Index 144 Read F_type Write Set_type;
    Property variant : TJsonSchemaTypevariant Index 152 Read Fvariant Write Setvariant;
  end;
  TJsonSchemaClass = Class of TJsonSchema;
  
  { --------------------------------------------------------------------
    TRestDescriptionTypeauthTypeoauth2Typescopes
    --------------------------------------------------------------------}
  
  TRestDescriptionTypeauthTypeoauth2Typescopes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionTypeauthTypeoauth2TypescopesClass = Class of TRestDescriptionTypeauthTypeoauth2Typescopes;
  
  { --------------------------------------------------------------------
    TRestDescriptionTypeauthTypeoauth2
    --------------------------------------------------------------------}
  
  TRestDescriptionTypeauthTypeoauth2 = Class(TGoogleBaseObject)
  Private
    Fscopes : TRestDescriptionTypeauthTypeoauth2Typescopes;
  Protected
    //Property setters
    Procedure Setscopes(AIndex : Integer; AValue : TRestDescriptionTypeauthTypeoauth2Typescopes); virtual;
  Public
  Published
    Property scopes : TRestDescriptionTypeauthTypeoauth2Typescopes Index 0 Read Fscopes Write Setscopes;
  end;
  TRestDescriptionTypeauthTypeoauth2Class = Class of TRestDescriptionTypeauthTypeoauth2;
  
  { --------------------------------------------------------------------
    TRestDescriptionTypeauth
    --------------------------------------------------------------------}
  
  TRestDescriptionTypeauth = Class(TGoogleBaseObject)
  Private
    Foauth2 : TRestDescriptionTypeauthTypeoauth2;
  Protected
    //Property setters
    Procedure Setoauth2(AIndex : Integer; AValue : TRestDescriptionTypeauthTypeoauth2); virtual;
  Public
  Published
    Property oauth2 : TRestDescriptionTypeauthTypeoauth2 Index 0 Read Foauth2 Write Setoauth2;
  end;
  TRestDescriptionTypeauthClass = Class of TRestDescriptionTypeauth;
  
  { --------------------------------------------------------------------
    TRestDescriptionTypeicons
    --------------------------------------------------------------------}
  
  TRestDescriptionTypeicons = Class(TGoogleBaseObject)
  Private
    Fx16 : String;
    Fx32 : String;
  Protected
    //Property setters
    Procedure Setx16(AIndex : Integer; AValue : String); virtual;
    Procedure Setx32(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property x16 : String Index 0 Read Fx16 Write Setx16;
    Property x32 : String Index 8 Read Fx32 Write Setx32;
  end;
  TRestDescriptionTypeiconsClass = Class of TRestDescriptionTypeicons;
  
  { --------------------------------------------------------------------
    TRestDescriptionTypemethods
    --------------------------------------------------------------------}
  
  TRestDescriptionTypemethods = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionTypemethodsClass = Class of TRestDescriptionTypemethods;
  
  { --------------------------------------------------------------------
    TRestDescriptionTypeparameters
    --------------------------------------------------------------------}
  
  TRestDescriptionTypeparameters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionTypeparametersClass = Class of TRestDescriptionTypeparameters;
  
  { --------------------------------------------------------------------
    TRestDescriptionTyperesources
    --------------------------------------------------------------------}
  
  TRestDescriptionTyperesources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionTyperesourcesClass = Class of TRestDescriptionTyperesources;
  
  { --------------------------------------------------------------------
    TRestDescriptionTypeschemas
    --------------------------------------------------------------------}
  
  TRestDescriptionTypeschemas = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionTypeschemasClass = Class of TRestDescriptionTypeschemas;
  
  { --------------------------------------------------------------------
    TRestDescription
    --------------------------------------------------------------------}
  
  TRestDescription = Class(TGoogleBaseObject)
  Private
    Fauth : TRestDescriptionTypeauth;
    FbasePath : String;
    FbaseUrl : String;
    FbatchPath : String;
    FcanonicalName : String;
    Fdescription : String;
    FdiscoveryVersion : String;
    FdocumentationLink : String;
    Fetag : String;
    Ffeatures : TStringArray;
    Ficons : TRestDescriptionTypeicons;
    Fid : String;
    Fkind : String;
    Flabels : TStringArray;
    Fmethods : TRestDescriptionTypemethods;
    Fname : String;
    FownerDomain : String;
    FownerName : String;
    FpackagePath : String;
    Fparameters : TRestDescriptionTypeparameters;
    Fprotocol : String;
    Fresources : TRestDescriptionTyperesources;
    Frevision : String;
    FrootUrl : String;
    Fschemas : TRestDescriptionTypeschemas;
    FservicePath : String;
    Ftitle : String;
    Fversion : String;
  Protected
    //Property setters
    Procedure Setauth(AIndex : Integer; AValue : TRestDescriptionTypeauth); virtual;
    Procedure SetbasePath(AIndex : Integer; AValue : String); virtual;
    Procedure SetbaseUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetbatchPath(AIndex : Integer; AValue : String); virtual;
    Procedure SetcanonicalName(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdiscoveryVersion(AIndex : Integer; AValue : String); virtual;
    Procedure SetdocumentationLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setfeatures(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Seticons(AIndex : Integer; AValue : TRestDescriptionTypeicons); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setmethods(AIndex : Integer; AValue : TRestDescriptionTypemethods); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetownerDomain(AIndex : Integer; AValue : String); virtual;
    Procedure SetownerName(AIndex : Integer; AValue : String); virtual;
    Procedure SetpackagePath(AIndex : Integer; AValue : String); virtual;
    Procedure Setparameters(AIndex : Integer; AValue : TRestDescriptionTypeparameters); virtual;
    Procedure Setprotocol(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TRestDescriptionTyperesources); virtual;
    Procedure Setrevision(AIndex : Integer; AValue : String); virtual;
    Procedure SetrootUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setschemas(AIndex : Integer; AValue : TRestDescriptionTypeschemas); virtual;
    Procedure SetservicePath(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property auth : TRestDescriptionTypeauth Index 0 Read Fauth Write Setauth;
    Property basePath : String Index 8 Read FbasePath Write SetbasePath;
    Property baseUrl : String Index 16 Read FbaseUrl Write SetbaseUrl;
    Property batchPath : String Index 24 Read FbatchPath Write SetbatchPath;
    Property canonicalName : String Index 32 Read FcanonicalName Write SetcanonicalName;
    Property description : String Index 40 Read Fdescription Write Setdescription;
    Property discoveryVersion : String Index 48 Read FdiscoveryVersion Write SetdiscoveryVersion;
    Property documentationLink : String Index 56 Read FdocumentationLink Write SetdocumentationLink;
    Property etag : String Index 64 Read Fetag Write Setetag;
    Property features : TStringArray Index 72 Read Ffeatures Write Setfeatures;
    Property icons : TRestDescriptionTypeicons Index 80 Read Ficons Write Seticons;
    Property id : String Index 88 Read Fid Write Setid;
    Property kind : String Index 96 Read Fkind Write Setkind;
    Property labels : TStringArray Index 104 Read Flabels Write Setlabels;
    Property methods : TRestDescriptionTypemethods Index 112 Read Fmethods Write Setmethods;
    Property name : String Index 120 Read Fname Write Setname;
    Property ownerDomain : String Index 128 Read FownerDomain Write SetownerDomain;
    Property ownerName : String Index 136 Read FownerName Write SetownerName;
    Property packagePath : String Index 144 Read FpackagePath Write SetpackagePath;
    Property parameters : TRestDescriptionTypeparameters Index 152 Read Fparameters Write Setparameters;
    Property protocol : String Index 160 Read Fprotocol Write Setprotocol;
    Property resources : TRestDescriptionTyperesources Index 168 Read Fresources Write Setresources;
    Property revision : String Index 176 Read Frevision Write Setrevision;
    Property rootUrl : String Index 184 Read FrootUrl Write SetrootUrl;
    Property schemas : TRestDescriptionTypeschemas Index 192 Read Fschemas Write Setschemas;
    Property servicePath : String Index 200 Read FservicePath Write SetservicePath;
    Property title : String Index 208 Read Ftitle Write Settitle;
    Property version : String Index 216 Read Fversion Write Setversion;
  end;
  TRestDescriptionClass = Class of TRestDescription;
  
  { --------------------------------------------------------------------
    TRestMethodTypemediaUploadTypeprotocolsTyperesumable
    --------------------------------------------------------------------}
  
  TRestMethodTypemediaUploadTypeprotocolsTyperesumable = Class(TGoogleBaseObject)
  Private
    Fmultipart : boolean;
    Fpath : String;
  Protected
    //Property setters
    Procedure Setmultipart(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setpath(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property multipart : boolean Index 0 Read Fmultipart Write Setmultipart;
    Property path : String Index 8 Read Fpath Write Setpath;
  end;
  TRestMethodTypemediaUploadTypeprotocolsTyperesumableClass = Class of TRestMethodTypemediaUploadTypeprotocolsTyperesumable;
  
  { --------------------------------------------------------------------
    TRestMethodTypemediaUploadTypeprotocolsTypesimple
    --------------------------------------------------------------------}
  
  TRestMethodTypemediaUploadTypeprotocolsTypesimple = Class(TGoogleBaseObject)
  Private
    Fmultipart : boolean;
    Fpath : String;
  Protected
    //Property setters
    Procedure Setmultipart(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setpath(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property multipart : boolean Index 0 Read Fmultipart Write Setmultipart;
    Property path : String Index 8 Read Fpath Write Setpath;
  end;
  TRestMethodTypemediaUploadTypeprotocolsTypesimpleClass = Class of TRestMethodTypemediaUploadTypeprotocolsTypesimple;
  
  { --------------------------------------------------------------------
    TRestMethodTypemediaUploadTypeprotocols
    --------------------------------------------------------------------}
  
  TRestMethodTypemediaUploadTypeprotocols = Class(TGoogleBaseObject)
  Private
    Fresumable : TRestMethodTypemediaUploadTypeprotocolsTyperesumable;
    Fsimple : TRestMethodTypemediaUploadTypeprotocolsTypesimple;
  Protected
    //Property setters
    Procedure Setresumable(AIndex : Integer; AValue : TRestMethodTypemediaUploadTypeprotocolsTyperesumable); virtual;
    Procedure Setsimple(AIndex : Integer; AValue : TRestMethodTypemediaUploadTypeprotocolsTypesimple); virtual;
  Public
  Published
    Property resumable : TRestMethodTypemediaUploadTypeprotocolsTyperesumable Index 0 Read Fresumable Write Setresumable;
    Property simple : TRestMethodTypemediaUploadTypeprotocolsTypesimple Index 8 Read Fsimple Write Setsimple;
  end;
  TRestMethodTypemediaUploadTypeprotocolsClass = Class of TRestMethodTypemediaUploadTypeprotocols;
  
  { --------------------------------------------------------------------
    TRestMethodTypemediaUpload
    --------------------------------------------------------------------}
  
  TRestMethodTypemediaUpload = Class(TGoogleBaseObject)
  Private
    Faccept : TStringArray;
    FmaxSize : String;
    Fprotocols : TRestMethodTypemediaUploadTypeprotocols;
  Protected
    //Property setters
    Procedure Setaccept(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetmaxSize(AIndex : Integer; AValue : String); virtual;
    Procedure Setprotocols(AIndex : Integer; AValue : TRestMethodTypemediaUploadTypeprotocols); virtual;
  Public
  Published
    Property accept : TStringArray Index 0 Read Faccept Write Setaccept;
    Property maxSize : String Index 8 Read FmaxSize Write SetmaxSize;
    Property protocols : TRestMethodTypemediaUploadTypeprotocols Index 16 Read Fprotocols Write Setprotocols;
  end;
  TRestMethodTypemediaUploadClass = Class of TRestMethodTypemediaUpload;
  
  { --------------------------------------------------------------------
    TRestMethodTypeparameters
    --------------------------------------------------------------------}
  
  TRestMethodTypeparameters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestMethodTypeparametersClass = Class of TRestMethodTypeparameters;
  
  { --------------------------------------------------------------------
    TRestMethodTyperequest
    --------------------------------------------------------------------}
  
  TRestMethodTyperequest = Class(TGoogleBaseObject)
  Private
    Fref : String;
    FparameterName : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setref(AIndex : Integer; AValue : String); virtual;
    Procedure SetparameterName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ref : String Index 0 Read Fref Write Setref;
    Property parameterName : String Index 8 Read FparameterName Write SetparameterName;
  end;
  TRestMethodTyperequestClass = Class of TRestMethodTyperequest;
  
  { --------------------------------------------------------------------
    TRestMethodTyperesponse
    --------------------------------------------------------------------}
  
  TRestMethodTyperesponse = Class(TGoogleBaseObject)
  Private
    Fref : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setref(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ref : String Index 0 Read Fref Write Setref;
  end;
  TRestMethodTyperesponseClass = Class of TRestMethodTyperesponse;
  
  { --------------------------------------------------------------------
    TRestMethod
    --------------------------------------------------------------------}
  
  TRestMethod = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    FetagRequired : boolean;
    FhttpMethod : String;
    Fid : String;
    FmediaUpload : TRestMethodTypemediaUpload;
    FparameterOrder : TStringArray;
    Fparameters : TRestMethodTypeparameters;
    Fpath : String;
    Frequest : TRestMethodTyperequest;
    Fresponse : TRestMethodTyperesponse;
    Fscopes : TStringArray;
    FsupportsMediaDownload : boolean;
    FsupportsMediaUpload : boolean;
    FsupportsSubscription : boolean;
    FuseMediaDownloadService : boolean;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetetagRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SethttpMethod(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetmediaUpload(AIndex : Integer; AValue : TRestMethodTypemediaUpload); virtual;
    Procedure SetparameterOrder(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setparameters(AIndex : Integer; AValue : TRestMethodTypeparameters); virtual;
    Procedure Setpath(AIndex : Integer; AValue : String); virtual;
    Procedure Setrequest(AIndex : Integer; AValue : TRestMethodTyperequest); virtual;
    Procedure Setresponse(AIndex : Integer; AValue : TRestMethodTyperesponse); virtual;
    Procedure Setscopes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsupportsMediaDownload(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportsMediaUpload(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportsSubscription(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetuseMediaDownloadService(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property etagRequired : boolean Index 8 Read FetagRequired Write SetetagRequired;
    Property httpMethod : String Index 16 Read FhttpMethod Write SethttpMethod;
    Property id : String Index 24 Read Fid Write Setid;
    Property mediaUpload : TRestMethodTypemediaUpload Index 32 Read FmediaUpload Write SetmediaUpload;
    Property parameterOrder : TStringArray Index 40 Read FparameterOrder Write SetparameterOrder;
    Property parameters : TRestMethodTypeparameters Index 48 Read Fparameters Write Setparameters;
    Property path : String Index 56 Read Fpath Write Setpath;
    Property request : TRestMethodTyperequest Index 64 Read Frequest Write Setrequest;
    Property response : TRestMethodTyperesponse Index 72 Read Fresponse Write Setresponse;
    Property scopes : TStringArray Index 80 Read Fscopes Write Setscopes;
    Property supportsMediaDownload : boolean Index 88 Read FsupportsMediaDownload Write SetsupportsMediaDownload;
    Property supportsMediaUpload : boolean Index 96 Read FsupportsMediaUpload Write SetsupportsMediaUpload;
    Property supportsSubscription : boolean Index 104 Read FsupportsSubscription Write SetsupportsSubscription;
    Property useMediaDownloadService : boolean Index 112 Read FuseMediaDownloadService Write SetuseMediaDownloadService;
  end;
  TRestMethodClass = Class of TRestMethod;
  
  { --------------------------------------------------------------------
    TRestResourceTypemethods
    --------------------------------------------------------------------}
  
  TRestResourceTypemethods = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestResourceTypemethodsClass = Class of TRestResourceTypemethods;
  
  { --------------------------------------------------------------------
    TRestResourceTyperesources
    --------------------------------------------------------------------}
  
  TRestResourceTyperesources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestResourceTyperesourcesClass = Class of TRestResourceTyperesources;
  
  { --------------------------------------------------------------------
    TRestResource
    --------------------------------------------------------------------}
  
  TRestResource = Class(TGoogleBaseObject)
  Private
    Fmethods : TRestResourceTypemethods;
    Fresources : TRestResourceTyperesources;
  Protected
    //Property setters
    Procedure Setmethods(AIndex : Integer; AValue : TRestResourceTypemethods); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TRestResourceTyperesources); virtual;
  Public
  Published
    Property methods : TRestResourceTypemethods Index 0 Read Fmethods Write Setmethods;
    Property resources : TRestResourceTyperesources Index 8 Read Fresources Write Setresources;
  end;
  TRestResourceClass = Class of TRestResource;
  
  { --------------------------------------------------------------------
    TApisResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TApisResource, method List
  
  TApisListOptions = Record
    _name : String;
    preferred : boolean;
  end;
  
  TApisResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetRest(_api: string; version: string) : TRestDescription;
    Function List(AQuery : string  = '') : TDirectoryList;
    Function List(AQuery : TApislistOptions) : TDirectoryList;
  end;
  
  
  { --------------------------------------------------------------------
    TDiscoveryAPI
    --------------------------------------------------------------------}
  
  TDiscoveryAPI = Class(TGoogleAPI)
  Private
    FApisInstance : TApisResource;
    Function GetApisInstance : TApisResource;virtual;
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
    Function CreateApisResource(AOwner : TComponent) : TApisResource;virtual;overload;
    Function CreateApisResource : TApisResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ApisResource : TApisResource Read GetApisInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TDirectoryListTypeitemsItemTypeicons
  --------------------------------------------------------------------}


Procedure TDirectoryListTypeitemsItemTypeicons.Setx16(AIndex : Integer; AValue : String); 

begin
  If (Fx16=AValue) then exit;
  Fx16:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItemTypeicons.Setx32(AIndex : Integer; AValue : String); 

begin
  If (Fx32=AValue) then exit;
  Fx32:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectoryListTypeitemsItem
  --------------------------------------------------------------------}


Procedure TDirectoryListTypeitemsItem.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.SetdiscoveryLink(AIndex : Integer; AValue : String); 

begin
  If (FdiscoveryLink=AValue) then exit;
  FdiscoveryLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.SetdiscoveryRestUrl(AIndex : Integer; AValue : String); 

begin
  If (FdiscoveryRestUrl=AValue) then exit;
  FdiscoveryRestUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.SetdocumentationLink(AIndex : Integer; AValue : String); 

begin
  If (FdocumentationLink=AValue) then exit;
  FdocumentationLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.Seticons(AIndex : Integer; AValue : TDirectoryListTypeitemsItemTypeicons); 

begin
  If (Ficons=AValue) then exit;
  Ficons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.Setlabels(AIndex : Integer; AValue : TStringArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.Setpreferred(AIndex : Integer; AValue : boolean); 

begin
  If (Fpreferred=AValue) then exit;
  Fpreferred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListTypeitemsItem.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectoryList
  --------------------------------------------------------------------}


Procedure TDirectoryList.SetdiscoveryVersion(AIndex : Integer; AValue : String); 

begin
  If (FdiscoveryVersion=AValue) then exit;
  FdiscoveryVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryList.Setitems(AIndex : Integer; AValue : TDirectoryListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJsonSchemaTypeannotations
  --------------------------------------------------------------------}


Procedure TJsonSchemaTypeannotations.Setrequired(AIndex : Integer; AValue : TStringArray); 

begin
  If (Frequired=AValue) then exit;
  Frequired:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJsonSchemaTypeproperties
  --------------------------------------------------------------------}


Class Function TJsonSchemaTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TJsonSchemaTypevariantTypemapItem
  --------------------------------------------------------------------}


Procedure TJsonSchemaTypevariantTypemapItem.Setref(AIndex : Integer; AValue : String); 

begin
  If (Fref=AValue) then exit;
  Fref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchemaTypevariantTypemapItem.Settype_value(AIndex : Integer; AValue : String); 

begin
  If (Ftype_value=AValue) then exit;
  Ftype_value:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TJsonSchemaTypevariantTypemapItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'ref' : Result:='$ref';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TJsonSchemaTypevariant
  --------------------------------------------------------------------}


Procedure TJsonSchemaTypevariant.Setdiscriminant(AIndex : Integer; AValue : String); 

begin
  If (Fdiscriminant=AValue) then exit;
  Fdiscriminant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchemaTypevariant.Setmap(AIndex : Integer; AValue : TJsonSchemaTypevariantTypemapArray); 

begin
  If (Fmap=AValue) then exit;
  Fmap:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJsonSchema
  --------------------------------------------------------------------}


Procedure TJsonSchema.Setref(AIndex : Integer; AValue : String); 

begin
  If (Fref=AValue) then exit;
  Fref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.SetadditionalProperties(AIndex : Integer; AValue : TJsonSchema); 

begin
  If (FadditionalProperties=AValue) then exit;
  FadditionalProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setannotations(AIndex : Integer; AValue : TJsonSchemaTypeannotations); 

begin
  If (Fannotations=AValue) then exit;
  Fannotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setdefault(AIndex : Integer; AValue : String); 

begin
  If (Fdefault=AValue) then exit;
  Fdefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setenum(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fenum=AValue) then exit;
  Fenum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.SetenumDescriptions(AIndex : Integer; AValue : TStringArray); 

begin
  If (FenumDescriptions=AValue) then exit;
  FenumDescriptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setitems(AIndex : Integer; AValue : TJsonSchema); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setmaximum(AIndex : Integer; AValue : String); 

begin
  If (Fmaximum=AValue) then exit;
  Fmaximum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setminimum(AIndex : Integer; AValue : String); 

begin
  If (Fminimum=AValue) then exit;
  Fminimum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setpattern(AIndex : Integer; AValue : String); 

begin
  If (Fpattern=AValue) then exit;
  Fpattern:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setproperties(AIndex : Integer; AValue : TJsonSchemaTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.SetreadOnly(AIndex : Integer; AValue : boolean); 

begin
  If (FreadOnly=AValue) then exit;
  FreadOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setrepeated(AIndex : Integer; AValue : boolean); 

begin
  If (Frepeated=AValue) then exit;
  Frepeated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setrequired(AIndex : Integer; AValue : boolean); 

begin
  If (Frequired=AValue) then exit;
  Frequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setvariant(AIndex : Integer; AValue : TJsonSchemaTypevariant); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TJsonSchema.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'ref' : Result:='$ref';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRestDescriptionTypeauthTypeoauth2Typescopes
  --------------------------------------------------------------------}


Class Function TRestDescriptionTypeauthTypeoauth2Typescopes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescriptionTypeauthTypeoauth2
  --------------------------------------------------------------------}


Procedure TRestDescriptionTypeauthTypeoauth2.Setscopes(AIndex : Integer; AValue : TRestDescriptionTypeauthTypeoauth2Typescopes); 

begin
  If (Fscopes=AValue) then exit;
  Fscopes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestDescriptionTypeauth
  --------------------------------------------------------------------}


Procedure TRestDescriptionTypeauth.Setoauth2(AIndex : Integer; AValue : TRestDescriptionTypeauthTypeoauth2); 

begin
  If (Foauth2=AValue) then exit;
  Foauth2:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestDescriptionTypeicons
  --------------------------------------------------------------------}


Procedure TRestDescriptionTypeicons.Setx16(AIndex : Integer; AValue : String); 

begin
  If (Fx16=AValue) then exit;
  Fx16:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescriptionTypeicons.Setx32(AIndex : Integer; AValue : String); 

begin
  If (Fx32=AValue) then exit;
  Fx32:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestDescriptionTypemethods
  --------------------------------------------------------------------}


Class Function TRestDescriptionTypemethods.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescriptionTypeparameters
  --------------------------------------------------------------------}


Class Function TRestDescriptionTypeparameters.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescriptionTyperesources
  --------------------------------------------------------------------}


Class Function TRestDescriptionTyperesources.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescriptionTypeschemas
  --------------------------------------------------------------------}


Class Function TRestDescriptionTypeschemas.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescription
  --------------------------------------------------------------------}


Procedure TRestDescription.Setauth(AIndex : Integer; AValue : TRestDescriptionTypeauth); 

begin
  If (Fauth=AValue) then exit;
  Fauth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetbasePath(AIndex : Integer; AValue : String); 

begin
  If (FbasePath=AValue) then exit;
  FbasePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetbaseUrl(AIndex : Integer; AValue : String); 

begin
  If (FbaseUrl=AValue) then exit;
  FbaseUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetbatchPath(AIndex : Integer; AValue : String); 

begin
  If (FbatchPath=AValue) then exit;
  FbatchPath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetcanonicalName(AIndex : Integer; AValue : String); 

begin
  If (FcanonicalName=AValue) then exit;
  FcanonicalName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetdiscoveryVersion(AIndex : Integer; AValue : String); 

begin
  If (FdiscoveryVersion=AValue) then exit;
  FdiscoveryVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetdocumentationLink(AIndex : Integer; AValue : String); 

begin
  If (FdocumentationLink=AValue) then exit;
  FdocumentationLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setfeatures(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Seticons(AIndex : Integer; AValue : TRestDescriptionTypeicons); 

begin
  If (Ficons=AValue) then exit;
  Ficons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setlabels(AIndex : Integer; AValue : TStringArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setmethods(AIndex : Integer; AValue : TRestDescriptionTypemethods); 

begin
  If (Fmethods=AValue) then exit;
  Fmethods:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetownerDomain(AIndex : Integer; AValue : String); 

begin
  If (FownerDomain=AValue) then exit;
  FownerDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetownerName(AIndex : Integer; AValue : String); 

begin
  If (FownerName=AValue) then exit;
  FownerName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetpackagePath(AIndex : Integer; AValue : String); 

begin
  If (FpackagePath=AValue) then exit;
  FpackagePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setparameters(AIndex : Integer; AValue : TRestDescriptionTypeparameters); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setprotocol(AIndex : Integer; AValue : String); 

begin
  If (Fprotocol=AValue) then exit;
  Fprotocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setresources(AIndex : Integer; AValue : TRestDescriptionTyperesources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setrevision(AIndex : Integer; AValue : String); 

begin
  If (Frevision=AValue) then exit;
  Frevision:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetrootUrl(AIndex : Integer; AValue : String); 

begin
  If (FrootUrl=AValue) then exit;
  FrootUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setschemas(AIndex : Integer; AValue : TRestDescriptionTypeschemas); 

begin
  If (Fschemas=AValue) then exit;
  Fschemas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetservicePath(AIndex : Integer; AValue : String); 

begin
  If (FservicePath=AValue) then exit;
  FservicePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodTypemediaUploadTypeprotocolsTyperesumable
  --------------------------------------------------------------------}


Procedure TRestMethodTypemediaUploadTypeprotocolsTyperesumable.Setmultipart(AIndex : Integer; AValue : boolean); 

begin
  If (Fmultipart=AValue) then exit;
  Fmultipart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodTypemediaUploadTypeprotocolsTyperesumable.Setpath(AIndex : Integer; AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodTypemediaUploadTypeprotocolsTypesimple
  --------------------------------------------------------------------}


Procedure TRestMethodTypemediaUploadTypeprotocolsTypesimple.Setmultipart(AIndex : Integer; AValue : boolean); 

begin
  If (Fmultipart=AValue) then exit;
  Fmultipart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodTypemediaUploadTypeprotocolsTypesimple.Setpath(AIndex : Integer; AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodTypemediaUploadTypeprotocols
  --------------------------------------------------------------------}


Procedure TRestMethodTypemediaUploadTypeprotocols.Setresumable(AIndex : Integer; AValue : TRestMethodTypemediaUploadTypeprotocolsTyperesumable); 

begin
  If (Fresumable=AValue) then exit;
  Fresumable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodTypemediaUploadTypeprotocols.Setsimple(AIndex : Integer; AValue : TRestMethodTypemediaUploadTypeprotocolsTypesimple); 

begin
  If (Fsimple=AValue) then exit;
  Fsimple:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodTypemediaUpload
  --------------------------------------------------------------------}


Procedure TRestMethodTypemediaUpload.Setaccept(AIndex : Integer; AValue : TStringArray); 

begin
  If (Faccept=AValue) then exit;
  Faccept:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodTypemediaUpload.SetmaxSize(AIndex : Integer; AValue : String); 

begin
  If (FmaxSize=AValue) then exit;
  FmaxSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodTypemediaUpload.Setprotocols(AIndex : Integer; AValue : TRestMethodTypemediaUploadTypeprotocols); 

begin
  If (Fprotocols=AValue) then exit;
  Fprotocols:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodTypeparameters
  --------------------------------------------------------------------}


Class Function TRestMethodTypeparameters.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestMethodTyperequest
  --------------------------------------------------------------------}


Procedure TRestMethodTyperequest.Setref(AIndex : Integer; AValue : String); 

begin
  If (Fref=AValue) then exit;
  Fref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodTyperequest.SetparameterName(AIndex : Integer; AValue : String); 

begin
  If (FparameterName=AValue) then exit;
  FparameterName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRestMethodTyperequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'ref' : Result:='$ref';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRestMethodTyperesponse
  --------------------------------------------------------------------}


Procedure TRestMethodTyperesponse.Setref(AIndex : Integer; AValue : String); 

begin
  If (Fref=AValue) then exit;
  Fref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRestMethodTyperesponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'ref' : Result:='$ref';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRestMethod
  --------------------------------------------------------------------}


Procedure TRestMethod.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetetagRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FetagRequired=AValue) then exit;
  FetagRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SethttpMethod(AIndex : Integer; AValue : String); 

begin
  If (FhttpMethod=AValue) then exit;
  FhttpMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetmediaUpload(AIndex : Integer; AValue : TRestMethodTypemediaUpload); 

begin
  If (FmediaUpload=AValue) then exit;
  FmediaUpload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetparameterOrder(AIndex : Integer; AValue : TStringArray); 

begin
  If (FparameterOrder=AValue) then exit;
  FparameterOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setparameters(AIndex : Integer; AValue : TRestMethodTypeparameters); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setpath(AIndex : Integer; AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setrequest(AIndex : Integer; AValue : TRestMethodTyperequest); 

begin
  If (Frequest=AValue) then exit;
  Frequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setresponse(AIndex : Integer; AValue : TRestMethodTyperesponse); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setscopes(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fscopes=AValue) then exit;
  Fscopes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetsupportsMediaDownload(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportsMediaDownload=AValue) then exit;
  FsupportsMediaDownload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetsupportsMediaUpload(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportsMediaUpload=AValue) then exit;
  FsupportsMediaUpload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetsupportsSubscription(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportsSubscription=AValue) then exit;
  FsupportsSubscription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetuseMediaDownloadService(AIndex : Integer; AValue : boolean); 

begin
  If (FuseMediaDownloadService=AValue) then exit;
  FuseMediaDownloadService:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestResourceTypemethods
  --------------------------------------------------------------------}


Class Function TRestResourceTypemethods.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestResourceTyperesources
  --------------------------------------------------------------------}


Class Function TRestResourceTyperesources.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestResource
  --------------------------------------------------------------------}


Procedure TRestResource.Setmethods(AIndex : Integer; AValue : TRestResourceTypemethods); 

begin
  If (Fmethods=AValue) then exit;
  Fmethods:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestResource.Setresources(AIndex : Integer; AValue : TRestResourceTyperesources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApisResource
  --------------------------------------------------------------------}


Class Function TApisResource.ResourceName : String;

begin
  Result:='apis';
end;

Class Function TApisResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdiscoveryAPI;
end;

Function TApisResource.GetRest(_api: string; version: string) : TRestDescription;

Const
  _HTTPMethod = 'GET';
  _Path       = 'apis/{api}/{version}/rest';
  _Methodid   = 'discovery.apis.getRest';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['api',_api,'version',version]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRestDescription) as TRestDescription;
end;

Function TApisResource.List(AQuery : string = '') : TDirectoryList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'apis';
  _Methodid   = 'discovery.apis.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TDirectoryList) as TDirectoryList;
end;


Function TApisResource.List(AQuery : TApislistOptions) : TDirectoryList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'name',AQuery._name);
  AddToQuery(_Q,'preferred',AQuery.preferred);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TDiscoveryAPI
  --------------------------------------------------------------------}

Class Function TDiscoveryAPI.APIName : String;

begin
  Result:='discovery';
end;

Class Function TDiscoveryAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TDiscoveryAPI.APIRevision : String;

begin
  Result:='';
end;

Class Function TDiscoveryAPI.APIID : String;

begin
  Result:='discovery:v1';
end;

Class Function TDiscoveryAPI.APITitle : String;

begin
  Result:='APIs Discovery Service';
end;

Class Function TDiscoveryAPI.APIDescription : String;

begin
  Result:='Lets you discover information about other Google APIs, such as what APIs are available, the resource and method details for each API.';
end;

Class Function TDiscoveryAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDiscoveryAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDiscoveryAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/feature/filing_cabinet_search-g16.png';
end;

Class Function TDiscoveryAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/feature/filing_cabinet_search-g32.png';
end;

Class Function TDiscoveryAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/discovery/';
end;

Class Function TDiscoveryAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TDiscoveryAPI.APIbasePath : string;

begin
  Result:='/discovery/v1/';
end;

Class Function TDiscoveryAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/discovery/v1/';
end;

Class Function TDiscoveryAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDiscoveryAPI.APIservicePath : string;

begin
  Result:='discovery/v1/';
end;

Class Function TDiscoveryAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDiscoveryAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TDiscoveryAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TDiscoveryAPI.RegisterAPIResources;

begin
  TDirectoryListTypeitemsItemTypeicons.RegisterObject;
  TDirectoryListTypeitemsItem.RegisterObject;
  TDirectoryList.RegisterObject;
  TJsonSchemaTypeannotations.RegisterObject;
  TJsonSchemaTypeproperties.RegisterObject;
  TJsonSchemaTypevariantTypemapItem.RegisterObject;
  TJsonSchemaTypevariant.RegisterObject;
  TJsonSchema.RegisterObject;
  TRestDescriptionTypeauthTypeoauth2Typescopes.RegisterObject;
  TRestDescriptionTypeauthTypeoauth2.RegisterObject;
  TRestDescriptionTypeauth.RegisterObject;
  TRestDescriptionTypeicons.RegisterObject;
  TRestDescriptionTypemethods.RegisterObject;
  TRestDescriptionTypeparameters.RegisterObject;
  TRestDescriptionTyperesources.RegisterObject;
  TRestDescriptionTypeschemas.RegisterObject;
  TRestDescription.RegisterObject;
  TRestMethodTypemediaUploadTypeprotocolsTyperesumable.RegisterObject;
  TRestMethodTypemediaUploadTypeprotocolsTypesimple.RegisterObject;
  TRestMethodTypemediaUploadTypeprotocols.RegisterObject;
  TRestMethodTypemediaUpload.RegisterObject;
  TRestMethodTypeparameters.RegisterObject;
  TRestMethodTyperequest.RegisterObject;
  TRestMethodTyperesponse.RegisterObject;
  TRestMethod.RegisterObject;
  TRestResourceTypemethods.RegisterObject;
  TRestResourceTyperesources.RegisterObject;
  TRestResource.RegisterObject;
end;


Function TDiscoveryAPI.GetApisInstance : TApisResource;

begin
  if (FApisInstance=Nil) then
    FApisInstance:=CreateApisResource;
  Result:=FApisInstance;
end;

Function TDiscoveryAPI.CreateApisResource : TApisResource;

begin
  Result:=CreateApisResource(Self);
end;


Function TDiscoveryAPI.CreateApisResource(AOwner : TComponent) : TApisResource;

begin
  Result:=TApisResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TDiscoveryAPI.RegisterAPI;
end.
