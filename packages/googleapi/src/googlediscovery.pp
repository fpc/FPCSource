unit googlediscovery;
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
  TDirectoryList = class;
  TDirectoryListArray = Array of TDirectoryList;
  TDirectoryListitems = class;
  TDirectoryListitemsArray = Array of TDirectoryListitems;
  TDirectoryListitemsicons = class;
  TDirectoryListitemsiconsArray = Array of TDirectoryListitemsicons;
  TDirectoryListitemslabels = class;
  TDirectoryListitemslabelsArray = Array of TDirectoryListitemslabels;
  TJsonSchema = class;
  TJsonSchemaArray = Array of TJsonSchema;
  TJsonSchemaannotations = class;
  TJsonSchemaannotationsArray = Array of TJsonSchemaannotations;
  TJsonSchemaannotationsrequired = class;
  TJsonSchemaannotationsrequiredArray = Array of TJsonSchemaannotationsrequired;
  TJsonSchemaenum = class;
  TJsonSchemaenumArray = Array of TJsonSchemaenum;
  TJsonSchemaenumDescriptions = class;
  TJsonSchemaenumDescriptionsArray = Array of TJsonSchemaenumDescriptions;
  TJsonSchemaproperties = class;
  TJsonSchemapropertiesArray = Array of TJsonSchemaproperties;
  TJsonSchemavariant = class;
  TJsonSchemavariantArray = Array of TJsonSchemavariant;
  TJsonSchemavariantmap = class;
  TJsonSchemavariantmapArray = Array of TJsonSchemavariantmap;
  TRestDescription = class;
  TRestDescriptionArray = Array of TRestDescription;
  TRestDescriptionauth = class;
  TRestDescriptionauthArray = Array of TRestDescriptionauth;
  TRestDescriptionauthoauth2 = class;
  TRestDescriptionauthoauth2Array = Array of TRestDescriptionauthoauth2;
  TRestDescriptionauthoauth2scopes = class;
  TRestDescriptionauthoauth2scopesArray = Array of TRestDescriptionauthoauth2scopes;
  TRestDescriptionfeatures = class;
  TRestDescriptionfeaturesArray = Array of TRestDescriptionfeatures;
  TRestDescriptionicons = class;
  TRestDescriptioniconsArray = Array of TRestDescriptionicons;
  TRestDescriptionlabels = class;
  TRestDescriptionlabelsArray = Array of TRestDescriptionlabels;
  TRestDescriptionmethods = class;
  TRestDescriptionmethodsArray = Array of TRestDescriptionmethods;
  TRestDescriptionparameters = class;
  TRestDescriptionparametersArray = Array of TRestDescriptionparameters;
  TRestDescriptionresources = class;
  TRestDescriptionresourcesArray = Array of TRestDescriptionresources;
  TRestDescriptionschemas = class;
  TRestDescriptionschemasArray = Array of TRestDescriptionschemas;
  TRestMethod = class;
  TRestMethodArray = Array of TRestMethod;
  TRestMethodmediaUpload = class;
  TRestMethodmediaUploadArray = Array of TRestMethodmediaUpload;
  TRestMethodmediaUploadaccept = class;
  TRestMethodmediaUploadacceptArray = Array of TRestMethodmediaUploadaccept;
  TRestMethodmediaUploadprotocols = class;
  TRestMethodmediaUploadprotocolsArray = Array of TRestMethodmediaUploadprotocols;
  TRestMethodmediaUploadprotocolsresumable = class;
  TRestMethodmediaUploadprotocolsresumableArray = Array of TRestMethodmediaUploadprotocolsresumable;
  TRestMethodmediaUploadprotocolssimple = class;
  TRestMethodmediaUploadprotocolssimpleArray = Array of TRestMethodmediaUploadprotocolssimple;
  TRestMethodparameterOrder = class;
  TRestMethodparameterOrderArray = Array of TRestMethodparameterOrder;
  TRestMethodparameters = class;
  TRestMethodparametersArray = Array of TRestMethodparameters;
  TRestMethodrequest = class;
  TRestMethodrequestArray = Array of TRestMethodrequest;
  TRestMethodresponse = class;
  TRestMethodresponseArray = Array of TRestMethodresponse;
  TRestMethodscopes = class;
  TRestMethodscopesArray = Array of TRestMethodscopes;
  TRestResource = class;
  TRestResourceArray = Array of TRestResource;
  TRestResourcemethods = class;
  TRestResourcemethodsArray = Array of TRestResourcemethods;
  TRestResourceresources = class;
  TRestResourceresourcesArray = Array of TRestResourceresources;
  
  { --------------------------------------------------------------------
    TDirectoryList
    --------------------------------------------------------------------}
  
  TDirectoryList = Class(TGoogleBaseObject)
  Private
    FdiscoveryVersion : string;
    Fitems : TDirectoryListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetdiscoveryVersion(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TDirectoryListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property discoveryVersion : string Index 0 Read FdiscoveryVersion Write SetdiscoveryVersion;
    Property items : TDirectoryListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TDirectoryListClass = Class of TDirectoryList;
  
  { --------------------------------------------------------------------
    TDirectoryListitems
    --------------------------------------------------------------------}
  
  TDirectoryListitems = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    FdiscoveryLink : string;
    FdiscoveryRestUrl : string;
    FdocumentationLink : string;
    Ficons : TDirectoryListitemsicons;
    Fid : string;
    Fkind : string;
    Flabels : TDirectoryListitemslabels;
    Fname : string;
    Fpreferred : boolean;
    Ftitle : string;
    Fversion : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiscoveryLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiscoveryRestUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetdocumentationLink(AIndex : Integer; AValue : string); virtual;
    Procedure Seticons(AIndex : Integer; AValue : TDirectoryListitemsicons); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TDirectoryListitemslabels); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setpreferred(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property discoveryLink : string Index 8 Read FdiscoveryLink Write SetdiscoveryLink;
    Property discoveryRestUrl : string Index 16 Read FdiscoveryRestUrl Write SetdiscoveryRestUrl;
    Property documentationLink : string Index 24 Read FdocumentationLink Write SetdocumentationLink;
    Property icons : TDirectoryListitemsicons Index 32 Read Ficons Write Seticons;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property labels : TDirectoryListitemslabels Index 56 Read Flabels Write Setlabels;
    Property name : string Index 64 Read Fname Write Setname;
    Property preferred : boolean Index 72 Read Fpreferred Write Setpreferred;
    Property title : string Index 80 Read Ftitle Write Settitle;
    Property version : string Index 88 Read Fversion Write Setversion;
  end;
  TDirectoryListitemsClass = Class of TDirectoryListitems;
  
  { --------------------------------------------------------------------
    TDirectoryListitemsicons
    --------------------------------------------------------------------}
  
  TDirectoryListitemsicons = Class(TGoogleBaseObject)
  Private
    Fx16 : string;
    Fx32 : string;
  Protected
    //Property setters
    Procedure Setx16(AIndex : Integer; AValue : string); virtual;
    Procedure Setx32(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property x16 : string Index 0 Read Fx16 Write Setx16;
    Property x32 : string Index 8 Read Fx32 Write Setx32;
  end;
  TDirectoryListitemsiconsClass = Class of TDirectoryListitemsicons;
  
  { --------------------------------------------------------------------
    TDirectoryListitemslabels
    --------------------------------------------------------------------}
  
  TDirectoryListitemslabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDirectoryListitemslabelsClass = Class of TDirectoryListitemslabels;
  
  { --------------------------------------------------------------------
    TJsonSchema
    --------------------------------------------------------------------}
  
  TJsonSchema = Class(TGoogleBaseObject)
  Private
    Fref : string;
    FadditionalProperties : TJsonSchema;
    Fannotations : TJsonSchemaannotations;
    Fdefault : string;
    Fdescription : string;
    Fenum : TJsonSchemaenum;
    FenumDescriptions : TJsonSchemaenumDescriptions;
    Fformat : string;
    Fid : string;
    Fitems : TJsonSchema;
    Flocation : string;
    Fmaximum : string;
    Fminimum : string;
    Fpattern : string;
    Fproperties : TJsonSchemaproperties;
    FreadOnly : boolean;
    Frepeated : boolean;
    Frequired : boolean;
    F_type : string;
    Fvariant : TJsonSchemavariant;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setref(AIndex : Integer; AValue : string); virtual;
    Procedure SetadditionalProperties(AIndex : Integer; AValue : TJsonSchema); virtual;
    Procedure Setannotations(AIndex : Integer; AValue : TJsonSchemaannotations); virtual;
    Procedure Setdefault(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setenum(AIndex : Integer; AValue : TJsonSchemaenum); virtual;
    Procedure SetenumDescriptions(AIndex : Integer; AValue : TJsonSchemaenumDescriptions); virtual;
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TJsonSchema); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setmaximum(AIndex : Integer; AValue : string); virtual;
    Procedure Setminimum(AIndex : Integer; AValue : string); virtual;
    Procedure Setpattern(AIndex : Integer; AValue : string); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TJsonSchemaproperties); virtual;
    Procedure SetreadOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setrepeated(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setrequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : TJsonSchemavariant); virtual;
  Public
  Published
    Property ref : string Index 0 Read Fref Write Setref;
    Property additionalProperties : TJsonSchema Index 8 Read FadditionalProperties Write SetadditionalProperties;
    Property annotations : TJsonSchemaannotations Index 16 Read Fannotations Write Setannotations;
    Property default : string Index 24 Read Fdefault Write Setdefault;
    Property description : string Index 32 Read Fdescription Write Setdescription;
    Property enum : TJsonSchemaenum Index 40 Read Fenum Write Setenum;
    Property enumDescriptions : TJsonSchemaenumDescriptions Index 48 Read FenumDescriptions Write SetenumDescriptions;
    Property format : string Index 56 Read Fformat Write Setformat;
    Property id : string Index 64 Read Fid Write Setid;
    Property items : TJsonSchema Index 72 Read Fitems Write Setitems;
    Property location : string Index 80 Read Flocation Write Setlocation;
    Property maximum : string Index 88 Read Fmaximum Write Setmaximum;
    Property minimum : string Index 96 Read Fminimum Write Setminimum;
    Property pattern : string Index 104 Read Fpattern Write Setpattern;
    Property properties : TJsonSchemaproperties Index 112 Read Fproperties Write Setproperties;
    Property readOnly : boolean Index 120 Read FreadOnly Write SetreadOnly;
    Property repeated : boolean Index 128 Read Frepeated Write Setrepeated;
    Property required : boolean Index 136 Read Frequired Write Setrequired;
    Property _type : string Index 144 Read F_type Write Set_type;
    Property variant : TJsonSchemavariant Index 152 Read Fvariant Write Setvariant;
  end;
  TJsonSchemaClass = Class of TJsonSchema;
  
  { --------------------------------------------------------------------
    TJsonSchemaannotations
    --------------------------------------------------------------------}
  
  TJsonSchemaannotations = Class(TGoogleBaseObject)
  Private
    Frequired : TJsonSchemaannotationsrequired;
  Protected
    //Property setters
    Procedure Setrequired(AIndex : Integer; AValue : TJsonSchemaannotationsrequired); virtual;
  Public
  Published
    Property required : TJsonSchemaannotationsrequired Index 0 Read Frequired Write Setrequired;
  end;
  TJsonSchemaannotationsClass = Class of TJsonSchemaannotations;
  
  { --------------------------------------------------------------------
    TJsonSchemaannotationsrequired
    --------------------------------------------------------------------}
  
  TJsonSchemaannotationsrequired = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJsonSchemaannotationsrequiredClass = Class of TJsonSchemaannotationsrequired;
  
  { --------------------------------------------------------------------
    TJsonSchemaenum
    --------------------------------------------------------------------}
  
  TJsonSchemaenum = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJsonSchemaenumClass = Class of TJsonSchemaenum;
  
  { --------------------------------------------------------------------
    TJsonSchemaenumDescriptions
    --------------------------------------------------------------------}
  
  TJsonSchemaenumDescriptions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJsonSchemaenumDescriptionsClass = Class of TJsonSchemaenumDescriptions;
  
  { --------------------------------------------------------------------
    TJsonSchemaproperties
    --------------------------------------------------------------------}
  
  TJsonSchemaproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TJsonSchemapropertiesClass = Class of TJsonSchemaproperties;
  
  { --------------------------------------------------------------------
    TJsonSchemavariant
    --------------------------------------------------------------------}
  
  TJsonSchemavariant = Class(TGoogleBaseObject)
  Private
    Fdiscriminant : string;
    Fmap : TJsonSchemavariantmap;
  Protected
    //Property setters
    Procedure Setdiscriminant(AIndex : Integer; AValue : string); virtual;
    Procedure Setmap(AIndex : Integer; AValue : TJsonSchemavariantmap); virtual;
  Public
  Published
    Property discriminant : string Index 0 Read Fdiscriminant Write Setdiscriminant;
    Property map : TJsonSchemavariantmap Index 8 Read Fmap Write Setmap;
  end;
  TJsonSchemavariantClass = Class of TJsonSchemavariant;
  
  { --------------------------------------------------------------------
    TJsonSchemavariantmap
    --------------------------------------------------------------------}
  
  TJsonSchemavariantmap = Class(TGoogleBaseObject)
  Private
    Fref : string;
    Ftype_value : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setref(AIndex : Integer; AValue : string); virtual;
    Procedure Settype_value(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property ref : string Index 0 Read Fref Write Setref;
    Property type_value : string Index 8 Read Ftype_value Write Settype_value;
  end;
  TJsonSchemavariantmapClass = Class of TJsonSchemavariantmap;
  
  { --------------------------------------------------------------------
    TRestDescription
    --------------------------------------------------------------------}
  
  TRestDescription = Class(TGoogleBaseObject)
  Private
    Fauth : TRestDescriptionauth;
    FbasePath : string;
    FbaseUrl : string;
    FbatchPath : string;
    FcanonicalName : string;
    Fdescription : string;
    FdiscoveryVersion : string;
    FdocumentationLink : string;
    Fetag : string;
    Ffeatures : TRestDescriptionfeatures;
    Ficons : TRestDescriptionicons;
    Fid : string;
    Fkind : string;
    Flabels : TRestDescriptionlabels;
    Fmethods : TRestDescriptionmethods;
    Fname : string;
    FownerDomain : string;
    FownerName : string;
    FpackagePath : string;
    Fparameters : TRestDescriptionparameters;
    Fprotocol : string;
    Fresources : TRestDescriptionresources;
    Frevision : string;
    FrootUrl : string;
    Fschemas : TRestDescriptionschemas;
    FservicePath : string;
    Ftitle : string;
    Fversion : string;
  Protected
    //Property setters
    Procedure Setauth(AIndex : Integer; AValue : TRestDescriptionauth); virtual;
    Procedure SetbasePath(AIndex : Integer; AValue : string); virtual;
    Procedure SetbaseUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetbatchPath(AIndex : Integer; AValue : string); virtual;
    Procedure SetcanonicalName(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiscoveryVersion(AIndex : Integer; AValue : string); virtual;
    Procedure SetdocumentationLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setfeatures(AIndex : Integer; AValue : TRestDescriptionfeatures); virtual;
    Procedure Seticons(AIndex : Integer; AValue : TRestDescriptionicons); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TRestDescriptionlabels); virtual;
    Procedure Setmethods(AIndex : Integer; AValue : TRestDescriptionmethods); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetownerDomain(AIndex : Integer; AValue : string); virtual;
    Procedure SetownerName(AIndex : Integer; AValue : string); virtual;
    Procedure SetpackagePath(AIndex : Integer; AValue : string); virtual;
    Procedure Setparameters(AIndex : Integer; AValue : TRestDescriptionparameters); virtual;
    Procedure Setprotocol(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TRestDescriptionresources); virtual;
    Procedure Setrevision(AIndex : Integer; AValue : string); virtual;
    Procedure SetrootUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setschemas(AIndex : Integer; AValue : TRestDescriptionschemas); virtual;
    Procedure SetservicePath(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property auth : TRestDescriptionauth Index 0 Read Fauth Write Setauth;
    Property basePath : string Index 8 Read FbasePath Write SetbasePath;
    Property baseUrl : string Index 16 Read FbaseUrl Write SetbaseUrl;
    Property batchPath : string Index 24 Read FbatchPath Write SetbatchPath;
    Property canonicalName : string Index 32 Read FcanonicalName Write SetcanonicalName;
    Property description : string Index 40 Read Fdescription Write Setdescription;
    Property discoveryVersion : string Index 48 Read FdiscoveryVersion Write SetdiscoveryVersion;
    Property documentationLink : string Index 56 Read FdocumentationLink Write SetdocumentationLink;
    Property etag : string Index 64 Read Fetag Write Setetag;
    Property features : TRestDescriptionfeatures Index 72 Read Ffeatures Write Setfeatures;
    Property icons : TRestDescriptionicons Index 80 Read Ficons Write Seticons;
    Property id : string Index 88 Read Fid Write Setid;
    Property kind : string Index 96 Read Fkind Write Setkind;
    Property labels : TRestDescriptionlabels Index 104 Read Flabels Write Setlabels;
    Property methods : TRestDescriptionmethods Index 112 Read Fmethods Write Setmethods;
    Property name : string Index 120 Read Fname Write Setname;
    Property ownerDomain : string Index 128 Read FownerDomain Write SetownerDomain;
    Property ownerName : string Index 136 Read FownerName Write SetownerName;
    Property packagePath : string Index 144 Read FpackagePath Write SetpackagePath;
    Property parameters : TRestDescriptionparameters Index 152 Read Fparameters Write Setparameters;
    Property protocol : string Index 160 Read Fprotocol Write Setprotocol;
    Property resources : TRestDescriptionresources Index 168 Read Fresources Write Setresources;
    Property revision : string Index 176 Read Frevision Write Setrevision;
    Property rootUrl : string Index 184 Read FrootUrl Write SetrootUrl;
    Property schemas : TRestDescriptionschemas Index 192 Read Fschemas Write Setschemas;
    Property servicePath : string Index 200 Read FservicePath Write SetservicePath;
    Property title : string Index 208 Read Ftitle Write Settitle;
    Property version : string Index 216 Read Fversion Write Setversion;
  end;
  TRestDescriptionClass = Class of TRestDescription;
  
  { --------------------------------------------------------------------
    TRestDescriptionauth
    --------------------------------------------------------------------}
  
  TRestDescriptionauth = Class(TGoogleBaseObject)
  Private
    Foauth2 : TRestDescriptionauthoauth2;
  Protected
    //Property setters
    Procedure Setoauth2(AIndex : Integer; AValue : TRestDescriptionauthoauth2); virtual;
  Public
  Published
    Property oauth2 : TRestDescriptionauthoauth2 Index 0 Read Foauth2 Write Setoauth2;
  end;
  TRestDescriptionauthClass = Class of TRestDescriptionauth;
  
  { --------------------------------------------------------------------
    TRestDescriptionauthoauth2
    --------------------------------------------------------------------}
  
  TRestDescriptionauthoauth2 = Class(TGoogleBaseObject)
  Private
    Fscopes : TRestDescriptionauthoauth2scopes;
  Protected
    //Property setters
    Procedure Setscopes(AIndex : Integer; AValue : TRestDescriptionauthoauth2scopes); virtual;
  Public
  Published
    Property scopes : TRestDescriptionauthoauth2scopes Index 0 Read Fscopes Write Setscopes;
  end;
  TRestDescriptionauthoauth2Class = Class of TRestDescriptionauthoauth2;
  
  { --------------------------------------------------------------------
    TRestDescriptionauthoauth2scopes
    --------------------------------------------------------------------}
  
  TRestDescriptionauthoauth2scopes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionauthoauth2scopesClass = Class of TRestDescriptionauthoauth2scopes;
  
  { --------------------------------------------------------------------
    TRestDescriptionfeatures
    --------------------------------------------------------------------}
  
  TRestDescriptionfeatures = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRestDescriptionfeaturesClass = Class of TRestDescriptionfeatures;
  
  { --------------------------------------------------------------------
    TRestDescriptionicons
    --------------------------------------------------------------------}
  
  TRestDescriptionicons = Class(TGoogleBaseObject)
  Private
    Fx16 : string;
    Fx32 : string;
  Protected
    //Property setters
    Procedure Setx16(AIndex : Integer; AValue : string); virtual;
    Procedure Setx32(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property x16 : string Index 0 Read Fx16 Write Setx16;
    Property x32 : string Index 8 Read Fx32 Write Setx32;
  end;
  TRestDescriptioniconsClass = Class of TRestDescriptionicons;
  
  { --------------------------------------------------------------------
    TRestDescriptionlabels
    --------------------------------------------------------------------}
  
  TRestDescriptionlabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRestDescriptionlabelsClass = Class of TRestDescriptionlabels;
  
  { --------------------------------------------------------------------
    TRestDescriptionmethods
    --------------------------------------------------------------------}
  
  TRestDescriptionmethods = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionmethodsClass = Class of TRestDescriptionmethods;
  
  { --------------------------------------------------------------------
    TRestDescriptionparameters
    --------------------------------------------------------------------}
  
  TRestDescriptionparameters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionparametersClass = Class of TRestDescriptionparameters;
  
  { --------------------------------------------------------------------
    TRestDescriptionresources
    --------------------------------------------------------------------}
  
  TRestDescriptionresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionresourcesClass = Class of TRestDescriptionresources;
  
  { --------------------------------------------------------------------
    TRestDescriptionschemas
    --------------------------------------------------------------------}
  
  TRestDescriptionschemas = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestDescriptionschemasClass = Class of TRestDescriptionschemas;
  
  { --------------------------------------------------------------------
    TRestMethod
    --------------------------------------------------------------------}
  
  TRestMethod = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    FetagRequired : boolean;
    FhttpMethod : string;
    Fid : string;
    FmediaUpload : TRestMethodmediaUpload;
    FparameterOrder : TRestMethodparameterOrder;
    Fparameters : TRestMethodparameters;
    Fpath : string;
    Frequest : TRestMethodrequest;
    Fresponse : TRestMethodresponse;
    Fscopes : TRestMethodscopes;
    FsupportsMediaDownload : boolean;
    FsupportsMediaUpload : boolean;
    FsupportsSubscription : boolean;
    FuseMediaDownloadService : boolean;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetetagRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SethttpMethod(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetmediaUpload(AIndex : Integer; AValue : TRestMethodmediaUpload); virtual;
    Procedure SetparameterOrder(AIndex : Integer; AValue : TRestMethodparameterOrder); virtual;
    Procedure Setparameters(AIndex : Integer; AValue : TRestMethodparameters); virtual;
    Procedure Setpath(AIndex : Integer; AValue : string); virtual;
    Procedure Setrequest(AIndex : Integer; AValue : TRestMethodrequest); virtual;
    Procedure Setresponse(AIndex : Integer; AValue : TRestMethodresponse); virtual;
    Procedure Setscopes(AIndex : Integer; AValue : TRestMethodscopes); virtual;
    Procedure SetsupportsMediaDownload(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportsMediaUpload(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportsSubscription(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetuseMediaDownloadService(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property etagRequired : boolean Index 8 Read FetagRequired Write SetetagRequired;
    Property httpMethod : string Index 16 Read FhttpMethod Write SethttpMethod;
    Property id : string Index 24 Read Fid Write Setid;
    Property mediaUpload : TRestMethodmediaUpload Index 32 Read FmediaUpload Write SetmediaUpload;
    Property parameterOrder : TRestMethodparameterOrder Index 40 Read FparameterOrder Write SetparameterOrder;
    Property parameters : TRestMethodparameters Index 48 Read Fparameters Write Setparameters;
    Property path : string Index 56 Read Fpath Write Setpath;
    Property request : TRestMethodrequest Index 64 Read Frequest Write Setrequest;
    Property response : TRestMethodresponse Index 72 Read Fresponse Write Setresponse;
    Property scopes : TRestMethodscopes Index 80 Read Fscopes Write Setscopes;
    Property supportsMediaDownload : boolean Index 88 Read FsupportsMediaDownload Write SetsupportsMediaDownload;
    Property supportsMediaUpload : boolean Index 96 Read FsupportsMediaUpload Write SetsupportsMediaUpload;
    Property supportsSubscription : boolean Index 104 Read FsupportsSubscription Write SetsupportsSubscription;
    Property useMediaDownloadService : boolean Index 112 Read FuseMediaDownloadService Write SetuseMediaDownloadService;
  end;
  TRestMethodClass = Class of TRestMethod;
  
  { --------------------------------------------------------------------
    TRestMethodmediaUpload
    --------------------------------------------------------------------}
  
  TRestMethodmediaUpload = Class(TGoogleBaseObject)
  Private
    Faccept : TRestMethodmediaUploadaccept;
    FmaxSize : string;
    Fprotocols : TRestMethodmediaUploadprotocols;
  Protected
    //Property setters
    Procedure Setaccept(AIndex : Integer; AValue : TRestMethodmediaUploadaccept); virtual;
    Procedure SetmaxSize(AIndex : Integer; AValue : string); virtual;
    Procedure Setprotocols(AIndex : Integer; AValue : TRestMethodmediaUploadprotocols); virtual;
  Public
  Published
    Property accept : TRestMethodmediaUploadaccept Index 0 Read Faccept Write Setaccept;
    Property maxSize : string Index 8 Read FmaxSize Write SetmaxSize;
    Property protocols : TRestMethodmediaUploadprotocols Index 16 Read Fprotocols Write Setprotocols;
  end;
  TRestMethodmediaUploadClass = Class of TRestMethodmediaUpload;
  
  { --------------------------------------------------------------------
    TRestMethodmediaUploadaccept
    --------------------------------------------------------------------}
  
  TRestMethodmediaUploadaccept = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRestMethodmediaUploadacceptClass = Class of TRestMethodmediaUploadaccept;
  
  { --------------------------------------------------------------------
    TRestMethodmediaUploadprotocols
    --------------------------------------------------------------------}
  
  TRestMethodmediaUploadprotocols = Class(TGoogleBaseObject)
  Private
    Fresumable : TRestMethodmediaUploadprotocolsresumable;
    Fsimple : TRestMethodmediaUploadprotocolssimple;
  Protected
    //Property setters
    Procedure Setresumable(AIndex : Integer; AValue : TRestMethodmediaUploadprotocolsresumable); virtual;
    Procedure Setsimple(AIndex : Integer; AValue : TRestMethodmediaUploadprotocolssimple); virtual;
  Public
  Published
    Property resumable : TRestMethodmediaUploadprotocolsresumable Index 0 Read Fresumable Write Setresumable;
    Property simple : TRestMethodmediaUploadprotocolssimple Index 8 Read Fsimple Write Setsimple;
  end;
  TRestMethodmediaUploadprotocolsClass = Class of TRestMethodmediaUploadprotocols;
  
  { --------------------------------------------------------------------
    TRestMethodmediaUploadprotocolsresumable
    --------------------------------------------------------------------}
  
  TRestMethodmediaUploadprotocolsresumable = Class(TGoogleBaseObject)
  Private
    Fmultipart : boolean;
    Fpath : string;
  Protected
    //Property setters
    Procedure Setmultipart(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setpath(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property multipart : boolean Index 0 Read Fmultipart Write Setmultipart;
    Property path : string Index 8 Read Fpath Write Setpath;
  end;
  TRestMethodmediaUploadprotocolsresumableClass = Class of TRestMethodmediaUploadprotocolsresumable;
  
  { --------------------------------------------------------------------
    TRestMethodmediaUploadprotocolssimple
    --------------------------------------------------------------------}
  
  TRestMethodmediaUploadprotocolssimple = Class(TGoogleBaseObject)
  Private
    Fmultipart : boolean;
    Fpath : string;
  Protected
    //Property setters
    Procedure Setmultipart(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setpath(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property multipart : boolean Index 0 Read Fmultipart Write Setmultipart;
    Property path : string Index 8 Read Fpath Write Setpath;
  end;
  TRestMethodmediaUploadprotocolssimpleClass = Class of TRestMethodmediaUploadprotocolssimple;
  
  { --------------------------------------------------------------------
    TRestMethodparameterOrder
    --------------------------------------------------------------------}
  
  TRestMethodparameterOrder = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRestMethodparameterOrderClass = Class of TRestMethodparameterOrder;
  
  { --------------------------------------------------------------------
    TRestMethodparameters
    --------------------------------------------------------------------}
  
  TRestMethodparameters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestMethodparametersClass = Class of TRestMethodparameters;
  
  { --------------------------------------------------------------------
    TRestMethodrequest
    --------------------------------------------------------------------}
  
  TRestMethodrequest = Class(TGoogleBaseObject)
  Private
    Fref : string;
    FparameterName : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setref(AIndex : Integer; AValue : string); virtual;
    Procedure SetparameterName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property ref : string Index 0 Read Fref Write Setref;
    Property parameterName : string Index 8 Read FparameterName Write SetparameterName;
  end;
  TRestMethodrequestClass = Class of TRestMethodrequest;
  
  { --------------------------------------------------------------------
    TRestMethodresponse
    --------------------------------------------------------------------}
  
  TRestMethodresponse = Class(TGoogleBaseObject)
  Private
    Fref : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setref(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property ref : string Index 0 Read Fref Write Setref;
  end;
  TRestMethodresponseClass = Class of TRestMethodresponse;
  
  { --------------------------------------------------------------------
    TRestMethodscopes
    --------------------------------------------------------------------}
  
  TRestMethodscopes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRestMethodscopesClass = Class of TRestMethodscopes;
  
  { --------------------------------------------------------------------
    TRestResource
    --------------------------------------------------------------------}
  
  TRestResource = Class(TGoogleBaseObject)
  Private
    Fmethods : TRestResourcemethods;
    Fresources : TRestResourceresources;
  Protected
    //Property setters
    Procedure Setmethods(AIndex : Integer; AValue : TRestResourcemethods); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TRestResourceresources); virtual;
  Public
  Published
    Property methods : TRestResourcemethods Index 0 Read Fmethods Write Setmethods;
    Property resources : TRestResourceresources Index 8 Read Fresources Write Setresources;
  end;
  TRestResourceClass = Class of TRestResource;
  
  { --------------------------------------------------------------------
    TRestResourcemethods
    --------------------------------------------------------------------}
  
  TRestResourcemethods = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestResourcemethodsClass = Class of TRestResourcemethods;
  
  { --------------------------------------------------------------------
    TRestResourceresources
    --------------------------------------------------------------------}
  
  TRestResourceresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRestResourceresourcesClass = Class of TRestResourceresources;
  
  { --------------------------------------------------------------------
    TApisResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TApisResource, method List
  
  TApisListOptions = Record
    _name : string;
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
  TDirectoryList
  --------------------------------------------------------------------}


Procedure TDirectoryList.SetdiscoveryVersion(AIndex : Integer; AValue : string); 

begin
  If (FdiscoveryVersion=AValue) then exit;
  FdiscoveryVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryList.Setitems(AIndex : Integer; AValue : TDirectoryListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectoryListitems
  --------------------------------------------------------------------}


Procedure TDirectoryListitems.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.SetdiscoveryLink(AIndex : Integer; AValue : string); 

begin
  If (FdiscoveryLink=AValue) then exit;
  FdiscoveryLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.SetdiscoveryRestUrl(AIndex : Integer; AValue : string); 

begin
  If (FdiscoveryRestUrl=AValue) then exit;
  FdiscoveryRestUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.SetdocumentationLink(AIndex : Integer; AValue : string); 

begin
  If (FdocumentationLink=AValue) then exit;
  FdocumentationLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.Seticons(AIndex : Integer; AValue : TDirectoryListitemsicons); 

begin
  If (Ficons=AValue) then exit;
  Ficons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.Setlabels(AIndex : Integer; AValue : TDirectoryListitemslabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.Setpreferred(AIndex : Integer; AValue : boolean); 

begin
  If (Fpreferred=AValue) then exit;
  Fpreferred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitems.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectoryListitemsicons
  --------------------------------------------------------------------}


Procedure TDirectoryListitemsicons.Setx16(AIndex : Integer; AValue : string); 

begin
  If (Fx16=AValue) then exit;
  Fx16:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectoryListitemsicons.Setx32(AIndex : Integer; AValue : string); 

begin
  If (Fx32=AValue) then exit;
  Fx32:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectoryListitemslabels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJsonSchema
  --------------------------------------------------------------------}


Procedure TJsonSchema.Setref(AIndex : Integer; AValue : string); 

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



Procedure TJsonSchema.Setannotations(AIndex : Integer; AValue : TJsonSchemaannotations); 

begin
  If (Fannotations=AValue) then exit;
  Fannotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setdefault(AIndex : Integer; AValue : string); 

begin
  If (Fdefault=AValue) then exit;
  Fdefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setenum(AIndex : Integer; AValue : TJsonSchemaenum); 

begin
  If (Fenum=AValue) then exit;
  Fenum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.SetenumDescriptions(AIndex : Integer; AValue : TJsonSchemaenumDescriptions); 

begin
  If (FenumDescriptions=AValue) then exit;
  FenumDescriptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setid(AIndex : Integer; AValue : string); 

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



Procedure TJsonSchema.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setmaximum(AIndex : Integer; AValue : string); 

begin
  If (Fmaximum=AValue) then exit;
  Fmaximum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setminimum(AIndex : Integer; AValue : string); 

begin
  If (Fminimum=AValue) then exit;
  Fminimum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setpattern(AIndex : Integer; AValue : string); 

begin
  If (Fpattern=AValue) then exit;
  Fpattern:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setproperties(AIndex : Integer; AValue : TJsonSchemaproperties); 

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



Procedure TJsonSchema.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchema.Setvariant(AIndex : Integer; AValue : TJsonSchemavariant); 

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
  TJsonSchemaannotations
  --------------------------------------------------------------------}


Procedure TJsonSchemaannotations.Setrequired(AIndex : Integer; AValue : TJsonSchemaannotationsrequired); 

begin
  If (Frequired=AValue) then exit;
  Frequired:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJsonSchemaannotationsrequired
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJsonSchemaenum
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJsonSchemaenumDescriptions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJsonSchemaproperties
  --------------------------------------------------------------------}


Class Function TJsonSchemaproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TJsonSchemavariant
  --------------------------------------------------------------------}


Procedure TJsonSchemavariant.Setdiscriminant(AIndex : Integer; AValue : string); 

begin
  If (Fdiscriminant=AValue) then exit;
  Fdiscriminant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchemavariant.Setmap(AIndex : Integer; AValue : TJsonSchemavariantmap); 

begin
  If (Fmap=AValue) then exit;
  Fmap:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJsonSchemavariantmap
  --------------------------------------------------------------------}


Procedure TJsonSchemavariantmap.Setref(AIndex : Integer; AValue : string); 

begin
  If (Fref=AValue) then exit;
  Fref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJsonSchemavariantmap.Settype_value(AIndex : Integer; AValue : string); 

begin
  If (Ftype_value=AValue) then exit;
  Ftype_value:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TJsonSchemavariantmap.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'ref' : Result:='$ref';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRestDescription
  --------------------------------------------------------------------}


Procedure TRestDescription.Setauth(AIndex : Integer; AValue : TRestDescriptionauth); 

begin
  If (Fauth=AValue) then exit;
  Fauth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetbasePath(AIndex : Integer; AValue : string); 

begin
  If (FbasePath=AValue) then exit;
  FbasePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetbaseUrl(AIndex : Integer; AValue : string); 

begin
  If (FbaseUrl=AValue) then exit;
  FbaseUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetbatchPath(AIndex : Integer; AValue : string); 

begin
  If (FbatchPath=AValue) then exit;
  FbatchPath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetcanonicalName(AIndex : Integer; AValue : string); 

begin
  If (FcanonicalName=AValue) then exit;
  FcanonicalName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetdiscoveryVersion(AIndex : Integer; AValue : string); 

begin
  If (FdiscoveryVersion=AValue) then exit;
  FdiscoveryVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetdocumentationLink(AIndex : Integer; AValue : string); 

begin
  If (FdocumentationLink=AValue) then exit;
  FdocumentationLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setfeatures(AIndex : Integer; AValue : TRestDescriptionfeatures); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Seticons(AIndex : Integer; AValue : TRestDescriptionicons); 

begin
  If (Ficons=AValue) then exit;
  Ficons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setlabels(AIndex : Integer; AValue : TRestDescriptionlabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setmethods(AIndex : Integer; AValue : TRestDescriptionmethods); 

begin
  If (Fmethods=AValue) then exit;
  Fmethods:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetownerDomain(AIndex : Integer; AValue : string); 

begin
  If (FownerDomain=AValue) then exit;
  FownerDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetownerName(AIndex : Integer; AValue : string); 

begin
  If (FownerName=AValue) then exit;
  FownerName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetpackagePath(AIndex : Integer; AValue : string); 

begin
  If (FpackagePath=AValue) then exit;
  FpackagePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setparameters(AIndex : Integer; AValue : TRestDescriptionparameters); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setprotocol(AIndex : Integer; AValue : string); 

begin
  If (Fprotocol=AValue) then exit;
  Fprotocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setresources(AIndex : Integer; AValue : TRestDescriptionresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setrevision(AIndex : Integer; AValue : string); 

begin
  If (Frevision=AValue) then exit;
  Frevision:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetrootUrl(AIndex : Integer; AValue : string); 

begin
  If (FrootUrl=AValue) then exit;
  FrootUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setschemas(AIndex : Integer; AValue : TRestDescriptionschemas); 

begin
  If (Fschemas=AValue) then exit;
  Fschemas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.SetservicePath(AIndex : Integer; AValue : string); 

begin
  If (FservicePath=AValue) then exit;
  FservicePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescription.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestDescriptionauth
  --------------------------------------------------------------------}


Procedure TRestDescriptionauth.Setoauth2(AIndex : Integer; AValue : TRestDescriptionauthoauth2); 

begin
  If (Foauth2=AValue) then exit;
  Foauth2:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestDescriptionauthoauth2
  --------------------------------------------------------------------}


Procedure TRestDescriptionauthoauth2.Setscopes(AIndex : Integer; AValue : TRestDescriptionauthoauth2scopes); 

begin
  If (Fscopes=AValue) then exit;
  Fscopes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestDescriptionauthoauth2scopes
  --------------------------------------------------------------------}


Class Function TRestDescriptionauthoauth2scopes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescriptionfeatures
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRestDescriptionicons
  --------------------------------------------------------------------}


Procedure TRestDescriptionicons.Setx16(AIndex : Integer; AValue : string); 

begin
  If (Fx16=AValue) then exit;
  Fx16:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestDescriptionicons.Setx32(AIndex : Integer; AValue : string); 

begin
  If (Fx32=AValue) then exit;
  Fx32:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestDescriptionlabels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRestDescriptionmethods
  --------------------------------------------------------------------}


Class Function TRestDescriptionmethods.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescriptionparameters
  --------------------------------------------------------------------}


Class Function TRestDescriptionparameters.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescriptionresources
  --------------------------------------------------------------------}


Class Function TRestDescriptionresources.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestDescriptionschemas
  --------------------------------------------------------------------}


Class Function TRestDescriptionschemas.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestMethod
  --------------------------------------------------------------------}


Procedure TRestMethod.Setdescription(AIndex : Integer; AValue : string); 

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



Procedure TRestMethod.SethttpMethod(AIndex : Integer; AValue : string); 

begin
  If (FhttpMethod=AValue) then exit;
  FhttpMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetmediaUpload(AIndex : Integer; AValue : TRestMethodmediaUpload); 

begin
  If (FmediaUpload=AValue) then exit;
  FmediaUpload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.SetparameterOrder(AIndex : Integer; AValue : TRestMethodparameterOrder); 

begin
  If (FparameterOrder=AValue) then exit;
  FparameterOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setparameters(AIndex : Integer; AValue : TRestMethodparameters); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setpath(AIndex : Integer; AValue : string); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setrequest(AIndex : Integer; AValue : TRestMethodrequest); 

begin
  If (Frequest=AValue) then exit;
  Frequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setresponse(AIndex : Integer; AValue : TRestMethodresponse); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethod.Setscopes(AIndex : Integer; AValue : TRestMethodscopes); 

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
  TRestMethodmediaUpload
  --------------------------------------------------------------------}


Procedure TRestMethodmediaUpload.Setaccept(AIndex : Integer; AValue : TRestMethodmediaUploadaccept); 

begin
  If (Faccept=AValue) then exit;
  Faccept:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodmediaUpload.SetmaxSize(AIndex : Integer; AValue : string); 

begin
  If (FmaxSize=AValue) then exit;
  FmaxSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodmediaUpload.Setprotocols(AIndex : Integer; AValue : TRestMethodmediaUploadprotocols); 

begin
  If (Fprotocols=AValue) then exit;
  Fprotocols:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodmediaUploadaccept
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRestMethodmediaUploadprotocols
  --------------------------------------------------------------------}


Procedure TRestMethodmediaUploadprotocols.Setresumable(AIndex : Integer; AValue : TRestMethodmediaUploadprotocolsresumable); 

begin
  If (Fresumable=AValue) then exit;
  Fresumable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodmediaUploadprotocols.Setsimple(AIndex : Integer; AValue : TRestMethodmediaUploadprotocolssimple); 

begin
  If (Fsimple=AValue) then exit;
  Fsimple:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodmediaUploadprotocolsresumable
  --------------------------------------------------------------------}


Procedure TRestMethodmediaUploadprotocolsresumable.Setmultipart(AIndex : Integer; AValue : boolean); 

begin
  If (Fmultipart=AValue) then exit;
  Fmultipart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodmediaUploadprotocolsresumable.Setpath(AIndex : Integer; AValue : string); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodmediaUploadprotocolssimple
  --------------------------------------------------------------------}


Procedure TRestMethodmediaUploadprotocolssimple.Setmultipart(AIndex : Integer; AValue : boolean); 

begin
  If (Fmultipart=AValue) then exit;
  Fmultipart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodmediaUploadprotocolssimple.Setpath(AIndex : Integer; AValue : string); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestMethodparameterOrder
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRestMethodparameters
  --------------------------------------------------------------------}


Class Function TRestMethodparameters.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestMethodrequest
  --------------------------------------------------------------------}


Procedure TRestMethodrequest.Setref(AIndex : Integer; AValue : string); 

begin
  If (Fref=AValue) then exit;
  Fref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestMethodrequest.SetparameterName(AIndex : Integer; AValue : string); 

begin
  If (FparameterName=AValue) then exit;
  FparameterName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRestMethodrequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'ref' : Result:='$ref';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRestMethodresponse
  --------------------------------------------------------------------}


Procedure TRestMethodresponse.Setref(AIndex : Integer; AValue : string); 

begin
  If (Fref=AValue) then exit;
  Fref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRestMethodresponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'ref' : Result:='$ref';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRestMethodscopes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRestResource
  --------------------------------------------------------------------}


Procedure TRestResource.Setmethods(AIndex : Integer; AValue : TRestResourcemethods); 

begin
  If (Fmethods=AValue) then exit;
  Fmethods:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestResource.Setresources(AIndex : Integer; AValue : TRestResourceresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestResourcemethods
  --------------------------------------------------------------------}


Class Function TRestResourcemethods.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRestResourceresources
  --------------------------------------------------------------------}


Class Function TRestResourceresources.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
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
  TDirectoryList.RegisterObject;
  TDirectoryListitems.RegisterObject;
  TDirectoryListitemsicons.RegisterObject;
  TDirectoryListitemslabels.RegisterObject;
  TJsonSchema.RegisterObject;
  TJsonSchemaannotations.RegisterObject;
  TJsonSchemaannotationsrequired.RegisterObject;
  TJsonSchemaenum.RegisterObject;
  TJsonSchemaenumDescriptions.RegisterObject;
  TJsonSchemaproperties.RegisterObject;
  TJsonSchemavariant.RegisterObject;
  TJsonSchemavariantmap.RegisterObject;
  TRestDescription.RegisterObject;
  TRestDescriptionauth.RegisterObject;
  TRestDescriptionauthoauth2.RegisterObject;
  TRestDescriptionauthoauth2scopes.RegisterObject;
  TRestDescriptionfeatures.RegisterObject;
  TRestDescriptionicons.RegisterObject;
  TRestDescriptionlabels.RegisterObject;
  TRestDescriptionmethods.RegisterObject;
  TRestDescriptionparameters.RegisterObject;
  TRestDescriptionresources.RegisterObject;
  TRestDescriptionschemas.RegisterObject;
  TRestMethod.RegisterObject;
  TRestMethodmediaUpload.RegisterObject;
  TRestMethodmediaUploadaccept.RegisterObject;
  TRestMethodmediaUploadprotocols.RegisterObject;
  TRestMethodmediaUploadprotocolsresumable.RegisterObject;
  TRestMethodmediaUploadprotocolssimple.RegisterObject;
  TRestMethodparameterOrder.RegisterObject;
  TRestMethodparameters.RegisterObject;
  TRestMethodrequest.RegisterObject;
  TRestMethodresponse.RegisterObject;
  TRestMethodscopes.RegisterObject;
  TRestResource.RegisterObject;
  TRestResourcemethods.RegisterObject;
  TRestResourceresources.RegisterObject;
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
