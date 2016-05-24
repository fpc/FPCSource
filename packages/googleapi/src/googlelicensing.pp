unit googlelicensing;
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
  TLicenseAssignment = class;
  TLicenseAssignmentArray = Array of TLicenseAssignment;
  TLicenseAssignmentInsert = class;
  TLicenseAssignmentInsertArray = Array of TLicenseAssignmentInsert;
  TLicenseAssignmentList = class;
  TLicenseAssignmentListArray = Array of TLicenseAssignmentList;
  TLicenseAssignmentListitems = class;
  TLicenseAssignmentListitemsArray = Array of TLicenseAssignmentListitems;
  
  { --------------------------------------------------------------------
    TLicenseAssignment
    --------------------------------------------------------------------}
  
  TLicenseAssignment = Class(TGoogleBaseObject)
  Private
    Fetags : string;
    Fkind : string;
    FproductId : string;
    FselfLink : string;
    FskuId : string;
    FuserId : string;
  Protected
    //Property setters
    Procedure Setetags(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetskuId(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etags : string Index 0 Read Fetags Write Setetags;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property productId : string Index 16 Read FproductId Write SetproductId;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
    Property skuId : string Index 32 Read FskuId Write SetskuId;
    Property userId : string Index 40 Read FuserId Write SetuserId;
  end;
  TLicenseAssignmentClass = Class of TLicenseAssignment;
  
  { --------------------------------------------------------------------
    TLicenseAssignmentInsert
    --------------------------------------------------------------------}
  
  TLicenseAssignmentInsert = Class(TGoogleBaseObject)
  Private
    FuserId : string;
  Protected
    //Property setters
    Procedure SetuserId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property userId : string Index 0 Read FuserId Write SetuserId;
  end;
  TLicenseAssignmentInsertClass = Class of TLicenseAssignmentInsert;
  
  { --------------------------------------------------------------------
    TLicenseAssignmentList
    --------------------------------------------------------------------}
  
  TLicenseAssignmentList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TLicenseAssignmentListitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TLicenseAssignmentListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TLicenseAssignmentListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TLicenseAssignmentListClass = Class of TLicenseAssignmentList;
  
  { --------------------------------------------------------------------
    TLicenseAssignmentListitems
    --------------------------------------------------------------------}
  
  TLicenseAssignmentListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLicenseAssignmentListitemsClass = Class of TLicenseAssignmentListitems;
  
  { --------------------------------------------------------------------
    TLicenseAssignmentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLicenseAssignmentsResource, method ListForProduct
  
  TLicenseAssignmentsListForProductOptions = Record
    customerId : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TLicenseAssignmentsResource, method ListForProductAndSku
  
  TLicenseAssignmentsListForProductAndSkuOptions = Record
    customerId : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TLicenseAssignmentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(productId: string; skuId: string; userId: string);
    Function Get(productId: string; skuId: string; userId: string) : TLicenseAssignment;
    Function Insert(productId: string; skuId: string; aLicenseAssignmentInsert : TLicenseAssignmentInsert) : TLicenseAssignment;
    Function ListForProduct(productId: string; AQuery : string  = '') : TLicenseAssignmentList;
    Function ListForProduct(productId: string; AQuery : TLicenseAssignmentslistForProductOptions) : TLicenseAssignmentList;
    Function ListForProductAndSku(productId: string; skuId: string; AQuery : string  = '') : TLicenseAssignmentList;
    Function ListForProductAndSku(productId: string; skuId: string; AQuery : TLicenseAssignmentslistForProductAndSkuOptions) : TLicenseAssignmentList;
    Function Patch(productId: string; skuId: string; userId: string; aLicenseAssignment : TLicenseAssignment) : TLicenseAssignment;
    Function Update(productId: string; skuId: string; userId: string; aLicenseAssignment : TLicenseAssignment) : TLicenseAssignment;
  end;
  
  
  { --------------------------------------------------------------------
    TLicensingAPI
    --------------------------------------------------------------------}
  
  TLicensingAPI = Class(TGoogleAPI)
  Private
    FLicenseAssignmentsInstance : TLicenseAssignmentsResource;
    Function GetLicenseAssignmentsInstance : TLicenseAssignmentsResource;virtual;
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
    Function CreateLicenseAssignmentsResource(AOwner : TComponent) : TLicenseAssignmentsResource;virtual;overload;
    Function CreateLicenseAssignmentsResource : TLicenseAssignmentsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property LicenseAssignmentsResource : TLicenseAssignmentsResource Read GetLicenseAssignmentsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TLicenseAssignment
  --------------------------------------------------------------------}


Procedure TLicenseAssignment.Setetags(AIndex : Integer; AValue : string); 

begin
  If (Fetags=AValue) then exit;
  Fetags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicenseAssignment.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicenseAssignment.SetproductId(AIndex : Integer; AValue : string); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicenseAssignment.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicenseAssignment.SetskuId(AIndex : Integer; AValue : string); 

begin
  If (FskuId=AValue) then exit;
  FskuId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicenseAssignment.SetuserId(AIndex : Integer; AValue : string); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLicenseAssignmentInsert
  --------------------------------------------------------------------}


Procedure TLicenseAssignmentInsert.SetuserId(AIndex : Integer; AValue : string); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLicenseAssignmentList
  --------------------------------------------------------------------}


Procedure TLicenseAssignmentList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicenseAssignmentList.Setitems(AIndex : Integer; AValue : TLicenseAssignmentListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicenseAssignmentList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLicenseAssignmentList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLicenseAssignmentListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLicenseAssignmentsResource
  --------------------------------------------------------------------}


Class Function TLicenseAssignmentsResource.ResourceName : String;

begin
  Result:='licenseAssignments';
end;

Class Function TLicenseAssignmentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TlicensingAPI;
end;

Procedure TLicenseAssignmentsResource.Delete(productId: string; skuId: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{productId}/sku/{skuId}/user/{userId}';
  _Methodid   = 'licensing.licenseAssignments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['productId',productId,'skuId',skuId,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TLicenseAssignmentsResource.Get(productId: string; skuId: string; userId: string) : TLicenseAssignment;

Const
  _HTTPMethod = 'GET';
  _Path       = '{productId}/sku/{skuId}/user/{userId}';
  _Methodid   = 'licensing.licenseAssignments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['productId',productId,'skuId',skuId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLicenseAssignment) as TLicenseAssignment;
end;

Function TLicenseAssignmentsResource.Insert(productId: string; skuId: string; aLicenseAssignmentInsert : TLicenseAssignmentInsert) : TLicenseAssignment;

Const
  _HTTPMethod = 'POST';
  _Path       = '{productId}/sku/{skuId}/user';
  _Methodid   = 'licensing.licenseAssignments.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['productId',productId,'skuId',skuId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLicenseAssignmentInsert,TLicenseAssignment) as TLicenseAssignment;
end;

Function TLicenseAssignmentsResource.ListForProduct(productId: string; AQuery : string = '') : TLicenseAssignmentList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{productId}/users';
  _Methodid   = 'licensing.licenseAssignments.listForProduct';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['productId',productId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLicenseAssignmentList) as TLicenseAssignmentList;
end;


Function TLicenseAssignmentsResource.ListForProduct(productId: string; AQuery : TLicenseAssignmentslistForProductOptions) : TLicenseAssignmentList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'customerId',AQuery.customerId);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListForProduct(productId,_Q);
end;

Function TLicenseAssignmentsResource.ListForProductAndSku(productId: string; skuId: string; AQuery : string = '') : TLicenseAssignmentList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{productId}/sku/{skuId}/users';
  _Methodid   = 'licensing.licenseAssignments.listForProductAndSku';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['productId',productId,'skuId',skuId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLicenseAssignmentList) as TLicenseAssignmentList;
end;


Function TLicenseAssignmentsResource.ListForProductAndSku(productId: string; skuId: string; AQuery : TLicenseAssignmentslistForProductAndSkuOptions) : TLicenseAssignmentList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'customerId',AQuery.customerId);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListForProductAndSku(productId,skuId,_Q);
end;

Function TLicenseAssignmentsResource.Patch(productId: string; skuId: string; userId: string; aLicenseAssignment : TLicenseAssignment) : TLicenseAssignment;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{productId}/sku/{skuId}/user/{userId}';
  _Methodid   = 'licensing.licenseAssignments.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['productId',productId,'skuId',skuId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLicenseAssignment,TLicenseAssignment) as TLicenseAssignment;
end;

Function TLicenseAssignmentsResource.Update(productId: string; skuId: string; userId: string; aLicenseAssignment : TLicenseAssignment) : TLicenseAssignment;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{productId}/sku/{skuId}/user/{userId}';
  _Methodid   = 'licensing.licenseAssignments.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['productId',productId,'skuId',skuId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLicenseAssignment,TLicenseAssignment) as TLicenseAssignment;
end;



{ --------------------------------------------------------------------
  TLicensingAPI
  --------------------------------------------------------------------}

Class Function TLicensingAPI.APIName : String;

begin
  Result:='licensing';
end;

Class Function TLicensingAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TLicensingAPI.APIRevision : String;

begin
  Result:='20140122';
end;

Class Function TLicensingAPI.APIID : String;

begin
  Result:='licensing:v1';
end;

Class Function TLicensingAPI.APITitle : String;

begin
  Result:='Enterprise License Manager API';
end;

Class Function TLicensingAPI.APIDescription : String;

begin
  Result:='Licensing API to view and manage license for your domain.';
end;

Class Function TLicensingAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TLicensingAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TLicensingAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TLicensingAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TLicensingAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/google-apps/licensing/';
end;

Class Function TLicensingAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TLicensingAPI.APIbasePath : string;

begin
  Result:='/apps/licensing/v1/product/';
end;

Class Function TLicensingAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/apps/licensing/v1/product/';
end;

Class Function TLicensingAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TLicensingAPI.APIservicePath : string;

begin
  Result:='apps/licensing/v1/product/';
end;

Class Function TLicensingAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TLicensingAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/apps.licensing';
  Result[0].Description:='View and manage Google Apps licenses for your domain';
  
end;

Class Function TLicensingAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TLicensingAPI.RegisterAPIResources;

begin
  TLicenseAssignment.RegisterObject;
  TLicenseAssignmentInsert.RegisterObject;
  TLicenseAssignmentList.RegisterObject;
  TLicenseAssignmentListitems.RegisterObject;
end;


Function TLicensingAPI.GetLicenseAssignmentsInstance : TLicenseAssignmentsResource;

begin
  if (FLicenseAssignmentsInstance=Nil) then
    FLicenseAssignmentsInstance:=CreateLicenseAssignmentsResource;
  Result:=FLicenseAssignmentsInstance;
end;

Function TLicensingAPI.CreateLicenseAssignmentsResource : TLicenseAssignmentsResource;

begin
  Result:=CreateLicenseAssignmentsResource(Self);
end;


Function TLicensingAPI.CreateLicenseAssignmentsResource(AOwner : TComponent) : TLicenseAssignmentsResource;

begin
  Result:=TLicenseAssignmentsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TLicensingAPI.RegisterAPI;
end.
