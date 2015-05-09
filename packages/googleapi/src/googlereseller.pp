unit googlereseller;
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
//Generated on: 9-5-15 13:22:57
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAddress = class;
  TChangePlanRequest = class;
  TCustomer = class;
  TRenewalSettings = class;
  TSeats = class;
  TSubscription = class;
  TSubscriptions = class;
  TAddressArray = Array of TAddress;
  TChangePlanRequestArray = Array of TChangePlanRequest;
  TCustomerArray = Array of TCustomer;
  TRenewalSettingsArray = Array of TRenewalSettings;
  TSeatsArray = Array of TSeats;
  TSubscriptionArray = Array of TSubscription;
  TSubscriptionsArray = Array of TSubscriptions;
  //Anonymous types, using auto-generated names
  TSubscriptionTypeplanTypecommitmentInterval = class;
  TSubscriptionTypeplan = class;
  TSubscriptionTypetransferInfo = class;
  TSubscriptionTypetrialSettings = class;
  TSubscriptionsTypesubscriptionsArray = Array of TSubscription;
  
  { --------------------------------------------------------------------
    TAddress
    --------------------------------------------------------------------}
  
  TAddress = Class(TGoogleBaseObject)
  Private
    FaddressLine1 : String;
    FaddressLine2 : String;
    FaddressLine3 : String;
    FcontactName : String;
    FcountryCode : String;
    Fkind : String;
    Flocality : String;
    ForganizationName : String;
    FpostalCode : String;
    Fregion : String;
  Protected
    //Property setters
    Procedure SetaddressLine1(AIndex : Integer; AValue : String); virtual;
    Procedure SetaddressLine2(AIndex : Integer; AValue : String); virtual;
    Procedure SetaddressLine3(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontactName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountryCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocality(AIndex : Integer; AValue : String); virtual;
    Procedure SetorganizationName(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostalCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property addressLine1 : String Index 0 Read FaddressLine1 Write SetaddressLine1;
    Property addressLine2 : String Index 8 Read FaddressLine2 Write SetaddressLine2;
    Property addressLine3 : String Index 16 Read FaddressLine3 Write SetaddressLine3;
    Property contactName : String Index 24 Read FcontactName Write SetcontactName;
    Property countryCode : String Index 32 Read FcountryCode Write SetcountryCode;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property locality : String Index 48 Read Flocality Write Setlocality;
    Property organizationName : String Index 56 Read ForganizationName Write SetorganizationName;
    Property postalCode : String Index 64 Read FpostalCode Write SetpostalCode;
    Property region : String Index 72 Read Fregion Write Setregion;
  end;
  TAddressClass = Class of TAddress;
  
  { --------------------------------------------------------------------
    TChangePlanRequest
    --------------------------------------------------------------------}
  
  TChangePlanRequest = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FplanName : String;
    FpurchaseOrderId : String;
    Fseats : TSeats;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetplanName(AIndex : Integer; AValue : String); virtual;
    Procedure SetpurchaseOrderId(AIndex : Integer; AValue : String); virtual;
    Procedure Setseats(AIndex : Integer; AValue : TSeats); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property planName : String Index 8 Read FplanName Write SetplanName;
    Property purchaseOrderId : String Index 16 Read FpurchaseOrderId Write SetpurchaseOrderId;
    Property seats : TSeats Index 24 Read Fseats Write Setseats;
  end;
  TChangePlanRequestClass = Class of TChangePlanRequest;
  
  { --------------------------------------------------------------------
    TCustomer
    --------------------------------------------------------------------}
  
  TCustomer = Class(TGoogleBaseObject)
  Private
    FalternateEmail : String;
    FcustomerDomain : String;
    FcustomerId : String;
    Fkind : String;
    FphoneNumber : String;
    FpostalAddress : TAddress;
    FresourceUiUrl : String;
  Protected
    //Property setters
    Procedure SetalternateEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomerDomain(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomerId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetphoneNumber(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostalAddress(AIndex : Integer; AValue : TAddress); virtual;
    Procedure SetresourceUiUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property alternateEmail : String Index 0 Read FalternateEmail Write SetalternateEmail;
    Property customerDomain : String Index 8 Read FcustomerDomain Write SetcustomerDomain;
    Property customerId : String Index 16 Read FcustomerId Write SetcustomerId;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property phoneNumber : String Index 32 Read FphoneNumber Write SetphoneNumber;
    Property postalAddress : TAddress Index 40 Read FpostalAddress Write SetpostalAddress;
    Property resourceUiUrl : String Index 48 Read FresourceUiUrl Write SetresourceUiUrl;
  end;
  TCustomerClass = Class of TCustomer;
  
  { --------------------------------------------------------------------
    TRenewalSettings
    --------------------------------------------------------------------}
  
  TRenewalSettings = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FrenewalType : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrenewalType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property renewalType : String Index 8 Read FrenewalType Write SetrenewalType;
  end;
  TRenewalSettingsClass = Class of TRenewalSettings;
  
  { --------------------------------------------------------------------
    TSeats
    --------------------------------------------------------------------}
  
  TSeats = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FlicensedNumberOfSeats : integer;
    FmaximumNumberOfSeats : integer;
    FnumberOfSeats : integer;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlicensedNumberOfSeats(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaximumNumberOfSeats(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberOfSeats(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property licensedNumberOfSeats : integer Index 8 Read FlicensedNumberOfSeats Write SetlicensedNumberOfSeats;
    Property maximumNumberOfSeats : integer Index 16 Read FmaximumNumberOfSeats Write SetmaximumNumberOfSeats;
    Property numberOfSeats : integer Index 24 Read FnumberOfSeats Write SetnumberOfSeats;
  end;
  TSeatsClass = Class of TSeats;
  
  { --------------------------------------------------------------------
    TSubscriptionTypeplanTypecommitmentInterval
    --------------------------------------------------------------------}
  
  TSubscriptionTypeplanTypecommitmentInterval = Class(TGoogleBaseObject)
  Private
    FendTime : String;
    FstartTime : String;
  Protected
    //Property setters
    Procedure SetendTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property endTime : String Index 0 Read FendTime Write SetendTime;
    Property startTime : String Index 8 Read FstartTime Write SetstartTime;
  end;
  TSubscriptionTypeplanTypecommitmentIntervalClass = Class of TSubscriptionTypeplanTypecommitmentInterval;
  
  { --------------------------------------------------------------------
    TSubscriptionTypeplan
    --------------------------------------------------------------------}
  
  TSubscriptionTypeplan = Class(TGoogleBaseObject)
  Private
    FcommitmentInterval : TSubscriptionTypeplanTypecommitmentInterval;
    FisCommitmentPlan : boolean;
    FplanName : String;
  Protected
    //Property setters
    Procedure SetcommitmentInterval(AIndex : Integer; AValue : TSubscriptionTypeplanTypecommitmentInterval); virtual;
    Procedure SetisCommitmentPlan(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetplanName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property commitmentInterval : TSubscriptionTypeplanTypecommitmentInterval Index 0 Read FcommitmentInterval Write SetcommitmentInterval;
    Property isCommitmentPlan : boolean Index 8 Read FisCommitmentPlan Write SetisCommitmentPlan;
    Property planName : String Index 16 Read FplanName Write SetplanName;
  end;
  TSubscriptionTypeplanClass = Class of TSubscriptionTypeplan;
  
  { --------------------------------------------------------------------
    TSubscriptionTypetransferInfo
    --------------------------------------------------------------------}
  
  TSubscriptionTypetransferInfo = Class(TGoogleBaseObject)
  Private
    FminimumTransferableSeats : integer;
    FtransferabilityExpirationTime : String;
  Protected
    //Property setters
    Procedure SetminimumTransferableSeats(AIndex : Integer; AValue : integer); virtual;
    Procedure SettransferabilityExpirationTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property minimumTransferableSeats : integer Index 0 Read FminimumTransferableSeats Write SetminimumTransferableSeats;
    Property transferabilityExpirationTime : String Index 8 Read FtransferabilityExpirationTime Write SettransferabilityExpirationTime;
  end;
  TSubscriptionTypetransferInfoClass = Class of TSubscriptionTypetransferInfo;
  
  { --------------------------------------------------------------------
    TSubscriptionTypetrialSettings
    --------------------------------------------------------------------}
  
  TSubscriptionTypetrialSettings = Class(TGoogleBaseObject)
  Private
    FisInTrial : boolean;
    FtrialEndTime : String;
  Protected
    //Property setters
    Procedure SetisInTrial(AIndex : Integer; AValue : boolean); virtual;
    Procedure SettrialEndTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property isInTrial : boolean Index 0 Read FisInTrial Write SetisInTrial;
    Property trialEndTime : String Index 8 Read FtrialEndTime Write SettrialEndTime;
  end;
  TSubscriptionTypetrialSettingsClass = Class of TSubscriptionTypetrialSettings;
  
  { --------------------------------------------------------------------
    TSubscription
    --------------------------------------------------------------------}
  
  TSubscription = Class(TGoogleBaseObject)
  Private
    FbillingMethod : String;
    FcreationTime : String;
    FcustomerId : String;
    Fkind : String;
    Fplan : TSubscriptionTypeplan;
    FpurchaseOrderId : String;
    FrenewalSettings : TRenewalSettings;
    FresourceUiUrl : String;
    Fseats : TSeats;
    FskuId : String;
    Fstatus : String;
    FsubscriptionId : String;
    FtransferInfo : TSubscriptionTypetransferInfo;
    FtrialSettings : TSubscriptionTypetrialSettings;
  Protected
    //Property setters
    Procedure SetbillingMethod(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomerId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setplan(AIndex : Integer; AValue : TSubscriptionTypeplan); virtual;
    Procedure SetpurchaseOrderId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrenewalSettings(AIndex : Integer; AValue : TRenewalSettings); virtual;
    Procedure SetresourceUiUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setseats(AIndex : Integer; AValue : TSeats); virtual;
    Procedure SetskuId(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubscriptionId(AIndex : Integer; AValue : String); virtual;
    Procedure SettransferInfo(AIndex : Integer; AValue : TSubscriptionTypetransferInfo); virtual;
    Procedure SettrialSettings(AIndex : Integer; AValue : TSubscriptionTypetrialSettings); virtual;
  Public
  Published
    Property billingMethod : String Index 0 Read FbillingMethod Write SetbillingMethod;
    Property creationTime : String Index 8 Read FcreationTime Write SetcreationTime;
    Property customerId : String Index 16 Read FcustomerId Write SetcustomerId;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property plan : TSubscriptionTypeplan Index 32 Read Fplan Write Setplan;
    Property purchaseOrderId : String Index 40 Read FpurchaseOrderId Write SetpurchaseOrderId;
    Property renewalSettings : TRenewalSettings Index 48 Read FrenewalSettings Write SetrenewalSettings;
    Property resourceUiUrl : String Index 56 Read FresourceUiUrl Write SetresourceUiUrl;
    Property seats : TSeats Index 64 Read Fseats Write Setseats;
    Property skuId : String Index 72 Read FskuId Write SetskuId;
    Property status : String Index 80 Read Fstatus Write Setstatus;
    Property subscriptionId : String Index 88 Read FsubscriptionId Write SetsubscriptionId;
    Property transferInfo : TSubscriptionTypetransferInfo Index 96 Read FtransferInfo Write SettransferInfo;
    Property trialSettings : TSubscriptionTypetrialSettings Index 104 Read FtrialSettings Write SettrialSettings;
  end;
  TSubscriptionClass = Class of TSubscription;
  
  { --------------------------------------------------------------------
    TSubscriptions
    --------------------------------------------------------------------}
  
  TSubscriptions = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fsubscriptions : TSubscriptionsTypesubscriptionsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setsubscriptions(AIndex : Integer; AValue : TSubscriptionsTypesubscriptionsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property subscriptions : TSubscriptionsTypesubscriptionsArray Index 16 Read Fsubscriptions Write Setsubscriptions;
  end;
  TSubscriptionsClass = Class of TSubscriptions;
  
  { --------------------------------------------------------------------
    TCustomersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCustomersResource, method Insert
  
  TCustomersInsertOptions = Record
    customerAuthToken : String;
  end;
  
  TCustomersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(customerId: string) : TCustomer;
    Function Insert(aCustomer : TCustomer; AQuery : string  = '') : TCustomer;
    Function Insert(aCustomer : TCustomer; AQuery : TCustomersinsertOptions) : TCustomer;
    Function Patch(customerId: string; aCustomer : TCustomer) : TCustomer;
    Function Update(customerId: string; aCustomer : TCustomer) : TCustomer;
  end;
  
  
  { --------------------------------------------------------------------
    TSubscriptionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSubscriptionsResource, method Delete
  
  TSubscriptionsDeleteOptions = Record
    deletionType : String;
  end;
  
  
  //Optional query Options for TSubscriptionsResource, method Insert
  
  TSubscriptionsInsertOptions = Record
    customerAuthToken : String;
  end;
  
  
  //Optional query Options for TSubscriptionsResource, method List
  
  TSubscriptionsListOptions = Record
    customerAuthToken : String;
    customerId : String;
    customerNamePrefix : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  TSubscriptionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Activate(customerId: string; subscriptionId: string) : TSubscription;
    Function ChangePlan(customerId: string; subscriptionId: string; aChangePlanRequest : TChangePlanRequest) : TSubscription;
    Function ChangeRenewalSettings(customerId: string; subscriptionId: string; aRenewalSettings : TRenewalSettings) : TSubscription;
    Function ChangeSeats(customerId: string; subscriptionId: string; aSeats : TSeats) : TSubscription;
    Procedure Delete(customerId: string; subscriptionId: string; AQuery : string  = '');
    Procedure Delete(customerId: string; subscriptionId: string; AQuery : TSubscriptionsdeleteOptions);
    Function Get(customerId: string; subscriptionId: string) : TSubscription;
    Function Insert(customerId: string; aSubscription : TSubscription; AQuery : string  = '') : TSubscription;
    Function Insert(customerId: string; aSubscription : TSubscription; AQuery : TSubscriptionsinsertOptions) : TSubscription;
    Function List(AQuery : string  = '') : TSubscriptions;
    Function List(AQuery : TSubscriptionslistOptions) : TSubscriptions;
    Function StartPaidService(customerId: string; subscriptionId: string) : TSubscription;
    Function Suspend(customerId: string; subscriptionId: string) : TSubscription;
  end;
  
  
  { --------------------------------------------------------------------
    TResellerAPI
    --------------------------------------------------------------------}
  
  TResellerAPI = Class(TGoogleAPI)
  Private
    FCustomersInstance : TCustomersResource;
    FSubscriptionsInstance : TSubscriptionsResource;
    Function GetCustomersInstance : TCustomersResource;virtual;
    Function GetSubscriptionsInstance : TSubscriptionsResource;virtual;
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
    Function CreateCustomersResource(AOwner : TComponent) : TCustomersResource;virtual;overload;
    Function CreateCustomersResource : TCustomersResource;virtual;overload;
    Function CreateSubscriptionsResource(AOwner : TComponent) : TSubscriptionsResource;virtual;overload;
    Function CreateSubscriptionsResource : TSubscriptionsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property CustomersResource : TCustomersResource Read GetCustomersInstance;
    Property SubscriptionsResource : TSubscriptionsResource Read GetSubscriptionsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAddress
  --------------------------------------------------------------------}


Procedure TAddress.SetaddressLine1(AIndex : Integer; AValue : String); 

begin
  If (FaddressLine1=AValue) then exit;
  FaddressLine1:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetaddressLine2(AIndex : Integer; AValue : String); 

begin
  If (FaddressLine2=AValue) then exit;
  FaddressLine2:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetaddressLine3(AIndex : Integer; AValue : String); 

begin
  If (FaddressLine3=AValue) then exit;
  FaddressLine3:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetcontactName(AIndex : Integer; AValue : String); 

begin
  If (FcontactName=AValue) then exit;
  FcontactName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetcountryCode(AIndex : Integer; AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setlocality(AIndex : Integer; AValue : String); 

begin
  If (Flocality=AValue) then exit;
  Flocality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetorganizationName(AIndex : Integer; AValue : String); 

begin
  If (ForganizationName=AValue) then exit;
  ForganizationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetpostalCode(AIndex : Integer; AValue : String); 

begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChangePlanRequest
  --------------------------------------------------------------------}


Procedure TChangePlanRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangePlanRequest.SetplanName(AIndex : Integer; AValue : String); 

begin
  If (FplanName=AValue) then exit;
  FplanName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangePlanRequest.SetpurchaseOrderId(AIndex : Integer; AValue : String); 

begin
  If (FpurchaseOrderId=AValue) then exit;
  FpurchaseOrderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangePlanRequest.Setseats(AIndex : Integer; AValue : TSeats); 

begin
  If (Fseats=AValue) then exit;
  Fseats:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomer
  --------------------------------------------------------------------}


Procedure TCustomer.SetalternateEmail(AIndex : Integer; AValue : String); 

begin
  If (FalternateEmail=AValue) then exit;
  FalternateEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomer.SetcustomerDomain(AIndex : Integer; AValue : String); 

begin
  If (FcustomerDomain=AValue) then exit;
  FcustomerDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomer.SetcustomerId(AIndex : Integer; AValue : String); 

begin
  If (FcustomerId=AValue) then exit;
  FcustomerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomer.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomer.SetphoneNumber(AIndex : Integer; AValue : String); 

begin
  If (FphoneNumber=AValue) then exit;
  FphoneNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomer.SetpostalAddress(AIndex : Integer; AValue : TAddress); 

begin
  If (FpostalAddress=AValue) then exit;
  FpostalAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomer.SetresourceUiUrl(AIndex : Integer; AValue : String); 

begin
  If (FresourceUiUrl=AValue) then exit;
  FresourceUiUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRenewalSettings
  --------------------------------------------------------------------}


Procedure TRenewalSettings.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRenewalSettings.SetrenewalType(AIndex : Integer; AValue : String); 

begin
  If (FrenewalType=AValue) then exit;
  FrenewalType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSeats
  --------------------------------------------------------------------}


Procedure TSeats.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeats.SetlicensedNumberOfSeats(AIndex : Integer; AValue : integer); 

begin
  If (FlicensedNumberOfSeats=AValue) then exit;
  FlicensedNumberOfSeats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeats.SetmaximumNumberOfSeats(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumNumberOfSeats=AValue) then exit;
  FmaximumNumberOfSeats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeats.SetnumberOfSeats(AIndex : Integer; AValue : integer); 

begin
  If (FnumberOfSeats=AValue) then exit;
  FnumberOfSeats:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionTypeplanTypecommitmentInterval
  --------------------------------------------------------------------}


Procedure TSubscriptionTypeplanTypecommitmentInterval.SetendTime(AIndex : Integer; AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionTypeplanTypecommitmentInterval.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionTypeplan
  --------------------------------------------------------------------}


Procedure TSubscriptionTypeplan.SetcommitmentInterval(AIndex : Integer; AValue : TSubscriptionTypeplanTypecommitmentInterval); 

begin
  If (FcommitmentInterval=AValue) then exit;
  FcommitmentInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionTypeplan.SetisCommitmentPlan(AIndex : Integer; AValue : boolean); 

begin
  If (FisCommitmentPlan=AValue) then exit;
  FisCommitmentPlan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionTypeplan.SetplanName(AIndex : Integer; AValue : String); 

begin
  If (FplanName=AValue) then exit;
  FplanName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionTypetransferInfo
  --------------------------------------------------------------------}


Procedure TSubscriptionTypetransferInfo.SetminimumTransferableSeats(AIndex : Integer; AValue : integer); 

begin
  If (FminimumTransferableSeats=AValue) then exit;
  FminimumTransferableSeats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionTypetransferInfo.SettransferabilityExpirationTime(AIndex : Integer; AValue : String); 

begin
  If (FtransferabilityExpirationTime=AValue) then exit;
  FtransferabilityExpirationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionTypetrialSettings
  --------------------------------------------------------------------}


Procedure TSubscriptionTypetrialSettings.SetisInTrial(AIndex : Integer; AValue : boolean); 

begin
  If (FisInTrial=AValue) then exit;
  FisInTrial:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionTypetrialSettings.SettrialEndTime(AIndex : Integer; AValue : String); 

begin
  If (FtrialEndTime=AValue) then exit;
  FtrialEndTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscription
  --------------------------------------------------------------------}


Procedure TSubscription.SetbillingMethod(AIndex : Integer; AValue : String); 

begin
  If (FbillingMethod=AValue) then exit;
  FbillingMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetcreationTime(AIndex : Integer; AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetcustomerId(AIndex : Integer; AValue : String); 

begin
  If (FcustomerId=AValue) then exit;
  FcustomerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setplan(AIndex : Integer; AValue : TSubscriptionTypeplan); 

begin
  If (Fplan=AValue) then exit;
  Fplan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetpurchaseOrderId(AIndex : Integer; AValue : String); 

begin
  If (FpurchaseOrderId=AValue) then exit;
  FpurchaseOrderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetrenewalSettings(AIndex : Integer; AValue : TRenewalSettings); 

begin
  If (FrenewalSettings=AValue) then exit;
  FrenewalSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetresourceUiUrl(AIndex : Integer; AValue : String); 

begin
  If (FresourceUiUrl=AValue) then exit;
  FresourceUiUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setseats(AIndex : Integer; AValue : TSeats); 

begin
  If (Fseats=AValue) then exit;
  Fseats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetskuId(AIndex : Integer; AValue : String); 

begin
  If (FskuId=AValue) then exit;
  FskuId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetsubscriptionId(AIndex : Integer; AValue : String); 

begin
  If (FsubscriptionId=AValue) then exit;
  FsubscriptionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SettransferInfo(AIndex : Integer; AValue : TSubscriptionTypetransferInfo); 

begin
  If (FtransferInfo=AValue) then exit;
  FtransferInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SettrialSettings(AIndex : Integer; AValue : TSubscriptionTypetrialSettings); 

begin
  If (FtrialSettings=AValue) then exit;
  FtrialSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptions
  --------------------------------------------------------------------}


Procedure TSubscriptions.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptions.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptions.Setsubscriptions(AIndex : Integer; AValue : TSubscriptionsTypesubscriptionsArray); 

begin
  If (Fsubscriptions=AValue) then exit;
  Fsubscriptions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomersResource
  --------------------------------------------------------------------}


Class Function TCustomersResource.ResourceName : String;

begin
  Result:='customers';
end;

Class Function TCustomersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TresellerAPI;
end;

Function TCustomersResource.Get(customerId: string) : TCustomer;

Const
  _HTTPMethod = 'GET';
  _Path       = 'customers/{customerId}';
  _Methodid   = 'reseller.customers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCustomer) as TCustomer;
end;

Function TCustomersResource.Insert(aCustomer : TCustomer; AQuery : string = '') : TCustomer;

Const
  _HTTPMethod = 'POST';
  _Path       = 'customers';
  _Methodid   = 'reseller.customers.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aCustomer,TCustomer) as TCustomer;
end;


Function TCustomersResource.Insert(aCustomer : TCustomer; AQuery : TCustomersinsertOptions) : TCustomer;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'customerAuthToken',AQuery.customerAuthToken);
  Result:=Insert(aCustomer,_Q);
end;

Function TCustomersResource.Patch(customerId: string; aCustomer : TCustomer) : TCustomer;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'customers/{customerId}';
  _Methodid   = 'reseller.customers.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCustomer,TCustomer) as TCustomer;
end;

Function TCustomersResource.Update(customerId: string; aCustomer : TCustomer) : TCustomer;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'customers/{customerId}';
  _Methodid   = 'reseller.customers.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCustomer,TCustomer) as TCustomer;
end;



{ --------------------------------------------------------------------
  TSubscriptionsResource
  --------------------------------------------------------------------}


Class Function TSubscriptionsResource.ResourceName : String;

begin
  Result:='subscriptions';
end;

Class Function TSubscriptionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TresellerAPI;
end;

Function TSubscriptionsResource.Activate(customerId: string; subscriptionId: string) : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'customers/{customerId}/subscriptions/{subscriptionId}/activate';
  _Methodid   = 'reseller.subscriptions.activate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId,'subscriptionId',subscriptionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSubscription) as TSubscription;
end;

Function TSubscriptionsResource.ChangePlan(customerId: string; subscriptionId: string; aChangePlanRequest : TChangePlanRequest) : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'customers/{customerId}/subscriptions/{subscriptionId}/changePlan';
  _Methodid   = 'reseller.subscriptions.changePlan';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId,'subscriptionId',subscriptionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aChangePlanRequest,TSubscription) as TSubscription;
end;

Function TSubscriptionsResource.ChangeRenewalSettings(customerId: string; subscriptionId: string; aRenewalSettings : TRenewalSettings) : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'customers/{customerId}/subscriptions/{subscriptionId}/changeRenewalSettings';
  _Methodid   = 'reseller.subscriptions.changeRenewalSettings';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId,'subscriptionId',subscriptionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRenewalSettings,TSubscription) as TSubscription;
end;

Function TSubscriptionsResource.ChangeSeats(customerId: string; subscriptionId: string; aSeats : TSeats) : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'customers/{customerId}/subscriptions/{subscriptionId}/changeSeats';
  _Methodid   = 'reseller.subscriptions.changeSeats';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId,'subscriptionId',subscriptionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSeats,TSubscription) as TSubscription;
end;

Procedure TSubscriptionsResource.Delete(customerId: string; subscriptionId: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'customers/{customerId}/subscriptions/{subscriptionId}';
  _Methodid   = 'reseller.subscriptions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId,'subscriptionId',subscriptionId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TSubscriptionsResource.Delete(customerId: string; subscriptionId: string; AQuery : TSubscriptionsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'deletionType',AQuery.deletionType);
  Delete(customerId,subscriptionId,_Q);
end;

Function TSubscriptionsResource.Get(customerId: string; subscriptionId: string) : TSubscription;

Const
  _HTTPMethod = 'GET';
  _Path       = 'customers/{customerId}/subscriptions/{subscriptionId}';
  _Methodid   = 'reseller.subscriptions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId,'subscriptionId',subscriptionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSubscription) as TSubscription;
end;

Function TSubscriptionsResource.Insert(customerId: string; aSubscription : TSubscription; AQuery : string = '') : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'customers/{customerId}/subscriptions';
  _Methodid   = 'reseller.subscriptions.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aSubscription,TSubscription) as TSubscription;
end;


Function TSubscriptionsResource.Insert(customerId: string; aSubscription : TSubscription; AQuery : TSubscriptionsinsertOptions) : TSubscription;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'customerAuthToken',AQuery.customerAuthToken);
  Result:=Insert(customerId,aSubscription,_Q);
end;

Function TSubscriptionsResource.List(AQuery : string = '') : TSubscriptions;

Const
  _HTTPMethod = 'GET';
  _Path       = 'subscriptions';
  _Methodid   = 'reseller.subscriptions.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSubscriptions) as TSubscriptions;
end;


Function TSubscriptionsResource.List(AQuery : TSubscriptionslistOptions) : TSubscriptions;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'customerAuthToken',AQuery.customerAuthToken);
  AddToQuery(_Q,'customerId',AQuery.customerId);
  AddToQuery(_Q,'customerNamePrefix',AQuery.customerNamePrefix);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;

Function TSubscriptionsResource.StartPaidService(customerId: string; subscriptionId: string) : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'customers/{customerId}/subscriptions/{subscriptionId}/startPaidService';
  _Methodid   = 'reseller.subscriptions.startPaidService';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId,'subscriptionId',subscriptionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSubscription) as TSubscription;
end;

Function TSubscriptionsResource.Suspend(customerId: string; subscriptionId: string) : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'customers/{customerId}/subscriptions/{subscriptionId}/suspend';
  _Methodid   = 'reseller.subscriptions.suspend';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['customerId',customerId,'subscriptionId',subscriptionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSubscription) as TSubscription;
end;



{ --------------------------------------------------------------------
  TResellerAPI
  --------------------------------------------------------------------}

Class Function TResellerAPI.APIName : String;

begin
  Result:='reseller';
end;

Class Function TResellerAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TResellerAPI.APIRevision : String;

begin
  Result:='20141112';
end;

Class Function TResellerAPI.APIID : String;

begin
  Result:='reseller:v1';
end;

Class Function TResellerAPI.APITitle : String;

begin
  Result:='Enterprise Apps Reseller API';
end;

Class Function TResellerAPI.APIDescription : String;

begin
  Result:='Lets you create and manage your customers and their subscriptions.';
end;

Class Function TResellerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TResellerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TResellerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TResellerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TResellerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/google-apps/reseller/';
end;

Class Function TResellerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TResellerAPI.APIbasePath : string;

begin
  Result:='/apps/reseller/v1/';
end;

Class Function TResellerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/apps/reseller/v1/';
end;

Class Function TResellerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TResellerAPI.APIservicePath : string;

begin
  Result:='apps/reseller/v1/';
end;

Class Function TResellerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TResellerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/apps.order';
  Result[0].Description:='Manage users on your domain';
  Result[1].Name:='https://www.googleapis.com/auth/apps.order.readonly';
  Result[1].Description:='Manage users on your domain';
  
end;

Class Function TResellerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TResellerAPI.RegisterAPIResources;

begin
  TAddress.RegisterObject;
  TChangePlanRequest.RegisterObject;
  TCustomer.RegisterObject;
  TRenewalSettings.RegisterObject;
  TSeats.RegisterObject;
  TSubscriptionTypeplanTypecommitmentInterval.RegisterObject;
  TSubscriptionTypeplan.RegisterObject;
  TSubscriptionTypetransferInfo.RegisterObject;
  TSubscriptionTypetrialSettings.RegisterObject;
  TSubscription.RegisterObject;
  TSubscriptions.RegisterObject;
end;


Function TResellerAPI.GetCustomersInstance : TCustomersResource;

begin
  if (FCustomersInstance=Nil) then
    FCustomersInstance:=CreateCustomersResource;
  Result:=FCustomersInstance;
end;

Function TResellerAPI.CreateCustomersResource : TCustomersResource;

begin
  Result:=CreateCustomersResource(Self);
end;


Function TResellerAPI.CreateCustomersResource(AOwner : TComponent) : TCustomersResource;

begin
  Result:=TCustomersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TResellerAPI.GetSubscriptionsInstance : TSubscriptionsResource;

begin
  if (FSubscriptionsInstance=Nil) then
    FSubscriptionsInstance:=CreateSubscriptionsResource;
  Result:=FSubscriptionsInstance;
end;

Function TResellerAPI.CreateSubscriptionsResource : TSubscriptionsResource;

begin
  Result:=CreateSubscriptionsResource(Self);
end;


Function TResellerAPI.CreateSubscriptionsResource(AOwner : TComponent) : TSubscriptionsResource;

begin
  Result:=TSubscriptionsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TResellerAPI.RegisterAPI;
end.
