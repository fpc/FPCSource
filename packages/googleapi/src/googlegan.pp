unit googlegan;
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
//Generated on: 16-5-15 08:53:04
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAdvertiser = Class;
  TAdvertisers = Class;
  TCcOffer = Class;
  TCcOffers = Class;
  TEvent = Class;
  TEvents = Class;
  TLink = Class;
  TLinks = Class;
  TMoney = Class;
  TPublisher = Class;
  TPublishers = Class;
  TReport = Class;
  TAdvertiserArray = Array of TAdvertiser;
  TAdvertisersArray = Array of TAdvertisers;
  TCcOfferArray = Array of TCcOffer;
  TCcOffersArray = Array of TCcOffers;
  TEventArray = Array of TEvent;
  TEventsArray = Array of TEvents;
  TLinkArray = Array of TLink;
  TLinksArray = Array of TLinks;
  TMoneyArray = Array of TMoney;
  TPublisherArray = Array of TPublisher;
  TPublishersArray = Array of TPublishers;
  TReportArray = Array of TReport;
  //Anonymous types, using auto-generated names
  TCcOfferTypebonusRewardsItem = Class;
  TCcOfferTypedefaultFeesItem = Class;
  TCcOfferTyperewardsItem = Class;
  TEventTypeproductsItem = Class;
  TLinkTypespecialOffers = Class;
  TAdvertisersTypeitemsArray = Array of TAdvertiser;
  TCcOfferTypebonusRewardsArray = Array of TCcOfferTypebonusRewardsItem;
  TCcOfferTypedefaultFeesArray = Array of TCcOfferTypedefaultFeesItem;
  TCcOfferTyperewardsArray = Array of TCcOfferTyperewardsItem;
  TCcOffersTypeitemsArray = Array of TCcOffer;
  TEventTypeproductsArray = Array of TEventTypeproductsItem;
  TEventsTypeitemsArray = Array of TEvent;
  TLinksTypeitemsArray = Array of TLink;
  TPublishersTypeitemsArray = Array of TPublisher;
  TReportTyperowsArray = Array of TTJSONSchemaArray;
  TReportTypetotals_rowsArray = Array of TTJSONSchemaArray;
  
  { --------------------------------------------------------------------
    TAdvertiser
    --------------------------------------------------------------------}
  
  TAdvertiser = Class(TGoogleBaseObject)
  Private
    FallowPublisherCreatedLinks : boolean;
    Fcategory : String;
    FcommissionDuration : integer;
    FcontactEmail : String;
    FcontactPhone : String;
    FdefaultLinkId : String;
    Fdescription : String;
    FepcNinetyDayAverage : TMoney;
    FepcSevenDayAverage : TMoney;
    Fid : String;
    Fitem : TAdvertiser;
    FjoinDate : TDatetime;
    Fkind : String;
    FlogoUrl : String;
    FmerchantCenterIds : TStringArray;
    Fname : String;
    FpayoutRank : String;
    FproductFeedsEnabled : boolean;
    FredirectDomains : TStringArray;
    FsiteUrl : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure SetallowPublisherCreatedLinks(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcategory(AIndex : Integer; AValue : String); virtual;
    Procedure SetcommissionDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcontactEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontactPhone(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLinkId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitem(AIndex : Integer; AValue : TAdvertiser); virtual;
    Procedure SetjoinDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlogoUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetmerchantCenterIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpayoutRank(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductFeedsEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetredirectDomains(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsiteUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property allowPublisherCreatedLinks : boolean Index 0 Read FallowPublisherCreatedLinks Write SetallowPublisherCreatedLinks;
    Property category : String Index 8 Read Fcategory Write Setcategory;
    Property commissionDuration : integer Index 16 Read FcommissionDuration Write SetcommissionDuration;
    Property contactEmail : String Index 24 Read FcontactEmail Write SetcontactEmail;
    Property contactPhone : String Index 32 Read FcontactPhone Write SetcontactPhone;
    Property defaultLinkId : String Index 40 Read FdefaultLinkId Write SetdefaultLinkId;
    Property description : String Index 48 Read Fdescription Write Setdescription;
    Property epcNinetyDayAverage : TMoney Index 56 Read FepcNinetyDayAverage Write SetepcNinetyDayAverage;
    Property epcSevenDayAverage : TMoney Index 64 Read FepcSevenDayAverage Write SetepcSevenDayAverage;
    Property id : String Index 72 Read Fid Write Setid;
    Property item : TAdvertiser Index 80 Read Fitem Write Setitem;
    Property joinDate : TDatetime Index 88 Read FjoinDate Write SetjoinDate;
    Property kind : String Index 96 Read Fkind Write Setkind;
    Property logoUrl : String Index 104 Read FlogoUrl Write SetlogoUrl;
    Property merchantCenterIds : TStringArray Index 112 Read FmerchantCenterIds Write SetmerchantCenterIds;
    Property name : String Index 120 Read Fname Write Setname;
    Property payoutRank : String Index 128 Read FpayoutRank Write SetpayoutRank;
    Property productFeedsEnabled : boolean Index 136 Read FproductFeedsEnabled Write SetproductFeedsEnabled;
    Property redirectDomains : TStringArray Index 144 Read FredirectDomains Write SetredirectDomains;
    Property siteUrl : String Index 152 Read FsiteUrl Write SetsiteUrl;
    Property status : String Index 160 Read Fstatus Write Setstatus;
  end;
  TAdvertiserClass = Class of TAdvertiser;
  
  { --------------------------------------------------------------------
    TAdvertisers
    --------------------------------------------------------------------}
  
  TAdvertisers = Class(TGoogleBaseObject)
  Private
    Fitems : TAdvertisersTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAdvertisersTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TAdvertisersTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdvertisersClass = Class of TAdvertisers;
  
  { --------------------------------------------------------------------
    TCcOfferTypebonusRewardsItem
    --------------------------------------------------------------------}
  
  TCcOfferTypebonusRewardsItem = Class(TGoogleBaseObject)
  Private
    Famount : double;
    Fdetails : String;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; AValue : double); virtual;
    Procedure Setdetails(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property amount : double Index 0 Read Famount Write Setamount;
    Property details : String Index 8 Read Fdetails Write Setdetails;
  end;
  TCcOfferTypebonusRewardsItemClass = Class of TCcOfferTypebonusRewardsItem;
  
  { --------------------------------------------------------------------
    TCcOfferTypedefaultFeesItem
    --------------------------------------------------------------------}
  
  TCcOfferTypedefaultFeesItem = Class(TGoogleBaseObject)
  Private
    Fcategory : String;
    FmaxRate : double;
    FminRate : double;
    FrateType : String;
  Protected
    //Property setters
    Procedure Setcategory(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxRate(AIndex : Integer; AValue : double); virtual;
    Procedure SetminRate(AIndex : Integer; AValue : double); virtual;
    Procedure SetrateType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property category : String Index 0 Read Fcategory Write Setcategory;
    Property maxRate : double Index 8 Read FmaxRate Write SetmaxRate;
    Property minRate : double Index 16 Read FminRate Write SetminRate;
    Property rateType : String Index 24 Read FrateType Write SetrateType;
  end;
  TCcOfferTypedefaultFeesItemClass = Class of TCcOfferTypedefaultFeesItem;
  
  { --------------------------------------------------------------------
    TCcOfferTyperewardsItem
    --------------------------------------------------------------------}
  
  TCcOfferTyperewardsItem = Class(TGoogleBaseObject)
  Private
    FadditionalDetails : String;
    Famount : double;
    Fcategory : String;
    FexpirationMonths : double;
    FmaxRewardTier : double;
    FminRewardTier : double;
  Protected
    //Property setters
    Procedure SetadditionalDetails(AIndex : Integer; AValue : String); virtual;
    Procedure Setamount(AIndex : Integer; AValue : double); virtual;
    Procedure Setcategory(AIndex : Integer; AValue : String); virtual;
    Procedure SetexpirationMonths(AIndex : Integer; AValue : double); virtual;
    Procedure SetmaxRewardTier(AIndex : Integer; AValue : double); virtual;
    Procedure SetminRewardTier(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property additionalDetails : String Index 0 Read FadditionalDetails Write SetadditionalDetails;
    Property amount : double Index 8 Read Famount Write Setamount;
    Property category : String Index 16 Read Fcategory Write Setcategory;
    Property expirationMonths : double Index 24 Read FexpirationMonths Write SetexpirationMonths;
    Property maxRewardTier : double Index 32 Read FmaxRewardTier Write SetmaxRewardTier;
    Property minRewardTier : double Index 40 Read FminRewardTier Write SetminRewardTier;
  end;
  TCcOfferTyperewardsItemClass = Class of TCcOfferTyperewardsItem;
  
  { --------------------------------------------------------------------
    TCcOffer
    --------------------------------------------------------------------}
  
  TCcOffer = Class(TGoogleBaseObject)
  Private
    FadditionalCardBenefits : TStringArray;
    FadditionalCardHolderFee : String;
    FageMinimum : double;
    FageMinimumDetails : String;
    FannualFee : double;
    FannualFeeDisplay : String;
    FannualRewardMaximum : double;
    FapprovedCategories : TStringArray;
    FaprDisplay : String;
    FbalanceComputationMethod : String;
    FbalanceTransferTerms : String;
    FbonusRewards : TCcOfferTypebonusRewardsArray;
    FcarRentalInsurance : String;
    FcardBenefits : TStringArray;
    FcardName : String;
    FcardType : String;
    FcashAdvanceTerms : String;
    FcreditLimitMax : double;
    FcreditLimitMin : double;
    FcreditRatingDisplay : String;
    FdefaultFees : TCcOfferTypedefaultFeesArray;
    Fdisclaimer : String;
    FemergencyInsurance : String;
    FexistingCustomerOnly : boolean;
    FextendedWarranty : String;
    FfirstYearAnnualFee : double;
    FflightAccidentInsurance : String;
    FforeignCurrencyTransactionFee : String;
    FfraudLiability : String;
    FgracePeriodDisplay : String;
    FimageUrl : String;
    FinitialSetupAndProcessingFee : String;
    FintroBalanceTransferTerms : String;
    FintroCashAdvanceTerms : String;
    FintroPurchaseTerms : String;
    Fissuer : String;
    FissuerId : String;
    FissuerWebsite : String;
    Fkind : String;
    FlandingPageUrl : String;
    FlatePaymentFee : String;
    FluggageInsurance : String;
    FmaxPurchaseRate : double;
    FminPurchaseRate : double;
    FminimumFinanceCharge : String;
    Fnetwork : String;
    FofferId : String;
    FoffersImmediateCashReward : boolean;
    FoverLimitFee : String;
    FprohibitedCategories : TStringArray;
    FpurchaseRateAdditionalDetails : String;
    FpurchaseRateType : String;
    FreturnedPaymentFee : String;
    FrewardPartner : String;
    FrewardUnit : String;
    Frewards : TCcOfferTyperewardsArray;
    FrewardsExpire : boolean;
    FrewardsHaveBlackoutDates : boolean;
    FstatementCopyFee : String;
    FtrackingUrl : String;
    FtravelInsurance : String;
    FvariableRatesLastUpdated : String;
    FvariableRatesUpdateFrequency : String;
  Protected
    //Property setters
    Procedure SetadditionalCardBenefits(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetadditionalCardHolderFee(AIndex : Integer; AValue : String); virtual;
    Procedure SetageMinimum(AIndex : Integer; AValue : double); virtual;
    Procedure SetageMinimumDetails(AIndex : Integer; AValue : String); virtual;
    Procedure SetannualFee(AIndex : Integer; AValue : double); virtual;
    Procedure SetannualFeeDisplay(AIndex : Integer; AValue : String); virtual;
    Procedure SetannualRewardMaximum(AIndex : Integer; AValue : double); virtual;
    Procedure SetapprovedCategories(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetaprDisplay(AIndex : Integer; AValue : String); virtual;
    Procedure SetbalanceComputationMethod(AIndex : Integer; AValue : String); virtual;
    Procedure SetbalanceTransferTerms(AIndex : Integer; AValue : String); virtual;
    Procedure SetbonusRewards(AIndex : Integer; AValue : TCcOfferTypebonusRewardsArray); virtual;
    Procedure SetcarRentalInsurance(AIndex : Integer; AValue : String); virtual;
    Procedure SetcardBenefits(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetcardName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcardType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcashAdvanceTerms(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreditLimitMax(AIndex : Integer; AValue : double); virtual;
    Procedure SetcreditLimitMin(AIndex : Integer; AValue : double); virtual;
    Procedure SetcreditRatingDisplay(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultFees(AIndex : Integer; AValue : TCcOfferTypedefaultFeesArray); virtual;
    Procedure Setdisclaimer(AIndex : Integer; AValue : String); virtual;
    Procedure SetemergencyInsurance(AIndex : Integer; AValue : String); virtual;
    Procedure SetexistingCustomerOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetextendedWarranty(AIndex : Integer; AValue : String); virtual;
    Procedure SetfirstYearAnnualFee(AIndex : Integer; AValue : double); virtual;
    Procedure SetflightAccidentInsurance(AIndex : Integer; AValue : String); virtual;
    Procedure SetforeignCurrencyTransactionFee(AIndex : Integer; AValue : String); virtual;
    Procedure SetfraudLiability(AIndex : Integer; AValue : String); virtual;
    Procedure SetgracePeriodDisplay(AIndex : Integer; AValue : String); virtual;
    Procedure SetimageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetinitialSetupAndProcessingFee(AIndex : Integer; AValue : String); virtual;
    Procedure SetintroBalanceTransferTerms(AIndex : Integer; AValue : String); virtual;
    Procedure SetintroCashAdvanceTerms(AIndex : Integer; AValue : String); virtual;
    Procedure SetintroPurchaseTerms(AIndex : Integer; AValue : String); virtual;
    Procedure Setissuer(AIndex : Integer; AValue : String); virtual;
    Procedure SetissuerId(AIndex : Integer; AValue : String); virtual;
    Procedure SetissuerWebsite(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlandingPageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetlatePaymentFee(AIndex : Integer; AValue : String); virtual;
    Procedure SetluggageInsurance(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxPurchaseRate(AIndex : Integer; AValue : double); virtual;
    Procedure SetminPurchaseRate(AIndex : Integer; AValue : double); virtual;
    Procedure SetminimumFinanceCharge(AIndex : Integer; AValue : String); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : String); virtual;
    Procedure SetofferId(AIndex : Integer; AValue : String); virtual;
    Procedure SetoffersImmediateCashReward(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetoverLimitFee(AIndex : Integer; AValue : String); virtual;
    Procedure SetprohibitedCategories(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetpurchaseRateAdditionalDetails(AIndex : Integer; AValue : String); virtual;
    Procedure SetpurchaseRateType(AIndex : Integer; AValue : String); virtual;
    Procedure SetreturnedPaymentFee(AIndex : Integer; AValue : String); virtual;
    Procedure SetrewardPartner(AIndex : Integer; AValue : String); virtual;
    Procedure SetrewardUnit(AIndex : Integer; AValue : String); virtual;
    Procedure Setrewards(AIndex : Integer; AValue : TCcOfferTyperewardsArray); virtual;
    Procedure SetrewardsExpire(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetrewardsHaveBlackoutDates(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstatementCopyFee(AIndex : Integer; AValue : String); virtual;
    Procedure SettrackingUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SettravelInsurance(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariableRatesLastUpdated(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariableRatesUpdateFrequency(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property additionalCardBenefits : TStringArray Index 0 Read FadditionalCardBenefits Write SetadditionalCardBenefits;
    Property additionalCardHolderFee : String Index 8 Read FadditionalCardHolderFee Write SetadditionalCardHolderFee;
    Property ageMinimum : double Index 16 Read FageMinimum Write SetageMinimum;
    Property ageMinimumDetails : String Index 24 Read FageMinimumDetails Write SetageMinimumDetails;
    Property annualFee : double Index 32 Read FannualFee Write SetannualFee;
    Property annualFeeDisplay : String Index 40 Read FannualFeeDisplay Write SetannualFeeDisplay;
    Property annualRewardMaximum : double Index 48 Read FannualRewardMaximum Write SetannualRewardMaximum;
    Property approvedCategories : TStringArray Index 56 Read FapprovedCategories Write SetapprovedCategories;
    Property aprDisplay : String Index 64 Read FaprDisplay Write SetaprDisplay;
    Property balanceComputationMethod : String Index 72 Read FbalanceComputationMethod Write SetbalanceComputationMethod;
    Property balanceTransferTerms : String Index 80 Read FbalanceTransferTerms Write SetbalanceTransferTerms;
    Property bonusRewards : TCcOfferTypebonusRewardsArray Index 88 Read FbonusRewards Write SetbonusRewards;
    Property carRentalInsurance : String Index 96 Read FcarRentalInsurance Write SetcarRentalInsurance;
    Property cardBenefits : TStringArray Index 104 Read FcardBenefits Write SetcardBenefits;
    Property cardName : String Index 112 Read FcardName Write SetcardName;
    Property cardType : String Index 120 Read FcardType Write SetcardType;
    Property cashAdvanceTerms : String Index 128 Read FcashAdvanceTerms Write SetcashAdvanceTerms;
    Property creditLimitMax : double Index 136 Read FcreditLimitMax Write SetcreditLimitMax;
    Property creditLimitMin : double Index 144 Read FcreditLimitMin Write SetcreditLimitMin;
    Property creditRatingDisplay : String Index 152 Read FcreditRatingDisplay Write SetcreditRatingDisplay;
    Property defaultFees : TCcOfferTypedefaultFeesArray Index 160 Read FdefaultFees Write SetdefaultFees;
    Property disclaimer : String Index 168 Read Fdisclaimer Write Setdisclaimer;
    Property emergencyInsurance : String Index 176 Read FemergencyInsurance Write SetemergencyInsurance;
    Property existingCustomerOnly : boolean Index 184 Read FexistingCustomerOnly Write SetexistingCustomerOnly;
    Property extendedWarranty : String Index 192 Read FextendedWarranty Write SetextendedWarranty;
    Property firstYearAnnualFee : double Index 200 Read FfirstYearAnnualFee Write SetfirstYearAnnualFee;
    Property flightAccidentInsurance : String Index 208 Read FflightAccidentInsurance Write SetflightAccidentInsurance;
    Property foreignCurrencyTransactionFee : String Index 216 Read FforeignCurrencyTransactionFee Write SetforeignCurrencyTransactionFee;
    Property fraudLiability : String Index 224 Read FfraudLiability Write SetfraudLiability;
    Property gracePeriodDisplay : String Index 232 Read FgracePeriodDisplay Write SetgracePeriodDisplay;
    Property imageUrl : String Index 240 Read FimageUrl Write SetimageUrl;
    Property initialSetupAndProcessingFee : String Index 248 Read FinitialSetupAndProcessingFee Write SetinitialSetupAndProcessingFee;
    Property introBalanceTransferTerms : String Index 256 Read FintroBalanceTransferTerms Write SetintroBalanceTransferTerms;
    Property introCashAdvanceTerms : String Index 264 Read FintroCashAdvanceTerms Write SetintroCashAdvanceTerms;
    Property introPurchaseTerms : String Index 272 Read FintroPurchaseTerms Write SetintroPurchaseTerms;
    Property issuer : String Index 280 Read Fissuer Write Setissuer;
    Property issuerId : String Index 288 Read FissuerId Write SetissuerId;
    Property issuerWebsite : String Index 296 Read FissuerWebsite Write SetissuerWebsite;
    Property kind : String Index 304 Read Fkind Write Setkind;
    Property landingPageUrl : String Index 312 Read FlandingPageUrl Write SetlandingPageUrl;
    Property latePaymentFee : String Index 320 Read FlatePaymentFee Write SetlatePaymentFee;
    Property luggageInsurance : String Index 328 Read FluggageInsurance Write SetluggageInsurance;
    Property maxPurchaseRate : double Index 336 Read FmaxPurchaseRate Write SetmaxPurchaseRate;
    Property minPurchaseRate : double Index 344 Read FminPurchaseRate Write SetminPurchaseRate;
    Property minimumFinanceCharge : String Index 352 Read FminimumFinanceCharge Write SetminimumFinanceCharge;
    Property network : String Index 360 Read Fnetwork Write Setnetwork;
    Property offerId : String Index 368 Read FofferId Write SetofferId;
    Property offersImmediateCashReward : boolean Index 376 Read FoffersImmediateCashReward Write SetoffersImmediateCashReward;
    Property overLimitFee : String Index 384 Read FoverLimitFee Write SetoverLimitFee;
    Property prohibitedCategories : TStringArray Index 392 Read FprohibitedCategories Write SetprohibitedCategories;
    Property purchaseRateAdditionalDetails : String Index 400 Read FpurchaseRateAdditionalDetails Write SetpurchaseRateAdditionalDetails;
    Property purchaseRateType : String Index 408 Read FpurchaseRateType Write SetpurchaseRateType;
    Property returnedPaymentFee : String Index 416 Read FreturnedPaymentFee Write SetreturnedPaymentFee;
    Property rewardPartner : String Index 424 Read FrewardPartner Write SetrewardPartner;
    Property rewardUnit : String Index 432 Read FrewardUnit Write SetrewardUnit;
    Property rewards : TCcOfferTyperewardsArray Index 440 Read Frewards Write Setrewards;
    Property rewardsExpire : boolean Index 448 Read FrewardsExpire Write SetrewardsExpire;
    Property rewardsHaveBlackoutDates : boolean Index 456 Read FrewardsHaveBlackoutDates Write SetrewardsHaveBlackoutDates;
    Property statementCopyFee : String Index 464 Read FstatementCopyFee Write SetstatementCopyFee;
    Property trackingUrl : String Index 472 Read FtrackingUrl Write SettrackingUrl;
    Property travelInsurance : String Index 480 Read FtravelInsurance Write SettravelInsurance;
    Property variableRatesLastUpdated : String Index 488 Read FvariableRatesLastUpdated Write SetvariableRatesLastUpdated;
    Property variableRatesUpdateFrequency : String Index 496 Read FvariableRatesUpdateFrequency Write SetvariableRatesUpdateFrequency;
  end;
  TCcOfferClass = Class of TCcOffer;
  
  { --------------------------------------------------------------------
    TCcOffers
    --------------------------------------------------------------------}
  
  TCcOffers = Class(TGoogleBaseObject)
  Private
    Fitems : TCcOffersTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCcOffersTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TCcOffersTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCcOffersClass = Class of TCcOffers;
  
  { --------------------------------------------------------------------
    TEventTypeproductsItem
    --------------------------------------------------------------------}
  
  TEventTypeproductsItem = Class(TGoogleBaseObject)
  Private
    FcategoryId : String;
    FcategoryName : String;
    Fearnings : TMoney;
    FnetworkFee : TMoney;
    FpublisherFee : TMoney;
    Fquantity : String;
    Fsku : String;
    FskuName : String;
    FunitPrice : TMoney;
  Protected
    //Property setters
    Procedure SetcategoryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcategoryName(AIndex : Integer; AValue : String); virtual;
    Procedure Setearnings(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetnetworkFee(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpublisherFee(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setquantity(AIndex : Integer; AValue : String); virtual;
    Procedure Setsku(AIndex : Integer; AValue : String); virtual;
    Procedure SetskuName(AIndex : Integer; AValue : String); virtual;
    Procedure SetunitPrice(AIndex : Integer; AValue : TMoney); virtual;
  Public
  Published
    Property categoryId : String Index 0 Read FcategoryId Write SetcategoryId;
    Property categoryName : String Index 8 Read FcategoryName Write SetcategoryName;
    Property earnings : TMoney Index 16 Read Fearnings Write Setearnings;
    Property networkFee : TMoney Index 24 Read FnetworkFee Write SetnetworkFee;
    Property publisherFee : TMoney Index 32 Read FpublisherFee Write SetpublisherFee;
    Property quantity : String Index 40 Read Fquantity Write Setquantity;
    Property sku : String Index 48 Read Fsku Write Setsku;
    Property skuName : String Index 56 Read FskuName Write SetskuName;
    Property unitPrice : TMoney Index 64 Read FunitPrice Write SetunitPrice;
  end;
  TEventTypeproductsItemClass = Class of TEventTypeproductsItem;
  
  { --------------------------------------------------------------------
    TEvent
    --------------------------------------------------------------------}
  
  TEvent = Class(TGoogleBaseObject)
  Private
    FadvertiserId : String;
    FadvertiserName : String;
    FchargeId : String;
    FchargeType : String;
    FcommissionableSales : TMoney;
    Fearnings : TMoney;
    FeventDate : TDatetime;
    Fkind : String;
    FmemberId : String;
    FmodifyDate : TDatetime;
    FnetworkFee : TMoney;
    ForderId : String;
    Fproducts : TEventTypeproductsArray;
    FpublisherFee : TMoney;
    FpublisherId : String;
    FpublisherName : String;
    Fstatus : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserName(AIndex : Integer; AValue : String); virtual;
    Procedure SetchargeId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchargeType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcommissionableSales(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setearnings(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SeteventDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmemberId(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodifyDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetnetworkFee(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetorderId(AIndex : Integer; AValue : String); virtual;
    Procedure Setproducts(AIndex : Integer; AValue : TEventTypeproductsArray); virtual;
    Procedure SetpublisherFee(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpublisherId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublisherName(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property advertiserId : String Index 0 Read FadvertiserId Write SetadvertiserId;
    Property advertiserName : String Index 8 Read FadvertiserName Write SetadvertiserName;
    Property chargeId : String Index 16 Read FchargeId Write SetchargeId;
    Property chargeType : String Index 24 Read FchargeType Write SetchargeType;
    Property commissionableSales : TMoney Index 32 Read FcommissionableSales Write SetcommissionableSales;
    Property earnings : TMoney Index 40 Read Fearnings Write Setearnings;
    Property eventDate : TDatetime Index 48 Read FeventDate Write SeteventDate;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property memberId : String Index 64 Read FmemberId Write SetmemberId;
    Property modifyDate : TDatetime Index 72 Read FmodifyDate Write SetmodifyDate;
    Property networkFee : TMoney Index 80 Read FnetworkFee Write SetnetworkFee;
    Property orderId : String Index 88 Read ForderId Write SetorderId;
    Property products : TEventTypeproductsArray Index 96 Read Fproducts Write Setproducts;
    Property publisherFee : TMoney Index 104 Read FpublisherFee Write SetpublisherFee;
    Property publisherId : String Index 112 Read FpublisherId Write SetpublisherId;
    Property publisherName : String Index 120 Read FpublisherName Write SetpublisherName;
    Property status : String Index 128 Read Fstatus Write Setstatus;
    Property _type : String Index 136 Read F_type Write Set_type;
  end;
  TEventClass = Class of TEvent;
  
  { --------------------------------------------------------------------
    TEvents
    --------------------------------------------------------------------}
  
  TEvents = Class(TGoogleBaseObject)
  Private
    Fitems : TEventsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TEventsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TEventsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TEventsClass = Class of TEvents;
  
  { --------------------------------------------------------------------
    TLinkTypespecialOffers
    --------------------------------------------------------------------}
  
  TLinkTypespecialOffers = Class(TGoogleBaseObject)
  Private
    FfreeGift : boolean;
    FfreeShipping : boolean;
    FfreeShippingMin : TMoney;
    FpercentOff : double;
    FpercentOffMin : TMoney;
    FpriceCut : TMoney;
    FpriceCutMin : TMoney;
    FpromotionCodes : TStringArray;
  Protected
    //Property setters
    Procedure SetfreeGift(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfreeShipping(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfreeShippingMin(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpercentOff(AIndex : Integer; AValue : double); virtual;
    Procedure SetpercentOffMin(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpriceCut(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpriceCutMin(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpromotionCodes(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property freeGift : boolean Index 0 Read FfreeGift Write SetfreeGift;
    Property freeShipping : boolean Index 8 Read FfreeShipping Write SetfreeShipping;
    Property freeShippingMin : TMoney Index 16 Read FfreeShippingMin Write SetfreeShippingMin;
    Property percentOff : double Index 24 Read FpercentOff Write SetpercentOff;
    Property percentOffMin : TMoney Index 32 Read FpercentOffMin Write SetpercentOffMin;
    Property priceCut : TMoney Index 40 Read FpriceCut Write SetpriceCut;
    Property priceCutMin : TMoney Index 48 Read FpriceCutMin Write SetpriceCutMin;
    Property promotionCodes : TStringArray Index 56 Read FpromotionCodes Write SetpromotionCodes;
  end;
  TLinkTypespecialOffersClass = Class of TLinkTypespecialOffers;
  
  { --------------------------------------------------------------------
    TLink
    --------------------------------------------------------------------}
  
  TLink = Class(TGoogleBaseObject)
  Private
    FadvertiserId : String;
    Fauthorship : String;
    Favailability : String;
    FclickTrackingUrl : String;
    FcreateDate : TDatetime;
    Fdescription : String;
    FdestinationUrl : String;
    Fduration : String;
    FendDate : TDatetime;
    FepcNinetyDayAverage : TMoney;
    FepcSevenDayAverage : TMoney;
    Fid : String;
    FimageAltText : String;
    FimpressionTrackingUrl : String;
    FisActive : boolean;
    Fkind : String;
    FlinkType : String;
    Fname : String;
    FpromotionType : String;
    FspecialOffers : TLinkTypespecialOffers;
    FstartDate : TDatetime;
  Protected
    //Property setters
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure Setauthorship(AIndex : Integer; AValue : String); virtual;
    Procedure Setavailability(AIndex : Integer; AValue : String); virtual;
    Procedure SetclickTrackingUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreateDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdestinationUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setduration(AIndex : Integer; AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetimageAltText(AIndex : Integer; AValue : String); virtual;
    Procedure SetimpressionTrackingUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetisActive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlinkType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpromotionType(AIndex : Integer; AValue : String); virtual;
    Procedure SetspecialOffers(AIndex : Integer; AValue : TLinkTypespecialOffers); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property advertiserId : String Index 0 Read FadvertiserId Write SetadvertiserId;
    Property authorship : String Index 8 Read Fauthorship Write Setauthorship;
    Property availability : String Index 16 Read Favailability Write Setavailability;
    Property clickTrackingUrl : String Index 24 Read FclickTrackingUrl Write SetclickTrackingUrl;
    Property createDate : TDatetime Index 32 Read FcreateDate Write SetcreateDate;
    Property description : String Index 40 Read Fdescription Write Setdescription;
    Property destinationUrl : String Index 48 Read FdestinationUrl Write SetdestinationUrl;
    Property duration : String Index 56 Read Fduration Write Setduration;
    Property endDate : TDatetime Index 64 Read FendDate Write SetendDate;
    Property epcNinetyDayAverage : TMoney Index 72 Read FepcNinetyDayAverage Write SetepcNinetyDayAverage;
    Property epcSevenDayAverage : TMoney Index 80 Read FepcSevenDayAverage Write SetepcSevenDayAverage;
    Property id : String Index 88 Read Fid Write Setid;
    Property imageAltText : String Index 96 Read FimageAltText Write SetimageAltText;
    Property impressionTrackingUrl : String Index 104 Read FimpressionTrackingUrl Write SetimpressionTrackingUrl;
    Property isActive : boolean Index 112 Read FisActive Write SetisActive;
    Property kind : String Index 120 Read Fkind Write Setkind;
    Property linkType : String Index 128 Read FlinkType Write SetlinkType;
    Property name : String Index 136 Read Fname Write Setname;
    Property promotionType : String Index 144 Read FpromotionType Write SetpromotionType;
    Property specialOffers : TLinkTypespecialOffers Index 152 Read FspecialOffers Write SetspecialOffers;
    Property startDate : TDatetime Index 160 Read FstartDate Write SetstartDate;
  end;
  TLinkClass = Class of TLink;
  
  { --------------------------------------------------------------------
    TLinks
    --------------------------------------------------------------------}
  
  TLinks = Class(TGoogleBaseObject)
  Private
    Fitems : TLinksTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLinksTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TLinksTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TLinksClass = Class of TLinks;
  
  { --------------------------------------------------------------------
    TMoney
    --------------------------------------------------------------------}
  
  TMoney = Class(TGoogleBaseObject)
  Private
    Famount : double;
    FcurrencyCode : String;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property amount : double Index 0 Read Famount Write Setamount;
    Property currencyCode : String Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TMoneyClass = Class of TMoney;
  
  { --------------------------------------------------------------------
    TPublisher
    --------------------------------------------------------------------}
  
  TPublisher = Class(TGoogleBaseObject)
  Private
    Fclassification : String;
    FepcNinetyDayAverage : TMoney;
    FepcSevenDayAverage : TMoney;
    Fid : String;
    Fitem : TPublisher;
    FjoinDate : TDatetime;
    Fkind : String;
    Fname : String;
    FpayoutRank : String;
    Fsites : TStringArray;
    Fstatus : String;
  Protected
    //Property setters
    Procedure Setclassification(AIndex : Integer; AValue : String); virtual;
    Procedure SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitem(AIndex : Integer; AValue : TPublisher); virtual;
    Procedure SetjoinDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpayoutRank(AIndex : Integer; AValue : String); virtual;
    Procedure Setsites(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property classification : String Index 0 Read Fclassification Write Setclassification;
    Property epcNinetyDayAverage : TMoney Index 8 Read FepcNinetyDayAverage Write SetepcNinetyDayAverage;
    Property epcSevenDayAverage : TMoney Index 16 Read FepcSevenDayAverage Write SetepcSevenDayAverage;
    Property id : String Index 24 Read Fid Write Setid;
    Property item : TPublisher Index 32 Read Fitem Write Setitem;
    Property joinDate : TDatetime Index 40 Read FjoinDate Write SetjoinDate;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property name : String Index 56 Read Fname Write Setname;
    Property payoutRank : String Index 64 Read FpayoutRank Write SetpayoutRank;
    Property sites : TStringArray Index 72 Read Fsites Write Setsites;
    Property status : String Index 80 Read Fstatus Write Setstatus;
  end;
  TPublisherClass = Class of TPublisher;
  
  { --------------------------------------------------------------------
    TPublishers
    --------------------------------------------------------------------}
  
  TPublishers = Class(TGoogleBaseObject)
  Private
    Fitems : TPublishersTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPublishersTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TPublishersTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TPublishersClass = Class of TPublishers;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    Fcolumn_names : TStringArray;
    Fend_date : String;
    Fkind : String;
    Fmatching_row_count : String;
    Frows : TReportTyperowsArray;
    Fstart_date : String;
    Ftotals_rows : TReportTypetotals_rowsArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolumn_names(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setend_date(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmatching_row_count(AIndex : Integer; AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TReportTyperowsArray); virtual;
    Procedure Setstart_date(AIndex : Integer; AValue : String); virtual;
    Procedure Settotals_rows(AIndex : Integer; AValue : TReportTypetotals_rowsArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property column_names : TStringArray Index 0 Read Fcolumn_names Write Setcolumn_names;
    Property end_date : String Index 8 Read Fend_date Write Setend_date;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property matching_row_count : String Index 24 Read Fmatching_row_count Write Setmatching_row_count;
    Property rows : TReportTyperowsArray Index 32 Read Frows Write Setrows;
    Property start_date : String Index 40 Read Fstart_date Write Setstart_date;
    Property totals_rows : TReportTypetotals_rowsArray Index 48 Read Ftotals_rows Write Settotals_rows;
    Property _type : String Index 56 Read F_type Write Set_type;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TAdvertisersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdvertisersResource, method Get
  
  TAdvertisersGetOptions = Record
    advertiserId : String;
  end;
  
  
  //Optional query Options for TAdvertisersResource, method List
  
  TAdvertisersListOptions = Record
    advertiserCategory : String;
    maxResults : integer;
    minNinetyDayEpc : double;
    minPayoutRank : integer;
    minSevenDayEpc : double;
    pageToken : String;
    relationshipStatus : String;
  end;
  
  TAdvertisersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(role: string; roleId: string; AQuery : string  = '') : TAdvertiser;
    Function Get(role: string; roleId: string; AQuery : TAdvertisersgetOptions) : TAdvertiser;
    Function List(role: string; roleId: string; AQuery : string  = '') : TAdvertisers;
    Function List(role: string; roleId: string; AQuery : TAdvertiserslistOptions) : TAdvertisers;
  end;
  
  
  { --------------------------------------------------------------------
    TCcOffersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCcOffersResource, method List
  
  TCcOffersListOptions = Record
    advertiser : String;
    projection : String;
  end;
  
  TCcOffersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(publisher: string; AQuery : string  = '') : TCcOffers;
    Function List(publisher: string; AQuery : TCcOfferslistOptions) : TCcOffers;
  end;
  
  
  { --------------------------------------------------------------------
    TEventsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEventsResource, method List
  
  TEventsListOptions = Record
    advertiserId : String;
    chargeType : String;
    eventDateMax : String;
    eventDateMin : String;
    linkId : String;
    maxResults : integer;
    memberId : String;
    modifyDateMax : String;
    modifyDateMin : String;
    orderId : String;
    pageToken : String;
    productCategory : String;
    publisherId : String;
    sku : String;
    status : String;
    _type : String;
  end;
  
  TEventsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(role: string; roleId: string; AQuery : string  = '') : TEvents;
    Function List(role: string; roleId: string; AQuery : TEventslistOptions) : TEvents;
  end;
  
  
  { --------------------------------------------------------------------
    TLinksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLinksResource, method List
  
  TLinksListOptions = Record
    advertiserId : int64;
    assetSize : String;
    authorship : String;
    createDateMax : String;
    createDateMin : String;
    linkType : String;
    maxResults : integer;
    pageToken : String;
    promotionType : String;
    relationshipStatus : String;
    searchText : String;
    startDateMax : String;
    startDateMin : String;
  end;
  
  TLinksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(linkId: string; role: string; roleId: string) : TLink;
    Function Insert(role: string; roleId: string; aLink : TLink) : TLink;
    Function List(role: string; roleId: string; AQuery : string  = '') : TLinks;
    Function List(role: string; roleId: string; AQuery : TLinkslistOptions) : TLinks;
  end;
  
  
  { --------------------------------------------------------------------
    TPublishersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPublishersResource, method Get
  
  TPublishersGetOptions = Record
    publisherId : String;
  end;
  
  
  //Optional query Options for TPublishersResource, method List
  
  TPublishersListOptions = Record
    maxResults : integer;
    minNinetyDayEpc : double;
    minPayoutRank : integer;
    minSevenDayEpc : double;
    pageToken : String;
    publisherCategory : String;
    relationshipStatus : String;
  end;
  
  TPublishersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(role: string; roleId: string; AQuery : string  = '') : TPublisher;
    Function Get(role: string; roleId: string; AQuery : TPublishersgetOptions) : TPublisher;
    Function List(role: string; roleId: string; AQuery : string  = '') : TPublishers;
    Function List(role: string; roleId: string; AQuery : TPublisherslistOptions) : TPublishers;
  end;
  
  
  { --------------------------------------------------------------------
    TReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReportsResource, method Get
  
  TReportsGetOptions = Record
    advertiserId : String;
    calculateTotals : boolean;
    endDate : String;
    eventType : String;
    linkId : String;
    maxResults : integer;
    orderId : String;
    publisherId : String;
    startDate : String;
    startIndex : integer;
    status : String;
  end;
  
  TReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(reportType: string; role: string; roleId: string; AQuery : string  = '') : TReport;
    Function Get(reportType: string; role: string; roleId: string; AQuery : TReportsgetOptions) : TReport;
  end;
  
  
  { --------------------------------------------------------------------
    TGanAPI
    --------------------------------------------------------------------}
  
  TGanAPI = Class(TGoogleAPI)
  Private
    FAdvertisersInstance : TAdvertisersResource;
    FCcOffersInstance : TCcOffersResource;
    FEventsInstance : TEventsResource;
    FLinksInstance : TLinksResource;
    FPublishersInstance : TPublishersResource;
    FReportsInstance : TReportsResource;
    Function GetAdvertisersInstance : TAdvertisersResource;virtual;
    Function GetCcOffersInstance : TCcOffersResource;virtual;
    Function GetEventsInstance : TEventsResource;virtual;
    Function GetLinksInstance : TLinksResource;virtual;
    Function GetPublishersInstance : TPublishersResource;virtual;
    Function GetReportsInstance : TReportsResource;virtual;
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
    Function CreateAdvertisersResource(AOwner : TComponent) : TAdvertisersResource;virtual;overload;
    Function CreateAdvertisersResource : TAdvertisersResource;virtual;overload;
    Function CreateCcOffersResource(AOwner : TComponent) : TCcOffersResource;virtual;overload;
    Function CreateCcOffersResource : TCcOffersResource;virtual;overload;
    Function CreateEventsResource(AOwner : TComponent) : TEventsResource;virtual;overload;
    Function CreateEventsResource : TEventsResource;virtual;overload;
    Function CreateLinksResource(AOwner : TComponent) : TLinksResource;virtual;overload;
    Function CreateLinksResource : TLinksResource;virtual;overload;
    Function CreatePublishersResource(AOwner : TComponent) : TPublishersResource;virtual;overload;
    Function CreatePublishersResource : TPublishersResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AdvertisersResource : TAdvertisersResource Read GetAdvertisersInstance;
    Property CcOffersResource : TCcOffersResource Read GetCcOffersInstance;
    Property EventsResource : TEventsResource Read GetEventsInstance;
    Property LinksResource : TLinksResource Read GetLinksInstance;
    Property PublishersResource : TPublishersResource Read GetPublishersInstance;
    Property ReportsResource : TReportsResource Read GetReportsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAdvertiser
  --------------------------------------------------------------------}


Procedure TAdvertiser.SetallowPublisherCreatedLinks(AIndex : Integer; AValue : boolean); 

begin
  If (FallowPublisherCreatedLinks=AValue) then exit;
  FallowPublisherCreatedLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setcategory(AIndex : Integer; AValue : String); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetcommissionDuration(AIndex : Integer; AValue : integer); 

begin
  If (FcommissionDuration=AValue) then exit;
  FcommissionDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetcontactEmail(AIndex : Integer; AValue : String); 

begin
  If (FcontactEmail=AValue) then exit;
  FcontactEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetcontactPhone(AIndex : Integer; AValue : String); 

begin
  If (FcontactPhone=AValue) then exit;
  FcontactPhone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetdefaultLinkId(AIndex : Integer; AValue : String); 

begin
  If (FdefaultLinkId=AValue) then exit;
  FdefaultLinkId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); 

begin
  If (FepcNinetyDayAverage=AValue) then exit;
  FepcNinetyDayAverage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); 

begin
  If (FepcSevenDayAverage=AValue) then exit;
  FepcSevenDayAverage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setitem(AIndex : Integer; AValue : TAdvertiser); 

begin
  If (Fitem=AValue) then exit;
  Fitem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetjoinDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FjoinDate=AValue) then exit;
  FjoinDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetlogoUrl(AIndex : Integer; AValue : String); 

begin
  If (FlogoUrl=AValue) then exit;
  FlogoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetmerchantCenterIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmerchantCenterIds=AValue) then exit;
  FmerchantCenterIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetpayoutRank(AIndex : Integer; AValue : String); 

begin
  If (FpayoutRank=AValue) then exit;
  FpayoutRank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetproductFeedsEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FproductFeedsEnabled=AValue) then exit;
  FproductFeedsEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetredirectDomains(AIndex : Integer; AValue : TStringArray); 

begin
  If (FredirectDomains=AValue) then exit;
  FredirectDomains:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetsiteUrl(AIndex : Integer; AValue : String); 

begin
  If (FsiteUrl=AValue) then exit;
  FsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAdvertiser.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'merchantcenterids' : SetLength(FmerchantCenterIds,ALength);
  'redirectdomains' : SetLength(FredirectDomains,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAdvertisers
  --------------------------------------------------------------------}


Procedure TAdvertisers.Setitems(AIndex : Integer; AValue : TAdvertisersTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertisers.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertisers.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAdvertisers.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCcOfferTypebonusRewardsItem
  --------------------------------------------------------------------}


Procedure TCcOfferTypebonusRewardsItem.Setamount(AIndex : Integer; AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTypebonusRewardsItem.Setdetails(AIndex : Integer; AValue : String); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCcOfferTypedefaultFeesItem
  --------------------------------------------------------------------}


Procedure TCcOfferTypedefaultFeesItem.Setcategory(AIndex : Integer; AValue : String); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTypedefaultFeesItem.SetmaxRate(AIndex : Integer; AValue : double); 

begin
  If (FmaxRate=AValue) then exit;
  FmaxRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTypedefaultFeesItem.SetminRate(AIndex : Integer; AValue : double); 

begin
  If (FminRate=AValue) then exit;
  FminRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTypedefaultFeesItem.SetrateType(AIndex : Integer; AValue : String); 

begin
  If (FrateType=AValue) then exit;
  FrateType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCcOfferTyperewardsItem
  --------------------------------------------------------------------}


Procedure TCcOfferTyperewardsItem.SetadditionalDetails(AIndex : Integer; AValue : String); 

begin
  If (FadditionalDetails=AValue) then exit;
  FadditionalDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTyperewardsItem.Setamount(AIndex : Integer; AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTyperewardsItem.Setcategory(AIndex : Integer; AValue : String); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTyperewardsItem.SetexpirationMonths(AIndex : Integer; AValue : double); 

begin
  If (FexpirationMonths=AValue) then exit;
  FexpirationMonths:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTyperewardsItem.SetmaxRewardTier(AIndex : Integer; AValue : double); 

begin
  If (FmaxRewardTier=AValue) then exit;
  FmaxRewardTier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferTyperewardsItem.SetminRewardTier(AIndex : Integer; AValue : double); 

begin
  If (FminRewardTier=AValue) then exit;
  FminRewardTier:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCcOffer
  --------------------------------------------------------------------}


Procedure TCcOffer.SetadditionalCardBenefits(AIndex : Integer; AValue : TStringArray); 

begin
  If (FadditionalCardBenefits=AValue) then exit;
  FadditionalCardBenefits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetadditionalCardHolderFee(AIndex : Integer; AValue : String); 

begin
  If (FadditionalCardHolderFee=AValue) then exit;
  FadditionalCardHolderFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetageMinimum(AIndex : Integer; AValue : double); 

begin
  If (FageMinimum=AValue) then exit;
  FageMinimum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetageMinimumDetails(AIndex : Integer; AValue : String); 

begin
  If (FageMinimumDetails=AValue) then exit;
  FageMinimumDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetannualFee(AIndex : Integer; AValue : double); 

begin
  If (FannualFee=AValue) then exit;
  FannualFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetannualFeeDisplay(AIndex : Integer; AValue : String); 

begin
  If (FannualFeeDisplay=AValue) then exit;
  FannualFeeDisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetannualRewardMaximum(AIndex : Integer; AValue : double); 

begin
  If (FannualRewardMaximum=AValue) then exit;
  FannualRewardMaximum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetapprovedCategories(AIndex : Integer; AValue : TStringArray); 

begin
  If (FapprovedCategories=AValue) then exit;
  FapprovedCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetaprDisplay(AIndex : Integer; AValue : String); 

begin
  If (FaprDisplay=AValue) then exit;
  FaprDisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetbalanceComputationMethod(AIndex : Integer; AValue : String); 

begin
  If (FbalanceComputationMethod=AValue) then exit;
  FbalanceComputationMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetbalanceTransferTerms(AIndex : Integer; AValue : String); 

begin
  If (FbalanceTransferTerms=AValue) then exit;
  FbalanceTransferTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetbonusRewards(AIndex : Integer; AValue : TCcOfferTypebonusRewardsArray); 

begin
  If (FbonusRewards=AValue) then exit;
  FbonusRewards:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcarRentalInsurance(AIndex : Integer; AValue : String); 

begin
  If (FcarRentalInsurance=AValue) then exit;
  FcarRentalInsurance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcardBenefits(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcardBenefits=AValue) then exit;
  FcardBenefits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcardName(AIndex : Integer; AValue : String); 

begin
  If (FcardName=AValue) then exit;
  FcardName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcardType(AIndex : Integer; AValue : String); 

begin
  If (FcardType=AValue) then exit;
  FcardType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcashAdvanceTerms(AIndex : Integer; AValue : String); 

begin
  If (FcashAdvanceTerms=AValue) then exit;
  FcashAdvanceTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcreditLimitMax(AIndex : Integer; AValue : double); 

begin
  If (FcreditLimitMax=AValue) then exit;
  FcreditLimitMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcreditLimitMin(AIndex : Integer; AValue : double); 

begin
  If (FcreditLimitMin=AValue) then exit;
  FcreditLimitMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcreditRatingDisplay(AIndex : Integer; AValue : String); 

begin
  If (FcreditRatingDisplay=AValue) then exit;
  FcreditRatingDisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetdefaultFees(AIndex : Integer; AValue : TCcOfferTypedefaultFeesArray); 

begin
  If (FdefaultFees=AValue) then exit;
  FdefaultFees:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setdisclaimer(AIndex : Integer; AValue : String); 

begin
  If (Fdisclaimer=AValue) then exit;
  Fdisclaimer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetemergencyInsurance(AIndex : Integer; AValue : String); 

begin
  If (FemergencyInsurance=AValue) then exit;
  FemergencyInsurance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetexistingCustomerOnly(AIndex : Integer; AValue : boolean); 

begin
  If (FexistingCustomerOnly=AValue) then exit;
  FexistingCustomerOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetextendedWarranty(AIndex : Integer; AValue : String); 

begin
  If (FextendedWarranty=AValue) then exit;
  FextendedWarranty:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetfirstYearAnnualFee(AIndex : Integer; AValue : double); 

begin
  If (FfirstYearAnnualFee=AValue) then exit;
  FfirstYearAnnualFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetflightAccidentInsurance(AIndex : Integer; AValue : String); 

begin
  If (FflightAccidentInsurance=AValue) then exit;
  FflightAccidentInsurance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetforeignCurrencyTransactionFee(AIndex : Integer; AValue : String); 

begin
  If (FforeignCurrencyTransactionFee=AValue) then exit;
  FforeignCurrencyTransactionFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetfraudLiability(AIndex : Integer; AValue : String); 

begin
  If (FfraudLiability=AValue) then exit;
  FfraudLiability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetgracePeriodDisplay(AIndex : Integer; AValue : String); 

begin
  If (FgracePeriodDisplay=AValue) then exit;
  FgracePeriodDisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetimageUrl(AIndex : Integer; AValue : String); 

begin
  If (FimageUrl=AValue) then exit;
  FimageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetinitialSetupAndProcessingFee(AIndex : Integer; AValue : String); 

begin
  If (FinitialSetupAndProcessingFee=AValue) then exit;
  FinitialSetupAndProcessingFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetintroBalanceTransferTerms(AIndex : Integer; AValue : String); 

begin
  If (FintroBalanceTransferTerms=AValue) then exit;
  FintroBalanceTransferTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetintroCashAdvanceTerms(AIndex : Integer; AValue : String); 

begin
  If (FintroCashAdvanceTerms=AValue) then exit;
  FintroCashAdvanceTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetintroPurchaseTerms(AIndex : Integer; AValue : String); 

begin
  If (FintroPurchaseTerms=AValue) then exit;
  FintroPurchaseTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setissuer(AIndex : Integer; AValue : String); 

begin
  If (Fissuer=AValue) then exit;
  Fissuer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetissuerId(AIndex : Integer; AValue : String); 

begin
  If (FissuerId=AValue) then exit;
  FissuerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetissuerWebsite(AIndex : Integer; AValue : String); 

begin
  If (FissuerWebsite=AValue) then exit;
  FissuerWebsite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetlandingPageUrl(AIndex : Integer; AValue : String); 

begin
  If (FlandingPageUrl=AValue) then exit;
  FlandingPageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetlatePaymentFee(AIndex : Integer; AValue : String); 

begin
  If (FlatePaymentFee=AValue) then exit;
  FlatePaymentFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetluggageInsurance(AIndex : Integer; AValue : String); 

begin
  If (FluggageInsurance=AValue) then exit;
  FluggageInsurance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetmaxPurchaseRate(AIndex : Integer; AValue : double); 

begin
  If (FmaxPurchaseRate=AValue) then exit;
  FmaxPurchaseRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetminPurchaseRate(AIndex : Integer; AValue : double); 

begin
  If (FminPurchaseRate=AValue) then exit;
  FminPurchaseRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetminimumFinanceCharge(AIndex : Integer; AValue : String); 

begin
  If (FminimumFinanceCharge=AValue) then exit;
  FminimumFinanceCharge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setnetwork(AIndex : Integer; AValue : String); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetofferId(AIndex : Integer; AValue : String); 

begin
  If (FofferId=AValue) then exit;
  FofferId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetoffersImmediateCashReward(AIndex : Integer; AValue : boolean); 

begin
  If (FoffersImmediateCashReward=AValue) then exit;
  FoffersImmediateCashReward:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetoverLimitFee(AIndex : Integer; AValue : String); 

begin
  If (FoverLimitFee=AValue) then exit;
  FoverLimitFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetprohibitedCategories(AIndex : Integer; AValue : TStringArray); 

begin
  If (FprohibitedCategories=AValue) then exit;
  FprohibitedCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetpurchaseRateAdditionalDetails(AIndex : Integer; AValue : String); 

begin
  If (FpurchaseRateAdditionalDetails=AValue) then exit;
  FpurchaseRateAdditionalDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetpurchaseRateType(AIndex : Integer; AValue : String); 

begin
  If (FpurchaseRateType=AValue) then exit;
  FpurchaseRateType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetreturnedPaymentFee(AIndex : Integer; AValue : String); 

begin
  If (FreturnedPaymentFee=AValue) then exit;
  FreturnedPaymentFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetrewardPartner(AIndex : Integer; AValue : String); 

begin
  If (FrewardPartner=AValue) then exit;
  FrewardPartner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetrewardUnit(AIndex : Integer; AValue : String); 

begin
  If (FrewardUnit=AValue) then exit;
  FrewardUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setrewards(AIndex : Integer; AValue : TCcOfferTyperewardsArray); 

begin
  If (Frewards=AValue) then exit;
  Frewards:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetrewardsExpire(AIndex : Integer; AValue : boolean); 

begin
  If (FrewardsExpire=AValue) then exit;
  FrewardsExpire:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetrewardsHaveBlackoutDates(AIndex : Integer; AValue : boolean); 

begin
  If (FrewardsHaveBlackoutDates=AValue) then exit;
  FrewardsHaveBlackoutDates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetstatementCopyFee(AIndex : Integer; AValue : String); 

begin
  If (FstatementCopyFee=AValue) then exit;
  FstatementCopyFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SettrackingUrl(AIndex : Integer; AValue : String); 

begin
  If (FtrackingUrl=AValue) then exit;
  FtrackingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SettravelInsurance(AIndex : Integer; AValue : String); 

begin
  If (FtravelInsurance=AValue) then exit;
  FtravelInsurance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetvariableRatesLastUpdated(AIndex : Integer; AValue : String); 

begin
  If (FvariableRatesLastUpdated=AValue) then exit;
  FvariableRatesLastUpdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetvariableRatesUpdateFrequency(AIndex : Integer; AValue : String); 

begin
  If (FvariableRatesUpdateFrequency=AValue) then exit;
  FvariableRatesUpdateFrequency:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCcOffer.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'additionalcardbenefits' : SetLength(FadditionalCardBenefits,ALength);
  'approvedcategories' : SetLength(FapprovedCategories,ALength);
  'bonusrewards' : SetLength(FbonusRewards,ALength);
  'cardbenefits' : SetLength(FcardBenefits,ALength);
  'defaultfees' : SetLength(FdefaultFees,ALength);
  'prohibitedcategories' : SetLength(FprohibitedCategories,ALength);
  'rewards' : SetLength(Frewards,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCcOffers
  --------------------------------------------------------------------}


Procedure TCcOffers.Setitems(AIndex : Integer; AValue : TCcOffersTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffers.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCcOffers.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEventTypeproductsItem
  --------------------------------------------------------------------}


Procedure TEventTypeproductsItem.SetcategoryId(AIndex : Integer; AValue : String); 

begin
  If (FcategoryId=AValue) then exit;
  FcategoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTypeproductsItem.SetcategoryName(AIndex : Integer; AValue : String); 

begin
  If (FcategoryName=AValue) then exit;
  FcategoryName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTypeproductsItem.Setearnings(AIndex : Integer; AValue : TMoney); 

begin
  If (Fearnings=AValue) then exit;
  Fearnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTypeproductsItem.SetnetworkFee(AIndex : Integer; AValue : TMoney); 

begin
  If (FnetworkFee=AValue) then exit;
  FnetworkFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTypeproductsItem.SetpublisherFee(AIndex : Integer; AValue : TMoney); 

begin
  If (FpublisherFee=AValue) then exit;
  FpublisherFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTypeproductsItem.Setquantity(AIndex : Integer; AValue : String); 

begin
  If (Fquantity=AValue) then exit;
  Fquantity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTypeproductsItem.Setsku(AIndex : Integer; AValue : String); 

begin
  If (Fsku=AValue) then exit;
  Fsku:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTypeproductsItem.SetskuName(AIndex : Integer; AValue : String); 

begin
  If (FskuName=AValue) then exit;
  FskuName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTypeproductsItem.SetunitPrice(AIndex : Integer; AValue : TMoney); 

begin
  If (FunitPrice=AValue) then exit;
  FunitPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEvent
  --------------------------------------------------------------------}


Procedure TEvent.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetadvertiserName(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserName=AValue) then exit;
  FadvertiserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetchargeId(AIndex : Integer; AValue : String); 

begin
  If (FchargeId=AValue) then exit;
  FchargeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetchargeType(AIndex : Integer; AValue : String); 

begin
  If (FchargeType=AValue) then exit;
  FchargeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetcommissionableSales(AIndex : Integer; AValue : TMoney); 

begin
  If (FcommissionableSales=AValue) then exit;
  FcommissionableSales:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setearnings(AIndex : Integer; AValue : TMoney); 

begin
  If (Fearnings=AValue) then exit;
  Fearnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SeteventDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FeventDate=AValue) then exit;
  FeventDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetmemberId(AIndex : Integer; AValue : String); 

begin
  If (FmemberId=AValue) then exit;
  FmemberId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetmodifyDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmodifyDate=AValue) then exit;
  FmodifyDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetnetworkFee(AIndex : Integer; AValue : TMoney); 

begin
  If (FnetworkFee=AValue) then exit;
  FnetworkFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetorderId(AIndex : Integer; AValue : String); 

begin
  If (ForderId=AValue) then exit;
  ForderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setproducts(AIndex : Integer; AValue : TEventTypeproductsArray); 

begin
  If (Fproducts=AValue) then exit;
  Fproducts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetpublisherFee(AIndex : Integer; AValue : TMoney); 

begin
  If (FpublisherFee=AValue) then exit;
  FpublisherFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetpublisherId(AIndex : Integer; AValue : String); 

begin
  If (FpublisherId=AValue) then exit;
  FpublisherId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetpublisherName(AIndex : Integer; AValue : String); 

begin
  If (FpublisherName=AValue) then exit;
  FpublisherName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEvent.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEvent.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'products' : SetLength(Fproducts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEvents
  --------------------------------------------------------------------}


Procedure TEvents.Setitems(AIndex : Integer; AValue : TEventsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEvents.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLinkTypespecialOffers
  --------------------------------------------------------------------}


Procedure TLinkTypespecialOffers.SetfreeGift(AIndex : Integer; AValue : boolean); 

begin
  If (FfreeGift=AValue) then exit;
  FfreeGift:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkTypespecialOffers.SetfreeShipping(AIndex : Integer; AValue : boolean); 

begin
  If (FfreeShipping=AValue) then exit;
  FfreeShipping:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkTypespecialOffers.SetfreeShippingMin(AIndex : Integer; AValue : TMoney); 

begin
  If (FfreeShippingMin=AValue) then exit;
  FfreeShippingMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkTypespecialOffers.SetpercentOff(AIndex : Integer; AValue : double); 

begin
  If (FpercentOff=AValue) then exit;
  FpercentOff:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkTypespecialOffers.SetpercentOffMin(AIndex : Integer; AValue : TMoney); 

begin
  If (FpercentOffMin=AValue) then exit;
  FpercentOffMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkTypespecialOffers.SetpriceCut(AIndex : Integer; AValue : TMoney); 

begin
  If (FpriceCut=AValue) then exit;
  FpriceCut:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkTypespecialOffers.SetpriceCutMin(AIndex : Integer; AValue : TMoney); 

begin
  If (FpriceCutMin=AValue) then exit;
  FpriceCutMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkTypespecialOffers.SetpromotionCodes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FpromotionCodes=AValue) then exit;
  FpromotionCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLinkTypespecialOffers.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'promotioncodes' : SetLength(FpromotionCodes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLink
  --------------------------------------------------------------------}


Procedure TLink.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setauthorship(AIndex : Integer; AValue : String); 

begin
  If (Fauthorship=AValue) then exit;
  Fauthorship:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setavailability(AIndex : Integer; AValue : String); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetclickTrackingUrl(AIndex : Integer; AValue : String); 

begin
  If (FclickTrackingUrl=AValue) then exit;
  FclickTrackingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetcreateDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreateDate=AValue) then exit;
  FcreateDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetdestinationUrl(AIndex : Integer; AValue : String); 

begin
  If (FdestinationUrl=AValue) then exit;
  FdestinationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setduration(AIndex : Integer; AValue : String); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetendDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); 

begin
  If (FepcNinetyDayAverage=AValue) then exit;
  FepcNinetyDayAverage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); 

begin
  If (FepcSevenDayAverage=AValue) then exit;
  FepcSevenDayAverage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetimageAltText(AIndex : Integer; AValue : String); 

begin
  If (FimageAltText=AValue) then exit;
  FimageAltText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetimpressionTrackingUrl(AIndex : Integer; AValue : String); 

begin
  If (FimpressionTrackingUrl=AValue) then exit;
  FimpressionTrackingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetisActive(AIndex : Integer; AValue : boolean); 

begin
  If (FisActive=AValue) then exit;
  FisActive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetlinkType(AIndex : Integer; AValue : String); 

begin
  If (FlinkType=AValue) then exit;
  FlinkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetpromotionType(AIndex : Integer; AValue : String); 

begin
  If (FpromotionType=AValue) then exit;
  FpromotionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetspecialOffers(AIndex : Integer; AValue : TLinkTypespecialOffers); 

begin
  If (FspecialOffers=AValue) then exit;
  FspecialOffers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetstartDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinks
  --------------------------------------------------------------------}


Procedure TLinks.Setitems(AIndex : Integer; AValue : TLinksTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinks.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinks.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLinks.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMoney
  --------------------------------------------------------------------}


Procedure TMoney.Setamount(AIndex : Integer; AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoney.SetcurrencyCode(AIndex : Integer; AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublisher
  --------------------------------------------------------------------}


Procedure TPublisher.Setclassification(AIndex : Integer; AValue : String); 

begin
  If (Fclassification=AValue) then exit;
  Fclassification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); 

begin
  If (FepcNinetyDayAverage=AValue) then exit;
  FepcNinetyDayAverage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); 

begin
  If (FepcSevenDayAverage=AValue) then exit;
  FepcSevenDayAverage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setitem(AIndex : Integer; AValue : TPublisher); 

begin
  If (Fitem=AValue) then exit;
  Fitem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.SetjoinDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FjoinDate=AValue) then exit;
  FjoinDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.SetpayoutRank(AIndex : Integer; AValue : String); 

begin
  If (FpayoutRank=AValue) then exit;
  FpayoutRank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setsites(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fsites=AValue) then exit;
  Fsites:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPublisher.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sites' : SetLength(Fsites,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPublishers
  --------------------------------------------------------------------}


Procedure TPublishers.Setitems(AIndex : Integer; AValue : TPublishersTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishers.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishers.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPublishers.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.Setcolumn_names(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fcolumn_names=AValue) then exit;
  Fcolumn_names:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setend_date(AIndex : Integer; AValue : String); 

begin
  If (Fend_date=AValue) then exit;
  Fend_date:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setmatching_row_count(AIndex : Integer; AValue : String); 

begin
  If (Fmatching_row_count=AValue) then exit;
  Fmatching_row_count:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setrows(AIndex : Integer; AValue : TReportTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setstart_date(AIndex : Integer; AValue : String); 

begin
  If (Fstart_date=AValue) then exit;
  Fstart_date:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Settotals_rows(AIndex : Integer; AValue : TReportTypetotals_rowsArray); 

begin
  If (Ftotals_rows=AValue) then exit;
  Ftotals_rows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TReport.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReport.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'column_names' : SetLength(Fcolumn_names,ALength);
  'rows' : SetLength(Frows,ALength);
  'totals_rows' : SetLength(Ftotals_rows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAdvertisersResource
  --------------------------------------------------------------------}


Class Function TAdvertisersResource.ResourceName : String;

begin
  Result:='advertisers';
end;

Class Function TAdvertisersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TganAPI;
end;

Function TAdvertisersResource.Get(role: string; roleId: string; AQuery : string = '') : TAdvertiser;

Const
  _HTTPMethod = 'GET';
  _Path       = '{role}/{roleId}/advertiser';
  _Methodid   = 'gan.advertisers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdvertiser) as TAdvertiser;
end;


Function TAdvertisersResource.Get(role: string; roleId: string; AQuery : TAdvertisersgetOptions) : TAdvertiser;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  Result:=Get(role,roleId,_Q);
end;

Function TAdvertisersResource.List(role: string; roleId: string; AQuery : string = '') : TAdvertisers;

Const
  _HTTPMethod = 'GET';
  _Path       = '{role}/{roleId}/advertisers';
  _Methodid   = 'gan.advertisers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdvertisers) as TAdvertisers;
end;


Function TAdvertisersResource.List(role: string; roleId: string; AQuery : TAdvertiserslistOptions) : TAdvertisers;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserCategory',AQuery.advertiserCategory);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'minNinetyDayEpc',AQuery.minNinetyDayEpc);
  AddToQuery(_Q,'minPayoutRank',AQuery.minPayoutRank);
  AddToQuery(_Q,'minSevenDayEpc',AQuery.minSevenDayEpc);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'relationshipStatus',AQuery.relationshipStatus);
  Result:=List(role,roleId,_Q);
end;



{ --------------------------------------------------------------------
  TCcOffersResource
  --------------------------------------------------------------------}


Class Function TCcOffersResource.ResourceName : String;

begin
  Result:='ccOffers';
end;

Class Function TCcOffersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TganAPI;
end;

Function TCcOffersResource.List(publisher: string; AQuery : string = '') : TCcOffers;

Const
  _HTTPMethod = 'GET';
  _Path       = 'publishers/{publisher}/ccOffers';
  _Methodid   = 'gan.ccOffers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['publisher',publisher]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCcOffers) as TCcOffers;
end;


Function TCcOffersResource.List(publisher: string; AQuery : TCcOfferslistOptions) : TCcOffers;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiser',AQuery.advertiser);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=List(publisher,_Q);
end;



{ --------------------------------------------------------------------
  TEventsResource
  --------------------------------------------------------------------}


Class Function TEventsResource.ResourceName : String;

begin
  Result:='events';
end;

Class Function TEventsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TganAPI;
end;

Function TEventsResource.List(role: string; roleId: string; AQuery : string = '') : TEvents;

Const
  _HTTPMethod = 'GET';
  _Path       = '{role}/{roleId}/events';
  _Methodid   = 'gan.events.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEvents) as TEvents;
end;


Function TEventsResource.List(role: string; roleId: string; AQuery : TEventslistOptions) : TEvents;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'chargeType',AQuery.chargeType);
  AddToQuery(_Q,'eventDateMax',AQuery.eventDateMax);
  AddToQuery(_Q,'eventDateMin',AQuery.eventDateMin);
  AddToQuery(_Q,'linkId',AQuery.linkId);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'memberId',AQuery.memberId);
  AddToQuery(_Q,'modifyDateMax',AQuery.modifyDateMax);
  AddToQuery(_Q,'modifyDateMin',AQuery.modifyDateMin);
  AddToQuery(_Q,'orderId',AQuery.orderId);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'productCategory',AQuery.productCategory);
  AddToQuery(_Q,'publisherId',AQuery.publisherId);
  AddToQuery(_Q,'sku',AQuery.sku);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'type',AQuery._type);
  Result:=List(role,roleId,_Q);
end;



{ --------------------------------------------------------------------
  TLinksResource
  --------------------------------------------------------------------}


Class Function TLinksResource.ResourceName : String;

begin
  Result:='links';
end;

Class Function TLinksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TganAPI;
end;

Function TLinksResource.Get(linkId: string; role: string; roleId: string) : TLink;

Const
  _HTTPMethod = 'GET';
  _Path       = '{role}/{roleId}/link/{linkId}';
  _Methodid   = 'gan.links.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['linkId',linkId,'role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLink) as TLink;
end;

Function TLinksResource.Insert(role: string; roleId: string; aLink : TLink) : TLink;

Const
  _HTTPMethod = 'POST';
  _Path       = '{role}/{roleId}/link';
  _Methodid   = 'gan.links.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLink,TLink) as TLink;
end;

Function TLinksResource.List(role: string; roleId: string; AQuery : string = '') : TLinks;

Const
  _HTTPMethod = 'GET';
  _Path       = '{role}/{roleId}/links';
  _Methodid   = 'gan.links.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLinks) as TLinks;
end;


Function TLinksResource.List(role: string; roleId: string; AQuery : TLinkslistOptions) : TLinks;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'assetSize',AQuery.assetSize);
  AddToQuery(_Q,'authorship',AQuery.authorship);
  AddToQuery(_Q,'createDateMax',AQuery.createDateMax);
  AddToQuery(_Q,'createDateMin',AQuery.createDateMin);
  AddToQuery(_Q,'linkType',AQuery.linkType);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'promotionType',AQuery.promotionType);
  AddToQuery(_Q,'relationshipStatus',AQuery.relationshipStatus);
  AddToQuery(_Q,'searchText',AQuery.searchText);
  AddToQuery(_Q,'startDateMax',AQuery.startDateMax);
  AddToQuery(_Q,'startDateMin',AQuery.startDateMin);
  Result:=List(role,roleId,_Q);
end;



{ --------------------------------------------------------------------
  TPublishersResource
  --------------------------------------------------------------------}


Class Function TPublishersResource.ResourceName : String;

begin
  Result:='publishers';
end;

Class Function TPublishersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TganAPI;
end;

Function TPublishersResource.Get(role: string; roleId: string; AQuery : string = '') : TPublisher;

Const
  _HTTPMethod = 'GET';
  _Path       = '{role}/{roleId}/publisher';
  _Methodid   = 'gan.publishers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPublisher) as TPublisher;
end;


Function TPublishersResource.Get(role: string; roleId: string; AQuery : TPublishersgetOptions) : TPublisher;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'publisherId',AQuery.publisherId);
  Result:=Get(role,roleId,_Q);
end;

Function TPublishersResource.List(role: string; roleId: string; AQuery : string = '') : TPublishers;

Const
  _HTTPMethod = 'GET';
  _Path       = '{role}/{roleId}/publishers';
  _Methodid   = 'gan.publishers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPublishers) as TPublishers;
end;


Function TPublishersResource.List(role: string; roleId: string; AQuery : TPublisherslistOptions) : TPublishers;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'minNinetyDayEpc',AQuery.minNinetyDayEpc);
  AddToQuery(_Q,'minPayoutRank',AQuery.minPayoutRank);
  AddToQuery(_Q,'minSevenDayEpc',AQuery.minSevenDayEpc);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'publisherCategory',AQuery.publisherCategory);
  AddToQuery(_Q,'relationshipStatus',AQuery.relationshipStatus);
  Result:=List(role,roleId,_Q);
end;



{ --------------------------------------------------------------------
  TReportsResource
  --------------------------------------------------------------------}


Class Function TReportsResource.ResourceName : String;

begin
  Result:='reports';
end;

Class Function TReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TganAPI;
end;

Function TReportsResource.Get(reportType: string; role: string; roleId: string; AQuery : string = '') : TReport;

Const
  _HTTPMethod = 'GET';
  _Path       = '{role}/{roleId}/report/{reportType}';
  _Methodid   = 'gan.reports.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['reportType',reportType,'role',role,'roleId',roleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TReport) as TReport;
end;


Function TReportsResource.Get(reportType: string; role: string; roleId: string; AQuery : TReportsgetOptions) : TReport;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'calculateTotals',AQuery.calculateTotals);
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'eventType',AQuery.eventType);
  AddToQuery(_Q,'linkId',AQuery.linkId);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderId',AQuery.orderId);
  AddToQuery(_Q,'publisherId',AQuery.publisherId);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  AddToQuery(_Q,'status',AQuery.status);
  Result:=Get(reportType,role,roleId,_Q);
end;



{ --------------------------------------------------------------------
  TGanAPI
  --------------------------------------------------------------------}

Class Function TGanAPI.APIName : String;

begin
  Result:='gan';
end;

Class Function TGanAPI.APIVersion : String;

begin
  Result:='v1beta1';
end;

Class Function TGanAPI.APIRevision : String;

begin
  Result:='20130205';
end;

Class Function TGanAPI.APIID : String;

begin
  Result:='gan:v1beta1';
end;

Class Function TGanAPI.APITitle : String;

begin
  Result:='Google Affiliate Network API';
end;

Class Function TGanAPI.APIDescription : String;

begin
  Result:='Lets you have programmatic access to your Google Affiliate Network data.';
end;

Class Function TGanAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TGanAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TGanAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/affiliatenetwork-16.png';
end;

Class Function TGanAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/affiliatenetwork-32.png';
end;

Class Function TGanAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/affiliate-network/';
end;

Class Function TGanAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TGanAPI.APIbasePath : string;

begin
  Result:='/gan/v1beta1/';
end;

Class Function TGanAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/gan/v1beta1/';
end;

Class Function TGanAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGanAPI.APIservicePath : string;

begin
  Result:='gan/v1beta1/';
end;

Class Function TGanAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TGanAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TGanAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TGanAPI.RegisterAPIResources;

begin
  TAdvertiser.RegisterObject;
  TAdvertisers.RegisterObject;
  TCcOfferTypebonusRewardsItem.RegisterObject;
  TCcOfferTypedefaultFeesItem.RegisterObject;
  TCcOfferTyperewardsItem.RegisterObject;
  TCcOffer.RegisterObject;
  TCcOffers.RegisterObject;
  TEventTypeproductsItem.RegisterObject;
  TEvent.RegisterObject;
  TEvents.RegisterObject;
  TLinkTypespecialOffers.RegisterObject;
  TLink.RegisterObject;
  TLinks.RegisterObject;
  TMoney.RegisterObject;
  TPublisher.RegisterObject;
  TPublishers.RegisterObject;
  TReport.RegisterObject;
end;


Function TGanAPI.GetAdvertisersInstance : TAdvertisersResource;

begin
  if (FAdvertisersInstance=Nil) then
    FAdvertisersInstance:=CreateAdvertisersResource;
  Result:=FAdvertisersInstance;
end;

Function TGanAPI.CreateAdvertisersResource : TAdvertisersResource;

begin
  Result:=CreateAdvertisersResource(Self);
end;


Function TGanAPI.CreateAdvertisersResource(AOwner : TComponent) : TAdvertisersResource;

begin
  Result:=TAdvertisersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGanAPI.GetCcOffersInstance : TCcOffersResource;

begin
  if (FCcOffersInstance=Nil) then
    FCcOffersInstance:=CreateCcOffersResource;
  Result:=FCcOffersInstance;
end;

Function TGanAPI.CreateCcOffersResource : TCcOffersResource;

begin
  Result:=CreateCcOffersResource(Self);
end;


Function TGanAPI.CreateCcOffersResource(AOwner : TComponent) : TCcOffersResource;

begin
  Result:=TCcOffersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGanAPI.GetEventsInstance : TEventsResource;

begin
  if (FEventsInstance=Nil) then
    FEventsInstance:=CreateEventsResource;
  Result:=FEventsInstance;
end;

Function TGanAPI.CreateEventsResource : TEventsResource;

begin
  Result:=CreateEventsResource(Self);
end;


Function TGanAPI.CreateEventsResource(AOwner : TComponent) : TEventsResource;

begin
  Result:=TEventsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGanAPI.GetLinksInstance : TLinksResource;

begin
  if (FLinksInstance=Nil) then
    FLinksInstance:=CreateLinksResource;
  Result:=FLinksInstance;
end;

Function TGanAPI.CreateLinksResource : TLinksResource;

begin
  Result:=CreateLinksResource(Self);
end;


Function TGanAPI.CreateLinksResource(AOwner : TComponent) : TLinksResource;

begin
  Result:=TLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGanAPI.GetPublishersInstance : TPublishersResource;

begin
  if (FPublishersInstance=Nil) then
    FPublishersInstance:=CreatePublishersResource;
  Result:=FPublishersInstance;
end;

Function TGanAPI.CreatePublishersResource : TPublishersResource;

begin
  Result:=CreatePublishersResource(Self);
end;


Function TGanAPI.CreatePublishersResource(AOwner : TComponent) : TPublishersResource;

begin
  Result:=TPublishersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGanAPI.GetReportsInstance : TReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TGanAPI.CreateReportsResource : TReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TGanAPI.CreateReportsResource(AOwner : TComponent) : TReportsResource;

begin
  Result:=TReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TGanAPI.RegisterAPI;
end.
