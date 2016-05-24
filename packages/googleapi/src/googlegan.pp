unit googlegan;
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
  TAdvertiser = class;
  TAdvertiserArray = Array of TAdvertiser;
  TAdvertisermerchantCenterIds = class;
  TAdvertisermerchantCenterIdsArray = Array of TAdvertisermerchantCenterIds;
  TAdvertiserredirectDomains = class;
  TAdvertiserredirectDomainsArray = Array of TAdvertiserredirectDomains;
  TAdvertisers = class;
  TAdvertisersArray = Array of TAdvertisers;
  TAdvertisersitems = class;
  TAdvertisersitemsArray = Array of TAdvertisersitems;
  TCcOffer = class;
  TCcOfferArray = Array of TCcOffer;
  TCcOfferadditionalCardBenefits = class;
  TCcOfferadditionalCardBenefitsArray = Array of TCcOfferadditionalCardBenefits;
  TCcOfferapprovedCategories = class;
  TCcOfferapprovedCategoriesArray = Array of TCcOfferapprovedCategories;
  TCcOfferbonusRewards = class;
  TCcOfferbonusRewardsArray = Array of TCcOfferbonusRewards;
  TCcOffercardBenefits = class;
  TCcOffercardBenefitsArray = Array of TCcOffercardBenefits;
  TCcOfferdefaultFees = class;
  TCcOfferdefaultFeesArray = Array of TCcOfferdefaultFees;
  TCcOfferprohibitedCategories = class;
  TCcOfferprohibitedCategoriesArray = Array of TCcOfferprohibitedCategories;
  TCcOfferrewards = class;
  TCcOfferrewardsArray = Array of TCcOfferrewards;
  TCcOffers = class;
  TCcOffersArray = Array of TCcOffers;
  TCcOffersitems = class;
  TCcOffersitemsArray = Array of TCcOffersitems;
  TEvent = class;
  TEventArray = Array of TEvent;
  TEventproducts = class;
  TEventproductsArray = Array of TEventproducts;
  TEvents = class;
  TEventsArray = Array of TEvents;
  TEventsitems = class;
  TEventsitemsArray = Array of TEventsitems;
  TLink = class;
  TLinkArray = Array of TLink;
  TLinkspecialOffers = class;
  TLinkspecialOffersArray = Array of TLinkspecialOffers;
  TLinkspecialOfferspromotionCodes = class;
  TLinkspecialOfferspromotionCodesArray = Array of TLinkspecialOfferspromotionCodes;
  TLinks = class;
  TLinksArray = Array of TLinks;
  TLinksitems = class;
  TLinksitemsArray = Array of TLinksitems;
  TMoney = class;
  TMoneyArray = Array of TMoney;
  TPublisher = class;
  TPublisherArray = Array of TPublisher;
  TPublishersites = class;
  TPublishersitesArray = Array of TPublishersites;
  TPublishers = class;
  TPublishersArray = Array of TPublishers;
  TPublishersitems = class;
  TPublishersitemsArray = Array of TPublishersitems;
  TReport = class;
  TReportArray = Array of TReport;
  TReportcolumn_names = class;
  TReportcolumn_namesArray = Array of TReportcolumn_names;
  TReportrows = class;
  TReportrowsArray = Array of TReportrows;
  TReporttotals_rows = class;
  TReporttotals_rowsArray = Array of TReporttotals_rows;
  
  { --------------------------------------------------------------------
    TAdvertiser
    --------------------------------------------------------------------}
  
  TAdvertiser = Class(TGoogleBaseObject)
  Private
    FallowPublisherCreatedLinks : boolean;
    Fcategory : string;
    FcommissionDuration : integer;
    FcontactEmail : string;
    FcontactPhone : string;
    FdefaultLinkId : string;
    Fdescription : string;
    FepcNinetyDayAverage : TMoney;
    FepcSevenDayAverage : TMoney;
    Fid : string;
    Fitem : TAdvertiser;
    FjoinDate : TDatetime;
    Fkind : string;
    FlogoUrl : string;
    FmerchantCenterIds : TAdvertisermerchantCenterIds;
    Fname : string;
    FpayoutRank : string;
    FproductFeedsEnabled : boolean;
    FredirectDomains : TAdvertiserredirectDomains;
    FsiteUrl : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure SetallowPublisherCreatedLinks(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcategory(AIndex : Integer; AValue : string); virtual;
    Procedure SetcommissionDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcontactEmail(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontactPhone(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultLinkId(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitem(AIndex : Integer; AValue : TAdvertiser); virtual;
    Procedure SetjoinDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlogoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetmerchantCenterIds(AIndex : Integer; AValue : TAdvertisermerchantCenterIds); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpayoutRank(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductFeedsEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetredirectDomains(AIndex : Integer; AValue : TAdvertiserredirectDomains); virtual;
    Procedure SetsiteUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property allowPublisherCreatedLinks : boolean Index 0 Read FallowPublisherCreatedLinks Write SetallowPublisherCreatedLinks;
    Property category : string Index 8 Read Fcategory Write Setcategory;
    Property commissionDuration : integer Index 16 Read FcommissionDuration Write SetcommissionDuration;
    Property contactEmail : string Index 24 Read FcontactEmail Write SetcontactEmail;
    Property contactPhone : string Index 32 Read FcontactPhone Write SetcontactPhone;
    Property defaultLinkId : string Index 40 Read FdefaultLinkId Write SetdefaultLinkId;
    Property description : string Index 48 Read Fdescription Write Setdescription;
    Property epcNinetyDayAverage : TMoney Index 56 Read FepcNinetyDayAverage Write SetepcNinetyDayAverage;
    Property epcSevenDayAverage : TMoney Index 64 Read FepcSevenDayAverage Write SetepcSevenDayAverage;
    Property id : string Index 72 Read Fid Write Setid;
    Property item : TAdvertiser Index 80 Read Fitem Write Setitem;
    Property joinDate : TDatetime Index 88 Read FjoinDate Write SetjoinDate;
    Property kind : string Index 96 Read Fkind Write Setkind;
    Property logoUrl : string Index 104 Read FlogoUrl Write SetlogoUrl;
    Property merchantCenterIds : TAdvertisermerchantCenterIds Index 112 Read FmerchantCenterIds Write SetmerchantCenterIds;
    Property name : string Index 120 Read Fname Write Setname;
    Property payoutRank : string Index 128 Read FpayoutRank Write SetpayoutRank;
    Property productFeedsEnabled : boolean Index 136 Read FproductFeedsEnabled Write SetproductFeedsEnabled;
    Property redirectDomains : TAdvertiserredirectDomains Index 144 Read FredirectDomains Write SetredirectDomains;
    Property siteUrl : string Index 152 Read FsiteUrl Write SetsiteUrl;
    Property status : string Index 160 Read Fstatus Write Setstatus;
  end;
  TAdvertiserClass = Class of TAdvertiser;
  
  { --------------------------------------------------------------------
    TAdvertisermerchantCenterIds
    --------------------------------------------------------------------}
  
  TAdvertisermerchantCenterIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdvertisermerchantCenterIdsClass = Class of TAdvertisermerchantCenterIds;
  
  { --------------------------------------------------------------------
    TAdvertiserredirectDomains
    --------------------------------------------------------------------}
  
  TAdvertiserredirectDomains = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdvertiserredirectDomainsClass = Class of TAdvertiserredirectDomains;
  
  { --------------------------------------------------------------------
    TAdvertisers
    --------------------------------------------------------------------}
  
  TAdvertisers = Class(TGoogleBaseObject)
  Private
    Fitems : TAdvertisersitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAdvertisersitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TAdvertisersitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdvertisersClass = Class of TAdvertisers;
  
  { --------------------------------------------------------------------
    TAdvertisersitems
    --------------------------------------------------------------------}
  
  TAdvertisersitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdvertisersitemsClass = Class of TAdvertisersitems;
  
  { --------------------------------------------------------------------
    TCcOffer
    --------------------------------------------------------------------}
  
  TCcOffer = Class(TGoogleBaseObject)
  Private
    FadditionalCardBenefits : TCcOfferadditionalCardBenefits;
    FadditionalCardHolderFee : string;
    FageMinimum : double;
    FageMinimumDetails : string;
    FannualFee : double;
    FannualFeeDisplay : string;
    FannualRewardMaximum : double;
    FapprovedCategories : TCcOfferapprovedCategories;
    FaprDisplay : string;
    FbalanceComputationMethod : string;
    FbalanceTransferTerms : string;
    FbonusRewards : TCcOfferbonusRewards;
    FcarRentalInsurance : string;
    FcardBenefits : TCcOffercardBenefits;
    FcardName : string;
    FcardType : string;
    FcashAdvanceTerms : string;
    FcreditLimitMax : double;
    FcreditLimitMin : double;
    FcreditRatingDisplay : string;
    FdefaultFees : TCcOfferdefaultFees;
    Fdisclaimer : string;
    FemergencyInsurance : string;
    FexistingCustomerOnly : boolean;
    FextendedWarranty : string;
    FfirstYearAnnualFee : double;
    FflightAccidentInsurance : string;
    FforeignCurrencyTransactionFee : string;
    FfraudLiability : string;
    FgracePeriodDisplay : string;
    FimageUrl : string;
    FinitialSetupAndProcessingFee : string;
    FintroBalanceTransferTerms : string;
    FintroCashAdvanceTerms : string;
    FintroPurchaseTerms : string;
    Fissuer : string;
    FissuerId : string;
    FissuerWebsite : string;
    Fkind : string;
    FlandingPageUrl : string;
    FlatePaymentFee : string;
    FluggageInsurance : string;
    FmaxPurchaseRate : double;
    FminPurchaseRate : double;
    FminimumFinanceCharge : string;
    Fnetwork : string;
    FofferId : string;
    FoffersImmediateCashReward : boolean;
    FoverLimitFee : string;
    FprohibitedCategories : TCcOfferprohibitedCategories;
    FpurchaseRateAdditionalDetails : string;
    FpurchaseRateType : string;
    FreturnedPaymentFee : string;
    FrewardPartner : string;
    FrewardUnit : string;
    Frewards : TCcOfferrewards;
    FrewardsExpire : boolean;
    FrewardsHaveBlackoutDates : boolean;
    FstatementCopyFee : string;
    FtrackingUrl : string;
    FtravelInsurance : string;
    FvariableRatesLastUpdated : string;
    FvariableRatesUpdateFrequency : string;
  Protected
    //Property setters
    Procedure SetadditionalCardBenefits(AIndex : Integer; AValue : TCcOfferadditionalCardBenefits); virtual;
    Procedure SetadditionalCardHolderFee(AIndex : Integer; AValue : string); virtual;
    Procedure SetageMinimum(AIndex : Integer; AValue : double); virtual;
    Procedure SetageMinimumDetails(AIndex : Integer; AValue : string); virtual;
    Procedure SetannualFee(AIndex : Integer; AValue : double); virtual;
    Procedure SetannualFeeDisplay(AIndex : Integer; AValue : string); virtual;
    Procedure SetannualRewardMaximum(AIndex : Integer; AValue : double); virtual;
    Procedure SetapprovedCategories(AIndex : Integer; AValue : TCcOfferapprovedCategories); virtual;
    Procedure SetaprDisplay(AIndex : Integer; AValue : string); virtual;
    Procedure SetbalanceComputationMethod(AIndex : Integer; AValue : string); virtual;
    Procedure SetbalanceTransferTerms(AIndex : Integer; AValue : string); virtual;
    Procedure SetbonusRewards(AIndex : Integer; AValue : TCcOfferbonusRewards); virtual;
    Procedure SetcarRentalInsurance(AIndex : Integer; AValue : string); virtual;
    Procedure SetcardBenefits(AIndex : Integer; AValue : TCcOffercardBenefits); virtual;
    Procedure SetcardName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcardType(AIndex : Integer; AValue : string); virtual;
    Procedure SetcashAdvanceTerms(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreditLimitMax(AIndex : Integer; AValue : double); virtual;
    Procedure SetcreditLimitMin(AIndex : Integer; AValue : double); virtual;
    Procedure SetcreditRatingDisplay(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultFees(AIndex : Integer; AValue : TCcOfferdefaultFees); virtual;
    Procedure Setdisclaimer(AIndex : Integer; AValue : string); virtual;
    Procedure SetemergencyInsurance(AIndex : Integer; AValue : string); virtual;
    Procedure SetexistingCustomerOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetextendedWarranty(AIndex : Integer; AValue : string); virtual;
    Procedure SetfirstYearAnnualFee(AIndex : Integer; AValue : double); virtual;
    Procedure SetflightAccidentInsurance(AIndex : Integer; AValue : string); virtual;
    Procedure SetforeignCurrencyTransactionFee(AIndex : Integer; AValue : string); virtual;
    Procedure SetfraudLiability(AIndex : Integer; AValue : string); virtual;
    Procedure SetgracePeriodDisplay(AIndex : Integer; AValue : string); virtual;
    Procedure SetimageUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetinitialSetupAndProcessingFee(AIndex : Integer; AValue : string); virtual;
    Procedure SetintroBalanceTransferTerms(AIndex : Integer; AValue : string); virtual;
    Procedure SetintroCashAdvanceTerms(AIndex : Integer; AValue : string); virtual;
    Procedure SetintroPurchaseTerms(AIndex : Integer; AValue : string); virtual;
    Procedure Setissuer(AIndex : Integer; AValue : string); virtual;
    Procedure SetissuerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetissuerWebsite(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlandingPageUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetlatePaymentFee(AIndex : Integer; AValue : string); virtual;
    Procedure SetluggageInsurance(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxPurchaseRate(AIndex : Integer; AValue : double); virtual;
    Procedure SetminPurchaseRate(AIndex : Integer; AValue : double); virtual;
    Procedure SetminimumFinanceCharge(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetofferId(AIndex : Integer; AValue : string); virtual;
    Procedure SetoffersImmediateCashReward(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetoverLimitFee(AIndex : Integer; AValue : string); virtual;
    Procedure SetprohibitedCategories(AIndex : Integer; AValue : TCcOfferprohibitedCategories); virtual;
    Procedure SetpurchaseRateAdditionalDetails(AIndex : Integer; AValue : string); virtual;
    Procedure SetpurchaseRateType(AIndex : Integer; AValue : string); virtual;
    Procedure SetreturnedPaymentFee(AIndex : Integer; AValue : string); virtual;
    Procedure SetrewardPartner(AIndex : Integer; AValue : string); virtual;
    Procedure SetrewardUnit(AIndex : Integer; AValue : string); virtual;
    Procedure Setrewards(AIndex : Integer; AValue : TCcOfferrewards); virtual;
    Procedure SetrewardsExpire(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetrewardsHaveBlackoutDates(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstatementCopyFee(AIndex : Integer; AValue : string); virtual;
    Procedure SettrackingUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SettravelInsurance(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariableRatesLastUpdated(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariableRatesUpdateFrequency(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property additionalCardBenefits : TCcOfferadditionalCardBenefits Index 0 Read FadditionalCardBenefits Write SetadditionalCardBenefits;
    Property additionalCardHolderFee : string Index 8 Read FadditionalCardHolderFee Write SetadditionalCardHolderFee;
    Property ageMinimum : double Index 16 Read FageMinimum Write SetageMinimum;
    Property ageMinimumDetails : string Index 24 Read FageMinimumDetails Write SetageMinimumDetails;
    Property annualFee : double Index 32 Read FannualFee Write SetannualFee;
    Property annualFeeDisplay : string Index 40 Read FannualFeeDisplay Write SetannualFeeDisplay;
    Property annualRewardMaximum : double Index 48 Read FannualRewardMaximum Write SetannualRewardMaximum;
    Property approvedCategories : TCcOfferapprovedCategories Index 56 Read FapprovedCategories Write SetapprovedCategories;
    Property aprDisplay : string Index 64 Read FaprDisplay Write SetaprDisplay;
    Property balanceComputationMethod : string Index 72 Read FbalanceComputationMethod Write SetbalanceComputationMethod;
    Property balanceTransferTerms : string Index 80 Read FbalanceTransferTerms Write SetbalanceTransferTerms;
    Property bonusRewards : TCcOfferbonusRewards Index 88 Read FbonusRewards Write SetbonusRewards;
    Property carRentalInsurance : string Index 96 Read FcarRentalInsurance Write SetcarRentalInsurance;
    Property cardBenefits : TCcOffercardBenefits Index 104 Read FcardBenefits Write SetcardBenefits;
    Property cardName : string Index 112 Read FcardName Write SetcardName;
    Property cardType : string Index 120 Read FcardType Write SetcardType;
    Property cashAdvanceTerms : string Index 128 Read FcashAdvanceTerms Write SetcashAdvanceTerms;
    Property creditLimitMax : double Index 136 Read FcreditLimitMax Write SetcreditLimitMax;
    Property creditLimitMin : double Index 144 Read FcreditLimitMin Write SetcreditLimitMin;
    Property creditRatingDisplay : string Index 152 Read FcreditRatingDisplay Write SetcreditRatingDisplay;
    Property defaultFees : TCcOfferdefaultFees Index 160 Read FdefaultFees Write SetdefaultFees;
    Property disclaimer : string Index 168 Read Fdisclaimer Write Setdisclaimer;
    Property emergencyInsurance : string Index 176 Read FemergencyInsurance Write SetemergencyInsurance;
    Property existingCustomerOnly : boolean Index 184 Read FexistingCustomerOnly Write SetexistingCustomerOnly;
    Property extendedWarranty : string Index 192 Read FextendedWarranty Write SetextendedWarranty;
    Property firstYearAnnualFee : double Index 200 Read FfirstYearAnnualFee Write SetfirstYearAnnualFee;
    Property flightAccidentInsurance : string Index 208 Read FflightAccidentInsurance Write SetflightAccidentInsurance;
    Property foreignCurrencyTransactionFee : string Index 216 Read FforeignCurrencyTransactionFee Write SetforeignCurrencyTransactionFee;
    Property fraudLiability : string Index 224 Read FfraudLiability Write SetfraudLiability;
    Property gracePeriodDisplay : string Index 232 Read FgracePeriodDisplay Write SetgracePeriodDisplay;
    Property imageUrl : string Index 240 Read FimageUrl Write SetimageUrl;
    Property initialSetupAndProcessingFee : string Index 248 Read FinitialSetupAndProcessingFee Write SetinitialSetupAndProcessingFee;
    Property introBalanceTransferTerms : string Index 256 Read FintroBalanceTransferTerms Write SetintroBalanceTransferTerms;
    Property introCashAdvanceTerms : string Index 264 Read FintroCashAdvanceTerms Write SetintroCashAdvanceTerms;
    Property introPurchaseTerms : string Index 272 Read FintroPurchaseTerms Write SetintroPurchaseTerms;
    Property issuer : string Index 280 Read Fissuer Write Setissuer;
    Property issuerId : string Index 288 Read FissuerId Write SetissuerId;
    Property issuerWebsite : string Index 296 Read FissuerWebsite Write SetissuerWebsite;
    Property kind : string Index 304 Read Fkind Write Setkind;
    Property landingPageUrl : string Index 312 Read FlandingPageUrl Write SetlandingPageUrl;
    Property latePaymentFee : string Index 320 Read FlatePaymentFee Write SetlatePaymentFee;
    Property luggageInsurance : string Index 328 Read FluggageInsurance Write SetluggageInsurance;
    Property maxPurchaseRate : double Index 336 Read FmaxPurchaseRate Write SetmaxPurchaseRate;
    Property minPurchaseRate : double Index 344 Read FminPurchaseRate Write SetminPurchaseRate;
    Property minimumFinanceCharge : string Index 352 Read FminimumFinanceCharge Write SetminimumFinanceCharge;
    Property network : string Index 360 Read Fnetwork Write Setnetwork;
    Property offerId : string Index 368 Read FofferId Write SetofferId;
    Property offersImmediateCashReward : boolean Index 376 Read FoffersImmediateCashReward Write SetoffersImmediateCashReward;
    Property overLimitFee : string Index 384 Read FoverLimitFee Write SetoverLimitFee;
    Property prohibitedCategories : TCcOfferprohibitedCategories Index 392 Read FprohibitedCategories Write SetprohibitedCategories;
    Property purchaseRateAdditionalDetails : string Index 400 Read FpurchaseRateAdditionalDetails Write SetpurchaseRateAdditionalDetails;
    Property purchaseRateType : string Index 408 Read FpurchaseRateType Write SetpurchaseRateType;
    Property returnedPaymentFee : string Index 416 Read FreturnedPaymentFee Write SetreturnedPaymentFee;
    Property rewardPartner : string Index 424 Read FrewardPartner Write SetrewardPartner;
    Property rewardUnit : string Index 432 Read FrewardUnit Write SetrewardUnit;
    Property rewards : TCcOfferrewards Index 440 Read Frewards Write Setrewards;
    Property rewardsExpire : boolean Index 448 Read FrewardsExpire Write SetrewardsExpire;
    Property rewardsHaveBlackoutDates : boolean Index 456 Read FrewardsHaveBlackoutDates Write SetrewardsHaveBlackoutDates;
    Property statementCopyFee : string Index 464 Read FstatementCopyFee Write SetstatementCopyFee;
    Property trackingUrl : string Index 472 Read FtrackingUrl Write SettrackingUrl;
    Property travelInsurance : string Index 480 Read FtravelInsurance Write SettravelInsurance;
    Property variableRatesLastUpdated : string Index 488 Read FvariableRatesLastUpdated Write SetvariableRatesLastUpdated;
    Property variableRatesUpdateFrequency : string Index 496 Read FvariableRatesUpdateFrequency Write SetvariableRatesUpdateFrequency;
  end;
  TCcOfferClass = Class of TCcOffer;
  
  { --------------------------------------------------------------------
    TCcOfferadditionalCardBenefits
    --------------------------------------------------------------------}
  
  TCcOfferadditionalCardBenefits = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCcOfferadditionalCardBenefitsClass = Class of TCcOfferadditionalCardBenefits;
  
  { --------------------------------------------------------------------
    TCcOfferapprovedCategories
    --------------------------------------------------------------------}
  
  TCcOfferapprovedCategories = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCcOfferapprovedCategoriesClass = Class of TCcOfferapprovedCategories;
  
  { --------------------------------------------------------------------
    TCcOfferbonusRewards
    --------------------------------------------------------------------}
  
  TCcOfferbonusRewards = Class(TGoogleBaseObject)
  Private
    Famount : double;
    Fdetails : string;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; AValue : double); virtual;
    Procedure Setdetails(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property amount : double Index 0 Read Famount Write Setamount;
    Property details : string Index 8 Read Fdetails Write Setdetails;
  end;
  TCcOfferbonusRewardsClass = Class of TCcOfferbonusRewards;
  
  { --------------------------------------------------------------------
    TCcOffercardBenefits
    --------------------------------------------------------------------}
  
  TCcOffercardBenefits = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCcOffercardBenefitsClass = Class of TCcOffercardBenefits;
  
  { --------------------------------------------------------------------
    TCcOfferdefaultFees
    --------------------------------------------------------------------}
  
  TCcOfferdefaultFees = Class(TGoogleBaseObject)
  Private
    Fcategory : string;
    FmaxRate : double;
    FminRate : double;
    FrateType : string;
  Protected
    //Property setters
    Procedure Setcategory(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxRate(AIndex : Integer; AValue : double); virtual;
    Procedure SetminRate(AIndex : Integer; AValue : double); virtual;
    Procedure SetrateType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property category : string Index 0 Read Fcategory Write Setcategory;
    Property maxRate : double Index 8 Read FmaxRate Write SetmaxRate;
    Property minRate : double Index 16 Read FminRate Write SetminRate;
    Property rateType : string Index 24 Read FrateType Write SetrateType;
  end;
  TCcOfferdefaultFeesClass = Class of TCcOfferdefaultFees;
  
  { --------------------------------------------------------------------
    TCcOfferprohibitedCategories
    --------------------------------------------------------------------}
  
  TCcOfferprohibitedCategories = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCcOfferprohibitedCategoriesClass = Class of TCcOfferprohibitedCategories;
  
  { --------------------------------------------------------------------
    TCcOfferrewards
    --------------------------------------------------------------------}
  
  TCcOfferrewards = Class(TGoogleBaseObject)
  Private
    FadditionalDetails : string;
    Famount : double;
    Fcategory : string;
    FexpirationMonths : double;
    FmaxRewardTier : double;
    FminRewardTier : double;
  Protected
    //Property setters
    Procedure SetadditionalDetails(AIndex : Integer; AValue : string); virtual;
    Procedure Setamount(AIndex : Integer; AValue : double); virtual;
    Procedure Setcategory(AIndex : Integer; AValue : string); virtual;
    Procedure SetexpirationMonths(AIndex : Integer; AValue : double); virtual;
    Procedure SetmaxRewardTier(AIndex : Integer; AValue : double); virtual;
    Procedure SetminRewardTier(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property additionalDetails : string Index 0 Read FadditionalDetails Write SetadditionalDetails;
    Property amount : double Index 8 Read Famount Write Setamount;
    Property category : string Index 16 Read Fcategory Write Setcategory;
    Property expirationMonths : double Index 24 Read FexpirationMonths Write SetexpirationMonths;
    Property maxRewardTier : double Index 32 Read FmaxRewardTier Write SetmaxRewardTier;
    Property minRewardTier : double Index 40 Read FminRewardTier Write SetminRewardTier;
  end;
  TCcOfferrewardsClass = Class of TCcOfferrewards;
  
  { --------------------------------------------------------------------
    TCcOffers
    --------------------------------------------------------------------}
  
  TCcOffers = Class(TGoogleBaseObject)
  Private
    Fitems : TCcOffersitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCcOffersitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCcOffersitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TCcOffersClass = Class of TCcOffers;
  
  { --------------------------------------------------------------------
    TCcOffersitems
    --------------------------------------------------------------------}
  
  TCcOffersitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCcOffersitemsClass = Class of TCcOffersitems;
  
  { --------------------------------------------------------------------
    TEvent
    --------------------------------------------------------------------}
  
  TEvent = Class(TGoogleBaseObject)
  Private
    FadvertiserId : string;
    FadvertiserName : string;
    FchargeId : string;
    FchargeType : string;
    FcommissionableSales : TMoney;
    Fearnings : TMoney;
    FeventDate : TDatetime;
    Fkind : string;
    FmemberId : string;
    FmodifyDate : TDatetime;
    FnetworkFee : TMoney;
    ForderId : string;
    Fproducts : TEventproducts;
    FpublisherFee : TMoney;
    FpublisherId : string;
    FpublisherName : string;
    Fstatus : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetadvertiserId(AIndex : Integer; AValue : string); virtual;
    Procedure SetadvertiserName(AIndex : Integer; AValue : string); virtual;
    Procedure SetchargeId(AIndex : Integer; AValue : string); virtual;
    Procedure SetchargeType(AIndex : Integer; AValue : string); virtual;
    Procedure SetcommissionableSales(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setearnings(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SeteventDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmemberId(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodifyDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetnetworkFee(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetorderId(AIndex : Integer; AValue : string); virtual;
    Procedure Setproducts(AIndex : Integer; AValue : TEventproducts); virtual;
    Procedure SetpublisherFee(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpublisherId(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublisherName(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property advertiserId : string Index 0 Read FadvertiserId Write SetadvertiserId;
    Property advertiserName : string Index 8 Read FadvertiserName Write SetadvertiserName;
    Property chargeId : string Index 16 Read FchargeId Write SetchargeId;
    Property chargeType : string Index 24 Read FchargeType Write SetchargeType;
    Property commissionableSales : TMoney Index 32 Read FcommissionableSales Write SetcommissionableSales;
    Property earnings : TMoney Index 40 Read Fearnings Write Setearnings;
    Property eventDate : TDatetime Index 48 Read FeventDate Write SeteventDate;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property memberId : string Index 64 Read FmemberId Write SetmemberId;
    Property modifyDate : TDatetime Index 72 Read FmodifyDate Write SetmodifyDate;
    Property networkFee : TMoney Index 80 Read FnetworkFee Write SetnetworkFee;
    Property orderId : string Index 88 Read ForderId Write SetorderId;
    Property products : TEventproducts Index 96 Read Fproducts Write Setproducts;
    Property publisherFee : TMoney Index 104 Read FpublisherFee Write SetpublisherFee;
    Property publisherId : string Index 112 Read FpublisherId Write SetpublisherId;
    Property publisherName : string Index 120 Read FpublisherName Write SetpublisherName;
    Property status : string Index 128 Read Fstatus Write Setstatus;
    Property _type : string Index 136 Read F_type Write Set_type;
  end;
  TEventClass = Class of TEvent;
  
  { --------------------------------------------------------------------
    TEventproducts
    --------------------------------------------------------------------}
  
  TEventproducts = Class(TGoogleBaseObject)
  Private
    FcategoryId : string;
    FcategoryName : string;
    Fearnings : TMoney;
    FnetworkFee : TMoney;
    FpublisherFee : TMoney;
    Fquantity : string;
    Fsku : string;
    FskuName : string;
    FunitPrice : TMoney;
  Protected
    //Property setters
    Procedure SetcategoryId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcategoryName(AIndex : Integer; AValue : string); virtual;
    Procedure Setearnings(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetnetworkFee(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpublisherFee(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setquantity(AIndex : Integer; AValue : string); virtual;
    Procedure Setsku(AIndex : Integer; AValue : string); virtual;
    Procedure SetskuName(AIndex : Integer; AValue : string); virtual;
    Procedure SetunitPrice(AIndex : Integer; AValue : TMoney); virtual;
  Public
  Published
    Property categoryId : string Index 0 Read FcategoryId Write SetcategoryId;
    Property categoryName : string Index 8 Read FcategoryName Write SetcategoryName;
    Property earnings : TMoney Index 16 Read Fearnings Write Setearnings;
    Property networkFee : TMoney Index 24 Read FnetworkFee Write SetnetworkFee;
    Property publisherFee : TMoney Index 32 Read FpublisherFee Write SetpublisherFee;
    Property quantity : string Index 40 Read Fquantity Write Setquantity;
    Property sku : string Index 48 Read Fsku Write Setsku;
    Property skuName : string Index 56 Read FskuName Write SetskuName;
    Property unitPrice : TMoney Index 64 Read FunitPrice Write SetunitPrice;
  end;
  TEventproductsClass = Class of TEventproducts;
  
  { --------------------------------------------------------------------
    TEvents
    --------------------------------------------------------------------}
  
  TEvents = Class(TGoogleBaseObject)
  Private
    Fitems : TEventsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TEventsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TEventsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TEventsClass = Class of TEvents;
  
  { --------------------------------------------------------------------
    TEventsitems
    --------------------------------------------------------------------}
  
  TEventsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventsitemsClass = Class of TEventsitems;
  
  { --------------------------------------------------------------------
    TLink
    --------------------------------------------------------------------}
  
  TLink = Class(TGoogleBaseObject)
  Private
    FadvertiserId : string;
    Fauthorship : string;
    Favailability : string;
    FclickTrackingUrl : string;
    FcreateDate : TDatetime;
    Fdescription : string;
    FdestinationUrl : string;
    Fduration : string;
    FendDate : TDatetime;
    FepcNinetyDayAverage : TMoney;
    FepcSevenDayAverage : TMoney;
    Fid : string;
    FimageAltText : string;
    FimpressionTrackingUrl : string;
    FisActive : boolean;
    Fkind : string;
    FlinkType : string;
    Fname : string;
    FpromotionType : string;
    FspecialOffers : TLinkspecialOffers;
    FstartDate : TDatetime;
  Protected
    //Property setters
    Procedure SetadvertiserId(AIndex : Integer; AValue : string); virtual;
    Procedure Setauthorship(AIndex : Integer; AValue : string); virtual;
    Procedure Setavailability(AIndex : Integer; AValue : string); virtual;
    Procedure SetclickTrackingUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreateDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdestinationUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setduration(AIndex : Integer; AValue : string); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetimageAltText(AIndex : Integer; AValue : string); virtual;
    Procedure SetimpressionTrackingUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetisActive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlinkType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpromotionType(AIndex : Integer; AValue : string); virtual;
    Procedure SetspecialOffers(AIndex : Integer; AValue : TLinkspecialOffers); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property advertiserId : string Index 0 Read FadvertiserId Write SetadvertiserId;
    Property authorship : string Index 8 Read Fauthorship Write Setauthorship;
    Property availability : string Index 16 Read Favailability Write Setavailability;
    Property clickTrackingUrl : string Index 24 Read FclickTrackingUrl Write SetclickTrackingUrl;
    Property createDate : TDatetime Index 32 Read FcreateDate Write SetcreateDate;
    Property description : string Index 40 Read Fdescription Write Setdescription;
    Property destinationUrl : string Index 48 Read FdestinationUrl Write SetdestinationUrl;
    Property duration : string Index 56 Read Fduration Write Setduration;
    Property endDate : TDatetime Index 64 Read FendDate Write SetendDate;
    Property epcNinetyDayAverage : TMoney Index 72 Read FepcNinetyDayAverage Write SetepcNinetyDayAverage;
    Property epcSevenDayAverage : TMoney Index 80 Read FepcSevenDayAverage Write SetepcSevenDayAverage;
    Property id : string Index 88 Read Fid Write Setid;
    Property imageAltText : string Index 96 Read FimageAltText Write SetimageAltText;
    Property impressionTrackingUrl : string Index 104 Read FimpressionTrackingUrl Write SetimpressionTrackingUrl;
    Property isActive : boolean Index 112 Read FisActive Write SetisActive;
    Property kind : string Index 120 Read Fkind Write Setkind;
    Property linkType : string Index 128 Read FlinkType Write SetlinkType;
    Property name : string Index 136 Read Fname Write Setname;
    Property promotionType : string Index 144 Read FpromotionType Write SetpromotionType;
    Property specialOffers : TLinkspecialOffers Index 152 Read FspecialOffers Write SetspecialOffers;
    Property startDate : TDatetime Index 160 Read FstartDate Write SetstartDate;
  end;
  TLinkClass = Class of TLink;
  
  { --------------------------------------------------------------------
    TLinkspecialOffers
    --------------------------------------------------------------------}
  
  TLinkspecialOffers = Class(TGoogleBaseObject)
  Private
    FfreeGift : boolean;
    FfreeShipping : boolean;
    FfreeShippingMin : TMoney;
    FpercentOff : double;
    FpercentOffMin : TMoney;
    FpriceCut : TMoney;
    FpriceCutMin : TMoney;
    FpromotionCodes : TLinkspecialOfferspromotionCodes;
  Protected
    //Property setters
    Procedure SetfreeGift(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfreeShipping(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfreeShippingMin(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpercentOff(AIndex : Integer; AValue : double); virtual;
    Procedure SetpercentOffMin(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpriceCut(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpriceCutMin(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetpromotionCodes(AIndex : Integer; AValue : TLinkspecialOfferspromotionCodes); virtual;
  Public
  Published
    Property freeGift : boolean Index 0 Read FfreeGift Write SetfreeGift;
    Property freeShipping : boolean Index 8 Read FfreeShipping Write SetfreeShipping;
    Property freeShippingMin : TMoney Index 16 Read FfreeShippingMin Write SetfreeShippingMin;
    Property percentOff : double Index 24 Read FpercentOff Write SetpercentOff;
    Property percentOffMin : TMoney Index 32 Read FpercentOffMin Write SetpercentOffMin;
    Property priceCut : TMoney Index 40 Read FpriceCut Write SetpriceCut;
    Property priceCutMin : TMoney Index 48 Read FpriceCutMin Write SetpriceCutMin;
    Property promotionCodes : TLinkspecialOfferspromotionCodes Index 56 Read FpromotionCodes Write SetpromotionCodes;
  end;
  TLinkspecialOffersClass = Class of TLinkspecialOffers;
  
  { --------------------------------------------------------------------
    TLinkspecialOfferspromotionCodes
    --------------------------------------------------------------------}
  
  TLinkspecialOfferspromotionCodes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLinkspecialOfferspromotionCodesClass = Class of TLinkspecialOfferspromotionCodes;
  
  { --------------------------------------------------------------------
    TLinks
    --------------------------------------------------------------------}
  
  TLinks = Class(TGoogleBaseObject)
  Private
    Fitems : TLinksitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLinksitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TLinksitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TLinksClass = Class of TLinks;
  
  { --------------------------------------------------------------------
    TLinksitems
    --------------------------------------------------------------------}
  
  TLinksitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLinksitemsClass = Class of TLinksitems;
  
  { --------------------------------------------------------------------
    TMoney
    --------------------------------------------------------------------}
  
  TMoney = Class(TGoogleBaseObject)
  Private
    Famount : double;
    FcurrencyCode : string;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property amount : double Index 0 Read Famount Write Setamount;
    Property currencyCode : string Index 8 Read FcurrencyCode Write SetcurrencyCode;
  end;
  TMoneyClass = Class of TMoney;
  
  { --------------------------------------------------------------------
    TPublisher
    --------------------------------------------------------------------}
  
  TPublisher = Class(TGoogleBaseObject)
  Private
    Fclassification : string;
    FepcNinetyDayAverage : TMoney;
    FepcSevenDayAverage : TMoney;
    Fid : string;
    Fitem : TPublisher;
    FjoinDate : TDatetime;
    Fkind : string;
    Fname : string;
    FpayoutRank : string;
    Fsites : TPublishersites;
    Fstatus : string;
  Protected
    //Property setters
    Procedure Setclassification(AIndex : Integer; AValue : string); virtual;
    Procedure SetepcNinetyDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure SetepcSevenDayAverage(AIndex : Integer; AValue : TMoney); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitem(AIndex : Integer; AValue : TPublisher); virtual;
    Procedure SetjoinDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpayoutRank(AIndex : Integer; AValue : string); virtual;
    Procedure Setsites(AIndex : Integer; AValue : TPublishersites); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property classification : string Index 0 Read Fclassification Write Setclassification;
    Property epcNinetyDayAverage : TMoney Index 8 Read FepcNinetyDayAverage Write SetepcNinetyDayAverage;
    Property epcSevenDayAverage : TMoney Index 16 Read FepcSevenDayAverage Write SetepcSevenDayAverage;
    Property id : string Index 24 Read Fid Write Setid;
    Property item : TPublisher Index 32 Read Fitem Write Setitem;
    Property joinDate : TDatetime Index 40 Read FjoinDate Write SetjoinDate;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property payoutRank : string Index 64 Read FpayoutRank Write SetpayoutRank;
    Property sites : TPublishersites Index 72 Read Fsites Write Setsites;
    Property status : string Index 80 Read Fstatus Write Setstatus;
  end;
  TPublisherClass = Class of TPublisher;
  
  { --------------------------------------------------------------------
    TPublishersites
    --------------------------------------------------------------------}
  
  TPublishersites = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPublishersitesClass = Class of TPublishersites;
  
  { --------------------------------------------------------------------
    TPublishers
    --------------------------------------------------------------------}
  
  TPublishers = Class(TGoogleBaseObject)
  Private
    Fitems : TPublishersitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPublishersitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TPublishersitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TPublishersClass = Class of TPublishers;
  
  { --------------------------------------------------------------------
    TPublishersitems
    --------------------------------------------------------------------}
  
  TPublishersitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPublishersitemsClass = Class of TPublishersitems;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    Fcolumn_names : TReportcolumn_names;
    Fend_date : string;
    Fkind : string;
    Fmatching_row_count : string;
    Frows : TReportrows;
    Fstart_date : string;
    Ftotals_rows : TReporttotals_rows;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolumn_names(AIndex : Integer; AValue : TReportcolumn_names); virtual;
    Procedure Setend_date(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmatching_row_count(AIndex : Integer; AValue : string); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TReportrows); virtual;
    Procedure Setstart_date(AIndex : Integer; AValue : string); virtual;
    Procedure Settotals_rows(AIndex : Integer; AValue : TReporttotals_rows); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property column_names : TReportcolumn_names Index 0 Read Fcolumn_names Write Setcolumn_names;
    Property end_date : string Index 8 Read Fend_date Write Setend_date;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property matching_row_count : string Index 24 Read Fmatching_row_count Write Setmatching_row_count;
    Property rows : TReportrows Index 32 Read Frows Write Setrows;
    Property start_date : string Index 40 Read Fstart_date Write Setstart_date;
    Property totals_rows : TReporttotals_rows Index 48 Read Ftotals_rows Write Settotals_rows;
    Property _type : string Index 56 Read F_type Write Set_type;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TReportcolumn_names
    --------------------------------------------------------------------}
  
  TReportcolumn_names = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportcolumn_namesClass = Class of TReportcolumn_names;
  
  { --------------------------------------------------------------------
    TReportrows
    --------------------------------------------------------------------}
  
  TReportrows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportrowsClass = Class of TReportrows;
  
  { --------------------------------------------------------------------
    TReporttotals_rows
    --------------------------------------------------------------------}
  
  TReporttotals_rows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReporttotals_rowsClass = Class of TReporttotals_rows;
  
  { --------------------------------------------------------------------
    TAdvertisersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdvertisersResource, method Get
  
  TAdvertisersGetOptions = Record
    advertiserId : string;
  end;
  
  
  //Optional query Options for TAdvertisersResource, method List
  
  TAdvertisersListOptions = Record
    advertiserCategory : string;
    maxResults : integer;
    minNinetyDayEpc : double;
    minPayoutRank : integer;
    minSevenDayEpc : double;
    pageToken : string;
    relationshipStatus : string;
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
    advertiser : string;
    projection : string;
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
    advertiserId : string;
    chargeType : string;
    eventDateMax : string;
    eventDateMin : string;
    linkId : string;
    maxResults : integer;
    memberId : string;
    modifyDateMax : string;
    modifyDateMin : string;
    orderId : string;
    pageToken : string;
    productCategory : string;
    publisherId : string;
    sku : string;
    status : string;
    _type : string;
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
    assetSize : string;
    authorship : string;
    createDateMax : string;
    createDateMin : string;
    linkType : string;
    maxResults : integer;
    pageToken : string;
    promotionType : string;
    relationshipStatus : string;
    searchText : string;
    startDateMax : string;
    startDateMin : string;
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
    publisherId : string;
  end;
  
  
  //Optional query Options for TPublishersResource, method List
  
  TPublishersListOptions = Record
    maxResults : integer;
    minNinetyDayEpc : double;
    minPayoutRank : integer;
    minSevenDayEpc : double;
    pageToken : string;
    publisherCategory : string;
    relationshipStatus : string;
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
    advertiserId : string;
    calculateTotals : boolean;
    endDate : string;
    eventType : string;
    linkId : string;
    maxResults : integer;
    orderId : string;
    publisherId : string;
    startDate : string;
    startIndex : integer;
    status : string;
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



Procedure TAdvertiser.Setcategory(AIndex : Integer; AValue : string); 

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



Procedure TAdvertiser.SetcontactEmail(AIndex : Integer; AValue : string); 

begin
  If (FcontactEmail=AValue) then exit;
  FcontactEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetcontactPhone(AIndex : Integer; AValue : string); 

begin
  If (FcontactPhone=AValue) then exit;
  FcontactPhone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetdefaultLinkId(AIndex : Integer; AValue : string); 

begin
  If (FdefaultLinkId=AValue) then exit;
  FdefaultLinkId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setdescription(AIndex : Integer; AValue : string); 

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



Procedure TAdvertiser.Setid(AIndex : Integer; AValue : string); 

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



Procedure TAdvertiser.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetlogoUrl(AIndex : Integer; AValue : string); 

begin
  If (FlogoUrl=AValue) then exit;
  FlogoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetmerchantCenterIds(AIndex : Integer; AValue : TAdvertisermerchantCenterIds); 

begin
  If (FmerchantCenterIds=AValue) then exit;
  FmerchantCenterIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetpayoutRank(AIndex : Integer; AValue : string); 

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



Procedure TAdvertiser.SetredirectDomains(AIndex : Integer; AValue : TAdvertiserredirectDomains); 

begin
  If (FredirectDomains=AValue) then exit;
  FredirectDomains:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.SetsiteUrl(AIndex : Integer; AValue : string); 

begin
  If (FsiteUrl=AValue) then exit;
  FsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertiser.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdvertisermerchantCenterIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdvertiserredirectDomains
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdvertisers
  --------------------------------------------------------------------}


Procedure TAdvertisers.Setitems(AIndex : Integer; AValue : TAdvertisersitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertisers.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertisers.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdvertisersitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCcOffer
  --------------------------------------------------------------------}


Procedure TCcOffer.SetadditionalCardBenefits(AIndex : Integer; AValue : TCcOfferadditionalCardBenefits); 

begin
  If (FadditionalCardBenefits=AValue) then exit;
  FadditionalCardBenefits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetadditionalCardHolderFee(AIndex : Integer; AValue : string); 

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



Procedure TCcOffer.SetageMinimumDetails(AIndex : Integer; AValue : string); 

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



Procedure TCcOffer.SetannualFeeDisplay(AIndex : Integer; AValue : string); 

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



Procedure TCcOffer.SetapprovedCategories(AIndex : Integer; AValue : TCcOfferapprovedCategories); 

begin
  If (FapprovedCategories=AValue) then exit;
  FapprovedCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetaprDisplay(AIndex : Integer; AValue : string); 

begin
  If (FaprDisplay=AValue) then exit;
  FaprDisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetbalanceComputationMethod(AIndex : Integer; AValue : string); 

begin
  If (FbalanceComputationMethod=AValue) then exit;
  FbalanceComputationMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetbalanceTransferTerms(AIndex : Integer; AValue : string); 

begin
  If (FbalanceTransferTerms=AValue) then exit;
  FbalanceTransferTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetbonusRewards(AIndex : Integer; AValue : TCcOfferbonusRewards); 

begin
  If (FbonusRewards=AValue) then exit;
  FbonusRewards:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcarRentalInsurance(AIndex : Integer; AValue : string); 

begin
  If (FcarRentalInsurance=AValue) then exit;
  FcarRentalInsurance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcardBenefits(AIndex : Integer; AValue : TCcOffercardBenefits); 

begin
  If (FcardBenefits=AValue) then exit;
  FcardBenefits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcardName(AIndex : Integer; AValue : string); 

begin
  If (FcardName=AValue) then exit;
  FcardName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcardType(AIndex : Integer; AValue : string); 

begin
  If (FcardType=AValue) then exit;
  FcardType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetcashAdvanceTerms(AIndex : Integer; AValue : string); 

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



Procedure TCcOffer.SetcreditRatingDisplay(AIndex : Integer; AValue : string); 

begin
  If (FcreditRatingDisplay=AValue) then exit;
  FcreditRatingDisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetdefaultFees(AIndex : Integer; AValue : TCcOfferdefaultFees); 

begin
  If (FdefaultFees=AValue) then exit;
  FdefaultFees:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setdisclaimer(AIndex : Integer; AValue : string); 

begin
  If (Fdisclaimer=AValue) then exit;
  Fdisclaimer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetemergencyInsurance(AIndex : Integer; AValue : string); 

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



Procedure TCcOffer.SetextendedWarranty(AIndex : Integer; AValue : string); 

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



Procedure TCcOffer.SetflightAccidentInsurance(AIndex : Integer; AValue : string); 

begin
  If (FflightAccidentInsurance=AValue) then exit;
  FflightAccidentInsurance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetforeignCurrencyTransactionFee(AIndex : Integer; AValue : string); 

begin
  If (FforeignCurrencyTransactionFee=AValue) then exit;
  FforeignCurrencyTransactionFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetfraudLiability(AIndex : Integer; AValue : string); 

begin
  If (FfraudLiability=AValue) then exit;
  FfraudLiability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetgracePeriodDisplay(AIndex : Integer; AValue : string); 

begin
  If (FgracePeriodDisplay=AValue) then exit;
  FgracePeriodDisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetimageUrl(AIndex : Integer; AValue : string); 

begin
  If (FimageUrl=AValue) then exit;
  FimageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetinitialSetupAndProcessingFee(AIndex : Integer; AValue : string); 

begin
  If (FinitialSetupAndProcessingFee=AValue) then exit;
  FinitialSetupAndProcessingFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetintroBalanceTransferTerms(AIndex : Integer; AValue : string); 

begin
  If (FintroBalanceTransferTerms=AValue) then exit;
  FintroBalanceTransferTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetintroCashAdvanceTerms(AIndex : Integer; AValue : string); 

begin
  If (FintroCashAdvanceTerms=AValue) then exit;
  FintroCashAdvanceTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetintroPurchaseTerms(AIndex : Integer; AValue : string); 

begin
  If (FintroPurchaseTerms=AValue) then exit;
  FintroPurchaseTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setissuer(AIndex : Integer; AValue : string); 

begin
  If (Fissuer=AValue) then exit;
  Fissuer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetissuerId(AIndex : Integer; AValue : string); 

begin
  If (FissuerId=AValue) then exit;
  FissuerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetissuerWebsite(AIndex : Integer; AValue : string); 

begin
  If (FissuerWebsite=AValue) then exit;
  FissuerWebsite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetlandingPageUrl(AIndex : Integer; AValue : string); 

begin
  If (FlandingPageUrl=AValue) then exit;
  FlandingPageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetlatePaymentFee(AIndex : Integer; AValue : string); 

begin
  If (FlatePaymentFee=AValue) then exit;
  FlatePaymentFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetluggageInsurance(AIndex : Integer; AValue : string); 

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



Procedure TCcOffer.SetminimumFinanceCharge(AIndex : Integer; AValue : string); 

begin
  If (FminimumFinanceCharge=AValue) then exit;
  FminimumFinanceCharge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetofferId(AIndex : Integer; AValue : string); 

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



Procedure TCcOffer.SetoverLimitFee(AIndex : Integer; AValue : string); 

begin
  If (FoverLimitFee=AValue) then exit;
  FoverLimitFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetprohibitedCategories(AIndex : Integer; AValue : TCcOfferprohibitedCategories); 

begin
  If (FprohibitedCategories=AValue) then exit;
  FprohibitedCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetpurchaseRateAdditionalDetails(AIndex : Integer; AValue : string); 

begin
  If (FpurchaseRateAdditionalDetails=AValue) then exit;
  FpurchaseRateAdditionalDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetpurchaseRateType(AIndex : Integer; AValue : string); 

begin
  If (FpurchaseRateType=AValue) then exit;
  FpurchaseRateType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetreturnedPaymentFee(AIndex : Integer; AValue : string); 

begin
  If (FreturnedPaymentFee=AValue) then exit;
  FreturnedPaymentFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetrewardPartner(AIndex : Integer; AValue : string); 

begin
  If (FrewardPartner=AValue) then exit;
  FrewardPartner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetrewardUnit(AIndex : Integer; AValue : string); 

begin
  If (FrewardUnit=AValue) then exit;
  FrewardUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.Setrewards(AIndex : Integer; AValue : TCcOfferrewards); 

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



Procedure TCcOffer.SetstatementCopyFee(AIndex : Integer; AValue : string); 

begin
  If (FstatementCopyFee=AValue) then exit;
  FstatementCopyFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SettrackingUrl(AIndex : Integer; AValue : string); 

begin
  If (FtrackingUrl=AValue) then exit;
  FtrackingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SettravelInsurance(AIndex : Integer; AValue : string); 

begin
  If (FtravelInsurance=AValue) then exit;
  FtravelInsurance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetvariableRatesLastUpdated(AIndex : Integer; AValue : string); 

begin
  If (FvariableRatesLastUpdated=AValue) then exit;
  FvariableRatesLastUpdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffer.SetvariableRatesUpdateFrequency(AIndex : Integer; AValue : string); 

begin
  If (FvariableRatesUpdateFrequency=AValue) then exit;
  FvariableRatesUpdateFrequency:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCcOfferadditionalCardBenefits
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCcOfferapprovedCategories
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCcOfferbonusRewards
  --------------------------------------------------------------------}


Procedure TCcOfferbonusRewards.Setamount(AIndex : Integer; AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferbonusRewards.Setdetails(AIndex : Integer; AValue : string); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCcOffercardBenefits
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCcOfferdefaultFees
  --------------------------------------------------------------------}


Procedure TCcOfferdefaultFees.Setcategory(AIndex : Integer; AValue : string); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferdefaultFees.SetmaxRate(AIndex : Integer; AValue : double); 

begin
  If (FmaxRate=AValue) then exit;
  FmaxRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferdefaultFees.SetminRate(AIndex : Integer; AValue : double); 

begin
  If (FminRate=AValue) then exit;
  FminRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferdefaultFees.SetrateType(AIndex : Integer; AValue : string); 

begin
  If (FrateType=AValue) then exit;
  FrateType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCcOfferprohibitedCategories
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCcOfferrewards
  --------------------------------------------------------------------}


Procedure TCcOfferrewards.SetadditionalDetails(AIndex : Integer; AValue : string); 

begin
  If (FadditionalDetails=AValue) then exit;
  FadditionalDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferrewards.Setamount(AIndex : Integer; AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferrewards.Setcategory(AIndex : Integer; AValue : string); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferrewards.SetexpirationMonths(AIndex : Integer; AValue : double); 

begin
  If (FexpirationMonths=AValue) then exit;
  FexpirationMonths:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferrewards.SetmaxRewardTier(AIndex : Integer; AValue : double); 

begin
  If (FmaxRewardTier=AValue) then exit;
  FmaxRewardTier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOfferrewards.SetminRewardTier(AIndex : Integer; AValue : double); 

begin
  If (FminRewardTier=AValue) then exit;
  FminRewardTier:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCcOffers
  --------------------------------------------------------------------}


Procedure TCcOffers.Setitems(AIndex : Integer; AValue : TCcOffersitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCcOffers.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCcOffersitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEvent
  --------------------------------------------------------------------}


Procedure TEvent.SetadvertiserId(AIndex : Integer; AValue : string); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetadvertiserName(AIndex : Integer; AValue : string); 

begin
  If (FadvertiserName=AValue) then exit;
  FadvertiserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetchargeId(AIndex : Integer; AValue : string); 

begin
  If (FchargeId=AValue) then exit;
  FchargeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetchargeType(AIndex : Integer; AValue : string); 

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



Procedure TEvent.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetmemberId(AIndex : Integer; AValue : string); 

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



Procedure TEvent.SetorderId(AIndex : Integer; AValue : string); 

begin
  If (ForderId=AValue) then exit;
  ForderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setproducts(AIndex : Integer; AValue : TEventproducts); 

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



Procedure TEvent.SetpublisherId(AIndex : Integer; AValue : string); 

begin
  If (FpublisherId=AValue) then exit;
  FpublisherId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetpublisherName(AIndex : Integer; AValue : string); 

begin
  If (FpublisherName=AValue) then exit;
  FpublisherName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Set_type(AIndex : Integer; AValue : string); 

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




{ --------------------------------------------------------------------
  TEventproducts
  --------------------------------------------------------------------}


Procedure TEventproducts.SetcategoryId(AIndex : Integer; AValue : string); 

begin
  If (FcategoryId=AValue) then exit;
  FcategoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventproducts.SetcategoryName(AIndex : Integer; AValue : string); 

begin
  If (FcategoryName=AValue) then exit;
  FcategoryName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventproducts.Setearnings(AIndex : Integer; AValue : TMoney); 

begin
  If (Fearnings=AValue) then exit;
  Fearnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventproducts.SetnetworkFee(AIndex : Integer; AValue : TMoney); 

begin
  If (FnetworkFee=AValue) then exit;
  FnetworkFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventproducts.SetpublisherFee(AIndex : Integer; AValue : TMoney); 

begin
  If (FpublisherFee=AValue) then exit;
  FpublisherFee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventproducts.Setquantity(AIndex : Integer; AValue : string); 

begin
  If (Fquantity=AValue) then exit;
  Fquantity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventproducts.Setsku(AIndex : Integer; AValue : string); 

begin
  If (Fsku=AValue) then exit;
  Fsku:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventproducts.SetskuName(AIndex : Integer; AValue : string); 

begin
  If (FskuName=AValue) then exit;
  FskuName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventproducts.SetunitPrice(AIndex : Integer; AValue : TMoney); 

begin
  If (FunitPrice=AValue) then exit;
  FunitPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEvents
  --------------------------------------------------------------------}


Procedure TEvents.Setitems(AIndex : Integer; AValue : TEventsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLink
  --------------------------------------------------------------------}


Procedure TLink.SetadvertiserId(AIndex : Integer; AValue : string); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setauthorship(AIndex : Integer; AValue : string); 

begin
  If (Fauthorship=AValue) then exit;
  Fauthorship:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setavailability(AIndex : Integer; AValue : string); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetclickTrackingUrl(AIndex : Integer; AValue : string); 

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



Procedure TLink.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetdestinationUrl(AIndex : Integer; AValue : string); 

begin
  If (FdestinationUrl=AValue) then exit;
  FdestinationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setduration(AIndex : Integer; AValue : string); 

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



Procedure TLink.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetimageAltText(AIndex : Integer; AValue : string); 

begin
  If (FimageAltText=AValue) then exit;
  FimageAltText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetimpressionTrackingUrl(AIndex : Integer; AValue : string); 

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



Procedure TLink.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetlinkType(AIndex : Integer; AValue : string); 

begin
  If (FlinkType=AValue) then exit;
  FlinkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetpromotionType(AIndex : Integer; AValue : string); 

begin
  If (FpromotionType=AValue) then exit;
  FpromotionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetspecialOffers(AIndex : Integer; AValue : TLinkspecialOffers); 

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
  TLinkspecialOffers
  --------------------------------------------------------------------}


Procedure TLinkspecialOffers.SetfreeGift(AIndex : Integer; AValue : boolean); 

begin
  If (FfreeGift=AValue) then exit;
  FfreeGift:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkspecialOffers.SetfreeShipping(AIndex : Integer; AValue : boolean); 

begin
  If (FfreeShipping=AValue) then exit;
  FfreeShipping:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkspecialOffers.SetfreeShippingMin(AIndex : Integer; AValue : TMoney); 

begin
  If (FfreeShippingMin=AValue) then exit;
  FfreeShippingMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkspecialOffers.SetpercentOff(AIndex : Integer; AValue : double); 

begin
  If (FpercentOff=AValue) then exit;
  FpercentOff:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkspecialOffers.SetpercentOffMin(AIndex : Integer; AValue : TMoney); 

begin
  If (FpercentOffMin=AValue) then exit;
  FpercentOffMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkspecialOffers.SetpriceCut(AIndex : Integer; AValue : TMoney); 

begin
  If (FpriceCut=AValue) then exit;
  FpriceCut:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkspecialOffers.SetpriceCutMin(AIndex : Integer; AValue : TMoney); 

begin
  If (FpriceCutMin=AValue) then exit;
  FpriceCutMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinkspecialOffers.SetpromotionCodes(AIndex : Integer; AValue : TLinkspecialOfferspromotionCodes); 

begin
  If (FpromotionCodes=AValue) then exit;
  FpromotionCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinkspecialOfferspromotionCodes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLinks
  --------------------------------------------------------------------}


Procedure TLinks.Setitems(AIndex : Integer; AValue : TLinksitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinks.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinks.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinksitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMoney
  --------------------------------------------------------------------}


Procedure TMoney.Setamount(AIndex : Integer; AValue : double); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoney.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublisher
  --------------------------------------------------------------------}


Procedure TPublisher.Setclassification(AIndex : Integer; AValue : string); 

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



Procedure TPublisher.Setid(AIndex : Integer; AValue : string); 

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



Procedure TPublisher.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.SetpayoutRank(AIndex : Integer; AValue : string); 

begin
  If (FpayoutRank=AValue) then exit;
  FpayoutRank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setsites(AIndex : Integer; AValue : TPublishersites); 

begin
  If (Fsites=AValue) then exit;
  Fsites:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisher.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishersites
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPublishers
  --------------------------------------------------------------------}


Procedure TPublishers.Setitems(AIndex : Integer; AValue : TPublishersitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishers.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishers.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishersitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.Setcolumn_names(AIndex : Integer; AValue : TReportcolumn_names); 

begin
  If (Fcolumn_names=AValue) then exit;
  Fcolumn_names:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setend_date(AIndex : Integer; AValue : string); 

begin
  If (Fend_date=AValue) then exit;
  Fend_date:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setmatching_row_count(AIndex : Integer; AValue : string); 

begin
  If (Fmatching_row_count=AValue) then exit;
  Fmatching_row_count:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setrows(AIndex : Integer; AValue : TReportrows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setstart_date(AIndex : Integer; AValue : string); 

begin
  If (Fstart_date=AValue) then exit;
  Fstart_date:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Settotals_rows(AIndex : Integer; AValue : TReporttotals_rows); 

begin
  If (Ftotals_rows=AValue) then exit;
  Ftotals_rows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Set_type(AIndex : Integer; AValue : string); 

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




{ --------------------------------------------------------------------
  TReportcolumn_names
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportrows
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReporttotals_rows
  --------------------------------------------------------------------}




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
  Result:='https://www.googleapis.com/';
end;

Class Function TGanAPI.APIbasePath : string;

begin
  Result:='/gan/v1beta1/';
end;

Class Function TGanAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/gan/v1beta1/';
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
  TAdvertisermerchantCenterIds.RegisterObject;
  TAdvertiserredirectDomains.RegisterObject;
  TAdvertisers.RegisterObject;
  TAdvertisersitems.RegisterObject;
  TCcOffer.RegisterObject;
  TCcOfferadditionalCardBenefits.RegisterObject;
  TCcOfferapprovedCategories.RegisterObject;
  TCcOfferbonusRewards.RegisterObject;
  TCcOffercardBenefits.RegisterObject;
  TCcOfferdefaultFees.RegisterObject;
  TCcOfferprohibitedCategories.RegisterObject;
  TCcOfferrewards.RegisterObject;
  TCcOffers.RegisterObject;
  TCcOffersitems.RegisterObject;
  TEvent.RegisterObject;
  TEventproducts.RegisterObject;
  TEvents.RegisterObject;
  TEventsitems.RegisterObject;
  TLink.RegisterObject;
  TLinkspecialOffers.RegisterObject;
  TLinkspecialOfferspromotionCodes.RegisterObject;
  TLinks.RegisterObject;
  TLinksitems.RegisterObject;
  TMoney.RegisterObject;
  TPublisher.RegisterObject;
  TPublishersites.RegisterObject;
  TPublishers.RegisterObject;
  TPublishersitems.RegisterObject;
  TReport.RegisterObject;
  TReportcolumn_names.RegisterObject;
  TReportrows.RegisterObject;
  TReporttotals_rows.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TGanAPI.RegisterAPI;
end.
