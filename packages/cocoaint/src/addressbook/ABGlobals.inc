{ Parsed from AddressBook.framework ABGlobals.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  kABUIDProperty: NSString; cvar; external;
  kABCreationDateProperty: NSString; cvar; external;
  kABModificationDateProperty: NSString; cvar; external;
  kABFirstNameProperty: NSString; cvar; external;
  kABLastNameProperty: NSString; cvar; external;
  kABFirstNamePhoneticProperty: NSString; cvar; external;
  kABLastNamePhoneticProperty: NSString; cvar; external;
  kABNicknameProperty: NSString; cvar; external;
  kABMaidenNameProperty: NSString; cvar; external;
  kABBirthdayProperty: NSString; cvar; external;
  kABBirthdayComponentsProperty: NSString; cvar; external;
  kABAlternateBirthdayComponentsProperty: NSString; cvar; external;
  kABOrganizationProperty: NSString; cvar; external;
  kABJobTitleProperty: NSString; cvar; external;
  kABHomePageProperty: NSString; cvar; external;
  kABURLsProperty: NSString; cvar; external;
  kABHomePageLabel: NSString; cvar; external;
  kABCalendarURIsProperty: NSString; cvar; external;
  kABEmailProperty: NSString; cvar; external;
  kABEmailWorkLabel: NSString; cvar; external;
  kABEmailHomeLabel: NSString; cvar; external;
  kABEmailMobileMeLabel: NSString; cvar; external;
  kABAddressProperty: NSString; cvar; external;
  kABAddressStreetKey: NSString; cvar; external;
  kABAddressCityKey: NSString; cvar; external;
  kABAddressStateKey: NSString; cvar; external;
  kABAddressZIPKey: NSString; cvar; external;
  kABAddressCountryKey: NSString; cvar; external;
  kABAddressCountryCodeKey: NSString; cvar; external;
  kABAddressHomeLabel: NSString; cvar; external;
  kABAddressWorkLabel: NSString; cvar; external;
  kABOtherDatesProperty: NSString; cvar; external;
  kABOtherDateComponentsProperty: NSString; cvar; external;
  kABAnniversaryLabel: NSString; cvar; external;
  kABRelatedNamesProperty: NSString; cvar; external;
  kABFatherLabel: NSString; cvar; external;
  kABMotherLabel: NSString; cvar; external;
  kABParentLabel: NSString; cvar; external;
  kABBrotherLabel: NSString; cvar; external;
  kABSisterLabel: NSString; cvar; external;
  kABChildLabel: NSString; cvar; external;
  kABFriendLabel: NSString; cvar; external;
  kABSpouseLabel: NSString; cvar; external;
  kABPartnerLabel: NSString; cvar; external;
  kABAssistantLabel: NSString; cvar; external;
  kABManagerLabel: NSString; cvar; external;
  kABDepartmentProperty: NSString; cvar; external;
  kABPersonFlags: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kABShowAsMask = 000007;
  kABShowAsPerson = 000000;
  kABShowAsCompany = 000001;
  kABShowAsResource = 000002;
  kABShowAsRoom = 000003;
  kABNameOrderingMask = 000070;
  kABDefaultNameOrdering = 000000;
  kABFirstNameFirst = 000040;
  kABLastNameFirst = 000020;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kABPhoneProperty: NSString; cvar; external;
  kABPhoneWorkLabel: NSString; cvar; external;
  kABPhoneHomeLabel: NSString; cvar; external;
  kABPhoneiPhoneLabel: NSString; cvar; external;
  kABPhoneMobileLabel: NSString; cvar; external;
  kABPhoneMainLabel: NSString; cvar; external;
  kABPhoneHomeFAXLabel: NSString; cvar; external;
  kABPhoneWorkFAXLabel: NSString; cvar; external;
  kABPhonePagerLabel: NSString; cvar; external;
  kABAIMInstantProperty: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABAIMWorkLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABAIMHomeLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABAIMMobileMeLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABJabberInstantProperty: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABJabberWorkLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABJabberHomeLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABMSNInstantProperty: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABMSNWorkLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABMSNHomeLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABYahooInstantProperty: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABYahooWorkLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABYahooHomeLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABICQInstantProperty: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABICQWorkLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABICQHomeLabel: NSString deprecated 'in 10.7 and later'; cvar; external;
  kABInstantMessageProperty: NSString; cvar; external;
  kABInstantMessageUsernameKey: NSString; cvar; external;
  kABInstantMessageServiceKey: NSString; cvar; external;
  kABInstantMessageServiceAIM: NSString; cvar; external;
  kABInstantMessageServiceFacebook: NSString; cvar; external;
  kABInstantMessageServiceGaduGadu: NSString; cvar; external;
  kABInstantMessageServiceGoogleTalk: NSString; cvar; external;
  kABInstantMessageServiceICQ: NSString; cvar; external;
  kABInstantMessageServiceJabber: NSString; cvar; external;
  kABInstantMessageServiceMSN: NSString; cvar; external;
  kABInstantMessageServiceQQ: NSString; cvar; external;
  kABInstantMessageServiceSkype: NSString; cvar; external;
  kABInstantMessageServiceYahoo: NSString; cvar; external;
  kABSocialProfileProperty: NSString; cvar; external;
  kABSocialProfileURLKey: NSString; cvar; external;
  kABSocialProfileUsernameKey: NSString; cvar; external;
  kABSocialProfileUserIdentifierKey: NSString; cvar; external;
  kABSocialProfileServiceKey: NSString; cvar; external;
  kABSocialProfileServiceTwitter: NSString; cvar; external;
  kABSocialProfileServiceFacebook: NSString; cvar; external;
  kABSocialProfileServiceLinkedIn: NSString; cvar; external;
  kABSocialProfileServiceFlickr: NSString; cvar; external;
  kABSocialProfileServiceMySpace: NSString; cvar; external;
  kABSocialProfileServiceSinaWeibo: NSString; cvar; external;
  kABSocialProfileServiceTencentWeibo: NSString; cvar; external;
  kABSocialProfileServiceYelp: NSString; cvar; external;
  kABNoteProperty: NSString; cvar; external;
  kABMiddleNameProperty: NSString; cvar; external;
  kABMiddleNamePhoneticProperty: NSString; cvar; external;
  kABTitleProperty: NSString; cvar; external;
  kABSuffixProperty: NSString; cvar; external;
  kABGroupNameProperty: NSString; cvar; external;
  kABWorkLabel: NSString; cvar; external;
  kABHomeLabel: NSString; cvar; external;
  kABOtherLabel: NSString; cvar; external;
  kABMobileMeLabel: NSString; cvar; external;
  kABDatabaseChangedNotification: NSString; cvar; external;
  kABDatabaseChangedExternallyNotification: NSString; cvar; external;
  kABInsertedRecords: NSString; cvar; external;
  kABUpdatedRecords: NSString; cvar; external;
  kABDeletedRecords: NSString; cvar; external;
{$endif}

