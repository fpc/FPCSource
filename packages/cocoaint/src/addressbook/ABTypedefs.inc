{ Parsed from AddressBook.framework ABTypedefs.h }

{$ifndef __ABTYPEDEFS__}
{$ifdef TYPES}
const
  kABMultiValueMask = $100;

const
  kABErrorInProperty = $0;
  kABStringProperty = $1;
  kABIntegerProperty = $2;
  kABRealProperty = $3;
  kABDateProperty = $4;
  kABArrayProperty = $5;
  kABDictionaryProperty = $6;
  kABDataProperty = $7;
  {$if (MAC_OS_X_VERSION_10_7 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
  kABDateComponentsProperty = $8;
  {$endif}
  kABMultiStringProperty = kABMultiValueMask + kABStringProperty;
  kABMultiIntegerProperty = kABMultiValueMask + kABIntegerProperty;
  kABMultiRealProperty = kABMultiValueMask + kABRealProperty;
  kABMultiDateProperty = kABMultiValueMask + kABDateProperty;
  kABMultiArrayProperty = kABMultiValueMask + kABArrayProperty;
  kABMultiDictionaryProperty = kABMultiValueMask + kABDictionaryProperty;
  kABMultiDataProperty = kABMultiValueMask + kABDataProperty;
  {$if (MAC_OS_X_VERSION_10_7 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
  kABMultiDateComponentsProperty = kABMultiValueMask + kABDateComponentsProperty;
  {$endif}

type
  ABPropertyType = CFIndex;
  ABPropertyTypePtr = ^ABPropertyType;

const
  kABEqual = 0;
  kABNotEqual = 1;
  kABLessThan = 2;
  kABLessThanOrEqual = 3;
  kABGreaterThan = 4;
  kABGreaterThanOrEqual = 5;
  kABEqualCaseInsensitive = 6;
  kABContainsSubString = 7;
  kABContainsSubStringCaseInsensitive = 8;
  kABPrefixMatch = 9;
  kABPrefixMatchCaseInsensitive = 10;
  {$if (MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
  kABBitsInBitFieldMatch = 11;
  {$endif}
  {$if (MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
  kABDoesNotContainSubString = 12;
  kABDoesNotContainSubStringCaseInsensitive = 13;
  kABNotEqualCaseInsensitive = 14;
  kABSuffixMatch = 15;
  kABSuffixMatchCaseInsensitive = 16;
  kABWithinIntervalAroundToday = 17;
  kABWithinIntervalAroundTodayYearless = 18;
  kABNotWithinIntervalAroundToday = 19;
  kABNotWithinIntervalAroundTodayYearless = 20;
  kABWithinIntervalFromToday = 21;
  kABWithinIntervalFromTodayYearless = 22;
  kABNotWithinIntervalFromToday = 23;
  kABNotWithinIntervalFromTodayYearless = 24;
  {$endif}

type
  ABSearchComparison = CFIndex;
  ABSearchComparisonPtr = ^ABSearchComparison;

const
  kABSearchAnd = 0;
  kABSearchOr = 1;

type
  ABSearchConjunction = CFIndex;
  ABSearchConjunctionPtr = ^ABSearchConjunction;
{$endif}

{$endif}
