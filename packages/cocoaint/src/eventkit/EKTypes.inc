{ Parsed from EventKit.framework EKTypes.h }

{$ifdef TYPES}
const
  EKSunday = 1;
  EKMonday = 2;
  EKTuesday = 3;
  EKWednesday = 4;
  EKThursday = 5;
  EKFriday = 6;
  EKSaturday = 7;
const
  EKRecurrenceFrequencyDaily = 0;
  EKRecurrenceFrequencyWeekly = 1;
  EKRecurrenceFrequencyMonthly = 2;
  EKRecurrenceFrequencyYearly = 3;
type
  EKRecurrenceFrequency = clong;
const
  EKParticipantTypeUnknown = 0;
  EKParticipantTypePerson = 1;
  EKParticipantTypeRoom = 2;
  EKParticipantTypeResource = 3;
  EKParticipantTypeGroup = 4;
type
  EKParticipantType = clong;
const
  EKParticipantRoleUnknown = 0;
  EKParticipantRoleRequired = 1;
  EKParticipantRoleOptional = 2;
  EKParticipantRoleChair = 3;
  EKParticipantRoleNonParticipant = 4;
type
  EKParticipantRole = clong;
const
  EKParticipantStatusUnknown = 0;
  EKParticipantStatusPending = 1;
  EKParticipantStatusAccepted = 2;
  EKParticipantStatusDeclined = 3;
  EKParticipantStatusTentative = 4;
  EKParticipantStatusDelegated = 5;
  EKParticipantStatusCompleted = 6;
  EKParticipantStatusInProcess = 7;
type
  EKParticipantStatus = clong;
const
  EKCalendarTypeLocal = 0;
  EKCalendarTypeCalDAV = 1;
  EKCalendarTypeExchange = 2;
  EKCalendarTypeSubscription = 3;
  EKCalendarTypeBirthday = 4;
type
  EKCalendarType = clong;
const
  EKCalendarEventAvailabilityNone = 0;
  EKCalendarEventAvailabilityBusy = 1 shl 0;
  EKCalendarEventAvailabilityFree = 1 shl 1;
  EKCalendarEventAvailabilityTentative = 1 shl 2;
  EKCalendarEventAvailabilityUnavailable = 1 shl 3;

type
  EKCalendarEventAvailabilityMask = NSUInteger;
  EKCalendarEventAvailabilityMaskPtr = ^EKCalendarEventAvailabilityMask;

const
  EKSourceTypeLocal = 0;
  EKSourceTypeExchange = 1;
  EKSourceTypeCalDAV = 2;
  EKSourceTypeMobileMe = 3;
  EKSourceTypeSubscribed = 4;
  EKSourceTypeBirthdays = 5;
type
  EKSourceType = clong;
const
  EKEntityTypeEvent = 0;
  EKEntityTypeReminder = 1;

type
  EKEntityType = NSUInteger;
  EKEntityTypePtr = ^EKEntityType;

const
  EKEntityMaskEvent = 1 shl EKEntityTypeEvent;
  EKEntityMaskReminder = 1 shl EKEntityTypeReminder;

type
  EKEntityMask = NSUInteger;
  EKEntityMaskPtr = ^EKEntityMask;

const
  EKAlarmProximityNone = 0;
  EKAlarmProximityEnter = 1;
  EKAlarmProximityLeave = 2;

type
  EKAlarmProximity = NSInteger;
  EKAlarmProximityPtr = ^EKAlarmProximity;

const
  EKAlarmTypeDisplay = 0;
  EKAlarmTypeAudio = 1;
  EKAlarmTypeProcedure = 2;
  EKAlarmTypeEmail = 3;
type
  EKAlarmType = clong;
const
  EKReminderPriorityNone = 0;
  EKReminderPriorityHigh = 1;
  EKReminderPriorityMedium = 5;
  EKReminderPriorityLow = 9;
type
  EKReminderPriority = NSUInteger;
{$endif}

