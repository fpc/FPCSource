(******************************************************************************
 *
 * Copyright (c) 1997-2000 Palm Computing, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: PdiConst.h
 *
 * Description:
 *    PDI Library constants
 *
 * History:
 *       Created by ABa (PdiMakeDictionary tool)
 *
 *****************************************************************************)

(******************************************************************************
 * Property fields access
 *****************************************************************************)

unit pdiconst;

interface

uses palmos;

const
  kPdiPVF_ADR_POST_OFFICE = UInt8(0);
  kPdiPVF_ADR_EXTENDED    = UInt8(1);
  kPdiPVF_ADR_STREET      = UInt8(2);
  kPdiPVF_ADR_LOCALITY    = UInt8(3);
  kPdiPVF_ADR_REGION      = UInt8(4);
  kPdiPVF_ADR_POSTAL_CODE = UInt8(5);
  kPdiPVF_ADR_COUNTRY     = UInt8(6);
  kPdiPVF_GEO_LATITUDE    = UInt8(0);
  kPdiPVF_GEO_LONGITUDE   = UInt8(1);
  kPdiPVF_N_FAMILY        = UInt8(0);
  kPdiPVF_N_GIVEN         = UInt8(1);
  kPdiPVF_N_ADDITIONAL    = UInt8(2);
  kPdiPVF_N_PREFIXES      = UInt8(3);
  kPdiPVF_N_SUFFIXES      = UInt8(4);

(******************************************************************************
 * Properties constants
 ******************************************************************************)

  kPdiPRN_FREEBUSY         = UInt16(988);
  kPdiPRN_X_PALM_CUSTOM    = UInt16(1044);
  kPdiPRN_METHOD           = UInt16(1108);
  kPdiPRN_ORG              = UInt16(1236);
  kPdiPRN_X_PALM_CATEGORY  = UInt16(1250);
  kPdiPRN_TITLE            = UInt16(1488);
  kPdiPRN_ORGANIZER        = UInt16(1528);
  kPdiPRN_TZ               = UInt16(1566);
  kPdiPRN_VERSION          = UInt16(1682);
  kPdiPRN_TZID             = UInt16(1722);
  kPdiPRN_CLASS            = UInt16(1814);
  kPdiPRN_TZURL            = UInt16(1832);
  kPdiPRN_EXDATE           = UInt16(1886);
  kPdiPRN_EXRULE           = UInt16(1906);
  kPdiPRN_PRODID           = UInt16(1926);
  kPdiPRN_TZNAME           = UInt16(1946);
  kPdiPRN_GEO              = UInt16(1966);
  kPdiPRN_UID              = UInt16(1980);
  kPdiPRN_PROFILE          = UInt16(1994);
  kPdiPRN_PRIORITY         = UInt16(2032);
  kPdiPRN_ROLE             = UInt16(2056);
  kPdiPRN_TZOFFSET         = UInt16(2072);
  kPdiPRN_AALARM           = UInt16(2160);
  kPdiPRN_TZOFFSETTO       = UInt16(2180);
  kPdiPRN_TZOFFSETFROM     = UInt16(2244);
  kPdiPRN_SOUND            = UInt16(2312);
  kPdiPRN_ACTION           = UInt16(2410);
  kPdiPRN_SOURCE           = UInt16(2430);
  kPdiPRN_ADR              = UInt16(2472);
  kPdiPRN_COMMENT          = UInt16(2486);
  kPdiPRN_CONTACT          = UInt16(2530);
  kPdiPRN_NICKNAME         = UInt16(2568);
  kPdiPRN_COMPLETED        = UInt16(2620);
  kPdiPRN_RRULE            = UInt16(2646);
  kPdiPRN_ATTACH           = UInt16(2692);
  kPdiPRN_SORT_STRING      = UInt16(2728);
  kPdiPRN_ATTENDEE         = UInt16(2776);
  kPdiPRN_LOGO             = UInt16(2800);
  kPdiPRN_EMAIL            = UInt16(2832);
  kPdiPRN_END              = UInt16(2852);
  kPdiPRN_BDAY             = UInt16(2866);
  kPdiPRN_CALSCALE         = UInt16(2882);
  kPdiPRN_LOCATION         = UInt16(2906);
  kPdiPRN_PERCENT_COMPLETE = UInt16(2930);
  kPdiPRN_PHOTO            = UInt16(2970);
  kPdiPRN_RDATE            = UInt16(2988);
  kPdiPRN_CATEGORIES       = UInt16(3026);
  kPdiPRN_CREATED          = UInt16(3074);
  kPdiPRN_REV              = UInt16(3096);
  kPdiPRN_LABEL            = UInt16(3226);
  kPdiPRN_BEGIN            = UInt16(3244);
  kPdiPRN_END_VCARD        = UInt16(3262);
  kPdiPRN_END_VTODO        = UInt16(3288);
  kPdiPRN_AGENT            = UInt16(3314);
  kPdiPRN_DALARM           = UInt16(3372);
  kPdiPRN_FN               = UInt16(3392);
  kPdiPRN_REPEAT           = UInt16(3404);
  kPdiPRN_END_VEVENT       = UInt16(3424);
  kPdiPRN_END_VJOURNAL     = UInt16(3560);
  kPdiPRN_END_VCALENDAR    = UInt16(3694);
  kPdiPRN_RESOURCES        = UInt16(3728);
  kPdiPRN_END_VFREEBUSY    = UInt16(3754);
  kPdiPRN_END_VTIMEZONE    = UInt16(3788);
  kPdiPRN_STATUS           = UInt16(3870);
  kPdiPRN_RELATED_TO       = UInt16(3890);
  kPdiPRN_TRANSP           = UInt16(3918);
  kPdiPRN_KEY              = UInt16(3938);
  kPdiPRN_BEGIN_VCARD      = UInt16(3952);
  kPdiPRN_BEGIN_VTODO      = UInt16(3982);
  kPdiPRN_TRIGGER          = UInt16(4012);
  kPdiPRN_NOTE             = UInt16(4034);
  kPdiPRN_BEGIN_VEVENT     = UInt16(4050);
  kPdiPRN_N                = UInt16(4118);
  kPdiPRN_LAST_MODIFIED    = UInt16(4128);
  kPdiPRN_RECURRENCE_ID    = UInt16(4162);
  kPdiPRN_MAILER           = UInt16(4216);
  kPdiPRN_REQUEST_STATUS   = UInt16(4236);
  kPdiPRN_BEGIN_VJOURNAL   = UInt16(4272);
  kPdiPRN_SUMMARY          = UInt16(4366);
  kPdiPRN_BEGIN_VCALENDAR  = UInt16(4388);
  kPdiPRN_URL              = UInt16(4426);
  kPdiPRN_BEGIN_VFREEBUSY  = UInt16(4440);
  kPdiPRN_BEGIN_VTIMEZONE  = UInt16(4478);
  kPdiPRN_SEQUENCE         = UInt16(4594);
  kPdiPRN_DTEND            = UInt16(4660);
  kPdiPRN_DTSTART          = UInt16(4678);
  kPdiPRN_DUE              = UInt16(4700);
  kPdiPRN_TEL              = UInt16(4714);
  kPdiPRN_DTSTAMP          = UInt16(4744);
  kPdiPRN_NAME             = UInt16(4800);
  kPdiPRN_DURATION         = UInt16(4954);
  kPdiPRN_DESCRIPTION      = UInt16(5270);

(******************************************************************************
 * Parameters constants
 *****************************************************************************)

  kPdiPAN_DELEGATED_TO   = UInt16(1012);
  kPdiPAN_X              = UInt16(1098);
  kPdiPAN_DELEGATED_FROM = UInt16(1128);
  kPdiPAN_MEMBER         = UInt16(1164);
  kPdiPAN_UTC_OFFSET     = UInt16(1186);
  kPdiPAN_DIR            = UInt16(1350);
  kPdiPAN_TYPE           = UInt16(1428);
  kPdiPAN_TIME           = UInt16(1446);
  kPdiPAN_PARTSTAT       = UInt16(1738);
  kPdiPAN_ROLE           = UInt16(2056);
  kPdiPAN_CN             = UInt16(2208);
  kPdiPAN_SOUND          = UInt16(2312);
  kPdiPAN_RANGE          = UInt16(2330);
  kPdiPAN_CONTEXT        = UInt16(2508);
  kPdiPAN_RSVP           = UInt16(2816);
  kPdiPAN_ENCODE         = UInt16(3054);
  kPdiPAN_ENCODING       = UInt16(3166);
  kPdiPAN_FMTTYPE        = UInt16(3452);
  kPdiPAN_RELATED        = UInt16(3474);
  kPdiPAN_RELTYPE        = UInt16(3496);
  kPdiPAN_LANGUAGE       = UInt16(3618);
  kPdiPAN_STATUS         = UInt16(3870);
  kPdiPAN_CUTYPE         = UInt16(4308);
  kPdiPAN_SENT_BY        = UInt16(4556);
  kPdiPAN_URI            = UInt16(4580);
  kPdiPAN_VALUE          = UInt16(4852);
  kPdiPAN_ALTREP         = UInt16(5200);
  kPdiPAN_FBTYPE         = UInt16(5220);
  kPdiPAN_CHARSET        = UInt16(5300);

(******************************************************************************
 * Parameter pairs constants
 *****************************************************************************)

  kPdiPAV_TYPE_HOME                 = UInt16(0);
  kPdiPAV_VALUE_VCARD               = UInt16(2);
  kPdiPAV_TYPE_VCARD                = UInt16(4);
  kPdiPAV_VALUE_UTC_OFFSET          = UInt16(6);
  kPdiPAV_TYPE_POSTAL               = UInt16(8);
  kPdiPAV_RELTYPE_SIBLING           = UInt16(10);
  kPdiPAV_TYPE_INTL                 = UInt16(12);
  kPdiPAV_CUTYPE_GROUP              = UInt16(14);
  kPdiPAV_ROLE_OPT_PARTICIPANT      = UInt16(16);
  kPdiPAV_VALUE_INTEGER             = UInt16(18);
  kPdiPAV_VALUE_TIME                = UInt16(20);
  kPdiPAV_TYPE_INTERNET             = UInt16(22);
  kPdiPAV_TYPE_PAGER                = UInt16(24);
  kPdiPAV_ROLE_ORGANIZER            = UInt16(26);
  kPdiPAV_ENCODING_Q                = UInt16(28);
  kPdiPAV_CUTYPE_INDIVIDUAL         = UInt16(30);
  kPdiPAV_PARTSTAT_IN_PROCESS       = UInt16(32);
  kPdiPAV_RELTYPE_PARENT            = UInt16(34);
  kPdiPAV_TYPE_PARCEL               = UInt16(36);
  kPdiPAV_TYPE_PREF                 = UInt16(38);
  kPdiPAV_RANGE_THISANDPRIOR        = UInt16(40);
  kPdiPAV_ENCODING_8BIT             = UInt16(42);
  kPdiPAV_RANGE_THISANDFUTURE       = UInt16(44);
  kPdiPAV_TYPE_PCS                  = UInt16(46);
  kPdiPAV_CUTYPE_ROOM               = UInt16(48);
  kPdiPAV_PARTSTAT_NEEDS_ACTION     = UInt16(50);
  kPdiPAV_STATUS_NEEDS_ACTION       = UInt16(52);
  kPdiPAV_ENCODING_B                = UInt16(54);
  kPdiPAV_VALUE_BOOLEAN             = UInt16(56);
  kPdiPAV_TYPE_X400                 = UInt16(58);
  kPdiPAV_TYPE_ISDN                 = UInt16(60);
  kPdiPAV_ROLE_OWNER                = UInt16(62);
  kPdiPAV_TYPE_VIDEO                = UInt16(64);
  kPdiPAV_ENCODING_BASE64           = UInt16(66);
  kPdiPAV_VALUE_PERIOD              = UInt16(68);
  kPdiPAV_TYPE_BBS                  = UInt16(70);
  kPdiPAV_PARTSTAT_ACCEPTED         = UInt16(72);
  kPdiPAV_STATUS_ACCEPTED           = UInt16(74);
  kPdiPAV_PARTSTAT_COMPLETED        = UInt16(76);
  kPdiPAV_STATUS_COMPLETED          = UInt16(78);
  kPdiPAV_STATUS_CONFIRMED          = UInt16(80);
  kPdiPAV_TYPE_CAR                  = UInt16(82);
  kPdiPAV_TYPE_DOM                  = UInt16(84);
  kPdiPAV_ROLE_ATTENDEE             = UInt16(86);
  kPdiPAV_RELATED_END               = UInt16(88);
  kPdiPAV_VALUE_FLOAT               = UInt16(90);
  kPdiPAV_CUTYPE_UNKNOWN            = UInt16(92);
  kPdiPAV_VALUE_CAL_ADDRESS         = UInt16(94);
  kPdiPAV_FBTYPE_BUSY               = UInt16(96);
  kPdiPAV_VALUE_DATE                = UInt16(98);
  kPdiPAV_VALUE_RECUR               = UInt16(100);
  kPdiPAV_TYPE_MODEM                = UInt16(102);
  kPdiPAV_ENCODING_QUOTED_PRINTABLE = UInt16(104);
  kPdiPAV_CUTYPE_RESOURCE           = UInt16(106);
  kPdiPAV_RSVP_TRUE                 = UInt16(108);
  kPdiPAV_VALUE_PHONE_NUMBER        = UInt16(110);
  kPdiPAV_RELATED_START             = UInt16(112);
  kPdiPAV_VALUE_DATE_TIME           = UInt16(114);
  kPdiPAV_TYPE_CELL                 = UInt16(116);
  kPdiPAV_STATUS_SENT               = UInt16(118);
  kPdiPAV_TYPE_VOICE                = UInt16(120);
  kPdiPAV_FBTYPE_BUSY_TENTATIVE     = UInt16(122);
  kPdiPAV_ROLE_REQ_PARTICIPANT      = UInt16(124);
  kPdiPAV_VALUE_URI                 = UInt16(126);
  kPdiPAV_FBTYPE_BUSY_UNAVAILABLE   = UInt16(128);
  kPdiPAV_TYPE_FAX                  = UInt16(130);
  kPdiPAV_TYPE_MSG                  = UInt16(132);
  kPdiPAV_TYPE_WORK                 = UInt16(134);
  kPdiPAV_VALUE_TEXT                = UInt16(136);
  kPdiPAV_CONTEXT_WORD              = UInt16(138);
  kPdiPAV_RSVP_FALSE                = UInt16(140);
  kPdiPAV_VALUE_BINARY              = UInt16(142);
  kPdiPAV_ROLE_NON_PARTICIPANT      = UInt16(144);
  kPdiPAV_VALUE_DURATION            = UInt16(146);
  kPdiPAV_X_X_PALM_N                = UInt16(148);
  kPdiPAV_X_X_IRMC_N                = UInt16(150);
  kPdiPAV_FBTYPE_FREE               = UInt16(152);
  kPdiPAV_PARTSTAT_DECLINED         = UInt16(154);
  kPdiPAV_STATUS_DECLINED           = UInt16(156);
  kPdiPAV_PARTSTAT_TENTATIVE        = UInt16(158);
  kPdiPAV_STATUS_TENTATIVE          = UInt16(160);
  kPdiPAV_PARTSTAT_DELEGATED        = UInt16(162);
  kPdiPAV_STATUS_DELEGATED          = UInt16(164);
  kPdiPAV_RELTYPE_CHILD             = UInt16(166);
  kPdiPAV_ROLE_CHAIR                = UInt16(168);
  kPdiPAV_X_X_PALM_ORG              = UInt16(170);
  kPdiPAV_X_X_IRMC_ORG              = UInt16(172);
  kPdiPAV_X_X_PALM_MAIN             = UInt16(174);

(******************************************************************************
 * Properties types constants
 *****************************************************************************)

  kPdiType_DATE_TIME    = kPdiPAV_VALUE_DATE_TIME;
  kPdiType_TEXT         = kPdiPAV_VALUE_TEXT;
  kPdiType_CAL_ADDRESS  = kPdiPAV_VALUE_CAL_ADDRESS;
  kPdiType_DURATION     = kPdiPAV_VALUE_DURATION;
  kPdiType_RECUR        = kPdiPAV_VALUE_RECUR;
  kPdiType_PERIOD       = kPdiPAV_VALUE_PERIOD;
  kPdiType_FLOAT        = kPdiPAV_VALUE_FLOAT;
  kPdiType_BINARY       = kPdiPAV_VALUE_BINARY;
  kPdiType_INTEGER      = kPdiPAV_VALUE_INTEGER;
  kPdiType_UTC_OFFSET   = kPdiPAV_VALUE_UTC_OFFSET;
  kPdiType_URI          = kPdiPAV_VALUE_URI;
  kPdiType_BOOLEAN      = kPdiPAV_VALUE_BOOLEAN;
  kPdiType_DATE         = kPdiPAV_VALUE_DATE;
  kPdiType_TIME         = kPdiPAV_VALUE_TIME;
  kPdiType_VCARD        = kPdiPAV_VALUE_VCARD;
  kPdiType_PHONE_NUMBER = kPdiPAV_VALUE_PHONE_NUMBER;

implementation

end.
