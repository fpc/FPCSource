{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Find.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines field structures and routines.
 *
 * History:
 *    August 29, 1994   Created by Art Lamb
 *
 *****************************************************************************)

unit find_;

interface

uses palmos, coretraps, rect;

const
  maxFinds      = 9;
  maxFindStrLen = 16;

type
  FindMatchType = record
    appCardNo: UInt16;      // card number of the application
    appDbID: LocalID;                // LocalID of the application
    foundInCaller: Boolean; // true if found in app that called Find
    reserved: UInt8;

    dbCardNo: UInt16;       // card number of the database record was found in
    dbID: LocalID;                   // LocalID of the database record was found in
    recordNum: UInt16;      // index of record that contain a match
    matchPos: UInt16;       // postion in record of the match.
    matchFieldNum: UInt16;  // field number
    matchCustom: UInt32;    // app specific data
  end;

  FindMatchPtr = ^FindMatchType;

  FindParamsType = record
    // These fields are used by the applications.
    dbAccesMode: UInt16;                          // read mode and maybe show secret
    recordNum: UInt16;                            // index of last record that contained a match
    more: Boolean;                                // true of more matches to display
    strAsTyped: array [0..maxFindStrLen] of Char; // search string as entered
    strToFind: array [0..maxFindStrLen] of Char;  // search string is lower case
    reserved1: UInt8;

    // The lineNumber field can be modified by the app. The continuation field can
    // be tested by the app. All other fields are private to the Find routine and
    // should NOT be accessed by applications.
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FINDPARAMS} // These fields will not be available in the next OS release!
    numMatches: UInt16;                           // # of matches
    lineNumber: UInt16;                           // next line in the results tabel
    continuation: Boolean;                        // true if contining search of same app
    searchedCaller: Boolean;                      // true after we've searched app that initiated the find

    callerAppDbID: LocalID;                       // dbID of app that initiated search
    callerAppCardNo: UInt16;                      // cardNo of app that initiated search

    appDbID: LocalID;                             // dbID of app that we're currently searching
    appCardNo: UInt16;                            // card number of app that we're currently searching

    newSearch: Boolean;                           // true for first search
    reserved2: UInt8;
    searchState: DmSearchStateType;               // search state
    match: array [0..maxFinds-1] of FindMatchType;
  {$else}
    noAccessAllowed1: UInt16;  // # of matches
    lineNumber: UInt16;        // next line in the results tabel
    continuation: Boolean;     // true if contining search of same app
    noAccessAllowed2: Boolean; // padding
  {$endif}
  end;

  FindParamsPtr = ^FindParamsType;

// Param Block passsed with the sysAppLaunchCmdGoto Command
  GoToParamsType = record
    searchStrLen: Int16;   // length of search string.
    dbCardNo: UInt16;      // card number of the database
    dbID: LocalID;         // LocalID of the database
    recordNum: UInt16;     // index of record that contain a match
    matchPos: UInt16;      // postion in record of the match.
    matchFieldNum: UInt16; // field number string was found int
    matchCustom: UInt32;   // application specific info
  end;

  GoToParamsPtr = ^GoToParamsType;

//----------------------------------------------------------
//  Find Functions
//----------------------------------------------------------

procedure Find(goToP: GoToParamsPtr); syscall sysTrapFind;

function FindStrInStr(strToSearch, strToFind: PChar; var posP: UInt16): Boolean; syscall sysTrapFindStrInStr;

function FindSaveMatch(findParams: FindParamsPtr; recordNum, pos, fieldNum: UInt16;
                       appCustom: UInt32; cardNo: UInt16; dbID: LocalID): Boolean; syscall sysTrapFindSaveMatch;

procedure FindGetLineBounds(const findParams: FindParamsPtr; r: RectanglePtr); syscall sysTrapFindGetLineBounds;

function FindDrawHeader(findParams: FindParamsPtr; title: PChar): Boolean; syscall sysTrapFindDrawHeader;

implementation

end.
