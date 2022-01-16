{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Yury Sidorov,
    member of the Free Pascal development team.

    Header of Android-specific part of the System unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

const
  // Android system log priority
  ANDROID_LOG_VERBOSE = 2;
  ANDROID_LOG_DEBUG   = 3;
  ANDROID_LOG_INFO    = 4;
  ANDROID_LOG_WARN    = 5;
  ANDROID_LOG_ERROR   = 6;
  ANDROID_LOG_FATAL   = 7;

// Default priority for syslog messages.
var DefaultSysLogPriority: longint = ANDROID_LOG_DEBUG;

// Set default tag for syslog messages. Initially the tag is set to the current module name.
procedure SetDefaultSysLogTag(const Tag: string);

// Write a message to the Android system log.
procedure SysLogWrite(Priority: longint; Tag, Msg: PAnsiChar); overload;
procedure SysLogWrite(Priority: longint; Msg: PAnsiChar); overload;
procedure SysLogWrite(Msg: PAnsiChar); overload;

// Redirects standard output and error to the Android system log.
// The redirection is performed automatically for shared libraries loaded by Java applications.
procedure RedirectOutputToSysLog;

// Returns an Android system property.
function GetSystemProperty(Name: PAnsiChar): shortstring;

// Returns an Android API level of the host system.
function SystemApiLevel: shortint;

// True when the current program is a shared library loaded by a Java application.
var IsJniLibrary: boolean;

