;    $Id: fclel.mc,v 1.1 2003/02/19 20:25:16 michael Exp $
;    This file is part of the Free Pascal run time library.
;    Copyright (c) 2003 by the Free Pascal development team
;
;    Messages for event logging facility
;    
;    See the file COPYING.FPC, included in this distribution,
;    for details about the copyright.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;
;
;******************************************************
; Default messages for FPC eventlog class
;******************************************************
; Categories are mapped from 1 to 4
; 1 : etInfo
; 2 : etWarning
; 3 : etError
; 4 : etDebug
;
; Categories (1-4)
MessageId=1
SymbolicName=ECInfo
Language=English
Information
.

MessageId=2
SymbolicName=ECWarning
Language=English
Warning
.

MessageId=3
SymbolicName=ECError
Language=English
Error
.

MessageId=4
SymbolicName=ECDebug
Language=English
Debug
.

;
; Message Definitions (1000-1004)
;
MessageId=1000
Language=English
%1.
.

; Information
MessageId=1001
Language=English
Information: %1
.

; Warnings
MessageId=1002
Language=English
Warning: %1
.

; Error
MessageId=1003
Language=English
Error: %1
.

; Debug
MessageId=1004
Language=English
Debug: %1
.
