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
