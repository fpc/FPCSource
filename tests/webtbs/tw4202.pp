{$mode delphi}

type
  XTask   = type Pointer;
  XInt4   = Integer;
  XID     = type Longword;
  XUniStr = type Pointer;
  XResult = type Integer;

function XLogS(const Task: XTask; const Severity: XInt4; const Msg: XID; const Args:
array of const): XResult; overload;
begin
  Result := 0;
end;

function XLogS(const Task: XTask; const Severity: XInt4; const Text: XUniStr):
XResult; overload;
begin
  Result := 0;
end;

function XLogS(const Task: XTask; const Severity: XInt4; const FormatStr: XUniStr;
const Args: array of const): XResult; overload;
begin
  Result := 0;
end;

const
  XSeverityDebug = 12;
  msg_sys_object_create = 1;
var
  FTask: XTask;
begin
  XLogS(FTask, XSeverityDebug, msg_sys_object_create, [1]);
end.
