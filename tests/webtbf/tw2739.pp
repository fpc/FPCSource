{ %fail }

{$mode Delphi}
unit tw2739;

interface



type
  TJclExceptNotifyProc = procedure (ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
  TJclExceptNotifyMethod = procedure (ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean) of object;



function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProc ): Boolean; overload;
function JclAddExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod): Boolean; overload;


implementation

function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProc ): Boolean; overload;

begin
end;



function JclAddExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod): Boolean; overload;
var
//  p : pointer;
  p : TJclExceptNotifyMethod;
begin
  { This fails @NotifyMethod returns a pointer to the proc }
  p:=@NotifyMethod;
end;


begin
end.
