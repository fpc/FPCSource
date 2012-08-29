unit uw22741b;
{$mode objfpc}

interface

type
    iBase = interface
        function getSelf: tObject;
    end;

    tInterfaceObject= class(tObject, iBase)
        public
            function getSelf: tObject;
            function queryInterface({$IFDEF FPC_HAS_CONSTREF}constRef{$ELSE}const{$ENDIF} iid: tGuid; out obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
            function _addRef: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
            function _release: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    end;


implementation


function tInterfaceObject.getSelf: tObject;
begin
    result:= self;
end;

function tInterfaceObject.queryInterface({$IFDEF FPC_HAS_CONSTREF}constRef{$ELSE}const{$ENDIF} iid: tGuid; out obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
    if getInterface(iId, obj) then
        result:= S_OK
    else
        result:= longint(E_NOINTERFACE);
end;

function tInterfaceObject._addRef: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
    result:= 1;
end;

function tInterfaceObject._release: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
    result:= 1;
end;


end.

