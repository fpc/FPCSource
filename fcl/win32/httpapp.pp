{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit httpapp;

  interface

    uses
       sysutils,classes,syncobjs;

    const
       DateFormat = 'ddd, dd mmm yyyy hh:mm:ss';

       MAX_STRINGS = 12;
       MAX_INTEGERS = 1;
       MAX_DATETIMES = 3;

    type
       TCharSet = set of Char;
       TMethodType = (mtAny,mtGet,mtPut,mtPost,mtHead);

       TWebApp = class(TComponent)
       protected
          function ActivateWebModule : TDataModule;
          procedure DeactivateWebModule(DataModule : TDataModule);
          procedure DoHandleException(E : Exception);dynamic;
          function HandleRequest(Request : TWebRequest;Response : TWebResponse) : Boolean;
       public
          constructor Create(AOwner : TComponent);override;
          procedure CreateForm(InstanceClass: TComponentClass;var Reference);virtual;
          destructor Destroy;override;
          procedure Initialize;virtual;
          procedure Run;virtual;
       end;

    function DosPathToUnixPath(const Path : string) : string;
    function UnixPathToDosPath(const Path : string) : string;

    function HTTPDecode(const str : String) : string;
    function HTTPEncode(const str : String) : string;

    function ParseDate(const DateStr : string) : TDateTime;
    procedure ExtractHTTPFields(Separators,WhiteSpace : TCharSet;
      Content : PChar;Strings : TStrings);
    procedure ExtractHeaderFields(Separators,WhiteSpace : TCharSet;
      Content: PChar;Strings : TStrings;Decode : Boolean);

    function StatusString(StatusCode : Integer) : string;

    const
       Application : TWebApp = nil;

  implementation

    function TWebApp.ActivateWebModule : TDataModule;

      begin
      end;

    procedure TWebApp.DeactivateWebModule(DataModule : TDataModule);

      begin
      end;

    procedure TWebApp.DoHandleException(E : Exception);

      begin
      end;

    function TWebApp.HandleRequest(Request : TWebRequest;Response : TWebResponse) : Boolean;

      begin
      end;

    constructor TWebApp.Create(AOwner : TComponent);

      begin
      end;

    procedure TWebApp.CreateForm(InstanceClass: TComponentClass;var Reference);

      begin
      end;

    destructor TWebApp.Destroy;

      begin
      end;

    procedure TWebApp.Initialize;

      begin
      end;

    procedure TWebApp.Run;

      begin
      end;

    function DosPathToUnixPath(const Path : string) : string;

      var
         i : integer;

      begin
         Result:=Path;
         for i:=1 to Length(Result) do
           if Result[i]='\' then
             Result[i]:='/';
      end;

    function UnixPathToDosPath(const Path : string) : string;

      var
         i : integer;

      begin
         Result:=Path;
         for i:=1 to Length(Result) do
           if Result[i]='/' then
             Result[i]:='\';
      end;

    function HTTPDecode(const str : String) : string;

      begin
      end;

    function HTTPEncode(const str : String) : string;

      const
         noconvert = ['A'..'Z','a'..'z','*','@','.',
           '.','_','-','0'..'9','$','!','''','(',')'];

      const
         hex2str : array[0..15] of char = '0123456789ABCDEF';

      var
         i : integer;
         c : char;
         s : shortstring;

      begin
         // allocate some space for the result
         SetLength(Result,Length(str));
         for i:=1 to length(str) do
           begin
              c:=str[i];
              if c in noconvert then
                Result:=Result+c;
              else if c=' ' then
                Result:=Result+'+'
              else
                Result:=Result+'%'+
                  hex2str[ord(c) shr 4]+
                  hex2str[ord(c) and $f];
           end;
      end;

    function ParseDate(const DateStr : string) : TDateTime;

      begin
      end;

    procedure ExtractHTTPFields(Separators,WhiteSpace : TCharSet;
      Content : PChar;Strings : TStrings);

      begin
         ExtractHeaderFields(Separators,WhiteSpace,Content,Strings,True);
      end;

    procedure ExtractHeaderFields(Separators,WhiteSpace : TCharSet;
      Content: PChar;Strings : TStrings;Decode : Boolean);

      begin
      end;

    function StatusString(StatusCode : Integer) : string;

      begin
      end;

end.
