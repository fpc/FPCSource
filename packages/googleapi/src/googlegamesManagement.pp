unit googlegamesManagement;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 9-5-15 13:22:54
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAchievementResetAllResponse = class;
  TAchievementResetMultipleForAllRequest = class;
  TAchievementResetResponse = class;
  TEventsResetMultipleForAllRequest = class;
  TGamesPlayedResource = class;
  TGamesPlayerExperienceInfoResource = class;
  TGamesPlayerLevelResource = class;
  THiddenPlayer = class;
  THiddenPlayerList = class;
  TPlayer = class;
  TPlayerScoreResetAllResponse = class;
  TPlayerScoreResetResponse = class;
  TQuestsResetMultipleForAllRequest = class;
  TScoresResetMultipleForAllRequest = class;
  TAchievementResetAllResponseArray = Array of TAchievementResetAllResponse;
  TAchievementResetMultipleForAllRequestArray = Array of TAchievementResetMultipleForAllRequest;
  TAchievementResetResponseArray = Array of TAchievementResetResponse;
  TEventsResetMultipleForAllRequestArray = Array of TEventsResetMultipleForAllRequest;
  TGamesPlayedResourceArray = Array of TGamesPlayedResource;
  TGamesPlayerExperienceInfoResourceArray = Array of TGamesPlayerExperienceInfoResource;
  TGamesPlayerLevelResourceArray = Array of TGamesPlayerLevelResource;
  THiddenPlayerArray = Array of THiddenPlayer;
  THiddenPlayerListArray = Array of THiddenPlayerList;
  TPlayerArray = Array of TPlayer;
  TPlayerScoreResetAllResponseArray = Array of TPlayerScoreResetAllResponse;
  TPlayerScoreResetResponseArray = Array of TPlayerScoreResetResponse;
  TQuestsResetMultipleForAllRequestArray = Array of TQuestsResetMultipleForAllRequest;
  TScoresResetMultipleForAllRequestArray = Array of TScoresResetMultipleForAllRequest;
  //Anonymous types, using auto-generated names
  TPlayerTypename = class;
  TAchievementResetAllResponseTyperesultsArray = Array of TAchievementResetResponse;
  THiddenPlayerListTypeitemsArray = Array of THiddenPlayer;
  TPlayerScoreResetAllResponseTyperesultsArray = Array of TPlayerScoreResetResponse;
  
  { --------------------------------------------------------------------
    TAchievementResetAllResponse
    --------------------------------------------------------------------}
  
  TAchievementResetAllResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fresults : TAchievementResetAllResponseTyperesultsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setresults(AIndex : Integer; AValue : TAchievementResetAllResponseTyperesultsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property results : TAchievementResetAllResponseTyperesultsArray Index 8 Read Fresults Write Setresults;
  end;
  TAchievementResetAllResponseClass = Class of TAchievementResetAllResponse;
  
  { --------------------------------------------------------------------
    TAchievementResetMultipleForAllRequest
    --------------------------------------------------------------------}
  
  TAchievementResetMultipleForAllRequest = Class(TGoogleBaseObject)
  Private
    Fachievement_ids : TStringArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setachievement_ids(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property achievement_ids : TStringArray Index 0 Read Fachievement_ids Write Setachievement_ids;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAchievementResetMultipleForAllRequestClass = Class of TAchievementResetMultipleForAllRequest;
  
  { --------------------------------------------------------------------
    TAchievementResetResponse
    --------------------------------------------------------------------}
  
  TAchievementResetResponse = Class(TGoogleBaseObject)
  Private
    FcurrentState : String;
    FdefinitionId : String;
    Fkind : String;
    FupdateOccurred : boolean;
  Protected
    //Property setters
    Procedure SetcurrentState(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefinitionId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetupdateOccurred(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property currentState : String Index 0 Read FcurrentState Write SetcurrentState;
    Property definitionId : String Index 8 Read FdefinitionId Write SetdefinitionId;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property updateOccurred : boolean Index 24 Read FupdateOccurred Write SetupdateOccurred;
  end;
  TAchievementResetResponseClass = Class of TAchievementResetResponse;
  
  { --------------------------------------------------------------------
    TEventsResetMultipleForAllRequest
    --------------------------------------------------------------------}
  
  TEventsResetMultipleForAllRequest = Class(TGoogleBaseObject)
  Private
    Fevent_ids : TStringArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setevent_ids(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property event_ids : TStringArray Index 0 Read Fevent_ids Write Setevent_ids;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TEventsResetMultipleForAllRequestClass = Class of TEventsResetMultipleForAllRequest;
  
  { --------------------------------------------------------------------
    TGamesPlayedResource
    --------------------------------------------------------------------}
  
  TGamesPlayedResource = Class(TGoogleBaseObject)
  Private
    FautoMatched : boolean;
    FtimeMillis : String;
  Protected
    //Property setters
    Procedure SetautoMatched(AIndex : Integer; AValue : boolean); virtual;
    Procedure SettimeMillis(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoMatched : boolean Index 0 Read FautoMatched Write SetautoMatched;
    Property timeMillis : String Index 8 Read FtimeMillis Write SettimeMillis;
  end;
  TGamesPlayedResourceClass = Class of TGamesPlayedResource;
  
  { --------------------------------------------------------------------
    TGamesPlayerExperienceInfoResource
    --------------------------------------------------------------------}
  
  TGamesPlayerExperienceInfoResource = Class(TGoogleBaseObject)
  Private
    FcurrentExperiencePoints : String;
    FcurrentLevel : TGamesPlayerLevelResource;
    FlastLevelUpTimestampMillis : String;
    FnextLevel : TGamesPlayerLevelResource;
  Protected
    //Property setters
    Procedure SetcurrentExperiencePoints(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrentLevel(AIndex : Integer; AValue : TGamesPlayerLevelResource); virtual;
    Procedure SetlastLevelUpTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLevel(AIndex : Integer; AValue : TGamesPlayerLevelResource); virtual;
  Public
  Published
    Property currentExperiencePoints : String Index 0 Read FcurrentExperiencePoints Write SetcurrentExperiencePoints;
    Property currentLevel : TGamesPlayerLevelResource Index 8 Read FcurrentLevel Write SetcurrentLevel;
    Property lastLevelUpTimestampMillis : String Index 16 Read FlastLevelUpTimestampMillis Write SetlastLevelUpTimestampMillis;
    Property nextLevel : TGamesPlayerLevelResource Index 24 Read FnextLevel Write SetnextLevel;
  end;
  TGamesPlayerExperienceInfoResourceClass = Class of TGamesPlayerExperienceInfoResource;
  
  { --------------------------------------------------------------------
    TGamesPlayerLevelResource
    --------------------------------------------------------------------}
  
  TGamesPlayerLevelResource = Class(TGoogleBaseObject)
  Private
    Flevel : integer;
    FmaxExperiencePoints : String;
    FminExperiencePoints : String;
  Protected
    //Property setters
    Procedure Setlevel(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxExperiencePoints(AIndex : Integer; AValue : String); virtual;
    Procedure SetminExperiencePoints(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property level : integer Index 0 Read Flevel Write Setlevel;
    Property maxExperiencePoints : String Index 8 Read FmaxExperiencePoints Write SetmaxExperiencePoints;
    Property minExperiencePoints : String Index 16 Read FminExperiencePoints Write SetminExperiencePoints;
  end;
  TGamesPlayerLevelResourceClass = Class of TGamesPlayerLevelResource;
  
  { --------------------------------------------------------------------
    THiddenPlayer
    --------------------------------------------------------------------}
  
  THiddenPlayer = Class(TGoogleBaseObject)
  Private
    FhiddenTimeMillis : String;
    Fkind : String;
    Fplayer : TPlayer;
  Protected
    //Property setters
    Procedure SethiddenTimeMillis(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setplayer(AIndex : Integer; AValue : TPlayer); virtual;
  Public
  Published
    Property hiddenTimeMillis : String Index 0 Read FhiddenTimeMillis Write SethiddenTimeMillis;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property player : TPlayer Index 16 Read Fplayer Write Setplayer;
  end;
  THiddenPlayerClass = Class of THiddenPlayer;
  
  { --------------------------------------------------------------------
    THiddenPlayerList
    --------------------------------------------------------------------}
  
  THiddenPlayerList = Class(TGoogleBaseObject)
  Private
    Fitems : THiddenPlayerListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : THiddenPlayerListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : THiddenPlayerListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  THiddenPlayerListClass = Class of THiddenPlayerList;
  
  { --------------------------------------------------------------------
    TPlayerTypename
    --------------------------------------------------------------------}
  
  TPlayerTypename = Class(TGoogleBaseObject)
  Private
    FfamilyName : String;
    FgivenName : String;
  Protected
    //Property setters
    Procedure SetfamilyName(AIndex : Integer; AValue : String); virtual;
    Procedure SetgivenName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property familyName : String Index 0 Read FfamilyName Write SetfamilyName;
    Property givenName : String Index 8 Read FgivenName Write SetgivenName;
  end;
  TPlayerTypenameClass = Class of TPlayerTypename;
  
  { --------------------------------------------------------------------
    TPlayer
    --------------------------------------------------------------------}
  
  TPlayer = Class(TGoogleBaseObject)
  Private
    FavatarImageUrl : String;
    FdisplayName : String;
    FexperienceInfo : TGamesPlayerExperienceInfoResource;
    Fkind : String;
    FlastPlayedWith : TGamesPlayedResource;
    Fname : TPlayerTypename;
    FplayerId : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetavatarImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure SetexperienceInfo(AIndex : Integer; AValue : TGamesPlayerExperienceInfoResource); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastPlayedWith(AIndex : Integer; AValue : TGamesPlayedResource); virtual;
    Procedure Setname(AIndex : Integer; AValue : TPlayerTypename); virtual;
    Procedure SetplayerId(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property avatarImageUrl : String Index 0 Read FavatarImageUrl Write SetavatarImageUrl;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property experienceInfo : TGamesPlayerExperienceInfoResource Index 16 Read FexperienceInfo Write SetexperienceInfo;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property lastPlayedWith : TGamesPlayedResource Index 32 Read FlastPlayedWith Write SetlastPlayedWith;
    Property name : TPlayerTypename Index 40 Read Fname Write Setname;
    Property playerId : String Index 48 Read FplayerId Write SetplayerId;
    Property title : String Index 56 Read Ftitle Write Settitle;
  end;
  TPlayerClass = Class of TPlayer;
  
  { --------------------------------------------------------------------
    TPlayerScoreResetAllResponse
    --------------------------------------------------------------------}
  
  TPlayerScoreResetAllResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fresults : TPlayerScoreResetAllResponseTyperesultsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setresults(AIndex : Integer; AValue : TPlayerScoreResetAllResponseTyperesultsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property results : TPlayerScoreResetAllResponseTyperesultsArray Index 8 Read Fresults Write Setresults;
  end;
  TPlayerScoreResetAllResponseClass = Class of TPlayerScoreResetAllResponse;
  
  { --------------------------------------------------------------------
    TPlayerScoreResetResponse
    --------------------------------------------------------------------}
  
  TPlayerScoreResetResponse = Class(TGoogleBaseObject)
  Private
    FdefinitionId : String;
    Fkind : String;
    FresetScoreTimeSpans : TStringArray;
  Protected
    //Property setters
    Procedure SetdefinitionId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetresetScoreTimeSpans(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property definitionId : String Index 0 Read FdefinitionId Write SetdefinitionId;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property resetScoreTimeSpans : TStringArray Index 16 Read FresetScoreTimeSpans Write SetresetScoreTimeSpans;
  end;
  TPlayerScoreResetResponseClass = Class of TPlayerScoreResetResponse;
  
  { --------------------------------------------------------------------
    TQuestsResetMultipleForAllRequest
    --------------------------------------------------------------------}
  
  TQuestsResetMultipleForAllRequest = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fquest_ids : TStringArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setquest_ids(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property quest_ids : TStringArray Index 8 Read Fquest_ids Write Setquest_ids;
  end;
  TQuestsResetMultipleForAllRequestClass = Class of TQuestsResetMultipleForAllRequest;
  
  { --------------------------------------------------------------------
    TScoresResetMultipleForAllRequest
    --------------------------------------------------------------------}
  
  TScoresResetMultipleForAllRequest = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fleaderboard_ids : TStringArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setleaderboard_ids(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property leaderboard_ids : TStringArray Index 8 Read Fleaderboard_ids Write Setleaderboard_ids;
  end;
  TScoresResetMultipleForAllRequestClass = Class of TScoresResetMultipleForAllRequest;
  
  { --------------------------------------------------------------------
    TAchievementsResource
    --------------------------------------------------------------------}
  
  TAchievementsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Reset(achievementId: string) : TAchievementResetResponse;
    Function ResetAll : TAchievementResetAllResponse;
    Procedure ResetAllForAllPlayers;
    Procedure ResetForAllPlayers(achievementId: string);
    Procedure ResetMultipleForAllPlayers(aAchievementResetMultipleForAllRequest : TAchievementResetMultipleForAllRequest);
  end;
  
  
  { --------------------------------------------------------------------
    TApplicationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TApplicationsResource, method ListHidden
  
  TApplicationsListHiddenOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TApplicationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function ListHidden(applicationId: string; AQuery : string  = '') : THiddenPlayerList;
    Function ListHidden(applicationId: string; AQuery : TApplicationslistHiddenOptions) : THiddenPlayerList;
  end;
  
  
  { --------------------------------------------------------------------
    TEventsResource
    --------------------------------------------------------------------}
  
  TEventsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Reset(eventId: string);
    Procedure ResetAll;
    Procedure ResetAllForAllPlayers;
    Procedure ResetForAllPlayers(eventId: string);
    Procedure ResetMultipleForAllPlayers(aEventsResetMultipleForAllRequest : TEventsResetMultipleForAllRequest);
  end;
  
  
  { --------------------------------------------------------------------
    TPlayersResource
    --------------------------------------------------------------------}
  
  TPlayersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Hide(applicationId: string; playerId: string);
    Procedure Unhide(applicationId: string; playerId: string);
  end;
  
  
  { --------------------------------------------------------------------
    TQuestsResource
    --------------------------------------------------------------------}
  
  TQuestsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Reset(questId: string);
    Procedure ResetAll;
    Procedure ResetAllForAllPlayers;
    Procedure ResetForAllPlayers(questId: string);
    Procedure ResetMultipleForAllPlayers(aQuestsResetMultipleForAllRequest : TQuestsResetMultipleForAllRequest);
  end;
  
  
  { --------------------------------------------------------------------
    TRoomsResource
    --------------------------------------------------------------------}
  
  TRoomsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Reset;
    Procedure ResetForAllPlayers;
  end;
  
  
  { --------------------------------------------------------------------
    TScoresResource
    --------------------------------------------------------------------}
  
  TScoresResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Reset(leaderboardId: string) : TPlayerScoreResetResponse;
    Function ResetAll : TPlayerScoreResetAllResponse;
    Procedure ResetAllForAllPlayers;
    Procedure ResetForAllPlayers(leaderboardId: string);
    Procedure ResetMultipleForAllPlayers(aScoresResetMultipleForAllRequest : TScoresResetMultipleForAllRequest);
  end;
  
  
  { --------------------------------------------------------------------
    TTurnBasedMatchesResource
    --------------------------------------------------------------------}
  
  TTurnBasedMatchesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Reset;
    Procedure ResetForAllPlayers;
  end;
  
  
  { --------------------------------------------------------------------
    TGamesManagementAPI
    --------------------------------------------------------------------}
  
  TGamesManagementAPI = Class(TGoogleAPI)
  Private
    FAchievementsInstance : TAchievementsResource;
    FApplicationsInstance : TApplicationsResource;
    FEventsInstance : TEventsResource;
    FPlayersInstance : TPlayersResource;
    FQuestsInstance : TQuestsResource;
    FRoomsInstance : TRoomsResource;
    FScoresInstance : TScoresResource;
    FTurnBasedMatchesInstance : TTurnBasedMatchesResource;
    Function GetAchievementsInstance : TAchievementsResource;virtual;
    Function GetApplicationsInstance : TApplicationsResource;virtual;
    Function GetEventsInstance : TEventsResource;virtual;
    Function GetPlayersInstance : TPlayersResource;virtual;
    Function GetQuestsInstance : TQuestsResource;virtual;
    Function GetRoomsInstance : TRoomsResource;virtual;
    Function GetScoresInstance : TScoresResource;virtual;
    Function GetTurnBasedMatchesInstance : TTurnBasedMatchesResource;virtual;
  Public
    //Override class functions with API info
    Class Function APIName : String; override;
    Class Function APIVersion : String; override;
    Class Function APIRevision : String; override;
    Class Function APIID : String; override;
    Class Function APITitle : String; override;
    Class Function APIDescription : String; override;
    Class Function APIOwnerDomain : String; override;
    Class Function APIOwnerName : String; override;
    Class Function APIIcon16 : String; override;
    Class Function APIIcon32 : String; override;
    Class Function APIdocumentationLink : String; override;
    Class Function APIrootUrl : string; override;
    Class Function APIbasePath : string;override;
    Class Function APIbaseURL : String;override;
    Class Function APIProtocol : string;override;
    Class Function APIservicePath : string;override;
    Class Function APIbatchPath : String;override;
    Class Function APIAuthScopes : TScopeInfoArray;override;
    Class Function APINeedsAuth : Boolean;override;
    Class Procedure RegisterAPIResources; override;
    //Add create function for resources
    Function CreateAchievementsResource(AOwner : TComponent) : TAchievementsResource;virtual;overload;
    Function CreateAchievementsResource : TAchievementsResource;virtual;overload;
    Function CreateApplicationsResource(AOwner : TComponent) : TApplicationsResource;virtual;overload;
    Function CreateApplicationsResource : TApplicationsResource;virtual;overload;
    Function CreateEventsResource(AOwner : TComponent) : TEventsResource;virtual;overload;
    Function CreateEventsResource : TEventsResource;virtual;overload;
    Function CreatePlayersResource(AOwner : TComponent) : TPlayersResource;virtual;overload;
    Function CreatePlayersResource : TPlayersResource;virtual;overload;
    Function CreateQuestsResource(AOwner : TComponent) : TQuestsResource;virtual;overload;
    Function CreateQuestsResource : TQuestsResource;virtual;overload;
    Function CreateRoomsResource(AOwner : TComponent) : TRoomsResource;virtual;overload;
    Function CreateRoomsResource : TRoomsResource;virtual;overload;
    Function CreateScoresResource(AOwner : TComponent) : TScoresResource;virtual;overload;
    Function CreateScoresResource : TScoresResource;virtual;overload;
    Function CreateTurnBasedMatchesResource(AOwner : TComponent) : TTurnBasedMatchesResource;virtual;overload;
    Function CreateTurnBasedMatchesResource : TTurnBasedMatchesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AchievementsResource : TAchievementsResource Read GetAchievementsInstance;
    Property ApplicationsResource : TApplicationsResource Read GetApplicationsInstance;
    Property EventsResource : TEventsResource Read GetEventsInstance;
    Property PlayersResource : TPlayersResource Read GetPlayersInstance;
    Property QuestsResource : TQuestsResource Read GetQuestsInstance;
    Property RoomsResource : TRoomsResource Read GetRoomsInstance;
    Property ScoresResource : TScoresResource Read GetScoresInstance;
    Property TurnBasedMatchesResource : TTurnBasedMatchesResource Read GetTurnBasedMatchesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAchievementResetAllResponse
  --------------------------------------------------------------------}


Procedure TAchievementResetAllResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementResetAllResponse.Setresults(AIndex : Integer; AValue : TAchievementResetAllResponseTyperesultsArray); 

begin
  If (Fresults=AValue) then exit;
  Fresults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementResetMultipleForAllRequest
  --------------------------------------------------------------------}


Procedure TAchievementResetMultipleForAllRequest.Setachievement_ids(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fachievement_ids=AValue) then exit;
  Fachievement_ids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementResetMultipleForAllRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementResetResponse
  --------------------------------------------------------------------}


Procedure TAchievementResetResponse.SetcurrentState(AIndex : Integer; AValue : String); 

begin
  If (FcurrentState=AValue) then exit;
  FcurrentState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementResetResponse.SetdefinitionId(AIndex : Integer; AValue : String); 

begin
  If (FdefinitionId=AValue) then exit;
  FdefinitionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementResetResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementResetResponse.SetupdateOccurred(AIndex : Integer; AValue : boolean); 

begin
  If (FupdateOccurred=AValue) then exit;
  FupdateOccurred:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventsResetMultipleForAllRequest
  --------------------------------------------------------------------}


Procedure TEventsResetMultipleForAllRequest.Setevent_ids(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fevent_ids=AValue) then exit;
  Fevent_ids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventsResetMultipleForAllRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGamesPlayedResource
  --------------------------------------------------------------------}


Procedure TGamesPlayedResource.SetautoMatched(AIndex : Integer; AValue : boolean); 

begin
  If (FautoMatched=AValue) then exit;
  FautoMatched:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesPlayedResource.SettimeMillis(AIndex : Integer; AValue : String); 

begin
  If (FtimeMillis=AValue) then exit;
  FtimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGamesPlayerExperienceInfoResource
  --------------------------------------------------------------------}


Procedure TGamesPlayerExperienceInfoResource.SetcurrentExperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FcurrentExperiencePoints=AValue) then exit;
  FcurrentExperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesPlayerExperienceInfoResource.SetcurrentLevel(AIndex : Integer; AValue : TGamesPlayerLevelResource); 

begin
  If (FcurrentLevel=AValue) then exit;
  FcurrentLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesPlayerExperienceInfoResource.SetlastLevelUpTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FlastLevelUpTimestampMillis=AValue) then exit;
  FlastLevelUpTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesPlayerExperienceInfoResource.SetnextLevel(AIndex : Integer; AValue : TGamesPlayerLevelResource); 

begin
  If (FnextLevel=AValue) then exit;
  FnextLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGamesPlayerLevelResource
  --------------------------------------------------------------------}


Procedure TGamesPlayerLevelResource.Setlevel(AIndex : Integer; AValue : integer); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesPlayerLevelResource.SetmaxExperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FmaxExperiencePoints=AValue) then exit;
  FmaxExperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesPlayerLevelResource.SetminExperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FminExperiencePoints=AValue) then exit;
  FminExperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THiddenPlayer
  --------------------------------------------------------------------}


Procedure THiddenPlayer.SethiddenTimeMillis(AIndex : Integer; AValue : String); 

begin
  If (FhiddenTimeMillis=AValue) then exit;
  FhiddenTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiddenPlayer.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiddenPlayer.Setplayer(AIndex : Integer; AValue : TPlayer); 

begin
  If (Fplayer=AValue) then exit;
  Fplayer:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THiddenPlayerList
  --------------------------------------------------------------------}


Procedure THiddenPlayerList.Setitems(AIndex : Integer; AValue : THiddenPlayerListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiddenPlayerList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiddenPlayerList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerTypename
  --------------------------------------------------------------------}


Procedure TPlayerTypename.SetfamilyName(AIndex : Integer; AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerTypename.SetgivenName(AIndex : Integer; AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayer
  --------------------------------------------------------------------}


Procedure TPlayer.SetavatarImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FavatarImageUrl=AValue) then exit;
  FavatarImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayer.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayer.SetexperienceInfo(AIndex : Integer; AValue : TGamesPlayerExperienceInfoResource); 

begin
  If (FexperienceInfo=AValue) then exit;
  FexperienceInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayer.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayer.SetlastPlayedWith(AIndex : Integer; AValue : TGamesPlayedResource); 

begin
  If (FlastPlayedWith=AValue) then exit;
  FlastPlayedWith:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayer.Setname(AIndex : Integer; AValue : TPlayerTypename); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayer.SetplayerId(AIndex : Integer; AValue : String); 

begin
  If (FplayerId=AValue) then exit;
  FplayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayer.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerScoreResetAllResponse
  --------------------------------------------------------------------}


Procedure TPlayerScoreResetAllResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreResetAllResponse.Setresults(AIndex : Integer; AValue : TPlayerScoreResetAllResponseTyperesultsArray); 

begin
  If (Fresults=AValue) then exit;
  Fresults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerScoreResetResponse
  --------------------------------------------------------------------}


Procedure TPlayerScoreResetResponse.SetdefinitionId(AIndex : Integer; AValue : String); 

begin
  If (FdefinitionId=AValue) then exit;
  FdefinitionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreResetResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreResetResponse.SetresetScoreTimeSpans(AIndex : Integer; AValue : TStringArray); 

begin
  If (FresetScoreTimeSpans=AValue) then exit;
  FresetScoreTimeSpans:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuestsResetMultipleForAllRequest
  --------------------------------------------------------------------}


Procedure TQuestsResetMultipleForAllRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestsResetMultipleForAllRequest.Setquest_ids(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fquest_ids=AValue) then exit;
  Fquest_ids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TScoresResetMultipleForAllRequest
  --------------------------------------------------------------------}


Procedure TScoresResetMultipleForAllRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScoresResetMultipleForAllRequest.Setleaderboard_ids(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fleaderboard_ids=AValue) then exit;
  Fleaderboard_ids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementsResource
  --------------------------------------------------------------------}


Class Function TAchievementsResource.ResourceName : String;

begin
  Result:='achievements';
end;

Class Function TAchievementsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesManagementAPI;
end;

Function TAchievementsResource.Reset(achievementId: string) : TAchievementResetResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/{achievementId}/reset';
  _Methodid   = 'gamesManagement.achievements.reset';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAchievementResetResponse) as TAchievementResetResponse;
end;

Function TAchievementsResource.ResetAll : TAchievementResetAllResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/reset';
  _Methodid   = 'gamesManagement.achievements.resetAll';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TAchievementResetAllResponse) as TAchievementResetAllResponse;
end;

Procedure TAchievementsResource.ResetAllForAllPlayers;

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/resetAllForAllPlayers';
  _Methodid   = 'gamesManagement.achievements.resetAllForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TAchievementsResource.ResetForAllPlayers(achievementId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/{achievementId}/resetForAllPlayers';
  _Methodid   = 'gamesManagement.achievements.resetForAllPlayers';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TAchievementsResource.ResetMultipleForAllPlayers(aAchievementResetMultipleForAllRequest : TAchievementResetMultipleForAllRequest);

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/resetMultipleForAllPlayers';
  _Methodid   = 'gamesManagement.achievements.resetMultipleForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',aAchievementResetMultipleForAllRequest,Nil);
end;



{ --------------------------------------------------------------------
  TApplicationsResource
  --------------------------------------------------------------------}


Class Function TApplicationsResource.ResourceName : String;

begin
  Result:='applications';
end;

Class Function TApplicationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesManagementAPI;
end;

Function TApplicationsResource.ListHidden(applicationId: string; AQuery : string = '') : THiddenPlayerList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'applications/{applicationId}/players/hidden';
  _Methodid   = 'gamesManagement.applications.listHidden';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,THiddenPlayerList) as THiddenPlayerList;
end;


Function TApplicationsResource.ListHidden(applicationId: string; AQuery : TApplicationslistHiddenOptions) : THiddenPlayerList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListHidden(applicationId,_Q);
end;



{ --------------------------------------------------------------------
  TEventsResource
  --------------------------------------------------------------------}


Class Function TEventsResource.ResourceName : String;

begin
  Result:='events';
end;

Class Function TEventsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesManagementAPI;
end;

Procedure TEventsResource.Reset(eventId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'events/{eventId}/reset';
  _Methodid   = 'gamesManagement.events.reset';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['eventId',eventId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TEventsResource.ResetAll;

Const
  _HTTPMethod = 'POST';
  _Path       = 'events/reset';
  _Methodid   = 'gamesManagement.events.resetAll';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TEventsResource.ResetAllForAllPlayers;

Const
  _HTTPMethod = 'POST';
  _Path       = 'events/resetAllForAllPlayers';
  _Methodid   = 'gamesManagement.events.resetAllForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TEventsResource.ResetForAllPlayers(eventId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'events/{eventId}/resetForAllPlayers';
  _Methodid   = 'gamesManagement.events.resetForAllPlayers';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['eventId',eventId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TEventsResource.ResetMultipleForAllPlayers(aEventsResetMultipleForAllRequest : TEventsResetMultipleForAllRequest);

Const
  _HTTPMethod = 'POST';
  _Path       = 'events/resetMultipleForAllPlayers';
  _Methodid   = 'gamesManagement.events.resetMultipleForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',aEventsResetMultipleForAllRequest,Nil);
end;



{ --------------------------------------------------------------------
  TPlayersResource
  --------------------------------------------------------------------}


Class Function TPlayersResource.ResourceName : String;

begin
  Result:='players';
end;

Class Function TPlayersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesManagementAPI;
end;

Procedure TPlayersResource.Hide(applicationId: string; playerId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'applications/{applicationId}/players/hidden/{playerId}';
  _Methodid   = 'gamesManagement.players.hide';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId,'playerId',playerId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TPlayersResource.Unhide(applicationId: string; playerId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'applications/{applicationId}/players/hidden/{playerId}';
  _Methodid   = 'gamesManagement.players.unhide';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId,'playerId',playerId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;



{ --------------------------------------------------------------------
  TQuestsResource
  --------------------------------------------------------------------}


Class Function TQuestsResource.ResourceName : String;

begin
  Result:='quests';
end;

Class Function TQuestsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesManagementAPI;
end;

Procedure TQuestsResource.Reset(questId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'quests/{questId}/reset';
  _Methodid   = 'gamesManagement.quests.reset';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['questId',questId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TQuestsResource.ResetAll;

Const
  _HTTPMethod = 'POST';
  _Path       = 'quests/reset';
  _Methodid   = 'gamesManagement.quests.resetAll';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TQuestsResource.ResetAllForAllPlayers;

Const
  _HTTPMethod = 'POST';
  _Path       = 'quests/resetAllForAllPlayers';
  _Methodid   = 'gamesManagement.quests.resetAllForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TQuestsResource.ResetForAllPlayers(questId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'quests/{questId}/resetForAllPlayers';
  _Methodid   = 'gamesManagement.quests.resetForAllPlayers';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['questId',questId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TQuestsResource.ResetMultipleForAllPlayers(aQuestsResetMultipleForAllRequest : TQuestsResetMultipleForAllRequest);

Const
  _HTTPMethod = 'POST';
  _Path       = 'quests/resetMultipleForAllPlayers';
  _Methodid   = 'gamesManagement.quests.resetMultipleForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',aQuestsResetMultipleForAllRequest,Nil);
end;



{ --------------------------------------------------------------------
  TRoomsResource
  --------------------------------------------------------------------}


Class Function TRoomsResource.ResourceName : String;

begin
  Result:='rooms';
end;

Class Function TRoomsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesManagementAPI;
end;

Procedure TRoomsResource.Reset;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rooms/reset';
  _Methodid   = 'gamesManagement.rooms.reset';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TRoomsResource.ResetForAllPlayers;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rooms/resetForAllPlayers';
  _Methodid   = 'gamesManagement.rooms.resetForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;



{ --------------------------------------------------------------------
  TScoresResource
  --------------------------------------------------------------------}


Class Function TScoresResource.ResourceName : String;

begin
  Result:='scores';
end;

Class Function TScoresResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesManagementAPI;
end;

Function TScoresResource.Reset(leaderboardId: string) : TPlayerScoreResetResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'leaderboards/{leaderboardId}/scores/reset';
  _Methodid   = 'gamesManagement.scores.reset';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPlayerScoreResetResponse) as TPlayerScoreResetResponse;
end;

Function TScoresResource.ResetAll : TPlayerScoreResetAllResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'scores/reset';
  _Methodid   = 'gamesManagement.scores.resetAll';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TPlayerScoreResetAllResponse) as TPlayerScoreResetAllResponse;
end;

Procedure TScoresResource.ResetAllForAllPlayers;

Const
  _HTTPMethod = 'POST';
  _Path       = 'scores/resetAllForAllPlayers';
  _Methodid   = 'gamesManagement.scores.resetAllForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TScoresResource.ResetForAllPlayers(leaderboardId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'leaderboards/{leaderboardId}/scores/resetForAllPlayers';
  _Methodid   = 'gamesManagement.scores.resetForAllPlayers';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TScoresResource.ResetMultipleForAllPlayers(aScoresResetMultipleForAllRequest : TScoresResetMultipleForAllRequest);

Const
  _HTTPMethod = 'POST';
  _Path       = 'scores/resetMultipleForAllPlayers';
  _Methodid   = 'gamesManagement.scores.resetMultipleForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',aScoresResetMultipleForAllRequest,Nil);
end;



{ --------------------------------------------------------------------
  TTurnBasedMatchesResource
  --------------------------------------------------------------------}


Class Function TTurnBasedMatchesResource.ResourceName : String;

begin
  Result:='turnBasedMatches';
end;

Class Function TTurnBasedMatchesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesManagementAPI;
end;

Procedure TTurnBasedMatchesResource.Reset;

Const
  _HTTPMethod = 'POST';
  _Path       = 'turnbasedmatches/reset';
  _Methodid   = 'gamesManagement.turnBasedMatches.reset';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TTurnBasedMatchesResource.ResetForAllPlayers;

Const
  _HTTPMethod = 'POST';
  _Path       = 'turnbasedmatches/resetForAllPlayers';
  _Methodid   = 'gamesManagement.turnBasedMatches.resetForAllPlayers';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;



{ --------------------------------------------------------------------
  TGamesManagementAPI
  --------------------------------------------------------------------}

Class Function TGamesManagementAPI.APIName : String;

begin
  Result:='gamesManagement';
end;

Class Function TGamesManagementAPI.APIVersion : String;

begin
  Result:='v1management';
end;

Class Function TGamesManagementAPI.APIRevision : String;

begin
  Result:='20150421';
end;

Class Function TGamesManagementAPI.APIID : String;

begin
  Result:='gamesManagement:v1management';
end;

Class Function TGamesManagementAPI.APITitle : String;

begin
  Result:='Google Play Game Services Management API';
end;

Class Function TGamesManagementAPI.APIDescription : String;

begin
  Result:='The Management API for Google Play Game Services.';
end;

Class Function TGamesManagementAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TGamesManagementAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TGamesManagementAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TGamesManagementAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TGamesManagementAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/games/services';
end;

Class Function TGamesManagementAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TGamesManagementAPI.APIbasePath : string;

begin
  Result:='/games/v1management/';
end;

Class Function TGamesManagementAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/games/v1management/';
end;

Class Function TGamesManagementAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGamesManagementAPI.APIservicePath : string;

begin
  Result:='games/v1management/';
end;

Class Function TGamesManagementAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TGamesManagementAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/games';
  Result[0].Description:='Share your Google+ profile information and view and manage your game activity';
  Result[1].Name:='https://www.googleapis.com/auth/plus.login';
  Result[1].Description:='Know your basic profile info and list of people in your circles.';
  
end;

Class Function TGamesManagementAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TGamesManagementAPI.RegisterAPIResources;

begin
  TAchievementResetAllResponse.RegisterObject;
  TAchievementResetMultipleForAllRequest.RegisterObject;
  TAchievementResetResponse.RegisterObject;
  TEventsResetMultipleForAllRequest.RegisterObject;
  TGamesPlayedResource.RegisterObject;
  TGamesPlayerExperienceInfoResource.RegisterObject;
  TGamesPlayerLevelResource.RegisterObject;
  THiddenPlayer.RegisterObject;
  THiddenPlayerList.RegisterObject;
  TPlayerTypename.RegisterObject;
  TPlayer.RegisterObject;
  TPlayerScoreResetAllResponse.RegisterObject;
  TPlayerScoreResetResponse.RegisterObject;
  TQuestsResetMultipleForAllRequest.RegisterObject;
  TScoresResetMultipleForAllRequest.RegisterObject;
end;


Function TGamesManagementAPI.GetAchievementsInstance : TAchievementsResource;

begin
  if (FAchievementsInstance=Nil) then
    FAchievementsInstance:=CreateAchievementsResource;
  Result:=FAchievementsInstance;
end;

Function TGamesManagementAPI.CreateAchievementsResource : TAchievementsResource;

begin
  Result:=CreateAchievementsResource(Self);
end;


Function TGamesManagementAPI.CreateAchievementsResource(AOwner : TComponent) : TAchievementsResource;

begin
  Result:=TAchievementsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesManagementAPI.GetApplicationsInstance : TApplicationsResource;

begin
  if (FApplicationsInstance=Nil) then
    FApplicationsInstance:=CreateApplicationsResource;
  Result:=FApplicationsInstance;
end;

Function TGamesManagementAPI.CreateApplicationsResource : TApplicationsResource;

begin
  Result:=CreateApplicationsResource(Self);
end;


Function TGamesManagementAPI.CreateApplicationsResource(AOwner : TComponent) : TApplicationsResource;

begin
  Result:=TApplicationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesManagementAPI.GetEventsInstance : TEventsResource;

begin
  if (FEventsInstance=Nil) then
    FEventsInstance:=CreateEventsResource;
  Result:=FEventsInstance;
end;

Function TGamesManagementAPI.CreateEventsResource : TEventsResource;

begin
  Result:=CreateEventsResource(Self);
end;


Function TGamesManagementAPI.CreateEventsResource(AOwner : TComponent) : TEventsResource;

begin
  Result:=TEventsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesManagementAPI.GetPlayersInstance : TPlayersResource;

begin
  if (FPlayersInstance=Nil) then
    FPlayersInstance:=CreatePlayersResource;
  Result:=FPlayersInstance;
end;

Function TGamesManagementAPI.CreatePlayersResource : TPlayersResource;

begin
  Result:=CreatePlayersResource(Self);
end;


Function TGamesManagementAPI.CreatePlayersResource(AOwner : TComponent) : TPlayersResource;

begin
  Result:=TPlayersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesManagementAPI.GetQuestsInstance : TQuestsResource;

begin
  if (FQuestsInstance=Nil) then
    FQuestsInstance:=CreateQuestsResource;
  Result:=FQuestsInstance;
end;

Function TGamesManagementAPI.CreateQuestsResource : TQuestsResource;

begin
  Result:=CreateQuestsResource(Self);
end;


Function TGamesManagementAPI.CreateQuestsResource(AOwner : TComponent) : TQuestsResource;

begin
  Result:=TQuestsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesManagementAPI.GetRoomsInstance : TRoomsResource;

begin
  if (FRoomsInstance=Nil) then
    FRoomsInstance:=CreateRoomsResource;
  Result:=FRoomsInstance;
end;

Function TGamesManagementAPI.CreateRoomsResource : TRoomsResource;

begin
  Result:=CreateRoomsResource(Self);
end;


Function TGamesManagementAPI.CreateRoomsResource(AOwner : TComponent) : TRoomsResource;

begin
  Result:=TRoomsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesManagementAPI.GetScoresInstance : TScoresResource;

begin
  if (FScoresInstance=Nil) then
    FScoresInstance:=CreateScoresResource;
  Result:=FScoresInstance;
end;

Function TGamesManagementAPI.CreateScoresResource : TScoresResource;

begin
  Result:=CreateScoresResource(Self);
end;


Function TGamesManagementAPI.CreateScoresResource(AOwner : TComponent) : TScoresResource;

begin
  Result:=TScoresResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesManagementAPI.GetTurnBasedMatchesInstance : TTurnBasedMatchesResource;

begin
  if (FTurnBasedMatchesInstance=Nil) then
    FTurnBasedMatchesInstance:=CreateTurnBasedMatchesResource;
  Result:=FTurnBasedMatchesInstance;
end;

Function TGamesManagementAPI.CreateTurnBasedMatchesResource : TTurnBasedMatchesResource;

begin
  Result:=CreateTurnBasedMatchesResource(Self);
end;


Function TGamesManagementAPI.CreateTurnBasedMatchesResource(AOwner : TComponent) : TTurnBasedMatchesResource;

begin
  Result:=TTurnBasedMatchesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TGamesManagementAPI.RegisterAPI;
end.
