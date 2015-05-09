unit googlegames;
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
  TAchievementDefinition = class;
  TAchievementDefinitionsListResponse = class;
  TAchievementIncrementResponse = class;
  TAchievementRevealResponse = class;
  TAchievementSetStepsAtLeastResponse = class;
  TAchievementUnlockResponse = class;
  TAchievementUpdateMultipleRequest = class;
  TAchievementUpdateMultipleResponse = class;
  TAchievementUpdateRequest = class;
  TAchievementUpdateResponse = class;
  TAggregateStats = class;
  TAnonymousPlayer = class;
  TApplication = class;
  TApplicationCategory = class;
  TCategory = class;
  TCategoryListResponse = class;
  TEventBatchRecordFailure = class;
  TEventChild = class;
  TEventDefinition = class;
  TEventDefinitionListResponse = class;
  TEventPeriodRange = class;
  TEventPeriodUpdate = class;
  TEventRecordFailure = class;
  TEventRecordRequest = class;
  TEventUpdateRequest = class;
  TEventUpdateResponse = class;
  TGamesAchievementIncrement = class;
  TGamesAchievementSetStepsAtLeast = class;
  TImageAsset = class;
  TInstance = class;
  TInstanceAndroidDetails = class;
  TInstanceIosDetails = class;
  TInstanceWebDetails = class;
  TLeaderboard = class;
  TLeaderboardEntry = class;
  TLeaderboardListResponse = class;
  TLeaderboardScoreRank = class;
  TLeaderboardScores = class;
  TMetagameConfig = class;
  TNetworkDiagnostics = class;
  TParticipantResult = class;
  TPeerChannelDiagnostics = class;
  TPeerSessionDiagnostics = class;
  TPlayed = class;
  TPlayer = class;
  TPlayerAchievement = class;
  TPlayerAchievementListResponse = class;
  TPlayerEvent = class;
  TPlayerEventListResponse = class;
  TPlayerExperienceInfo = class;
  TPlayerLeaderboardScore = class;
  TPlayerLeaderboardScoreListResponse = class;
  TPlayerLevel = class;
  TPlayerListResponse = class;
  TPlayerScore = class;
  TPlayerScoreListResponse = class;
  TPlayerScoreResponse = class;
  TPlayerScoreSubmissionList = class;
  TPushToken = class;
  TPushTokenId = class;
  TQuest = class;
  TQuestContribution = class;
  TQuestCriterion = class;
  TQuestListResponse = class;
  TQuestMilestone = class;
  TRevisionCheckResponse = class;
  TRoom = class;
  TRoomAutoMatchStatus = class;
  TRoomAutoMatchingCriteria = class;
  TRoomClientAddress = class;
  TRoomCreateRequest = class;
  TRoomJoinRequest = class;
  TRoomLeaveDiagnostics = class;
  TRoomLeaveRequest = class;
  TRoomList = class;
  TRoomModification = class;
  TRoomP2PStatus = class;
  TRoomP2PStatuses = class;
  TRoomParticipant = class;
  TRoomStatus = class;
  TScoreSubmission = class;
  TSnapshot = class;
  TSnapshotImage = class;
  TSnapshotListResponse = class;
  TTurnBasedAutoMatchingCriteria = class;
  TTurnBasedMatch = class;
  TTurnBasedMatchCreateRequest = class;
  TTurnBasedMatchData = class;
  TTurnBasedMatchDataRequest = class;
  TTurnBasedMatchList = class;
  TTurnBasedMatchModification = class;
  TTurnBasedMatchParticipant = class;
  TTurnBasedMatchRematch = class;
  TTurnBasedMatchResults = class;
  TTurnBasedMatchSync = class;
  TTurnBasedMatchTurn = class;
  TAchievementDefinitionArray = Array of TAchievementDefinition;
  TAchievementDefinitionsListResponseArray = Array of TAchievementDefinitionsListResponse;
  TAchievementIncrementResponseArray = Array of TAchievementIncrementResponse;
  TAchievementRevealResponseArray = Array of TAchievementRevealResponse;
  TAchievementSetStepsAtLeastResponseArray = Array of TAchievementSetStepsAtLeastResponse;
  TAchievementUnlockResponseArray = Array of TAchievementUnlockResponse;
  TAchievementUpdateMultipleRequestArray = Array of TAchievementUpdateMultipleRequest;
  TAchievementUpdateMultipleResponseArray = Array of TAchievementUpdateMultipleResponse;
  TAchievementUpdateRequestArray = Array of TAchievementUpdateRequest;
  TAchievementUpdateResponseArray = Array of TAchievementUpdateResponse;
  TAggregateStatsArray = Array of TAggregateStats;
  TAnonymousPlayerArray = Array of TAnonymousPlayer;
  TApplicationArray = Array of TApplication;
  TApplicationCategoryArray = Array of TApplicationCategory;
  TCategoryArray = Array of TCategory;
  TCategoryListResponseArray = Array of TCategoryListResponse;
  TEventBatchRecordFailureArray = Array of TEventBatchRecordFailure;
  TEventChildArray = Array of TEventChild;
  TEventDefinitionArray = Array of TEventDefinition;
  TEventDefinitionListResponseArray = Array of TEventDefinitionListResponse;
  TEventPeriodRangeArray = Array of TEventPeriodRange;
  TEventPeriodUpdateArray = Array of TEventPeriodUpdate;
  TEventRecordFailureArray = Array of TEventRecordFailure;
  TEventRecordRequestArray = Array of TEventRecordRequest;
  TEventUpdateRequestArray = Array of TEventUpdateRequest;
  TEventUpdateResponseArray = Array of TEventUpdateResponse;
  TGamesAchievementIncrementArray = Array of TGamesAchievementIncrement;
  TGamesAchievementSetStepsAtLeastArray = Array of TGamesAchievementSetStepsAtLeast;
  TImageAssetArray = Array of TImageAsset;
  TInstanceArray = Array of TInstance;
  TInstanceAndroidDetailsArray = Array of TInstanceAndroidDetails;
  TInstanceIosDetailsArray = Array of TInstanceIosDetails;
  TInstanceWebDetailsArray = Array of TInstanceWebDetails;
  TLeaderboardArray = Array of TLeaderboard;
  TLeaderboardEntryArray = Array of TLeaderboardEntry;
  TLeaderboardListResponseArray = Array of TLeaderboardListResponse;
  TLeaderboardScoreRankArray = Array of TLeaderboardScoreRank;
  TLeaderboardScoresArray = Array of TLeaderboardScores;
  TMetagameConfigArray = Array of TMetagameConfig;
  TNetworkDiagnosticsArray = Array of TNetworkDiagnostics;
  TParticipantResultArray = Array of TParticipantResult;
  TPeerChannelDiagnosticsArray = Array of TPeerChannelDiagnostics;
  TPeerSessionDiagnosticsArray = Array of TPeerSessionDiagnostics;
  TPlayedArray = Array of TPlayed;
  TPlayerArray = Array of TPlayer;
  TPlayerAchievementArray = Array of TPlayerAchievement;
  TPlayerAchievementListResponseArray = Array of TPlayerAchievementListResponse;
  TPlayerEventArray = Array of TPlayerEvent;
  TPlayerEventListResponseArray = Array of TPlayerEventListResponse;
  TPlayerExperienceInfoArray = Array of TPlayerExperienceInfo;
  TPlayerLeaderboardScoreArray = Array of TPlayerLeaderboardScore;
  TPlayerLeaderboardScoreListResponseArray = Array of TPlayerLeaderboardScoreListResponse;
  TPlayerLevelArray = Array of TPlayerLevel;
  TPlayerListResponseArray = Array of TPlayerListResponse;
  TPlayerScoreArray = Array of TPlayerScore;
  TPlayerScoreListResponseArray = Array of TPlayerScoreListResponse;
  TPlayerScoreResponseArray = Array of TPlayerScoreResponse;
  TPlayerScoreSubmissionListArray = Array of TPlayerScoreSubmissionList;
  TPushTokenArray = Array of TPushToken;
  TPushTokenIdArray = Array of TPushTokenId;
  TQuestArray = Array of TQuest;
  TQuestContributionArray = Array of TQuestContribution;
  TQuestCriterionArray = Array of TQuestCriterion;
  TQuestListResponseArray = Array of TQuestListResponse;
  TQuestMilestoneArray = Array of TQuestMilestone;
  TRevisionCheckResponseArray = Array of TRevisionCheckResponse;
  TRoomArray = Array of TRoom;
  TRoomAutoMatchStatusArray = Array of TRoomAutoMatchStatus;
  TRoomAutoMatchingCriteriaArray = Array of TRoomAutoMatchingCriteria;
  TRoomClientAddressArray = Array of TRoomClientAddress;
  TRoomCreateRequestArray = Array of TRoomCreateRequest;
  TRoomJoinRequestArray = Array of TRoomJoinRequest;
  TRoomLeaveDiagnosticsArray = Array of TRoomLeaveDiagnostics;
  TRoomLeaveRequestArray = Array of TRoomLeaveRequest;
  TRoomListArray = Array of TRoomList;
  TRoomModificationArray = Array of TRoomModification;
  TRoomP2PStatusArray = Array of TRoomP2PStatus;
  TRoomP2PStatusesArray = Array of TRoomP2PStatuses;
  TRoomParticipantArray = Array of TRoomParticipant;
  TRoomStatusArray = Array of TRoomStatus;
  TScoreSubmissionArray = Array of TScoreSubmission;
  TSnapshotArray = Array of TSnapshot;
  TSnapshotImageArray = Array of TSnapshotImage;
  TSnapshotListResponseArray = Array of TSnapshotListResponse;
  TTurnBasedAutoMatchingCriteriaArray = Array of TTurnBasedAutoMatchingCriteria;
  TTurnBasedMatchArray = Array of TTurnBasedMatch;
  TTurnBasedMatchCreateRequestArray = Array of TTurnBasedMatchCreateRequest;
  TTurnBasedMatchDataArray = Array of TTurnBasedMatchData;
  TTurnBasedMatchDataRequestArray = Array of TTurnBasedMatchDataRequest;
  TTurnBasedMatchListArray = Array of TTurnBasedMatchList;
  TTurnBasedMatchModificationArray = Array of TTurnBasedMatchModification;
  TTurnBasedMatchParticipantArray = Array of TTurnBasedMatchParticipant;
  TTurnBasedMatchRematchArray = Array of TTurnBasedMatchRematch;
  TTurnBasedMatchResultsArray = Array of TTurnBasedMatchResults;
  TTurnBasedMatchSyncArray = Array of TTurnBasedMatchSync;
  TTurnBasedMatchTurnArray = Array of TTurnBasedMatchTurn;
  //Anonymous types, using auto-generated names
  TPlayerTypename = class;
  TPushTokenIdTypeios = class;
  TAchievementDefinitionsListResponseTypeitemsArray = Array of TAchievementDefinition;
  TAchievementUpdateMultipleRequestTypeupdatesArray = Array of TAchievementUpdateRequest;
  TAchievementUpdateMultipleResponseTypeupdatedAchievementsArray = Array of TAchievementUpdateResponse;
  TApplicationTypeassetsArray = Array of TImageAsset;
  TApplicationTypeinstancesArray = Array of TInstance;
  TCategoryListResponseTypeitemsArray = Array of TCategory;
  TEventDefinitionTypechildEventsArray = Array of TEventChild;
  TEventDefinitionListResponseTypeitemsArray = Array of TEventDefinition;
  TEventPeriodUpdateTypeupdatesArray = Array of TEventUpdateRequest;
  TEventRecordRequestTypetimePeriodsArray = Array of TEventPeriodUpdate;
  TEventUpdateResponseTypebatchFailuresArray = Array of TEventBatchRecordFailure;
  TEventUpdateResponseTypeeventFailuresArray = Array of TEventRecordFailure;
  TEventUpdateResponseTypeplayerEventsArray = Array of TPlayerEvent;
  TLeaderboardListResponseTypeitemsArray = Array of TLeaderboard;
  TLeaderboardScoresTypeitemsArray = Array of TLeaderboardEntry;
  TMetagameConfigTypeplayerLevelsArray = Array of TPlayerLevel;
  TPlayerAchievementListResponseTypeitemsArray = Array of TPlayerAchievement;
  TPlayerEventListResponseTypeitemsArray = Array of TPlayerEvent;
  TPlayerLeaderboardScoreListResponseTypeitemsArray = Array of TPlayerLeaderboardScore;
  TPlayerListResponseTypeitemsArray = Array of TPlayer;
  TPlayerScoreListResponseTypesubmittedScoresArray = Array of TPlayerScoreResponse;
  TPlayerScoreResponseTypeunbeatenScoresArray = Array of TPlayerScore;
  TPlayerScoreSubmissionListTypescoresArray = Array of TScoreSubmission;
  TQuestTypemilestonesArray = Array of TQuestMilestone;
  TQuestListResponseTypeitemsArray = Array of TQuest;
  TQuestMilestoneTypecriteriaArray = Array of TQuestCriterion;
  TRoomTypeparticipantsArray = Array of TRoomParticipant;
  TRoomLeaveDiagnosticsTypepeerSessionArray = Array of TPeerSessionDiagnostics;
  TRoomListTypeitemsArray = Array of TRoom;
  TRoomP2PStatusesTypeupdatesArray = Array of TRoomP2PStatus;
  TRoomStatusTypeparticipantsArray = Array of TRoomParticipant;
  TSnapshotListResponseTypeitemsArray = Array of TSnapshot;
  TTurnBasedMatchTypeparticipantsArray = Array of TTurnBasedMatchParticipant;
  TTurnBasedMatchTyperesultsArray = Array of TParticipantResult;
  TTurnBasedMatchListTypeitemsArray = Array of TTurnBasedMatch;
  TTurnBasedMatchResultsTyperesultsArray = Array of TParticipantResult;
  TTurnBasedMatchSyncTypeitemsArray = Array of TTurnBasedMatch;
  TTurnBasedMatchTurnTyperesultsArray = Array of TParticipantResult;
  
  { --------------------------------------------------------------------
    TAchievementDefinition
    --------------------------------------------------------------------}
  
  TAchievementDefinition = Class(TGoogleBaseObject)
  Private
    FachievementType : String;
    Fdescription : String;
    FexperiencePoints : String;
    FformattedTotalSteps : String;
    Fid : String;
    FinitialState : String;
    FisRevealedIconUrlDefault : boolean;
    FisUnlockedIconUrlDefault : boolean;
    Fkind : String;
    Fname : String;
    FrevealedIconUrl : String;
    FtotalSteps : integer;
    FunlockedIconUrl : String;
  Protected
    //Property setters
    Procedure SetachievementType(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetexperiencePoints(AIndex : Integer; AValue : String); virtual;
    Procedure SetformattedTotalSteps(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinitialState(AIndex : Integer; AValue : String); virtual;
    Procedure SetisRevealedIconUrlDefault(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisUnlockedIconUrlDefault(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetrevealedIconUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalSteps(AIndex : Integer; AValue : integer); virtual;
    Procedure SetunlockedIconUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property achievementType : String Index 0 Read FachievementType Write SetachievementType;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property experiencePoints : String Index 16 Read FexperiencePoints Write SetexperiencePoints;
    Property formattedTotalSteps : String Index 24 Read FformattedTotalSteps Write SetformattedTotalSteps;
    Property id : String Index 32 Read Fid Write Setid;
    Property initialState : String Index 40 Read FinitialState Write SetinitialState;
    Property isRevealedIconUrlDefault : boolean Index 48 Read FisRevealedIconUrlDefault Write SetisRevealedIconUrlDefault;
    Property isUnlockedIconUrlDefault : boolean Index 56 Read FisUnlockedIconUrlDefault Write SetisUnlockedIconUrlDefault;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property name : String Index 72 Read Fname Write Setname;
    Property revealedIconUrl : String Index 80 Read FrevealedIconUrl Write SetrevealedIconUrl;
    Property totalSteps : integer Index 88 Read FtotalSteps Write SettotalSteps;
    Property unlockedIconUrl : String Index 96 Read FunlockedIconUrl Write SetunlockedIconUrl;
  end;
  TAchievementDefinitionClass = Class of TAchievementDefinition;
  
  { --------------------------------------------------------------------
    TAchievementDefinitionsListResponse
    --------------------------------------------------------------------}
  
  TAchievementDefinitionsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TAchievementDefinitionsListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAchievementDefinitionsListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TAchievementDefinitionsListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAchievementDefinitionsListResponseClass = Class of TAchievementDefinitionsListResponse;
  
  { --------------------------------------------------------------------
    TAchievementIncrementResponse
    --------------------------------------------------------------------}
  
  TAchievementIncrementResponse = Class(TGoogleBaseObject)
  Private
    FcurrentSteps : integer;
    Fkind : String;
    FnewlyUnlocked : boolean;
  Protected
    //Property setters
    Procedure SetcurrentSteps(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnewlyUnlocked(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property currentSteps : integer Index 0 Read FcurrentSteps Write SetcurrentSteps;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property newlyUnlocked : boolean Index 16 Read FnewlyUnlocked Write SetnewlyUnlocked;
  end;
  TAchievementIncrementResponseClass = Class of TAchievementIncrementResponse;
  
  { --------------------------------------------------------------------
    TAchievementRevealResponse
    --------------------------------------------------------------------}
  
  TAchievementRevealResponse = Class(TGoogleBaseObject)
  Private
    FcurrentState : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetcurrentState(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property currentState : String Index 0 Read FcurrentState Write SetcurrentState;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAchievementRevealResponseClass = Class of TAchievementRevealResponse;
  
  { --------------------------------------------------------------------
    TAchievementSetStepsAtLeastResponse
    --------------------------------------------------------------------}
  
  TAchievementSetStepsAtLeastResponse = Class(TGoogleBaseObject)
  Private
    FcurrentSteps : integer;
    Fkind : String;
    FnewlyUnlocked : boolean;
  Protected
    //Property setters
    Procedure SetcurrentSteps(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnewlyUnlocked(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property currentSteps : integer Index 0 Read FcurrentSteps Write SetcurrentSteps;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property newlyUnlocked : boolean Index 16 Read FnewlyUnlocked Write SetnewlyUnlocked;
  end;
  TAchievementSetStepsAtLeastResponseClass = Class of TAchievementSetStepsAtLeastResponse;
  
  { --------------------------------------------------------------------
    TAchievementUnlockResponse
    --------------------------------------------------------------------}
  
  TAchievementUnlockResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnewlyUnlocked : boolean;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnewlyUnlocked(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property newlyUnlocked : boolean Index 8 Read FnewlyUnlocked Write SetnewlyUnlocked;
  end;
  TAchievementUnlockResponseClass = Class of TAchievementUnlockResponse;
  
  { --------------------------------------------------------------------
    TAchievementUpdateMultipleRequest
    --------------------------------------------------------------------}
  
  TAchievementUpdateMultipleRequest = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fupdates : TAchievementUpdateMultipleRequestTypeupdatesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdates(AIndex : Integer; AValue : TAchievementUpdateMultipleRequestTypeupdatesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property updates : TAchievementUpdateMultipleRequestTypeupdatesArray Index 8 Read Fupdates Write Setupdates;
  end;
  TAchievementUpdateMultipleRequestClass = Class of TAchievementUpdateMultipleRequest;
  
  { --------------------------------------------------------------------
    TAchievementUpdateMultipleResponse
    --------------------------------------------------------------------}
  
  TAchievementUpdateMultipleResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FupdatedAchievements : TAchievementUpdateMultipleResponseTypeupdatedAchievementsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetupdatedAchievements(AIndex : Integer; AValue : TAchievementUpdateMultipleResponseTypeupdatedAchievementsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property updatedAchievements : TAchievementUpdateMultipleResponseTypeupdatedAchievementsArray Index 8 Read FupdatedAchievements Write SetupdatedAchievements;
  end;
  TAchievementUpdateMultipleResponseClass = Class of TAchievementUpdateMultipleResponse;
  
  { --------------------------------------------------------------------
    TAchievementUpdateRequest
    --------------------------------------------------------------------}
  
  TAchievementUpdateRequest = Class(TGoogleBaseObject)
  Private
    FachievementId : String;
    FincrementPayload : TGamesAchievementIncrement;
    Fkind : String;
    FsetStepsAtLeastPayload : TGamesAchievementSetStepsAtLeast;
    FupdateType : String;
  Protected
    //Property setters
    Procedure SetachievementId(AIndex : Integer; AValue : String); virtual;
    Procedure SetincrementPayload(AIndex : Integer; AValue : TGamesAchievementIncrement); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetsetStepsAtLeastPayload(AIndex : Integer; AValue : TGamesAchievementSetStepsAtLeast); virtual;
    Procedure SetupdateType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property achievementId : String Index 0 Read FachievementId Write SetachievementId;
    Property incrementPayload : TGamesAchievementIncrement Index 8 Read FincrementPayload Write SetincrementPayload;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property setStepsAtLeastPayload : TGamesAchievementSetStepsAtLeast Index 24 Read FsetStepsAtLeastPayload Write SetsetStepsAtLeastPayload;
    Property updateType : String Index 32 Read FupdateType Write SetupdateType;
  end;
  TAchievementUpdateRequestClass = Class of TAchievementUpdateRequest;
  
  { --------------------------------------------------------------------
    TAchievementUpdateResponse
    --------------------------------------------------------------------}
  
  TAchievementUpdateResponse = Class(TGoogleBaseObject)
  Private
    FachievementId : String;
    FcurrentState : String;
    FcurrentSteps : integer;
    Fkind : String;
    FnewlyUnlocked : boolean;
    FupdateOccurred : boolean;
  Protected
    //Property setters
    Procedure SetachievementId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrentState(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrentSteps(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnewlyUnlocked(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetupdateOccurred(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property achievementId : String Index 0 Read FachievementId Write SetachievementId;
    Property currentState : String Index 8 Read FcurrentState Write SetcurrentState;
    Property currentSteps : integer Index 16 Read FcurrentSteps Write SetcurrentSteps;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property newlyUnlocked : boolean Index 32 Read FnewlyUnlocked Write SetnewlyUnlocked;
    Property updateOccurred : boolean Index 40 Read FupdateOccurred Write SetupdateOccurred;
  end;
  TAchievementUpdateResponseClass = Class of TAchievementUpdateResponse;
  
  { --------------------------------------------------------------------
    TAggregateStats
    --------------------------------------------------------------------}
  
  TAggregateStats = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    Fkind : String;
    Fmax : String;
    Fmin : String;
    Fsum : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmax(AIndex : Integer; AValue : String); virtual;
    Procedure Setmin(AIndex : Integer; AValue : String); virtual;
    Procedure Setsum(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property max : String Index 16 Read Fmax Write Setmax;
    Property min : String Index 24 Read Fmin Write Setmin;
    Property sum : String Index 32 Read Fsum Write Setsum;
  end;
  TAggregateStatsClass = Class of TAggregateStats;
  
  { --------------------------------------------------------------------
    TAnonymousPlayer
    --------------------------------------------------------------------}
  
  TAnonymousPlayer = Class(TGoogleBaseObject)
  Private
    FavatarImageUrl : String;
    FdisplayName : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetavatarImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property avatarImageUrl : String Index 0 Read FavatarImageUrl Write SetavatarImageUrl;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TAnonymousPlayerClass = Class of TAnonymousPlayer;
  
  { --------------------------------------------------------------------
    TApplication
    --------------------------------------------------------------------}
  
  TApplication = Class(TGoogleBaseObject)
  Private
    Fachievement_count : integer;
    Fassets : TApplicationTypeassetsArray;
    Fauthor : String;
    Fcategory : TApplicationCategory;
    Fdescription : String;
    FenabledFeatures : TStringArray;
    Fid : String;
    Finstances : TApplicationTypeinstancesArray;
    Fkind : String;
    FlastUpdatedTimestamp : String;
    Fleaderboard_count : integer;
    Fname : String;
    FthemeColor : String;
  Protected
    //Property setters
    Procedure Setachievement_count(AIndex : Integer; AValue : integer); virtual;
    Procedure Setassets(AIndex : Integer; AValue : TApplicationTypeassetsArray); virtual;
    Procedure Setauthor(AIndex : Integer; AValue : String); virtual;
    Procedure Setcategory(AIndex : Integer; AValue : TApplicationCategory); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetenabledFeatures(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinstances(AIndex : Integer; AValue : TApplicationTypeinstancesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastUpdatedTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setleaderboard_count(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetthemeColor(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property achievement_count : integer Index 0 Read Fachievement_count Write Setachievement_count;
    Property assets : TApplicationTypeassetsArray Index 8 Read Fassets Write Setassets;
    Property author : String Index 16 Read Fauthor Write Setauthor;
    Property category : TApplicationCategory Index 24 Read Fcategory Write Setcategory;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property enabledFeatures : TStringArray Index 40 Read FenabledFeatures Write SetenabledFeatures;
    Property id : String Index 48 Read Fid Write Setid;
    Property instances : TApplicationTypeinstancesArray Index 56 Read Finstances Write Setinstances;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property lastUpdatedTimestamp : String Index 72 Read FlastUpdatedTimestamp Write SetlastUpdatedTimestamp;
    Property leaderboard_count : integer Index 80 Read Fleaderboard_count Write Setleaderboard_count;
    Property name : String Index 88 Read Fname Write Setname;
    Property themeColor : String Index 96 Read FthemeColor Write SetthemeColor;
  end;
  TApplicationClass = Class of TApplication;
  
  { --------------------------------------------------------------------
    TApplicationCategory
    --------------------------------------------------------------------}
  
  TApplicationCategory = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fprimary : String;
    Fsecondary : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setprimary(AIndex : Integer; AValue : String); virtual;
    Procedure Setsecondary(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property primary : String Index 8 Read Fprimary Write Setprimary;
    Property secondary : String Index 16 Read Fsecondary Write Setsecondary;
  end;
  TApplicationCategoryClass = Class of TApplicationCategory;
  
  { --------------------------------------------------------------------
    TCategory
    --------------------------------------------------------------------}
  
  TCategory = Class(TGoogleBaseObject)
  Private
    Fcategory : String;
    FexperiencePoints : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setcategory(AIndex : Integer; AValue : String); virtual;
    Procedure SetexperiencePoints(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property category : String Index 0 Read Fcategory Write Setcategory;
    Property experiencePoints : String Index 8 Read FexperiencePoints Write SetexperiencePoints;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TCategoryClass = Class of TCategory;
  
  { --------------------------------------------------------------------
    TCategoryListResponse
    --------------------------------------------------------------------}
  
  TCategoryListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TCategoryListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCategoryListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TCategoryListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCategoryListResponseClass = Class of TCategoryListResponse;
  
  { --------------------------------------------------------------------
    TEventBatchRecordFailure
    --------------------------------------------------------------------}
  
  TEventBatchRecordFailure = Class(TGoogleBaseObject)
  Private
    FfailureCause : String;
    Fkind : String;
    Frange : TEventPeriodRange;
  Protected
    //Property setters
    Procedure SetfailureCause(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setrange(AIndex : Integer; AValue : TEventPeriodRange); virtual;
  Public
  Published
    Property failureCause : String Index 0 Read FfailureCause Write SetfailureCause;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property range : TEventPeriodRange Index 16 Read Frange Write Setrange;
  end;
  TEventBatchRecordFailureClass = Class of TEventBatchRecordFailure;
  
  { --------------------------------------------------------------------
    TEventChild
    --------------------------------------------------------------------}
  
  TEventChild = Class(TGoogleBaseObject)
  Private
    FchildId : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetchildId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property childId : String Index 0 Read FchildId Write SetchildId;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TEventChildClass = Class of TEventChild;
  
  { --------------------------------------------------------------------
    TEventDefinition
    --------------------------------------------------------------------}
  
  TEventDefinition = Class(TGoogleBaseObject)
  Private
    FchildEvents : TEventDefinitionTypechildEventsArray;
    Fdescription : String;
    FdisplayName : String;
    Fid : String;
    FimageUrl : String;
    FisDefaultImageUrl : boolean;
    Fkind : String;
    Fvisibility : String;
  Protected
    //Property setters
    Procedure SetchildEvents(AIndex : Integer; AValue : TEventDefinitionTypechildEventsArray); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetimageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetisDefaultImageUrl(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property childEvents : TEventDefinitionTypechildEventsArray Index 0 Read FchildEvents Write SetchildEvents;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property displayName : String Index 16 Read FdisplayName Write SetdisplayName;
    Property id : String Index 24 Read Fid Write Setid;
    Property imageUrl : String Index 32 Read FimageUrl Write SetimageUrl;
    Property isDefaultImageUrl : boolean Index 40 Read FisDefaultImageUrl Write SetisDefaultImageUrl;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property visibility : String Index 56 Read Fvisibility Write Setvisibility;
  end;
  TEventDefinitionClass = Class of TEventDefinition;
  
  { --------------------------------------------------------------------
    TEventDefinitionListResponse
    --------------------------------------------------------------------}
  
  TEventDefinitionListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TEventDefinitionListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TEventDefinitionListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TEventDefinitionListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TEventDefinitionListResponseClass = Class of TEventDefinitionListResponse;
  
  { --------------------------------------------------------------------
    TEventPeriodRange
    --------------------------------------------------------------------}
  
  TEventPeriodRange = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FperiodEndMillis : String;
    FperiodStartMillis : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetperiodEndMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetperiodStartMillis(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property periodEndMillis : String Index 8 Read FperiodEndMillis Write SetperiodEndMillis;
    Property periodStartMillis : String Index 16 Read FperiodStartMillis Write SetperiodStartMillis;
  end;
  TEventPeriodRangeClass = Class of TEventPeriodRange;
  
  { --------------------------------------------------------------------
    TEventPeriodUpdate
    --------------------------------------------------------------------}
  
  TEventPeriodUpdate = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FtimePeriod : TEventPeriodRange;
    Fupdates : TEventPeriodUpdateTypeupdatesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SettimePeriod(AIndex : Integer; AValue : TEventPeriodRange); virtual;
    Procedure Setupdates(AIndex : Integer; AValue : TEventPeriodUpdateTypeupdatesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property timePeriod : TEventPeriodRange Index 8 Read FtimePeriod Write SettimePeriod;
    Property updates : TEventPeriodUpdateTypeupdatesArray Index 16 Read Fupdates Write Setupdates;
  end;
  TEventPeriodUpdateClass = Class of TEventPeriodUpdate;
  
  { --------------------------------------------------------------------
    TEventRecordFailure
    --------------------------------------------------------------------}
  
  TEventRecordFailure = Class(TGoogleBaseObject)
  Private
    FeventId : String;
    FfailureCause : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfailureCause(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property eventId : String Index 0 Read FeventId Write SeteventId;
    Property failureCause : String Index 8 Read FfailureCause Write SetfailureCause;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TEventRecordFailureClass = Class of TEventRecordFailure;
  
  { --------------------------------------------------------------------
    TEventRecordRequest
    --------------------------------------------------------------------}
  
  TEventRecordRequest = Class(TGoogleBaseObject)
  Private
    FcurrentTimeMillis : String;
    Fkind : String;
    FrequestId : String;
    FtimePeriods : TEventRecordRequestTypetimePeriodsArray;
  Protected
    //Property setters
    Procedure SetcurrentTimeMillis(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestId(AIndex : Integer; AValue : String); virtual;
    Procedure SettimePeriods(AIndex : Integer; AValue : TEventRecordRequestTypetimePeriodsArray); virtual;
  Public
  Published
    Property currentTimeMillis : String Index 0 Read FcurrentTimeMillis Write SetcurrentTimeMillis;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property requestId : String Index 16 Read FrequestId Write SetrequestId;
    Property timePeriods : TEventRecordRequestTypetimePeriodsArray Index 24 Read FtimePeriods Write SettimePeriods;
  end;
  TEventRecordRequestClass = Class of TEventRecordRequest;
  
  { --------------------------------------------------------------------
    TEventUpdateRequest
    --------------------------------------------------------------------}
  
  TEventUpdateRequest = Class(TGoogleBaseObject)
  Private
    FdefinitionId : String;
    Fkind : String;
    FupdateCount : String;
  Protected
    //Property setters
    Procedure SetdefinitionId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetupdateCount(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property definitionId : String Index 0 Read FdefinitionId Write SetdefinitionId;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property updateCount : String Index 16 Read FupdateCount Write SetupdateCount;
  end;
  TEventUpdateRequestClass = Class of TEventUpdateRequest;
  
  { --------------------------------------------------------------------
    TEventUpdateResponse
    --------------------------------------------------------------------}
  
  TEventUpdateResponse = Class(TGoogleBaseObject)
  Private
    FbatchFailures : TEventUpdateResponseTypebatchFailuresArray;
    FeventFailures : TEventUpdateResponseTypeeventFailuresArray;
    Fkind : String;
    FplayerEvents : TEventUpdateResponseTypeplayerEventsArray;
  Protected
    //Property setters
    Procedure SetbatchFailures(AIndex : Integer; AValue : TEventUpdateResponseTypebatchFailuresArray); virtual;
    Procedure SeteventFailures(AIndex : Integer; AValue : TEventUpdateResponseTypeeventFailuresArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetplayerEvents(AIndex : Integer; AValue : TEventUpdateResponseTypeplayerEventsArray); virtual;
  Public
  Published
    Property batchFailures : TEventUpdateResponseTypebatchFailuresArray Index 0 Read FbatchFailures Write SetbatchFailures;
    Property eventFailures : TEventUpdateResponseTypeeventFailuresArray Index 8 Read FeventFailures Write SeteventFailures;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property playerEvents : TEventUpdateResponseTypeplayerEventsArray Index 24 Read FplayerEvents Write SetplayerEvents;
  end;
  TEventUpdateResponseClass = Class of TEventUpdateResponse;
  
  { --------------------------------------------------------------------
    TGamesAchievementIncrement
    --------------------------------------------------------------------}
  
  TGamesAchievementIncrement = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FrequestId : String;
    Fsteps : integer;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestId(AIndex : Integer; AValue : String); virtual;
    Procedure Setsteps(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property requestId : String Index 8 Read FrequestId Write SetrequestId;
    Property steps : integer Index 16 Read Fsteps Write Setsteps;
  end;
  TGamesAchievementIncrementClass = Class of TGamesAchievementIncrement;
  
  { --------------------------------------------------------------------
    TGamesAchievementSetStepsAtLeast
    --------------------------------------------------------------------}
  
  TGamesAchievementSetStepsAtLeast = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fsteps : integer;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsteps(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property steps : integer Index 8 Read Fsteps Write Setsteps;
  end;
  TGamesAchievementSetStepsAtLeastClass = Class of TGamesAchievementSetStepsAtLeast;
  
  { --------------------------------------------------------------------
    TImageAsset
    --------------------------------------------------------------------}
  
  TImageAsset = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fkind : String;
    Fname : String;
    Furl : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property url : String Index 24 Read Furl Write Seturl;
    Property width : integer Index 32 Read Fwidth Write Setwidth;
  end;
  TImageAssetClass = Class of TImageAsset;
  
  { --------------------------------------------------------------------
    TInstance
    --------------------------------------------------------------------}
  
  TInstance = Class(TGoogleBaseObject)
  Private
    FacquisitionUri : String;
    FandroidInstance : TInstanceAndroidDetails;
    FiosInstance : TInstanceIosDetails;
    Fkind : String;
    Fname : String;
    FplatformType : String;
    FrealtimePlay : boolean;
    FturnBasedPlay : boolean;
    FwebInstance : TInstanceWebDetails;
  Protected
    //Property setters
    Procedure SetacquisitionUri(AIndex : Integer; AValue : String); virtual;
    Procedure SetandroidInstance(AIndex : Integer; AValue : TInstanceAndroidDetails); virtual;
    Procedure SetiosInstance(AIndex : Integer; AValue : TInstanceIosDetails); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetplatformType(AIndex : Integer; AValue : String); virtual;
    Procedure SetrealtimePlay(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetturnBasedPlay(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetwebInstance(AIndex : Integer; AValue : TInstanceWebDetails); virtual;
  Public
  Published
    Property acquisitionUri : String Index 0 Read FacquisitionUri Write SetacquisitionUri;
    Property androidInstance : TInstanceAndroidDetails Index 8 Read FandroidInstance Write SetandroidInstance;
    Property iosInstance : TInstanceIosDetails Index 16 Read FiosInstance Write SetiosInstance;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property platformType : String Index 40 Read FplatformType Write SetplatformType;
    Property realtimePlay : boolean Index 48 Read FrealtimePlay Write SetrealtimePlay;
    Property turnBasedPlay : boolean Index 56 Read FturnBasedPlay Write SetturnBasedPlay;
    Property webInstance : TInstanceWebDetails Index 64 Read FwebInstance Write SetwebInstance;
  end;
  TInstanceClass = Class of TInstance;
  
  { --------------------------------------------------------------------
    TInstanceAndroidDetails
    --------------------------------------------------------------------}
  
  TInstanceAndroidDetails = Class(TGoogleBaseObject)
  Private
    FenablePiracyCheck : boolean;
    Fkind : String;
    FpackageName : String;
    Fpreferred : boolean;
  Protected
    //Property setters
    Procedure SetenablePiracyCheck(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpackageName(AIndex : Integer; AValue : String); virtual;
    Procedure Setpreferred(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property enablePiracyCheck : boolean Index 0 Read FenablePiracyCheck Write SetenablePiracyCheck;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property packageName : String Index 16 Read FpackageName Write SetpackageName;
    Property preferred : boolean Index 24 Read Fpreferred Write Setpreferred;
  end;
  TInstanceAndroidDetailsClass = Class of TInstanceAndroidDetails;
  
  { --------------------------------------------------------------------
    TInstanceIosDetails
    --------------------------------------------------------------------}
  
  TInstanceIosDetails = Class(TGoogleBaseObject)
  Private
    FbundleIdentifier : String;
    FitunesAppId : String;
    Fkind : String;
    FpreferredForIpad : boolean;
    FpreferredForIphone : boolean;
    FsupportIpad : boolean;
    FsupportIphone : boolean;
  Protected
    //Property setters
    Procedure SetbundleIdentifier(AIndex : Integer; AValue : String); virtual;
    Procedure SetitunesAppId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreferredForIpad(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpreferredForIphone(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportIpad(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportIphone(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bundleIdentifier : String Index 0 Read FbundleIdentifier Write SetbundleIdentifier;
    Property itunesAppId : String Index 8 Read FitunesAppId Write SetitunesAppId;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property preferredForIpad : boolean Index 24 Read FpreferredForIpad Write SetpreferredForIpad;
    Property preferredForIphone : boolean Index 32 Read FpreferredForIphone Write SetpreferredForIphone;
    Property supportIpad : boolean Index 40 Read FsupportIpad Write SetsupportIpad;
    Property supportIphone : boolean Index 48 Read FsupportIphone Write SetsupportIphone;
  end;
  TInstanceIosDetailsClass = Class of TInstanceIosDetails;
  
  { --------------------------------------------------------------------
    TInstanceWebDetails
    --------------------------------------------------------------------}
  
  TInstanceWebDetails = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FlaunchUrl : String;
    Fpreferred : boolean;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlaunchUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setpreferred(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property launchUrl : String Index 8 Read FlaunchUrl Write SetlaunchUrl;
    Property preferred : boolean Index 16 Read Fpreferred Write Setpreferred;
  end;
  TInstanceWebDetailsClass = Class of TInstanceWebDetails;
  
  { --------------------------------------------------------------------
    TLeaderboard
    --------------------------------------------------------------------}
  
  TLeaderboard = Class(TGoogleBaseObject)
  Private
    FiconUrl : String;
    Fid : String;
    FisIconUrlDefault : boolean;
    Fkind : String;
    Fname : String;
    Forder : String;
  Protected
    //Property setters
    Procedure SeticonUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetisIconUrlDefault(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setorder(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property iconUrl : String Index 0 Read FiconUrl Write SeticonUrl;
    Property id : String Index 8 Read Fid Write Setid;
    Property isIconUrlDefault : boolean Index 16 Read FisIconUrlDefault Write SetisIconUrlDefault;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property order : String Index 40 Read Forder Write Setorder;
  end;
  TLeaderboardClass = Class of TLeaderboard;
  
  { --------------------------------------------------------------------
    TLeaderboardEntry
    --------------------------------------------------------------------}
  
  TLeaderboardEntry = Class(TGoogleBaseObject)
  Private
    FformattedScore : String;
    FformattedScoreRank : String;
    Fkind : String;
    Fplayer : TPlayer;
    FscoreRank : String;
    FscoreTag : String;
    FscoreValue : String;
    FtimeSpan : String;
    FwriteTimestampMillis : String;
  Protected
    //Property setters
    Procedure SetformattedScore(AIndex : Integer; AValue : String); virtual;
    Procedure SetformattedScoreRank(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setplayer(AIndex : Integer; AValue : TPlayer); virtual;
    Procedure SetscoreRank(AIndex : Integer; AValue : String); virtual;
    Procedure SetscoreTag(AIndex : Integer; AValue : String); virtual;
    Procedure SetscoreValue(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeSpan(AIndex : Integer; AValue : String); virtual;
    Procedure SetwriteTimestampMillis(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property formattedScore : String Index 0 Read FformattedScore Write SetformattedScore;
    Property formattedScoreRank : String Index 8 Read FformattedScoreRank Write SetformattedScoreRank;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property player : TPlayer Index 24 Read Fplayer Write Setplayer;
    Property scoreRank : String Index 32 Read FscoreRank Write SetscoreRank;
    Property scoreTag : String Index 40 Read FscoreTag Write SetscoreTag;
    Property scoreValue : String Index 48 Read FscoreValue Write SetscoreValue;
    Property timeSpan : String Index 56 Read FtimeSpan Write SettimeSpan;
    Property writeTimestampMillis : String Index 64 Read FwriteTimestampMillis Write SetwriteTimestampMillis;
  end;
  TLeaderboardEntryClass = Class of TLeaderboardEntry;
  
  { --------------------------------------------------------------------
    TLeaderboardListResponse
    --------------------------------------------------------------------}
  
  TLeaderboardListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TLeaderboardListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLeaderboardListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TLeaderboardListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TLeaderboardListResponseClass = Class of TLeaderboardListResponse;
  
  { --------------------------------------------------------------------
    TLeaderboardScoreRank
    --------------------------------------------------------------------}
  
  TLeaderboardScoreRank = Class(TGoogleBaseObject)
  Private
    FformattedNumScores : String;
    FformattedRank : String;
    Fkind : String;
    FnumScores : String;
    Frank : String;
  Protected
    //Property setters
    Procedure SetformattedNumScores(AIndex : Integer; AValue : String); virtual;
    Procedure SetformattedRank(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumScores(AIndex : Integer; AValue : String); virtual;
    Procedure Setrank(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property formattedNumScores : String Index 0 Read FformattedNumScores Write SetformattedNumScores;
    Property formattedRank : String Index 8 Read FformattedRank Write SetformattedRank;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property numScores : String Index 24 Read FnumScores Write SetnumScores;
    Property rank : String Index 32 Read Frank Write Setrank;
  end;
  TLeaderboardScoreRankClass = Class of TLeaderboardScoreRank;
  
  { --------------------------------------------------------------------
    TLeaderboardScores
    --------------------------------------------------------------------}
  
  TLeaderboardScores = Class(TGoogleBaseObject)
  Private
    Fitems : TLeaderboardScoresTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FnumScores : String;
    FplayerScore : TLeaderboardEntry;
    FprevPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLeaderboardScoresTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumScores(AIndex : Integer; AValue : String); virtual;
    Procedure SetplayerScore(AIndex : Integer; AValue : TLeaderboardEntry); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TLeaderboardScoresTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property numScores : String Index 24 Read FnumScores Write SetnumScores;
    Property playerScore : TLeaderboardEntry Index 32 Read FplayerScore Write SetplayerScore;
    Property prevPageToken : String Index 40 Read FprevPageToken Write SetprevPageToken;
  end;
  TLeaderboardScoresClass = Class of TLeaderboardScores;
  
  { --------------------------------------------------------------------
    TMetagameConfig
    --------------------------------------------------------------------}
  
  TMetagameConfig = Class(TGoogleBaseObject)
  Private
    FcurrentVersion : integer;
    Fkind : String;
    FplayerLevels : TMetagameConfigTypeplayerLevelsArray;
  Protected
    //Property setters
    Procedure SetcurrentVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetplayerLevels(AIndex : Integer; AValue : TMetagameConfigTypeplayerLevelsArray); virtual;
  Public
  Published
    Property currentVersion : integer Index 0 Read FcurrentVersion Write SetcurrentVersion;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property playerLevels : TMetagameConfigTypeplayerLevelsArray Index 16 Read FplayerLevels Write SetplayerLevels;
  end;
  TMetagameConfigClass = Class of TMetagameConfig;
  
  { --------------------------------------------------------------------
    TNetworkDiagnostics
    --------------------------------------------------------------------}
  
  TNetworkDiagnostics = Class(TGoogleBaseObject)
  Private
    FandroidNetworkSubtype : integer;
    FandroidNetworkType : integer;
    FiosNetworkType : integer;
    Fkind : String;
    FnetworkOperatorCode : String;
    FnetworkOperatorName : String;
    FregistrationLatencyMillis : integer;
  Protected
    //Property setters
    Procedure SetandroidNetworkSubtype(AIndex : Integer; AValue : integer); virtual;
    Procedure SetandroidNetworkType(AIndex : Integer; AValue : integer); virtual;
    Procedure SetiosNetworkType(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnetworkOperatorCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetnetworkOperatorName(AIndex : Integer; AValue : String); virtual;
    Procedure SetregistrationLatencyMillis(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property androidNetworkSubtype : integer Index 0 Read FandroidNetworkSubtype Write SetandroidNetworkSubtype;
    Property androidNetworkType : integer Index 8 Read FandroidNetworkType Write SetandroidNetworkType;
    Property iosNetworkType : integer Index 16 Read FiosNetworkType Write SetiosNetworkType;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property networkOperatorCode : String Index 32 Read FnetworkOperatorCode Write SetnetworkOperatorCode;
    Property networkOperatorName : String Index 40 Read FnetworkOperatorName Write SetnetworkOperatorName;
    Property registrationLatencyMillis : integer Index 48 Read FregistrationLatencyMillis Write SetregistrationLatencyMillis;
  end;
  TNetworkDiagnosticsClass = Class of TNetworkDiagnostics;
  
  { --------------------------------------------------------------------
    TParticipantResult
    --------------------------------------------------------------------}
  
  TParticipantResult = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FparticipantId : String;
    Fplacing : integer;
    Fresult : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetparticipantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setplacing(AIndex : Integer; AValue : integer); virtual;
    Procedure Setresult(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property participantId : String Index 8 Read FparticipantId Write SetparticipantId;
    Property placing : integer Index 16 Read Fplacing Write Setplacing;
    Property result : String Index 24 Read Fresult Write Setresult;
  end;
  TParticipantResultClass = Class of TParticipantResult;
  
  { --------------------------------------------------------------------
    TPeerChannelDiagnostics
    --------------------------------------------------------------------}
  
  TPeerChannelDiagnostics = Class(TGoogleBaseObject)
  Private
    FbytesReceived : TAggregateStats;
    FbytesSent : TAggregateStats;
    Fkind : String;
    FnumMessagesLost : integer;
    FnumMessagesReceived : integer;
    FnumMessagesSent : integer;
    FnumSendFailures : integer;
    FroundtripLatencyMillis : TAggregateStats;
  Protected
    //Property setters
    Procedure SetbytesReceived(AIndex : Integer; AValue : TAggregateStats); virtual;
    Procedure SetbytesSent(AIndex : Integer; AValue : TAggregateStats); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumMessagesLost(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumMessagesReceived(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumMessagesSent(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumSendFailures(AIndex : Integer; AValue : integer); virtual;
    Procedure SetroundtripLatencyMillis(AIndex : Integer; AValue : TAggregateStats); virtual;
  Public
  Published
    Property bytesReceived : TAggregateStats Index 0 Read FbytesReceived Write SetbytesReceived;
    Property bytesSent : TAggregateStats Index 8 Read FbytesSent Write SetbytesSent;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property numMessagesLost : integer Index 24 Read FnumMessagesLost Write SetnumMessagesLost;
    Property numMessagesReceived : integer Index 32 Read FnumMessagesReceived Write SetnumMessagesReceived;
    Property numMessagesSent : integer Index 40 Read FnumMessagesSent Write SetnumMessagesSent;
    Property numSendFailures : integer Index 48 Read FnumSendFailures Write SetnumSendFailures;
    Property roundtripLatencyMillis : TAggregateStats Index 56 Read FroundtripLatencyMillis Write SetroundtripLatencyMillis;
  end;
  TPeerChannelDiagnosticsClass = Class of TPeerChannelDiagnostics;
  
  { --------------------------------------------------------------------
    TPeerSessionDiagnostics
    --------------------------------------------------------------------}
  
  TPeerSessionDiagnostics = Class(TGoogleBaseObject)
  Private
    FconnectedTimestampMillis : String;
    Fkind : String;
    FparticipantId : String;
    FreliableChannel : TPeerChannelDiagnostics;
    FunreliableChannel : TPeerChannelDiagnostics;
  Protected
    //Property setters
    Procedure SetconnectedTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetparticipantId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreliableChannel(AIndex : Integer; AValue : TPeerChannelDiagnostics); virtual;
    Procedure SetunreliableChannel(AIndex : Integer; AValue : TPeerChannelDiagnostics); virtual;
  Public
  Published
    Property connectedTimestampMillis : String Index 0 Read FconnectedTimestampMillis Write SetconnectedTimestampMillis;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property participantId : String Index 16 Read FparticipantId Write SetparticipantId;
    Property reliableChannel : TPeerChannelDiagnostics Index 24 Read FreliableChannel Write SetreliableChannel;
    Property unreliableChannel : TPeerChannelDiagnostics Index 32 Read FunreliableChannel Write SetunreliableChannel;
  end;
  TPeerSessionDiagnosticsClass = Class of TPeerSessionDiagnostics;
  
  { --------------------------------------------------------------------
    TPlayed
    --------------------------------------------------------------------}
  
  TPlayed = Class(TGoogleBaseObject)
  Private
    FautoMatched : boolean;
    Fkind : String;
    FtimeMillis : String;
  Protected
    //Property setters
    Procedure SetautoMatched(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeMillis(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoMatched : boolean Index 0 Read FautoMatched Write SetautoMatched;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property timeMillis : String Index 16 Read FtimeMillis Write SettimeMillis;
  end;
  TPlayedClass = Class of TPlayed;
  
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
    FexperienceInfo : TPlayerExperienceInfo;
    Fkind : String;
    FlastPlayedWith : TPlayed;
    Fname : TPlayerTypename;
    FplayerId : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetavatarImageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure SetexperienceInfo(AIndex : Integer; AValue : TPlayerExperienceInfo); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastPlayedWith(AIndex : Integer; AValue : TPlayed); virtual;
    Procedure Setname(AIndex : Integer; AValue : TPlayerTypename); virtual;
    Procedure SetplayerId(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property avatarImageUrl : String Index 0 Read FavatarImageUrl Write SetavatarImageUrl;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property experienceInfo : TPlayerExperienceInfo Index 16 Read FexperienceInfo Write SetexperienceInfo;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property lastPlayedWith : TPlayed Index 32 Read FlastPlayedWith Write SetlastPlayedWith;
    Property name : TPlayerTypename Index 40 Read Fname Write Setname;
    Property playerId : String Index 48 Read FplayerId Write SetplayerId;
    Property title : String Index 56 Read Ftitle Write Settitle;
  end;
  TPlayerClass = Class of TPlayer;
  
  { --------------------------------------------------------------------
    TPlayerAchievement
    --------------------------------------------------------------------}
  
  TPlayerAchievement = Class(TGoogleBaseObject)
  Private
    FachievementState : String;
    FcurrentSteps : integer;
    FexperiencePoints : String;
    FformattedCurrentStepsString : String;
    Fid : String;
    Fkind : String;
    FlastUpdatedTimestamp : String;
  Protected
    //Property setters
    Procedure SetachievementState(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrentSteps(AIndex : Integer; AValue : integer); virtual;
    Procedure SetexperiencePoints(AIndex : Integer; AValue : String); virtual;
    Procedure SetformattedCurrentStepsString(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastUpdatedTimestamp(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property achievementState : String Index 0 Read FachievementState Write SetachievementState;
    Property currentSteps : integer Index 8 Read FcurrentSteps Write SetcurrentSteps;
    Property experiencePoints : String Index 16 Read FexperiencePoints Write SetexperiencePoints;
    Property formattedCurrentStepsString : String Index 24 Read FformattedCurrentStepsString Write SetformattedCurrentStepsString;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property lastUpdatedTimestamp : String Index 48 Read FlastUpdatedTimestamp Write SetlastUpdatedTimestamp;
  end;
  TPlayerAchievementClass = Class of TPlayerAchievement;
  
  { --------------------------------------------------------------------
    TPlayerAchievementListResponse
    --------------------------------------------------------------------}
  
  TPlayerAchievementListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TPlayerAchievementListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPlayerAchievementListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TPlayerAchievementListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TPlayerAchievementListResponseClass = Class of TPlayerAchievementListResponse;
  
  { --------------------------------------------------------------------
    TPlayerEvent
    --------------------------------------------------------------------}
  
  TPlayerEvent = Class(TGoogleBaseObject)
  Private
    FdefinitionId : String;
    FformattedNumEvents : String;
    Fkind : String;
    FnumEvents : String;
    FplayerId : String;
  Protected
    //Property setters
    Procedure SetdefinitionId(AIndex : Integer; AValue : String); virtual;
    Procedure SetformattedNumEvents(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumEvents(AIndex : Integer; AValue : String); virtual;
    Procedure SetplayerId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property definitionId : String Index 0 Read FdefinitionId Write SetdefinitionId;
    Property formattedNumEvents : String Index 8 Read FformattedNumEvents Write SetformattedNumEvents;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property numEvents : String Index 24 Read FnumEvents Write SetnumEvents;
    Property playerId : String Index 32 Read FplayerId Write SetplayerId;
  end;
  TPlayerEventClass = Class of TPlayerEvent;
  
  { --------------------------------------------------------------------
    TPlayerEventListResponse
    --------------------------------------------------------------------}
  
  TPlayerEventListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TPlayerEventListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPlayerEventListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TPlayerEventListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TPlayerEventListResponseClass = Class of TPlayerEventListResponse;
  
  { --------------------------------------------------------------------
    TPlayerExperienceInfo
    --------------------------------------------------------------------}
  
  TPlayerExperienceInfo = Class(TGoogleBaseObject)
  Private
    FcurrentExperiencePoints : String;
    FcurrentLevel : TPlayerLevel;
    Fkind : String;
    FlastLevelUpTimestampMillis : String;
    FnextLevel : TPlayerLevel;
  Protected
    //Property setters
    Procedure SetcurrentExperiencePoints(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrentLevel(AIndex : Integer; AValue : TPlayerLevel); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastLevelUpTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLevel(AIndex : Integer; AValue : TPlayerLevel); virtual;
  Public
  Published
    Property currentExperiencePoints : String Index 0 Read FcurrentExperiencePoints Write SetcurrentExperiencePoints;
    Property currentLevel : TPlayerLevel Index 8 Read FcurrentLevel Write SetcurrentLevel;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property lastLevelUpTimestampMillis : String Index 24 Read FlastLevelUpTimestampMillis Write SetlastLevelUpTimestampMillis;
    Property nextLevel : TPlayerLevel Index 32 Read FnextLevel Write SetnextLevel;
  end;
  TPlayerExperienceInfoClass = Class of TPlayerExperienceInfo;
  
  { --------------------------------------------------------------------
    TPlayerLeaderboardScore
    --------------------------------------------------------------------}
  
  TPlayerLeaderboardScore = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fleaderboard_id : String;
    FpublicRank : TLeaderboardScoreRank;
    FscoreString : String;
    FscoreTag : String;
    FscoreValue : String;
    FsocialRank : TLeaderboardScoreRank;
    FtimeSpan : String;
    FwriteTimestamp : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setleaderboard_id(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublicRank(AIndex : Integer; AValue : TLeaderboardScoreRank); virtual;
    Procedure SetscoreString(AIndex : Integer; AValue : String); virtual;
    Procedure SetscoreTag(AIndex : Integer; AValue : String); virtual;
    Procedure SetscoreValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetsocialRank(AIndex : Integer; AValue : TLeaderboardScoreRank); virtual;
    Procedure SettimeSpan(AIndex : Integer; AValue : String); virtual;
    Procedure SetwriteTimestamp(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property leaderboard_id : String Index 8 Read Fleaderboard_id Write Setleaderboard_id;
    Property publicRank : TLeaderboardScoreRank Index 16 Read FpublicRank Write SetpublicRank;
    Property scoreString : String Index 24 Read FscoreString Write SetscoreString;
    Property scoreTag : String Index 32 Read FscoreTag Write SetscoreTag;
    Property scoreValue : String Index 40 Read FscoreValue Write SetscoreValue;
    Property socialRank : TLeaderboardScoreRank Index 48 Read FsocialRank Write SetsocialRank;
    Property timeSpan : String Index 56 Read FtimeSpan Write SettimeSpan;
    Property writeTimestamp : String Index 64 Read FwriteTimestamp Write SetwriteTimestamp;
  end;
  TPlayerLeaderboardScoreClass = Class of TPlayerLeaderboardScore;
  
  { --------------------------------------------------------------------
    TPlayerLeaderboardScoreListResponse
    --------------------------------------------------------------------}
  
  TPlayerLeaderboardScoreListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TPlayerLeaderboardScoreListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    Fplayer : TPlayer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPlayerLeaderboardScoreListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setplayer(AIndex : Integer; AValue : TPlayer); virtual;
  Public
  Published
    Property items : TPlayerLeaderboardScoreListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property player : TPlayer Index 24 Read Fplayer Write Setplayer;
  end;
  TPlayerLeaderboardScoreListResponseClass = Class of TPlayerLeaderboardScoreListResponse;
  
  { --------------------------------------------------------------------
    TPlayerLevel
    --------------------------------------------------------------------}
  
  TPlayerLevel = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Flevel : integer;
    FmaxExperiencePoints : String;
    FminExperiencePoints : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxExperiencePoints(AIndex : Integer; AValue : String); virtual;
    Procedure SetminExperiencePoints(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property level : integer Index 8 Read Flevel Write Setlevel;
    Property maxExperiencePoints : String Index 16 Read FmaxExperiencePoints Write SetmaxExperiencePoints;
    Property minExperiencePoints : String Index 24 Read FminExperiencePoints Write SetminExperiencePoints;
  end;
  TPlayerLevelClass = Class of TPlayerLevel;
  
  { --------------------------------------------------------------------
    TPlayerListResponse
    --------------------------------------------------------------------}
  
  TPlayerListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TPlayerListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPlayerListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TPlayerListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TPlayerListResponseClass = Class of TPlayerListResponse;
  
  { --------------------------------------------------------------------
    TPlayerScore
    --------------------------------------------------------------------}
  
  TPlayerScore = Class(TGoogleBaseObject)
  Private
    FformattedScore : String;
    Fkind : String;
    Fscore : String;
    FscoreTag : String;
    FtimeSpan : String;
  Protected
    //Property setters
    Procedure SetformattedScore(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setscore(AIndex : Integer; AValue : String); virtual;
    Procedure SetscoreTag(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeSpan(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property formattedScore : String Index 0 Read FformattedScore Write SetformattedScore;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property score : String Index 16 Read Fscore Write Setscore;
    Property scoreTag : String Index 24 Read FscoreTag Write SetscoreTag;
    Property timeSpan : String Index 32 Read FtimeSpan Write SettimeSpan;
  end;
  TPlayerScoreClass = Class of TPlayerScore;
  
  { --------------------------------------------------------------------
    TPlayerScoreListResponse
    --------------------------------------------------------------------}
  
  TPlayerScoreListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FsubmittedScores : TPlayerScoreListResponseTypesubmittedScoresArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubmittedScores(AIndex : Integer; AValue : TPlayerScoreListResponseTypesubmittedScoresArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property submittedScores : TPlayerScoreListResponseTypesubmittedScoresArray Index 8 Read FsubmittedScores Write SetsubmittedScores;
  end;
  TPlayerScoreListResponseClass = Class of TPlayerScoreListResponse;
  
  { --------------------------------------------------------------------
    TPlayerScoreResponse
    --------------------------------------------------------------------}
  
  TPlayerScoreResponse = Class(TGoogleBaseObject)
  Private
    FbeatenScoreTimeSpans : TStringArray;
    FformattedScore : String;
    Fkind : String;
    FleaderboardId : String;
    FscoreTag : String;
    FunbeatenScores : TPlayerScoreResponseTypeunbeatenScoresArray;
  Protected
    //Property setters
    Procedure SetbeatenScoreTimeSpans(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetformattedScore(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetleaderboardId(AIndex : Integer; AValue : String); virtual;
    Procedure SetscoreTag(AIndex : Integer; AValue : String); virtual;
    Procedure SetunbeatenScores(AIndex : Integer; AValue : TPlayerScoreResponseTypeunbeatenScoresArray); virtual;
  Public
  Published
    Property beatenScoreTimeSpans : TStringArray Index 0 Read FbeatenScoreTimeSpans Write SetbeatenScoreTimeSpans;
    Property formattedScore : String Index 8 Read FformattedScore Write SetformattedScore;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property leaderboardId : String Index 24 Read FleaderboardId Write SetleaderboardId;
    Property scoreTag : String Index 32 Read FscoreTag Write SetscoreTag;
    Property unbeatenScores : TPlayerScoreResponseTypeunbeatenScoresArray Index 40 Read FunbeatenScores Write SetunbeatenScores;
  end;
  TPlayerScoreResponseClass = Class of TPlayerScoreResponse;
  
  { --------------------------------------------------------------------
    TPlayerScoreSubmissionList
    --------------------------------------------------------------------}
  
  TPlayerScoreSubmissionList = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fscores : TPlayerScoreSubmissionListTypescoresArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setscores(AIndex : Integer; AValue : TPlayerScoreSubmissionListTypescoresArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property scores : TPlayerScoreSubmissionListTypescoresArray Index 8 Read Fscores Write Setscores;
  end;
  TPlayerScoreSubmissionListClass = Class of TPlayerScoreSubmissionList;
  
  { --------------------------------------------------------------------
    TPushToken
    --------------------------------------------------------------------}
  
  TPushToken = Class(TGoogleBaseObject)
  Private
    FclientRevision : String;
    Fid : TPushTokenId;
    Fkind : String;
    Flanguage : String;
  Protected
    //Property setters
    Procedure SetclientRevision(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : TPushTokenId); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property clientRevision : String Index 0 Read FclientRevision Write SetclientRevision;
    Property id : TPushTokenId Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property language : String Index 24 Read Flanguage Write Setlanguage;
  end;
  TPushTokenClass = Class of TPushToken;
  
  { --------------------------------------------------------------------
    TPushTokenIdTypeios
    --------------------------------------------------------------------}
  
  TPushTokenIdTypeios = Class(TGoogleBaseObject)
  Private
    Fapns_device_token : String;
    Fapns_environment : String;
  Protected
    //Property setters
    Procedure Setapns_device_token(AIndex : Integer; AValue : String); virtual;
    Procedure Setapns_environment(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property apns_device_token : String Index 0 Read Fapns_device_token Write Setapns_device_token;
    Property apns_environment : String Index 8 Read Fapns_environment Write Setapns_environment;
  end;
  TPushTokenIdTypeiosClass = Class of TPushTokenIdTypeios;
  
  { --------------------------------------------------------------------
    TPushTokenId
    --------------------------------------------------------------------}
  
  TPushTokenId = Class(TGoogleBaseObject)
  Private
    Fios : TPushTokenIdTypeios;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setios(AIndex : Integer; AValue : TPushTokenIdTypeios); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ios : TPushTokenIdTypeios Index 0 Read Fios Write Setios;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TPushTokenIdClass = Class of TPushTokenId;
  
  { --------------------------------------------------------------------
    TQuest
    --------------------------------------------------------------------}
  
  TQuest = Class(TGoogleBaseObject)
  Private
    FacceptedTimestampMillis : String;
    FapplicationId : String;
    FbannerUrl : String;
    Fdescription : String;
    FendTimestampMillis : String;
    FiconUrl : String;
    Fid : String;
    FisDefaultBannerUrl : boolean;
    FisDefaultIconUrl : boolean;
    Fkind : String;
    FlastUpdatedTimestampMillis : String;
    Fmilestones : TQuestTypemilestonesArray;
    Fname : String;
    FnotifyTimestampMillis : String;
    FstartTimestampMillis : String;
    Fstate : String;
  Protected
    //Property setters
    Procedure SetacceptedTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetapplicationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetbannerUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetendTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SeticonUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetisDefaultBannerUrl(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisDefaultIconUrl(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastUpdatedTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure Setmilestones(AIndex : Integer; AValue : TQuestTypemilestonesArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetnotifyTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property acceptedTimestampMillis : String Index 0 Read FacceptedTimestampMillis Write SetacceptedTimestampMillis;
    Property applicationId : String Index 8 Read FapplicationId Write SetapplicationId;
    Property bannerUrl : String Index 16 Read FbannerUrl Write SetbannerUrl;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property endTimestampMillis : String Index 32 Read FendTimestampMillis Write SetendTimestampMillis;
    Property iconUrl : String Index 40 Read FiconUrl Write SeticonUrl;
    Property id : String Index 48 Read Fid Write Setid;
    Property isDefaultBannerUrl : boolean Index 56 Read FisDefaultBannerUrl Write SetisDefaultBannerUrl;
    Property isDefaultIconUrl : boolean Index 64 Read FisDefaultIconUrl Write SetisDefaultIconUrl;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property lastUpdatedTimestampMillis : String Index 80 Read FlastUpdatedTimestampMillis Write SetlastUpdatedTimestampMillis;
    Property milestones : TQuestTypemilestonesArray Index 88 Read Fmilestones Write Setmilestones;
    Property name : String Index 96 Read Fname Write Setname;
    Property notifyTimestampMillis : String Index 104 Read FnotifyTimestampMillis Write SetnotifyTimestampMillis;
    Property startTimestampMillis : String Index 112 Read FstartTimestampMillis Write SetstartTimestampMillis;
    Property state : String Index 120 Read Fstate Write Setstate;
  end;
  TQuestClass = Class of TQuest;
  
  { --------------------------------------------------------------------
    TQuestContribution
    --------------------------------------------------------------------}
  
  TQuestContribution = Class(TGoogleBaseObject)
  Private
    FformattedValue : String;
    Fkind : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure SetformattedValue(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property formattedValue : String Index 0 Read FformattedValue Write SetformattedValue;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TQuestContributionClass = Class of TQuestContribution;
  
  { --------------------------------------------------------------------
    TQuestCriterion
    --------------------------------------------------------------------}
  
  TQuestCriterion = Class(TGoogleBaseObject)
  Private
    FcompletionContribution : TQuestContribution;
    FcurrentContribution : TQuestContribution;
    FeventId : String;
    FinitialPlayerProgress : TQuestContribution;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetcompletionContribution(AIndex : Integer; AValue : TQuestContribution); virtual;
    Procedure SetcurrentContribution(AIndex : Integer; AValue : TQuestContribution); virtual;
    Procedure SeteventId(AIndex : Integer; AValue : String); virtual;
    Procedure SetinitialPlayerProgress(AIndex : Integer; AValue : TQuestContribution); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property completionContribution : TQuestContribution Index 0 Read FcompletionContribution Write SetcompletionContribution;
    Property currentContribution : TQuestContribution Index 8 Read FcurrentContribution Write SetcurrentContribution;
    Property eventId : String Index 16 Read FeventId Write SeteventId;
    Property initialPlayerProgress : TQuestContribution Index 24 Read FinitialPlayerProgress Write SetinitialPlayerProgress;
    Property kind : String Index 32 Read Fkind Write Setkind;
  end;
  TQuestCriterionClass = Class of TQuestCriterion;
  
  { --------------------------------------------------------------------
    TQuestListResponse
    --------------------------------------------------------------------}
  
  TQuestListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TQuestListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TQuestListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TQuestListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TQuestListResponseClass = Class of TQuestListResponse;
  
  { --------------------------------------------------------------------
    TQuestMilestone
    --------------------------------------------------------------------}
  
  TQuestMilestone = Class(TGoogleBaseObject)
  Private
    FcompletionRewardData : String;
    Fcriteria : TQuestMilestoneTypecriteriaArray;
    Fid : String;
    Fkind : String;
    Fstate : String;
  Protected
    //Property setters
    Procedure SetcompletionRewardData(AIndex : Integer; AValue : String); virtual;
    Procedure Setcriteria(AIndex : Integer; AValue : TQuestMilestoneTypecriteriaArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property completionRewardData : String Index 0 Read FcompletionRewardData Write SetcompletionRewardData;
    Property criteria : TQuestMilestoneTypecriteriaArray Index 8 Read Fcriteria Write Setcriteria;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property state : String Index 32 Read Fstate Write Setstate;
  end;
  TQuestMilestoneClass = Class of TQuestMilestone;
  
  { --------------------------------------------------------------------
    TRevisionCheckResponse
    --------------------------------------------------------------------}
  
  TRevisionCheckResponse = Class(TGoogleBaseObject)
  Private
    FapiVersion : String;
    Fkind : String;
    FrevisionStatus : String;
  Protected
    //Property setters
    Procedure SetapiVersion(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrevisionStatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property apiVersion : String Index 0 Read FapiVersion Write SetapiVersion;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property revisionStatus : String Index 16 Read FrevisionStatus Write SetrevisionStatus;
  end;
  TRevisionCheckResponseClass = Class of TRevisionCheckResponse;
  
  { --------------------------------------------------------------------
    TRoom
    --------------------------------------------------------------------}
  
  TRoom = Class(TGoogleBaseObject)
  Private
    FapplicationId : String;
    FautoMatchingCriteria : TRoomAutoMatchingCriteria;
    FautoMatchingStatus : TRoomAutoMatchStatus;
    FcreationDetails : TRoomModification;
    Fdescription : String;
    FinviterId : String;
    Fkind : String;
    FlastUpdateDetails : TRoomModification;
    Fparticipants : TRoomTypeparticipantsArray;
    FroomId : String;
    FroomStatusVersion : integer;
    Fstatus : String;
    Fvariant : integer;
  Protected
    //Property setters
    Procedure SetapplicationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetautoMatchingCriteria(AIndex : Integer; AValue : TRoomAutoMatchingCriteria); virtual;
    Procedure SetautoMatchingStatus(AIndex : Integer; AValue : TRoomAutoMatchStatus); virtual;
    Procedure SetcreationDetails(AIndex : Integer; AValue : TRoomModification); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetinviterId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastUpdateDetails(AIndex : Integer; AValue : TRoomModification); virtual;
    Procedure Setparticipants(AIndex : Integer; AValue : TRoomTypeparticipantsArray); virtual;
    Procedure SetroomId(AIndex : Integer; AValue : String); virtual;
    Procedure SetroomStatusVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property applicationId : String Index 0 Read FapplicationId Write SetapplicationId;
    Property autoMatchingCriteria : TRoomAutoMatchingCriteria Index 8 Read FautoMatchingCriteria Write SetautoMatchingCriteria;
    Property autoMatchingStatus : TRoomAutoMatchStatus Index 16 Read FautoMatchingStatus Write SetautoMatchingStatus;
    Property creationDetails : TRoomModification Index 24 Read FcreationDetails Write SetcreationDetails;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property inviterId : String Index 40 Read FinviterId Write SetinviterId;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property lastUpdateDetails : TRoomModification Index 56 Read FlastUpdateDetails Write SetlastUpdateDetails;
    Property participants : TRoomTypeparticipantsArray Index 64 Read Fparticipants Write Setparticipants;
    Property roomId : String Index 72 Read FroomId Write SetroomId;
    Property roomStatusVersion : integer Index 80 Read FroomStatusVersion Write SetroomStatusVersion;
    Property status : String Index 88 Read Fstatus Write Setstatus;
    Property variant : integer Index 96 Read Fvariant Write Setvariant;
  end;
  TRoomClass = Class of TRoom;
  
  { --------------------------------------------------------------------
    TRoomAutoMatchStatus
    --------------------------------------------------------------------}
  
  TRoomAutoMatchStatus = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FwaitEstimateSeconds : integer;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetwaitEstimateSeconds(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property waitEstimateSeconds : integer Index 8 Read FwaitEstimateSeconds Write SetwaitEstimateSeconds;
  end;
  TRoomAutoMatchStatusClass = Class of TRoomAutoMatchStatus;
  
  { --------------------------------------------------------------------
    TRoomAutoMatchingCriteria
    --------------------------------------------------------------------}
  
  TRoomAutoMatchingCriteria = Class(TGoogleBaseObject)
  Private
    FexclusiveBitmask : String;
    Fkind : String;
    FmaxAutoMatchingPlayers : integer;
    FminAutoMatchingPlayers : integer;
  Protected
    //Property setters
    Procedure SetexclusiveBitmask(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxAutoMatchingPlayers(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminAutoMatchingPlayers(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property exclusiveBitmask : String Index 0 Read FexclusiveBitmask Write SetexclusiveBitmask;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property maxAutoMatchingPlayers : integer Index 16 Read FmaxAutoMatchingPlayers Write SetmaxAutoMatchingPlayers;
    Property minAutoMatchingPlayers : integer Index 24 Read FminAutoMatchingPlayers Write SetminAutoMatchingPlayers;
  end;
  TRoomAutoMatchingCriteriaClass = Class of TRoomAutoMatchingCriteria;
  
  { --------------------------------------------------------------------
    TRoomClientAddress
    --------------------------------------------------------------------}
  
  TRoomClientAddress = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FxmppAddress : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetxmppAddress(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property xmppAddress : String Index 8 Read FxmppAddress Write SetxmppAddress;
  end;
  TRoomClientAddressClass = Class of TRoomClientAddress;
  
  { --------------------------------------------------------------------
    TRoomCreateRequest
    --------------------------------------------------------------------}
  
  TRoomCreateRequest = Class(TGoogleBaseObject)
  Private
    FautoMatchingCriteria : TRoomAutoMatchingCriteria;
    Fcapabilities : TStringArray;
    FclientAddress : TRoomClientAddress;
    FinvitedPlayerIds : TStringArray;
    Fkind : String;
    FnetworkDiagnostics : TNetworkDiagnostics;
    FrequestId : String;
    Fvariant : integer;
  Protected
    //Property setters
    Procedure SetautoMatchingCriteria(AIndex : Integer; AValue : TRoomAutoMatchingCriteria); virtual;
    Procedure Setcapabilities(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetclientAddress(AIndex : Integer; AValue : TRoomClientAddress); virtual;
    Procedure SetinvitedPlayerIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnetworkDiagnostics(AIndex : Integer; AValue : TNetworkDiagnostics); virtual;
    Procedure SetrequestId(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property autoMatchingCriteria : TRoomAutoMatchingCriteria Index 0 Read FautoMatchingCriteria Write SetautoMatchingCriteria;
    Property capabilities : TStringArray Index 8 Read Fcapabilities Write Setcapabilities;
    Property clientAddress : TRoomClientAddress Index 16 Read FclientAddress Write SetclientAddress;
    Property invitedPlayerIds : TStringArray Index 24 Read FinvitedPlayerIds Write SetinvitedPlayerIds;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property networkDiagnostics : TNetworkDiagnostics Index 40 Read FnetworkDiagnostics Write SetnetworkDiagnostics;
    Property requestId : String Index 48 Read FrequestId Write SetrequestId;
    Property variant : integer Index 56 Read Fvariant Write Setvariant;
  end;
  TRoomCreateRequestClass = Class of TRoomCreateRequest;
  
  { --------------------------------------------------------------------
    TRoomJoinRequest
    --------------------------------------------------------------------}
  
  TRoomJoinRequest = Class(TGoogleBaseObject)
  Private
    Fcapabilities : TStringArray;
    FclientAddress : TRoomClientAddress;
    Fkind : String;
    FnetworkDiagnostics : TNetworkDiagnostics;
  Protected
    //Property setters
    Procedure Setcapabilities(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetclientAddress(AIndex : Integer; AValue : TRoomClientAddress); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnetworkDiagnostics(AIndex : Integer; AValue : TNetworkDiagnostics); virtual;
  Public
  Published
    Property capabilities : TStringArray Index 0 Read Fcapabilities Write Setcapabilities;
    Property clientAddress : TRoomClientAddress Index 8 Read FclientAddress Write SetclientAddress;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property networkDiagnostics : TNetworkDiagnostics Index 24 Read FnetworkDiagnostics Write SetnetworkDiagnostics;
  end;
  TRoomJoinRequestClass = Class of TRoomJoinRequest;
  
  { --------------------------------------------------------------------
    TRoomLeaveDiagnostics
    --------------------------------------------------------------------}
  
  TRoomLeaveDiagnostics = Class(TGoogleBaseObject)
  Private
    FandroidNetworkSubtype : integer;
    FandroidNetworkType : integer;
    FiosNetworkType : integer;
    Fkind : String;
    FnetworkOperatorCode : String;
    FnetworkOperatorName : String;
    FpeerSession : TRoomLeaveDiagnosticsTypepeerSessionArray;
    FsocketsUsed : boolean;
  Protected
    //Property setters
    Procedure SetandroidNetworkSubtype(AIndex : Integer; AValue : integer); virtual;
    Procedure SetandroidNetworkType(AIndex : Integer; AValue : integer); virtual;
    Procedure SetiosNetworkType(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnetworkOperatorCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetnetworkOperatorName(AIndex : Integer; AValue : String); virtual;
    Procedure SetpeerSession(AIndex : Integer; AValue : TRoomLeaveDiagnosticsTypepeerSessionArray); virtual;
    Procedure SetsocketsUsed(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property androidNetworkSubtype : integer Index 0 Read FandroidNetworkSubtype Write SetandroidNetworkSubtype;
    Property androidNetworkType : integer Index 8 Read FandroidNetworkType Write SetandroidNetworkType;
    Property iosNetworkType : integer Index 16 Read FiosNetworkType Write SetiosNetworkType;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property networkOperatorCode : String Index 32 Read FnetworkOperatorCode Write SetnetworkOperatorCode;
    Property networkOperatorName : String Index 40 Read FnetworkOperatorName Write SetnetworkOperatorName;
    Property peerSession : TRoomLeaveDiagnosticsTypepeerSessionArray Index 48 Read FpeerSession Write SetpeerSession;
    Property socketsUsed : boolean Index 56 Read FsocketsUsed Write SetsocketsUsed;
  end;
  TRoomLeaveDiagnosticsClass = Class of TRoomLeaveDiagnostics;
  
  { --------------------------------------------------------------------
    TRoomLeaveRequest
    --------------------------------------------------------------------}
  
  TRoomLeaveRequest = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FleaveDiagnostics : TRoomLeaveDiagnostics;
    Freason : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetleaveDiagnostics(AIndex : Integer; AValue : TRoomLeaveDiagnostics); virtual;
    Procedure Setreason(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property leaveDiagnostics : TRoomLeaveDiagnostics Index 8 Read FleaveDiagnostics Write SetleaveDiagnostics;
    Property reason : String Index 16 Read Freason Write Setreason;
  end;
  TRoomLeaveRequestClass = Class of TRoomLeaveRequest;
  
  { --------------------------------------------------------------------
    TRoomList
    --------------------------------------------------------------------}
  
  TRoomList = Class(TGoogleBaseObject)
  Private
    Fitems : TRoomListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TRoomListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TRoomListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TRoomListClass = Class of TRoomList;
  
  { --------------------------------------------------------------------
    TRoomModification
    --------------------------------------------------------------------}
  
  TRoomModification = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FmodifiedTimestampMillis : String;
    FparticipantId : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodifiedTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetparticipantId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property modifiedTimestampMillis : String Index 8 Read FmodifiedTimestampMillis Write SetmodifiedTimestampMillis;
    Property participantId : String Index 16 Read FparticipantId Write SetparticipantId;
  end;
  TRoomModificationClass = Class of TRoomModification;
  
  { --------------------------------------------------------------------
    TRoomP2PStatus
    --------------------------------------------------------------------}
  
  TRoomP2PStatus = Class(TGoogleBaseObject)
  Private
    FconnectionSetupLatencyMillis : integer;
    Ferror : String;
    Ferror_reason : String;
    Fkind : String;
    FparticipantId : String;
    Fstatus : String;
    FunreliableRoundtripLatencyMillis : integer;
  Protected
    //Property setters
    Procedure SetconnectionSetupLatencyMillis(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterror(AIndex : Integer; AValue : String); virtual;
    Procedure Seterror_reason(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetparticipantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetunreliableRoundtripLatencyMillis(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property connectionSetupLatencyMillis : integer Index 0 Read FconnectionSetupLatencyMillis Write SetconnectionSetupLatencyMillis;
    Property error : String Index 8 Read Ferror Write Seterror;
    Property error_reason : String Index 16 Read Ferror_reason Write Seterror_reason;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property participantId : String Index 32 Read FparticipantId Write SetparticipantId;
    Property status : String Index 40 Read Fstatus Write Setstatus;
    Property unreliableRoundtripLatencyMillis : integer Index 48 Read FunreliableRoundtripLatencyMillis Write SetunreliableRoundtripLatencyMillis;
  end;
  TRoomP2PStatusClass = Class of TRoomP2PStatus;
  
  { --------------------------------------------------------------------
    TRoomP2PStatuses
    --------------------------------------------------------------------}
  
  TRoomP2PStatuses = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fupdates : TRoomP2PStatusesTypeupdatesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdates(AIndex : Integer; AValue : TRoomP2PStatusesTypeupdatesArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property updates : TRoomP2PStatusesTypeupdatesArray Index 8 Read Fupdates Write Setupdates;
  end;
  TRoomP2PStatusesClass = Class of TRoomP2PStatuses;
  
  { --------------------------------------------------------------------
    TRoomParticipant
    --------------------------------------------------------------------}
  
  TRoomParticipant = Class(TGoogleBaseObject)
  Private
    FautoMatched : boolean;
    FautoMatchedPlayer : TAnonymousPlayer;
    Fcapabilities : TStringArray;
    FclientAddress : TRoomClientAddress;
    Fconnected : boolean;
    Fid : String;
    Fkind : String;
    FleaveReason : String;
    Fplayer : TPlayer;
    Fstatus : String;
  Protected
    //Property setters
    Procedure SetautoMatched(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetautoMatchedPlayer(AIndex : Integer; AValue : TAnonymousPlayer); virtual;
    Procedure Setcapabilities(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetclientAddress(AIndex : Integer; AValue : TRoomClientAddress); virtual;
    Procedure Setconnected(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetleaveReason(AIndex : Integer; AValue : String); virtual;
    Procedure Setplayer(AIndex : Integer; AValue : TPlayer); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoMatched : boolean Index 0 Read FautoMatched Write SetautoMatched;
    Property autoMatchedPlayer : TAnonymousPlayer Index 8 Read FautoMatchedPlayer Write SetautoMatchedPlayer;
    Property capabilities : TStringArray Index 16 Read Fcapabilities Write Setcapabilities;
    Property clientAddress : TRoomClientAddress Index 24 Read FclientAddress Write SetclientAddress;
    Property connected : boolean Index 32 Read Fconnected Write Setconnected;
    Property id : String Index 40 Read Fid Write Setid;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property leaveReason : String Index 56 Read FleaveReason Write SetleaveReason;
    Property player : TPlayer Index 64 Read Fplayer Write Setplayer;
    Property status : String Index 72 Read Fstatus Write Setstatus;
  end;
  TRoomParticipantClass = Class of TRoomParticipant;
  
  { --------------------------------------------------------------------
    TRoomStatus
    --------------------------------------------------------------------}
  
  TRoomStatus = Class(TGoogleBaseObject)
  Private
    FautoMatchingStatus : TRoomAutoMatchStatus;
    Fkind : String;
    Fparticipants : TRoomStatusTypeparticipantsArray;
    FroomId : String;
    Fstatus : String;
    FstatusVersion : integer;
  Protected
    //Property setters
    Procedure SetautoMatchingStatus(AIndex : Integer; AValue : TRoomAutoMatchStatus); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setparticipants(AIndex : Integer; AValue : TRoomStatusTypeparticipantsArray); virtual;
    Procedure SetroomId(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetstatusVersion(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property autoMatchingStatus : TRoomAutoMatchStatus Index 0 Read FautoMatchingStatus Write SetautoMatchingStatus;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property participants : TRoomStatusTypeparticipantsArray Index 16 Read Fparticipants Write Setparticipants;
    Property roomId : String Index 24 Read FroomId Write SetroomId;
    Property status : String Index 32 Read Fstatus Write Setstatus;
    Property statusVersion : integer Index 40 Read FstatusVersion Write SetstatusVersion;
  end;
  TRoomStatusClass = Class of TRoomStatus;
  
  { --------------------------------------------------------------------
    TScoreSubmission
    --------------------------------------------------------------------}
  
  TScoreSubmission = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FleaderboardId : String;
    Fscore : String;
    FscoreTag : String;
    Fsignature : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetleaderboardId(AIndex : Integer; AValue : String); virtual;
    Procedure Setscore(AIndex : Integer; AValue : String); virtual;
    Procedure SetscoreTag(AIndex : Integer; AValue : String); virtual;
    Procedure Setsignature(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property leaderboardId : String Index 8 Read FleaderboardId Write SetleaderboardId;
    Property score : String Index 16 Read Fscore Write Setscore;
    Property scoreTag : String Index 24 Read FscoreTag Write SetscoreTag;
    Property signature : String Index 32 Read Fsignature Write Setsignature;
  end;
  TScoreSubmissionClass = Class of TScoreSubmission;
  
  { --------------------------------------------------------------------
    TSnapshot
    --------------------------------------------------------------------}
  
  TSnapshot = Class(TGoogleBaseObject)
  Private
    FcoverImage : TSnapshotImage;
    Fdescription : String;
    FdriveId : String;
    FdurationMillis : String;
    Fid : String;
    Fkind : String;
    FlastModifiedMillis : String;
    FprogressValue : String;
    Ftitle : String;
    F_type : String;
    FuniqueName : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcoverImage(AIndex : Integer; AValue : TSnapshotImage); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdriveId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdurationMillis(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModifiedMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetprogressValue(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure SetuniqueName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property coverImage : TSnapshotImage Index 0 Read FcoverImage Write SetcoverImage;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property driveId : String Index 16 Read FdriveId Write SetdriveId;
    Property durationMillis : String Index 24 Read FdurationMillis Write SetdurationMillis;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property lastModifiedMillis : String Index 48 Read FlastModifiedMillis Write SetlastModifiedMillis;
    Property progressValue : String Index 56 Read FprogressValue Write SetprogressValue;
    Property title : String Index 64 Read Ftitle Write Settitle;
    Property _type : String Index 72 Read F_type Write Set_type;
    Property uniqueName : String Index 80 Read FuniqueName Write SetuniqueName;
  end;
  TSnapshotClass = Class of TSnapshot;
  
  { --------------------------------------------------------------------
    TSnapshotImage
    --------------------------------------------------------------------}
  
  TSnapshotImage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fkind : String;
    Fmime_type : String;
    Furl : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmime_type(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property mime_type : String Index 16 Read Fmime_type Write Setmime_type;
    Property url : String Index 24 Read Furl Write Seturl;
    Property width : integer Index 32 Read Fwidth Write Setwidth;
  end;
  TSnapshotImageClass = Class of TSnapshotImage;
  
  { --------------------------------------------------------------------
    TSnapshotListResponse
    --------------------------------------------------------------------}
  
  TSnapshotListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TSnapshotListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSnapshotListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TSnapshotListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TSnapshotListResponseClass = Class of TSnapshotListResponse;
  
  { --------------------------------------------------------------------
    TTurnBasedAutoMatchingCriteria
    --------------------------------------------------------------------}
  
  TTurnBasedAutoMatchingCriteria = Class(TGoogleBaseObject)
  Private
    FexclusiveBitmask : String;
    Fkind : String;
    FmaxAutoMatchingPlayers : integer;
    FminAutoMatchingPlayers : integer;
  Protected
    //Property setters
    Procedure SetexclusiveBitmask(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxAutoMatchingPlayers(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminAutoMatchingPlayers(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property exclusiveBitmask : String Index 0 Read FexclusiveBitmask Write SetexclusiveBitmask;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property maxAutoMatchingPlayers : integer Index 16 Read FmaxAutoMatchingPlayers Write SetmaxAutoMatchingPlayers;
    Property minAutoMatchingPlayers : integer Index 24 Read FminAutoMatchingPlayers Write SetminAutoMatchingPlayers;
  end;
  TTurnBasedAutoMatchingCriteriaClass = Class of TTurnBasedAutoMatchingCriteria;
  
  { --------------------------------------------------------------------
    TTurnBasedMatch
    --------------------------------------------------------------------}
  
  TTurnBasedMatch = Class(TGoogleBaseObject)
  Private
    FapplicationId : String;
    FautoMatchingCriteria : TTurnBasedAutoMatchingCriteria;
    FcreationDetails : TTurnBasedMatchModification;
    Fdata : TTurnBasedMatchData;
    Fdescription : String;
    FinviterId : String;
    Fkind : String;
    FlastUpdateDetails : TTurnBasedMatchModification;
    FmatchId : String;
    FmatchNumber : integer;
    FmatchVersion : integer;
    Fparticipants : TTurnBasedMatchTypeparticipantsArray;
    FpendingParticipantId : String;
    FpreviousMatchData : TTurnBasedMatchData;
    FrematchId : String;
    Fresults : TTurnBasedMatchTyperesultsArray;
    Fstatus : String;
    FuserMatchStatus : String;
    Fvariant : integer;
    FwithParticipantId : String;
  Protected
    //Property setters
    Procedure SetapplicationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetautoMatchingCriteria(AIndex : Integer; AValue : TTurnBasedAutoMatchingCriteria); virtual;
    Procedure SetcreationDetails(AIndex : Integer; AValue : TTurnBasedMatchModification); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TTurnBasedMatchData); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetinviterId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastUpdateDetails(AIndex : Integer; AValue : TTurnBasedMatchModification); virtual;
    Procedure SetmatchId(AIndex : Integer; AValue : String); virtual;
    Procedure SetmatchNumber(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmatchVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure Setparticipants(AIndex : Integer; AValue : TTurnBasedMatchTypeparticipantsArray); virtual;
    Procedure SetpendingParticipantId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousMatchData(AIndex : Integer; AValue : TTurnBasedMatchData); virtual;
    Procedure SetrematchId(AIndex : Integer; AValue : String); virtual;
    Procedure Setresults(AIndex : Integer; AValue : TTurnBasedMatchTyperesultsArray); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserMatchStatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : integer); virtual;
    Procedure SetwithParticipantId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property applicationId : String Index 0 Read FapplicationId Write SetapplicationId;
    Property autoMatchingCriteria : TTurnBasedAutoMatchingCriteria Index 8 Read FautoMatchingCriteria Write SetautoMatchingCriteria;
    Property creationDetails : TTurnBasedMatchModification Index 16 Read FcreationDetails Write SetcreationDetails;
    Property data : TTurnBasedMatchData Index 24 Read Fdata Write Setdata;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property inviterId : String Index 40 Read FinviterId Write SetinviterId;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property lastUpdateDetails : TTurnBasedMatchModification Index 56 Read FlastUpdateDetails Write SetlastUpdateDetails;
    Property matchId : String Index 64 Read FmatchId Write SetmatchId;
    Property matchNumber : integer Index 72 Read FmatchNumber Write SetmatchNumber;
    Property matchVersion : integer Index 80 Read FmatchVersion Write SetmatchVersion;
    Property participants : TTurnBasedMatchTypeparticipantsArray Index 88 Read Fparticipants Write Setparticipants;
    Property pendingParticipantId : String Index 96 Read FpendingParticipantId Write SetpendingParticipantId;
    Property previousMatchData : TTurnBasedMatchData Index 104 Read FpreviousMatchData Write SetpreviousMatchData;
    Property rematchId : String Index 112 Read FrematchId Write SetrematchId;
    Property results : TTurnBasedMatchTyperesultsArray Index 120 Read Fresults Write Setresults;
    Property status : String Index 128 Read Fstatus Write Setstatus;
    Property userMatchStatus : String Index 136 Read FuserMatchStatus Write SetuserMatchStatus;
    Property variant : integer Index 144 Read Fvariant Write Setvariant;
    Property withParticipantId : String Index 152 Read FwithParticipantId Write SetwithParticipantId;
  end;
  TTurnBasedMatchClass = Class of TTurnBasedMatch;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchCreateRequest
    --------------------------------------------------------------------}
  
  TTurnBasedMatchCreateRequest = Class(TGoogleBaseObject)
  Private
    FautoMatchingCriteria : TTurnBasedAutoMatchingCriteria;
    FinvitedPlayerIds : TStringArray;
    Fkind : String;
    FrequestId : String;
    Fvariant : integer;
  Protected
    //Property setters
    Procedure SetautoMatchingCriteria(AIndex : Integer; AValue : TTurnBasedAutoMatchingCriteria); virtual;
    Procedure SetinvitedPlayerIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestId(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property autoMatchingCriteria : TTurnBasedAutoMatchingCriteria Index 0 Read FautoMatchingCriteria Write SetautoMatchingCriteria;
    Property invitedPlayerIds : TStringArray Index 8 Read FinvitedPlayerIds Write SetinvitedPlayerIds;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property requestId : String Index 24 Read FrequestId Write SetrequestId;
    Property variant : integer Index 32 Read Fvariant Write Setvariant;
  end;
  TTurnBasedMatchCreateRequestClass = Class of TTurnBasedMatchCreateRequest;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchData
    --------------------------------------------------------------------}
  
  TTurnBasedMatchData = Class(TGoogleBaseObject)
  Private
    Fdata : String;
    FdataAvailable : boolean;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; AValue : String); virtual;
    Procedure SetdataAvailable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property data : String Index 0 Read Fdata Write Setdata;
    Property dataAvailable : boolean Index 8 Read FdataAvailable Write SetdataAvailable;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TTurnBasedMatchDataClass = Class of TTurnBasedMatchData;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchDataRequest
    --------------------------------------------------------------------}
  
  TTurnBasedMatchDataRequest = Class(TGoogleBaseObject)
  Private
    Fdata : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property data : String Index 0 Read Fdata Write Setdata;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TTurnBasedMatchDataRequestClass = Class of TTurnBasedMatchDataRequest;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchList
    --------------------------------------------------------------------}
  
  TTurnBasedMatchList = Class(TGoogleBaseObject)
  Private
    Fitems : TTurnBasedMatchListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTurnBasedMatchListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TTurnBasedMatchListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TTurnBasedMatchListClass = Class of TTurnBasedMatchList;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchModification
    --------------------------------------------------------------------}
  
  TTurnBasedMatchModification = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FmodifiedTimestampMillis : String;
    FparticipantId : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodifiedTimestampMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetparticipantId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property modifiedTimestampMillis : String Index 8 Read FmodifiedTimestampMillis Write SetmodifiedTimestampMillis;
    Property participantId : String Index 16 Read FparticipantId Write SetparticipantId;
  end;
  TTurnBasedMatchModificationClass = Class of TTurnBasedMatchModification;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchParticipant
    --------------------------------------------------------------------}
  
  TTurnBasedMatchParticipant = Class(TGoogleBaseObject)
  Private
    FautoMatched : boolean;
    FautoMatchedPlayer : TAnonymousPlayer;
    Fid : String;
    Fkind : String;
    Fplayer : TPlayer;
    Fstatus : String;
  Protected
    //Property setters
    Procedure SetautoMatched(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetautoMatchedPlayer(AIndex : Integer; AValue : TAnonymousPlayer); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setplayer(AIndex : Integer; AValue : TPlayer); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoMatched : boolean Index 0 Read FautoMatched Write SetautoMatched;
    Property autoMatchedPlayer : TAnonymousPlayer Index 8 Read FautoMatchedPlayer Write SetautoMatchedPlayer;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property player : TPlayer Index 32 Read Fplayer Write Setplayer;
    Property status : String Index 40 Read Fstatus Write Setstatus;
  end;
  TTurnBasedMatchParticipantClass = Class of TTurnBasedMatchParticipant;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchRematch
    --------------------------------------------------------------------}
  
  TTurnBasedMatchRematch = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FpreviousMatch : TTurnBasedMatch;
    Frematch : TTurnBasedMatch;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousMatch(AIndex : Integer; AValue : TTurnBasedMatch); virtual;
    Procedure Setrematch(AIndex : Integer; AValue : TTurnBasedMatch); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property previousMatch : TTurnBasedMatch Index 8 Read FpreviousMatch Write SetpreviousMatch;
    Property rematch : TTurnBasedMatch Index 16 Read Frematch Write Setrematch;
  end;
  TTurnBasedMatchRematchClass = Class of TTurnBasedMatchRematch;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchResults
    --------------------------------------------------------------------}
  
  TTurnBasedMatchResults = Class(TGoogleBaseObject)
  Private
    Fdata : TTurnBasedMatchDataRequest;
    Fkind : String;
    FmatchVersion : integer;
    Fresults : TTurnBasedMatchResultsTyperesultsArray;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; AValue : TTurnBasedMatchDataRequest); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmatchVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure Setresults(AIndex : Integer; AValue : TTurnBasedMatchResultsTyperesultsArray); virtual;
  Public
  Published
    Property data : TTurnBasedMatchDataRequest Index 0 Read Fdata Write Setdata;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property matchVersion : integer Index 16 Read FmatchVersion Write SetmatchVersion;
    Property results : TTurnBasedMatchResultsTyperesultsArray Index 24 Read Fresults Write Setresults;
  end;
  TTurnBasedMatchResultsClass = Class of TTurnBasedMatchResults;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchSync
    --------------------------------------------------------------------}
  
  TTurnBasedMatchSync = Class(TGoogleBaseObject)
  Private
    Fitems : TTurnBasedMatchSyncTypeitemsArray;
    Fkind : String;
    FmoreAvailable : boolean;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTurnBasedMatchSyncTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmoreAvailable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TTurnBasedMatchSyncTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property moreAvailable : boolean Index 16 Read FmoreAvailable Write SetmoreAvailable;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TTurnBasedMatchSyncClass = Class of TTurnBasedMatchSync;
  
  { --------------------------------------------------------------------
    TTurnBasedMatchTurn
    --------------------------------------------------------------------}
  
  TTurnBasedMatchTurn = Class(TGoogleBaseObject)
  Private
    Fdata : TTurnBasedMatchDataRequest;
    Fkind : String;
    FmatchVersion : integer;
    FpendingParticipantId : String;
    Fresults : TTurnBasedMatchTurnTyperesultsArray;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; AValue : TTurnBasedMatchDataRequest); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmatchVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpendingParticipantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setresults(AIndex : Integer; AValue : TTurnBasedMatchTurnTyperesultsArray); virtual;
  Public
  Published
    Property data : TTurnBasedMatchDataRequest Index 0 Read Fdata Write Setdata;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property matchVersion : integer Index 16 Read FmatchVersion Write SetmatchVersion;
    Property pendingParticipantId : String Index 24 Read FpendingParticipantId Write SetpendingParticipantId;
    Property results : TTurnBasedMatchTurnTyperesultsArray Index 32 Read Fresults Write Setresults;
  end;
  TTurnBasedMatchTurnClass = Class of TTurnBasedMatchTurn;
  
  { --------------------------------------------------------------------
    TAchievementDefinitionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAchievementDefinitionsResource, method List
  
  TAchievementDefinitionsListOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  TAchievementDefinitionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TAchievementDefinitionsListResponse;
    Function List(AQuery : TAchievementDefinitionslistOptions) : TAchievementDefinitionsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAchievementsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAchievementsResource, method Increment
  
  TAchievementsIncrementOptions = Record
    requestId : int64;
    stepsToIncrement : integer;
  end;
  
  
  //Optional query Options for TAchievementsResource, method List
  
  TAchievementsListOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
    state : String;
  end;
  
  
  //Optional query Options for TAchievementsResource, method SetStepsAtLeast
  
  TAchievementsSetStepsAtLeastOptions = Record
    steps : integer;
  end;
  
  TAchievementsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Increment(achievementId: string; AQuery : string  = '') : TAchievementIncrementResponse;
    Function Increment(achievementId: string; AQuery : TAchievementsincrementOptions) : TAchievementIncrementResponse;
    Function List(playerId: string; AQuery : string  = '') : TPlayerAchievementListResponse;
    Function List(playerId: string; AQuery : TAchievementslistOptions) : TPlayerAchievementListResponse;
    Function Reveal(achievementId: string) : TAchievementRevealResponse;
    Function SetStepsAtLeast(achievementId: string; AQuery : string  = '') : TAchievementSetStepsAtLeastResponse;
    Function SetStepsAtLeast(achievementId: string; AQuery : TAchievementssetStepsAtLeastOptions) : TAchievementSetStepsAtLeastResponse;
    Function Unlock(achievementId: string) : TAchievementUnlockResponse;
    Function UpdateMultiple(aAchievementUpdateMultipleRequest : TAchievementUpdateMultipleRequest) : TAchievementUpdateMultipleResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TApplicationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TApplicationsResource, method Get
  
  TApplicationsGetOptions = Record
    language : String;
    platformType : String;
  end;
  
  TApplicationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(applicationId: string; AQuery : string  = '') : TApplication;
    Function Get(applicationId: string; AQuery : TApplicationsgetOptions) : TApplication;
    Procedure Played;
  end;
  
  
  { --------------------------------------------------------------------
    TEventsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEventsResource, method ListByPlayer
  
  TEventsListByPlayerOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TEventsResource, method ListDefinitions
  
  TEventsListDefinitionsOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TEventsResource, method Record
  
  TEventsRecordOptions = Record
    language : String;
  end;
  
  TEventsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function ListByPlayer(AQuery : string  = '') : TPlayerEventListResponse;
    Function ListByPlayer(AQuery : TEventslistByPlayerOptions) : TPlayerEventListResponse;
    Function ListDefinitions(AQuery : string  = '') : TEventDefinitionListResponse;
    Function ListDefinitions(AQuery : TEventslistDefinitionsOptions) : TEventDefinitionListResponse;
    Function _record(aEventRecordRequest : TEventRecordRequest; AQuery : string  = '') : TEventUpdateResponse;
    Function _record(aEventRecordRequest : TEventRecordRequest; AQuery : TEventsrecordOptions) : TEventUpdateResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TLeaderboardsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLeaderboardsResource, method Get
  
  TLeaderboardsGetOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TLeaderboardsResource, method List
  
  TLeaderboardsListOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  TLeaderboardsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(leaderboardId: string; AQuery : string  = '') : TLeaderboard;
    Function Get(leaderboardId: string; AQuery : TLeaderboardsgetOptions) : TLeaderboard;
    Function List(AQuery : string  = '') : TLeaderboardListResponse;
    Function List(AQuery : TLeaderboardslistOptions) : TLeaderboardListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TMetagameResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMetagameResource, method ListCategoriesByPlayer
  
  TMetagameListCategoriesByPlayerOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  TMetagameResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetMetagameConfig : TMetagameConfig;
    Function ListCategoriesByPlayer(collection: string; playerId: string; AQuery : string  = '') : TCategoryListResponse;
    Function ListCategoriesByPlayer(collection: string; playerId: string; AQuery : TMetagamelistCategoriesByPlayerOptions) : TCategoryListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TPlayersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPlayersResource, method Get
  
  TPlayersGetOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TPlayersResource, method List
  
  TPlayersListOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  TPlayersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(playerId: string; AQuery : string  = '') : TPlayer;
    Function Get(playerId: string; AQuery : TPlayersgetOptions) : TPlayer;
    Function List(collection: string; AQuery : string  = '') : TPlayerListResponse;
    Function List(collection: string; AQuery : TPlayerslistOptions) : TPlayerListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TPushtokensResource
    --------------------------------------------------------------------}
  
  TPushtokensResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Remove(aPushTokenId : TPushTokenId);
    Procedure Update(aPushToken : TPushToken);
  end;
  
  
  { --------------------------------------------------------------------
    TQuestMilestonesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TQuestMilestonesResource, method Claim
  
  TQuestMilestonesClaimOptions = Record
    requestId : int64;
  end;
  
  TQuestMilestonesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Claim(milestoneId: string; questId: string; AQuery : string  = '');
    Procedure Claim(milestoneId: string; questId: string; AQuery : TQuestMilestonesclaimOptions);
  end;
  
  
  { --------------------------------------------------------------------
    TQuestsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TQuestsResource, method Accept
  
  TQuestsAcceptOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TQuestsResource, method List
  
  TQuestsListOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  TQuestsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Accept(questId: string; AQuery : string  = '') : TQuest;
    Function Accept(questId: string; AQuery : TQuestsacceptOptions) : TQuest;
    Function List(playerId: string; AQuery : string  = '') : TQuestListResponse;
    Function List(playerId: string; AQuery : TQuestslistOptions) : TQuestListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TRevisionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRevisionsResource, method Check
  
  TRevisionsCheckOptions = Record
    clientRevision : String;
  end;
  
  TRevisionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Check(AQuery : string  = '') : TRevisionCheckResponse;
    Function Check(AQuery : TRevisionscheckOptions) : TRevisionCheckResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TRoomsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRoomsResource, method Create
  
  TRoomsCreateOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TRoomsResource, method Decline
  
  TRoomsDeclineOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TRoomsResource, method Get
  
  TRoomsGetOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TRoomsResource, method Join
  
  TRoomsJoinOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TRoomsResource, method Leave
  
  TRoomsLeaveOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TRoomsResource, method List
  
  TRoomsListOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TRoomsResource, method ReportStatus
  
  TRoomsReportStatusOptions = Record
    language : String;
  end;
  
  TRoomsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aRoomCreateRequest : TRoomCreateRequest; AQuery : string  = '') : TRoom;overload;
    Function Create(aRoomCreateRequest : TRoomCreateRequest; AQuery : TRoomscreateOptions) : TRoom;overload;
    Function Decline(roomId: string; AQuery : string  = '') : TRoom;
    Function Decline(roomId: string; AQuery : TRoomsdeclineOptions) : TRoom;
    Procedure Dismiss(roomId: string);
    Function Get(roomId: string; AQuery : string  = '') : TRoom;
    Function Get(roomId: string; AQuery : TRoomsgetOptions) : TRoom;
    Function Join(roomId: string; aRoomJoinRequest : TRoomJoinRequest; AQuery : string  = '') : TRoom;
    Function Join(roomId: string; aRoomJoinRequest : TRoomJoinRequest; AQuery : TRoomsjoinOptions) : TRoom;
    Function Leave(roomId: string; aRoomLeaveRequest : TRoomLeaveRequest; AQuery : string  = '') : TRoom;
    Function Leave(roomId: string; aRoomLeaveRequest : TRoomLeaveRequest; AQuery : TRoomsleaveOptions) : TRoom;
    Function List(AQuery : string  = '') : TRoomList;
    Function List(AQuery : TRoomslistOptions) : TRoomList;
    Function ReportStatus(roomId: string; aRoomP2PStatuses : TRoomP2PStatuses; AQuery : string  = '') : TRoomStatus;
    Function ReportStatus(roomId: string; aRoomP2PStatuses : TRoomP2PStatuses; AQuery : TRoomsreportStatusOptions) : TRoomStatus;
  end;
  
  
  { --------------------------------------------------------------------
    TScoresResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TScoresResource, method Get
  
  TScoresGetOptions = Record
    includeRankType : String;
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TScoresResource, method List
  
  TScoresListOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
    timeSpan : String;
  end;
  
  
  //Optional query Options for TScoresResource, method ListWindow
  
  TScoresListWindowOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
    resultsAbove : integer;
    returnTopIfAbsent : boolean;
    timeSpan : String;
  end;
  
  
  //Optional query Options for TScoresResource, method Submit
  
  TScoresSubmitOptions = Record
    language : String;
    score : int64;
    scoreTag : String;
  end;
  
  
  //Optional query Options for TScoresResource, method SubmitMultiple
  
  TScoresSubmitMultipleOptions = Record
    language : String;
  end;
  
  TScoresResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(leaderboardId: string; playerId: string; timeSpan: string; AQuery : string  = '') : TPlayerLeaderboardScoreListResponse;
    Function Get(leaderboardId: string; playerId: string; timeSpan: string; AQuery : TScoresgetOptions) : TPlayerLeaderboardScoreListResponse;
    Function List(collection: string; leaderboardId: string; AQuery : string  = '') : TLeaderboardScores;
    Function List(collection: string; leaderboardId: string; AQuery : TScoreslistOptions) : TLeaderboardScores;
    Function ListWindow(collection: string; leaderboardId: string; AQuery : string  = '') : TLeaderboardScores;
    Function ListWindow(collection: string; leaderboardId: string; AQuery : TScoreslistWindowOptions) : TLeaderboardScores;
    Function Submit(leaderboardId: string; AQuery : string  = '') : TPlayerScoreResponse;
    Function Submit(leaderboardId: string; AQuery : TScoressubmitOptions) : TPlayerScoreResponse;
    Function SubmitMultiple(aPlayerScoreSubmissionList : TPlayerScoreSubmissionList; AQuery : string  = '') : TPlayerScoreListResponse;
    Function SubmitMultiple(aPlayerScoreSubmissionList : TPlayerScoreSubmissionList; AQuery : TScoressubmitMultipleOptions) : TPlayerScoreListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSnapshotsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSnapshotsResource, method Get
  
  TSnapshotsGetOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TSnapshotsResource, method List
  
  TSnapshotsListOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  TSnapshotsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(snapshotId: string; AQuery : string  = '') : TSnapshot;
    Function Get(snapshotId: string; AQuery : TSnapshotsgetOptions) : TSnapshot;
    Function List(playerId: string; AQuery : string  = '') : TSnapshotListResponse;
    Function List(playerId: string; AQuery : TSnapshotslistOptions) : TSnapshotListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTurnBasedMatchesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTurnBasedMatchesResource, method Create
  
  TTurnBasedMatchesCreateOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method Decline
  
  TTurnBasedMatchesDeclineOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method Finish
  
  TTurnBasedMatchesFinishOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method Get
  
  TTurnBasedMatchesGetOptions = Record
    includeMatchData : boolean;
    language : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method Join
  
  TTurnBasedMatchesJoinOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method Leave
  
  TTurnBasedMatchesLeaveOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method LeaveTurn
  
  TTurnBasedMatchesLeaveTurnOptions = Record
    language : String;
    matchVersion : integer;
    pendingParticipantId : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method List
  
  TTurnBasedMatchesListOptions = Record
    includeMatchData : boolean;
    language : String;
    maxCompletedMatches : integer;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method Rematch
  
  TTurnBasedMatchesRematchOptions = Record
    language : String;
    requestId : int64;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method Sync
  
  TTurnBasedMatchesSyncOptions = Record
    includeMatchData : boolean;
    language : String;
    maxCompletedMatches : integer;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TTurnBasedMatchesResource, method TakeTurn
  
  TTurnBasedMatchesTakeTurnOptions = Record
    language : String;
  end;
  
  TTurnBasedMatchesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Cancel(matchId: string);
    Function Create(aTurnBasedMatchCreateRequest : TTurnBasedMatchCreateRequest; AQuery : string  = '') : TTurnBasedMatch;overload;
    Function Create(aTurnBasedMatchCreateRequest : TTurnBasedMatchCreateRequest; AQuery : TTurnBasedMatchescreateOptions) : TTurnBasedMatch;overload;
    Function Decline(matchId: string; AQuery : string  = '') : TTurnBasedMatch;
    Function Decline(matchId: string; AQuery : TTurnBasedMatchesdeclineOptions) : TTurnBasedMatch;
    Procedure Dismiss(matchId: string);
    Function Finish(matchId: string; aTurnBasedMatchResults : TTurnBasedMatchResults; AQuery : string  = '') : TTurnBasedMatch;
    Function Finish(matchId: string; aTurnBasedMatchResults : TTurnBasedMatchResults; AQuery : TTurnBasedMatchesfinishOptions) : TTurnBasedMatch;
    Function Get(matchId: string; AQuery : string  = '') : TTurnBasedMatch;
    Function Get(matchId: string; AQuery : TTurnBasedMatchesgetOptions) : TTurnBasedMatch;
    Function Join(matchId: string; AQuery : string  = '') : TTurnBasedMatch;
    Function Join(matchId: string; AQuery : TTurnBasedMatchesjoinOptions) : TTurnBasedMatch;
    Function Leave(matchId: string; AQuery : string  = '') : TTurnBasedMatch;
    Function Leave(matchId: string; AQuery : TTurnBasedMatchesleaveOptions) : TTurnBasedMatch;
    Function LeaveTurn(matchId: string; AQuery : string  = '') : TTurnBasedMatch;
    Function LeaveTurn(matchId: string; AQuery : TTurnBasedMatchesleaveTurnOptions) : TTurnBasedMatch;
    Function List(AQuery : string  = '') : TTurnBasedMatchList;
    Function List(AQuery : TTurnBasedMatcheslistOptions) : TTurnBasedMatchList;
    Function Rematch(matchId: string; AQuery : string  = '') : TTurnBasedMatchRematch;
    Function Rematch(matchId: string; AQuery : TTurnBasedMatchesrematchOptions) : TTurnBasedMatchRematch;
    Function Sync(AQuery : string  = '') : TTurnBasedMatchSync;
    Function Sync(AQuery : TTurnBasedMatchessyncOptions) : TTurnBasedMatchSync;
    Function TakeTurn(matchId: string; aTurnBasedMatchTurn : TTurnBasedMatchTurn; AQuery : string  = '') : TTurnBasedMatch;
    Function TakeTurn(matchId: string; aTurnBasedMatchTurn : TTurnBasedMatchTurn; AQuery : TTurnBasedMatchestakeTurnOptions) : TTurnBasedMatch;
  end;
  
  
  { --------------------------------------------------------------------
    TGamesAPI
    --------------------------------------------------------------------}
  
  TGamesAPI = Class(TGoogleAPI)
  Private
    FAchievementDefinitionsInstance : TAchievementDefinitionsResource;
    FAchievementsInstance : TAchievementsResource;
    FApplicationsInstance : TApplicationsResource;
    FEventsInstance : TEventsResource;
    FLeaderboardsInstance : TLeaderboardsResource;
    FMetagameInstance : TMetagameResource;
    FPlayersInstance : TPlayersResource;
    FPushtokensInstance : TPushtokensResource;
    FQuestMilestonesInstance : TQuestMilestonesResource;
    FQuestsInstance : TQuestsResource;
    FRevisionsInstance : TRevisionsResource;
    FRoomsInstance : TRoomsResource;
    FScoresInstance : TScoresResource;
    FSnapshotsInstance : TSnapshotsResource;
    FTurnBasedMatchesInstance : TTurnBasedMatchesResource;
    Function GetAchievementDefinitionsInstance : TAchievementDefinitionsResource;virtual;
    Function GetAchievementsInstance : TAchievementsResource;virtual;
    Function GetApplicationsInstance : TApplicationsResource;virtual;
    Function GetEventsInstance : TEventsResource;virtual;
    Function GetLeaderboardsInstance : TLeaderboardsResource;virtual;
    Function GetMetagameInstance : TMetagameResource;virtual;
    Function GetPlayersInstance : TPlayersResource;virtual;
    Function GetPushtokensInstance : TPushtokensResource;virtual;
    Function GetQuestMilestonesInstance : TQuestMilestonesResource;virtual;
    Function GetQuestsInstance : TQuestsResource;virtual;
    Function GetRevisionsInstance : TRevisionsResource;virtual;
    Function GetRoomsInstance : TRoomsResource;virtual;
    Function GetScoresInstance : TScoresResource;virtual;
    Function GetSnapshotsInstance : TSnapshotsResource;virtual;
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
    Function CreateAchievementDefinitionsResource(AOwner : TComponent) : TAchievementDefinitionsResource;virtual;overload;
    Function CreateAchievementDefinitionsResource : TAchievementDefinitionsResource;virtual;overload;
    Function CreateAchievementsResource(AOwner : TComponent) : TAchievementsResource;virtual;overload;
    Function CreateAchievementsResource : TAchievementsResource;virtual;overload;
    Function CreateApplicationsResource(AOwner : TComponent) : TApplicationsResource;virtual;overload;
    Function CreateApplicationsResource : TApplicationsResource;virtual;overload;
    Function CreateEventsResource(AOwner : TComponent) : TEventsResource;virtual;overload;
    Function CreateEventsResource : TEventsResource;virtual;overload;
    Function CreateLeaderboardsResource(AOwner : TComponent) : TLeaderboardsResource;virtual;overload;
    Function CreateLeaderboardsResource : TLeaderboardsResource;virtual;overload;
    Function CreateMetagameResource(AOwner : TComponent) : TMetagameResource;virtual;overload;
    Function CreateMetagameResource : TMetagameResource;virtual;overload;
    Function CreatePlayersResource(AOwner : TComponent) : TPlayersResource;virtual;overload;
    Function CreatePlayersResource : TPlayersResource;virtual;overload;
    Function CreatePushtokensResource(AOwner : TComponent) : TPushtokensResource;virtual;overload;
    Function CreatePushtokensResource : TPushtokensResource;virtual;overload;
    Function CreateQuestMilestonesResource(AOwner : TComponent) : TQuestMilestonesResource;virtual;overload;
    Function CreateQuestMilestonesResource : TQuestMilestonesResource;virtual;overload;
    Function CreateQuestsResource(AOwner : TComponent) : TQuestsResource;virtual;overload;
    Function CreateQuestsResource : TQuestsResource;virtual;overload;
    Function CreateRevisionsResource(AOwner : TComponent) : TRevisionsResource;virtual;overload;
    Function CreateRevisionsResource : TRevisionsResource;virtual;overload;
    Function CreateRoomsResource(AOwner : TComponent) : TRoomsResource;virtual;overload;
    Function CreateRoomsResource : TRoomsResource;virtual;overload;
    Function CreateScoresResource(AOwner : TComponent) : TScoresResource;virtual;overload;
    Function CreateScoresResource : TScoresResource;virtual;overload;
    Function CreateSnapshotsResource(AOwner : TComponent) : TSnapshotsResource;virtual;overload;
    Function CreateSnapshotsResource : TSnapshotsResource;virtual;overload;
    Function CreateTurnBasedMatchesResource(AOwner : TComponent) : TTurnBasedMatchesResource;virtual;overload;
    Function CreateTurnBasedMatchesResource : TTurnBasedMatchesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AchievementDefinitionsResource : TAchievementDefinitionsResource Read GetAchievementDefinitionsInstance;
    Property AchievementsResource : TAchievementsResource Read GetAchievementsInstance;
    Property ApplicationsResource : TApplicationsResource Read GetApplicationsInstance;
    Property EventsResource : TEventsResource Read GetEventsInstance;
    Property LeaderboardsResource : TLeaderboardsResource Read GetLeaderboardsInstance;
    Property MetagameResource : TMetagameResource Read GetMetagameInstance;
    Property PlayersResource : TPlayersResource Read GetPlayersInstance;
    Property PushtokensResource : TPushtokensResource Read GetPushtokensInstance;
    Property QuestMilestonesResource : TQuestMilestonesResource Read GetQuestMilestonesInstance;
    Property QuestsResource : TQuestsResource Read GetQuestsInstance;
    Property RevisionsResource : TRevisionsResource Read GetRevisionsInstance;
    Property RoomsResource : TRoomsResource Read GetRoomsInstance;
    Property ScoresResource : TScoresResource Read GetScoresInstance;
    Property SnapshotsResource : TSnapshotsResource Read GetSnapshotsInstance;
    Property TurnBasedMatchesResource : TTurnBasedMatchesResource Read GetTurnBasedMatchesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAchievementDefinition
  --------------------------------------------------------------------}


Procedure TAchievementDefinition.SetachievementType(AIndex : Integer; AValue : String); 

begin
  If (FachievementType=AValue) then exit;
  FachievementType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.SetexperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FexperiencePoints=AValue) then exit;
  FexperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.SetformattedTotalSteps(AIndex : Integer; AValue : String); 

begin
  If (FformattedTotalSteps=AValue) then exit;
  FformattedTotalSteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.SetinitialState(AIndex : Integer; AValue : String); 

begin
  If (FinitialState=AValue) then exit;
  FinitialState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.SetisRevealedIconUrlDefault(AIndex : Integer; AValue : boolean); 

begin
  If (FisRevealedIconUrlDefault=AValue) then exit;
  FisRevealedIconUrlDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.SetisUnlockedIconUrlDefault(AIndex : Integer; AValue : boolean); 

begin
  If (FisUnlockedIconUrlDefault=AValue) then exit;
  FisUnlockedIconUrlDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.SetrevealedIconUrl(AIndex : Integer; AValue : String); 

begin
  If (FrevealedIconUrl=AValue) then exit;
  FrevealedIconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.SettotalSteps(AIndex : Integer; AValue : integer); 

begin
  If (FtotalSteps=AValue) then exit;
  FtotalSteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinition.SetunlockedIconUrl(AIndex : Integer; AValue : String); 

begin
  If (FunlockedIconUrl=AValue) then exit;
  FunlockedIconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementDefinitionsListResponse
  --------------------------------------------------------------------}


Procedure TAchievementDefinitionsListResponse.Setitems(AIndex : Integer; AValue : TAchievementDefinitionsListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinitionsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementDefinitionsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementIncrementResponse
  --------------------------------------------------------------------}


Procedure TAchievementIncrementResponse.SetcurrentSteps(AIndex : Integer; AValue : integer); 

begin
  If (FcurrentSteps=AValue) then exit;
  FcurrentSteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementIncrementResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementIncrementResponse.SetnewlyUnlocked(AIndex : Integer; AValue : boolean); 

begin
  If (FnewlyUnlocked=AValue) then exit;
  FnewlyUnlocked:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementRevealResponse
  --------------------------------------------------------------------}


Procedure TAchievementRevealResponse.SetcurrentState(AIndex : Integer; AValue : String); 

begin
  If (FcurrentState=AValue) then exit;
  FcurrentState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementRevealResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementSetStepsAtLeastResponse
  --------------------------------------------------------------------}


Procedure TAchievementSetStepsAtLeastResponse.SetcurrentSteps(AIndex : Integer; AValue : integer); 

begin
  If (FcurrentSteps=AValue) then exit;
  FcurrentSteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementSetStepsAtLeastResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementSetStepsAtLeastResponse.SetnewlyUnlocked(AIndex : Integer; AValue : boolean); 

begin
  If (FnewlyUnlocked=AValue) then exit;
  FnewlyUnlocked:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementUnlockResponse
  --------------------------------------------------------------------}


Procedure TAchievementUnlockResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUnlockResponse.SetnewlyUnlocked(AIndex : Integer; AValue : boolean); 

begin
  If (FnewlyUnlocked=AValue) then exit;
  FnewlyUnlocked:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementUpdateMultipleRequest
  --------------------------------------------------------------------}


Procedure TAchievementUpdateMultipleRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateMultipleRequest.Setupdates(AIndex : Integer; AValue : TAchievementUpdateMultipleRequestTypeupdatesArray); 

begin
  If (Fupdates=AValue) then exit;
  Fupdates:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementUpdateMultipleResponse
  --------------------------------------------------------------------}


Procedure TAchievementUpdateMultipleResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateMultipleResponse.SetupdatedAchievements(AIndex : Integer; AValue : TAchievementUpdateMultipleResponseTypeupdatedAchievementsArray); 

begin
  If (FupdatedAchievements=AValue) then exit;
  FupdatedAchievements:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementUpdateRequest
  --------------------------------------------------------------------}


Procedure TAchievementUpdateRequest.SetachievementId(AIndex : Integer; AValue : String); 

begin
  If (FachievementId=AValue) then exit;
  FachievementId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateRequest.SetincrementPayload(AIndex : Integer; AValue : TGamesAchievementIncrement); 

begin
  If (FincrementPayload=AValue) then exit;
  FincrementPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateRequest.SetsetStepsAtLeastPayload(AIndex : Integer; AValue : TGamesAchievementSetStepsAtLeast); 

begin
  If (FsetStepsAtLeastPayload=AValue) then exit;
  FsetStepsAtLeastPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateRequest.SetupdateType(AIndex : Integer; AValue : String); 

begin
  If (FupdateType=AValue) then exit;
  FupdateType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementUpdateResponse
  --------------------------------------------------------------------}


Procedure TAchievementUpdateResponse.SetachievementId(AIndex : Integer; AValue : String); 

begin
  If (FachievementId=AValue) then exit;
  FachievementId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateResponse.SetcurrentState(AIndex : Integer; AValue : String); 

begin
  If (FcurrentState=AValue) then exit;
  FcurrentState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateResponse.SetcurrentSteps(AIndex : Integer; AValue : integer); 

begin
  If (FcurrentSteps=AValue) then exit;
  FcurrentSteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateResponse.SetnewlyUnlocked(AIndex : Integer; AValue : boolean); 

begin
  If (FnewlyUnlocked=AValue) then exit;
  FnewlyUnlocked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementUpdateResponse.SetupdateOccurred(AIndex : Integer; AValue : boolean); 

begin
  If (FupdateOccurred=AValue) then exit;
  FupdateOccurred:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAggregateStats
  --------------------------------------------------------------------}


Procedure TAggregateStats.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateStats.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateStats.Setmax(AIndex : Integer; AValue : String); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateStats.Setmin(AIndex : Integer; AValue : String); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateStats.Setsum(AIndex : Integer; AValue : String); 

begin
  If (Fsum=AValue) then exit;
  Fsum:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnonymousPlayer
  --------------------------------------------------------------------}


Procedure TAnonymousPlayer.SetavatarImageUrl(AIndex : Integer; AValue : String); 

begin
  If (FavatarImageUrl=AValue) then exit;
  FavatarImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnonymousPlayer.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnonymousPlayer.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApplication
  --------------------------------------------------------------------}


Procedure TApplication.Setachievement_count(AIndex : Integer; AValue : integer); 

begin
  If (Fachievement_count=AValue) then exit;
  Fachievement_count:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setassets(AIndex : Integer; AValue : TApplicationTypeassetsArray); 

begin
  If (Fassets=AValue) then exit;
  Fassets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setauthor(AIndex : Integer; AValue : String); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setcategory(AIndex : Integer; AValue : TApplicationCategory); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetenabledFeatures(AIndex : Integer; AValue : TStringArray); 

begin
  If (FenabledFeatures=AValue) then exit;
  FenabledFeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setinstances(AIndex : Integer; AValue : TApplicationTypeinstancesArray); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetlastUpdatedTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FlastUpdatedTimestamp=AValue) then exit;
  FlastUpdatedTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setleaderboard_count(AIndex : Integer; AValue : integer); 

begin
  If (Fleaderboard_count=AValue) then exit;
  Fleaderboard_count:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetthemeColor(AIndex : Integer; AValue : String); 

begin
  If (FthemeColor=AValue) then exit;
  FthemeColor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApplicationCategory
  --------------------------------------------------------------------}


Procedure TApplicationCategory.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplicationCategory.Setprimary(AIndex : Integer; AValue : String); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplicationCategory.Setsecondary(AIndex : Integer; AValue : String); 

begin
  If (Fsecondary=AValue) then exit;
  Fsecondary:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCategory
  --------------------------------------------------------------------}


Procedure TCategory.Setcategory(AIndex : Integer; AValue : String); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategory.SetexperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FexperiencePoints=AValue) then exit;
  FexperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategory.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCategoryListResponse
  --------------------------------------------------------------------}


Procedure TCategoryListResponse.Setitems(AIndex : Integer; AValue : TCategoryListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategoryListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCategoryListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventBatchRecordFailure
  --------------------------------------------------------------------}


Procedure TEventBatchRecordFailure.SetfailureCause(AIndex : Integer; AValue : String); 

begin
  If (FfailureCause=AValue) then exit;
  FfailureCause:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventBatchRecordFailure.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventBatchRecordFailure.Setrange(AIndex : Integer; AValue : TEventPeriodRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventChild
  --------------------------------------------------------------------}


Procedure TEventChild.SetchildId(AIndex : Integer; AValue : String); 

begin
  If (FchildId=AValue) then exit;
  FchildId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventChild.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventDefinition
  --------------------------------------------------------------------}


Procedure TEventDefinition.SetchildEvents(AIndex : Integer; AValue : TEventDefinitionTypechildEventsArray); 

begin
  If (FchildEvents=AValue) then exit;
  FchildEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinition.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinition.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinition.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinition.SetimageUrl(AIndex : Integer; AValue : String); 

begin
  If (FimageUrl=AValue) then exit;
  FimageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinition.SetisDefaultImageUrl(AIndex : Integer; AValue : boolean); 

begin
  If (FisDefaultImageUrl=AValue) then exit;
  FisDefaultImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinition.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinition.Setvisibility(AIndex : Integer; AValue : String); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventDefinitionListResponse
  --------------------------------------------------------------------}


Procedure TEventDefinitionListResponse.Setitems(AIndex : Integer; AValue : TEventDefinitionListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinitionListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDefinitionListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventPeriodRange
  --------------------------------------------------------------------}


Procedure TEventPeriodRange.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventPeriodRange.SetperiodEndMillis(AIndex : Integer; AValue : String); 

begin
  If (FperiodEndMillis=AValue) then exit;
  FperiodEndMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventPeriodRange.SetperiodStartMillis(AIndex : Integer; AValue : String); 

begin
  If (FperiodStartMillis=AValue) then exit;
  FperiodStartMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventPeriodUpdate
  --------------------------------------------------------------------}


Procedure TEventPeriodUpdate.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventPeriodUpdate.SettimePeriod(AIndex : Integer; AValue : TEventPeriodRange); 

begin
  If (FtimePeriod=AValue) then exit;
  FtimePeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventPeriodUpdate.Setupdates(AIndex : Integer; AValue : TEventPeriodUpdateTypeupdatesArray); 

begin
  If (Fupdates=AValue) then exit;
  Fupdates:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventRecordFailure
  --------------------------------------------------------------------}


Procedure TEventRecordFailure.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventRecordFailure.SetfailureCause(AIndex : Integer; AValue : String); 

begin
  If (FfailureCause=AValue) then exit;
  FfailureCause:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventRecordFailure.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventRecordRequest
  --------------------------------------------------------------------}


Procedure TEventRecordRequest.SetcurrentTimeMillis(AIndex : Integer; AValue : String); 

begin
  If (FcurrentTimeMillis=AValue) then exit;
  FcurrentTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventRecordRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventRecordRequest.SetrequestId(AIndex : Integer; AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventRecordRequest.SettimePeriods(AIndex : Integer; AValue : TEventRecordRequestTypetimePeriodsArray); 

begin
  If (FtimePeriods=AValue) then exit;
  FtimePeriods:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventUpdateRequest
  --------------------------------------------------------------------}


Procedure TEventUpdateRequest.SetdefinitionId(AIndex : Integer; AValue : String); 

begin
  If (FdefinitionId=AValue) then exit;
  FdefinitionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventUpdateRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventUpdateRequest.SetupdateCount(AIndex : Integer; AValue : String); 

begin
  If (FupdateCount=AValue) then exit;
  FupdateCount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventUpdateResponse
  --------------------------------------------------------------------}


Procedure TEventUpdateResponse.SetbatchFailures(AIndex : Integer; AValue : TEventUpdateResponseTypebatchFailuresArray); 

begin
  If (FbatchFailures=AValue) then exit;
  FbatchFailures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventUpdateResponse.SeteventFailures(AIndex : Integer; AValue : TEventUpdateResponseTypeeventFailuresArray); 

begin
  If (FeventFailures=AValue) then exit;
  FeventFailures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventUpdateResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventUpdateResponse.SetplayerEvents(AIndex : Integer; AValue : TEventUpdateResponseTypeplayerEventsArray); 

begin
  If (FplayerEvents=AValue) then exit;
  FplayerEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGamesAchievementIncrement
  --------------------------------------------------------------------}


Procedure TGamesAchievementIncrement.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesAchievementIncrement.SetrequestId(AIndex : Integer; AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesAchievementIncrement.Setsteps(AIndex : Integer; AValue : integer); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGamesAchievementSetStepsAtLeast
  --------------------------------------------------------------------}


Procedure TGamesAchievementSetStepsAtLeast.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesAchievementSetStepsAtLeast.Setsteps(AIndex : Integer; AValue : integer); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImageAsset
  --------------------------------------------------------------------}


Procedure TImageAsset.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageAsset.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageAsset.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageAsset.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageAsset.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstance
  --------------------------------------------------------------------}


Procedure TInstance.SetacquisitionUri(AIndex : Integer; AValue : String); 

begin
  If (FacquisitionUri=AValue) then exit;
  FacquisitionUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetandroidInstance(AIndex : Integer; AValue : TInstanceAndroidDetails); 

begin
  If (FandroidInstance=AValue) then exit;
  FandroidInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetiosInstance(AIndex : Integer; AValue : TInstanceIosDetails); 

begin
  If (FiosInstance=AValue) then exit;
  FiosInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetplatformType(AIndex : Integer; AValue : String); 

begin
  If (FplatformType=AValue) then exit;
  FplatformType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetrealtimePlay(AIndex : Integer; AValue : boolean); 

begin
  If (FrealtimePlay=AValue) then exit;
  FrealtimePlay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetturnBasedPlay(AIndex : Integer; AValue : boolean); 

begin
  If (FturnBasedPlay=AValue) then exit;
  FturnBasedPlay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetwebInstance(AIndex : Integer; AValue : TInstanceWebDetails); 

begin
  If (FwebInstance=AValue) then exit;
  FwebInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceAndroidDetails
  --------------------------------------------------------------------}


Procedure TInstanceAndroidDetails.SetenablePiracyCheck(AIndex : Integer; AValue : boolean); 

begin
  If (FenablePiracyCheck=AValue) then exit;
  FenablePiracyCheck:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceAndroidDetails.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceAndroidDetails.SetpackageName(AIndex : Integer; AValue : String); 

begin
  If (FpackageName=AValue) then exit;
  FpackageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceAndroidDetails.Setpreferred(AIndex : Integer; AValue : boolean); 

begin
  If (Fpreferred=AValue) then exit;
  Fpreferred:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceIosDetails
  --------------------------------------------------------------------}


Procedure TInstanceIosDetails.SetbundleIdentifier(AIndex : Integer; AValue : String); 

begin
  If (FbundleIdentifier=AValue) then exit;
  FbundleIdentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceIosDetails.SetitunesAppId(AIndex : Integer; AValue : String); 

begin
  If (FitunesAppId=AValue) then exit;
  FitunesAppId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceIosDetails.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceIosDetails.SetpreferredForIpad(AIndex : Integer; AValue : boolean); 

begin
  If (FpreferredForIpad=AValue) then exit;
  FpreferredForIpad:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceIosDetails.SetpreferredForIphone(AIndex : Integer; AValue : boolean); 

begin
  If (FpreferredForIphone=AValue) then exit;
  FpreferredForIphone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceIosDetails.SetsupportIpad(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportIpad=AValue) then exit;
  FsupportIpad:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceIosDetails.SetsupportIphone(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportIphone=AValue) then exit;
  FsupportIphone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceWebDetails
  --------------------------------------------------------------------}


Procedure TInstanceWebDetails.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceWebDetails.SetlaunchUrl(AIndex : Integer; AValue : String); 

begin
  If (FlaunchUrl=AValue) then exit;
  FlaunchUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceWebDetails.Setpreferred(AIndex : Integer; AValue : boolean); 

begin
  If (Fpreferred=AValue) then exit;
  Fpreferred:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaderboard
  --------------------------------------------------------------------}


Procedure TLeaderboard.SeticonUrl(AIndex : Integer; AValue : String); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboard.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboard.SetisIconUrlDefault(AIndex : Integer; AValue : boolean); 

begin
  If (FisIconUrlDefault=AValue) then exit;
  FisIconUrlDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboard.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboard.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboard.Setorder(AIndex : Integer; AValue : String); 

begin
  If (Forder=AValue) then exit;
  Forder:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaderboardEntry
  --------------------------------------------------------------------}


Procedure TLeaderboardEntry.SetformattedScore(AIndex : Integer; AValue : String); 

begin
  If (FformattedScore=AValue) then exit;
  FformattedScore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardEntry.SetformattedScoreRank(AIndex : Integer; AValue : String); 

begin
  If (FformattedScoreRank=AValue) then exit;
  FformattedScoreRank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardEntry.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardEntry.Setplayer(AIndex : Integer; AValue : TPlayer); 

begin
  If (Fplayer=AValue) then exit;
  Fplayer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardEntry.SetscoreRank(AIndex : Integer; AValue : String); 

begin
  If (FscoreRank=AValue) then exit;
  FscoreRank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardEntry.SetscoreTag(AIndex : Integer; AValue : String); 

begin
  If (FscoreTag=AValue) then exit;
  FscoreTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardEntry.SetscoreValue(AIndex : Integer; AValue : String); 

begin
  If (FscoreValue=AValue) then exit;
  FscoreValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardEntry.SettimeSpan(AIndex : Integer; AValue : String); 

begin
  If (FtimeSpan=AValue) then exit;
  FtimeSpan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardEntry.SetwriteTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FwriteTimestampMillis=AValue) then exit;
  FwriteTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaderboardListResponse
  --------------------------------------------------------------------}


Procedure TLeaderboardListResponse.Setitems(AIndex : Integer; AValue : TLeaderboardListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaderboardScoreRank
  --------------------------------------------------------------------}


Procedure TLeaderboardScoreRank.SetformattedNumScores(AIndex : Integer; AValue : String); 

begin
  If (FformattedNumScores=AValue) then exit;
  FformattedNumScores:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScoreRank.SetformattedRank(AIndex : Integer; AValue : String); 

begin
  If (FformattedRank=AValue) then exit;
  FformattedRank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScoreRank.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScoreRank.SetnumScores(AIndex : Integer; AValue : String); 

begin
  If (FnumScores=AValue) then exit;
  FnumScores:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScoreRank.Setrank(AIndex : Integer; AValue : String); 

begin
  If (Frank=AValue) then exit;
  Frank:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaderboardScores
  --------------------------------------------------------------------}


Procedure TLeaderboardScores.Setitems(AIndex : Integer; AValue : TLeaderboardScoresTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScores.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScores.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScores.SetnumScores(AIndex : Integer; AValue : String); 

begin
  If (FnumScores=AValue) then exit;
  FnumScores:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScores.SetplayerScore(AIndex : Integer; AValue : TLeaderboardEntry); 

begin
  If (FplayerScore=AValue) then exit;
  FplayerScore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardScores.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetagameConfig
  --------------------------------------------------------------------}


Procedure TMetagameConfig.SetcurrentVersion(AIndex : Integer; AValue : integer); 

begin
  If (FcurrentVersion=AValue) then exit;
  FcurrentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetagameConfig.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetagameConfig.SetplayerLevels(AIndex : Integer; AValue : TMetagameConfigTypeplayerLevelsArray); 

begin
  If (FplayerLevels=AValue) then exit;
  FplayerLevels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetworkDiagnostics
  --------------------------------------------------------------------}


Procedure TNetworkDiagnostics.SetandroidNetworkSubtype(AIndex : Integer; AValue : integer); 

begin
  If (FandroidNetworkSubtype=AValue) then exit;
  FandroidNetworkSubtype:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkDiagnostics.SetandroidNetworkType(AIndex : Integer; AValue : integer); 

begin
  If (FandroidNetworkType=AValue) then exit;
  FandroidNetworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkDiagnostics.SetiosNetworkType(AIndex : Integer; AValue : integer); 

begin
  If (FiosNetworkType=AValue) then exit;
  FiosNetworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkDiagnostics.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkDiagnostics.SetnetworkOperatorCode(AIndex : Integer; AValue : String); 

begin
  If (FnetworkOperatorCode=AValue) then exit;
  FnetworkOperatorCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkDiagnostics.SetnetworkOperatorName(AIndex : Integer; AValue : String); 

begin
  If (FnetworkOperatorName=AValue) then exit;
  FnetworkOperatorName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkDiagnostics.SetregistrationLatencyMillis(AIndex : Integer; AValue : integer); 

begin
  If (FregistrationLatencyMillis=AValue) then exit;
  FregistrationLatencyMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParticipantResult
  --------------------------------------------------------------------}


Procedure TParticipantResult.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParticipantResult.SetparticipantId(AIndex : Integer; AValue : String); 

begin
  If (FparticipantId=AValue) then exit;
  FparticipantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParticipantResult.Setplacing(AIndex : Integer; AValue : integer); 

begin
  If (Fplacing=AValue) then exit;
  Fplacing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParticipantResult.Setresult(AIndex : Integer; AValue : String); 

begin
  If (Fresult=AValue) then exit;
  Fresult:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPeerChannelDiagnostics
  --------------------------------------------------------------------}


Procedure TPeerChannelDiagnostics.SetbytesReceived(AIndex : Integer; AValue : TAggregateStats); 

begin
  If (FbytesReceived=AValue) then exit;
  FbytesReceived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerChannelDiagnostics.SetbytesSent(AIndex : Integer; AValue : TAggregateStats); 

begin
  If (FbytesSent=AValue) then exit;
  FbytesSent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerChannelDiagnostics.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerChannelDiagnostics.SetnumMessagesLost(AIndex : Integer; AValue : integer); 

begin
  If (FnumMessagesLost=AValue) then exit;
  FnumMessagesLost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerChannelDiagnostics.SetnumMessagesReceived(AIndex : Integer; AValue : integer); 

begin
  If (FnumMessagesReceived=AValue) then exit;
  FnumMessagesReceived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerChannelDiagnostics.SetnumMessagesSent(AIndex : Integer; AValue : integer); 

begin
  If (FnumMessagesSent=AValue) then exit;
  FnumMessagesSent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerChannelDiagnostics.SetnumSendFailures(AIndex : Integer; AValue : integer); 

begin
  If (FnumSendFailures=AValue) then exit;
  FnumSendFailures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerChannelDiagnostics.SetroundtripLatencyMillis(AIndex : Integer; AValue : TAggregateStats); 

begin
  If (FroundtripLatencyMillis=AValue) then exit;
  FroundtripLatencyMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPeerSessionDiagnostics
  --------------------------------------------------------------------}


Procedure TPeerSessionDiagnostics.SetconnectedTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FconnectedTimestampMillis=AValue) then exit;
  FconnectedTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerSessionDiagnostics.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerSessionDiagnostics.SetparticipantId(AIndex : Integer; AValue : String); 

begin
  If (FparticipantId=AValue) then exit;
  FparticipantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerSessionDiagnostics.SetreliableChannel(AIndex : Integer; AValue : TPeerChannelDiagnostics); 

begin
  If (FreliableChannel=AValue) then exit;
  FreliableChannel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeerSessionDiagnostics.SetunreliableChannel(AIndex : Integer; AValue : TPeerChannelDiagnostics); 

begin
  If (FunreliableChannel=AValue) then exit;
  FunreliableChannel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayed
  --------------------------------------------------------------------}


Procedure TPlayed.SetautoMatched(AIndex : Integer; AValue : boolean); 

begin
  If (FautoMatched=AValue) then exit;
  FautoMatched:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayed.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayed.SettimeMillis(AIndex : Integer; AValue : String); 

begin
  If (FtimeMillis=AValue) then exit;
  FtimeMillis:=AValue;
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



Procedure TPlayer.SetexperienceInfo(AIndex : Integer; AValue : TPlayerExperienceInfo); 

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



Procedure TPlayer.SetlastPlayedWith(AIndex : Integer; AValue : TPlayed); 

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
  TPlayerAchievement
  --------------------------------------------------------------------}


Procedure TPlayerAchievement.SetachievementState(AIndex : Integer; AValue : String); 

begin
  If (FachievementState=AValue) then exit;
  FachievementState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerAchievement.SetcurrentSteps(AIndex : Integer; AValue : integer); 

begin
  If (FcurrentSteps=AValue) then exit;
  FcurrentSteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerAchievement.SetexperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FexperiencePoints=AValue) then exit;
  FexperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerAchievement.SetformattedCurrentStepsString(AIndex : Integer; AValue : String); 

begin
  If (FformattedCurrentStepsString=AValue) then exit;
  FformattedCurrentStepsString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerAchievement.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerAchievement.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerAchievement.SetlastUpdatedTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FlastUpdatedTimestamp=AValue) then exit;
  FlastUpdatedTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerAchievementListResponse
  --------------------------------------------------------------------}


Procedure TPlayerAchievementListResponse.Setitems(AIndex : Integer; AValue : TPlayerAchievementListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerAchievementListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerAchievementListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerEvent
  --------------------------------------------------------------------}


Procedure TPlayerEvent.SetdefinitionId(AIndex : Integer; AValue : String); 

begin
  If (FdefinitionId=AValue) then exit;
  FdefinitionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerEvent.SetformattedNumEvents(AIndex : Integer; AValue : String); 

begin
  If (FformattedNumEvents=AValue) then exit;
  FformattedNumEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerEvent.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerEvent.SetnumEvents(AIndex : Integer; AValue : String); 

begin
  If (FnumEvents=AValue) then exit;
  FnumEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerEvent.SetplayerId(AIndex : Integer; AValue : String); 

begin
  If (FplayerId=AValue) then exit;
  FplayerId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerEventListResponse
  --------------------------------------------------------------------}


Procedure TPlayerEventListResponse.Setitems(AIndex : Integer; AValue : TPlayerEventListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerEventListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerEventListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerExperienceInfo
  --------------------------------------------------------------------}


Procedure TPlayerExperienceInfo.SetcurrentExperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FcurrentExperiencePoints=AValue) then exit;
  FcurrentExperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerExperienceInfo.SetcurrentLevel(AIndex : Integer; AValue : TPlayerLevel); 

begin
  If (FcurrentLevel=AValue) then exit;
  FcurrentLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerExperienceInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerExperienceInfo.SetlastLevelUpTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FlastLevelUpTimestampMillis=AValue) then exit;
  FlastLevelUpTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerExperienceInfo.SetnextLevel(AIndex : Integer; AValue : TPlayerLevel); 

begin
  If (FnextLevel=AValue) then exit;
  FnextLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerLeaderboardScore
  --------------------------------------------------------------------}


Procedure TPlayerLeaderboardScore.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScore.Setleaderboard_id(AIndex : Integer; AValue : String); 

begin
  If (Fleaderboard_id=AValue) then exit;
  Fleaderboard_id:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScore.SetpublicRank(AIndex : Integer; AValue : TLeaderboardScoreRank); 

begin
  If (FpublicRank=AValue) then exit;
  FpublicRank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScore.SetscoreString(AIndex : Integer; AValue : String); 

begin
  If (FscoreString=AValue) then exit;
  FscoreString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScore.SetscoreTag(AIndex : Integer; AValue : String); 

begin
  If (FscoreTag=AValue) then exit;
  FscoreTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScore.SetscoreValue(AIndex : Integer; AValue : String); 

begin
  If (FscoreValue=AValue) then exit;
  FscoreValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScore.SetsocialRank(AIndex : Integer; AValue : TLeaderboardScoreRank); 

begin
  If (FsocialRank=AValue) then exit;
  FsocialRank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScore.SettimeSpan(AIndex : Integer; AValue : String); 

begin
  If (FtimeSpan=AValue) then exit;
  FtimeSpan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScore.SetwriteTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FwriteTimestamp=AValue) then exit;
  FwriteTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerLeaderboardScoreListResponse
  --------------------------------------------------------------------}


Procedure TPlayerLeaderboardScoreListResponse.Setitems(AIndex : Integer; AValue : TPlayerLeaderboardScoreListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScoreListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScoreListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLeaderboardScoreListResponse.Setplayer(AIndex : Integer; AValue : TPlayer); 

begin
  If (Fplayer=AValue) then exit;
  Fplayer:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerLevel
  --------------------------------------------------------------------}


Procedure TPlayerLevel.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLevel.Setlevel(AIndex : Integer; AValue : integer); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLevel.SetmaxExperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FmaxExperiencePoints=AValue) then exit;
  FmaxExperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerLevel.SetminExperiencePoints(AIndex : Integer; AValue : String); 

begin
  If (FminExperiencePoints=AValue) then exit;
  FminExperiencePoints:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerListResponse
  --------------------------------------------------------------------}


Procedure TPlayerListResponse.Setitems(AIndex : Integer; AValue : TPlayerListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerScore
  --------------------------------------------------------------------}


Procedure TPlayerScore.SetformattedScore(AIndex : Integer; AValue : String); 

begin
  If (FformattedScore=AValue) then exit;
  FformattedScore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScore.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScore.Setscore(AIndex : Integer; AValue : String); 

begin
  If (Fscore=AValue) then exit;
  Fscore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScore.SetscoreTag(AIndex : Integer; AValue : String); 

begin
  If (FscoreTag=AValue) then exit;
  FscoreTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScore.SettimeSpan(AIndex : Integer; AValue : String); 

begin
  If (FtimeSpan=AValue) then exit;
  FtimeSpan:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerScoreListResponse
  --------------------------------------------------------------------}


Procedure TPlayerScoreListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreListResponse.SetsubmittedScores(AIndex : Integer; AValue : TPlayerScoreListResponseTypesubmittedScoresArray); 

begin
  If (FsubmittedScores=AValue) then exit;
  FsubmittedScores:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerScoreResponse
  --------------------------------------------------------------------}


Procedure TPlayerScoreResponse.SetbeatenScoreTimeSpans(AIndex : Integer; AValue : TStringArray); 

begin
  If (FbeatenScoreTimeSpans=AValue) then exit;
  FbeatenScoreTimeSpans:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreResponse.SetformattedScore(AIndex : Integer; AValue : String); 

begin
  If (FformattedScore=AValue) then exit;
  FformattedScore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreResponse.SetleaderboardId(AIndex : Integer; AValue : String); 

begin
  If (FleaderboardId=AValue) then exit;
  FleaderboardId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreResponse.SetscoreTag(AIndex : Integer; AValue : String); 

begin
  If (FscoreTag=AValue) then exit;
  FscoreTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreResponse.SetunbeatenScores(AIndex : Integer; AValue : TPlayerScoreResponseTypeunbeatenScoresArray); 

begin
  If (FunbeatenScores=AValue) then exit;
  FunbeatenScores:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlayerScoreSubmissionList
  --------------------------------------------------------------------}


Procedure TPlayerScoreSubmissionList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlayerScoreSubmissionList.Setscores(AIndex : Integer; AValue : TPlayerScoreSubmissionListTypescoresArray); 

begin
  If (Fscores=AValue) then exit;
  Fscores:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPushToken
  --------------------------------------------------------------------}


Procedure TPushToken.SetclientRevision(AIndex : Integer; AValue : String); 

begin
  If (FclientRevision=AValue) then exit;
  FclientRevision:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPushToken.Setid(AIndex : Integer; AValue : TPushTokenId); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPushToken.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPushToken.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPushTokenIdTypeios
  --------------------------------------------------------------------}


Procedure TPushTokenIdTypeios.Setapns_device_token(AIndex : Integer; AValue : String); 

begin
  If (Fapns_device_token=AValue) then exit;
  Fapns_device_token:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPushTokenIdTypeios.Setapns_environment(AIndex : Integer; AValue : String); 

begin
  If (Fapns_environment=AValue) then exit;
  Fapns_environment:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPushTokenId
  --------------------------------------------------------------------}


Procedure TPushTokenId.Setios(AIndex : Integer; AValue : TPushTokenIdTypeios); 

begin
  If (Fios=AValue) then exit;
  Fios:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPushTokenId.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuest
  --------------------------------------------------------------------}


Procedure TQuest.SetacceptedTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FacceptedTimestampMillis=AValue) then exit;
  FacceptedTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SetapplicationId(AIndex : Integer; AValue : String); 

begin
  If (FapplicationId=AValue) then exit;
  FapplicationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SetbannerUrl(AIndex : Integer; AValue : String); 

begin
  If (FbannerUrl=AValue) then exit;
  FbannerUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SetendTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FendTimestampMillis=AValue) then exit;
  FendTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SeticonUrl(AIndex : Integer; AValue : String); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SetisDefaultBannerUrl(AIndex : Integer; AValue : boolean); 

begin
  If (FisDefaultBannerUrl=AValue) then exit;
  FisDefaultBannerUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SetisDefaultIconUrl(AIndex : Integer; AValue : boolean); 

begin
  If (FisDefaultIconUrl=AValue) then exit;
  FisDefaultIconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SetlastUpdatedTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FlastUpdatedTimestampMillis=AValue) then exit;
  FlastUpdatedTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.Setmilestones(AIndex : Integer; AValue : TQuestTypemilestonesArray); 

begin
  If (Fmilestones=AValue) then exit;
  Fmilestones:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SetnotifyTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FnotifyTimestampMillis=AValue) then exit;
  FnotifyTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.SetstartTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FstartTimestampMillis=AValue) then exit;
  FstartTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuest.Setstate(AIndex : Integer; AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuestContribution
  --------------------------------------------------------------------}


Procedure TQuestContribution.SetformattedValue(AIndex : Integer; AValue : String); 

begin
  If (FformattedValue=AValue) then exit;
  FformattedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestContribution.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestContribution.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuestCriterion
  --------------------------------------------------------------------}


Procedure TQuestCriterion.SetcompletionContribution(AIndex : Integer; AValue : TQuestContribution); 

begin
  If (FcompletionContribution=AValue) then exit;
  FcompletionContribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestCriterion.SetcurrentContribution(AIndex : Integer; AValue : TQuestContribution); 

begin
  If (FcurrentContribution=AValue) then exit;
  FcurrentContribution:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestCriterion.SeteventId(AIndex : Integer; AValue : String); 

begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestCriterion.SetinitialPlayerProgress(AIndex : Integer; AValue : TQuestContribution); 

begin
  If (FinitialPlayerProgress=AValue) then exit;
  FinitialPlayerProgress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestCriterion.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuestListResponse
  --------------------------------------------------------------------}


Procedure TQuestListResponse.Setitems(AIndex : Integer; AValue : TQuestListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuestMilestone
  --------------------------------------------------------------------}


Procedure TQuestMilestone.SetcompletionRewardData(AIndex : Integer; AValue : String); 

begin
  If (FcompletionRewardData=AValue) then exit;
  FcompletionRewardData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestMilestone.Setcriteria(AIndex : Integer; AValue : TQuestMilestoneTypecriteriaArray); 

begin
  If (Fcriteria=AValue) then exit;
  Fcriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestMilestone.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestMilestone.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuestMilestone.Setstate(AIndex : Integer; AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRevisionCheckResponse
  --------------------------------------------------------------------}


Procedure TRevisionCheckResponse.SetapiVersion(AIndex : Integer; AValue : String); 

begin
  If (FapiVersion=AValue) then exit;
  FapiVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevisionCheckResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevisionCheckResponse.SetrevisionStatus(AIndex : Integer; AValue : String); 

begin
  If (FrevisionStatus=AValue) then exit;
  FrevisionStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoom
  --------------------------------------------------------------------}


Procedure TRoom.SetapplicationId(AIndex : Integer; AValue : String); 

begin
  If (FapplicationId=AValue) then exit;
  FapplicationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.SetautoMatchingCriteria(AIndex : Integer; AValue : TRoomAutoMatchingCriteria); 

begin
  If (FautoMatchingCriteria=AValue) then exit;
  FautoMatchingCriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.SetautoMatchingStatus(AIndex : Integer; AValue : TRoomAutoMatchStatus); 

begin
  If (FautoMatchingStatus=AValue) then exit;
  FautoMatchingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.SetcreationDetails(AIndex : Integer; AValue : TRoomModification); 

begin
  If (FcreationDetails=AValue) then exit;
  FcreationDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.SetinviterId(AIndex : Integer; AValue : String); 

begin
  If (FinviterId=AValue) then exit;
  FinviterId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.SetlastUpdateDetails(AIndex : Integer; AValue : TRoomModification); 

begin
  If (FlastUpdateDetails=AValue) then exit;
  FlastUpdateDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.Setparticipants(AIndex : Integer; AValue : TRoomTypeparticipantsArray); 

begin
  If (Fparticipants=AValue) then exit;
  Fparticipants:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.SetroomId(AIndex : Integer; AValue : String); 

begin
  If (FroomId=AValue) then exit;
  FroomId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.SetroomStatusVersion(AIndex : Integer; AValue : integer); 

begin
  If (FroomStatusVersion=AValue) then exit;
  FroomStatusVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoom.Setvariant(AIndex : Integer; AValue : integer); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomAutoMatchStatus
  --------------------------------------------------------------------}


Procedure TRoomAutoMatchStatus.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomAutoMatchStatus.SetwaitEstimateSeconds(AIndex : Integer; AValue : integer); 

begin
  If (FwaitEstimateSeconds=AValue) then exit;
  FwaitEstimateSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomAutoMatchingCriteria
  --------------------------------------------------------------------}


Procedure TRoomAutoMatchingCriteria.SetexclusiveBitmask(AIndex : Integer; AValue : String); 

begin
  If (FexclusiveBitmask=AValue) then exit;
  FexclusiveBitmask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomAutoMatchingCriteria.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomAutoMatchingCriteria.SetmaxAutoMatchingPlayers(AIndex : Integer; AValue : integer); 

begin
  If (FmaxAutoMatchingPlayers=AValue) then exit;
  FmaxAutoMatchingPlayers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomAutoMatchingCriteria.SetminAutoMatchingPlayers(AIndex : Integer; AValue : integer); 

begin
  If (FminAutoMatchingPlayers=AValue) then exit;
  FminAutoMatchingPlayers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomClientAddress
  --------------------------------------------------------------------}


Procedure TRoomClientAddress.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomClientAddress.SetxmppAddress(AIndex : Integer; AValue : String); 

begin
  If (FxmppAddress=AValue) then exit;
  FxmppAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomCreateRequest
  --------------------------------------------------------------------}


Procedure TRoomCreateRequest.SetautoMatchingCriteria(AIndex : Integer; AValue : TRoomAutoMatchingCriteria); 

begin
  If (FautoMatchingCriteria=AValue) then exit;
  FautoMatchingCriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomCreateRequest.Setcapabilities(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fcapabilities=AValue) then exit;
  Fcapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomCreateRequest.SetclientAddress(AIndex : Integer; AValue : TRoomClientAddress); 

begin
  If (FclientAddress=AValue) then exit;
  FclientAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomCreateRequest.SetinvitedPlayerIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FinvitedPlayerIds=AValue) then exit;
  FinvitedPlayerIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomCreateRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomCreateRequest.SetnetworkDiagnostics(AIndex : Integer; AValue : TNetworkDiagnostics); 

begin
  If (FnetworkDiagnostics=AValue) then exit;
  FnetworkDiagnostics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomCreateRequest.SetrequestId(AIndex : Integer; AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomCreateRequest.Setvariant(AIndex : Integer; AValue : integer); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomJoinRequest
  --------------------------------------------------------------------}


Procedure TRoomJoinRequest.Setcapabilities(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fcapabilities=AValue) then exit;
  Fcapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomJoinRequest.SetclientAddress(AIndex : Integer; AValue : TRoomClientAddress); 

begin
  If (FclientAddress=AValue) then exit;
  FclientAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomJoinRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomJoinRequest.SetnetworkDiagnostics(AIndex : Integer; AValue : TNetworkDiagnostics); 

begin
  If (FnetworkDiagnostics=AValue) then exit;
  FnetworkDiagnostics:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomLeaveDiagnostics
  --------------------------------------------------------------------}


Procedure TRoomLeaveDiagnostics.SetandroidNetworkSubtype(AIndex : Integer; AValue : integer); 

begin
  If (FandroidNetworkSubtype=AValue) then exit;
  FandroidNetworkSubtype:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveDiagnostics.SetandroidNetworkType(AIndex : Integer; AValue : integer); 

begin
  If (FandroidNetworkType=AValue) then exit;
  FandroidNetworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveDiagnostics.SetiosNetworkType(AIndex : Integer; AValue : integer); 

begin
  If (FiosNetworkType=AValue) then exit;
  FiosNetworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveDiagnostics.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveDiagnostics.SetnetworkOperatorCode(AIndex : Integer; AValue : String); 

begin
  If (FnetworkOperatorCode=AValue) then exit;
  FnetworkOperatorCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveDiagnostics.SetnetworkOperatorName(AIndex : Integer; AValue : String); 

begin
  If (FnetworkOperatorName=AValue) then exit;
  FnetworkOperatorName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveDiagnostics.SetpeerSession(AIndex : Integer; AValue : TRoomLeaveDiagnosticsTypepeerSessionArray); 

begin
  If (FpeerSession=AValue) then exit;
  FpeerSession:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveDiagnostics.SetsocketsUsed(AIndex : Integer; AValue : boolean); 

begin
  If (FsocketsUsed=AValue) then exit;
  FsocketsUsed:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomLeaveRequest
  --------------------------------------------------------------------}


Procedure TRoomLeaveRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveRequest.SetleaveDiagnostics(AIndex : Integer; AValue : TRoomLeaveDiagnostics); 

begin
  If (FleaveDiagnostics=AValue) then exit;
  FleaveDiagnostics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomLeaveRequest.Setreason(AIndex : Integer; AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomList
  --------------------------------------------------------------------}


Procedure TRoomList.Setitems(AIndex : Integer; AValue : TRoomListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomModification
  --------------------------------------------------------------------}


Procedure TRoomModification.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomModification.SetmodifiedTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FmodifiedTimestampMillis=AValue) then exit;
  FmodifiedTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomModification.SetparticipantId(AIndex : Integer; AValue : String); 

begin
  If (FparticipantId=AValue) then exit;
  FparticipantId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomP2PStatus
  --------------------------------------------------------------------}


Procedure TRoomP2PStatus.SetconnectionSetupLatencyMillis(AIndex : Integer; AValue : integer); 

begin
  If (FconnectionSetupLatencyMillis=AValue) then exit;
  FconnectionSetupLatencyMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomP2PStatus.Seterror(AIndex : Integer; AValue : String); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomP2PStatus.Seterror_reason(AIndex : Integer; AValue : String); 

begin
  If (Ferror_reason=AValue) then exit;
  Ferror_reason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomP2PStatus.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomP2PStatus.SetparticipantId(AIndex : Integer; AValue : String); 

begin
  If (FparticipantId=AValue) then exit;
  FparticipantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomP2PStatus.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomP2PStatus.SetunreliableRoundtripLatencyMillis(AIndex : Integer; AValue : integer); 

begin
  If (FunreliableRoundtripLatencyMillis=AValue) then exit;
  FunreliableRoundtripLatencyMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomP2PStatuses
  --------------------------------------------------------------------}


Procedure TRoomP2PStatuses.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomP2PStatuses.Setupdates(AIndex : Integer; AValue : TRoomP2PStatusesTypeupdatesArray); 

begin
  If (Fupdates=AValue) then exit;
  Fupdates:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomParticipant
  --------------------------------------------------------------------}


Procedure TRoomParticipant.SetautoMatched(AIndex : Integer; AValue : boolean); 

begin
  If (FautoMatched=AValue) then exit;
  FautoMatched:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.SetautoMatchedPlayer(AIndex : Integer; AValue : TAnonymousPlayer); 

begin
  If (FautoMatchedPlayer=AValue) then exit;
  FautoMatchedPlayer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.Setcapabilities(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fcapabilities=AValue) then exit;
  Fcapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.SetclientAddress(AIndex : Integer; AValue : TRoomClientAddress); 

begin
  If (FclientAddress=AValue) then exit;
  FclientAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.Setconnected(AIndex : Integer; AValue : boolean); 

begin
  If (Fconnected=AValue) then exit;
  Fconnected:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.SetleaveReason(AIndex : Integer; AValue : String); 

begin
  If (FleaveReason=AValue) then exit;
  FleaveReason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.Setplayer(AIndex : Integer; AValue : TPlayer); 

begin
  If (Fplayer=AValue) then exit;
  Fplayer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomParticipant.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRoomStatus
  --------------------------------------------------------------------}


Procedure TRoomStatus.SetautoMatchingStatus(AIndex : Integer; AValue : TRoomAutoMatchStatus); 

begin
  If (FautoMatchingStatus=AValue) then exit;
  FautoMatchingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomStatus.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomStatus.Setparticipants(AIndex : Integer; AValue : TRoomStatusTypeparticipantsArray); 

begin
  If (Fparticipants=AValue) then exit;
  Fparticipants:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomStatus.SetroomId(AIndex : Integer; AValue : String); 

begin
  If (FroomId=AValue) then exit;
  FroomId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomStatus.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRoomStatus.SetstatusVersion(AIndex : Integer; AValue : integer); 

begin
  If (FstatusVersion=AValue) then exit;
  FstatusVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TScoreSubmission
  --------------------------------------------------------------------}


Procedure TScoreSubmission.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScoreSubmission.SetleaderboardId(AIndex : Integer; AValue : String); 

begin
  If (FleaderboardId=AValue) then exit;
  FleaderboardId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScoreSubmission.Setscore(AIndex : Integer; AValue : String); 

begin
  If (Fscore=AValue) then exit;
  Fscore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScoreSubmission.SetscoreTag(AIndex : Integer; AValue : String); 

begin
  If (FscoreTag=AValue) then exit;
  FscoreTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScoreSubmission.Setsignature(AIndex : Integer; AValue : String); 

begin
  If (Fsignature=AValue) then exit;
  Fsignature:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSnapshot
  --------------------------------------------------------------------}


Procedure TSnapshot.SetcoverImage(AIndex : Integer; AValue : TSnapshotImage); 

begin
  If (FcoverImage=AValue) then exit;
  FcoverImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetdriveId(AIndex : Integer; AValue : String); 

begin
  If (FdriveId=AValue) then exit;
  FdriveId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetdurationMillis(AIndex : Integer; AValue : String); 

begin
  If (FdurationMillis=AValue) then exit;
  FdurationMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetlastModifiedMillis(AIndex : Integer; AValue : String); 

begin
  If (FlastModifiedMillis=AValue) then exit;
  FlastModifiedMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetprogressValue(AIndex : Integer; AValue : String); 

begin
  If (FprogressValue=AValue) then exit;
  FprogressValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshot.SetuniqueName(AIndex : Integer; AValue : String); 

begin
  If (FuniqueName=AValue) then exit;
  FuniqueName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSnapshot.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSnapshotImage
  --------------------------------------------------------------------}


Procedure TSnapshotImage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotImage.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotImage.Setmime_type(AIndex : Integer; AValue : String); 

begin
  If (Fmime_type=AValue) then exit;
  Fmime_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotImage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotImage.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSnapshotListResponse
  --------------------------------------------------------------------}


Procedure TSnapshotListResponse.Setitems(AIndex : Integer; AValue : TSnapshotListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSnapshotListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedAutoMatchingCriteria
  --------------------------------------------------------------------}


Procedure TTurnBasedAutoMatchingCriteria.SetexclusiveBitmask(AIndex : Integer; AValue : String); 

begin
  If (FexclusiveBitmask=AValue) then exit;
  FexclusiveBitmask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedAutoMatchingCriteria.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedAutoMatchingCriteria.SetmaxAutoMatchingPlayers(AIndex : Integer; AValue : integer); 

begin
  If (FmaxAutoMatchingPlayers=AValue) then exit;
  FmaxAutoMatchingPlayers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedAutoMatchingCriteria.SetminAutoMatchingPlayers(AIndex : Integer; AValue : integer); 

begin
  If (FminAutoMatchingPlayers=AValue) then exit;
  FminAutoMatchingPlayers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatch
  --------------------------------------------------------------------}


Procedure TTurnBasedMatch.SetapplicationId(AIndex : Integer; AValue : String); 

begin
  If (FapplicationId=AValue) then exit;
  FapplicationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetautoMatchingCriteria(AIndex : Integer; AValue : TTurnBasedAutoMatchingCriteria); 

begin
  If (FautoMatchingCriteria=AValue) then exit;
  FautoMatchingCriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetcreationDetails(AIndex : Integer; AValue : TTurnBasedMatchModification); 

begin
  If (FcreationDetails=AValue) then exit;
  FcreationDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.Setdata(AIndex : Integer; AValue : TTurnBasedMatchData); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetinviterId(AIndex : Integer; AValue : String); 

begin
  If (FinviterId=AValue) then exit;
  FinviterId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetlastUpdateDetails(AIndex : Integer; AValue : TTurnBasedMatchModification); 

begin
  If (FlastUpdateDetails=AValue) then exit;
  FlastUpdateDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetmatchId(AIndex : Integer; AValue : String); 

begin
  If (FmatchId=AValue) then exit;
  FmatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetmatchNumber(AIndex : Integer; AValue : integer); 

begin
  If (FmatchNumber=AValue) then exit;
  FmatchNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetmatchVersion(AIndex : Integer; AValue : integer); 

begin
  If (FmatchVersion=AValue) then exit;
  FmatchVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.Setparticipants(AIndex : Integer; AValue : TTurnBasedMatchTypeparticipantsArray); 

begin
  If (Fparticipants=AValue) then exit;
  Fparticipants:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetpendingParticipantId(AIndex : Integer; AValue : String); 

begin
  If (FpendingParticipantId=AValue) then exit;
  FpendingParticipantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetpreviousMatchData(AIndex : Integer; AValue : TTurnBasedMatchData); 

begin
  If (FpreviousMatchData=AValue) then exit;
  FpreviousMatchData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetrematchId(AIndex : Integer; AValue : String); 

begin
  If (FrematchId=AValue) then exit;
  FrematchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.Setresults(AIndex : Integer; AValue : TTurnBasedMatchTyperesultsArray); 

begin
  If (Fresults=AValue) then exit;
  Fresults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetuserMatchStatus(AIndex : Integer; AValue : String); 

begin
  If (FuserMatchStatus=AValue) then exit;
  FuserMatchStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.Setvariant(AIndex : Integer; AValue : integer); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatch.SetwithParticipantId(AIndex : Integer; AValue : String); 

begin
  If (FwithParticipantId=AValue) then exit;
  FwithParticipantId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchCreateRequest
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchCreateRequest.SetautoMatchingCriteria(AIndex : Integer; AValue : TTurnBasedAutoMatchingCriteria); 

begin
  If (FautoMatchingCriteria=AValue) then exit;
  FautoMatchingCriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchCreateRequest.SetinvitedPlayerIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FinvitedPlayerIds=AValue) then exit;
  FinvitedPlayerIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchCreateRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchCreateRequest.SetrequestId(AIndex : Integer; AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchCreateRequest.Setvariant(AIndex : Integer; AValue : integer); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchData
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchData.Setdata(AIndex : Integer; AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchData.SetdataAvailable(AIndex : Integer; AValue : boolean); 

begin
  If (FdataAvailable=AValue) then exit;
  FdataAvailable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchDataRequest
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchDataRequest.Setdata(AIndex : Integer; AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchDataRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchList
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchList.Setitems(AIndex : Integer; AValue : TTurnBasedMatchListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchModification
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchModification.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchModification.SetmodifiedTimestampMillis(AIndex : Integer; AValue : String); 

begin
  If (FmodifiedTimestampMillis=AValue) then exit;
  FmodifiedTimestampMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchModification.SetparticipantId(AIndex : Integer; AValue : String); 

begin
  If (FparticipantId=AValue) then exit;
  FparticipantId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchParticipant
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchParticipant.SetautoMatched(AIndex : Integer; AValue : boolean); 

begin
  If (FautoMatched=AValue) then exit;
  FautoMatched:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchParticipant.SetautoMatchedPlayer(AIndex : Integer; AValue : TAnonymousPlayer); 

begin
  If (FautoMatchedPlayer=AValue) then exit;
  FautoMatchedPlayer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchParticipant.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchParticipant.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchParticipant.Setplayer(AIndex : Integer; AValue : TPlayer); 

begin
  If (Fplayer=AValue) then exit;
  Fplayer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchParticipant.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchRematch
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchRematch.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchRematch.SetpreviousMatch(AIndex : Integer; AValue : TTurnBasedMatch); 

begin
  If (FpreviousMatch=AValue) then exit;
  FpreviousMatch:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchRematch.Setrematch(AIndex : Integer; AValue : TTurnBasedMatch); 

begin
  If (Frematch=AValue) then exit;
  Frematch:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchResults
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchResults.Setdata(AIndex : Integer; AValue : TTurnBasedMatchDataRequest); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchResults.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchResults.SetmatchVersion(AIndex : Integer; AValue : integer); 

begin
  If (FmatchVersion=AValue) then exit;
  FmatchVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchResults.Setresults(AIndex : Integer; AValue : TTurnBasedMatchResultsTyperesultsArray); 

begin
  If (Fresults=AValue) then exit;
  Fresults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchSync
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchSync.Setitems(AIndex : Integer; AValue : TTurnBasedMatchSyncTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchSync.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchSync.SetmoreAvailable(AIndex : Integer; AValue : boolean); 

begin
  If (FmoreAvailable=AValue) then exit;
  FmoreAvailable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchSync.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTurnBasedMatchTurn
  --------------------------------------------------------------------}


Procedure TTurnBasedMatchTurn.Setdata(AIndex : Integer; AValue : TTurnBasedMatchDataRequest); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchTurn.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchTurn.SetmatchVersion(AIndex : Integer; AValue : integer); 

begin
  If (FmatchVersion=AValue) then exit;
  FmatchVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchTurn.SetpendingParticipantId(AIndex : Integer; AValue : String); 

begin
  If (FpendingParticipantId=AValue) then exit;
  FpendingParticipantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTurnBasedMatchTurn.Setresults(AIndex : Integer; AValue : TTurnBasedMatchTurnTyperesultsArray); 

begin
  If (Fresults=AValue) then exit;
  Fresults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementDefinitionsResource
  --------------------------------------------------------------------}


Class Function TAchievementDefinitionsResource.ResourceName : String;

begin
  Result:='achievementDefinitions';
end;

Class Function TAchievementDefinitionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesAPI;
end;

Function TAchievementDefinitionsResource.List(AQuery : string = '') : TAchievementDefinitionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'achievements';
  _Methodid   = 'games.achievementDefinitions.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAchievementDefinitionsListResponse) as TAchievementDefinitionsListResponse;
end;


Function TAchievementDefinitionsResource.List(AQuery : TAchievementDefinitionslistOptions) : TAchievementDefinitionsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
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
  Result:=TgamesAPI;
end;

Function TAchievementsResource.Increment(achievementId: string; AQuery : string = '') : TAchievementIncrementResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/{achievementId}/increment';
  _Methodid   = 'games.achievements.increment';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAchievementIncrementResponse) as TAchievementIncrementResponse;
end;


Function TAchievementsResource.Increment(achievementId: string; AQuery : TAchievementsincrementOptions) : TAchievementIncrementResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestId',AQuery.requestId);
  AddToQuery(_Q,'stepsToIncrement',AQuery.stepsToIncrement);
  Result:=Increment(achievementId,_Q);
end;

Function TAchievementsResource.List(playerId: string; AQuery : string = '') : TPlayerAchievementListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'players/{playerId}/achievements';
  _Methodid   = 'games.achievements.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['playerId',playerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlayerAchievementListResponse) as TPlayerAchievementListResponse;
end;


Function TAchievementsResource.List(playerId: string; AQuery : TAchievementslistOptions) : TPlayerAchievementListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'state',AQuery.state);
  Result:=List(playerId,_Q);
end;

Function TAchievementsResource.Reveal(achievementId: string) : TAchievementRevealResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/{achievementId}/reveal';
  _Methodid   = 'games.achievements.reveal';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAchievementRevealResponse) as TAchievementRevealResponse;
end;

Function TAchievementsResource.SetStepsAtLeast(achievementId: string; AQuery : string = '') : TAchievementSetStepsAtLeastResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/{achievementId}/setStepsAtLeast';
  _Methodid   = 'games.achievements.setStepsAtLeast';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAchievementSetStepsAtLeastResponse) as TAchievementSetStepsAtLeastResponse;
end;


Function TAchievementsResource.SetStepsAtLeast(achievementId: string; AQuery : TAchievementssetStepsAtLeastOptions) : TAchievementSetStepsAtLeastResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'steps',AQuery.steps);
  Result:=SetStepsAtLeast(achievementId,_Q);
end;

Function TAchievementsResource.Unlock(achievementId: string) : TAchievementUnlockResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/{achievementId}/unlock';
  _Methodid   = 'games.achievements.unlock';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAchievementUnlockResponse) as TAchievementUnlockResponse;
end;

Function TAchievementsResource.UpdateMultiple(aAchievementUpdateMultipleRequest : TAchievementUpdateMultipleRequest) : TAchievementUpdateMultipleResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'achievements/updateMultiple';
  _Methodid   = 'games.achievements.updateMultiple';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAchievementUpdateMultipleRequest,TAchievementUpdateMultipleResponse) as TAchievementUpdateMultipleResponse;
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
  Result:=TgamesAPI;
end;

Function TApplicationsResource.Get(applicationId: string; AQuery : string = '') : TApplication;

Const
  _HTTPMethod = 'GET';
  _Path       = 'applications/{applicationId}';
  _Methodid   = 'games.applications.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TApplication) as TApplication;
end;


Function TApplicationsResource.Get(applicationId: string; AQuery : TApplicationsgetOptions) : TApplication;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'platformType',AQuery.platformType);
  Result:=Get(applicationId,_Q);
end;

Procedure TApplicationsResource.Played;

Const
  _HTTPMethod = 'POST';
  _Path       = 'applications/played';
  _Methodid   = 'games.applications.played';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
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
  Result:=TgamesAPI;
end;

Function TEventsResource.ListByPlayer(AQuery : string = '') : TPlayerEventListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'events';
  _Methodid   = 'games.events.listByPlayer';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TPlayerEventListResponse) as TPlayerEventListResponse;
end;


Function TEventsResource.ListByPlayer(AQuery : TEventslistByPlayerOptions) : TPlayerEventListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListByPlayer(_Q);
end;

Function TEventsResource.ListDefinitions(AQuery : string = '') : TEventDefinitionListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'eventDefinitions';
  _Methodid   = 'games.events.listDefinitions';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TEventDefinitionListResponse) as TEventDefinitionListResponse;
end;


Function TEventsResource.ListDefinitions(AQuery : TEventslistDefinitionsOptions) : TEventDefinitionListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListDefinitions(_Q);
end;

Function TEventsResource._record(aEventRecordRequest : TEventRecordRequest; AQuery : string = '') : TEventUpdateResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'events';
  _Methodid   = 'games.events.record';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aEventRecordRequest,TEventUpdateResponse) as TEventUpdateResponse;
end;


Function TEventsResource._record(aEventRecordRequest : TEventRecordRequest; AQuery : TEventsrecordOptions) : TEventUpdateResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=_record(aEventRecordRequest,_Q);
end;



{ --------------------------------------------------------------------
  TLeaderboardsResource
  --------------------------------------------------------------------}


Class Function TLeaderboardsResource.ResourceName : String;

begin
  Result:='leaderboards';
end;

Class Function TLeaderboardsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesAPI;
end;

Function TLeaderboardsResource.Get(leaderboardId: string; AQuery : string = '') : TLeaderboard;

Const
  _HTTPMethod = 'GET';
  _Path       = 'leaderboards/{leaderboardId}';
  _Methodid   = 'games.leaderboards.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLeaderboard) as TLeaderboard;
end;


Function TLeaderboardsResource.Get(leaderboardId: string; AQuery : TLeaderboardsgetOptions) : TLeaderboard;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Get(leaderboardId,_Q);
end;

Function TLeaderboardsResource.List(AQuery : string = '') : TLeaderboardListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'leaderboards';
  _Methodid   = 'games.leaderboards.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TLeaderboardListResponse) as TLeaderboardListResponse;
end;


Function TLeaderboardsResource.List(AQuery : TLeaderboardslistOptions) : TLeaderboardListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TMetagameResource
  --------------------------------------------------------------------}


Class Function TMetagameResource.ResourceName : String;

begin
  Result:='metagame';
end;

Class Function TMetagameResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesAPI;
end;

Function TMetagameResource.GetMetagameConfig : TMetagameConfig;

Const
  _HTTPMethod = 'GET';
  _Path       = 'metagameConfig';
  _Methodid   = 'games.metagame.getMetagameConfig';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TMetagameConfig) as TMetagameConfig;
end;

Function TMetagameResource.ListCategoriesByPlayer(collection: string; playerId: string; AQuery : string = '') : TCategoryListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'players/{playerId}/categories/{collection}';
  _Methodid   = 'games.metagame.listCategoriesByPlayer';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection,'playerId',playerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCategoryListResponse) as TCategoryListResponse;
end;


Function TMetagameResource.ListCategoriesByPlayer(collection: string; playerId: string; AQuery : TMetagamelistCategoriesByPlayerOptions) : TCategoryListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListCategoriesByPlayer(collection,playerId,_Q);
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
  Result:=TgamesAPI;
end;

Function TPlayersResource.Get(playerId: string; AQuery : string = '') : TPlayer;

Const
  _HTTPMethod = 'GET';
  _Path       = 'players/{playerId}';
  _Methodid   = 'games.players.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['playerId',playerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlayer) as TPlayer;
end;


Function TPlayersResource.Get(playerId: string; AQuery : TPlayersgetOptions) : TPlayer;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Get(playerId,_Q);
end;

Function TPlayersResource.List(collection: string; AQuery : string = '') : TPlayerListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'players/me/players/{collection}';
  _Methodid   = 'games.players.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlayerListResponse) as TPlayerListResponse;
end;


Function TPlayersResource.List(collection: string; AQuery : TPlayerslistOptions) : TPlayerListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(collection,_Q);
end;



{ --------------------------------------------------------------------
  TPushtokensResource
  --------------------------------------------------------------------}


Class Function TPushtokensResource.ResourceName : String;

begin
  Result:='pushtokens';
end;

Class Function TPushtokensResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesAPI;
end;

Procedure TPushtokensResource.Remove(aPushTokenId : TPushTokenId);

Const
  _HTTPMethod = 'POST';
  _Path       = 'pushtokens/remove';
  _Methodid   = 'games.pushtokens.remove';

begin
  ServiceCall(_HTTPMethod,_Path,'',aPushTokenId,Nil);
end;

Procedure TPushtokensResource.Update(aPushToken : TPushToken);

Const
  _HTTPMethod = 'PUT';
  _Path       = 'pushtokens';
  _Methodid   = 'games.pushtokens.update';

begin
  ServiceCall(_HTTPMethod,_Path,'',aPushToken,Nil);
end;



{ --------------------------------------------------------------------
  TQuestMilestonesResource
  --------------------------------------------------------------------}


Class Function TQuestMilestonesResource.ResourceName : String;

begin
  Result:='questMilestones';
end;

Class Function TQuestMilestonesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesAPI;
end;

Procedure TQuestMilestonesResource.Claim(milestoneId: string; questId: string; AQuery : string = '');

Const
  _HTTPMethod = 'PUT';
  _Path       = 'quests/{questId}/milestones/{milestoneId}/claim';
  _Methodid   = 'games.questMilestones.claim';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['milestoneId',milestoneId,'questId',questId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TQuestMilestonesResource.Claim(milestoneId: string; questId: string; AQuery : TQuestMilestonesclaimOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestId',AQuery.requestId);
  Claim(milestoneId,questId,_Q);
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
  Result:=TgamesAPI;
end;

Function TQuestsResource.Accept(questId: string; AQuery : string = '') : TQuest;

Const
  _HTTPMethod = 'POST';
  _Path       = 'quests/{questId}/accept';
  _Methodid   = 'games.quests.accept';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['questId',questId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TQuest) as TQuest;
end;


Function TQuestsResource.Accept(questId: string; AQuery : TQuestsacceptOptions) : TQuest;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Accept(questId,_Q);
end;

Function TQuestsResource.List(playerId: string; AQuery : string = '') : TQuestListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'players/{playerId}/quests';
  _Methodid   = 'games.quests.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['playerId',playerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TQuestListResponse) as TQuestListResponse;
end;


Function TQuestsResource.List(playerId: string; AQuery : TQuestslistOptions) : TQuestListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(playerId,_Q);
end;



{ --------------------------------------------------------------------
  TRevisionsResource
  --------------------------------------------------------------------}


Class Function TRevisionsResource.ResourceName : String;

begin
  Result:='revisions';
end;

Class Function TRevisionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesAPI;
end;

Function TRevisionsResource.Check(AQuery : string = '') : TRevisionCheckResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'revisions/check';
  _Methodid   = 'games.revisions.check';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TRevisionCheckResponse) as TRevisionCheckResponse;
end;


Function TRevisionsResource.Check(AQuery : TRevisionscheckOptions) : TRevisionCheckResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'clientRevision',AQuery.clientRevision);
  Result:=Check(_Q);
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
  Result:=TgamesAPI;
end;

Function TRoomsResource.Create(aRoomCreateRequest : TRoomCreateRequest; AQuery : string = '') : TRoom;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rooms/create';
  _Methodid   = 'games.rooms.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aRoomCreateRequest,TRoom) as TRoom;
end;


Function TRoomsResource.Create(aRoomCreateRequest : TRoomCreateRequest; AQuery : TRoomscreateOptions) : TRoom;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Create(aRoomCreateRequest,_Q);
end;

Function TRoomsResource.Decline(roomId: string; AQuery : string = '') : TRoom;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rooms/{roomId}/decline';
  _Methodid   = 'games.rooms.decline';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['roomId',roomId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRoom) as TRoom;
end;


Function TRoomsResource.Decline(roomId: string; AQuery : TRoomsdeclineOptions) : TRoom;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Decline(roomId,_Q);
end;

Procedure TRoomsResource.Dismiss(roomId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'rooms/{roomId}/dismiss';
  _Methodid   = 'games.rooms.dismiss';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['roomId',roomId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TRoomsResource.Get(roomId: string; AQuery : string = '') : TRoom;

Const
  _HTTPMethod = 'GET';
  _Path       = 'rooms/{roomId}';
  _Methodid   = 'games.rooms.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['roomId',roomId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRoom) as TRoom;
end;


Function TRoomsResource.Get(roomId: string; AQuery : TRoomsgetOptions) : TRoom;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Get(roomId,_Q);
end;

Function TRoomsResource.Join(roomId: string; aRoomJoinRequest : TRoomJoinRequest; AQuery : string = '') : TRoom;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rooms/{roomId}/join';
  _Methodid   = 'games.rooms.join';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['roomId',roomId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aRoomJoinRequest,TRoom) as TRoom;
end;


Function TRoomsResource.Join(roomId: string; aRoomJoinRequest : TRoomJoinRequest; AQuery : TRoomsjoinOptions) : TRoom;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Join(roomId,aRoomJoinRequest,_Q);
end;

Function TRoomsResource.Leave(roomId: string; aRoomLeaveRequest : TRoomLeaveRequest; AQuery : string = '') : TRoom;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rooms/{roomId}/leave';
  _Methodid   = 'games.rooms.leave';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['roomId',roomId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aRoomLeaveRequest,TRoom) as TRoom;
end;


Function TRoomsResource.Leave(roomId: string; aRoomLeaveRequest : TRoomLeaveRequest; AQuery : TRoomsleaveOptions) : TRoom;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Leave(roomId,aRoomLeaveRequest,_Q);
end;

Function TRoomsResource.List(AQuery : string = '') : TRoomList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'rooms';
  _Methodid   = 'games.rooms.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TRoomList) as TRoomList;
end;


Function TRoomsResource.List(AQuery : TRoomslistOptions) : TRoomList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;

Function TRoomsResource.ReportStatus(roomId: string; aRoomP2PStatuses : TRoomP2PStatuses; AQuery : string = '') : TRoomStatus;

Const
  _HTTPMethod = 'POST';
  _Path       = 'rooms/{roomId}/reportstatus';
  _Methodid   = 'games.rooms.reportStatus';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['roomId',roomId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aRoomP2PStatuses,TRoomStatus) as TRoomStatus;
end;


Function TRoomsResource.ReportStatus(roomId: string; aRoomP2PStatuses : TRoomP2PStatuses; AQuery : TRoomsreportStatusOptions) : TRoomStatus;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=ReportStatus(roomId,aRoomP2PStatuses,_Q);
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
  Result:=TgamesAPI;
end;

Function TScoresResource.Get(leaderboardId: string; playerId: string; timeSpan: string; AQuery : string = '') : TPlayerLeaderboardScoreListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'players/{playerId}/leaderboards/{leaderboardId}/scores/{timeSpan}';
  _Methodid   = 'games.scores.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId,'playerId',playerId,'timeSpan',timeSpan]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlayerLeaderboardScoreListResponse) as TPlayerLeaderboardScoreListResponse;
end;


Function TScoresResource.Get(leaderboardId: string; playerId: string; timeSpan: string; AQuery : TScoresgetOptions) : TPlayerLeaderboardScoreListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeRankType',AQuery.includeRankType);
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=Get(leaderboardId,playerId,timeSpan,_Q);
end;

Function TScoresResource.List(collection: string; leaderboardId: string; AQuery : string = '') : TLeaderboardScores;

Const
  _HTTPMethod = 'GET';
  _Path       = 'leaderboards/{leaderboardId}/scores/{collection}';
  _Methodid   = 'games.scores.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection,'leaderboardId',leaderboardId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLeaderboardScores) as TLeaderboardScores;
end;


Function TScoresResource.List(collection: string; leaderboardId: string; AQuery : TScoreslistOptions) : TLeaderboardScores;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'timeSpan',AQuery.timeSpan);
  Result:=List(collection,leaderboardId,_Q);
end;

Function TScoresResource.ListWindow(collection: string; leaderboardId: string; AQuery : string = '') : TLeaderboardScores;

Const
  _HTTPMethod = 'GET';
  _Path       = 'leaderboards/{leaderboardId}/window/{collection}';
  _Methodid   = 'games.scores.listWindow';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection,'leaderboardId',leaderboardId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLeaderboardScores) as TLeaderboardScores;
end;


Function TScoresResource.ListWindow(collection: string; leaderboardId: string; AQuery : TScoreslistWindowOptions) : TLeaderboardScores;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'resultsAbove',AQuery.resultsAbove);
  AddToQuery(_Q,'returnTopIfAbsent',AQuery.returnTopIfAbsent);
  AddToQuery(_Q,'timeSpan',AQuery.timeSpan);
  Result:=ListWindow(collection,leaderboardId,_Q);
end;

Function TScoresResource.Submit(leaderboardId: string; AQuery : string = '') : TPlayerScoreResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'leaderboards/{leaderboardId}/scores';
  _Methodid   = 'games.scores.submit';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPlayerScoreResponse) as TPlayerScoreResponse;
end;


Function TScoresResource.Submit(leaderboardId: string; AQuery : TScoressubmitOptions) : TPlayerScoreResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'score',AQuery.score);
  AddToQuery(_Q,'scoreTag',AQuery.scoreTag);
  Result:=Submit(leaderboardId,_Q);
end;

Function TScoresResource.SubmitMultiple(aPlayerScoreSubmissionList : TPlayerScoreSubmissionList; AQuery : string = '') : TPlayerScoreListResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'leaderboards/scores';
  _Methodid   = 'games.scores.submitMultiple';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aPlayerScoreSubmissionList,TPlayerScoreListResponse) as TPlayerScoreListResponse;
end;


Function TScoresResource.SubmitMultiple(aPlayerScoreSubmissionList : TPlayerScoreSubmissionList; AQuery : TScoressubmitMultipleOptions) : TPlayerScoreListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=SubmitMultiple(aPlayerScoreSubmissionList,_Q);
end;



{ --------------------------------------------------------------------
  TSnapshotsResource
  --------------------------------------------------------------------}


Class Function TSnapshotsResource.ResourceName : String;

begin
  Result:='snapshots';
end;

Class Function TSnapshotsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesAPI;
end;

Function TSnapshotsResource.Get(snapshotId: string; AQuery : string = '') : TSnapshot;

Const
  _HTTPMethod = 'GET';
  _Path       = 'snapshots/{snapshotId}';
  _Methodid   = 'games.snapshots.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['snapshotId',snapshotId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSnapshot) as TSnapshot;
end;


Function TSnapshotsResource.Get(snapshotId: string; AQuery : TSnapshotsgetOptions) : TSnapshot;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Get(snapshotId,_Q);
end;

Function TSnapshotsResource.List(playerId: string; AQuery : string = '') : TSnapshotListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'players/{playerId}/snapshots';
  _Methodid   = 'games.snapshots.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['playerId',playerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSnapshotListResponse) as TSnapshotListResponse;
end;


Function TSnapshotsResource.List(playerId: string; AQuery : TSnapshotslistOptions) : TSnapshotListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(playerId,_Q);
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
  Result:=TgamesAPI;
end;

Procedure TTurnBasedMatchesResource.Cancel(matchId: string);

Const
  _HTTPMethod = 'PUT';
  _Path       = 'turnbasedmatches/{matchId}/cancel';
  _Methodid   = 'games.turnBasedMatches.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTurnBasedMatchesResource.Create(aTurnBasedMatchCreateRequest : TTurnBasedMatchCreateRequest; AQuery : string = '') : TTurnBasedMatch;

Const
  _HTTPMethod = 'POST';
  _Path       = 'turnbasedmatches/create';
  _Methodid   = 'games.turnBasedMatches.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aTurnBasedMatchCreateRequest,TTurnBasedMatch) as TTurnBasedMatch;
end;


Function TTurnBasedMatchesResource.Create(aTurnBasedMatchCreateRequest : TTurnBasedMatchCreateRequest; AQuery : TTurnBasedMatchescreateOptions) : TTurnBasedMatch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Create(aTurnBasedMatchCreateRequest,_Q);
end;

Function TTurnBasedMatchesResource.Decline(matchId: string; AQuery : string = '') : TTurnBasedMatch;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'turnbasedmatches/{matchId}/decline';
  _Methodid   = 'games.turnBasedMatches.decline';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTurnBasedMatch) as TTurnBasedMatch;
end;


Function TTurnBasedMatchesResource.Decline(matchId: string; AQuery : TTurnBasedMatchesdeclineOptions) : TTurnBasedMatch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Decline(matchId,_Q);
end;

Procedure TTurnBasedMatchesResource.Dismiss(matchId: string);

Const
  _HTTPMethod = 'PUT';
  _Path       = 'turnbasedmatches/{matchId}/dismiss';
  _Methodid   = 'games.turnBasedMatches.dismiss';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTurnBasedMatchesResource.Finish(matchId: string; aTurnBasedMatchResults : TTurnBasedMatchResults; AQuery : string = '') : TTurnBasedMatch;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'turnbasedmatches/{matchId}/finish';
  _Methodid   = 'games.turnBasedMatches.finish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTurnBasedMatchResults,TTurnBasedMatch) as TTurnBasedMatch;
end;


Function TTurnBasedMatchesResource.Finish(matchId: string; aTurnBasedMatchResults : TTurnBasedMatchResults; AQuery : TTurnBasedMatchesfinishOptions) : TTurnBasedMatch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Finish(matchId,aTurnBasedMatchResults,_Q);
end;

Function TTurnBasedMatchesResource.Get(matchId: string; AQuery : string = '') : TTurnBasedMatch;

Const
  _HTTPMethod = 'GET';
  _Path       = 'turnbasedmatches/{matchId}';
  _Methodid   = 'games.turnBasedMatches.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTurnBasedMatch) as TTurnBasedMatch;
end;


Function TTurnBasedMatchesResource.Get(matchId: string; AQuery : TTurnBasedMatchesgetOptions) : TTurnBasedMatch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeMatchData',AQuery.includeMatchData);
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Get(matchId,_Q);
end;

Function TTurnBasedMatchesResource.Join(matchId: string; AQuery : string = '') : TTurnBasedMatch;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'turnbasedmatches/{matchId}/join';
  _Methodid   = 'games.turnBasedMatches.join';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTurnBasedMatch) as TTurnBasedMatch;
end;


Function TTurnBasedMatchesResource.Join(matchId: string; AQuery : TTurnBasedMatchesjoinOptions) : TTurnBasedMatch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Join(matchId,_Q);
end;

Function TTurnBasedMatchesResource.Leave(matchId: string; AQuery : string = '') : TTurnBasedMatch;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'turnbasedmatches/{matchId}/leave';
  _Methodid   = 'games.turnBasedMatches.leave';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTurnBasedMatch) as TTurnBasedMatch;
end;


Function TTurnBasedMatchesResource.Leave(matchId: string; AQuery : TTurnBasedMatchesleaveOptions) : TTurnBasedMatch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Leave(matchId,_Q);
end;

Function TTurnBasedMatchesResource.LeaveTurn(matchId: string; AQuery : string = '') : TTurnBasedMatch;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'turnbasedmatches/{matchId}/leaveTurn';
  _Methodid   = 'games.turnBasedMatches.leaveTurn';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTurnBasedMatch) as TTurnBasedMatch;
end;


Function TTurnBasedMatchesResource.LeaveTurn(matchId: string; AQuery : TTurnBasedMatchesleaveTurnOptions) : TTurnBasedMatch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'matchVersion',AQuery.matchVersion);
  AddToQuery(_Q,'pendingParticipantId',AQuery.pendingParticipantId);
  Result:=LeaveTurn(matchId,_Q);
end;

Function TTurnBasedMatchesResource.List(AQuery : string = '') : TTurnBasedMatchList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'turnbasedmatches';
  _Methodid   = 'games.turnBasedMatches.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTurnBasedMatchList) as TTurnBasedMatchList;
end;


Function TTurnBasedMatchesResource.List(AQuery : TTurnBasedMatcheslistOptions) : TTurnBasedMatchList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeMatchData',AQuery.includeMatchData);
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxCompletedMatches',AQuery.maxCompletedMatches);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;

Function TTurnBasedMatchesResource.Rematch(matchId: string; AQuery : string = '') : TTurnBasedMatchRematch;

Const
  _HTTPMethod = 'POST';
  _Path       = 'turnbasedmatches/{matchId}/rematch';
  _Methodid   = 'games.turnBasedMatches.rematch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTurnBasedMatchRematch) as TTurnBasedMatchRematch;
end;


Function TTurnBasedMatchesResource.Rematch(matchId: string; AQuery : TTurnBasedMatchesrematchOptions) : TTurnBasedMatchRematch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'requestId',AQuery.requestId);
  Result:=Rematch(matchId,_Q);
end;

Function TTurnBasedMatchesResource.Sync(AQuery : string = '') : TTurnBasedMatchSync;

Const
  _HTTPMethod = 'GET';
  _Path       = 'turnbasedmatches/sync';
  _Methodid   = 'games.turnBasedMatches.sync';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTurnBasedMatchSync) as TTurnBasedMatchSync;
end;


Function TTurnBasedMatchesResource.Sync(AQuery : TTurnBasedMatchessyncOptions) : TTurnBasedMatchSync;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeMatchData',AQuery.includeMatchData);
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxCompletedMatches',AQuery.maxCompletedMatches);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=Sync(_Q);
end;

Function TTurnBasedMatchesResource.TakeTurn(matchId: string; aTurnBasedMatchTurn : TTurnBasedMatchTurn; AQuery : string = '') : TTurnBasedMatch;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'turnbasedmatches/{matchId}/turn';
  _Methodid   = 'games.turnBasedMatches.takeTurn';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['matchId',matchId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTurnBasedMatchTurn,TTurnBasedMatch) as TTurnBasedMatch;
end;


Function TTurnBasedMatchesResource.TakeTurn(matchId: string; aTurnBasedMatchTurn : TTurnBasedMatchTurn; AQuery : TTurnBasedMatchestakeTurnOptions) : TTurnBasedMatch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=TakeTurn(matchId,aTurnBasedMatchTurn,_Q);
end;



{ --------------------------------------------------------------------
  TGamesAPI
  --------------------------------------------------------------------}

Class Function TGamesAPI.APIName : String;

begin
  Result:='games';
end;

Class Function TGamesAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TGamesAPI.APIRevision : String;

begin
  Result:='20150421';
end;

Class Function TGamesAPI.APIID : String;

begin
  Result:='games:v1';
end;

Class Function TGamesAPI.APITitle : String;

begin
  Result:='Google Play Game Services API';
end;

Class Function TGamesAPI.APIDescription : String;

begin
  Result:='The API for Google Play Game Services.';
end;

Class Function TGamesAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TGamesAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TGamesAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TGamesAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TGamesAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/games/services/';
end;

Class Function TGamesAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TGamesAPI.APIbasePath : string;

begin
  Result:='/games/v1/';
end;

Class Function TGamesAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/games/v1/';
end;

Class Function TGamesAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGamesAPI.APIservicePath : string;

begin
  Result:='games/v1/';
end;

Class Function TGamesAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TGamesAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/drive.appdata';
  Result[0].Description:='View and manage its own configuration data in your Google Drive';
  Result[1].Name:='https://www.googleapis.com/auth/games';
  Result[1].Description:='Share your Google+ profile information and view and manage your game activity';
  Result[2].Name:='https://www.googleapis.com/auth/plus.login';
  Result[2].Description:='Know your basic profile info and list of people in your circles.';
  
end;

Class Function TGamesAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TGamesAPI.RegisterAPIResources;

begin
  TAchievementDefinition.RegisterObject;
  TAchievementDefinitionsListResponse.RegisterObject;
  TAchievementIncrementResponse.RegisterObject;
  TAchievementRevealResponse.RegisterObject;
  TAchievementSetStepsAtLeastResponse.RegisterObject;
  TAchievementUnlockResponse.RegisterObject;
  TAchievementUpdateMultipleRequest.RegisterObject;
  TAchievementUpdateMultipleResponse.RegisterObject;
  TAchievementUpdateRequest.RegisterObject;
  TAchievementUpdateResponse.RegisterObject;
  TAggregateStats.RegisterObject;
  TAnonymousPlayer.RegisterObject;
  TApplication.RegisterObject;
  TApplicationCategory.RegisterObject;
  TCategory.RegisterObject;
  TCategoryListResponse.RegisterObject;
  TEventBatchRecordFailure.RegisterObject;
  TEventChild.RegisterObject;
  TEventDefinition.RegisterObject;
  TEventDefinitionListResponse.RegisterObject;
  TEventPeriodRange.RegisterObject;
  TEventPeriodUpdate.RegisterObject;
  TEventRecordFailure.RegisterObject;
  TEventRecordRequest.RegisterObject;
  TEventUpdateRequest.RegisterObject;
  TEventUpdateResponse.RegisterObject;
  TGamesAchievementIncrement.RegisterObject;
  TGamesAchievementSetStepsAtLeast.RegisterObject;
  TImageAsset.RegisterObject;
  TInstance.RegisterObject;
  TInstanceAndroidDetails.RegisterObject;
  TInstanceIosDetails.RegisterObject;
  TInstanceWebDetails.RegisterObject;
  TLeaderboard.RegisterObject;
  TLeaderboardEntry.RegisterObject;
  TLeaderboardListResponse.RegisterObject;
  TLeaderboardScoreRank.RegisterObject;
  TLeaderboardScores.RegisterObject;
  TMetagameConfig.RegisterObject;
  TNetworkDiagnostics.RegisterObject;
  TParticipantResult.RegisterObject;
  TPeerChannelDiagnostics.RegisterObject;
  TPeerSessionDiagnostics.RegisterObject;
  TPlayed.RegisterObject;
  TPlayerTypename.RegisterObject;
  TPlayer.RegisterObject;
  TPlayerAchievement.RegisterObject;
  TPlayerAchievementListResponse.RegisterObject;
  TPlayerEvent.RegisterObject;
  TPlayerEventListResponse.RegisterObject;
  TPlayerExperienceInfo.RegisterObject;
  TPlayerLeaderboardScore.RegisterObject;
  TPlayerLeaderboardScoreListResponse.RegisterObject;
  TPlayerLevel.RegisterObject;
  TPlayerListResponse.RegisterObject;
  TPlayerScore.RegisterObject;
  TPlayerScoreListResponse.RegisterObject;
  TPlayerScoreResponse.RegisterObject;
  TPlayerScoreSubmissionList.RegisterObject;
  TPushToken.RegisterObject;
  TPushTokenIdTypeios.RegisterObject;
  TPushTokenId.RegisterObject;
  TQuest.RegisterObject;
  TQuestContribution.RegisterObject;
  TQuestCriterion.RegisterObject;
  TQuestListResponse.RegisterObject;
  TQuestMilestone.RegisterObject;
  TRevisionCheckResponse.RegisterObject;
  TRoom.RegisterObject;
  TRoomAutoMatchStatus.RegisterObject;
  TRoomAutoMatchingCriteria.RegisterObject;
  TRoomClientAddress.RegisterObject;
  TRoomCreateRequest.RegisterObject;
  TRoomJoinRequest.RegisterObject;
  TRoomLeaveDiagnostics.RegisterObject;
  TRoomLeaveRequest.RegisterObject;
  TRoomList.RegisterObject;
  TRoomModification.RegisterObject;
  TRoomP2PStatus.RegisterObject;
  TRoomP2PStatuses.RegisterObject;
  TRoomParticipant.RegisterObject;
  TRoomStatus.RegisterObject;
  TScoreSubmission.RegisterObject;
  TSnapshot.RegisterObject;
  TSnapshotImage.RegisterObject;
  TSnapshotListResponse.RegisterObject;
  TTurnBasedAutoMatchingCriteria.RegisterObject;
  TTurnBasedMatch.RegisterObject;
  TTurnBasedMatchCreateRequest.RegisterObject;
  TTurnBasedMatchData.RegisterObject;
  TTurnBasedMatchDataRequest.RegisterObject;
  TTurnBasedMatchList.RegisterObject;
  TTurnBasedMatchModification.RegisterObject;
  TTurnBasedMatchParticipant.RegisterObject;
  TTurnBasedMatchRematch.RegisterObject;
  TTurnBasedMatchResults.RegisterObject;
  TTurnBasedMatchSync.RegisterObject;
  TTurnBasedMatchTurn.RegisterObject;
end;


Function TGamesAPI.GetAchievementDefinitionsInstance : TAchievementDefinitionsResource;

begin
  if (FAchievementDefinitionsInstance=Nil) then
    FAchievementDefinitionsInstance:=CreateAchievementDefinitionsResource;
  Result:=FAchievementDefinitionsInstance;
end;

Function TGamesAPI.CreateAchievementDefinitionsResource : TAchievementDefinitionsResource;

begin
  Result:=CreateAchievementDefinitionsResource(Self);
end;


Function TGamesAPI.CreateAchievementDefinitionsResource(AOwner : TComponent) : TAchievementDefinitionsResource;

begin
  Result:=TAchievementDefinitionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetAchievementsInstance : TAchievementsResource;

begin
  if (FAchievementsInstance=Nil) then
    FAchievementsInstance:=CreateAchievementsResource;
  Result:=FAchievementsInstance;
end;

Function TGamesAPI.CreateAchievementsResource : TAchievementsResource;

begin
  Result:=CreateAchievementsResource(Self);
end;


Function TGamesAPI.CreateAchievementsResource(AOwner : TComponent) : TAchievementsResource;

begin
  Result:=TAchievementsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetApplicationsInstance : TApplicationsResource;

begin
  if (FApplicationsInstance=Nil) then
    FApplicationsInstance:=CreateApplicationsResource;
  Result:=FApplicationsInstance;
end;

Function TGamesAPI.CreateApplicationsResource : TApplicationsResource;

begin
  Result:=CreateApplicationsResource(Self);
end;


Function TGamesAPI.CreateApplicationsResource(AOwner : TComponent) : TApplicationsResource;

begin
  Result:=TApplicationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetEventsInstance : TEventsResource;

begin
  if (FEventsInstance=Nil) then
    FEventsInstance:=CreateEventsResource;
  Result:=FEventsInstance;
end;

Function TGamesAPI.CreateEventsResource : TEventsResource;

begin
  Result:=CreateEventsResource(Self);
end;


Function TGamesAPI.CreateEventsResource(AOwner : TComponent) : TEventsResource;

begin
  Result:=TEventsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetLeaderboardsInstance : TLeaderboardsResource;

begin
  if (FLeaderboardsInstance=Nil) then
    FLeaderboardsInstance:=CreateLeaderboardsResource;
  Result:=FLeaderboardsInstance;
end;

Function TGamesAPI.CreateLeaderboardsResource : TLeaderboardsResource;

begin
  Result:=CreateLeaderboardsResource(Self);
end;


Function TGamesAPI.CreateLeaderboardsResource(AOwner : TComponent) : TLeaderboardsResource;

begin
  Result:=TLeaderboardsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetMetagameInstance : TMetagameResource;

begin
  if (FMetagameInstance=Nil) then
    FMetagameInstance:=CreateMetagameResource;
  Result:=FMetagameInstance;
end;

Function TGamesAPI.CreateMetagameResource : TMetagameResource;

begin
  Result:=CreateMetagameResource(Self);
end;


Function TGamesAPI.CreateMetagameResource(AOwner : TComponent) : TMetagameResource;

begin
  Result:=TMetagameResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetPlayersInstance : TPlayersResource;

begin
  if (FPlayersInstance=Nil) then
    FPlayersInstance:=CreatePlayersResource;
  Result:=FPlayersInstance;
end;

Function TGamesAPI.CreatePlayersResource : TPlayersResource;

begin
  Result:=CreatePlayersResource(Self);
end;


Function TGamesAPI.CreatePlayersResource(AOwner : TComponent) : TPlayersResource;

begin
  Result:=TPlayersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetPushtokensInstance : TPushtokensResource;

begin
  if (FPushtokensInstance=Nil) then
    FPushtokensInstance:=CreatePushtokensResource;
  Result:=FPushtokensInstance;
end;

Function TGamesAPI.CreatePushtokensResource : TPushtokensResource;

begin
  Result:=CreatePushtokensResource(Self);
end;


Function TGamesAPI.CreatePushtokensResource(AOwner : TComponent) : TPushtokensResource;

begin
  Result:=TPushtokensResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetQuestMilestonesInstance : TQuestMilestonesResource;

begin
  if (FQuestMilestonesInstance=Nil) then
    FQuestMilestonesInstance:=CreateQuestMilestonesResource;
  Result:=FQuestMilestonesInstance;
end;

Function TGamesAPI.CreateQuestMilestonesResource : TQuestMilestonesResource;

begin
  Result:=CreateQuestMilestonesResource(Self);
end;


Function TGamesAPI.CreateQuestMilestonesResource(AOwner : TComponent) : TQuestMilestonesResource;

begin
  Result:=TQuestMilestonesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetQuestsInstance : TQuestsResource;

begin
  if (FQuestsInstance=Nil) then
    FQuestsInstance:=CreateQuestsResource;
  Result:=FQuestsInstance;
end;

Function TGamesAPI.CreateQuestsResource : TQuestsResource;

begin
  Result:=CreateQuestsResource(Self);
end;


Function TGamesAPI.CreateQuestsResource(AOwner : TComponent) : TQuestsResource;

begin
  Result:=TQuestsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetRevisionsInstance : TRevisionsResource;

begin
  if (FRevisionsInstance=Nil) then
    FRevisionsInstance:=CreateRevisionsResource;
  Result:=FRevisionsInstance;
end;

Function TGamesAPI.CreateRevisionsResource : TRevisionsResource;

begin
  Result:=CreateRevisionsResource(Self);
end;


Function TGamesAPI.CreateRevisionsResource(AOwner : TComponent) : TRevisionsResource;

begin
  Result:=TRevisionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetRoomsInstance : TRoomsResource;

begin
  if (FRoomsInstance=Nil) then
    FRoomsInstance:=CreateRoomsResource;
  Result:=FRoomsInstance;
end;

Function TGamesAPI.CreateRoomsResource : TRoomsResource;

begin
  Result:=CreateRoomsResource(Self);
end;


Function TGamesAPI.CreateRoomsResource(AOwner : TComponent) : TRoomsResource;

begin
  Result:=TRoomsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetScoresInstance : TScoresResource;

begin
  if (FScoresInstance=Nil) then
    FScoresInstance:=CreateScoresResource;
  Result:=FScoresInstance;
end;

Function TGamesAPI.CreateScoresResource : TScoresResource;

begin
  Result:=CreateScoresResource(Self);
end;


Function TGamesAPI.CreateScoresResource(AOwner : TComponent) : TScoresResource;

begin
  Result:=TScoresResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetSnapshotsInstance : TSnapshotsResource;

begin
  if (FSnapshotsInstance=Nil) then
    FSnapshotsInstance:=CreateSnapshotsResource;
  Result:=FSnapshotsInstance;
end;

Function TGamesAPI.CreateSnapshotsResource : TSnapshotsResource;

begin
  Result:=CreateSnapshotsResource(Self);
end;


Function TGamesAPI.CreateSnapshotsResource(AOwner : TComponent) : TSnapshotsResource;

begin
  Result:=TSnapshotsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesAPI.GetTurnBasedMatchesInstance : TTurnBasedMatchesResource;

begin
  if (FTurnBasedMatchesInstance=Nil) then
    FTurnBasedMatchesInstance:=CreateTurnBasedMatchesResource;
  Result:=FTurnBasedMatchesInstance;
end;

Function TGamesAPI.CreateTurnBasedMatchesResource : TTurnBasedMatchesResource;

begin
  Result:=CreateTurnBasedMatchesResource(Self);
end;


Function TGamesAPI.CreateTurnBasedMatchesResource(AOwner : TComponent) : TTurnBasedMatchesResource;

begin
  Result:=TTurnBasedMatchesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TGamesAPI.RegisterAPI;
end.
