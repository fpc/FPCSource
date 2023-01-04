unit googleclassroom;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TCourse = Class;
  TDriveFolder = Class;
  TCourseMaterialSet = Class;
  TCourseMaterial = Class;
  TDriveFile = Class;
  TYouTubeVideo = Class;
  TLink = Class;
  TForm = Class;
  TEmpty = Class;
  TListCoursesResponse = Class;
  TCourseAlias = Class;
  TListCourseAliasesResponse = Class;
  TInvitation = Class;
  TListInvitationsResponse = Class;
  TUserProfile = Class;
  TName = Class;
  TGlobalPermission = Class;
  TTeacher = Class;
  TListTeachersResponse = Class;
  TStudent = Class;
  TListStudentsResponse = Class;
  TCourseWork = Class;
  TMaterial = Class;
  TSharedDriveFile = Class;
  TDate = Class;
  TTimeOfDay = Class;
  TAssignment = Class;
  TMultipleChoiceQuestion = Class;
  TListCourseWorkResponse = Class;
  TStudentSubmission = Class;
  TAssignmentSubmission = Class;
  TAttachment = Class;
  TShortAnswerSubmission = Class;
  TMultipleChoiceSubmission = Class;
  TListStudentSubmissionsResponse = Class;
  TTurnInStudentSubmissionRequest = Class;
  TReclaimStudentSubmissionRequest = Class;
  TReturnStudentSubmissionRequest = Class;
  TModifyAttachmentsRequest = Class;
  TCourseArray = Array of TCourse;
  TDriveFolderArray = Array of TDriveFolder;
  TCourseMaterialSetArray = Array of TCourseMaterialSet;
  TCourseMaterialArray = Array of TCourseMaterial;
  TDriveFileArray = Array of TDriveFile;
  TYouTubeVideoArray = Array of TYouTubeVideo;
  TLinkArray = Array of TLink;
  TFormArray = Array of TForm;
  TEmptyArray = Array of TEmpty;
  TListCoursesResponseArray = Array of TListCoursesResponse;
  TCourseAliasArray = Array of TCourseAlias;
  TListCourseAliasesResponseArray = Array of TListCourseAliasesResponse;
  TInvitationArray = Array of TInvitation;
  TListInvitationsResponseArray = Array of TListInvitationsResponse;
  TUserProfileArray = Array of TUserProfile;
  TNameArray = Array of TName;
  TGlobalPermissionArray = Array of TGlobalPermission;
  TTeacherArray = Array of TTeacher;
  TListTeachersResponseArray = Array of TListTeachersResponse;
  TStudentArray = Array of TStudent;
  TListStudentsResponseArray = Array of TListStudentsResponse;
  TCourseWorkArray = Array of TCourseWork;
  TMaterialArray = Array of TMaterial;
  TSharedDriveFileArray = Array of TSharedDriveFile;
  TDateArray = Array of TDate;
  TTimeOfDayArray = Array of TTimeOfDay;
  TAssignmentArray = Array of TAssignment;
  TMultipleChoiceQuestionArray = Array of TMultipleChoiceQuestion;
  TListCourseWorkResponseArray = Array of TListCourseWorkResponse;
  TStudentSubmissionArray = Array of TStudentSubmission;
  TAssignmentSubmissionArray = Array of TAssignmentSubmission;
  TAttachmentArray = Array of TAttachment;
  TShortAnswerSubmissionArray = Array of TShortAnswerSubmission;
  TMultipleChoiceSubmissionArray = Array of TMultipleChoiceSubmission;
  TListStudentSubmissionsResponseArray = Array of TListStudentSubmissionsResponse;
  TTurnInStudentSubmissionRequestArray = Array of TTurnInStudentSubmissionRequest;
  TReclaimStudentSubmissionRequestArray = Array of TReclaimStudentSubmissionRequest;
  TReturnStudentSubmissionRequestArray = Array of TReturnStudentSubmissionRequest;
  TModifyAttachmentsRequestArray = Array of TModifyAttachmentsRequest;
  //Anonymous types, using auto-generated names
  TCourseTypecourseMaterialSetsArray = Array of TCourseMaterialSet;
  TCourseMaterialSetTypematerialsArray = Array of TCourseMaterial;
  TListCoursesResponseTypecoursesArray = Array of TCourse;
  TListCourseAliasesResponseTypealiasesArray = Array of TCourseAlias;
  TListInvitationsResponseTypeinvitationsArray = Array of TInvitation;
  TUserProfileTypepermissionsArray = Array of TGlobalPermission;
  TListTeachersResponseTypeteachersArray = Array of TTeacher;
  TListStudentsResponseTypestudentsArray = Array of TStudent;
  TCourseWorkTypematerialsArray = Array of TMaterial;
  TListCourseWorkResponseTypecourseWorkArray = Array of TCourseWork;
  TAssignmentSubmissionTypeattachmentsArray = Array of TAttachment;
  TListStudentSubmissionsResponseTypestudentSubmissionsArray = Array of TStudentSubmission;
  TModifyAttachmentsRequestTypeaddAttachmentsArray = Array of TAttachment;
  
  { --------------------------------------------------------------------
    TCourse
    --------------------------------------------------------------------}
  
  TCourse = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : String;
    Fsection : String;
    FdescriptionHeading : String;
    Fdescription : String;
    Froom : String;
    FownerId : String;
    FcreationTime : String;
    FupdateTime : String;
    FenrollmentCode : String;
    FcourseState : String;
    FalternateLink : String;
    FteacherGroupEmail : String;
    FcourseGroupEmail : String;
    FteacherFolder : TDriveFolder;
    FcourseMaterialSets : TCourseTypecourseMaterialSetsArray;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsection(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdescriptionHeading(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setroom(AIndex : Integer; const AValue : String); virtual;
    Procedure SetownerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetenrollmentCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcourseState(AIndex : Integer; const AValue : String); virtual;
    Procedure SetalternateLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetteacherGroupEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcourseGroupEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetteacherFolder(AIndex : Integer; const AValue : TDriveFolder); virtual;
    Procedure SetcourseMaterialSets(AIndex : Integer; const AValue : TCourseTypecourseMaterialSetsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
    Property section : String Index 16 Read Fsection Write Setsection;
    Property descriptionHeading : String Index 24 Read FdescriptionHeading Write SetdescriptionHeading;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property room : String Index 40 Read Froom Write Setroom;
    Property ownerId : String Index 48 Read FownerId Write SetownerId;
    Property creationTime : String Index 56 Read FcreationTime Write SetcreationTime;
    Property updateTime : String Index 64 Read FupdateTime Write SetupdateTime;
    Property enrollmentCode : String Index 72 Read FenrollmentCode Write SetenrollmentCode;
    Property courseState : String Index 80 Read FcourseState Write SetcourseState;
    Property alternateLink : String Index 88 Read FalternateLink Write SetalternateLink;
    Property teacherGroupEmail : String Index 96 Read FteacherGroupEmail Write SetteacherGroupEmail;
    Property courseGroupEmail : String Index 104 Read FcourseGroupEmail Write SetcourseGroupEmail;
    Property teacherFolder : TDriveFolder Index 112 Read FteacherFolder Write SetteacherFolder;
    Property courseMaterialSets : TCourseTypecourseMaterialSetsArray Index 120 Read FcourseMaterialSets Write SetcourseMaterialSets;
  end;
  TCourseClass = Class of TCourse;
  
  { --------------------------------------------------------------------
    TDriveFolder
    --------------------------------------------------------------------}
  
  TDriveFolder = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Ftitle : String;
    FalternateLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetalternateLink(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property title : String Index 8 Read Ftitle Write Settitle;
    Property alternateLink : String Index 16 Read FalternateLink Write SetalternateLink;
  end;
  TDriveFolderClass = Class of TDriveFolder;
  
  { --------------------------------------------------------------------
    TCourseMaterialSet
    --------------------------------------------------------------------}
  
  TCourseMaterialSet = Class(TGoogleBaseObject)
  Private
    Ftitle : String;
    Fmaterials : TCourseMaterialSetTypematerialsArray;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmaterials(AIndex : Integer; const AValue : TCourseMaterialSetTypematerialsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property title : String Index 0 Read Ftitle Write Settitle;
    Property materials : TCourseMaterialSetTypematerialsArray Index 8 Read Fmaterials Write Setmaterials;
  end;
  TCourseMaterialSetClass = Class of TCourseMaterialSet;
  
  { --------------------------------------------------------------------
    TCourseMaterial
    --------------------------------------------------------------------}
  
  TCourseMaterial = Class(TGoogleBaseObject)
  Private
    FdriveFile : TDriveFile;
    FyouTubeVideo : TYouTubeVideo;
    Flink : TLink;
    Fform : TForm;
  Protected
    //Property setters
    Procedure SetdriveFile(AIndex : Integer; const AValue : TDriveFile); virtual;
    Procedure SetyouTubeVideo(AIndex : Integer; const AValue : TYouTubeVideo); virtual;
    Procedure Setlink(AIndex : Integer; const AValue : TLink); virtual;
    Procedure Setform(AIndex : Integer; const AValue : TForm); virtual;
  Public
  Published
    Property driveFile : TDriveFile Index 0 Read FdriveFile Write SetdriveFile;
    Property youTubeVideo : TYouTubeVideo Index 8 Read FyouTubeVideo Write SetyouTubeVideo;
    Property link : TLink Index 16 Read Flink Write Setlink;
    Property form : TForm Index 24 Read Fform Write Setform;
  end;
  TCourseMaterialClass = Class of TCourseMaterial;
  
  { --------------------------------------------------------------------
    TDriveFile
    --------------------------------------------------------------------}
  
  TDriveFile = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Ftitle : String;
    FalternateLink : String;
    FthumbnailUrl : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetalternateLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetthumbnailUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property title : String Index 8 Read Ftitle Write Settitle;
    Property alternateLink : String Index 16 Read FalternateLink Write SetalternateLink;
    Property thumbnailUrl : String Index 24 Read FthumbnailUrl Write SetthumbnailUrl;
  end;
  TDriveFileClass = Class of TDriveFile;
  
  { --------------------------------------------------------------------
    TYouTubeVideo
    --------------------------------------------------------------------}
  
  TYouTubeVideo = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Ftitle : String;
    FalternateLink : String;
    FthumbnailUrl : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetalternateLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetthumbnailUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property title : String Index 8 Read Ftitle Write Settitle;
    Property alternateLink : String Index 16 Read FalternateLink Write SetalternateLink;
    Property thumbnailUrl : String Index 24 Read FthumbnailUrl Write SetthumbnailUrl;
  end;
  TYouTubeVideoClass = Class of TYouTubeVideo;
  
  { --------------------------------------------------------------------
    TLink
    --------------------------------------------------------------------}
  
  TLink = Class(TGoogleBaseObject)
  Private
    Furl : String;
    Ftitle : String;
    FthumbnailUrl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetthumbnailUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
    Property title : String Index 8 Read Ftitle Write Settitle;
    Property thumbnailUrl : String Index 16 Read FthumbnailUrl Write SetthumbnailUrl;
  end;
  TLinkClass = Class of TLink;
  
  { --------------------------------------------------------------------
    TForm
    --------------------------------------------------------------------}
  
  TForm = Class(TGoogleBaseObject)
  Private
    FformUrl : String;
    FresponseUrl : String;
    Ftitle : String;
    FthumbnailUrl : String;
  Protected
    //Property setters
    Procedure SetformUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetresponseUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetthumbnailUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property formUrl : String Index 0 Read FformUrl Write SetformUrl;
    Property responseUrl : String Index 8 Read FresponseUrl Write SetresponseUrl;
    Property title : String Index 16 Read Ftitle Write Settitle;
    Property thumbnailUrl : String Index 24 Read FthumbnailUrl Write SetthumbnailUrl;
  end;
  TFormClass = Class of TForm;
  
  { --------------------------------------------------------------------
    TEmpty
    --------------------------------------------------------------------}
  
  TEmpty = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEmptyClass = Class of TEmpty;
  
  { --------------------------------------------------------------------
    TListCoursesResponse
    --------------------------------------------------------------------}
  
  TListCoursesResponse = Class(TGoogleBaseObject)
  Private
    Fcourses : TListCoursesResponseTypecoursesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setcourses(AIndex : Integer; const AValue : TListCoursesResponseTypecoursesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property courses : TListCoursesResponseTypecoursesArray Index 0 Read Fcourses Write Setcourses;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListCoursesResponseClass = Class of TListCoursesResponse;
  
  { --------------------------------------------------------------------
    TCourseAlias
    --------------------------------------------------------------------}
  
  TCourseAlias = Class(TGoogleBaseObject)
  Private
    Falias : String;
  Protected
    //Property setters
    Procedure Setalias(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property alias : String Index 0 Read Falias Write Setalias;
  end;
  TCourseAliasClass = Class of TCourseAlias;
  
  { --------------------------------------------------------------------
    TListCourseAliasesResponse
    --------------------------------------------------------------------}
  
  TListCourseAliasesResponse = Class(TGoogleBaseObject)
  Private
    Faliases : TListCourseAliasesResponseTypealiasesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setaliases(AIndex : Integer; const AValue : TListCourseAliasesResponseTypealiasesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property aliases : TListCourseAliasesResponseTypealiasesArray Index 0 Read Faliases Write Setaliases;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListCourseAliasesResponseClass = Class of TListCourseAliasesResponse;
  
  { --------------------------------------------------------------------
    TInvitation
    --------------------------------------------------------------------}
  
  TInvitation = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FuserId : String;
    FcourseId : String;
    Frole : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcourseId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrole(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property userId : String Index 8 Read FuserId Write SetuserId;
    Property courseId : String Index 16 Read FcourseId Write SetcourseId;
    Property role : String Index 24 Read Frole Write Setrole;
  end;
  TInvitationClass = Class of TInvitation;
  
  { --------------------------------------------------------------------
    TListInvitationsResponse
    --------------------------------------------------------------------}
  
  TListInvitationsResponse = Class(TGoogleBaseObject)
  Private
    Finvitations : TListInvitationsResponseTypeinvitationsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setinvitations(AIndex : Integer; const AValue : TListInvitationsResponseTypeinvitationsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property invitations : TListInvitationsResponseTypeinvitationsArray Index 0 Read Finvitations Write Setinvitations;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListInvitationsResponseClass = Class of TListInvitationsResponse;
  
  { --------------------------------------------------------------------
    TUserProfile
    --------------------------------------------------------------------}
  
  TUserProfile = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : TName;
    FemailAddress : String;
    FphotoUrl : String;
    Fpermissions : TUserProfileTypepermissionsArray;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : TName); virtual;
    Procedure SetemailAddress(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpermissions(AIndex : Integer; const AValue : TUserProfileTypepermissionsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : TName Index 8 Read Fname Write Setname;
    Property emailAddress : String Index 16 Read FemailAddress Write SetemailAddress;
    Property photoUrl : String Index 24 Read FphotoUrl Write SetphotoUrl;
    Property permissions : TUserProfileTypepermissionsArray Index 32 Read Fpermissions Write Setpermissions;
  end;
  TUserProfileClass = Class of TUserProfile;
  
  { --------------------------------------------------------------------
    TName
    --------------------------------------------------------------------}
  
  TName = Class(TGoogleBaseObject)
  Private
    FgivenName : String;
    FfamilyName : String;
    FfullName : String;
  Protected
    //Property setters
    Procedure SetgivenName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfamilyName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfullName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property givenName : String Index 0 Read FgivenName Write SetgivenName;
    Property familyName : String Index 8 Read FfamilyName Write SetfamilyName;
    Property fullName : String Index 16 Read FfullName Write SetfullName;
  end;
  TNameClass = Class of TName;
  
  { --------------------------------------------------------------------
    TGlobalPermission
    --------------------------------------------------------------------}
  
  TGlobalPermission = Class(TGoogleBaseObject)
  Private
    Fpermission : String;
  Protected
    //Property setters
    Procedure Setpermission(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property permission : String Index 0 Read Fpermission Write Setpermission;
  end;
  TGlobalPermissionClass = Class of TGlobalPermission;
  
  { --------------------------------------------------------------------
    TTeacher
    --------------------------------------------------------------------}
  
  TTeacher = Class(TGoogleBaseObject)
  Private
    FcourseId : String;
    FuserId : String;
    Fprofile : TUserProfile;
  Protected
    //Property setters
    Procedure SetcourseId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprofile(AIndex : Integer; const AValue : TUserProfile); virtual;
  Public
  Published
    Property courseId : String Index 0 Read FcourseId Write SetcourseId;
    Property userId : String Index 8 Read FuserId Write SetuserId;
    Property profile : TUserProfile Index 16 Read Fprofile Write Setprofile;
  end;
  TTeacherClass = Class of TTeacher;
  
  { --------------------------------------------------------------------
    TListTeachersResponse
    --------------------------------------------------------------------}
  
  TListTeachersResponse = Class(TGoogleBaseObject)
  Private
    Fteachers : TListTeachersResponseTypeteachersArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setteachers(AIndex : Integer; const AValue : TListTeachersResponseTypeteachersArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property teachers : TListTeachersResponseTypeteachersArray Index 0 Read Fteachers Write Setteachers;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListTeachersResponseClass = Class of TListTeachersResponse;
  
  { --------------------------------------------------------------------
    TStudent
    --------------------------------------------------------------------}
  
  TStudent = Class(TGoogleBaseObject)
  Private
    FcourseId : String;
    FuserId : String;
    Fprofile : TUserProfile;
    FstudentWorkFolder : TDriveFolder;
  Protected
    //Property setters
    Procedure SetcourseId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprofile(AIndex : Integer; const AValue : TUserProfile); virtual;
    Procedure SetstudentWorkFolder(AIndex : Integer; const AValue : TDriveFolder); virtual;
  Public
  Published
    Property courseId : String Index 0 Read FcourseId Write SetcourseId;
    Property userId : String Index 8 Read FuserId Write SetuserId;
    Property profile : TUserProfile Index 16 Read Fprofile Write Setprofile;
    Property studentWorkFolder : TDriveFolder Index 24 Read FstudentWorkFolder Write SetstudentWorkFolder;
  end;
  TStudentClass = Class of TStudent;
  
  { --------------------------------------------------------------------
    TListStudentsResponse
    --------------------------------------------------------------------}
  
  TListStudentsResponse = Class(TGoogleBaseObject)
  Private
    Fstudents : TListStudentsResponseTypestudentsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setstudents(AIndex : Integer; const AValue : TListStudentsResponseTypestudentsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property students : TListStudentsResponseTypestudentsArray Index 0 Read Fstudents Write Setstudents;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListStudentsResponseClass = Class of TListStudentsResponse;
  
  { --------------------------------------------------------------------
    TCourseWork
    --------------------------------------------------------------------}
  
  TCourseWork = Class(TGoogleBaseObject)
  Private
    FcourseId : String;
    Fid : String;
    Ftitle : String;
    Fdescription : String;
    Fmaterials : TCourseWorkTypematerialsArray;
    Fstate : String;
    FalternateLink : String;
    FcreationTime : String;
    FupdateTime : String;
    FdueDate : TDate;
    FdueTime : TTimeOfDay;
    FmaxPoints : double;
    FworkType : String;
    FassociatedWithDeveloper : boolean;
    FsubmissionModificationMode : String;
    Fassignment : TAssignment;
    FmultipleChoiceQuestion : TMultipleChoiceQuestion;
  Protected
    //Property setters
    Procedure SetcourseId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmaterials(AIndex : Integer; const AValue : TCourseWorkTypematerialsArray); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetalternateLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdueDate(AIndex : Integer; const AValue : TDate); virtual;
    Procedure SetdueTime(AIndex : Integer; const AValue : TTimeOfDay); virtual;
    Procedure SetmaxPoints(AIndex : Integer; const AValue : double); virtual;
    Procedure SetworkType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetassociatedWithDeveloper(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsubmissionModificationMode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setassignment(AIndex : Integer; const AValue : TAssignment); virtual;
    Procedure SetmultipleChoiceQuestion(AIndex : Integer; const AValue : TMultipleChoiceQuestion); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property courseId : String Index 0 Read FcourseId Write SetcourseId;
    Property id : String Index 8 Read Fid Write Setid;
    Property title : String Index 16 Read Ftitle Write Settitle;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property materials : TCourseWorkTypematerialsArray Index 32 Read Fmaterials Write Setmaterials;
    Property state : String Index 40 Read Fstate Write Setstate;
    Property alternateLink : String Index 48 Read FalternateLink Write SetalternateLink;
    Property creationTime : String Index 56 Read FcreationTime Write SetcreationTime;
    Property updateTime : String Index 64 Read FupdateTime Write SetupdateTime;
    Property dueDate : TDate Index 72 Read FdueDate Write SetdueDate;
    Property dueTime : TTimeOfDay Index 80 Read FdueTime Write SetdueTime;
    Property maxPoints : double Index 88 Read FmaxPoints Write SetmaxPoints;
    Property workType : String Index 96 Read FworkType Write SetworkType;
    Property associatedWithDeveloper : boolean Index 104 Read FassociatedWithDeveloper Write SetassociatedWithDeveloper;
    Property submissionModificationMode : String Index 112 Read FsubmissionModificationMode Write SetsubmissionModificationMode;
    Property assignment : TAssignment Index 120 Read Fassignment Write Setassignment;
    Property multipleChoiceQuestion : TMultipleChoiceQuestion Index 128 Read FmultipleChoiceQuestion Write SetmultipleChoiceQuestion;
  end;
  TCourseWorkClass = Class of TCourseWork;
  
  { --------------------------------------------------------------------
    TMaterial
    --------------------------------------------------------------------}
  
  TMaterial = Class(TGoogleBaseObject)
  Private
    FdriveFile : TSharedDriveFile;
    FyoutubeVideo : TYouTubeVideo;
    Flink : TLink;
    Fform : TForm;
  Protected
    //Property setters
    Procedure SetdriveFile(AIndex : Integer; const AValue : TSharedDriveFile); virtual;
    Procedure SetyoutubeVideo(AIndex : Integer; const AValue : TYouTubeVideo); virtual;
    Procedure Setlink(AIndex : Integer; const AValue : TLink); virtual;
    Procedure Setform(AIndex : Integer; const AValue : TForm); virtual;
  Public
  Published
    Property driveFile : TSharedDriveFile Index 0 Read FdriveFile Write SetdriveFile;
    Property youtubeVideo : TYouTubeVideo Index 8 Read FyoutubeVideo Write SetyoutubeVideo;
    Property link : TLink Index 16 Read Flink Write Setlink;
    Property form : TForm Index 24 Read Fform Write Setform;
  end;
  TMaterialClass = Class of TMaterial;
  
  { --------------------------------------------------------------------
    TSharedDriveFile
    --------------------------------------------------------------------}
  
  TSharedDriveFile = Class(TGoogleBaseObject)
  Private
    FdriveFile : TDriveFile;
    FshareMode : String;
  Protected
    //Property setters
    Procedure SetdriveFile(AIndex : Integer; const AValue : TDriveFile); virtual;
    Procedure SetshareMode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property driveFile : TDriveFile Index 0 Read FdriveFile Write SetdriveFile;
    Property shareMode : String Index 8 Read FshareMode Write SetshareMode;
  end;
  TSharedDriveFileClass = Class of TSharedDriveFile;
  
  { --------------------------------------------------------------------
    TDate
    --------------------------------------------------------------------}
  
  TDate = Class(TGoogleBaseObject)
  Private
    Fyear : integer;
    Fmonth : integer;
    Fday : integer;
  Protected
    //Property setters
    Procedure Setyear(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmonth(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setday(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property year : integer Index 0 Read Fyear Write Setyear;
    Property month : integer Index 8 Read Fmonth Write Setmonth;
    Property day : integer Index 16 Read Fday Write Setday;
  end;
  TDateClass = Class of TDate;
  
  { --------------------------------------------------------------------
    TTimeOfDay
    --------------------------------------------------------------------}
  
  TTimeOfDay = Class(TGoogleBaseObject)
  Private
    Fhours : integer;
    Fminutes : integer;
    Fseconds : integer;
    Fnanos : integer;
  Protected
    //Property setters
    Procedure Sethours(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setminutes(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setseconds(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setnanos(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property hours : integer Index 0 Read Fhours Write Sethours;
    Property minutes : integer Index 8 Read Fminutes Write Setminutes;
    Property seconds : integer Index 16 Read Fseconds Write Setseconds;
    Property nanos : integer Index 24 Read Fnanos Write Setnanos;
  end;
  TTimeOfDayClass = Class of TTimeOfDay;
  
  { --------------------------------------------------------------------
    TAssignment
    --------------------------------------------------------------------}
  
  TAssignment = Class(TGoogleBaseObject)
  Private
    FstudentWorkFolder : TDriveFolder;
  Protected
    //Property setters
    Procedure SetstudentWorkFolder(AIndex : Integer; const AValue : TDriveFolder); virtual;
  Public
  Published
    Property studentWorkFolder : TDriveFolder Index 0 Read FstudentWorkFolder Write SetstudentWorkFolder;
  end;
  TAssignmentClass = Class of TAssignment;
  
  { --------------------------------------------------------------------
    TMultipleChoiceQuestion
    --------------------------------------------------------------------}
  
  TMultipleChoiceQuestion = Class(TGoogleBaseObject)
  Private
    Fchoices : TStringArray;
  Protected
    //Property setters
    Procedure Setchoices(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property choices : TStringArray Index 0 Read Fchoices Write Setchoices;
  end;
  TMultipleChoiceQuestionClass = Class of TMultipleChoiceQuestion;
  
  { --------------------------------------------------------------------
    TListCourseWorkResponse
    --------------------------------------------------------------------}
  
  TListCourseWorkResponse = Class(TGoogleBaseObject)
  Private
    FcourseWork : TListCourseWorkResponseTypecourseWorkArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetcourseWork(AIndex : Integer; const AValue : TListCourseWorkResponseTypecourseWorkArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property courseWork : TListCourseWorkResponseTypecourseWorkArray Index 0 Read FcourseWork Write SetcourseWork;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListCourseWorkResponseClass = Class of TListCourseWorkResponse;
  
  { --------------------------------------------------------------------
    TStudentSubmission
    --------------------------------------------------------------------}
  
  TStudentSubmission = Class(TGoogleBaseObject)
  Private
    FcourseId : String;
    FcourseWorkId : String;
    Fid : String;
    FuserId : String;
    FcreationTime : String;
    FupdateTime : String;
    Fstate : String;
    Flate : boolean;
    FdraftGrade : double;
    FassignedGrade : double;
    FalternateLink : String;
    FcourseWorkType : String;
    FassociatedWithDeveloper : boolean;
    FassignmentSubmission : TAssignmentSubmission;
    FshortAnswerSubmission : TShortAnswerSubmission;
    FmultipleChoiceSubmission : TMultipleChoiceSubmission;
  Protected
    //Property setters
    Procedure SetcourseId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcourseWorkId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlate(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdraftGrade(AIndex : Integer; const AValue : double); virtual;
    Procedure SetassignedGrade(AIndex : Integer; const AValue : double); virtual;
    Procedure SetalternateLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcourseWorkType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetassociatedWithDeveloper(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetassignmentSubmission(AIndex : Integer; const AValue : TAssignmentSubmission); virtual;
    Procedure SetshortAnswerSubmission(AIndex : Integer; const AValue : TShortAnswerSubmission); virtual;
    Procedure SetmultipleChoiceSubmission(AIndex : Integer; const AValue : TMultipleChoiceSubmission); virtual;
  Public
  Published
    Property courseId : String Index 0 Read FcourseId Write SetcourseId;
    Property courseWorkId : String Index 8 Read FcourseWorkId Write SetcourseWorkId;
    Property id : String Index 16 Read Fid Write Setid;
    Property userId : String Index 24 Read FuserId Write SetuserId;
    Property creationTime : String Index 32 Read FcreationTime Write SetcreationTime;
    Property updateTime : String Index 40 Read FupdateTime Write SetupdateTime;
    Property state : String Index 48 Read Fstate Write Setstate;
    Property late : boolean Index 56 Read Flate Write Setlate;
    Property draftGrade : double Index 64 Read FdraftGrade Write SetdraftGrade;
    Property assignedGrade : double Index 72 Read FassignedGrade Write SetassignedGrade;
    Property alternateLink : String Index 80 Read FalternateLink Write SetalternateLink;
    Property courseWorkType : String Index 88 Read FcourseWorkType Write SetcourseWorkType;
    Property associatedWithDeveloper : boolean Index 96 Read FassociatedWithDeveloper Write SetassociatedWithDeveloper;
    Property assignmentSubmission : TAssignmentSubmission Index 104 Read FassignmentSubmission Write SetassignmentSubmission;
    Property shortAnswerSubmission : TShortAnswerSubmission Index 112 Read FshortAnswerSubmission Write SetshortAnswerSubmission;
    Property multipleChoiceSubmission : TMultipleChoiceSubmission Index 120 Read FmultipleChoiceSubmission Write SetmultipleChoiceSubmission;
  end;
  TStudentSubmissionClass = Class of TStudentSubmission;
  
  { --------------------------------------------------------------------
    TAssignmentSubmission
    --------------------------------------------------------------------}
  
  TAssignmentSubmission = Class(TGoogleBaseObject)
  Private
    Fattachments : TAssignmentSubmissionTypeattachmentsArray;
  Protected
    //Property setters
    Procedure Setattachments(AIndex : Integer; const AValue : TAssignmentSubmissionTypeattachmentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property attachments : TAssignmentSubmissionTypeattachmentsArray Index 0 Read Fattachments Write Setattachments;
  end;
  TAssignmentSubmissionClass = Class of TAssignmentSubmission;
  
  { --------------------------------------------------------------------
    TAttachment
    --------------------------------------------------------------------}
  
  TAttachment = Class(TGoogleBaseObject)
  Private
    FdriveFile : TDriveFile;
    FyouTubeVideo : TYouTubeVideo;
    Flink : TLink;
    Fform : TForm;
  Protected
    //Property setters
    Procedure SetdriveFile(AIndex : Integer; const AValue : TDriveFile); virtual;
    Procedure SetyouTubeVideo(AIndex : Integer; const AValue : TYouTubeVideo); virtual;
    Procedure Setlink(AIndex : Integer; const AValue : TLink); virtual;
    Procedure Setform(AIndex : Integer; const AValue : TForm); virtual;
  Public
  Published
    Property driveFile : TDriveFile Index 0 Read FdriveFile Write SetdriveFile;
    Property youTubeVideo : TYouTubeVideo Index 8 Read FyouTubeVideo Write SetyouTubeVideo;
    Property link : TLink Index 16 Read Flink Write Setlink;
    Property form : TForm Index 24 Read Fform Write Setform;
  end;
  TAttachmentClass = Class of TAttachment;
  
  { --------------------------------------------------------------------
    TShortAnswerSubmission
    --------------------------------------------------------------------}
  
  TShortAnswerSubmission = Class(TGoogleBaseObject)
  Private
    Fanswer : String;
  Protected
    //Property setters
    Procedure Setanswer(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property answer : String Index 0 Read Fanswer Write Setanswer;
  end;
  TShortAnswerSubmissionClass = Class of TShortAnswerSubmission;
  
  { --------------------------------------------------------------------
    TMultipleChoiceSubmission
    --------------------------------------------------------------------}
  
  TMultipleChoiceSubmission = Class(TGoogleBaseObject)
  Private
    Fanswer : String;
  Protected
    //Property setters
    Procedure Setanswer(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property answer : String Index 0 Read Fanswer Write Setanswer;
  end;
  TMultipleChoiceSubmissionClass = Class of TMultipleChoiceSubmission;
  
  { --------------------------------------------------------------------
    TListStudentSubmissionsResponse
    --------------------------------------------------------------------}
  
  TListStudentSubmissionsResponse = Class(TGoogleBaseObject)
  Private
    FstudentSubmissions : TListStudentSubmissionsResponseTypestudentSubmissionsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetstudentSubmissions(AIndex : Integer; const AValue : TListStudentSubmissionsResponseTypestudentSubmissionsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property studentSubmissions : TListStudentSubmissionsResponseTypestudentSubmissionsArray Index 0 Read FstudentSubmissions Write SetstudentSubmissions;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListStudentSubmissionsResponseClass = Class of TListStudentSubmissionsResponse;
  
  { --------------------------------------------------------------------
    TTurnInStudentSubmissionRequest
    --------------------------------------------------------------------}
  
  TTurnInStudentSubmissionRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTurnInStudentSubmissionRequestClass = Class of TTurnInStudentSubmissionRequest;
  
  { --------------------------------------------------------------------
    TReclaimStudentSubmissionRequest
    --------------------------------------------------------------------}
  
  TReclaimStudentSubmissionRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReclaimStudentSubmissionRequestClass = Class of TReclaimStudentSubmissionRequest;
  
  { --------------------------------------------------------------------
    TReturnStudentSubmissionRequest
    --------------------------------------------------------------------}
  
  TReturnStudentSubmissionRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReturnStudentSubmissionRequestClass = Class of TReturnStudentSubmissionRequest;
  
  { --------------------------------------------------------------------
    TModifyAttachmentsRequest
    --------------------------------------------------------------------}
  
  TModifyAttachmentsRequest = Class(TGoogleBaseObject)
  Private
    FaddAttachments : TModifyAttachmentsRequestTypeaddAttachmentsArray;
  Protected
    //Property setters
    Procedure SetaddAttachments(AIndex : Integer; const AValue : TModifyAttachmentsRequestTypeaddAttachmentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property addAttachments : TModifyAttachmentsRequestTypeaddAttachmentsArray Index 0 Read FaddAttachments Write SetaddAttachments;
  end;
  TModifyAttachmentsRequestClass = Class of TModifyAttachmentsRequest;
  
  { --------------------------------------------------------------------
    TCoursesAliasesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCoursesAliasesResource, method List
  
  TCoursesAliasesListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TCoursesAliasesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(courseId: string; aCourseAlias : TCourseAlias) : TCourseAlias;overload;
    Function Delete(courseId: string; alias: string) : TEmpty;
    Function List(courseId: string; AQuery : string  = '') : TListCourseAliasesResponse;
    Function List(courseId: string; AQuery : TCoursesAliaseslistOptions) : TListCourseAliasesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCoursesTeachersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCoursesTeachersResource, method List
  
  TCoursesTeachersListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TCoursesTeachersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(courseId: string; aTeacher : TTeacher) : TTeacher;overload;
    Function Get(courseId: string; userId: string) : TTeacher;
    Function Delete(courseId: string; userId: string) : TEmpty;
    Function List(courseId: string; AQuery : string  = '') : TListTeachersResponse;
    Function List(courseId: string; AQuery : TCoursesTeacherslistOptions) : TListTeachersResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCoursesStudentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCoursesStudentsResource, method Create
  
  TCoursesStudentsCreateOptions = Record
    enrollmentCode : String;
  end;
  
  
  //Optional query Options for TCoursesStudentsResource, method List
  
  TCoursesStudentsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TCoursesStudentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(courseId: string; aStudent : TStudent; AQuery : string  = '') : TStudent;overload;
    Function Create(courseId: string; aStudent : TStudent; AQuery : TCoursesStudentscreateOptions) : TStudent;overload;
    Function Get(courseId: string; userId: string) : TStudent;
    Function Delete(courseId: string; userId: string) : TEmpty;
    Function List(courseId: string; AQuery : string  = '') : TListStudentsResponse;
    Function List(courseId: string; AQuery : TCoursesStudentslistOptions) : TListStudentsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCoursesCourseWorkStudentSubmissionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCoursesCourseWorkStudentSubmissionsResource, method Patch
  
  TCoursesCourseWorkStudentSubmissionsPatchOptions = Record
    updateMask : String;
  end;
  
  
  //Optional query Options for TCoursesCourseWorkStudentSubmissionsResource, method List
  
  TCoursesCourseWorkStudentSubmissionsListOptions = Record
    userId : String;
    states : String;
    late : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TCoursesCourseWorkStudentSubmissionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(courseId: string; courseWorkId: string; id: string) : TStudentSubmission;
    Function Patch(courseId: string; courseWorkId: string; id: string; aStudentSubmission : TStudentSubmission; AQuery : string  = '') : TStudentSubmission;
    Function Patch(courseId: string; courseWorkId: string; id: string; aStudentSubmission : TStudentSubmission; AQuery : TCoursesCourseWorkStudentSubmissionspatchOptions) : TStudentSubmission;
    Function List(courseId: string; courseWorkId: string; AQuery : string  = '') : TListStudentSubmissionsResponse;
    Function List(courseId: string; courseWorkId: string; AQuery : TCoursesCourseWorkStudentSubmissionslistOptions) : TListStudentSubmissionsResponse;
    Function TurnIn(courseId: string; courseWorkId: string; id: string; aTurnInStudentSubmissionRequest : TTurnInStudentSubmissionRequest) : TEmpty;
    Function Reclaim(courseId: string; courseWorkId: string; id: string; aReclaimStudentSubmissionRequest : TReclaimStudentSubmissionRequest) : TEmpty;
    Function Return(courseId: string; courseWorkId: string; id: string; aReturnStudentSubmissionRequest : TReturnStudentSubmissionRequest) : TEmpty;
    Function ModifyAttachments(courseId: string; courseWorkId: string; id: string; aModifyAttachmentsRequest : TModifyAttachmentsRequest) : TStudentSubmission;
  end;
  
  
  { --------------------------------------------------------------------
    TCoursesCourseWorkResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCoursesCourseWorkResource, method List
  
  TCoursesCourseWorkListOptions = Record
    courseWorkStates : String;
    orderBy : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TCoursesCourseWorkResource = Class(TGoogleResource)
  Private
    FStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;
    Function GetStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(courseId: string; aCourseWork : TCourseWork) : TCourseWork;overload;
    Function Get(courseId: string; id: string) : TCourseWork;
    Function List(courseId: string; AQuery : string  = '') : TListCourseWorkResponse;
    Function List(courseId: string; AQuery : TCoursesCourseWorklistOptions) : TListCourseWorkResponse;
    Function CreateStudentSubmissionsResource(AOwner : TComponent) : TCoursesCourseWorkStudentSubmissionsResource;virtual;overload;
    Function CreateStudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource;virtual;overload;
    Property StudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource Read GetStudentSubmissionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TCoursesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCoursesResource, method Patch
  
  TCoursesPatchOptions = Record
    updateMask : String;
  end;
  
  
  //Optional query Options for TCoursesResource, method List
  
  TCoursesListOptions = Record
    studentId : String;
    teacherId : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TCoursesResource = Class(TGoogleResource)
  Private
    FAliasesInstance : TCoursesAliasesResource;
    FTeachersInstance : TCoursesTeachersResource;
    FStudentsInstance : TCoursesStudentsResource;
    FCourseWorkStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;
    FCourseWorkInstance : TCoursesCourseWorkResource;
    Function GetAliasesInstance : TCoursesAliasesResource;virtual;
    Function GetTeachersInstance : TCoursesTeachersResource;virtual;
    Function GetStudentsInstance : TCoursesStudentsResource;virtual;
    Function GetCourseWorkStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;virtual;
    Function GetCourseWorkInstance : TCoursesCourseWorkResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aCourse : TCourse) : TCourse;overload;
    Function Get(id: string) : TCourse;
    Function Update(id: string; aCourse : TCourse) : TCourse;
    Function Patch(id: string; aCourse : TCourse; AQuery : string  = '') : TCourse;
    Function Patch(id: string; aCourse : TCourse; AQuery : TCoursespatchOptions) : TCourse;
    Function Delete(id: string) : TEmpty;
    Function List(AQuery : string  = '') : TListCoursesResponse;
    Function List(AQuery : TCourseslistOptions) : TListCoursesResponse;
    Function CreateAliasesResource(AOwner : TComponent) : TCoursesAliasesResource;virtual;overload;
    Function CreateAliasesResource : TCoursesAliasesResource;virtual;overload;
    Function CreateTeachersResource(AOwner : TComponent) : TCoursesTeachersResource;virtual;overload;
    Function CreateTeachersResource : TCoursesTeachersResource;virtual;overload;
    Function CreateStudentsResource(AOwner : TComponent) : TCoursesStudentsResource;virtual;overload;
    Function CreateStudentsResource : TCoursesStudentsResource;virtual;overload;
    Function CreateCourseWorkStudentSubmissionsResource(AOwner : TComponent) : TCoursesCourseWorkStudentSubmissionsResource;virtual;overload;
    Function CreateCourseWorkStudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource;virtual;overload;
    Function CreateCourseWorkResource(AOwner : TComponent) : TCoursesCourseWorkResource;virtual;overload;
    Function CreateCourseWorkResource : TCoursesCourseWorkResource;virtual;overload;
    Property AliasesResource : TCoursesAliasesResource Read GetAliasesInstance;
    Property TeachersResource : TCoursesTeachersResource Read GetTeachersInstance;
    Property StudentsResource : TCoursesStudentsResource Read GetStudentsInstance;
    Property CourseWorkStudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource Read GetCourseWorkStudentSubmissionsInstance;
    Property CourseWorkResource : TCoursesCourseWorkResource Read GetCourseWorkInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TInvitationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TInvitationsResource, method List
  
  TInvitationsListOptions = Record
    userId : String;
    courseId : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TInvitationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aInvitation : TInvitation) : TInvitation;overload;
    Function Get(id: string) : TInvitation;
    Function Delete(id: string) : TEmpty;
    Function List(AQuery : string  = '') : TListInvitationsResponse;
    Function List(AQuery : TInvitationslistOptions) : TListInvitationsResponse;
    Function Accept(id: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TUserProfilesResource
    --------------------------------------------------------------------}
  
  TUserProfilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(userId: string) : TUserProfile;
  end;
  
  
  { --------------------------------------------------------------------
    TClassroomAPI
    --------------------------------------------------------------------}
  
  TClassroomAPI = Class(TGoogleAPI)
  Private
    FCoursesAliasesInstance : TCoursesAliasesResource;
    FCoursesTeachersInstance : TCoursesTeachersResource;
    FCoursesStudentsInstance : TCoursesStudentsResource;
    FCoursesCourseWorkStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;
    FCoursesCourseWorkInstance : TCoursesCourseWorkResource;
    FCoursesInstance : TCoursesResource;
    FInvitationsInstance : TInvitationsResource;
    FUserProfilesInstance : TUserProfilesResource;
    Function GetCoursesAliasesInstance : TCoursesAliasesResource;virtual;
    Function GetCoursesTeachersInstance : TCoursesTeachersResource;virtual;
    Function GetCoursesStudentsInstance : TCoursesStudentsResource;virtual;
    Function GetCoursesCourseWorkStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;virtual;
    Function GetCoursesCourseWorkInstance : TCoursesCourseWorkResource;virtual;
    Function GetCoursesInstance : TCoursesResource;virtual;
    Function GetInvitationsInstance : TInvitationsResource;virtual;
    Function GetUserProfilesInstance : TUserProfilesResource;virtual;
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
    Function CreateCoursesAliasesResource(AOwner : TComponent) : TCoursesAliasesResource;virtual;overload;
    Function CreateCoursesAliasesResource : TCoursesAliasesResource;virtual;overload;
    Function CreateCoursesTeachersResource(AOwner : TComponent) : TCoursesTeachersResource;virtual;overload;
    Function CreateCoursesTeachersResource : TCoursesTeachersResource;virtual;overload;
    Function CreateCoursesStudentsResource(AOwner : TComponent) : TCoursesStudentsResource;virtual;overload;
    Function CreateCoursesStudentsResource : TCoursesStudentsResource;virtual;overload;
    Function CreateCoursesCourseWorkStudentSubmissionsResource(AOwner : TComponent) : TCoursesCourseWorkStudentSubmissionsResource;virtual;overload;
    Function CreateCoursesCourseWorkStudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource;virtual;overload;
    Function CreateCoursesCourseWorkResource(AOwner : TComponent) : TCoursesCourseWorkResource;virtual;overload;
    Function CreateCoursesCourseWorkResource : TCoursesCourseWorkResource;virtual;overload;
    Function CreateCoursesResource(AOwner : TComponent) : TCoursesResource;virtual;overload;
    Function CreateCoursesResource : TCoursesResource;virtual;overload;
    Function CreateInvitationsResource(AOwner : TComponent) : TInvitationsResource;virtual;overload;
    Function CreateInvitationsResource : TInvitationsResource;virtual;overload;
    Function CreateUserProfilesResource(AOwner : TComponent) : TUserProfilesResource;virtual;overload;
    Function CreateUserProfilesResource : TUserProfilesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property CoursesAliasesResource : TCoursesAliasesResource Read GetCoursesAliasesInstance;
    Property CoursesTeachersResource : TCoursesTeachersResource Read GetCoursesTeachersInstance;
    Property CoursesStudentsResource : TCoursesStudentsResource Read GetCoursesStudentsInstance;
    Property CoursesCourseWorkStudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource Read GetCoursesCourseWorkStudentSubmissionsInstance;
    Property CoursesCourseWorkResource : TCoursesCourseWorkResource Read GetCoursesCourseWorkInstance;
    Property CoursesResource : TCoursesResource Read GetCoursesInstance;
    Property InvitationsResource : TInvitationsResource Read GetInvitationsInstance;
    Property UserProfilesResource : TUserProfilesResource Read GetUserProfilesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TCourse
  --------------------------------------------------------------------}


Procedure TCourse.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.Setsection(AIndex : Integer; const AValue : String); 

begin
  If (Fsection=AValue) then exit;
  Fsection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetdescriptionHeading(AIndex : Integer; const AValue : String); 

begin
  If (FdescriptionHeading=AValue) then exit;
  FdescriptionHeading:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.Setroom(AIndex : Integer; const AValue : String); 

begin
  If (Froom=AValue) then exit;
  Froom:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetownerId(AIndex : Integer; const AValue : String); 

begin
  If (FownerId=AValue) then exit;
  FownerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetcreationTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetupdateTime(AIndex : Integer; const AValue : String); 

begin
  If (FupdateTime=AValue) then exit;
  FupdateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetenrollmentCode(AIndex : Integer; const AValue : String); 

begin
  If (FenrollmentCode=AValue) then exit;
  FenrollmentCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetcourseState(AIndex : Integer; const AValue : String); 

begin
  If (FcourseState=AValue) then exit;
  FcourseState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetalternateLink(AIndex : Integer; const AValue : String); 

begin
  If (FalternateLink=AValue) then exit;
  FalternateLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetteacherGroupEmail(AIndex : Integer; const AValue : String); 

begin
  If (FteacherGroupEmail=AValue) then exit;
  FteacherGroupEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetcourseGroupEmail(AIndex : Integer; const AValue : String); 

begin
  If (FcourseGroupEmail=AValue) then exit;
  FcourseGroupEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetteacherFolder(AIndex : Integer; const AValue : TDriveFolder); 

begin
  If (FteacherFolder=AValue) then exit;
  FteacherFolder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourse.SetcourseMaterialSets(AIndex : Integer; const AValue : TCourseTypecourseMaterialSetsArray); 

begin
  If (FcourseMaterialSets=AValue) then exit;
  FcourseMaterialSets:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCourse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'coursematerialsets' : SetLength(FcourseMaterialSets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDriveFolder
  --------------------------------------------------------------------}


Procedure TDriveFolder.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDriveFolder.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDriveFolder.SetalternateLink(AIndex : Integer; const AValue : String); 

begin
  If (FalternateLink=AValue) then exit;
  FalternateLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCourseMaterialSet
  --------------------------------------------------------------------}


Procedure TCourseMaterialSet.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseMaterialSet.Setmaterials(AIndex : Integer; const AValue : TCourseMaterialSetTypematerialsArray); 

begin
  If (Fmaterials=AValue) then exit;
  Fmaterials:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCourseMaterialSet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'materials' : SetLength(Fmaterials,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCourseMaterial
  --------------------------------------------------------------------}


Procedure TCourseMaterial.SetdriveFile(AIndex : Integer; const AValue : TDriveFile); 

begin
  If (FdriveFile=AValue) then exit;
  FdriveFile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseMaterial.SetyouTubeVideo(AIndex : Integer; const AValue : TYouTubeVideo); 

begin
  If (FyouTubeVideo=AValue) then exit;
  FyouTubeVideo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseMaterial.Setlink(AIndex : Integer; const AValue : TLink); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseMaterial.Setform(AIndex : Integer; const AValue : TForm); 

begin
  If (Fform=AValue) then exit;
  Fform:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDriveFile
  --------------------------------------------------------------------}


Procedure TDriveFile.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDriveFile.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDriveFile.SetalternateLink(AIndex : Integer; const AValue : String); 

begin
  If (FalternateLink=AValue) then exit;
  FalternateLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDriveFile.SetthumbnailUrl(AIndex : Integer; const AValue : String); 

begin
  If (FthumbnailUrl=AValue) then exit;
  FthumbnailUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TYouTubeVideo
  --------------------------------------------------------------------}


Procedure TYouTubeVideo.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TYouTubeVideo.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TYouTubeVideo.SetalternateLink(AIndex : Integer; const AValue : String); 

begin
  If (FalternateLink=AValue) then exit;
  FalternateLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TYouTubeVideo.SetthumbnailUrl(AIndex : Integer; const AValue : String); 

begin
  If (FthumbnailUrl=AValue) then exit;
  FthumbnailUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLink
  --------------------------------------------------------------------}


Procedure TLink.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLink.SetthumbnailUrl(AIndex : Integer; const AValue : String); 

begin
  If (FthumbnailUrl=AValue) then exit;
  FthumbnailUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TForm
  --------------------------------------------------------------------}


Procedure TForm.SetformUrl(AIndex : Integer; const AValue : String); 

begin
  If (FformUrl=AValue) then exit;
  FformUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForm.SetresponseUrl(AIndex : Integer; const AValue : String); 

begin
  If (FresponseUrl=AValue) then exit;
  FresponseUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForm.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TForm.SetthumbnailUrl(AIndex : Integer; const AValue : String); 

begin
  If (FthumbnailUrl=AValue) then exit;
  FthumbnailUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListCoursesResponse
  --------------------------------------------------------------------}


Procedure TListCoursesResponse.Setcourses(AIndex : Integer; const AValue : TListCoursesResponseTypecoursesArray); 

begin
  If (Fcourses=AValue) then exit;
  Fcourses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCoursesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListCoursesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'courses' : SetLength(Fcourses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCourseAlias
  --------------------------------------------------------------------}


Procedure TCourseAlias.Setalias(AIndex : Integer; const AValue : String); 

begin
  If (Falias=AValue) then exit;
  Falias:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListCourseAliasesResponse
  --------------------------------------------------------------------}


Procedure TListCourseAliasesResponse.Setaliases(AIndex : Integer; const AValue : TListCourseAliasesResponseTypealiasesArray); 

begin
  If (Faliases=AValue) then exit;
  Faliases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCourseAliasesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListCourseAliasesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'aliases' : SetLength(Faliases,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInvitation
  --------------------------------------------------------------------}


Procedure TInvitation.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvitation.SetuserId(AIndex : Integer; const AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvitation.SetcourseId(AIndex : Integer; const AValue : String); 

begin
  If (FcourseId=AValue) then exit;
  FcourseId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInvitation.Setrole(AIndex : Integer; const AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListInvitationsResponse
  --------------------------------------------------------------------}


Procedure TListInvitationsResponse.Setinvitations(AIndex : Integer; const AValue : TListInvitationsResponseTypeinvitationsArray); 

begin
  If (Finvitations=AValue) then exit;
  Finvitations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListInvitationsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListInvitationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'invitations' : SetLength(Finvitations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUserProfile
  --------------------------------------------------------------------}


Procedure TUserProfile.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.Setname(AIndex : Integer; const AValue : TName); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.SetemailAddress(AIndex : Integer; const AValue : String); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.SetphotoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserProfile.Setpermissions(AIndex : Integer; const AValue : TUserProfileTypepermissionsArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUserProfile.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TName
  --------------------------------------------------------------------}


Procedure TName.SetgivenName(AIndex : Integer; const AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetfamilyName(AIndex : Integer; const AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetfullName(AIndex : Integer; const AValue : String); 

begin
  If (FfullName=AValue) then exit;
  FfullName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGlobalPermission
  --------------------------------------------------------------------}


Procedure TGlobalPermission.Setpermission(AIndex : Integer; const AValue : String); 

begin
  If (Fpermission=AValue) then exit;
  Fpermission:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTeacher
  --------------------------------------------------------------------}


Procedure TTeacher.SetcourseId(AIndex : Integer; const AValue : String); 

begin
  If (FcourseId=AValue) then exit;
  FcourseId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeacher.SetuserId(AIndex : Integer; const AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeacher.Setprofile(AIndex : Integer; const AValue : TUserProfile); 

begin
  If (Fprofile=AValue) then exit;
  Fprofile:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTeachersResponse
  --------------------------------------------------------------------}


Procedure TListTeachersResponse.Setteachers(AIndex : Integer; const AValue : TListTeachersResponseTypeteachersArray); 

begin
  If (Fteachers=AValue) then exit;
  Fteachers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTeachersResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListTeachersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'teachers' : SetLength(Fteachers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStudent
  --------------------------------------------------------------------}


Procedure TStudent.SetcourseId(AIndex : Integer; const AValue : String); 

begin
  If (FcourseId=AValue) then exit;
  FcourseId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudent.SetuserId(AIndex : Integer; const AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudent.Setprofile(AIndex : Integer; const AValue : TUserProfile); 

begin
  If (Fprofile=AValue) then exit;
  Fprofile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudent.SetstudentWorkFolder(AIndex : Integer; const AValue : TDriveFolder); 

begin
  If (FstudentWorkFolder=AValue) then exit;
  FstudentWorkFolder:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListStudentsResponse
  --------------------------------------------------------------------}


Procedure TListStudentsResponse.Setstudents(AIndex : Integer; const AValue : TListStudentsResponseTypestudentsArray); 

begin
  If (Fstudents=AValue) then exit;
  Fstudents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListStudentsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListStudentsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'students' : SetLength(Fstudents,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCourseWork
  --------------------------------------------------------------------}


Procedure TCourseWork.SetcourseId(AIndex : Integer; const AValue : String); 

begin
  If (FcourseId=AValue) then exit;
  FcourseId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.Setmaterials(AIndex : Integer; const AValue : TCourseWorkTypematerialsArray); 

begin
  If (Fmaterials=AValue) then exit;
  Fmaterials:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetalternateLink(AIndex : Integer; const AValue : String); 

begin
  If (FalternateLink=AValue) then exit;
  FalternateLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetcreationTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetupdateTime(AIndex : Integer; const AValue : String); 

begin
  If (FupdateTime=AValue) then exit;
  FupdateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetdueDate(AIndex : Integer; const AValue : TDate); 

begin
  If (FdueDate=AValue) then exit;
  FdueDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetdueTime(AIndex : Integer; const AValue : TTimeOfDay); 

begin
  If (FdueTime=AValue) then exit;
  FdueTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetmaxPoints(AIndex : Integer; const AValue : double); 

begin
  If (FmaxPoints=AValue) then exit;
  FmaxPoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetworkType(AIndex : Integer; const AValue : String); 

begin
  If (FworkType=AValue) then exit;
  FworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetassociatedWithDeveloper(AIndex : Integer; const AValue : boolean); 

begin
  If (FassociatedWithDeveloper=AValue) then exit;
  FassociatedWithDeveloper:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetsubmissionModificationMode(AIndex : Integer; const AValue : String); 

begin
  If (FsubmissionModificationMode=AValue) then exit;
  FsubmissionModificationMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.Setassignment(AIndex : Integer; const AValue : TAssignment); 

begin
  If (Fassignment=AValue) then exit;
  Fassignment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCourseWork.SetmultipleChoiceQuestion(AIndex : Integer; const AValue : TMultipleChoiceQuestion); 

begin
  If (FmultipleChoiceQuestion=AValue) then exit;
  FmultipleChoiceQuestion:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCourseWork.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'materials' : SetLength(Fmaterials,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMaterial
  --------------------------------------------------------------------}


Procedure TMaterial.SetdriveFile(AIndex : Integer; const AValue : TSharedDriveFile); 

begin
  If (FdriveFile=AValue) then exit;
  FdriveFile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMaterial.SetyoutubeVideo(AIndex : Integer; const AValue : TYouTubeVideo); 

begin
  If (FyoutubeVideo=AValue) then exit;
  FyoutubeVideo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMaterial.Setlink(AIndex : Integer; const AValue : TLink); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMaterial.Setform(AIndex : Integer; const AValue : TForm); 

begin
  If (Fform=AValue) then exit;
  Fform:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSharedDriveFile
  --------------------------------------------------------------------}


Procedure TSharedDriveFile.SetdriveFile(AIndex : Integer; const AValue : TDriveFile); 

begin
  If (FdriveFile=AValue) then exit;
  FdriveFile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSharedDriveFile.SetshareMode(AIndex : Integer; const AValue : String); 

begin
  If (FshareMode=AValue) then exit;
  FshareMode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDate
  --------------------------------------------------------------------}


Procedure TDate.Setyear(AIndex : Integer; const AValue : integer); 

begin
  If (Fyear=AValue) then exit;
  Fyear:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDate.Setmonth(AIndex : Integer; const AValue : integer); 

begin
  If (Fmonth=AValue) then exit;
  Fmonth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDate.Setday(AIndex : Integer; const AValue : integer); 

begin
  If (Fday=AValue) then exit;
  Fday:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeOfDay
  --------------------------------------------------------------------}


Procedure TTimeOfDay.Sethours(AIndex : Integer; const AValue : integer); 

begin
  If (Fhours=AValue) then exit;
  Fhours:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDay.Setminutes(AIndex : Integer; const AValue : integer); 

begin
  If (Fminutes=AValue) then exit;
  Fminutes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDay.Setseconds(AIndex : Integer; const AValue : integer); 

begin
  If (Fseconds=AValue) then exit;
  Fseconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDay.Setnanos(AIndex : Integer; const AValue : integer); 

begin
  If (Fnanos=AValue) then exit;
  Fnanos:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAssignment
  --------------------------------------------------------------------}


Procedure TAssignment.SetstudentWorkFolder(AIndex : Integer; const AValue : TDriveFolder); 

begin
  If (FstudentWorkFolder=AValue) then exit;
  FstudentWorkFolder:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMultipleChoiceQuestion
  --------------------------------------------------------------------}


Procedure TMultipleChoiceQuestion.Setchoices(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fchoices=AValue) then exit;
  Fchoices:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMultipleChoiceQuestion.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'choices' : SetLength(Fchoices,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListCourseWorkResponse
  --------------------------------------------------------------------}


Procedure TListCourseWorkResponse.SetcourseWork(AIndex : Integer; const AValue : TListCourseWorkResponseTypecourseWorkArray); 

begin
  If (FcourseWork=AValue) then exit;
  FcourseWork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCourseWorkResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListCourseWorkResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'coursework' : SetLength(FcourseWork,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStudentSubmission
  --------------------------------------------------------------------}


Procedure TStudentSubmission.SetcourseId(AIndex : Integer; const AValue : String); 

begin
  If (FcourseId=AValue) then exit;
  FcourseId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetcourseWorkId(AIndex : Integer; const AValue : String); 

begin
  If (FcourseWorkId=AValue) then exit;
  FcourseWorkId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetuserId(AIndex : Integer; const AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetcreationTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetupdateTime(AIndex : Integer; const AValue : String); 

begin
  If (FupdateTime=AValue) then exit;
  FupdateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.Setlate(AIndex : Integer; const AValue : boolean); 

begin
  If (Flate=AValue) then exit;
  Flate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetdraftGrade(AIndex : Integer; const AValue : double); 

begin
  If (FdraftGrade=AValue) then exit;
  FdraftGrade:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetassignedGrade(AIndex : Integer; const AValue : double); 

begin
  If (FassignedGrade=AValue) then exit;
  FassignedGrade:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetalternateLink(AIndex : Integer; const AValue : String); 

begin
  If (FalternateLink=AValue) then exit;
  FalternateLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetcourseWorkType(AIndex : Integer; const AValue : String); 

begin
  If (FcourseWorkType=AValue) then exit;
  FcourseWorkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetassociatedWithDeveloper(AIndex : Integer; const AValue : boolean); 

begin
  If (FassociatedWithDeveloper=AValue) then exit;
  FassociatedWithDeveloper:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetassignmentSubmission(AIndex : Integer; const AValue : TAssignmentSubmission); 

begin
  If (FassignmentSubmission=AValue) then exit;
  FassignmentSubmission:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetshortAnswerSubmission(AIndex : Integer; const AValue : TShortAnswerSubmission); 

begin
  If (FshortAnswerSubmission=AValue) then exit;
  FshortAnswerSubmission:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStudentSubmission.SetmultipleChoiceSubmission(AIndex : Integer; const AValue : TMultipleChoiceSubmission); 

begin
  If (FmultipleChoiceSubmission=AValue) then exit;
  FmultipleChoiceSubmission:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAssignmentSubmission
  --------------------------------------------------------------------}


Procedure TAssignmentSubmission.Setattachments(AIndex : Integer; const AValue : TAssignmentSubmissionTypeattachmentsArray); 

begin
  If (Fattachments=AValue) then exit;
  Fattachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAssignmentSubmission.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'attachments' : SetLength(Fattachments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAttachment
  --------------------------------------------------------------------}


Procedure TAttachment.SetdriveFile(AIndex : Integer; const AValue : TDriveFile); 

begin
  If (FdriveFile=AValue) then exit;
  FdriveFile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachment.SetyouTubeVideo(AIndex : Integer; const AValue : TYouTubeVideo); 

begin
  If (FyouTubeVideo=AValue) then exit;
  FyouTubeVideo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachment.Setlink(AIndex : Integer; const AValue : TLink); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachment.Setform(AIndex : Integer; const AValue : TForm); 

begin
  If (Fform=AValue) then exit;
  Fform:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TShortAnswerSubmission
  --------------------------------------------------------------------}


Procedure TShortAnswerSubmission.Setanswer(AIndex : Integer; const AValue : String); 

begin
  If (Fanswer=AValue) then exit;
  Fanswer:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMultipleChoiceSubmission
  --------------------------------------------------------------------}


Procedure TMultipleChoiceSubmission.Setanswer(AIndex : Integer; const AValue : String); 

begin
  If (Fanswer=AValue) then exit;
  Fanswer:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListStudentSubmissionsResponse
  --------------------------------------------------------------------}


Procedure TListStudentSubmissionsResponse.SetstudentSubmissions(AIndex : Integer; const AValue : TListStudentSubmissionsResponseTypestudentSubmissionsArray); 

begin
  If (FstudentSubmissions=AValue) then exit;
  FstudentSubmissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListStudentSubmissionsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListStudentSubmissionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'studentsubmissions' : SetLength(FstudentSubmissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTurnInStudentSubmissionRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReclaimStudentSubmissionRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReturnStudentSubmissionRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TModifyAttachmentsRequest
  --------------------------------------------------------------------}


Procedure TModifyAttachmentsRequest.SetaddAttachments(AIndex : Integer; const AValue : TModifyAttachmentsRequestTypeaddAttachmentsArray); 

begin
  If (FaddAttachments=AValue) then exit;
  FaddAttachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TModifyAttachmentsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'addattachments' : SetLength(FaddAttachments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCoursesAliasesResource
  --------------------------------------------------------------------}


Class Function TCoursesAliasesResource.ResourceName : String;

begin
  Result:='aliases';
end;

Class Function TCoursesAliasesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclassroomAPI;
end;

Function TCoursesAliasesResource.Create(courseId: string; aCourseAlias : TCourseAlias) : TCourseAlias;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses/{courseId}/aliases';
  _Methodid   = 'classroom.courses.aliases.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCourseAlias,TCourseAlias) as TCourseAlias;
end;

Function TCoursesAliasesResource.Delete(courseId: string; alias: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/courses/{courseId}/aliases/{alias}';
  _Methodid   = 'classroom.courses.aliases.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'alias',alias]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TCoursesAliasesResource.List(courseId: string; AQuery : string = '') : TListCourseAliasesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/aliases';
  _Methodid   = 'classroom.courses.aliases.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListCourseAliasesResponse) as TListCourseAliasesResponse;
end;


Function TCoursesAliasesResource.List(courseId: string; AQuery : TCoursesAliaseslistOptions) : TListCourseAliasesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(courseId,_Q);
end;



{ --------------------------------------------------------------------
  TCoursesTeachersResource
  --------------------------------------------------------------------}


Class Function TCoursesTeachersResource.ResourceName : String;

begin
  Result:='teachers';
end;

Class Function TCoursesTeachersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclassroomAPI;
end;

Function TCoursesTeachersResource.Create(courseId: string; aTeacher : TTeacher) : TTeacher;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses/{courseId}/teachers';
  _Methodid   = 'classroom.courses.teachers.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTeacher,TTeacher) as TTeacher;
end;

Function TCoursesTeachersResource.Get(courseId: string; userId: string) : TTeacher;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/teachers/{userId}';
  _Methodid   = 'classroom.courses.teachers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTeacher) as TTeacher;
end;

Function TCoursesTeachersResource.Delete(courseId: string; userId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/courses/{courseId}/teachers/{userId}';
  _Methodid   = 'classroom.courses.teachers.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TCoursesTeachersResource.List(courseId: string; AQuery : string = '') : TListTeachersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/teachers';
  _Methodid   = 'classroom.courses.teachers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListTeachersResponse) as TListTeachersResponse;
end;


Function TCoursesTeachersResource.List(courseId: string; AQuery : TCoursesTeacherslistOptions) : TListTeachersResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(courseId,_Q);
end;



{ --------------------------------------------------------------------
  TCoursesStudentsResource
  --------------------------------------------------------------------}


Class Function TCoursesStudentsResource.ResourceName : String;

begin
  Result:='students';
end;

Class Function TCoursesStudentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclassroomAPI;
end;

Function TCoursesStudentsResource.Create(courseId: string; aStudent : TStudent; AQuery : string = '') : TStudent;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses/{courseId}/students';
  _Methodid   = 'classroom.courses.students.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aStudent,TStudent) as TStudent;
end;


Function TCoursesStudentsResource.Create(courseId: string; aStudent : TStudent; AQuery : TCoursesStudentscreateOptions) : TStudent;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'enrollmentCode',AQuery.enrollmentCode);
  Result:=Create(courseId,aStudent,_Q);
end;

Function TCoursesStudentsResource.Get(courseId: string; userId: string) : TStudent;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/students/{userId}';
  _Methodid   = 'classroom.courses.students.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TStudent) as TStudent;
end;

Function TCoursesStudentsResource.Delete(courseId: string; userId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/courses/{courseId}/students/{userId}';
  _Methodid   = 'classroom.courses.students.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TCoursesStudentsResource.List(courseId: string; AQuery : string = '') : TListStudentsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/students';
  _Methodid   = 'classroom.courses.students.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListStudentsResponse) as TListStudentsResponse;
end;


Function TCoursesStudentsResource.List(courseId: string; AQuery : TCoursesStudentslistOptions) : TListStudentsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(courseId,_Q);
end;



{ --------------------------------------------------------------------
  TCoursesCourseWorkStudentSubmissionsResource
  --------------------------------------------------------------------}


Class Function TCoursesCourseWorkStudentSubmissionsResource.ResourceName : String;

begin
  Result:='studentSubmissions';
end;

Class Function TCoursesCourseWorkStudentSubmissionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclassroomAPI;
end;

Function TCoursesCourseWorkStudentSubmissionsResource.Get(courseId: string; courseWorkId: string; id: string) : TStudentSubmission;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/courseWork/{courseWorkId}/studentSubmissions/{id}';
  _Methodid   = 'classroom.courses.courseWork.studentSubmissions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'courseWorkId',courseWorkId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TStudentSubmission) as TStudentSubmission;
end;

Function TCoursesCourseWorkStudentSubmissionsResource.Patch(courseId: string; courseWorkId: string; id: string; aStudentSubmission : TStudentSubmission; AQuery : string = '') : TStudentSubmission;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/courses/{courseId}/courseWork/{courseWorkId}/studentSubmissions/{id}';
  _Methodid   = 'classroom.courses.courseWork.studentSubmissions.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'courseWorkId',courseWorkId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aStudentSubmission,TStudentSubmission) as TStudentSubmission;
end;


Function TCoursesCourseWorkStudentSubmissionsResource.Patch(courseId: string; courseWorkId: string; id: string; aStudentSubmission : TStudentSubmission; AQuery : TCoursesCourseWorkStudentSubmissionspatchOptions) : TStudentSubmission;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Patch(courseId,courseWorkId,id,aStudentSubmission,_Q);
end;

Function TCoursesCourseWorkStudentSubmissionsResource.List(courseId: string; courseWorkId: string; AQuery : string = '') : TListStudentSubmissionsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/courseWork/{courseWorkId}/studentSubmissions';
  _Methodid   = 'classroom.courses.courseWork.studentSubmissions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'courseWorkId',courseWorkId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListStudentSubmissionsResponse) as TListStudentSubmissionsResponse;
end;


Function TCoursesCourseWorkStudentSubmissionsResource.List(courseId: string; courseWorkId: string; AQuery : TCoursesCourseWorkStudentSubmissionslistOptions) : TListStudentSubmissionsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'userId',AQuery.userId);
  AddToQuery(_Q,'states',AQuery.states);
  AddToQuery(_Q,'late',AQuery.late);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(courseId,courseWorkId,_Q);
end;

Function TCoursesCourseWorkStudentSubmissionsResource.TurnIn(courseId: string; courseWorkId: string; id: string; aTurnInStudentSubmissionRequest : TTurnInStudentSubmissionRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses/{courseId}/courseWork/{courseWorkId}/studentSubmissions/{id}:turnIn';
  _Methodid   = 'classroom.courses.courseWork.studentSubmissions.turnIn';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'courseWorkId',courseWorkId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTurnInStudentSubmissionRequest,TEmpty) as TEmpty;
end;

Function TCoursesCourseWorkStudentSubmissionsResource.Reclaim(courseId: string; courseWorkId: string; id: string; aReclaimStudentSubmissionRequest : TReclaimStudentSubmissionRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses/{courseId}/courseWork/{courseWorkId}/studentSubmissions/{id}:reclaim';
  _Methodid   = 'classroom.courses.courseWork.studentSubmissions.reclaim';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'courseWorkId',courseWorkId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReclaimStudentSubmissionRequest,TEmpty) as TEmpty;
end;

Function TCoursesCourseWorkStudentSubmissionsResource.Return(courseId: string; courseWorkId: string; id: string; aReturnStudentSubmissionRequest : TReturnStudentSubmissionRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses/{courseId}/courseWork/{courseWorkId}/studentSubmissions/{id}:return';
  _Methodid   = 'classroom.courses.courseWork.studentSubmissions.return';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'courseWorkId',courseWorkId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReturnStudentSubmissionRequest,TEmpty) as TEmpty;
end;

Function TCoursesCourseWorkStudentSubmissionsResource.ModifyAttachments(courseId: string; courseWorkId: string; id: string; aModifyAttachmentsRequest : TModifyAttachmentsRequest) : TStudentSubmission;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses/{courseId}/courseWork/{courseWorkId}/studentSubmissions/{id}:modifyAttachments';
  _Methodid   = 'classroom.courses.courseWork.studentSubmissions.modifyAttachments';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'courseWorkId',courseWorkId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aModifyAttachmentsRequest,TStudentSubmission) as TStudentSubmission;
end;



{ --------------------------------------------------------------------
  TCoursesCourseWorkResource
  --------------------------------------------------------------------}


Class Function TCoursesCourseWorkResource.ResourceName : String;

begin
  Result:='courseWork';
end;

Class Function TCoursesCourseWorkResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclassroomAPI;
end;

Function TCoursesCourseWorkResource.Create(courseId: string; aCourseWork : TCourseWork) : TCourseWork;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses/{courseId}/courseWork';
  _Methodid   = 'classroom.courses.courseWork.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCourseWork,TCourseWork) as TCourseWork;
end;

Function TCoursesCourseWorkResource.Get(courseId: string; id: string) : TCourseWork;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/courseWork/{id}';
  _Methodid   = 'classroom.courses.courseWork.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCourseWork) as TCourseWork;
end;

Function TCoursesCourseWorkResource.List(courseId: string; AQuery : string = '') : TListCourseWorkResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{courseId}/courseWork';
  _Methodid   = 'classroom.courses.courseWork.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['courseId',courseId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListCourseWorkResponse) as TListCourseWorkResponse;
end;


Function TCoursesCourseWorkResource.List(courseId: string; AQuery : TCoursesCourseWorklistOptions) : TListCourseWorkResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'courseWorkStates',AQuery.courseWorkStates);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(courseId,_Q);
end;



Function TCoursesCourseWorkResource.GetStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;

begin
  if (FStudentSubmissionsInstance=Nil) then
    FStudentSubmissionsInstance:=CreateStudentSubmissionsResource;
  Result:=FStudentSubmissionsInstance;
end;

Function TCoursesCourseWorkResource.CreateStudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource;

begin
  Result:=CreateStudentSubmissionsResource(Self);
end;


Function TCoursesCourseWorkResource.CreateStudentSubmissionsResource(AOwner : TComponent) : TCoursesCourseWorkStudentSubmissionsResource;

begin
  Result:=TCoursesCourseWorkStudentSubmissionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TCoursesResource
  --------------------------------------------------------------------}


Class Function TCoursesResource.ResourceName : String;

begin
  Result:='courses';
end;

Class Function TCoursesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclassroomAPI;
end;

Function TCoursesResource.Create(aCourse : TCourse) : TCourse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/courses';
  _Methodid   = 'classroom.courses.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aCourse,TCourse) as TCourse;
end;

Function TCoursesResource.Get(id: string) : TCourse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses/{id}';
  _Methodid   = 'classroom.courses.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCourse) as TCourse;
end;

Function TCoursesResource.Update(id: string; aCourse : TCourse) : TCourse;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/courses/{id}';
  _Methodid   = 'classroom.courses.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCourse,TCourse) as TCourse;
end;

Function TCoursesResource.Patch(id: string; aCourse : TCourse; AQuery : string = '') : TCourse;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/courses/{id}';
  _Methodid   = 'classroom.courses.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCourse,TCourse) as TCourse;
end;


Function TCoursesResource.Patch(id: string; aCourse : TCourse; AQuery : TCoursespatchOptions) : TCourse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Patch(id,aCourse,_Q);
end;

Function TCoursesResource.Delete(id: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/courses/{id}';
  _Methodid   = 'classroom.courses.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TCoursesResource.List(AQuery : string = '') : TListCoursesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/courses';
  _Methodid   = 'classroom.courses.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListCoursesResponse) as TListCoursesResponse;
end;


Function TCoursesResource.List(AQuery : TCourseslistOptions) : TListCoursesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'studentId',AQuery.studentId);
  AddToQuery(_Q,'teacherId',AQuery.teacherId);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



Function TCoursesResource.GetAliasesInstance : TCoursesAliasesResource;

begin
  if (FAliasesInstance=Nil) then
    FAliasesInstance:=CreateAliasesResource;
  Result:=FAliasesInstance;
end;

Function TCoursesResource.CreateAliasesResource : TCoursesAliasesResource;

begin
  Result:=CreateAliasesResource(Self);
end;


Function TCoursesResource.CreateAliasesResource(AOwner : TComponent) : TCoursesAliasesResource;

begin
  Result:=TCoursesAliasesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCoursesResource.GetTeachersInstance : TCoursesTeachersResource;

begin
  if (FTeachersInstance=Nil) then
    FTeachersInstance:=CreateTeachersResource;
  Result:=FTeachersInstance;
end;

Function TCoursesResource.CreateTeachersResource : TCoursesTeachersResource;

begin
  Result:=CreateTeachersResource(Self);
end;


Function TCoursesResource.CreateTeachersResource(AOwner : TComponent) : TCoursesTeachersResource;

begin
  Result:=TCoursesTeachersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCoursesResource.GetStudentsInstance : TCoursesStudentsResource;

begin
  if (FStudentsInstance=Nil) then
    FStudentsInstance:=CreateStudentsResource;
  Result:=FStudentsInstance;
end;

Function TCoursesResource.CreateStudentsResource : TCoursesStudentsResource;

begin
  Result:=CreateStudentsResource(Self);
end;


Function TCoursesResource.CreateStudentsResource(AOwner : TComponent) : TCoursesStudentsResource;

begin
  Result:=TCoursesStudentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCoursesResource.GetCourseWorkStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;

begin
  if (FCourseWorkStudentSubmissionsInstance=Nil) then
    FCourseWorkStudentSubmissionsInstance:=CreateCourseWorkStudentSubmissionsResource;
  Result:=FCourseWorkStudentSubmissionsInstance;
end;

Function TCoursesResource.CreateCourseWorkStudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource;

begin
  Result:=CreateCourseWorkStudentSubmissionsResource(Self);
end;


Function TCoursesResource.CreateCourseWorkStudentSubmissionsResource(AOwner : TComponent) : TCoursesCourseWorkStudentSubmissionsResource;

begin
  Result:=TCoursesCourseWorkStudentSubmissionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCoursesResource.GetCourseWorkInstance : TCoursesCourseWorkResource;

begin
  if (FCourseWorkInstance=Nil) then
    FCourseWorkInstance:=CreateCourseWorkResource;
  Result:=FCourseWorkInstance;
end;

Function TCoursesResource.CreateCourseWorkResource : TCoursesCourseWorkResource;

begin
  Result:=CreateCourseWorkResource(Self);
end;


Function TCoursesResource.CreateCourseWorkResource(AOwner : TComponent) : TCoursesCourseWorkResource;

begin
  Result:=TCoursesCourseWorkResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TInvitationsResource
  --------------------------------------------------------------------}


Class Function TInvitationsResource.ResourceName : String;

begin
  Result:='invitations';
end;

Class Function TInvitationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclassroomAPI;
end;

Function TInvitationsResource.Create(aInvitation : TInvitation) : TInvitation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/invitations';
  _Methodid   = 'classroom.invitations.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aInvitation,TInvitation) as TInvitation;
end;

Function TInvitationsResource.Get(id: string) : TInvitation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/invitations/{id}';
  _Methodid   = 'classroom.invitations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInvitation) as TInvitation;
end;

Function TInvitationsResource.Delete(id: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/invitations/{id}';
  _Methodid   = 'classroom.invitations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TInvitationsResource.List(AQuery : string = '') : TListInvitationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/invitations';
  _Methodid   = 'classroom.invitations.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListInvitationsResponse) as TListInvitationsResponse;
end;


Function TInvitationsResource.List(AQuery : TInvitationslistOptions) : TListInvitationsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'userId',AQuery.userId);
  AddToQuery(_Q,'courseId',AQuery.courseId);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;

Function TInvitationsResource.Accept(id: string) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/invitations/{id}:accept';
  _Methodid   = 'classroom.invitations.accept';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TUserProfilesResource
  --------------------------------------------------------------------}


Class Function TUserProfilesResource.ResourceName : String;

begin
  Result:='userProfiles';
end;

Class Function TUserProfilesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclassroomAPI;
end;

Function TUserProfilesResource.Get(userId: string) : TUserProfile;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/userProfiles/{userId}';
  _Methodid   = 'classroom.userProfiles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUserProfile) as TUserProfile;
end;



{ --------------------------------------------------------------------
  TClassroomAPI
  --------------------------------------------------------------------}

Class Function TClassroomAPI.APIName : String;

begin
  Result:='classroom';
end;

Class Function TClassroomAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TClassroomAPI.APIRevision : String;

begin
  Result:='20160517';
end;

Class Function TClassroomAPI.APIID : String;

begin
  Result:='classroom:v1';
end;

Class Function TClassroomAPI.APITitle : String;

begin
  Result:='Google Classroom API';
end;

Class Function TClassroomAPI.APIDescription : String;

begin
  Result:='Manages classes, rosters, and invitations in Google Classroom.';
end;

Class Function TClassroomAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TClassroomAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TClassroomAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TClassroomAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TClassroomAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/classroom/';
end;

Class Function TClassroomAPI.APIrootUrl : string;

begin
  Result:='https://classroom.googleapis.com/';
end;

Class Function TClassroomAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TClassroomAPI.APIbaseURL : String;

begin
  Result:='https://classroom.googleapis.com/';
end;

Class Function TClassroomAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TClassroomAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TClassroomAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TClassroomAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,13);
  Result[0].Name:='https://www.googleapis.com/auth/classroom.course-work.readonly';
  Result[0].Description:='View instructions for teacher-assigned work in your Google Classroom classes';
  Result[1].Name:='https://www.googleapis.com/auth/classroom.courses';
  Result[1].Description:='Manage your Google Classroom classes';
  Result[2].Name:='https://www.googleapis.com/auth/classroom.courses.readonly';
  Result[2].Description:='View your Google Classroom classes';
  Result[3].Name:='https://www.googleapis.com/auth/classroom.coursework.me';
  Result[3].Description:='Manage your course work and view your grades in Google Classroom';
  Result[4].Name:='https://www.googleapis.com/auth/classroom.coursework.me.readonly';
  Result[4].Description:='View your course work and grades in Google Classroom';
  Result[5].Name:='https://www.googleapis.com/auth/classroom.coursework.students';
  Result[5].Description:='Manage course work and grades for students in the Google Classroom classes you teach and view the course work and grades for classes you administer';
  Result[6].Name:='https://www.googleapis.com/auth/classroom.coursework.students.readonly';
  Result[6].Description:='View course work and grades for students in the Google Classroom classes you teach or administer';
  Result[7].Name:='https://www.googleapis.com/auth/classroom.profile.emails';
  Result[7].Description:='View the email addresses of people in your classes';
  Result[8].Name:='https://www.googleapis.com/auth/classroom.profile.photos';
  Result[8].Description:='View the profile photos of people in your classes';
  Result[9].Name:='https://www.googleapis.com/auth/classroom.rosters';
  Result[9].Description:='Manage your Google Classroom class rosters';
  Result[10].Name:='https://www.googleapis.com/auth/classroom.rosters.readonly';
  Result[10].Description:='View your Google Classroom class rosters';
  Result[11].Name:='https://www.googleapis.com/auth/classroom.student-submissions.me.readonly';
  Result[11].Description:='View your course work and grades in Google Classroom';
  Result[12].Name:='https://www.googleapis.com/auth/classroom.student-submissions.students.readonly';
  Result[12].Description:='View course work and grades for students in the Google Classroom classes you teach or administer';
  
end;

Class Function TClassroomAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TClassroomAPI.RegisterAPIResources;

begin
  TCourse.RegisterObject;
  TDriveFolder.RegisterObject;
  TCourseMaterialSet.RegisterObject;
  TCourseMaterial.RegisterObject;
  TDriveFile.RegisterObject;
  TYouTubeVideo.RegisterObject;
  TLink.RegisterObject;
  TForm.RegisterObject;
  TEmpty.RegisterObject;
  TListCoursesResponse.RegisterObject;
  TCourseAlias.RegisterObject;
  TListCourseAliasesResponse.RegisterObject;
  TInvitation.RegisterObject;
  TListInvitationsResponse.RegisterObject;
  TUserProfile.RegisterObject;
  TName.RegisterObject;
  TGlobalPermission.RegisterObject;
  TTeacher.RegisterObject;
  TListTeachersResponse.RegisterObject;
  TStudent.RegisterObject;
  TListStudentsResponse.RegisterObject;
  TCourseWork.RegisterObject;
  TMaterial.RegisterObject;
  TSharedDriveFile.RegisterObject;
  TDate.RegisterObject;
  TTimeOfDay.RegisterObject;
  TAssignment.RegisterObject;
  TMultipleChoiceQuestion.RegisterObject;
  TListCourseWorkResponse.RegisterObject;
  TStudentSubmission.RegisterObject;
  TAssignmentSubmission.RegisterObject;
  TAttachment.RegisterObject;
  TShortAnswerSubmission.RegisterObject;
  TMultipleChoiceSubmission.RegisterObject;
  TListStudentSubmissionsResponse.RegisterObject;
  TTurnInStudentSubmissionRequest.RegisterObject;
  TReclaimStudentSubmissionRequest.RegisterObject;
  TReturnStudentSubmissionRequest.RegisterObject;
  TModifyAttachmentsRequest.RegisterObject;
end;


Function TClassroomAPI.GetCoursesAliasesInstance : TCoursesAliasesResource;

begin
  if (FCoursesAliasesInstance=Nil) then
    FCoursesAliasesInstance:=CreateCoursesAliasesResource;
  Result:=FCoursesAliasesInstance;
end;

Function TClassroomAPI.CreateCoursesAliasesResource : TCoursesAliasesResource;

begin
  Result:=CreateCoursesAliasesResource(Self);
end;


Function TClassroomAPI.CreateCoursesAliasesResource(AOwner : TComponent) : TCoursesAliasesResource;

begin
  Result:=TCoursesAliasesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClassroomAPI.GetCoursesTeachersInstance : TCoursesTeachersResource;

begin
  if (FCoursesTeachersInstance=Nil) then
    FCoursesTeachersInstance:=CreateCoursesTeachersResource;
  Result:=FCoursesTeachersInstance;
end;

Function TClassroomAPI.CreateCoursesTeachersResource : TCoursesTeachersResource;

begin
  Result:=CreateCoursesTeachersResource(Self);
end;


Function TClassroomAPI.CreateCoursesTeachersResource(AOwner : TComponent) : TCoursesTeachersResource;

begin
  Result:=TCoursesTeachersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClassroomAPI.GetCoursesStudentsInstance : TCoursesStudentsResource;

begin
  if (FCoursesStudentsInstance=Nil) then
    FCoursesStudentsInstance:=CreateCoursesStudentsResource;
  Result:=FCoursesStudentsInstance;
end;

Function TClassroomAPI.CreateCoursesStudentsResource : TCoursesStudentsResource;

begin
  Result:=CreateCoursesStudentsResource(Self);
end;


Function TClassroomAPI.CreateCoursesStudentsResource(AOwner : TComponent) : TCoursesStudentsResource;

begin
  Result:=TCoursesStudentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClassroomAPI.GetCoursesCourseWorkStudentSubmissionsInstance : TCoursesCourseWorkStudentSubmissionsResource;

begin
  if (FCoursesCourseWorkStudentSubmissionsInstance=Nil) then
    FCoursesCourseWorkStudentSubmissionsInstance:=CreateCoursesCourseWorkStudentSubmissionsResource;
  Result:=FCoursesCourseWorkStudentSubmissionsInstance;
end;

Function TClassroomAPI.CreateCoursesCourseWorkStudentSubmissionsResource : TCoursesCourseWorkStudentSubmissionsResource;

begin
  Result:=CreateCoursesCourseWorkStudentSubmissionsResource(Self);
end;


Function TClassroomAPI.CreateCoursesCourseWorkStudentSubmissionsResource(AOwner : TComponent) : TCoursesCourseWorkStudentSubmissionsResource;

begin
  Result:=TCoursesCourseWorkStudentSubmissionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClassroomAPI.GetCoursesCourseWorkInstance : TCoursesCourseWorkResource;

begin
  if (FCoursesCourseWorkInstance=Nil) then
    FCoursesCourseWorkInstance:=CreateCoursesCourseWorkResource;
  Result:=FCoursesCourseWorkInstance;
end;

Function TClassroomAPI.CreateCoursesCourseWorkResource : TCoursesCourseWorkResource;

begin
  Result:=CreateCoursesCourseWorkResource(Self);
end;


Function TClassroomAPI.CreateCoursesCourseWorkResource(AOwner : TComponent) : TCoursesCourseWorkResource;

begin
  Result:=TCoursesCourseWorkResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClassroomAPI.GetCoursesInstance : TCoursesResource;

begin
  if (FCoursesInstance=Nil) then
    FCoursesInstance:=CreateCoursesResource;
  Result:=FCoursesInstance;
end;

Function TClassroomAPI.CreateCoursesResource : TCoursesResource;

begin
  Result:=CreateCoursesResource(Self);
end;


Function TClassroomAPI.CreateCoursesResource(AOwner : TComponent) : TCoursesResource;

begin
  Result:=TCoursesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClassroomAPI.GetInvitationsInstance : TInvitationsResource;

begin
  if (FInvitationsInstance=Nil) then
    FInvitationsInstance:=CreateInvitationsResource;
  Result:=FInvitationsInstance;
end;

Function TClassroomAPI.CreateInvitationsResource : TInvitationsResource;

begin
  Result:=CreateInvitationsResource(Self);
end;


Function TClassroomAPI.CreateInvitationsResource(AOwner : TComponent) : TInvitationsResource;

begin
  Result:=TInvitationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClassroomAPI.GetUserProfilesInstance : TUserProfilesResource;

begin
  if (FUserProfilesInstance=Nil) then
    FUserProfilesInstance:=CreateUserProfilesResource;
  Result:=FUserProfilesInstance;
end;

Function TClassroomAPI.CreateUserProfilesResource : TUserProfilesResource;

begin
  Result:=CreateUserProfilesResource(Self);
end;


Function TClassroomAPI.CreateUserProfilesResource(AOwner : TComponent) : TUserProfilesResource;

begin
  Result:=TUserProfilesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TClassroomAPI.RegisterAPI;
end.
