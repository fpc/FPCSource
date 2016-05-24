unit googlecivicinfo;
{
  This is the file COPYING.FPC, it applies to the Free Pascal Run-Time Library 
  (RTL) and packages (packages) distributed by members of the Free Pascal 
  Development Team.
  
  The source code of the Free Pascal Runtime Libraries and packages are 
  distributed under the Library GNU General Public License 
  (see the file COPYING) with the following modification:
  
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,
  and to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the library, but you are
  not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.
  
  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
  
}
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  //
  TAdministrationRegion = class;
  TAdministrationRegionArray = Array of TAdministrationRegion;
  TAdministrationRegionsources = class;
  TAdministrationRegionsourcesArray = Array of TAdministrationRegionsources;
  TAdministrativeBody = class;
  TAdministrativeBodyArray = Array of TAdministrativeBody;
  TAdministrativeBodyelectionOfficials = class;
  TAdministrativeBodyelectionOfficialsArray = Array of TAdministrativeBodyelectionOfficials;
  TAdministrativeBodyvoter_services = class;
  TAdministrativeBodyvoter_servicesArray = Array of TAdministrativeBodyvoter_services;
  TCandidate = class;
  TCandidateArray = Array of TCandidate;
  TCandidatechannels = class;
  TCandidatechannelsArray = Array of TCandidatechannels;
  TChannel = class;
  TChannelArray = Array of TChannel;
  TContest = class;
  TContestArray = Array of TContest;
  TContestcandidates = class;
  TContestcandidatesArray = Array of TContestcandidates;
  TContestlevel = class;
  TContestlevelArray = Array of TContestlevel;
  TContestroles = class;
  TContestrolesArray = Array of TContestroles;
  TContestsources = class;
  TContestsourcesArray = Array of TContestsources;
  TDivisionSearchResponse = class;
  TDivisionSearchResponseArray = Array of TDivisionSearchResponse;
  TDivisionSearchResponseresults = class;
  TDivisionSearchResponseresultsArray = Array of TDivisionSearchResponseresults;
  TDivisionSearchResult = class;
  TDivisionSearchResultArray = Array of TDivisionSearchResult;
  TDivisionSearchResultaliases = class;
  TDivisionSearchResultaliasesArray = Array of TDivisionSearchResultaliases;
  TElection = class;
  TElectionArray = Array of TElection;
  TElectionOfficial = class;
  TElectionOfficialArray = Array of TElectionOfficial;
  TElectionsQueryResponse = class;
  TElectionsQueryResponseArray = Array of TElectionsQueryResponse;
  TElectionsQueryResponseelections = class;
  TElectionsQueryResponseelectionsArray = Array of TElectionsQueryResponseelections;
  TElectoralDistrict = class;
  TElectoralDistrictArray = Array of TElectoralDistrict;
  TGeographicDivision = class;
  TGeographicDivisionArray = Array of TGeographicDivision;
  TGeographicDivisionalsoKnownAs = class;
  TGeographicDivisionalsoKnownAsArray = Array of TGeographicDivisionalsoKnownAs;
  TGeographicDivisionofficeIndices = class;
  TGeographicDivisionofficeIndicesArray = Array of TGeographicDivisionofficeIndices;
  TOffice = class;
  TOfficeArray = Array of TOffice;
  TOfficelevels = class;
  TOfficelevelsArray = Array of TOfficelevels;
  TOfficeofficialIndices = class;
  TOfficeofficialIndicesArray = Array of TOfficeofficialIndices;
  TOfficeroles = class;
  TOfficerolesArray = Array of TOfficeroles;
  TOfficesources = class;
  TOfficesourcesArray = Array of TOfficesources;
  TOfficial = class;
  TOfficialArray = Array of TOfficial;
  TOfficialaddress = class;
  TOfficialaddressArray = Array of TOfficialaddress;
  TOfficialchannels = class;
  TOfficialchannelsArray = Array of TOfficialchannels;
  TOfficialemails = class;
  TOfficialemailsArray = Array of TOfficialemails;
  TOfficialphones = class;
  TOfficialphonesArray = Array of TOfficialphones;
  TOfficialurls = class;
  TOfficialurlsArray = Array of TOfficialurls;
  TPollingLocation = class;
  TPollingLocationArray = Array of TPollingLocation;
  TPollingLocationsources = class;
  TPollingLocationsourcesArray = Array of TPollingLocationsources;
  TRepresentativeInfoData = class;
  TRepresentativeInfoDataArray = Array of TRepresentativeInfoData;
  TRepresentativeInfoDatadivisions = class;
  TRepresentativeInfoDatadivisionsArray = Array of TRepresentativeInfoDatadivisions;
  TRepresentativeInfoDataoffices = class;
  TRepresentativeInfoDataofficesArray = Array of TRepresentativeInfoDataoffices;
  TRepresentativeInfoDataofficials = class;
  TRepresentativeInfoDataofficialsArray = Array of TRepresentativeInfoDataofficials;
  TRepresentativeInfoResponse = class;
  TRepresentativeInfoResponseArray = Array of TRepresentativeInfoResponse;
  TRepresentativeInfoResponsedivisions = class;
  TRepresentativeInfoResponsedivisionsArray = Array of TRepresentativeInfoResponsedivisions;
  TRepresentativeInfoResponseoffices = class;
  TRepresentativeInfoResponseofficesArray = Array of TRepresentativeInfoResponseoffices;
  TRepresentativeInfoResponseofficials = class;
  TRepresentativeInfoResponseofficialsArray = Array of TRepresentativeInfoResponseofficials;
  TSimpleAddressType = class;
  TSimpleAddressTypeArray = Array of TSimpleAddressType;
  TSource = class;
  TSourceArray = Array of TSource;
  TVoterInfoResponse = class;
  TVoterInfoResponseArray = Array of TVoterInfoResponse;
  TVoterInfoResponsecontests = class;
  TVoterInfoResponsecontestsArray = Array of TVoterInfoResponsecontests;
  TVoterInfoResponsedropOffLocations = class;
  TVoterInfoResponsedropOffLocationsArray = Array of TVoterInfoResponsedropOffLocations;
  TVoterInfoResponseearlyVoteSites = class;
  TVoterInfoResponseearlyVoteSitesArray = Array of TVoterInfoResponseearlyVoteSites;
  TVoterInfoResponseotherElections = class;
  TVoterInfoResponseotherElectionsArray = Array of TVoterInfoResponseotherElections;
  TVoterInfoResponsepollingLocations = class;
  TVoterInfoResponsepollingLocationsArray = Array of TVoterInfoResponsepollingLocations;
  TVoterInfoResponsestate = class;
  TVoterInfoResponsestateArray = Array of TVoterInfoResponsestate;
  
  { --------------------------------------------------------------------
    TAdministrationRegion
    --------------------------------------------------------------------}
  
  TAdministrationRegion = Class(TGoogleBaseObject)
  Private
    FelectionAdministrationBody : TAdministrativeBody;
    Fid : string;
    Flocal_jurisdiction : TAdministrationRegion;
    Fname : string;
    Fsources : TAdministrationRegionsources;
  Protected
    //Property setters
    Procedure SetelectionAdministrationBody(AIndex : Integer; AValue : TAdministrativeBody); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocal_jurisdiction(AIndex : Integer; AValue : TAdministrationRegion); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setsources(AIndex : Integer; AValue : TAdministrationRegionsources); virtual;
  Public
  Published
    Property electionAdministrationBody : TAdministrativeBody Index 0 Read FelectionAdministrationBody Write SetelectionAdministrationBody;
    Property id : string Index 8 Read Fid Write Setid;
    Property local_jurisdiction : TAdministrationRegion Index 16 Read Flocal_jurisdiction Write Setlocal_jurisdiction;
    Property name : string Index 24 Read Fname Write Setname;
    Property sources : TAdministrationRegionsources Index 32 Read Fsources Write Setsources;
  end;
  TAdministrationRegionClass = Class of TAdministrationRegion;
  
  { --------------------------------------------------------------------
    TAdministrationRegionsources
    --------------------------------------------------------------------}
  
  TAdministrationRegionsources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdministrationRegionsourcesClass = Class of TAdministrationRegionsources;
  
  { --------------------------------------------------------------------
    TAdministrativeBody
    --------------------------------------------------------------------}
  
  TAdministrativeBody = Class(TGoogleBaseObject)
  Private
    FabsenteeVotingInfoUrl : string;
    FballotInfoUrl : string;
    FcorrespondenceAddress : TSimpleAddressType;
    FelectionInfoUrl : string;
    FelectionOfficials : TAdministrativeBodyelectionOfficials;
    FelectionRegistrationConfirmationUrl : string;
    FelectionRegistrationUrl : string;
    FelectionRulesUrl : string;
    FhoursOfOperation : string;
    Fname : string;
    FphysicalAddress : TSimpleAddressType;
    Fvoter_services : TAdministrativeBodyvoter_services;
    FvotingLocationFinderUrl : string;
  Protected
    //Property setters
    Procedure SetabsenteeVotingInfoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetballotInfoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetcorrespondenceAddress(AIndex : Integer; AValue : TSimpleAddressType); virtual;
    Procedure SetelectionInfoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetelectionOfficials(AIndex : Integer; AValue : TAdministrativeBodyelectionOfficials); virtual;
    Procedure SetelectionRegistrationConfirmationUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetelectionRegistrationUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetelectionRulesUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SethoursOfOperation(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetphysicalAddress(AIndex : Integer; AValue : TSimpleAddressType); virtual;
    Procedure Setvoter_services(AIndex : Integer; AValue : TAdministrativeBodyvoter_services); virtual;
    Procedure SetvotingLocationFinderUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property absenteeVotingInfoUrl : string Index 0 Read FabsenteeVotingInfoUrl Write SetabsenteeVotingInfoUrl;
    Property ballotInfoUrl : string Index 8 Read FballotInfoUrl Write SetballotInfoUrl;
    Property correspondenceAddress : TSimpleAddressType Index 16 Read FcorrespondenceAddress Write SetcorrespondenceAddress;
    Property electionInfoUrl : string Index 24 Read FelectionInfoUrl Write SetelectionInfoUrl;
    Property electionOfficials : TAdministrativeBodyelectionOfficials Index 32 Read FelectionOfficials Write SetelectionOfficials;
    Property electionRegistrationConfirmationUrl : string Index 40 Read FelectionRegistrationConfirmationUrl Write SetelectionRegistrationConfirmationUrl;
    Property electionRegistrationUrl : string Index 48 Read FelectionRegistrationUrl Write SetelectionRegistrationUrl;
    Property electionRulesUrl : string Index 56 Read FelectionRulesUrl Write SetelectionRulesUrl;
    Property hoursOfOperation : string Index 64 Read FhoursOfOperation Write SethoursOfOperation;
    Property name : string Index 72 Read Fname Write Setname;
    Property physicalAddress : TSimpleAddressType Index 80 Read FphysicalAddress Write SetphysicalAddress;
    Property voter_services : TAdministrativeBodyvoter_services Index 88 Read Fvoter_services Write Setvoter_services;
    Property votingLocationFinderUrl : string Index 96 Read FvotingLocationFinderUrl Write SetvotingLocationFinderUrl;
  end;
  TAdministrativeBodyClass = Class of TAdministrativeBody;
  
  { --------------------------------------------------------------------
    TAdministrativeBodyelectionOfficials
    --------------------------------------------------------------------}
  
  TAdministrativeBodyelectionOfficials = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdministrativeBodyelectionOfficialsClass = Class of TAdministrativeBodyelectionOfficials;
  
  { --------------------------------------------------------------------
    TAdministrativeBodyvoter_services
    --------------------------------------------------------------------}
  
  TAdministrativeBodyvoter_services = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdministrativeBodyvoter_servicesClass = Class of TAdministrativeBodyvoter_services;
  
  { --------------------------------------------------------------------
    TCandidate
    --------------------------------------------------------------------}
  
  TCandidate = Class(TGoogleBaseObject)
  Private
    FcandidateUrl : string;
    Fchannels : TCandidatechannels;
    Femail : string;
    Fname : string;
    ForderOnBallot : string;
    Fparty : string;
    Fphone : string;
    FphotoUrl : string;
  Protected
    //Property setters
    Procedure SetcandidateUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setchannels(AIndex : Integer; AValue : TCandidatechannels); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetorderOnBallot(AIndex : Integer; AValue : string); virtual;
    Procedure Setparty(AIndex : Integer; AValue : string); virtual;
    Procedure Setphone(AIndex : Integer; AValue : string); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property candidateUrl : string Index 0 Read FcandidateUrl Write SetcandidateUrl;
    Property channels : TCandidatechannels Index 8 Read Fchannels Write Setchannels;
    Property email : string Index 16 Read Femail Write Setemail;
    Property name : string Index 24 Read Fname Write Setname;
    Property orderOnBallot : string Index 32 Read ForderOnBallot Write SetorderOnBallot;
    Property party : string Index 40 Read Fparty Write Setparty;
    Property phone : string Index 48 Read Fphone Write Setphone;
    Property photoUrl : string Index 56 Read FphotoUrl Write SetphotoUrl;
  end;
  TCandidateClass = Class of TCandidate;
  
  { --------------------------------------------------------------------
    TCandidatechannels
    --------------------------------------------------------------------}
  
  TCandidatechannels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCandidatechannelsClass = Class of TCandidatechannels;
  
  { --------------------------------------------------------------------
    TChannel
    --------------------------------------------------------------------}
  
  TChannel = Class(TGoogleBaseObject)
  Private
    Fid : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TChannelClass = Class of TChannel;
  
  { --------------------------------------------------------------------
    TContest
    --------------------------------------------------------------------}
  
  TContest = Class(TGoogleBaseObject)
  Private
    FballotPlacement : string;
    Fcandidates : TContestcandidates;
    Fdistrict : TElectoralDistrict;
    FelectorateSpecifications : string;
    Fid : string;
    Flevel : TContestlevel;
    FnumberElected : string;
    FnumberVotingFor : string;
    Foffice : string;
    FprimaryParty : string;
    FreferendumSubtitle : string;
    FreferendumTitle : string;
    FreferendumUrl : string;
    Froles : TContestroles;
    Fsources : TContestsources;
    Fspecial : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetballotPlacement(AIndex : Integer; AValue : string); virtual;
    Procedure Setcandidates(AIndex : Integer; AValue : TContestcandidates); virtual;
    Procedure Setdistrict(AIndex : Integer; AValue : TElectoralDistrict); virtual;
    Procedure SetelectorateSpecifications(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : TContestlevel); virtual;
    Procedure SetnumberElected(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumberVotingFor(AIndex : Integer; AValue : string); virtual;
    Procedure Setoffice(AIndex : Integer; AValue : string); virtual;
    Procedure SetprimaryParty(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferendumSubtitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferendumTitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferendumUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setroles(AIndex : Integer; AValue : TContestroles); virtual;
    Procedure Setsources(AIndex : Integer; AValue : TContestsources); virtual;
    Procedure Setspecial(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property ballotPlacement : string Index 0 Read FballotPlacement Write SetballotPlacement;
    Property candidates : TContestcandidates Index 8 Read Fcandidates Write Setcandidates;
    Property district : TElectoralDistrict Index 16 Read Fdistrict Write Setdistrict;
    Property electorateSpecifications : string Index 24 Read FelectorateSpecifications Write SetelectorateSpecifications;
    Property id : string Index 32 Read Fid Write Setid;
    Property level : TContestlevel Index 40 Read Flevel Write Setlevel;
    Property numberElected : string Index 48 Read FnumberElected Write SetnumberElected;
    Property numberVotingFor : string Index 56 Read FnumberVotingFor Write SetnumberVotingFor;
    Property office : string Index 64 Read Foffice Write Setoffice;
    Property primaryParty : string Index 72 Read FprimaryParty Write SetprimaryParty;
    Property referendumSubtitle : string Index 80 Read FreferendumSubtitle Write SetreferendumSubtitle;
    Property referendumTitle : string Index 88 Read FreferendumTitle Write SetreferendumTitle;
    Property referendumUrl : string Index 96 Read FreferendumUrl Write SetreferendumUrl;
    Property roles : TContestroles Index 104 Read Froles Write Setroles;
    Property sources : TContestsources Index 112 Read Fsources Write Setsources;
    Property special : string Index 120 Read Fspecial Write Setspecial;
    Property _type : string Index 128 Read F_type Write Set_type;
  end;
  TContestClass = Class of TContest;
  
  { --------------------------------------------------------------------
    TContestcandidates
    --------------------------------------------------------------------}
  
  TContestcandidates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContestcandidatesClass = Class of TContestcandidates;
  
  { --------------------------------------------------------------------
    TContestlevel
    --------------------------------------------------------------------}
  
  TContestlevel = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContestlevelClass = Class of TContestlevel;
  
  { --------------------------------------------------------------------
    TContestroles
    --------------------------------------------------------------------}
  
  TContestroles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContestrolesClass = Class of TContestroles;
  
  { --------------------------------------------------------------------
    TContestsources
    --------------------------------------------------------------------}
  
  TContestsources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContestsourcesClass = Class of TContestsources;
  
  { --------------------------------------------------------------------
    TDivisionSearchResponse
    --------------------------------------------------------------------}
  
  TDivisionSearchResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fresults : TDivisionSearchResponseresults;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setresults(AIndex : Integer; AValue : TDivisionSearchResponseresults); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property results : TDivisionSearchResponseresults Index 8 Read Fresults Write Setresults;
  end;
  TDivisionSearchResponseClass = Class of TDivisionSearchResponse;
  
  { --------------------------------------------------------------------
    TDivisionSearchResponseresults
    --------------------------------------------------------------------}
  
  TDivisionSearchResponseresults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDivisionSearchResponseresultsClass = Class of TDivisionSearchResponseresults;
  
  { --------------------------------------------------------------------
    TDivisionSearchResult
    --------------------------------------------------------------------}
  
  TDivisionSearchResult = Class(TGoogleBaseObject)
  Private
    Faliases : TDivisionSearchResultaliases;
    Fname : string;
    FocdId : string;
  Protected
    //Property setters
    Procedure Setaliases(AIndex : Integer; AValue : TDivisionSearchResultaliases); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetocdId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property aliases : TDivisionSearchResultaliases Index 0 Read Faliases Write Setaliases;
    Property name : string Index 8 Read Fname Write Setname;
    Property ocdId : string Index 16 Read FocdId Write SetocdId;
  end;
  TDivisionSearchResultClass = Class of TDivisionSearchResult;
  
  { --------------------------------------------------------------------
    TDivisionSearchResultaliases
    --------------------------------------------------------------------}
  
  TDivisionSearchResultaliases = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDivisionSearchResultaliasesClass = Class of TDivisionSearchResultaliases;
  
  { --------------------------------------------------------------------
    TElection
    --------------------------------------------------------------------}
  
  TElection = Class(TGoogleBaseObject)
  Private
    FelectionDay : string;
    Fid : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetelectionDay(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property electionDay : string Index 0 Read FelectionDay Write SetelectionDay;
    Property id : string Index 8 Read Fid Write Setid;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TElectionClass = Class of TElection;
  
  { --------------------------------------------------------------------
    TElectionOfficial
    --------------------------------------------------------------------}
  
  TElectionOfficial = Class(TGoogleBaseObject)
  Private
    FemailAddress : string;
    FfaxNumber : string;
    Fname : string;
    FofficePhoneNumber : string;
    Ftitle : string;
  Protected
    //Property setters
    Procedure SetemailAddress(AIndex : Integer; AValue : string); virtual;
    Procedure SetfaxNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetofficePhoneNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property emailAddress : string Index 0 Read FemailAddress Write SetemailAddress;
    Property faxNumber : string Index 8 Read FfaxNumber Write SetfaxNumber;
    Property name : string Index 16 Read Fname Write Setname;
    Property officePhoneNumber : string Index 24 Read FofficePhoneNumber Write SetofficePhoneNumber;
    Property title : string Index 32 Read Ftitle Write Settitle;
  end;
  TElectionOfficialClass = Class of TElectionOfficial;
  
  { --------------------------------------------------------------------
    TElectionsQueryResponse
    --------------------------------------------------------------------}
  
  TElectionsQueryResponse = Class(TGoogleBaseObject)
  Private
    Felections : TElectionsQueryResponseelections;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setelections(AIndex : Integer; AValue : TElectionsQueryResponseelections); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property elections : TElectionsQueryResponseelections Index 0 Read Felections Write Setelections;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TElectionsQueryResponseClass = Class of TElectionsQueryResponse;
  
  { --------------------------------------------------------------------
    TElectionsQueryResponseelections
    --------------------------------------------------------------------}
  
  TElectionsQueryResponseelections = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TElectionsQueryResponseelectionsClass = Class of TElectionsQueryResponseelections;
  
  { --------------------------------------------------------------------
    TElectoralDistrict
    --------------------------------------------------------------------}
  
  TElectoralDistrict = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fname : string;
    Fscope : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setscope(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property name : string Index 8 Read Fname Write Setname;
    Property scope : string Index 16 Read Fscope Write Setscope;
  end;
  TElectoralDistrictClass = Class of TElectoralDistrict;
  
  { --------------------------------------------------------------------
    TGeographicDivision
    --------------------------------------------------------------------}
  
  TGeographicDivision = Class(TGoogleBaseObject)
  Private
    FalsoKnownAs : TGeographicDivisionalsoKnownAs;
    Fname : string;
    FofficeIndices : TGeographicDivisionofficeIndices;
  Protected
    //Property setters
    Procedure SetalsoKnownAs(AIndex : Integer; AValue : TGeographicDivisionalsoKnownAs); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetofficeIndices(AIndex : Integer; AValue : TGeographicDivisionofficeIndices); virtual;
  Public
  Published
    Property alsoKnownAs : TGeographicDivisionalsoKnownAs Index 0 Read FalsoKnownAs Write SetalsoKnownAs;
    Property name : string Index 8 Read Fname Write Setname;
    Property officeIndices : TGeographicDivisionofficeIndices Index 16 Read FofficeIndices Write SetofficeIndices;
  end;
  TGeographicDivisionClass = Class of TGeographicDivision;
  
  { --------------------------------------------------------------------
    TGeographicDivisionalsoKnownAs
    --------------------------------------------------------------------}
  
  TGeographicDivisionalsoKnownAs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeographicDivisionalsoKnownAsClass = Class of TGeographicDivisionalsoKnownAs;
  
  { --------------------------------------------------------------------
    TGeographicDivisionofficeIndices
    --------------------------------------------------------------------}
  
  TGeographicDivisionofficeIndices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGeographicDivisionofficeIndicesClass = Class of TGeographicDivisionofficeIndices;
  
  { --------------------------------------------------------------------
    TOffice
    --------------------------------------------------------------------}
  
  TOffice = Class(TGoogleBaseObject)
  Private
    FdivisionId : string;
    Flevels : TOfficelevels;
    Fname : string;
    FofficialIndices : TOfficeofficialIndices;
    Froles : TOfficeroles;
    Fsources : TOfficesources;
  Protected
    //Property setters
    Procedure SetdivisionId(AIndex : Integer; AValue : string); virtual;
    Procedure Setlevels(AIndex : Integer; AValue : TOfficelevels); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetofficialIndices(AIndex : Integer; AValue : TOfficeofficialIndices); virtual;
    Procedure Setroles(AIndex : Integer; AValue : TOfficeroles); virtual;
    Procedure Setsources(AIndex : Integer; AValue : TOfficesources); virtual;
  Public
  Published
    Property divisionId : string Index 0 Read FdivisionId Write SetdivisionId;
    Property levels : TOfficelevels Index 8 Read Flevels Write Setlevels;
    Property name : string Index 16 Read Fname Write Setname;
    Property officialIndices : TOfficeofficialIndices Index 24 Read FofficialIndices Write SetofficialIndices;
    Property roles : TOfficeroles Index 32 Read Froles Write Setroles;
    Property sources : TOfficesources Index 40 Read Fsources Write Setsources;
  end;
  TOfficeClass = Class of TOffice;
  
  { --------------------------------------------------------------------
    TOfficelevels
    --------------------------------------------------------------------}
  
  TOfficelevels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficelevelsClass = Class of TOfficelevels;
  
  { --------------------------------------------------------------------
    TOfficeofficialIndices
    --------------------------------------------------------------------}
  
  TOfficeofficialIndices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficeofficialIndicesClass = Class of TOfficeofficialIndices;
  
  { --------------------------------------------------------------------
    TOfficeroles
    --------------------------------------------------------------------}
  
  TOfficeroles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficerolesClass = Class of TOfficeroles;
  
  { --------------------------------------------------------------------
    TOfficesources
    --------------------------------------------------------------------}
  
  TOfficesources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficesourcesClass = Class of TOfficesources;
  
  { --------------------------------------------------------------------
    TOfficial
    --------------------------------------------------------------------}
  
  TOfficial = Class(TGoogleBaseObject)
  Private
    Faddress : TOfficialaddress;
    Fchannels : TOfficialchannels;
    Femails : TOfficialemails;
    Fname : string;
    Fparty : string;
    Fphones : TOfficialphones;
    FphotoUrl : string;
    Furls : TOfficialurls;
  Protected
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : TOfficialaddress); virtual;
    Procedure Setchannels(AIndex : Integer; AValue : TOfficialchannels); virtual;
    Procedure Setemails(AIndex : Integer; AValue : TOfficialemails); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setparty(AIndex : Integer; AValue : string); virtual;
    Procedure Setphones(AIndex : Integer; AValue : TOfficialphones); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Seturls(AIndex : Integer; AValue : TOfficialurls); virtual;
  Public
  Published
    Property address : TOfficialaddress Index 0 Read Faddress Write Setaddress;
    Property channels : TOfficialchannels Index 8 Read Fchannels Write Setchannels;
    Property emails : TOfficialemails Index 16 Read Femails Write Setemails;
    Property name : string Index 24 Read Fname Write Setname;
    Property party : string Index 32 Read Fparty Write Setparty;
    Property phones : TOfficialphones Index 40 Read Fphones Write Setphones;
    Property photoUrl : string Index 48 Read FphotoUrl Write SetphotoUrl;
    Property urls : TOfficialurls Index 56 Read Furls Write Seturls;
  end;
  TOfficialClass = Class of TOfficial;
  
  { --------------------------------------------------------------------
    TOfficialaddress
    --------------------------------------------------------------------}
  
  TOfficialaddress = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficialaddressClass = Class of TOfficialaddress;
  
  { --------------------------------------------------------------------
    TOfficialchannels
    --------------------------------------------------------------------}
  
  TOfficialchannels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficialchannelsClass = Class of TOfficialchannels;
  
  { --------------------------------------------------------------------
    TOfficialemails
    --------------------------------------------------------------------}
  
  TOfficialemails = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficialemailsClass = Class of TOfficialemails;
  
  { --------------------------------------------------------------------
    TOfficialphones
    --------------------------------------------------------------------}
  
  TOfficialphones = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficialphonesClass = Class of TOfficialphones;
  
  { --------------------------------------------------------------------
    TOfficialurls
    --------------------------------------------------------------------}
  
  TOfficialurls = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOfficialurlsClass = Class of TOfficialurls;
  
  { --------------------------------------------------------------------
    TPollingLocation
    --------------------------------------------------------------------}
  
  TPollingLocation = Class(TGoogleBaseObject)
  Private
    Faddress : TSimpleAddressType;
    FendDate : string;
    Fid : string;
    Fname : string;
    Fnotes : string;
    FpollingHours : string;
    Fsources : TPollingLocationsources;
    FstartDate : string;
    FvoterServices : string;
  Protected
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : TSimpleAddressType); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure SetpollingHours(AIndex : Integer; AValue : string); virtual;
    Procedure Setsources(AIndex : Integer; AValue : TPollingLocationsources); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : string); virtual;
    Procedure SetvoterServices(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property address : TSimpleAddressType Index 0 Read Faddress Write Setaddress;
    Property endDate : string Index 8 Read FendDate Write SetendDate;
    Property id : string Index 16 Read Fid Write Setid;
    Property name : string Index 24 Read Fname Write Setname;
    Property notes : string Index 32 Read Fnotes Write Setnotes;
    Property pollingHours : string Index 40 Read FpollingHours Write SetpollingHours;
    Property sources : TPollingLocationsources Index 48 Read Fsources Write Setsources;
    Property startDate : string Index 56 Read FstartDate Write SetstartDate;
    Property voterServices : string Index 64 Read FvoterServices Write SetvoterServices;
  end;
  TPollingLocationClass = Class of TPollingLocation;
  
  { --------------------------------------------------------------------
    TPollingLocationsources
    --------------------------------------------------------------------}
  
  TPollingLocationsources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPollingLocationsourcesClass = Class of TPollingLocationsources;
  
  { --------------------------------------------------------------------
    TRepresentativeInfoData
    --------------------------------------------------------------------}
  
  TRepresentativeInfoData = Class(TGoogleBaseObject)
  Private
    Fdivisions : TRepresentativeInfoDatadivisions;
    Foffices : TRepresentativeInfoDataoffices;
    Fofficials : TRepresentativeInfoDataofficials;
  Protected
    //Property setters
    Procedure Setdivisions(AIndex : Integer; AValue : TRepresentativeInfoDatadivisions); virtual;
    Procedure Setoffices(AIndex : Integer; AValue : TRepresentativeInfoDataoffices); virtual;
    Procedure Setofficials(AIndex : Integer; AValue : TRepresentativeInfoDataofficials); virtual;
  Public
  Published
    Property divisions : TRepresentativeInfoDatadivisions Index 0 Read Fdivisions Write Setdivisions;
    Property offices : TRepresentativeInfoDataoffices Index 8 Read Foffices Write Setoffices;
    Property officials : TRepresentativeInfoDataofficials Index 16 Read Fofficials Write Setofficials;
  end;
  TRepresentativeInfoDataClass = Class of TRepresentativeInfoData;
  
  { --------------------------------------------------------------------
    TRepresentativeInfoDatadivisions
    --------------------------------------------------------------------}
  
  TRepresentativeInfoDatadivisions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRepresentativeInfoDatadivisionsClass = Class of TRepresentativeInfoDatadivisions;
  
  { --------------------------------------------------------------------
    TRepresentativeInfoDataoffices
    --------------------------------------------------------------------}
  
  TRepresentativeInfoDataoffices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRepresentativeInfoDataofficesClass = Class of TRepresentativeInfoDataoffices;
  
  { --------------------------------------------------------------------
    TRepresentativeInfoDataofficials
    --------------------------------------------------------------------}
  
  TRepresentativeInfoDataofficials = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRepresentativeInfoDataofficialsClass = Class of TRepresentativeInfoDataofficials;
  
  { --------------------------------------------------------------------
    TRepresentativeInfoResponse
    --------------------------------------------------------------------}
  
  TRepresentativeInfoResponse = Class(TGoogleBaseObject)
  Private
    Fdivisions : TRepresentativeInfoResponsedivisions;
    Fkind : string;
    FnormalizedInput : TSimpleAddressType;
    Foffices : TRepresentativeInfoResponseoffices;
    Fofficials : TRepresentativeInfoResponseofficials;
  Protected
    //Property setters
    Procedure Setdivisions(AIndex : Integer; AValue : TRepresentativeInfoResponsedivisions); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnormalizedInput(AIndex : Integer; AValue : TSimpleAddressType); virtual;
    Procedure Setoffices(AIndex : Integer; AValue : TRepresentativeInfoResponseoffices); virtual;
    Procedure Setofficials(AIndex : Integer; AValue : TRepresentativeInfoResponseofficials); virtual;
  Public
  Published
    Property divisions : TRepresentativeInfoResponsedivisions Index 0 Read Fdivisions Write Setdivisions;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property normalizedInput : TSimpleAddressType Index 16 Read FnormalizedInput Write SetnormalizedInput;
    Property offices : TRepresentativeInfoResponseoffices Index 24 Read Foffices Write Setoffices;
    Property officials : TRepresentativeInfoResponseofficials Index 32 Read Fofficials Write Setofficials;
  end;
  TRepresentativeInfoResponseClass = Class of TRepresentativeInfoResponse;
  
  { --------------------------------------------------------------------
    TRepresentativeInfoResponsedivisions
    --------------------------------------------------------------------}
  
  TRepresentativeInfoResponsedivisions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRepresentativeInfoResponsedivisionsClass = Class of TRepresentativeInfoResponsedivisions;
  
  { --------------------------------------------------------------------
    TRepresentativeInfoResponseoffices
    --------------------------------------------------------------------}
  
  TRepresentativeInfoResponseoffices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRepresentativeInfoResponseofficesClass = Class of TRepresentativeInfoResponseoffices;
  
  { --------------------------------------------------------------------
    TRepresentativeInfoResponseofficials
    --------------------------------------------------------------------}
  
  TRepresentativeInfoResponseofficials = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRepresentativeInfoResponseofficialsClass = Class of TRepresentativeInfoResponseofficials;
  
  { --------------------------------------------------------------------
    TSimpleAddressType
    --------------------------------------------------------------------}
  
  TSimpleAddressType = Class(TGoogleBaseObject)
  Private
    Fcity : string;
    Fline1 : string;
    Fline2 : string;
    Fline3 : string;
    FlocationName : string;
    Fstate : string;
    Fzip : string;
  Protected
    //Property setters
    Procedure Setcity(AIndex : Integer; AValue : string); virtual;
    Procedure Setline1(AIndex : Integer; AValue : string); virtual;
    Procedure Setline2(AIndex : Integer; AValue : string); virtual;
    Procedure Setline3(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocationName(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : string); virtual;
    Procedure Setzip(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property city : string Index 0 Read Fcity Write Setcity;
    Property line1 : string Index 8 Read Fline1 Write Setline1;
    Property line2 : string Index 16 Read Fline2 Write Setline2;
    Property line3 : string Index 24 Read Fline3 Write Setline3;
    Property locationName : string Index 32 Read FlocationName Write SetlocationName;
    Property state : string Index 40 Read Fstate Write Setstate;
    Property zip : string Index 48 Read Fzip Write Setzip;
  end;
  TSimpleAddressTypeClass = Class of TSimpleAddressType;
  
  { --------------------------------------------------------------------
    TSource
    --------------------------------------------------------------------}
  
  TSource = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fofficial : boolean;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setofficial(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property official : boolean Index 8 Read Fofficial Write Setofficial;
  end;
  TSourceClass = Class of TSource;
  
  { --------------------------------------------------------------------
    TVoterInfoResponse
    --------------------------------------------------------------------}
  
  TVoterInfoResponse = Class(TGoogleBaseObject)
  Private
    Fcontests : TVoterInfoResponsecontests;
    FdropOffLocations : TVoterInfoResponsedropOffLocations;
    FearlyVoteSites : TVoterInfoResponseearlyVoteSites;
    Felection : TElection;
    Fkind : string;
    FnormalizedInput : TSimpleAddressType;
    FotherElections : TVoterInfoResponseotherElections;
    FpollingLocations : TVoterInfoResponsepollingLocations;
    FprecinctId : string;
    Fstate : TVoterInfoResponsestate;
  Protected
    //Property setters
    Procedure Setcontests(AIndex : Integer; AValue : TVoterInfoResponsecontests); virtual;
    Procedure SetdropOffLocations(AIndex : Integer; AValue : TVoterInfoResponsedropOffLocations); virtual;
    Procedure SetearlyVoteSites(AIndex : Integer; AValue : TVoterInfoResponseearlyVoteSites); virtual;
    Procedure Setelection(AIndex : Integer; AValue : TElection); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnormalizedInput(AIndex : Integer; AValue : TSimpleAddressType); virtual;
    Procedure SetotherElections(AIndex : Integer; AValue : TVoterInfoResponseotherElections); virtual;
    Procedure SetpollingLocations(AIndex : Integer; AValue : TVoterInfoResponsepollingLocations); virtual;
    Procedure SetprecinctId(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : TVoterInfoResponsestate); virtual;
  Public
  Published
    Property contests : TVoterInfoResponsecontests Index 0 Read Fcontests Write Setcontests;
    Property dropOffLocations : TVoterInfoResponsedropOffLocations Index 8 Read FdropOffLocations Write SetdropOffLocations;
    Property earlyVoteSites : TVoterInfoResponseearlyVoteSites Index 16 Read FearlyVoteSites Write SetearlyVoteSites;
    Property election : TElection Index 24 Read Felection Write Setelection;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property normalizedInput : TSimpleAddressType Index 40 Read FnormalizedInput Write SetnormalizedInput;
    Property otherElections : TVoterInfoResponseotherElections Index 48 Read FotherElections Write SetotherElections;
    Property pollingLocations : TVoterInfoResponsepollingLocations Index 56 Read FpollingLocations Write SetpollingLocations;
    Property precinctId : string Index 64 Read FprecinctId Write SetprecinctId;
    Property state : TVoterInfoResponsestate Index 72 Read Fstate Write Setstate;
  end;
  TVoterInfoResponseClass = Class of TVoterInfoResponse;
  
  { --------------------------------------------------------------------
    TVoterInfoResponsecontests
    --------------------------------------------------------------------}
  
  TVoterInfoResponsecontests = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVoterInfoResponsecontestsClass = Class of TVoterInfoResponsecontests;
  
  { --------------------------------------------------------------------
    TVoterInfoResponsedropOffLocations
    --------------------------------------------------------------------}
  
  TVoterInfoResponsedropOffLocations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVoterInfoResponsedropOffLocationsClass = Class of TVoterInfoResponsedropOffLocations;
  
  { --------------------------------------------------------------------
    TVoterInfoResponseearlyVoteSites
    --------------------------------------------------------------------}
  
  TVoterInfoResponseearlyVoteSites = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVoterInfoResponseearlyVoteSitesClass = Class of TVoterInfoResponseearlyVoteSites;
  
  { --------------------------------------------------------------------
    TVoterInfoResponseotherElections
    --------------------------------------------------------------------}
  
  TVoterInfoResponseotherElections = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVoterInfoResponseotherElectionsClass = Class of TVoterInfoResponseotherElections;
  
  { --------------------------------------------------------------------
    TVoterInfoResponsepollingLocations
    --------------------------------------------------------------------}
  
  TVoterInfoResponsepollingLocations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVoterInfoResponsepollingLocationsClass = Class of TVoterInfoResponsepollingLocations;
  
  { --------------------------------------------------------------------
    TVoterInfoResponsestate
    --------------------------------------------------------------------}
  
  TVoterInfoResponsestate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVoterInfoResponsestateClass = Class of TVoterInfoResponsestate;
  
  { --------------------------------------------------------------------
    TDivisionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDivisionsResource, method Search
  
  TDivisionsSearchOptions = Record
    query : string;
  end;
  
  TDivisionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Search(AQuery : string  = '') : TDivisionSearchResponse;
    Function Search(AQuery : TDivisionssearchOptions) : TDivisionSearchResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TElectionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TElectionsResource, method VoterInfoQuery
  
  TElectionsVoterInfoQueryOptions = Record
    address : string;
    electionId : int64;
    officialOnly : boolean;
  end;
  
  TElectionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function ElectionQuery : TElectionsQueryResponse;
    Function VoterInfoQuery(AQuery : string  = '') : TVoterInfoResponse;
    Function VoterInfoQuery(AQuery : TElectionsvoterInfoQueryOptions) : TVoterInfoResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TRepresentativesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRepresentativesResource, method RepresentativeInfoByAddress
  
  TRepresentativesRepresentativeInfoByAddressOptions = Record
    address : string;
    includeOffices : boolean;
    levels : string;
    roles : string;
  end;
  
  
  //Optional query Options for TRepresentativesResource, method RepresentativeInfoByDivision
  
  TRepresentativesRepresentativeInfoByDivisionOptions = Record
    levels : string;
    recursive : boolean;
    roles : string;
  end;
  
  TRepresentativesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function RepresentativeInfoByAddress(AQuery : string  = '') : TRepresentativeInfoResponse;
    Function RepresentativeInfoByAddress(AQuery : TRepresentativesrepresentativeInfoByAddressOptions) : TRepresentativeInfoResponse;
    Function RepresentativeInfoByDivision(ocdId: string; AQuery : string  = '') : TRepresentativeInfoData;
    Function RepresentativeInfoByDivision(ocdId: string; AQuery : TRepresentativesrepresentativeInfoByDivisionOptions) : TRepresentativeInfoData;
  end;
  
  
  { --------------------------------------------------------------------
    TCivicinfoAPI
    --------------------------------------------------------------------}
  
  TCivicinfoAPI = Class(TGoogleAPI)
  Private
    FDivisionsInstance : TDivisionsResource;
    FElectionsInstance : TElectionsResource;
    FRepresentativesInstance : TRepresentativesResource;
    Function GetDivisionsInstance : TDivisionsResource;virtual;
    Function GetElectionsInstance : TElectionsResource;virtual;
    Function GetRepresentativesInstance : TRepresentativesResource;virtual;
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
    Function CreateDivisionsResource(AOwner : TComponent) : TDivisionsResource;virtual;overload;
    Function CreateDivisionsResource : TDivisionsResource;virtual;overload;
    Function CreateElectionsResource(AOwner : TComponent) : TElectionsResource;virtual;overload;
    Function CreateElectionsResource : TElectionsResource;virtual;overload;
    Function CreateRepresentativesResource(AOwner : TComponent) : TRepresentativesResource;virtual;overload;
    Function CreateRepresentativesResource : TRepresentativesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property DivisionsResource : TDivisionsResource Read GetDivisionsInstance;
    Property ElectionsResource : TElectionsResource Read GetElectionsInstance;
    Property RepresentativesResource : TRepresentativesResource Read GetRepresentativesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAdministrationRegion
  --------------------------------------------------------------------}


Procedure TAdministrationRegion.SetelectionAdministrationBody(AIndex : Integer; AValue : TAdministrativeBody); 

begin
  If (FelectionAdministrationBody=AValue) then exit;
  FelectionAdministrationBody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrationRegion.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrationRegion.Setlocal_jurisdiction(AIndex : Integer; AValue : TAdministrationRegion); 

begin
  If (Flocal_jurisdiction=AValue) then exit;
  Flocal_jurisdiction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrationRegion.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrationRegion.Setsources(AIndex : Integer; AValue : TAdministrationRegionsources); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdministrationRegionsources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdministrativeBody
  --------------------------------------------------------------------}


Procedure TAdministrativeBody.SetabsenteeVotingInfoUrl(AIndex : Integer; AValue : string); 

begin
  If (FabsenteeVotingInfoUrl=AValue) then exit;
  FabsenteeVotingInfoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetballotInfoUrl(AIndex : Integer; AValue : string); 

begin
  If (FballotInfoUrl=AValue) then exit;
  FballotInfoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetcorrespondenceAddress(AIndex : Integer; AValue : TSimpleAddressType); 

begin
  If (FcorrespondenceAddress=AValue) then exit;
  FcorrespondenceAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetelectionInfoUrl(AIndex : Integer; AValue : string); 

begin
  If (FelectionInfoUrl=AValue) then exit;
  FelectionInfoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetelectionOfficials(AIndex : Integer; AValue : TAdministrativeBodyelectionOfficials); 

begin
  If (FelectionOfficials=AValue) then exit;
  FelectionOfficials:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetelectionRegistrationConfirmationUrl(AIndex : Integer; AValue : string); 

begin
  If (FelectionRegistrationConfirmationUrl=AValue) then exit;
  FelectionRegistrationConfirmationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetelectionRegistrationUrl(AIndex : Integer; AValue : string); 

begin
  If (FelectionRegistrationUrl=AValue) then exit;
  FelectionRegistrationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetelectionRulesUrl(AIndex : Integer; AValue : string); 

begin
  If (FelectionRulesUrl=AValue) then exit;
  FelectionRulesUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SethoursOfOperation(AIndex : Integer; AValue : string); 

begin
  If (FhoursOfOperation=AValue) then exit;
  FhoursOfOperation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetphysicalAddress(AIndex : Integer; AValue : TSimpleAddressType); 

begin
  If (FphysicalAddress=AValue) then exit;
  FphysicalAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.Setvoter_services(AIndex : Integer; AValue : TAdministrativeBodyvoter_services); 

begin
  If (Fvoter_services=AValue) then exit;
  Fvoter_services:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdministrativeBody.SetvotingLocationFinderUrl(AIndex : Integer; AValue : string); 

begin
  If (FvotingLocationFinderUrl=AValue) then exit;
  FvotingLocationFinderUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdministrativeBodyelectionOfficials
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdministrativeBodyvoter_services
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCandidate
  --------------------------------------------------------------------}


Procedure TCandidate.SetcandidateUrl(AIndex : Integer; AValue : string); 

begin
  If (FcandidateUrl=AValue) then exit;
  FcandidateUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCandidate.Setchannels(AIndex : Integer; AValue : TCandidatechannels); 

begin
  If (Fchannels=AValue) then exit;
  Fchannels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCandidate.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCandidate.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCandidate.SetorderOnBallot(AIndex : Integer; AValue : string); 

begin
  If (ForderOnBallot=AValue) then exit;
  ForderOnBallot:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCandidate.Setparty(AIndex : Integer; AValue : string); 

begin
  If (Fparty=AValue) then exit;
  Fparty:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCandidate.Setphone(AIndex : Integer; AValue : string); 

begin
  If (Fphone=AValue) then exit;
  Fphone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCandidate.SetphotoUrl(AIndex : Integer; AValue : string); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCandidatechannels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TChannel
  --------------------------------------------------------------------}


Procedure TChannel.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TChannel.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TContest
  --------------------------------------------------------------------}


Procedure TContest.SetballotPlacement(AIndex : Integer; AValue : string); 

begin
  If (FballotPlacement=AValue) then exit;
  FballotPlacement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Setcandidates(AIndex : Integer; AValue : TContestcandidates); 

begin
  If (Fcandidates=AValue) then exit;
  Fcandidates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Setdistrict(AIndex : Integer; AValue : TElectoralDistrict); 

begin
  If (Fdistrict=AValue) then exit;
  Fdistrict:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.SetelectorateSpecifications(AIndex : Integer; AValue : string); 

begin
  If (FelectorateSpecifications=AValue) then exit;
  FelectorateSpecifications:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Setlevel(AIndex : Integer; AValue : TContestlevel); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.SetnumberElected(AIndex : Integer; AValue : string); 

begin
  If (FnumberElected=AValue) then exit;
  FnumberElected:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.SetnumberVotingFor(AIndex : Integer; AValue : string); 

begin
  If (FnumberVotingFor=AValue) then exit;
  FnumberVotingFor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Setoffice(AIndex : Integer; AValue : string); 

begin
  If (Foffice=AValue) then exit;
  Foffice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.SetprimaryParty(AIndex : Integer; AValue : string); 

begin
  If (FprimaryParty=AValue) then exit;
  FprimaryParty:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.SetreferendumSubtitle(AIndex : Integer; AValue : string); 

begin
  If (FreferendumSubtitle=AValue) then exit;
  FreferendumSubtitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.SetreferendumTitle(AIndex : Integer; AValue : string); 

begin
  If (FreferendumTitle=AValue) then exit;
  FreferendumTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.SetreferendumUrl(AIndex : Integer; AValue : string); 

begin
  If (FreferendumUrl=AValue) then exit;
  FreferendumUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Setroles(AIndex : Integer; AValue : TContestroles); 

begin
  If (Froles=AValue) then exit;
  Froles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Setsources(AIndex : Integer; AValue : TContestsources); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Setspecial(AIndex : Integer; AValue : string); 

begin
  If (Fspecial=AValue) then exit;
  Fspecial:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContest.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TContest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TContestcandidates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContestlevel
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContestroles
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContestsources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDivisionSearchResponse
  --------------------------------------------------------------------}


Procedure TDivisionSearchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDivisionSearchResponse.Setresults(AIndex : Integer; AValue : TDivisionSearchResponseresults); 

begin
  If (Fresults=AValue) then exit;
  Fresults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDivisionSearchResponseresults
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDivisionSearchResult
  --------------------------------------------------------------------}


Procedure TDivisionSearchResult.Setaliases(AIndex : Integer; AValue : TDivisionSearchResultaliases); 

begin
  If (Faliases=AValue) then exit;
  Faliases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDivisionSearchResult.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDivisionSearchResult.SetocdId(AIndex : Integer; AValue : string); 

begin
  If (FocdId=AValue) then exit;
  FocdId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDivisionSearchResultaliases
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TElection
  --------------------------------------------------------------------}


Procedure TElection.SetelectionDay(AIndex : Integer; AValue : string); 

begin
  If (FelectionDay=AValue) then exit;
  FelectionDay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElection.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElection.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TElectionOfficial
  --------------------------------------------------------------------}


Procedure TElectionOfficial.SetemailAddress(AIndex : Integer; AValue : string); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElectionOfficial.SetfaxNumber(AIndex : Integer; AValue : string); 

begin
  If (FfaxNumber=AValue) then exit;
  FfaxNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElectionOfficial.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElectionOfficial.SetofficePhoneNumber(AIndex : Integer; AValue : string); 

begin
  If (FofficePhoneNumber=AValue) then exit;
  FofficePhoneNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElectionOfficial.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TElectionsQueryResponse
  --------------------------------------------------------------------}


Procedure TElectionsQueryResponse.Setelections(AIndex : Integer; AValue : TElectionsQueryResponseelections); 

begin
  If (Felections=AValue) then exit;
  Felections:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElectionsQueryResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TElectionsQueryResponseelections
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TElectoralDistrict
  --------------------------------------------------------------------}


Procedure TElectoralDistrict.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElectoralDistrict.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TElectoralDistrict.Setscope(AIndex : Integer; AValue : string); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeographicDivision
  --------------------------------------------------------------------}


Procedure TGeographicDivision.SetalsoKnownAs(AIndex : Integer; AValue : TGeographicDivisionalsoKnownAs); 

begin
  If (FalsoKnownAs=AValue) then exit;
  FalsoKnownAs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeographicDivision.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeographicDivision.SetofficeIndices(AIndex : Integer; AValue : TGeographicDivisionofficeIndices); 

begin
  If (FofficeIndices=AValue) then exit;
  FofficeIndices:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeographicDivisionalsoKnownAs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGeographicDivisionofficeIndices
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOffice
  --------------------------------------------------------------------}


Procedure TOffice.SetdivisionId(AIndex : Integer; AValue : string); 

begin
  If (FdivisionId=AValue) then exit;
  FdivisionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffice.Setlevels(AIndex : Integer; AValue : TOfficelevels); 

begin
  If (Flevels=AValue) then exit;
  Flevels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffice.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffice.SetofficialIndices(AIndex : Integer; AValue : TOfficeofficialIndices); 

begin
  If (FofficialIndices=AValue) then exit;
  FofficialIndices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffice.Setroles(AIndex : Integer; AValue : TOfficeroles); 

begin
  If (Froles=AValue) then exit;
  Froles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOffice.Setsources(AIndex : Integer; AValue : TOfficesources); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOfficelevels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOfficeofficialIndices
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOfficeroles
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOfficesources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOfficial
  --------------------------------------------------------------------}


Procedure TOfficial.Setaddress(AIndex : Integer; AValue : TOfficialaddress); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOfficial.Setchannels(AIndex : Integer; AValue : TOfficialchannels); 

begin
  If (Fchannels=AValue) then exit;
  Fchannels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOfficial.Setemails(AIndex : Integer; AValue : TOfficialemails); 

begin
  If (Femails=AValue) then exit;
  Femails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOfficial.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOfficial.Setparty(AIndex : Integer; AValue : string); 

begin
  If (Fparty=AValue) then exit;
  Fparty:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOfficial.Setphones(AIndex : Integer; AValue : TOfficialphones); 

begin
  If (Fphones=AValue) then exit;
  Fphones:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOfficial.SetphotoUrl(AIndex : Integer; AValue : string); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOfficial.Seturls(AIndex : Integer; AValue : TOfficialurls); 

begin
  If (Furls=AValue) then exit;
  Furls:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOfficialaddress
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOfficialchannels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOfficialemails
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOfficialphones
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOfficialurls
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPollingLocation
  --------------------------------------------------------------------}


Procedure TPollingLocation.Setaddress(AIndex : Integer; AValue : TSimpleAddressType); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPollingLocation.SetendDate(AIndex : Integer; AValue : string); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPollingLocation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPollingLocation.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPollingLocation.Setnotes(AIndex : Integer; AValue : string); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPollingLocation.SetpollingHours(AIndex : Integer; AValue : string); 

begin
  If (FpollingHours=AValue) then exit;
  FpollingHours:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPollingLocation.Setsources(AIndex : Integer; AValue : TPollingLocationsources); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPollingLocation.SetstartDate(AIndex : Integer; AValue : string); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPollingLocation.SetvoterServices(AIndex : Integer; AValue : string); 

begin
  If (FvoterServices=AValue) then exit;
  FvoterServices:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPollingLocationsources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRepresentativeInfoData
  --------------------------------------------------------------------}


Procedure TRepresentativeInfoData.Setdivisions(AIndex : Integer; AValue : TRepresentativeInfoDatadivisions); 

begin
  If (Fdivisions=AValue) then exit;
  Fdivisions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepresentativeInfoData.Setoffices(AIndex : Integer; AValue : TRepresentativeInfoDataoffices); 

begin
  If (Foffices=AValue) then exit;
  Foffices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepresentativeInfoData.Setofficials(AIndex : Integer; AValue : TRepresentativeInfoDataofficials); 

begin
  If (Fofficials=AValue) then exit;
  Fofficials:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRepresentativeInfoDatadivisions
  --------------------------------------------------------------------}


Class Function TRepresentativeInfoDatadivisions.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRepresentativeInfoDataoffices
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRepresentativeInfoDataofficials
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRepresentativeInfoResponse
  --------------------------------------------------------------------}


Procedure TRepresentativeInfoResponse.Setdivisions(AIndex : Integer; AValue : TRepresentativeInfoResponsedivisions); 

begin
  If (Fdivisions=AValue) then exit;
  Fdivisions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepresentativeInfoResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepresentativeInfoResponse.SetnormalizedInput(AIndex : Integer; AValue : TSimpleAddressType); 

begin
  If (FnormalizedInput=AValue) then exit;
  FnormalizedInput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepresentativeInfoResponse.Setoffices(AIndex : Integer; AValue : TRepresentativeInfoResponseoffices); 

begin
  If (Foffices=AValue) then exit;
  Foffices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepresentativeInfoResponse.Setofficials(AIndex : Integer; AValue : TRepresentativeInfoResponseofficials); 

begin
  If (Fofficials=AValue) then exit;
  Fofficials:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRepresentativeInfoResponsedivisions
  --------------------------------------------------------------------}


Class Function TRepresentativeInfoResponsedivisions.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRepresentativeInfoResponseoffices
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRepresentativeInfoResponseofficials
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSimpleAddressType
  --------------------------------------------------------------------}


Procedure TSimpleAddressType.Setcity(AIndex : Integer; AValue : string); 

begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSimpleAddressType.Setline1(AIndex : Integer; AValue : string); 

begin
  If (Fline1=AValue) then exit;
  Fline1:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSimpleAddressType.Setline2(AIndex : Integer; AValue : string); 

begin
  If (Fline2=AValue) then exit;
  Fline2:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSimpleAddressType.Setline3(AIndex : Integer; AValue : string); 

begin
  If (Fline3=AValue) then exit;
  Fline3:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSimpleAddressType.SetlocationName(AIndex : Integer; AValue : string); 

begin
  If (FlocationName=AValue) then exit;
  FlocationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSimpleAddressType.Setstate(AIndex : Integer; AValue : string); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSimpleAddressType.Setzip(AIndex : Integer; AValue : string); 

begin
  If (Fzip=AValue) then exit;
  Fzip:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSource
  --------------------------------------------------------------------}


Procedure TSource.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.Setofficial(AIndex : Integer; AValue : boolean); 

begin
  If (Fofficial=AValue) then exit;
  Fofficial:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVoterInfoResponse
  --------------------------------------------------------------------}


Procedure TVoterInfoResponse.Setcontests(AIndex : Integer; AValue : TVoterInfoResponsecontests); 

begin
  If (Fcontests=AValue) then exit;
  Fcontests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.SetdropOffLocations(AIndex : Integer; AValue : TVoterInfoResponsedropOffLocations); 

begin
  If (FdropOffLocations=AValue) then exit;
  FdropOffLocations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.SetearlyVoteSites(AIndex : Integer; AValue : TVoterInfoResponseearlyVoteSites); 

begin
  If (FearlyVoteSites=AValue) then exit;
  FearlyVoteSites:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.Setelection(AIndex : Integer; AValue : TElection); 

begin
  If (Felection=AValue) then exit;
  Felection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.SetnormalizedInput(AIndex : Integer; AValue : TSimpleAddressType); 

begin
  If (FnormalizedInput=AValue) then exit;
  FnormalizedInput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.SetotherElections(AIndex : Integer; AValue : TVoterInfoResponseotherElections); 

begin
  If (FotherElections=AValue) then exit;
  FotherElections:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.SetpollingLocations(AIndex : Integer; AValue : TVoterInfoResponsepollingLocations); 

begin
  If (FpollingLocations=AValue) then exit;
  FpollingLocations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.SetprecinctId(AIndex : Integer; AValue : string); 

begin
  If (FprecinctId=AValue) then exit;
  FprecinctId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVoterInfoResponse.Setstate(AIndex : Integer; AValue : TVoterInfoResponsestate); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVoterInfoResponsecontests
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVoterInfoResponsedropOffLocations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVoterInfoResponseearlyVoteSites
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVoterInfoResponseotherElections
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVoterInfoResponsepollingLocations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVoterInfoResponsestate
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDivisionsResource
  --------------------------------------------------------------------}


Class Function TDivisionsResource.ResourceName : String;

begin
  Result:='divisions';
end;

Class Function TDivisionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcivicinfoAPI;
end;

Function TDivisionsResource.Search(AQuery : string = '') : TDivisionSearchResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'divisions';
  _Methodid   = 'civicinfo.divisions.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TDivisionSearchResponse) as TDivisionSearchResponse;
end;


Function TDivisionsResource.Search(AQuery : TDivisionssearchOptions) : TDivisionSearchResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'query',AQuery.query);
  Result:=Search(_Q);
end;



{ --------------------------------------------------------------------
  TElectionsResource
  --------------------------------------------------------------------}


Class Function TElectionsResource.ResourceName : String;

begin
  Result:='elections';
end;

Class Function TElectionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcivicinfoAPI;
end;

Function TElectionsResource.ElectionQuery : TElectionsQueryResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'elections';
  _Methodid   = 'civicinfo.elections.electionQuery';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TElectionsQueryResponse) as TElectionsQueryResponse;
end;

Function TElectionsResource.VoterInfoQuery(AQuery : string = '') : TVoterInfoResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'voterinfo';
  _Methodid   = 'civicinfo.elections.voterInfoQuery';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TVoterInfoResponse) as TVoterInfoResponse;
end;


Function TElectionsResource.VoterInfoQuery(AQuery : TElectionsvoterInfoQueryOptions) : TVoterInfoResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'address',AQuery.address);
  AddToQuery(_Q,'electionId',AQuery.electionId);
  AddToQuery(_Q,'officialOnly',AQuery.officialOnly);
  Result:=VoterInfoQuery(_Q);
end;



{ --------------------------------------------------------------------
  TRepresentativesResource
  --------------------------------------------------------------------}


Class Function TRepresentativesResource.ResourceName : String;

begin
  Result:='representatives';
end;

Class Function TRepresentativesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcivicinfoAPI;
end;

Function TRepresentativesResource.RepresentativeInfoByAddress(AQuery : string = '') : TRepresentativeInfoResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'representatives';
  _Methodid   = 'civicinfo.representatives.representativeInfoByAddress';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TRepresentativeInfoResponse) as TRepresentativeInfoResponse;
end;


Function TRepresentativesResource.RepresentativeInfoByAddress(AQuery : TRepresentativesrepresentativeInfoByAddressOptions) : TRepresentativeInfoResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'address',AQuery.address);
  AddToQuery(_Q,'includeOffices',AQuery.includeOffices);
  AddToQuery(_Q,'levels',AQuery.levels);
  AddToQuery(_Q,'roles',AQuery.roles);
  Result:=RepresentativeInfoByAddress(_Q);
end;

Function TRepresentativesResource.RepresentativeInfoByDivision(ocdId: string; AQuery : string = '') : TRepresentativeInfoData;

Const
  _HTTPMethod = 'GET';
  _Path       = 'representatives/{ocdId}';
  _Methodid   = 'civicinfo.representatives.representativeInfoByDivision';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['ocdId',ocdId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRepresentativeInfoData) as TRepresentativeInfoData;
end;


Function TRepresentativesResource.RepresentativeInfoByDivision(ocdId: string; AQuery : TRepresentativesrepresentativeInfoByDivisionOptions) : TRepresentativeInfoData;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'levels',AQuery.levels);
  AddToQuery(_Q,'recursive',AQuery.recursive);
  AddToQuery(_Q,'roles',AQuery.roles);
  Result:=RepresentativeInfoByDivision(ocdId,_Q);
end;



{ --------------------------------------------------------------------
  TCivicinfoAPI
  --------------------------------------------------------------------}

Class Function TCivicinfoAPI.APIName : String;

begin
  Result:='civicinfo';
end;

Class Function TCivicinfoAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TCivicinfoAPI.APIRevision : String;

begin
  Result:='20150302';
end;

Class Function TCivicinfoAPI.APIID : String;

begin
  Result:='civicinfo:v2';
end;

Class Function TCivicinfoAPI.APITitle : String;

begin
  Result:='Google Civic Information API';
end;

Class Function TCivicinfoAPI.APIDescription : String;

begin
  Result:='An API for accessing civic information.';
end;

Class Function TCivicinfoAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCivicinfoAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCivicinfoAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCivicinfoAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCivicinfoAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/civic-information';
end;

Class Function TCivicinfoAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TCivicinfoAPI.APIbasePath : string;

begin
  Result:='/civicinfo/v2/';
end;

Class Function TCivicinfoAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/civicinfo/v2/';
end;

Class Function TCivicinfoAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCivicinfoAPI.APIservicePath : string;

begin
  Result:='civicinfo/v2/';
end;

Class Function TCivicinfoAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCivicinfoAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TCivicinfoAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TCivicinfoAPI.RegisterAPIResources;

begin
  TAdministrationRegion.RegisterObject;
  TAdministrationRegionsources.RegisterObject;
  TAdministrativeBody.RegisterObject;
  TAdministrativeBodyelectionOfficials.RegisterObject;
  TAdministrativeBodyvoter_services.RegisterObject;
  TCandidate.RegisterObject;
  TCandidatechannels.RegisterObject;
  TChannel.RegisterObject;
  TContest.RegisterObject;
  TContestcandidates.RegisterObject;
  TContestlevel.RegisterObject;
  TContestroles.RegisterObject;
  TContestsources.RegisterObject;
  TDivisionSearchResponse.RegisterObject;
  TDivisionSearchResponseresults.RegisterObject;
  TDivisionSearchResult.RegisterObject;
  TDivisionSearchResultaliases.RegisterObject;
  TElection.RegisterObject;
  TElectionOfficial.RegisterObject;
  TElectionsQueryResponse.RegisterObject;
  TElectionsQueryResponseelections.RegisterObject;
  TElectoralDistrict.RegisterObject;
  TGeographicDivision.RegisterObject;
  TGeographicDivisionalsoKnownAs.RegisterObject;
  TGeographicDivisionofficeIndices.RegisterObject;
  TOffice.RegisterObject;
  TOfficelevels.RegisterObject;
  TOfficeofficialIndices.RegisterObject;
  TOfficeroles.RegisterObject;
  TOfficesources.RegisterObject;
  TOfficial.RegisterObject;
  TOfficialaddress.RegisterObject;
  TOfficialchannels.RegisterObject;
  TOfficialemails.RegisterObject;
  TOfficialphones.RegisterObject;
  TOfficialurls.RegisterObject;
  TPollingLocation.RegisterObject;
  TPollingLocationsources.RegisterObject;
  TRepresentativeInfoData.RegisterObject;
  TRepresentativeInfoDatadivisions.RegisterObject;
  TRepresentativeInfoDataoffices.RegisterObject;
  TRepresentativeInfoDataofficials.RegisterObject;
  TRepresentativeInfoResponse.RegisterObject;
  TRepresentativeInfoResponsedivisions.RegisterObject;
  TRepresentativeInfoResponseoffices.RegisterObject;
  TRepresentativeInfoResponseofficials.RegisterObject;
  TSimpleAddressType.RegisterObject;
  TSource.RegisterObject;
  TVoterInfoResponse.RegisterObject;
  TVoterInfoResponsecontests.RegisterObject;
  TVoterInfoResponsedropOffLocations.RegisterObject;
  TVoterInfoResponseearlyVoteSites.RegisterObject;
  TVoterInfoResponseotherElections.RegisterObject;
  TVoterInfoResponsepollingLocations.RegisterObject;
  TVoterInfoResponsestate.RegisterObject;
end;


Function TCivicinfoAPI.GetDivisionsInstance : TDivisionsResource;

begin
  if (FDivisionsInstance=Nil) then
    FDivisionsInstance:=CreateDivisionsResource;
  Result:=FDivisionsInstance;
end;

Function TCivicinfoAPI.CreateDivisionsResource : TDivisionsResource;

begin
  Result:=CreateDivisionsResource(Self);
end;


Function TCivicinfoAPI.CreateDivisionsResource(AOwner : TComponent) : TDivisionsResource;

begin
  Result:=TDivisionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCivicinfoAPI.GetElectionsInstance : TElectionsResource;

begin
  if (FElectionsInstance=Nil) then
    FElectionsInstance:=CreateElectionsResource;
  Result:=FElectionsInstance;
end;

Function TCivicinfoAPI.CreateElectionsResource : TElectionsResource;

begin
  Result:=CreateElectionsResource(Self);
end;


Function TCivicinfoAPI.CreateElectionsResource(AOwner : TComponent) : TElectionsResource;

begin
  Result:=TElectionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCivicinfoAPI.GetRepresentativesInstance : TRepresentativesResource;

begin
  if (FRepresentativesInstance=Nil) then
    FRepresentativesInstance:=CreateRepresentativesResource;
  Result:=FRepresentativesInstance;
end;

Function TCivicinfoAPI.CreateRepresentativesResource : TRepresentativesResource;

begin
  Result:=CreateRepresentativesResource(Self);
end;


Function TCivicinfoAPI.CreateRepresentativesResource(AOwner : TComponent) : TRepresentativesResource;

begin
  Result:=TRepresentativesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TCivicinfoAPI.RegisterAPI;
end.
