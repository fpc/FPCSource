unit googlecloudsearch;
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
  
  { --------------------------------------------------------------------
    TCloudsearchAPI
    --------------------------------------------------------------------}
  
  TCloudsearchAPI = Class(TGoogleAPI)
  Private
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
    //Add default on-demand instances for resources
  end;

implementation


{ --------------------------------------------------------------------
  TCloudsearchAPI
  --------------------------------------------------------------------}

Class Function TCloudsearchAPI.APIName : String;

begin
  Result:='cloudsearch';
end;

Class Function TCloudsearchAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TCloudsearchAPI.APIRevision : String;

begin
  Result:='20150416';
end;

Class Function TCloudsearchAPI.APIID : String;

begin
  Result:='cloudsearch:v1';
end;

Class Function TCloudsearchAPI.APITitle : String;

begin
  Result:='Google Cloud Search API';
end;

Class Function TCloudsearchAPI.APIDescription : String;

begin
  Result:='The Google Cloud Search API defines an application interface to index documents that contain structured data and to search those indexes. It supports full text search.';
end;

Class Function TCloudsearchAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCloudsearchAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCloudsearchAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCloudsearchAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCloudsearchAPI.APIdocumentationLink : String;

begin
  Result:='';
end;

Class Function TCloudsearchAPI.APIrootUrl : string;

begin
  Result:='https://cloudsearch.googleapis.com/';
end;

Class Function TCloudsearchAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TCloudsearchAPI.APIbaseURL : String;

begin
  Result:='https://cloudsearch.googleapis.com/';
end;

Class Function TCloudsearchAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCloudsearchAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TCloudsearchAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCloudsearchAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TCloudsearchAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TCloudsearchAPI.RegisterAPIResources;

begin
end;


initialization
  TCloudsearchAPI.RegisterAPI;
end.
