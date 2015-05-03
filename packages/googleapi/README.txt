This package offers support for the Google REST APIs.

Each Google Service API is a REST API that offers multiple resources.

All Google Service APIs are described in the Google Discovery service:
https://developers.google.com/discovery/
The Google APIS use JSON, JSON-Schema and OAuth2 to do their work.

These services can be converted to a object pascal implementation with the
googleapiconv program (see directory examples/generator)

It will fetch the JSON describing a REST API from the Google discovery server:

googleapiconv -s calendar/v3 -o calendar.pp

It can also download the rest description through an URL:

http://www.googleapis.com/discovery/v1/apis/calendar/v3/rest -o calendar.pp

It can also use downloaded versions of the JSON files.

The program has many options, running it without options gives an overview.

A Google API breaks down in 4 parts, all of which have a base class in the
following units:

TGoogleClient - unit googleclient
---------------------------------

This is a simple component that handles the transport and authorization, 
it needs a TFPWebClient descendent (such as TFPHTTPWebClient) and a 
TFPOauth2Handler descendent to communicate with Google servers.
These 2 classes are part of fcl-web, a synapse-based TFPWebclient descendant is
available separately.

TGoogleAPI - unit googleservice
-------------------------------

There is a descendent of this component for each Google service API, which 
handles all calls to the service. It uses a TGoogleClient component to handle 
actual communication. 

This class contains a method called ServiceCall which is used by all
resources in the API to execute service requests. It will use the client to
do the actual HTTP request.

Each unit google*.pp in this package contains a single component that
descends from the TGoogleAPI component.

TGoogleResource - unit googleservice
------------------------------------

For each resource exposed by the service, a descendent of this class is generated 
that has all the methods for that resource, as described in the REST service description.

TGoogleResource uses an instance of the TGoogleAPI class to handle all calls to 
the service. 

Each API unit google*.pp in this package contains one or more TGoogleResource
descendents, used in the API of that unit.

TGoogleBaseObject - unit googlebase
-----------------------------------

For each data type used in the API, a descendent of this class is used: it is a 
descendent of TBaseObject (unit restbase, part of fcl-web) and handles
loading from and saving to JSON.

Unit googlediscoverytopas
-------------------------

This unit contains the code to convert a Google REST API description to a
object pascal unit.

Authentication happens using OAuth2, the units for this are available in the
fcl-web package (fpjwt, fpoauth2, fpoauth2ini).

Example programs for Lazarus will be published separately.

