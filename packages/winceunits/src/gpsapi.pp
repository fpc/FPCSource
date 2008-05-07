{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
//
// GPS Intermediate Driver API
//
// gpsapi.h
//
//*********************************************************

//
// Microsoft Windows Mobile 5.0 for PocketPC SDK.
//

unit GPSApi;

{$CALLING cdecl}

interface

uses Windows;

const
      GPS_MAX_SATELLITES    = 12;
      GPS_MAX_PREFIX_NAME   = 16;
      GPS_MAX_FRIENDLY_NAME = 64;

      GPS_VERSION_1         = 1;
      GPS_VERSION_CURRENT   = GPS_VERSION_1;

type
     GPS_FIX_TYPE = (GPS_FIX_UNKNOWN := 0,
                     GPS_FIX_2D,
                     GPS_FIX_3D);

     GPS_FIX_SELECTION = (GPS_FIX_SELECTION_UNKNOWN := 0,
                          GPS_FIX_SELECTION_AUTO,
                          GPS_FIX_SELECTION_MANUAL);

     GPS_FIX_QUALITY = (GPS_FIX_QUALITY_UNKNOWN := 0,
                        GPS_FIX_QUALITY_GPS,
                        GPS_FIX_QUALITY_DGPS);

//
// GPS_VALID_XXX bit flags in GPS_POSITION structure are valid.
//
const
      GPS_VALID_UTC_TIME                                 = $00000001;
      GPS_VALID_LATITUDE                                 = $00000002;
      GPS_VALID_LONGITUDE                                = $00000004;
      GPS_VALID_SPEED                                    = $00000008;
      GPS_VALID_HEADING                                  = $00000010;
      GPS_VALID_MAGNETIC_VARIATION                       = $00000020;
      GPS_VALID_ALTITUDE_WRT_SEA_LEVEL                   = $00000040;
      GPS_VALID_ALTITUDE_WRT_ELLIPSOID                   = $00000080;
      GPS_VALID_POSITION_DILUTION_OF_PRECISION           = $00000100;
      GPS_VALID_HORIZONTAL_DILUTION_OF_PRECISION         = $00000200;
      GPS_VALID_VERTICAL_DILUTION_OF_PRECISION           = $00000400;
      GPS_VALID_SATELLITE_COUNT                          = $00000800;
      GPS_VALID_SATELLITES_USED_PRNS                     = $00001000;
      GPS_VALID_SATELLITES_IN_VIEW                       = $00002000;
      GPS_VALID_SATELLITES_IN_VIEW_PRNS                  = $00004000;
      GPS_VALID_SATELLITES_IN_VIEW_ELEVATION             = $00008000;
      GPS_VALID_SATELLITES_IN_VIEW_AZIMUTH               = $00010000;
      GPS_VALID_SATELLITES_IN_VIEW_SIGNAL_TO_NOISE_RATIO = $00020000;


//
// GPS_DATA_FLAGS_XXX bit flags set in GPS_POSITION dwFlags field
// provide additional information about the state of the query.
// 

// Set when GPS hardware is not connected to GPSID and we 
// are returning cached data.
const
      GPS_DATA_FLAGS_HARDWARE_OFF = $00000001;

//
// GPS_POSITION contains our latest physical coordinates, the time,
// and satellites used in determining these coordinates.
//

type
     _GPS_POSITION = record
	      dwVersion:DWORD;             // Current version of GPSID client is using.
	      dwSize:DWORD;                // sizeof(_GPS_POSITION)

      // Not all fields in the structure below are guaranteed to be valid.
	     // Which fields are valid depend on GPS device being used, how stale the API allows
	     // the data to be, and current signal.
	     // Valid fields are specified in dwValidFields, based on GPS_VALID_XXX flags.
	      dwValidFields:DWORD;

      // Additional information about this location structure (GPS_DATA_FLAGS_XXX)
	      dwFlags:DWORD;

      //** Time related
	      stUTCTime:SYSTEMTIME; 	//  UTC according to GPS clock.

      //** Position + heading related
       dblLatitude:double;            // Degrees latitude.  North is positive
       dblLongitude:double;           // Degrees longitude.  East is positive
       flSpeed:single;                // Speed in knots
       flHeading:single;              // Degrees heading (course made good).  True North=0
       dblMagneticVariation:double;   // Magnetic variation.  East is positive
       flAltitudeWRTSeaLevel:single;  // Altitute with regards to sea level, in meters
       flAltitudeWRTEllipsoid:single; // Altitude with regards to ellipsoid, in meters

      //** Quality of this fix
       FixQuality:GPS_FIX_QUALITY;             // Where did we get fix from?
       FixType:GPS_FIX_TYPE;                   // Is this 2d or 3d fix?
       SelectionType:GPS_FIX_SELECTION;        // Auto or manual selection between 2d or 3d mode
       flPositionDilutionOfPrecision:single;   // Position Dilution Of Precision
       flHorizontalDilutionOfPrecision:single; // Horizontal Dilution Of Precision
       flVerticalDilutionOfPrecision:single;   // Vertical Dilution Of Precision

      //** Satellite information
       dwSatelliteCount:DWORD;                                               // Number of satellites used in solution
       rgdwSatellitesUsedPRNs:array[0..GPS_MAX_SATELLITES-1] of DWORD;       // PRN numbers of satellites used in the solution

       dwSatellitesInView:DWORD;                      	                      // Number of satellites in view.  From 0-GPS_MAX_SATELLITES
       rgdwSatellitesInViewPRNs:array[0..GPS_MAX_SATELLITES-1] of DWORD;     // PRN numbers of satellites in view
       rgdwSatellitesInViewElevation:array[0..GPS_MAX_SATELLITES-1] of DWORD;// Elevation of each satellite in view
       rgdwSatellitesInViewAzimuth:array[0..GPS_MAX_SATELLITES-1] of DWORD;  // Azimuth of each satellite in view
       rgdwSatellitesInViewSignalToNoiseRatio:array[0..GPS_MAX_SATELLITES-1] of DWORD; // Signal to noise ratio of each satellite in view
     end;
     GPS_POSITION = _GPS_POSITION;
     PGPS_POSITION = ^GPS_POSITION;


//
// GPS_DEVICE contains information about the device driver and the
// service itself and is returned on a call to GPSGetDeviceState().
// States are indicated with SERVICE_STATE_XXX flags defined in service.h
// 
type
     _GPS_DEVICE = record
       dwVersion:DWORD;                                 // Current version of GPSID client is using.
	      dwSize:DWORD;                                    // sizeof this structure
	      dwServiceState:DWORD;                            // State of the GPS Intermediate Driver service.
	      dwDeviceState:DWORD;                             // Status of the actual GPS device driver.
	      ftLastDataReceived:FILETIME;                     // Last time that the actual GPS device sent information to the intermediate driver.
       szGPSDriverPrefix:array[0..GPS_MAX_PREFIX_NAME-1] of WCHAR;    // Prefix name we are using to communicate to the base GPS driver
	      szGPSMultiplexPrefix:array[0..GPS_MAX_PREFIX_NAME-1] of WCHAR; // Prefix name that GPS Intermediate Driver Multiplexer is running on
	      szGPSFriendlyName:array[0..GPS_MAX_FRIENDLY_NAME-1] of WCHAR;  // Friendly name real GPS device we are currently using
     end;
     GPS_DEVICE = _GPS_DEVICE;
     PGPS_DEVICE = ^_GPS_DEVICE;

const
      gpsapidll = 'gpsapi.dll';

function GPSOpenDevice(hNewLocationData:HANDLE; hDeviceStateChange:HANDLE; const szDeviceName:PWCHAR; dwFlags:DWORD):HANDLE; external gpsapidll name 'GPSOpenDevice';

function GPSCloseDevice(hGPSDevice:HANDLE):DWORD; external gpsapidll name 'GPSCloseDevice';

function GPSGetPosition(hGPSDevice:HANDLE; pGPSPosition:PGPS_POSITION; dwMaximumAge:DWORD; dwFlags:DWORD):DWORD; external gpsapidll name 'GPSGetPosition';

function GPSGetDeviceState(pGPSDevice:PGPS_DEVICE):DWORD; external gpsapidll name 'GPSGetDeviceState';

implementation

end.
