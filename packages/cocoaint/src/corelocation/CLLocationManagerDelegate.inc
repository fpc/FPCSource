{ Parsed from CoreLocation.framework CLLocationManagerDelegate.h }


{ Types from CLLocationManagerDelegateProtocol }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  CLLocationManagerDelegateProtocolPtr = ^CLLocationManagerDelegateProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  CLLocationManagerDelegateProtocol = objcprotocol external name 'CLLocationManagerDelegate' (NSObjectProtocol)
  optional
    procedure locationManager_didUpdateToLocation_fromLocation (manager: CLLocationManager; newLocation: CLLocation; oldLocation: CLLocation); message 'locationManager:didUpdateToLocation:fromLocation:'; deprecated 'in __MAC_10_6, __MAC_NA, __IPHONE_2_0, __IPHONE_6_0';
    procedure locationManager_didUpdateLocations (manager: CLLocationManager; locations: NSArray); message 'locationManager:didUpdateLocations:'; { available starting in __MAC_10_9,__IPHONE_6_0 }
    procedure locationManager_didUpdateHeading (manager: CLLocationManager; newHeading: CLHeading); message 'locationManager:didUpdateHeading:'; { available starting in __MAC_NA,__IPHONE_3_0 }
    function locationManagerShouldDisplayHeadingCalibration (manager: CLLocationManager): ObjCBOOL; message 'locationManagerShouldDisplayHeadingCalibration:'; { available starting in __MAC_NA,__IPHONE_3_0 }
    procedure locationManager_didDetermineState_forRegion (manager: CLLocationManager; state: CLRegionState; region: CLRegion); message 'locationManager:didDetermineState:forRegion:'; { available starting in __MAC_10_10,__IPHONE_7_0 }
    {$if defined(TARGET_OS_IPHONE)}
    procedure locationManager_didRangeBeacons_inRegion (manager: CLLocationManager; beacons: NSArray; region: CLBeaconRegion); message 'locationManager:didRangeBeacons:inRegion:'; { available starting in __MAC_NA,__IPHONE_7_0 }
    {$endif}
    {$if defined(TARGET_OS_IPHONE)}
    procedure locationManager_rangingBeaconsDidFailForRegion_withError (manager: CLLocationManager; region: CLBeaconRegion; error: NSError); message 'locationManager:rangingBeaconsDidFailForRegion:withError:'; { available starting in __MAC_NA,__IPHONE_7_0 }
    {$endif}
    procedure locationManager_didEnterRegion (manager: CLLocationManager; region: CLRegion); message 'locationManager:didEnterRegion:'; { available starting in __MAC_10_8,__IPHONE_4_0 }
    procedure locationManager_didExitRegion (manager: CLLocationManager; region: CLRegion); message 'locationManager:didExitRegion:'; { available starting in __MAC_10_8,__IPHONE_4_0 }
    procedure locationManager_didFailWithError (manager: CLLocationManager; error: NSError); message 'locationManager:didFailWithError:';
    procedure locationManager_monitoringDidFailForRegion_withError (manager: CLLocationManager; region: CLRegion; error: NSError); message 'locationManager:monitoringDidFailForRegion:withError:'; { available starting in __MAC_10_8,__IPHONE_4_0 }
    procedure locationManager_didChangeAuthorizationStatus (manager: CLLocationManager; status: CLAuthorizationStatus); message 'locationManager:didChangeAuthorizationStatus:'; { available starting in __MAC_10_7,__IPHONE_4_2 }
    procedure locationManager_didStartMonitoringForRegion (manager: CLLocationManager; region: CLRegion); message 'locationManager:didStartMonitoringForRegion:'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    procedure locationManagerDidPauseLocationUpdates (manager: CLLocationManager); message 'locationManagerDidPauseLocationUpdates:'; { available starting in __MAC_NA,__IPHONE_6_0 }
    procedure locationManagerDidResumeLocationUpdates (manager: CLLocationManager); message 'locationManagerDidResumeLocationUpdates:'; { available starting in __MAC_NA,__IPHONE_6_0 }
    procedure locationManager_didFinishDeferredUpdatesWithError (manager: CLLocationManager; error: NSError); message 'locationManager:didFinishDeferredUpdatesWithError:'; { available starting in __MAC_10_9,__IPHONE_6_0 }
  end;
{$endif}

