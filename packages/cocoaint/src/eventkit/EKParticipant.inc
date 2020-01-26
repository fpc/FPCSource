{ Parsed from EventKit.framework EKParticipant.h }


{$ifdef TYPES}
type
  EKParticipantPtr = ^EKParticipant;
{$endif}

{$ifdef CLASSES}

type
  EKParticipant = objcclass external (EKObject, NSCopyingProtocol)
  private
  public
    function URL: NSURL; message 'URL';
    function name: NSString; message 'name';
    function participantStatus: EKParticipantStatus; message 'participantStatus';
    function participantRole: EKParticipantRole; message 'participantRole';
    function participantType: EKParticipantType; message 'participantType';
    function isCurrentUser: ObjCBOOL; message 'isCurrentUser';
    function ABPersonInAddressBook (addressBook: ABAddressBook): ABPerson; message 'ABPersonInAddressBook:'; { available starting in __MAC_10_8,__IPHONE_NA }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

