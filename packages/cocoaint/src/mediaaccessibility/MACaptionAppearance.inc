{ Parsed from MediaAccessibility.framework MACaptionAppearance.h }

{$ifndef MediaAccessibility_MACaptionAppearance_h}
{$ifdef EXTERNAL_SYMBOLS}
var
  kMACaptionAppearanceSettingsChangedNotification: CFStringRef; cvar; external;
{$endif}

{$ifdef TYPES}
type
  MACaptionAppearanceDomain = CFIndex;
  MACaptionAppearanceDomainPtr = ^MACaptionAppearanceDomain;

const
  kMACaptionAppearanceDomainDefault = 0;
  kMACaptionAppearanceDomainUser = 1;

type
  MACaptionAppearanceDisplayType = CFIndex;
  MACaptionAppearanceDisplayTypePtr = ^MACaptionAppearanceDisplayType;

const
  kMACaptionAppearanceDisplayTypeForcedOnly = 0;
  kMACaptionAppearanceDisplayTypeAutomatic = 1;
  kMACaptionAppearanceDisplayTypeAlwaysOn = 2;

type
  MACaptionAppearanceBehavior = CFIndex;
  MACaptionAppearanceBehaviorPtr = ^MACaptionAppearanceBehavior;

const
  kMACaptionAppearanceBehaviorUseValue = 0;
  kMACaptionAppearanceBehaviorUseContentIfAvailable = 1;

type
  MACaptionAppearanceFontStyle = CFIndex;
  MACaptionAppearanceFontStylePtr = ^MACaptionAppearanceFontStyle;

const
  kMACaptionAppearanceFontStyleDefault = 0;
  kMACaptionAppearanceFontStyleMonospacedWithSerif = 1;
  kMACaptionAppearanceFontStyleProportionalWithSerif = 2;
  kMACaptionAppearanceFontStyleMonospacedWithoutSerif = 3;
  kMACaptionAppearanceFontStyleProportionalWithoutSerif = 4;
  kMACaptionAppearanceFontStyleCasual = 5;
  kMACaptionAppearanceFontStyleCursive = 6;
  kMACaptionAppearanceFontStyleSmallCapital = 7;

type
  MACaptionAppearanceTextEdgeStyle = CFIndex;
  MACaptionAppearanceTextEdgeStylePtr = ^MACaptionAppearanceTextEdgeStyle;

const
  kMACaptionAppearanceTextEdgeStyleUndefined = 0;
  kMACaptionAppearanceTextEdgeStyleNone = 1;
  kMACaptionAppearanceTextEdgeStyleRaised = 2;
  kMACaptionAppearanceTextEdgeStyleDepressed = 3;
  kMACaptionAppearanceTextEdgeStyleUniform = 4;
  kMACaptionAppearanceTextEdgeStyleDropShadow = 5;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  MAMediaCharacteristicDescribesMusicAndSoundForAccessibility: CFStringRef { available in 10_9, 7_0 }; cvar; external;
  MAMediaCharacteristicTranscribesSpokenDialogForAccessibility: CFStringRef { available in 10_9, 7_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function MACaptionAppearanceAddSelectedLanguage(domain: MACaptionAppearanceDomain; language: CFStringRef): cbool; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceCopySelectedLanguages(domain: MACaptionAppearanceDomain): CFArrayRef; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceGetDisplayType(domain: MACaptionAppearanceDomain): MACaptionAppearanceDisplayType; cdecl; external; { available in 10_9, 7_0 }
procedure MACaptionAppearanceSetDisplayType(domain: MACaptionAppearanceDomain; displayType: MACaptionAppearanceDisplayType); cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceCopyPreferredCaptioningMediaCharacteristics(domain: MACaptionAppearanceDomain): CFArrayRef; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceCopyForegroundColor(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): CGColorRef; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceCopyBackgroundColor(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): CGColorRef; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceCopyWindowColor(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): CGColorRef; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceGetForegroundOpacity(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): CGFloat; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceGetBackgroundOpacity(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): CGFloat; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceGetWindowOpacity(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): CGFloat; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceGetWindowRoundedCornerRadius(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): CGFloat; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceCopyFontDescriptorForStyle(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr; fontStyle: MACaptionAppearanceFontStyle): CTFontDescriptorRef; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceGetRelativeCharacterSize(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): CGFloat; cdecl; external; { available in 10_9, 7_0 }
function MACaptionAppearanceGetTextEdgeStyle(domain: MACaptionAppearanceDomain; behavior: MACaptionAppearanceBehaviorPtr): MACaptionAppearanceTextEdgeStyle; cdecl; external; { available in 10_9, 7_0 }
{$endif}

{$endif}
