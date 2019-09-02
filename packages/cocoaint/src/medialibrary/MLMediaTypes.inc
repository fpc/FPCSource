{ Parsed from MediaLibrary.framework MLMediaTypes.h }

{$ifdef TYPES}
type
  MLMediaSourceType = NSUInteger;
  MLMediaSourceTypePtr = ^MLMediaSourceType;

const
  MLMediaSourceTypeAudio = 1 shl 0;
  MLMediaSourceTypeImage = 1 shl 1;
  MLMediaSourceTypeMovie = 1 shl 2;

type
  MLMediaType = NSUInteger;
  MLMediaTypePtr = ^MLMediaType;

const
  MLMediaTypeAudio = 1 shl 0;
  MLMediaTypeImage = 1 shl 1;
  MLMediaTypeMovie = 1 shl 2;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  MLFolderRootGroupTypeIdentifier: NSString; cvar; external;
  MLFolderGroupTypeIdentifier: NSString; cvar; external;
  MLiTunesRootGroupTypeIdentifier: NSString; cvar; external;
  MLiTunesPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesPurchasedPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesPodcastPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesVideoPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesSmartPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesFolderPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesMoviesPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesTVShowsPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesAudioBooksPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesMusicPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesMusicVideosPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesGeniusPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesSavedGeniusPlaylistTypeIdentifier: NSString; cvar; external;
  MLiTunesiTunesUPlaylistTypeIdentifier: NSString; cvar; external;
  MLPhotosRootGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosSharedGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosAlbumsGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosAlbumTypeIdentifier: NSString; cvar; external;
  MLPhotosFolderTypeIdentifier: NSString; cvar; external;
  MLPhotosSmartAlbumTypeIdentifier: NSString; cvar; external;
  MLPhotosPublishedAlbumTypeIdentifier: NSString; cvar; external;
  MLPhotosAllMomentsGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosMomentGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosAllCollectionsGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosCollectionGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosAllYearsGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosYearGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosLastImportGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosMyPhotoStreamTypeIdentifier: NSString; cvar; external;
  MLPhotosSharedPhotoStreamTypeIdentifier: NSString; cvar; external;
  MLPhotosFavoritesGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosPanoramasGroupTypeIdentifier: NSString; cvar; external;
  MLPhotosVideosGroupTypeIdentifier: NSString; cvar; external;
  MLiPhotoRootGroupTypeIdentifier: NSString; cvar; external;
  MLiPhotoAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoLibraryAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoEventsFolderTypeIdentifier: NSString; cvar; external;
  MLiPhotoSmartAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoEventAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoLastImportAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoLastNMonthsAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoFlaggedAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoFolderAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoSubscribedAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoFacesAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoPlacesAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoPlacesCountryAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoPlacesProvinceAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoPlacesCityAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoPlacesPointOfInterestAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoFacebookAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoFlickrAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoFacebookGroupTypeIdentifier: NSString; cvar; external;
  MLiPhotoFlickrGroupTypeIdentifier: NSString; cvar; external;
  MLiPhotoSlideShowAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoLastViewedEventAlbumTypeIdentifier: NSString; cvar; external;
  MLiPhotoPhotoStreamAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureRootGroupTypeIdentifier: NSString; cvar; external;
  MLApertureUserAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureUserSmartAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureProjectAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureFolderAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureProjectFolderAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureLightTableTypeIdentifier: NSString; cvar; external;
  MLApertureFlickrGroupTypeIdentifier: NSString; cvar; external;
  MLApertureFlickrAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureFacebookGroupTypeIdentifier: NSString; cvar; external;
  MLApertureFacebookAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureSmugMugGroupTypeIdentifier: NSString; cvar; external;
  MLApertureSmugMugAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureSlideShowTypeIdentifier: NSString; cvar; external;
  MLApertureAllPhotosTypeIdentifier: NSString; cvar; external;
  MLApertureFlaggedTypeIdentifier: NSString; cvar; external;
  MLApertureAllProjectsTypeIdentifier: NSString; cvar; external;
  MLApertureFacesAlbumTypeIdentifier: NSString; cvar; external;
  MLAperturePlacesAlbumTypeIdentifier: NSString; cvar; external;
  MLAperturePlacesCountryAlbumTypeIdentifier: NSString; cvar; external;
  MLAperturePlacesProvinceAlbumTypeIdentifier: NSString; cvar; external;
  MLAperturePlacesCityAlbumTypeIdentifier: NSString; cvar; external;
  MLAperturePlacesPointOfInterestAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureLastImportAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureLastNMonthsAlbumTypeIdentifier: NSString; cvar; external;
  MLApertureLastViewedEventAlbumTypeIdentifier: NSString; cvar; external;
  MLAperturePhotoStreamAlbumTypeIdentifier: NSString; cvar; external;
  MLGarageBandRootGroupTypeIdentifier: NSString; cvar; external;
  MLGarageBandFolderGroupTypeIdentifier: NSString; cvar; external;
  MLLogicRootGroupTypeIdentifier: NSString; cvar; external;
  MLLogicBouncesGroupTypeIdentifier: NSString; cvar; external;
  MLLogicProjectsGroupTypeIdentifier: NSString; cvar; external;
  MLLogicProjectTypeIdentifier: NSString; cvar; external;
  MLiMovieRootGroupTypeIdentifier: NSString; cvar; external;
  MLiMovieEventGroupTypeIdentifier: NSString; cvar; external;
  MLiMovieProjectGroupTypeIdentifier: NSString; cvar; external;
  MLiMovieEventLibraryGroupTypeIdentifier: NSString; cvar; external;
  MLiMovieEventCalendarGroupTypeIdentifier: NSString; cvar; external;
  MLiMovieFolderGroupTypeIdentifier: NSString; cvar; external;
  MLFinalCutRootGroupTypeIdentifier: NSString; cvar; external;
  MLFinalCutEventGroupTypeIdentifier: NSString; cvar; external;
  MLFinalCutProjectGroupTypeIdentifier: NSString; cvar; external;
  MLFinalCutEventLibraryGroupTypeIdentifier: NSString; cvar; external;
  MLFinalCutEventCalendarGroupTypeIdentifier: NSString; cvar; external;
  MLFinalCutFolderGroupTypeIdentifier: NSString; cvar; external;
  MLMediaObjectDurationKey: NSString; cvar; external;
  MLMediaObjectArtistKey: NSString; cvar; external;
  MLMediaObjectAlbumKey: NSString; cvar; external;
  MLMediaObjectGenreKey: NSString; cvar; external;
  MLMediaObjectKindKey: NSString; cvar; external;
  MLMediaObjectTrackNumberKey: NSString; cvar; external;
  MLMediaObjectBitRateKey: NSString; cvar; external;
  MLMediaObjectSampleRateKey: NSString; cvar; external;
  MLMediaObjectChannelCountKey: NSString; cvar; external;
  MLMediaObjectResolutionStringKey: NSString; cvar; external;
  MLMediaObjectCommentsKey: NSString; cvar; external;
  MLMediaObjectKeywordsKey: NSString; cvar; external;
  MLMediaObjectProtectedKey: NSString; cvar; external;
{$endif}

