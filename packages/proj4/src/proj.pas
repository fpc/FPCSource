{
  Translation of the proj.4 headers for FreePascal
  Copyright (C) 2009 by Ivo Steinmann
}

(******************************************************************************
 * $Id: proj_api.h,v 1.17 2008/07/21 20:47:09 fwarmerdam Exp $
 *
 * Project:  PROJ.4
 * Purpose:  Public (application) include file for PROJ.4 API, and constants.
 * Author:   Frank Warmerdam, <warmerdam@pobox.com>
 *
 ******************************************************************************
 * Copyright (c) 2001, Frank Warmerdam <warmerdam@pobox.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************)

unit proj;

{$mode objfpc}
{$MINENUMSIZE 4}
{$h+}

interface

uses
  ctypes;

{$IFDEF WINDOWS}
  {$DEFINE DYNLINK}
{$ENDIF}

{$IFDEF DYNLINK}
const
{$IF Defined(WINDOWS)}
  proj4lib = 'libproj.dll';
{$ELSEIF Defined(UNIX)}
  proj4lib = 'libproj.so';
{$ELSE}
  {$MESSAGE ERROR 'DYNLINK not supported'}
{$IFEND}
{$ELSE}
  {$LINKLIB proj}
{$ENDIF}

{ List of projections (proj=)
        aea : Albers Equal Area
        	Conic Sph&Ell
        	lat_1= lat_2=
        aeqd : Azimuthal Equidistant
        	Azi, Sph&Ell
        	lat_0 guam
        airy : Airy
        	Misc Sph, no inv.
        	no_cut lat_b=
        aitoff : Aitoff
        	Misc Sph
        alsk : Mod. Stererographics of Alaska
        	Azi(mod)
        apian : Apian Globular I
        	Misc Sph, no inv.
        august : August Epicycloidal
        	Misc Sph, no inv.
        bacon : Bacon Globular
        	Misc Sph, no inv.
        bipc : Bipolar conic of western hemisphere
        	Conic Sph.
        boggs : Boggs Eumorphic
        	PCyl., no inv., Sph.
        bonne : Bonne (Werner lat_1=90)
        	Conic Sph&Ell
        	lat_1=
        cass : Cassini
        	Cyl, Sph&Ell
        cc : Central Cylindrical
        	Cyl, Sph
        cea : Equal Area Cylindrical
        	Cyl, Sph&Ell
        	lat_ts=
        chamb : Chamberlin Trimetric
        	Misc Sph, no inv.
        	lat_1= lon_1= lat_2= lon_2= lat_3= lon_3=
        collg : Collignon
        	PCyl, Sph.
        crast : Craster Parabolic (Putnins P4)
        	PCyl., Sph.
        denoy : Denoyer Semi-Elliptical
        	PCyl., no inv., Sph.
        eck1 : Eckert I
        	PCyl., Sph.
        eck2 : Eckert II
        	PCyl. Sph.
        eck3 : Eckert III
        	PCyl, Sph.
        eck4 : Eckert IV
        	PCyl, Sph.
        eck5 : Eckert V
        	PCyl, Sph.
        eck6 : Eckert VI
        	PCyl, Sph.
        eqc : Equidistant Cylindrical (Plate Caree)
        	Cyl, Sph
        	lat_ts=
        eqdc : Equidistant Conic
        	Conic, Sph&Ell
        	lat_1= lat_2=
        euler : Euler
        	Conic, Sph
        	lat_1= and lat_2=
        fahey : Fahey
        	Pcyl, Sph.
        fouc : Foucaut
        	PCyl., Sph.
        fouc_s : Foucaut Sinusoidal
        	PCyl., Sph.
        gall : Gall (Gall Stereographic)
        	Cyl, Sph
        geos : Geostationary Satellite View
        	Azi, Sph&Ell
        	h=
        gins8 : Ginsburg VIII (TsNIIGAiK)
        	PCyl, Sph., no inv.
        gn_sinu : General Sinusoidal Series
        	PCyl, Sph.
        	m= n=
        gnom : Gnomonic
        	Azi, Sph.
        goode : Goode Homolosine
        	PCyl, Sph.
        gs48 : Mod. Stererographics of 48 U.S.
        	Azi(mod)
        gs50 : Mod. Stererographics of 50 U.S.
        	Azi(mod)
        hammer : Hammer & Eckert-Greifendorff
        	Misc Sph, no inv.
        	W= M=
        hatano : Hatano Asymmetrical Equal Area
        	PCyl, Sph.
        imw_p : International Map of the World Polyconic
        	Mod. Polyconic, Ell
        	lat_1= and lat_2= [lon_1=]
        kav5 : Kavraisky V
        	PCyl., Sph.
        kav7 : Kavraisky VII
        	PCyl, Sph.
        krovak : Krovak
        	PCyl., Ellps.
        labrd : Laborde
        	Cyl, Sph
        	Special for Madagascar
        laea : Lambert Azimuthal Equal Area
        	Azi, Sph&Ell
        lagrng : Lagrange
        	Misc Sph, no inv.
        	W=
        larr : Larrivee
        	Misc Sph, no inv.
        lask : Laskowski
        	Misc Sph, no inv.
        lonlat : Lat/long (Geodetic)

        latlon : Lat/long (Geodetic alias)

        lcc : Lambert Conformal Conic
        	Conic, Sph&Ell
        	lat_1= and lat_2= or lat_0
        lcca : Lambert Conformal Conic Alternative
        	Conic, Sph&Ell
        	lat_0=
        leac : Lambert Equal Area Conic
        	Conic, Sph&Ell
        	lat_1= south
        lee_os : Lee Oblated Stereographic
        	Azi(mod)
        loxim : Loximuthal
        	PCyl Sph
        lsat : Space oblique for LANDSAT
        	Cyl, Sph&Ell
        	lsat= path=
        mbt_s : McBryde-Thomas Flat-Polar Sine (No. 1)
        	PCyl., Sph.
        mbt_fps : McBryde-Thomas Flat-Pole Sine (No. 2)
        	Cyl., Sph.
        mbtfpp : McBride-Thomas Flat-Polar Parabolic
        	Cyl., Sph.
        mbtfpq : McBryde-Thomas Flat-Polar Quartic
        	Cyl., Sph.
        mbtfps : McBryde-Thomas Flat-Polar Sinusoidal
        	PCyl, Sph.
        merc : Mercator
        	Cyl, Sph&Ell
        	lat_ts=
        mil_os : Miller Oblated Stereographic
        	Azi(mod)
        mill : Miller Cylindrical
        	Cyl, Sph
        moll : Mollweide
        	PCyl., Sph.
        murd1 : Murdoch I
        	Conic, Sph
        	lat_1= and lat_2=
        murd2 : Murdoch II
        	Conic, Sph
        	lat_1= and lat_2=
        murd3 : Murdoch III
        	Conic, Sph
        	lat_1= and lat_2=
        nell : Nell
        	PCyl., Sph.
        nell_h : Nell-Hammer
        	PCyl., Sph.
        nicol : Nicolosi Globular
        	Misc Sph, no inv.
        nsper : Near-sided perspective
        	Azi, Sph
        	h=
        nzmg : New Zealand Map Grid
        	fixed Earth
        ob_tran : General Oblique Transformation
        	Misc Sph
        	o_proj= plus parameters for projection
        	o_lat_p= o_lon_p= (new pole) or
        	o_alpha= o_lon_c= o_lat_c= or
        	o_lon_1= o_lat_1= o_lon_2= o_lat_2=
        ocea : Oblique Cylindrical Equal Area
        	Cyl, Sphlonc= alpha= or
        	lat_1= lat_2= lon_1= lon_2=
        oea : Oblated Equal Area
        	Misc Sph
        	n= m= theta=
        omerc : Oblique Mercator
        	Cyl, Sph&Ell
        	 no_rot rot_conv no_uoff and
        	alpha= lonc= or
        	 lon_1= lat_1= lon_2= lat_2=
        ortel : Ortelius Oval
        	Misc Sph, no inv.
        ortho : Orthographic
        	Azi, Sph.
        pconic : Perspective Conic
        	Conic, Sph
        	lat_1= and lat_2=
        poly : Polyconic (American)
        	Conic, Sph&Ell
        putp1 : Putnins P1
        	PCyl, Sph.
        putp2 : Putnins P2
        	PCyl., Sph.
        putp3 : Putnins P3
        	PCyl., Sph.
        putp3p : Putnins P3'
        	PCyl., no inv., Sph.
        putp4p : Putnins P4'
        	PCyl., Sph.
        putp5 : Putnins P5
        	PCyl., Sph.
        putp5p : Putnins P5'
        	PCyl., Sph.
        putp6 : Putnins P6
        	PCyl., Sph.
        putp6p : Putnins P6'
        	PCyl., Sph.
        qua_aut : Quartic Authalic
        	PCyl., Sph.
        robin : Robinson
        	PCyl., Sph.
        rouss : Roussilhe Stereographic
        	Azi., Ellps.
        rpoly : Rectangular Polyconic
        	Conic, Sph., no inv.
        	lat_ts=
        sinu : Sinusoidal (Sanson-Flamsteed)
        	PCyl, Sph&Ell
        somerc : Swiss. Obl. Mercator
        	Cyl, Ell
        	For CH1903
        stere : Stereographic
        	Azi, Sph&Ell
        	lat_ts=
        sterea : Oblique Stereographic Alternative
        	Azimuthal, Sph&Ell
        tcc : Transverse Central Cylindrical
        	Cyl, Sph, no inv.
        tcea : Transverse Cylindrical Equal Area
        	Cyl, Sph
        tissot : Tissot
        	Conic, Sph
        	lat_1= and lat_2=
        tmerc : Transverse Mercator
        	Cyl, Sph&Ell
        tpeqd : Two Point Equidistant
        	Misc Sph
        	lat_1= lon_1= lat_2= lon_2=
        tpers : Tilted perspective
        	Azi, Sph
        	tilt= azi= h=
        ups : Universal Polar Stereographic
        	Azi, Sph&Ell
        	south
        urm5 : Urmaev V
        	PCyl., Sph.
        	n= q= alphi=
        urmfps : Urmaev Flat-Polar Sinusoidal
        	PCyl, Sph.
        	n=
        utm : Universal Transverse Mercator (UTM)
        	Cyl, Sph
        	zone= south
        vandg : van der Grinten (I)
        	Misc Sph
        vandg2 : van der Grinten II
        	Misc Sph, no inv.
        vandg3 : van der Grinten III
        	Misc Sph, no inv.
        vandg4 : van der Grinten IV
        	Misc Sph, no inv.
        vitk1 : Vitkovsky I
        	Conic, Sph
        	lat_1= and lat_2=
        wag1 : Wagner I (Kavraisky VI)
        	PCyl, Sph.
        wag2 : Wagner II
        	PCyl., Sph.
        wag3 : Wagner III
        	PCyl., Sph.
        	lat_ts=
        wag4 : Wagner IV
        	PCyl., Sph.
        wag5 : Wagner V
        	PCyl., Sph.
        wag6 : Wagner VI
        	PCyl, Sph.
        wag7 : Wagner VII
        	Misc Sph, no inv.
        weren : Werenskiold I
        	PCyl., Sph.
        wink1 : Winkel I
        	PCyl., Sph.
        	lat_ts=
        wink2 : Winkel II
        	PCyl., Sph., no inv.
        	lat_1=
        wintri : Winkel Tripel
        	Misc Sph
        	lat_1
}


(* Try to update this every version! *)
const
  PJ_VERSION = 461;

//extern char const pj_release[]; /* global release id string */

const
  RAD_TO_DEG = 57.29577951308232;
  DEG_TO_RAD = 0.0174532925199432958;


//extern int pj_errno;	/* global error return code */

type
  projUV = record
    u, v: cdouble;
  end;

  projPJ = pointer;
  projXY = projUV;
  projLP = projUV;


(* procedure prototypes *)
function pj_fwd(val: projLP; proj: projPJ): projXY; cdecl; external;
function pj_inv(val: projXY; proj: projPJ): projLP; cdecl; external;
function pj_transform(src, dst: projPJ; point_count: clong; point_offset: cint; x,y,z: pcdouble): cint; cdecl; external;
function pj_datum_transform(src, dst: projPJ; point_count: clong; point_offset: cint; x,y,z: pcdouble): cint; cdecl; external;
function pj_geocentric_to_geodetic(a, es: cdouble; point_count: clong; point_offset: cint; x,y,z: pcdouble): cint; cdecl; external;
function pj_geodetic_to_geocentric(a, es: cdouble; point_count: clong; point_offset: cint; x,y,z: pcdouble): cint; cdecl; external;
function pj_compare_datums(srcdefn: projPJ; dstdefn: projPJ): cint; cdecl; external;
function pj_apply_gridshift(c: pchar; i: cint; point_count: clong; point_offset: cint; x,y,z: pcdouble): cint; cdecl; external;

type
  projFinder = function(s: pchar): pchar; cdecl;

procedure pj_deallocate_grids; cdecl; external;
function pj_is_latlong(proj: projPJ): cint; cdecl; external;
function pj_is_geocent(proj: projPJ): cint; cdecl; external;
procedure pj_pr_list(proj: projPJ); cdecl; external;
procedure pj_free(proj: projPJ); cdecl; external;
procedure pj_set_finder(finder: projFinder); cdecl; external;
procedure pj_set_searchpath(count: cint; path: ppchar); cdecl; external;
function pj_init(argc: cint; argv: ppchar): projPJ; cdecl; external;
function pj_init_plus(args: pchar): projPJ; cdecl; external;
function pj_get_def(proj: projPJ; i: cint): pchar; cdecl; external;
function pj_latlong_from_proj(proj: projPJ): projPJ; cdecl; external;
function pj_malloc(size: csize_t): pointer; cdecl; external;
procedure pj_dalloc(ptr: pointer); cdecl; external;
function pj_strerrno(err: cint): pchar; cdecl; external;
function pj_get_errno_ref: pcint; cdecl; external;
function pj_get_release: pchar; cdecl; external;



(***************************************************************************)
(* RSC IDENTIFIER:  GEOCENTRIC
 *
 * ABSTRACT
 *
 *    This component provides conversions between Geodetic coordinates (latitude,
 *    longitude in radians and height in meters) and Geocentric coordinates
 *    (X, Y, Z) in meters.
 *
 * ERROR HANDLING
 *
 *    This component checks parameters for valid values.  If an invalid value
 *    is found, the error code is combined with the current error code using
 *    the bitwise or.  This combining allows multiple error codes to be
 *    returned. The possible error codes are:
 *
 *      GEOCENT_NO_ERROR        : No errors occurred in function
 *      GEOCENT_LAT_ERROR       : Latitude out of valid range
 *                                 (-90 to 90 degrees)
 *      GEOCENT_LON_ERROR       : Longitude out of valid range
 *                                 (-180 to 360 degrees)
 *      GEOCENT_A_ERROR         : Semi-major axis less than or equal to zero
 *      GEOCENT_B_ERROR         : Semi-minor axis less than or equal to zero
 *      GEOCENT_A_LESS_B_ERROR  : Semi-major axis less than semi-minor axis
 *
 *
 * REUSE NOTES
 *
 *    GEOCENTRIC is intended for reuse by any application that performs
 *    coordinate conversions between geodetic coordinates and geocentric
 *    coordinates.
 *
 *
 * REFERENCES
 *
 *    An Improved Algorithm for Geocentric to Geodetic Coordinate Conversion,
 *    Ralph Toms, February 1996  UCRL-JC-123138.
 *
 *    Further information on GEOCENTRIC can be found in the Reuse Manual.
 *
 *    GEOCENTRIC originated from : U.S. Army Topographic Engineering Center
 *                                 Geospatial Information Division
 *                                 7701 Telegraph Road
 *                                 Alexandria, VA  22310-3864
 *
 * LICENSES
 *
 *    None apply to this component.
 *
 * RESTRICTIONS
 *
 *    GEOCENTRIC has no restrictions.
 *
 * ENVIRONMENT
 *
 *    GEOCENTRIC was tested and certified in the following environments:
 *
 *    1. Solaris 2.5 with GCC version 2.8.1
 *    2. Windows 95 with MS Visual C++ version 6
 *
 * MODIFICATIONS
 *
 *    Date              Description
 *    ----              -----------
 *
 *
 *)


(***************************************************************************)
(*
 *                              DEFINES
 *)
const
  GEOCENT_NO_ERROR        = $0000;
  GEOCENT_LAT_ERROR       = $0001;
  GEOCENT_LON_ERROR       = $0002;
  GEOCENT_A_ERROR         = $0004;
  GEOCENT_B_ERROR         = $0008;
  GEOCENT_A_LESS_B_ERROR  = $0010;


(***************************************************************************)
(*
 *                              FUNCTION PROTOTYPES
 *)

type
  PGeocentricInfo = ^GeocentricInfo;
  GeocentricInfo = record
    Geocent_a   : cdouble;       (* Semi-major axis of ellipsoid in meters *)
    Geocent_b   : cdouble;       (* Semi-minor axis of ellipsoid *)
    Geocent_a2  : cdouble;       (* Square of semi-major axis *)
    Geocent_b2  : cdouble;       (* Square of semi-minor axis *)
    Geocent_e2  : cdouble;       (* Eccentricity squared  *)
    Geocent_ep2 : cdouble;       (* 2nd eccentricity squared *)
  end;


procedure pj_Init_Geocentric(var gi: GeocentricInfo); cdecl; external;
function pj_Set_Geocentric_Parameters(var gi: GeocentricInfo; a, b: cdouble): clong; cdecl; external;

(*
 * The function Set_Geocentric_Parameters receives the ellipsoid parameters
 * as inputs and sets the corresponding state variables.
 *
 *    a  : Semi-major axis, in meters.          (input)
 *    b  : Semi-minor axis, in meters.          (input)
 *)


procedure pj_Get_Geocentric_Parameters(var gi: GeocentricInfo; var a, b: cdouble); cdecl; external;

(*
 * The function Get_Geocentric_Parameters returns the ellipsoid parameters
 * to be used in geocentric coordinate conversions.
 *
 *    a  : Semi-major axis, in meters.          (output)
 *    b  : Semi-minor axis, in meters.          (output)
 *)


function pj_Convert_Geodetic_To_Geocentric(var gi: GeocentricInfo; Latitude, Longitude, Height: cdouble;
  var X, Y, Z: cdouble): clong; cdecl; external;

(*
 * The function Convert_Geodetic_To_Geocentric converts geodetic coordinates
 * (latitude, longitude, and height) to geocentric coordinates (X, Y, Z),
 * according to the current ellipsoid parameters.
 *
 *    Latitude  : Geodetic latitude in radians                     (input)
 *    Longitude : Geodetic longitude in radians                    (input)
 *    Height    : Geodetic height, in meters                       (input)
 *    X         : Calculated Geocentric X coordinate, in meters.   (output)
 *    Y         : Calculated Geocentric Y coordinate, in meters.   (output)
 *    Z         : Calculated Geocentric Z coordinate, in meters.   (output)
 *
 *)


procedure pj_Convert_Geocentric_To_Geodetic(var gi: GeocentricInfo; X, Y, Z: cdouble;
  var Latitude, Longitude, Height: cdouble); cdecl; external;

(*
 * The function Convert_Geocentric_To_Geodetic converts geocentric
 * coordinates (X, Y, Z) to geodetic coordinates (latitude, longitude,
 * and height), according to the current ellipsoid parameters.
 *
 *    X         : Geocentric X coordinate, in meters.         (input)
 *    Y         : Geocentric Y coordinate, in meters.         (input)
 *    Z         : Geocentric Z coordinate, in meters.         (input)
 *    Latitude  : Calculated latitude value in radians.       (output)
 *    Longitude : Calculated longitude value in radians.      (output)
 *    Height    : Calculated height value, in meters.         (output)
 *)

implementation

end.
