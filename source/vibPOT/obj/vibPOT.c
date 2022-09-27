/* ../src/vibPOT.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal aaf[40]	/* was [4][10] */;
} aafcom_;

#define aafcom_1 aafcom_

struct {
    integer ms, ny;
} fms_;

#define fms_1 fms_

struct {
    doublereal aye, aze, ate, ase, are;
} eswitch_;

#define eswitch_1 eswitch_

struct {
    doublereal abe, aae, age, ae3, ae4, ae5, ae6, ae7;
} eswitc1_;

#define eswitc1_1 eswitc1_

struct {
    doublereal ade, abt, ax2, ax3, ax4, ax5, ax6, ax7;
} eswitc2_;

#define eswitc2_1 eswitc2_

struct {
    doublereal amu, re, de, we, wexe, weye, weze, wete, wese, were;
} spectra_;

#define spectra_1 spectra_

struct {
    doublereal be, alphae, gamae, der, betae, bryd;
} spectrb_;

#define spectrb_1 spectrb_

union {
    struct {
	doublereal fvec[40];
	integer nn;
    } _1;
    struct {
	doublereal fvec[40];
	integer n;
    } _2;
} newtv_;

#define newtv_1 (newtv_._1)
#define newtv_2 (newtv_._2)

struct {
    doublereal bt, c0, c02, c03, c04, c05, c06, c07, c08, c09, c010;
} pgdata1_;

#define pgdata1_1 pgdata1_

struct {
    doublereal re2, re4, re6, re8, re10, re12, re14, re16, re18, re20;
} pgdata2_;

#define pgdata2_1 pgdata2_

struct {
    doublereal ff2, betam;
} vnumpot_;

#define vnumpot_1 vnumpot_

struct {
    doublereal w0, we0, wex, wey, wez, wet, wes, wer;
} spectr0_;

#define spectr0_1 spectr0_

struct {
    doublereal bee, ale, gae, eta3, eta4, eta5, eta6, eta7;
} spectr1_;

#define spectr1_1 spectr1_

struct {
    doublereal dee, bete, xsi2, xsi3, xsi4, xsi5, xsi6, xsi7;
} spectr2_;

#define spectr2_1 spectr2_

struct {
    doublereal a0, a1, a2, a3, a4, a5, a6, a7, a8;
} vjcoef0_;

#define vjcoef0_1 vjcoef0_

struct {
    doublereal b3, b4, b5, b6, b7, b8;
} vjcoef1_;

#define vjcoef1_1 vjcoef1_

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__5 = 5;
static doublereal c_b218 = -1.;
static integer c__40 = 40;
static integer c__200 = 200;
static integer c__100 = 100;
static doublereal c_b488 = 2.;
static integer c__9 = 9;
static doublereal c_b672 = 1.;
static integer c_n1 = -1;

/*     program  vibPOT */
/* ----------------------------------------------------------------- */
/*          Modified by   Weiguo Sun   07/03/1999 */

/*     This program is to calculate the vibrational potentials */
/*   for diatomic molecules : */
/* 1.) MORSE:      V(R) = De*{ exp(-2*alpha*x) - 2*exp(-alpha*x) } */
/*             where     x = (R-Re)/Re */
/* 2.) Harmonic Oscillator:      V(R) = k*(R-Re)**2/2 */

/* 3.) Sun's Modified Murrell-Sorbie (x=R-Re) : */
/*           V_SMMS(R)=-De*beta*(1/beta + a1*x + a2*x**2 */
/*                                 + a3*x**3 + ...)*exp(-a1*beta*x) */

/* 4.) Huxley-Murrell-Sorbie (x=R-Re) : */
/*        V_MS(R)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x) */

/* 5.) SF (ECM) : V_ecm(R) = V_MS + Lamta(R)*delta_V(R) */
/*          V_MS:  Murrell & Sorbie potential in which the */
/*                 (a1,a2,a3) are calculated using SF formulae. */
/*          Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)] */
/*          delta_V(R) = V_MS - V_0 */
/*            For  R < Re : */
/*      Nturn = 0, V_ecm(R) = V_MS  For ms = 3 ONLY ; */
/*            = 1, V_ecm(R) = V_0,  V_0 = V_Morse   ; */
/*            = 2, V_ecm(R) = V_0,  V_0 = V_Rydberg ; */
/*            = 3, V_ecm(R) = V_0,  V_0 = V_PG      . */

/*    The program will output the cofficients an of MS potential */
/*  to fort.4  when energy = 1 . */
/*                  ========== */

/*  NOTES : */

/* 1.>  !!!  Search for  "NP=" & "NP1=" !!! */

/* 2.>   *   Whenever the subroutine "funcv" or the function "fmin" */
/*         is used,  the "fvec" array defined in common is CHENGED  * */

/* ----------------------------------------------------------------- */
/* Main program */ int MAIN__(void)
{
    /* Format strings */
    static char fmt_510[] = "(//\002  ***  MORSE vibrational potentials  **"
	    "*\002/,\002   R(a0)            V(R)  \002/)";
    static char fmt_520[] = "(f10.6,1x,(1pe24.16))";
    static char fmt_515[] = "(//\002  ***  Harmonic Oscillator potentials "
	    "***\002/,\002   R(a0)            V(R)  \002/)";
    static char fmt_501[] = "(//\002  The Radial is Re = \002,f10.6,\002 Ans"
	    "tr.  = \002,f10.6,\002 a_0\002)";
    static char fmt_502[] = "(//\002  The Spectroscopic Constants are: \002/"
	    "/)";
    static char fmt_503[] = "(5x,a,1pe16.8,a,1pe16.8,a,/)";
    static char fmt_530[] = "(//\002 For Sun-Murrell-Sorbie potentials, Lamb"
	    "da = \002,f8.4,2x,\002:\002,//,\002   The dissociation energy   "
	    "De  = \002,1pe14.6,//,\002   The adjustable parameter beta = "
	    "\002,1pe14.6,//,\002                     D = De*beta = \002,1pe1"
	    "4.6,//,\002   and the nth force constants are \002,//5x,\002  n "
	    "  f(n; Har/ao**n) \002/)";
    static char fmt_550[] = "(5x,i3,2x,3(1pe16.8,x))";
    static char fmt_560[] = "(1pe24.16)";
    static char fmt_540[] = "(//\002 The expansion coefficients in Sun - "
	    "\002,/,\002Murrell - Sorbie potentials are :\002,//5x,\002  n   "
	    " a(n; 1/ao**n) \002/)";
    static char fmt_800[] = "(///13x,\002=#=  OUTPUT from \"vibPOT.f\"  =#="
	    " \002,//6x,\002The switches for NUMerical derivatives & E_vj :"
	    " \002,//3x,\002[  Np = 0, use f's from n_th derivatives; \002,/3"
	    "x,\002      = 1, use f's from V_ecm & perturbation theory . \002"
	    ",/3x,\002   Ny = 1, use  Vmorse  for analytical derivatives; "
	    "\002,/3x,\002      = 2, use Vrydberg for numerical  derivatives; "
	    "\002,/3x,\002            Expon. of Vryd. : a = b*dsqrt(amu/De)"
	    " \002,/3x,\002            b = We + bryd;    originally b = We ."
	    " \002,/3x,\002      = 3, use Vpseudo_gaussian for nume. derivati"
	    "ves. \002,/3x,\002  Ner = 0, do NOT cal. ro-vib energies E_vj ;"
	    " \002,/3x,\002      > 0, calc. E_vj from f'n by num. derivatives"
	    ". \002,/3x,\002             Set Ner = # of force constants wante"
	    "d.  \002,/3x,\002   nv -> nv is the # of vibrational states  wan"
	    "ted.   ]\002,//14x,\002         Np     Ny     Ner     nv \002,/2"
	    "3x,i2,5x,i2,5x,i2,6x,i2///3x,\002[ bryd - Variational constant u"
	    "sed to adjust Vryd(R), \002,/3x,\002              bryd is meanin"
	    "gless for Ny = 1, 3 .     \002,/3x,\002  hs1, hs2 --, Estimated "
	    "initial stepsize used by the \002,/3x,\002                 numer"
	    "ical derivative code \"dfirdr\" ; \002,/3x,\002                 "
	    "hs1 for Vrydberg,   hs2 for V_p-g  . ]\002,//3x,\002 bryd =\002,"
	    "f16.12,3x,\002hs1 =\002,f8.4,3x,\002hs2 =\002,f8.4,///3x,\002   "
	    "nj = is no. of rotational states in each vib. state :\002,//23x"
	    ",\002  v      nj  \002,/)";
    static char fmt_810[] = "(23x,i3,4x,i4)";
    static char fmt_820[] = "(///3x,\002Switches for scale SOME calc. VIB-RO"
	    "T constants :\002,//3x,\002[ aye, ..., are = 0, Zero calc. VIB. "
	    "constants \002,/3x,\002                     WeYe, WeZe, WeTe, We"
	    "Se, WeRe ; \002,/3x,\002                = 1, Do NOT change sign "
	    "of constants; \002,/3x,\002                =-1, Change the sign "
	    "of VIB. constants. \002,//3x,\002  abe, aae, age, \002,/3x,\002 "
	    " ae3  ---  ae7 = 0, Zero calc. ROT. constants  \002,/3x,\002    "
	    "                 B_e,Alpha_e,Gamma_e,Eta3,...,Eta7 ; \002,/3x"
	    ",\002                = 1, Do NOT change sign of constants; \002,"
	    "/3x,\002                =-1, Change the sign of ROT. constants."
	    " \002,//3x,\002  ade, abt, ax2, \002,/3x,\002  ax3  ---  ax7 = 0"
	    ", Zero calc. ROT. constants  \002,/3x,\002                     D"
	    "_e,Beta_e,Xsi2,Xsi3, ...,Xsi7 ; \002,/3x,\002                = 1"
	    ", Do NOT change sign of constants; \002,/3x,\002                "
	    "=-1, Change the sign of ROT. constants. ]\002,//8x,\002        a"
	    "ye  aze  ate  ase  are   \002,/15x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,"
	    "1x,f4.1,1x,//3x,\002        abe  aae  age  ae3  ae4  ae5  ae6  a"
	    "e7  \002,/10x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4"
	    ".1,1x,f4.1,//3x,\002        ade  abt  ax2  ax3  ax4  ax5  ax6  a"
	    "x7  \002,/10x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4"
	    ".1,1x,f4.1,/)";
    static char fmt_532[] = "(//5x,\002{ iab = 1, use x1 of (a*x*x + b*x +c)"
	    " as f(3);\002,/5x,\002      = 2, use x2 of (a*x*x + b*x +c) as f"
	    "(3)  }\002,//20x,\002 a =\002,1pe16.8,/20x,\002 b =\002,1pe16.8,"
	    "/20x,\002 c =\002,1pe16.8,//5x,\002  iab =\002,i2,\002;     x1 "
	    "=\002,1pe16.8,/20x,\002x2 =\002,1pe16.8,/)";
    static char fmt_534[] = "(//3x,\002For Sun-Feng (ECM) potentials, Lambda"
	    " = \002,f8.4,2x,\002:\002,//,\002   The dissociation energy   De"
	    "  = \002,1pe16.8,//,\002   Equili. intern. distance  Re  = \002,"
	    "1pe16.8,//3x,\002 f(n) are from perturbation-theory formulae "
	    "\002,/3x,\002          and for Rydberg-like ECM V(R);\002,//3x"
	    ",\002g_(n) are from numerical derivative code \002,//3x,\002  er"
	    "r are the errors of g_(n).\002,////14x,\002   The nth force cons"
	    "tants are : \002,//5x,\002  n   f(n; Har/ao**n)  g_(n; Har/ao**n"
	    ")     err(g) \002/)";
    static char fmt_536[] = "(///11x,\002The nth force constants in other un"
	    "its are : \002,//7x,\002    e(n) are the force constatns from an"
	    "alytical \002,/7x,\002         derivatives of Rydberg potential."
	    "       \002,//10x,\002      1 aJ = 1 attojoule = 0.229371 Har."
	    " \002,/10x,\002             1 ao = 0.529177249 A \002,//5x,\002 "
	    " n    f(n; aJ/A**n)    g_(n; aJ/A**n)   e(n; aJ/A**n)\002/)";
    static char fmt_750[] = "(///3x,\002Force constants ( h ) are from numer"
	    "ical \002,\002derivative code  \"dfridr\" ;\002,/3x,\002Force co"
	    "nstants ( p & g ) are from ANAlytical formulae .\002,/3x,\002   "
	    "      For  V_p-g(R),  2 =< n =< 11  ONLY  . \002,//5x,\002From  "
	    "  V_morse(R)      V_rydberg(R)       V_p-g(R)  \002,/5x,\002  n "
	    "  p(n; Har/ao**n)  g(n; Har/ao**n)  h(n; Har/ao**n) \002/)";
    static char fmt_760[] = "(//12x,\002The ERRORS for these force constants"
	    " are : \002,//5x,\002  n     e( V_morse )    e( V_rydberg )     "
	    "e( V_p-g ) \002/)";
    static char fmt_730[] = "(///6x,\002--%&*  Checking the qualities of for"
	    "ce\002,\002 constants  *&%-- \002,//8x,\002  As R --> inf == Rma"
	    "x, physically CORRECT force \002,/8x,\002         constants f_n "
	    "should satisfy : \002,//8x,\002V(R->inf=Rmax)_i = De = Sum_n  f_"
	    "n*(Rm_i - Re)**n/n! \002,//7x,\002    De == De_g :   Rm_g = Rmax"
	    " + Rless ;  i == g \002,/7x,\002    De == De_l :   Rm_l = Rmax -"
	    " Rless ;  i == l \002,//10x,\002For De_i(m) = De_i(Morse) :  f_n"
	    " == f_n(Morse)  \002,/10x,\002For De_i(r) = De_i(Rydbg) :  f_n ="
	    "= f_n(Rydbg)  \002,//10x,\002Dconv - Tolerance used to check if "
	    "V(Rmax) = De \002,//11x,\002De\002,11x,\002Rmax\002,10x,\002Rless"
	    "\002,11x,\002Dconv\002,8x,\002Nryd\002,/4x,4(1pe14.6,x),3x,i3,///"
	    "\002  n\002,5x,\002De_g(m)\002,7x,\002De_l(m)\002,7x,\002De_g(r"
	    ")\002,7x,\002De_l(r)\002,4x,\002{De_g-De_l}_r \002,/)";
    static char fmt_740[] = "(i3,x,5(1pe14.6))";
    static char fmt_742[] = "(//17x,\002V(R->inf=Rmax) = De  is reached with"
	    " : \002,//23x,\002  n\002,5x,\002De(Rydbg; a.u)\002,/23x,i3,3x,1"
	    "pe16.8,//23x,\002  n\002,5x,\002De(Morse; a.u)\002,/23x,i3,3x,1p"
	    "e16.8,/)";
    static char fmt_542[] = "(//\002 The expansion coefficients in SF potent"
	    "ial are : \002,//5x,\002  n    a(n; 1/ao**n) \002/)";
    static char fmt_570[] = "(//\002 --- Using Huxley-Murrell formulae ---"
	    " \002)";
    static char fmt_580[] = "(//\002 The nth force constants for\002,\002 Mu"
	    "rrell - Sorbie potentials are :\002,//5x,\002F2 =\002,1pe14.6,4x"
	    ",/5x,\002F3 =\002,1pe14.6,4x,/5x,\002F4 =\002,1pe14.6,4x)";
    static char fmt_590[] = "(//\002 The expansion coefficients in\002,\002 "
	    "Sun-Murrell-Sorbie potentials are :\002,//5x,\002a1 =\002,1pe14."
	    "6,4x,/5x,\002a2 =\002,1pe14.6,4x,/5x,\002a3 =\002,1pe14.6,4x)";
    static char fmt_600[] = "(//\002  ***  3-term Sun-Feng (ECM) potentials "
	    "***\002,/,\002             ( Before shifting ) \002,//,\002    R"
	    "(a0)            V(R)  \002/)";
    static char fmt_610[] = "(//\002  ***  ms-term Sun-Feng (ECM) potentials"
	    " ***;  \002,\002 ms =\002,i3,/8x,\002       ( Before shifting )"
	    " \002,//,\002     R(a0)            V(R)  \002/)";
    static char fmt_615[] = "(//\002  ***  ECM potential differences ***\002"
	    ",/,\002          ( Before shifting ) \002,//,\002    R(a0)      "
	    " V(R) - V(R;3-term) \002/)";
    static char fmt_655[] = "(//\002  ***  3-term Sun-Feng (ECM) potentials "
	    "***\002,/,\002             ( After shifting ) \002,//,\002    R("
	    "a0)         V(R; a.u.)  \002/)";
    static char fmt_660[] = "(//\002  ***  ms-term Sun-Feng (ECM) potentials"
	    " ***\002,/,\002             ( After shifting ) \002,//,\002    R"
	    "(a0)         V(R; a.u.)  \002/)";
    static char fmt_670[] = "(//4x,\002  ***  Morse potentials ***\002,/3x"
	    ",\002       ( After shifting ) \002,//,\002    R(a0)         V(R"
	    "; a.u.)  \002/)";
    static char fmt_680[] = "(//3x,\002  ***  Rydberg potentials ***\002,/"
	    "3x,\002       ( After shifting ) \002,//,\002    R(a0)         V"
	    "(R; a.u.)  \002/)";
    static char fmt_690[] = "(//1x,\002  ***  Pseudo-Gaussion (PG) potential"
	    "s ***\002,/7x,\002       ( After shifting ) \002,//,\002    R(a0"
	    ")         V(R; a.u.)  \002/)";
    static char fmt_605[] = "(//\002  ***  Huxley-Murrell-Sorbie potentials "
	    "***\002,/,\002             ( Before shifting ) \002,//,\002    R"
	    "(a0)            V(R)  \002/)";
    static char fmt_665[] = "(//\002  ***  Huxley-Murrell-Sorbie potentials "
	    "***\002,/,\002             ( After shifting ) \002,//,\002    R("
	    "a0)         V(R; a.u.)  \002/)";
    static char fmt_525[] = "(//9x,\002V_ecm(R) = V_model(R) for those R =< "
	    "Rtemp : \002,//3x,\002Re(a0)\002,4x,\002Rtemp(a0)\002,2x,\002V_e"
	    "cm(cut;R)\002,2x,\002V_model(R)\002,4x,\002V_dif(R)\002,/5(f10.6"
	    ",2x))";
    static char fmt_527[] = "(//5x,\002The cutoff values when Rtemp < Rmin"
	    " : \002,/5x,\002Re(a0)\002,4x,\002Rmin(a0)\002,4x,\002Rtemp(a0"
	    ")\002,3x,\002V_ecm(cut;R)\002,/4x,3(f12.8,2x),1pe16.8)";
    static char fmt_620[] = "(//\002  ***  Huxley-Murrell-Sorbie potentials "
	    "***\002,/,\002       ( After shifting ) \002,//,\002   The shift"
	    "ing constant is  Bshift =\002,f12.8,//\002    R(a0)         V(R;"
	    " a.u.)  \002/)";
    static char fmt_630[] = "(//\002  ***  Huxley-Murrell-Sorbie potentials "
	    "***\002,/,\002       ( After shifting ) \002,//,\002   The shift"
	    "ing constant is  Bshift =\002,f12.8,//\002    R(a0)         V(R;"
	    " cm-1)  \002/)";
    static char fmt_625[] = "(//\002  ***  Sun-Feng (ECM) potentials ***\002"
	    ",/,\002           ( After shifting ) \002,//,\002   The shifting"
	    " constant is  Bshift =\002,f12.8,//\002    R(a0)         V(R; a."
	    "u.)  \002/)";
    static char fmt_635[] = "(//\002  ***  Sun-Feng (ECM) potentials ***\002"
	    ",/,\002           ( After shifting ) \002,//,\002   The shifting"
	    " constant is  Bshift =\002,f12.8,//\002    R(a0)         V(R; cm"
	    "-1)  \002/)";
    static char fmt_522[] = "(f10.6,2x,f17.6)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_wsfe(cilist *), e_wsfe(void);
    double exp(doublereal);
    integer do_fio(integer *, char *, ftnlen);
    double pow_di(doublereal *, integer *), sqrt(doublereal), pow_dd(
	    doublereal *, doublereal *);
    integer s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int vpotdata_(void);
    static doublereal vrydberg, a, b;
    static integer i__, k, m, n;
    static doublereal r__, u[2500], v[2500], x, a1, b0, d1, f0, f2, f3, f4;
    static integer i1;
    extern /* Subroutine */ int quadratic_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal r1, r2, x1, x2, ae, fa, fb, fc, ff[40], ge[100], gg[100]
	    , an[20], ak;
    static integer ij, kk, nj[200], kj;
    static doublereal bk, rg;
    static integer kc, iu, np, ks;
    static doublereal rl;
    static integer km;
    static doublereal rr[2500];
    static integer nv;
    static doublereal fy, ge1[100], ge2[100], gg1[100], gg2[100], gg3[100], 
	    ge3[100], pf1, pf2, hs1, hs2;
    static integer ms0;
    static doublereal ss0, add;
    static integer iab, kab;
    static doublereal ama, amb;
    static integer nad;
    static doublereal ggf[100];
    static integer nff;
    static doublereal ffk, ggk;
    static integer ini;
    static doublereal vde;
    static integer ner;
    static doublereal vdm, err;
    static integer imu;
    static doublereal vpg, ery, amu1, vpg0[2500], vsf1, vsf2, ums0[2500], 
	    vms0[2500], amae, beta, cmao;
    extern doublereal facx_(integer *);
    static doublereal alph;
    static integer mall;
    static doublereal rdel, vdeg, vdif, vdel, temp;
    extern doublereal fryd_(integer *, doublereal *);
    static doublereal rmax, vshi;
    static integer nryd;
    static doublereal vmsg, utoh, vmsl, vmos[2500], vryd[2500], vmsp, vecm0, 
	    temp3;
    extern /* Subroutine */ int calff_(doublereal *, integer *, integer *, 
	    integer *);
    static doublereal alpha;
    extern /* Subroutine */ int getff_(doublereal *, integer *, integer *, 
	    integer *);
    static doublereal pbeta, ralph, dconv, rbohr, rtemp, rless, vtemp;
    static integer nturn;
    static doublereal aotoa0;
    extern /* Subroutine */ int calaaf_(void);
    static doublereal alamta;
    extern /* Subroutine */ int calevj_(integer *, integer *, integer *, 
	    integer *, doublereal *);
    static integer iforce;
    extern doublereal dfridr_(integer *, integer *, doublereal *, doublereal *
	    , doublereal *);
    static integer method;
    static doublereal ajtoau;
    static integer ishift;
    static doublereal autocm;
    extern doublereal fmorse_(integer *);
    static doublereal vmorse;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 5, 0, 0, 0 };
    static cilist io___3 = { 0, 5, 0, 0, 0 };
    static cilist io___11 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___16 = { 0, 6, 0, fmt_520, 0 };
    static cilist io___17 = { 0, 7, 0, fmt_520, 0 };
    static cilist io___18 = { 0, 5, 0, 0, 0 };
    static cilist io___20 = { 0, 6, 0, fmt_515, 0 };
    static cilist io___21 = { 0, 6, 0, fmt_520, 0 };
    static cilist io___22 = { 0, 5, 0, 0, 0 };
    static cilist io___27 = { 0, 5, 0, 0, 0 };
    static cilist io___31 = { 0, 5, 0, 0, 0 };
    static cilist io___35 = { 0, 4, 0, 0, 0 };
    static cilist io___37 = { 0, 5, 0, 0, 0 };
    static cilist io___39 = { 0, 5, 0, 0, 0 };
    static cilist io___43 = { 0, 5, 0, 0, 0 };
    static cilist io___44 = { 0, 5, 0, 0, 0 };
    static cilist io___45 = { 0, 5, 0, 0, 0 };
    static cilist io___46 = { 0, 5, 0, 0, 0 };
    static cilist io___54 = { 0, 6, 0, fmt_501, 0 };
    static cilist io___55 = { 0, 6, 0, fmt_502, 0 };
    static cilist io___56 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___57 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___58 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___59 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___60 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___61 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___62 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___63 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___64 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___65 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___66 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___67 = { 0, 6, 0, fmt_503, 0 };
    static cilist io___73 = { 0, 6, 0, fmt_530, 0 };
    static cilist io___74 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___75 = { 0, 27, 0, fmt_560, 0 };
    static cilist io___76 = { 0, 2, 0, fmt_560, 0 };
    static cilist io___77 = { 0, 6, 0, fmt_540, 0 };
    static cilist io___78 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___79 = { 0, 3, 0, fmt_560, 0 };
    static cilist io___80 = { 0, 12, 0, 0, 0 };
    static cilist io___85 = { 0, 12, 0, 0, 0 };
    static cilist io___89 = { 0, 12, 0, 0, 0 };
    static cilist io___94 = { 0, 12, 0, 0, 0 };
    static cilist io___97 = { 0, 12, 0, 0, 0 };
    static cilist io___99 = { 0, 12, 0, 0, 0 };
    static cilist io___100 = { 0, 12, 0, 0, 0 };
    static cilist io___101 = { 0, 12, 0, 0, 0 };
    static cilist io___103 = { 0, 0, 0, fmt_800, 0 };
    static cilist io___104 = { 0, 0, 0, fmt_810, 0 };
    static cilist io___105 = { 0, 0, 0, fmt_820, 0 };
    static cilist io___106 = { 0, 12, 0, 0, 0 };
    static cilist io___114 = { 0, 6, 0, fmt_532, 0 };
    static cilist io___127 = { 0, 6, 0, fmt_534, 0 };
    static cilist io___128 = { 0, 35, 0, fmt_534, 0 };
    static cilist io___129 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___130 = { 0, 35, 0, fmt_550, 0 };
    static cilist io___131 = { 0, 2, 0, fmt_560, 0 };
    static cilist io___132 = { 0, 27, 0, 0, 0 };
    static cilist io___133 = { 0, 6, 0, fmt_536, 0 };
    static cilist io___137 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___138 = { 0, 6, 0, fmt_750, 0 };
    static cilist io___139 = { 0, 35, 0, fmt_750, 0 };
    static cilist io___140 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___141 = { 0, 35, 0, fmt_550, 0 };
    static cilist io___142 = { 0, 6, 0, 0, 0 };
    static cilist io___143 = { 0, 35, 0, 0, 0 };
    static cilist io___144 = { 0, 6, 0, fmt_760, 0 };
    static cilist io___145 = { 0, 35, 0, fmt_760, 0 };
    static cilist io___146 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___147 = { 0, 35, 0, fmt_550, 0 };
    static cilist io___157 = { 0, 6, 0, fmt_730, 0 };
    static cilist io___158 = { 0, 35, 0, fmt_730, 0 };
    static cilist io___159 = { 0, 6, 0, fmt_740, 0 };
    static cilist io___160 = { 0, 35, 0, fmt_740, 0 };
    static cilist io___161 = { 0, 6, 0, 0, 0 };
    static cilist io___162 = { 0, 35, 0, 0, 0 };
    static cilist io___167 = { 0, 6, 0, fmt_742, 0 };
    static cilist io___168 = { 0, 6, 0, fmt_542, 0 };
    static cilist io___169 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___170 = { 0, 3, 0, fmt_560, 0 };
    static cilist io___171 = { 0, 5, 0, 0, 0 };
    static cilist io___173 = { 0, 6, 0, fmt_570, 0 };
    static cilist io___177 = { 0, 6, 0, fmt_580, 0 };
    static cilist io___178 = { 0, 2, 0, fmt_560, 0 };
    static cilist io___179 = { 0, 6, 0, fmt_590, 0 };
    static cilist io___180 = { 0, 8, 0, fmt_600, 0 };
    static cilist io___181 = { 0, 9, 0, fmt_610, 0 };
    static cilist io___182 = { 0, 10, 0, fmt_615, 0 };
    static cilist io___183 = { 0, 13, 0, fmt_655, 0 };
    static cilist io___184 = { 0, 14, 0, fmt_660, 0 };
    static cilist io___185 = { 0, 15, 0, fmt_670, 0 };
    static cilist io___186 = { 0, 16, 0, fmt_680, 0 };
    static cilist io___187 = { 0, 17, 0, fmt_690, 0 };
    static cilist io___188 = { 0, 8, 0, fmt_605, 0 };
    static cilist io___189 = { 0, 11, 0, fmt_665, 0 };
    static cilist io___214 = { 0, 8, 0, fmt_520, 0 };
    static cilist io___215 = { 0, 9, 0, fmt_520, 0 };
    static cilist io___216 = { 0, 10, 0, fmt_520, 0 };
    static cilist io___217 = { 0, 6, 0, fmt_525, 0 };
    static cilist io___218 = { 0, 6, 0, fmt_527, 0 };
    static cilist io___221 = { 0, 6, 0, fmt_620, 0 };
    static cilist io___222 = { 0, 11, 0, fmt_630, 0 };
    static cilist io___223 = { 0, 6, 0, fmt_625, 0 };
    static cilist io___224 = { 0, 11, 0, fmt_635, 0 };
    static cilist io___226 = { 0, 6, 0, fmt_520, 0 };
    static cilist io___227 = { 0, 11, 0, fmt_522, 0 };
    static cilist io___228 = { 0, 13, 0, fmt_520, 0 };
    static cilist io___229 = { 0, 14, 0, fmt_520, 0 };
    static cilist io___230 = { 0, 15, 0, fmt_520, 0 };
    static cilist io___231 = { 0, 16, 0, fmt_520, 0 };
    static cilist io___232 = { 0, 17, 0, fmt_520, 0 };
    static cilist io___233 = { 0, 18, 0, fmt_520, 0 };
    static cilist io___234 = { 0, 4, 0, fmt_560, 0 };


/* ----------------------------------------------------------------- */
/*  Read data : */
/*      M = 1, calculate Morse vibrational potentials; */
/*        = 2, calculate Harmonic Oscillate potentials. */
/*        = 3, calculate Sun-Feng (ECM) potentials. */
/*        = 4, calculate Murrell-Sorbie potentials. */

/*      N -- the number of potential energies wanted; */
/*     De -- the energy scaling constant ("ionization energy"); */
/*     Re -- the equilibrium internuclear distance (R in a.u.); */
/*     R1 -- the beginning R value; */
/*     R2 -- the end R value. */

/*   beta == width (adjustable) parameter for SMMS. */
/* alamta == lamta, variational adjustable parameter for ECM. */
/* ---------------------------------- */
/*  For Morse potential : */
/*  alpha -- exponential parameter; */
/* ---------------------------------- */
/*  For Harmonic oscillator : */
/*    amu -- the reduced mass (a.u.) */
/*     We -- vibrational constant (a.u.) */
/* ---------------------------------- */
/*  For Murrell-Sorbie potential : */
/*    a1, a2, a3 -- The expansion coefficients in units of */
/*                  a1(ao-1), a2(a0-2), a3(ao-3). */
/*        iforce -- =0, do NOT calculate spectroscopic parameters; */
/*                  =1, calculate f2, f3, f4 for Harmonic Oscillator. */
/*                  =2, calculate f2, f3, f4 for NON-Harmonic Oscillator. */
/*                       iforce=1, & 2 corresponds to Sun's formulae. */
/*                  =3, calculate f2, f3, f4 Using Sun-Feng formulae. */
/*                  =4, calculate f2, f3, f4 Using Hurley-Murrell formulae. */
/*                       (If a1 =/= 0.0, HM's a2 & a3 are also obtained) */

/*        ishift -- =0, do NOT shift the potential V(i); */
/*                  >0, shift V(i) such that the V_minimum is ZERO. */
/*           amu -- the reduced mass (in a.u.) of the molecule. */
/*            we -- vibrational constant (in a.u.) of the molecule. */

/*            ms -- The highest power in the expansion of the MS potential. */
/*                    For Huxley & Murrell, set ms = 3 ONLY ! */
/*                    For Sun's fformula,   set 10 > ms >= 3 . */

/*          mall -- = 1, calculate f2,...,f6, & potentials. */
/*                  > 1, calculate f2,...,f6 ONLY. */

/*   iu = 0, Input spectroscopy constants in H.a.u. & Re in a_0 (Bohr); */
/*      = 1, Input spectroscopy constants in cm_1 & Re in A~0 (anstrom). */

/*   For iforce > 0,  a1 =/= 0.0,  a2=0.0 (input value), the code */
/* will calculate a2 & a3. */
/*      ***************** */

/*  Nturn = 0, Set V_ecm(R) = V_MS(R)   for ms = 3 ONLY ! */
/*               For ms > 3 : */
/*        = 1, Set V_ecm(R) = V_Morse(R)   ; */
/*        = 2, Set V_ecm(R) = V_Rydberg(R) ; */
/*        = 3, Set V_ecm(R) = V_PG(R)      . */
/*   Nad  > 0, To shift V(R) by Add ; */
/*   Nad  = 0, To shift V(R) by b (= -De) which is found by code. */

/*    ini = 1, Cal. f3,... using standard method. */
/*        = 2, Cal. f3,... using approxi. method. */

/*  When ms = 4, 5  and  ini = 1 : */
/*    met = 1, Use Broyden method to compute f3,... */
/*        = 2, Use  Newton method to compute f3,... */

/*  The roots (x1, x2) of the quadratic equation : */
/*                  a*x*x + b*x + c = 0 */
/*    q = -0.5d0*(b + sign(1.0,b)*dsqrt( b*b - 4.0*a*c) ) */
/*                      x1 = q/a ;      x2 = c/q */
/*  When ms = 3, 5  : */
/*    iab = 1, Use x1 as f(3) */
/*        = 2, Use x2 as f(3) */

/*  When ms = 4  and  ini = 2 : */
/*    kab = 1, Use Eq.1 to get F5; */
/*        = 2, Use Eq.2 to get F5. */

/*  Np  = 1, Use f's from V_ECM & perturbation theory; */
/*      = 0, Use f's from Vrydberg & n_th derivatives. */
/*  Ny  = 1, Use Vmorse as V(R) for n_th derivatives. */
/*      = 2, Use Vrydberg as V(R) for n_th derivatives. */
/*             Exponential of Vryd. : a = b * dsqrt( amu/De ) */
/*              b=We + bryd;          originally, b=We. */
/*      = 3, Use Vpseudo-gaussian as V(R) for n_th derivatives. */
/* bryd - The variational constant used to adjust Vryd(R). */
/*            bryd is meaningless for Vmorse & Vp_g. */
/*  hs -- Estimated initial stepsize used by code "dfridr". */
/*            Good range for  hs :  0.001 --> 4.0  for H_2. */

/* ========================================================================= */
    s_rsle(&io___1);
    do_lio(&c__3, &c__1, (char *)&m, (ftnlen)sizeof(integer));
    e_rsle();
    if (m == 2) {
	goto L30;
    }
    if (m == 3) {
	goto L50;
    }
    if (m == 4) {
	goto L50;
    }
    s_rsle(&io___3);
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&alpha, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&r1, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&r2, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&add, (ftnlen)sizeof(doublereal));
    e_rsle();
/* ------------------------------------- */
    rdel = (r2 - r1) / n;
    r__ = r1 - rdel;
    s_wsfe(&io___11);
    e_wsfe();
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	r__ += rdel;
	x = (r__ - spectra_1.re) / spectra_1.re;
	rr[i__ - 1] = r__;
	v[i__ - 1] = spectra_1.de * (exp(alpha * -2. * x) - exp(-alpha * x) * 
		2.);
	s_wsfe(&io___16);
	do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&v[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___17);
	do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	d__1 = v[i__ - 1] + add;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L10: */
    }
    goto L900;
/* ------------------------------------------------------------------- */
L30:
    s_rsle(&io___18);
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&spectra_1.amu, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&r1, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&r2, (ftnlen)sizeof(doublereal));
    e_rsle();
/* Computing 2nd power */
    d__1 = spectra_1.we;
    ak = spectra_1.amu * (d__1 * d__1);
    rdel = (r2 - r1) / n;
    r__ = r1 - rdel;
    s_wsfe(&io___20);
    e_wsfe();
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	r__ += rdel;
	rr[i__ - 1] = r__;
/* Computing 2nd power */
	d__1 = r__ - spectra_1.re;
	v[i__ - 1] = ak * .5 * (d__1 * d__1);
	s_wsfe(&io___21);
	do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&v[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L40: */
    }
    goto L900;
/* ------------------------------------------------------------------- */
/*    an(1) MUST be solved using code FINDroot.f */
/*       De in Hartree; Re, R1 & R2 in ao; */
/*       we in Hartree; amu in AMU. */
/*            ff(2) in Hartree+/(a_o**2) */
/*            ff(3) in Hartree+/(a_o**3) */
/*            ff(4) in Hartree+/(a_o**4) */
/*            ff(5) in Hartree+/(a_o**5) */
/*            ff(6) in Hartree+/(a_o**6); */
/*    an(1) in 1/a_o     ;  an(2) in 1/(a_o**2); */
/*    an(3) in 1/(a_o**3);  an(4) in 1/(a_o**4); */
/*    an(5) in 1/(a_o**5). */
/* ------------------------------------------------------------------- */
L50:
    s_rsle(&io___22);
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&iforce, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ishift, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&fms_1.ms, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&mall, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&iu, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___27);
    do_lio(&c__5, &c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&beta, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&alamta, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&r1, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&r2, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&an[0], (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___31);
    do_lio(&c__5, &c__1, (char *)&add, (ftnlen)sizeof(doublereal));
    do_lio(&c__3, &c__1, (char *)&nad, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&nturn, (ftnlen)sizeof(integer));
    e_rsle();
/* --- */
    a1 = an[0];
    if (an[0] > 95.) {
	s_rsle(&io___35);
	do_lio(&c__5, &c__1, (char *)&a1, (ftnlen)sizeof(doublereal));
	e_rsle();
	an[0] = a1;
    }
/* --- */
    d1 = spectra_1.de * beta;
/* ------------------------------------------------- */
/*  Calculate force constants f & coefficients an. */
/* ================================================= */
    if (iforce == 0) {
	goto L75;
    }
    s_rsle(&io___37);
    do_lio(&c__3, &c__1, (char *)&imu, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___39);
    do_lio(&c__5, &c__1, (char *)&ama, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&amb, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&amu1, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___43);
    do_lio(&c__5, &c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.wexe, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.weye, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.weze, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___44);
    do_lio(&c__5, &c__1, (char *)&spectra_1.wete, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.wese, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.were, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___45);
    do_lio(&c__5, &c__1, (char *)&spectrb_1.be, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectrb_1.alphae, (ftnlen)sizeof(doublereal)
	    );
    do_lio(&c__5, &c__1, (char *)&spectrb_1.gamae, (ftnlen)sizeof(doublereal))
	    ;
    e_rsle();
    s_rsle(&io___46);
    do_lio(&c__5, &c__1, (char *)&spectrb_1.der, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectrb_1.betae, (ftnlen)sizeof(doublereal))
	    ;
    e_rsle();

/* --- Prepare CONVERSION factors : */
    utoh = 34231772.5;
    cmao = 5.29177249e-9;
    autocm = 219474.63067;
    aotoa0 = .529177249;
    ajtoau = .229371;
    amae = 1822.9163036026441;
    rbohr = .529177249;
/* ------------------------------------------------- */
/*     AMAE=mass_unit/mass_e */
/*     AMAE=1.6605655D-27/9.1093897D-31=1822.9163 */
/* ------------------------------------------------- */
    if (iu == 1) {
	r1 /= rbohr;
	r2 /= rbohr;
	spectra_1.re /= rbohr;
	spectra_1.we /= autocm;
	spectra_1.wexe /= autocm;
	spectra_1.weye /= autocm;
	spectra_1.weze /= autocm;
	spectra_1.wete /= autocm;
	spectra_1.wese /= autocm;
	spectra_1.were /= autocm;
	spectrb_1.be /= autocm;
	spectrb_1.alphae /= autocm;
	spectrb_1.gamae /= autocm;
	spectrb_1.der /= autocm;
	spectrb_1.betae /= autocm;
    }
    s_wsfe(&io___54);
    d__1 = spectra_1.re * rbohr;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___55);
    e_wsfe();
    s_wsfe(&io___56);
    do_fio(&c__1, "   We = ", (ftnlen)8);
    d__1 = spectra_1.we * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___57);
    do_fio(&c__1, " Wexe = ", (ftnlen)8);
    d__1 = spectra_1.wexe * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectra_1.wexe, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___58);
    do_fio(&c__1, " Weye = ", (ftnlen)8);
    d__1 = spectra_1.weye * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectra_1.weye, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___59);
    do_fio(&c__1, " Weze = ", (ftnlen)8);
    d__1 = spectra_1.weze * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectra_1.weze, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___60);
    do_fio(&c__1, " Wete = ", (ftnlen)8);
    d__1 = spectra_1.wete * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectra_1.wete, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___61);
    do_fio(&c__1, " Wese = ", (ftnlen)8);
    d__1 = spectra_1.wese * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectra_1.wese, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___62);
    do_fio(&c__1, " Were = ", (ftnlen)8);
    d__1 = spectra_1.were * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectra_1.were, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___63);
    do_fio(&c__1, "   Be = ", (ftnlen)8);
    d__1 = spectrb_1.be * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectrb_1.be, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___64);
    do_fio(&c__1, "alphe = ", (ftnlen)8);
    d__1 = spectrb_1.alphae * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectrb_1.alphae, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___65);
    do_fio(&c__1, "gamae = ", (ftnlen)8);
    d__1 = spectrb_1.gamae * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectrb_1.gamae, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___66);
    do_fio(&c__1, "Der = ", (ftnlen)6);
    d__1 = spectrb_1.der * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectrb_1.der, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
    s_wsfe(&io___67);
    do_fio(&c__1, "betae = ", (ftnlen)8);
    d__1 = spectrb_1.betae * autocm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " cm_1  = ", (ftnlen)9);
    do_fio(&c__1, (char *)&spectrb_1.betae, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, " H.a.u.", (ftnlen)7);
    e_wsfe();
/* ------------------------------------------------- */
/*  Change mass unit into atomic unit from amu : */
/* ------------------------------------------------- */
    if (imu == 1) {
	spectra_1.amu = amb * ama / (ama + amb) * amae;
    } else if (imu == 2) {
	spectra_1.amu = amu1 * amae;
    }
/* --- Prefactor : */
    ss0 = .021657164;
/* --- Calculate the nth spectroscopic parameters */
/*      f2 is in Hartree/(a_o**2).   f2 has CORRECT value */
/*    when multiplied by prefactor ss0. */
/* ---      using Weiguo Sun's formulae */
/*      For NON-Harmonic Oscillator : */
/*    f2 = ss0* 4*pi*pi*C*C * amu*we*we = ss0* 1.000004496 * amu*we*we */

/* Computing 2nd power */
    d__1 = autocm * spectra_1.we * cmao;
    f2 = ss0 * 1.000004496 * (utoh * spectra_1.amu) * (d__1 * d__1);
    ff[1] = f2;
/* Computing 2nd power */
    d__1 = a1;
    an[1] = (d__1 * d__1 - f2 / d1) * .5;
/* ------------------------------------------------------------- */
/* --- Sun's formulae : */
    if (iforce == 2) {
	i__1 = fms_1.ms + 1;
	for (k = 3; k <= i__1; ++k) {
	    temp = pow_di(&c_b218, &k) * 2. * (k - 1);
	    temp = temp * sqrt(pow_di(&f2, &k)) / facx_(&k);
	    d__1 = k / 2. - 1.;
	    ff[k - 1] = temp / pow_dd(&d1, &d__1);
/* L60: */
	}
	s_wsfe(&io___73);
	do_fio(&c__1, (char *)&alamta, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&beta, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&d1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = fms_1.ms + 1;
	for (k = 2; k <= i__1; ++k) {
	    s_wsfe(&io___74);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ff[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___75);
	    do_fio(&c__1, (char *)&ff[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___76);
	    do_fio(&c__1, (char *)&ff[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/*              write(2,560) (ff(k), k=2,ms+1) */
	if (mall > 1) {
	    goto L900;
	}
/* -------- */
	i__1 = fms_1.ms;
	for (k = 3; k <= i__1; ++k) {
	    d__1 = -a1;
	    temp = ff[k - 1] * -.5 / d1 + pow_di(&d__1, &k) * (k - 1) / facx_(
		    &k);
	    i__2 = k - 1;
	    for (i__ = 2; i__ <= i__2; ++i__) {
		d__1 = -a1;
		i__3 = k - i__;
		i__4 = k - i__;
		temp -= pow_di(&d__1, &i__3) * an[i__ - 1] / facx_(&i__4);
	    }
	    an[k - 1] = temp;
/* L65: */
	}
	s_wsfe(&io___77);
	e_wsfe();
	i__1 = fms_1.ms;
	for (k = 1; k <= i__1; ++k) {
	    s_wsfe(&io___78);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&an[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___79);
	    do_fio(&c__1, (char *)&an[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }

/* ===  Feng's formulae (used in ECM) : */

    if (iforce == 3) {
	s_rsle(&io___80);
	do_lio(&c__3, &c__1, (char *)&ini, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&method, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&iab, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&kab, (ftnlen)sizeof(integer));
	e_rsle();
	s_rsle(&io___85);
	do_lio(&c__3, &c__1, (char *)&np, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&fms_1.ny, (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&spectrb_1.bryd, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&hs1, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&hs2, (ftnlen)sizeof(doublereal));
	e_rsle();
	s_rsle(&io___89);
	do_lio(&c__3, &c__1, (char *)&nryd, (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&rmax, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&rless, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&dconv, (ftnlen)sizeof(doublereal));
	e_rsle();
	s_rsle(&io___94);
	do_lio(&c__3, &c__1, (char *)&ner, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nv, (ftnlen)sizeof(integer));
	e_rsle();
	s_rsle(&io___97);
	i__1 = nv;
	for (k = 1; k <= i__1; ++k) {
	    do_lio(&c__3, &c__1, (char *)&nj[k - 1], (ftnlen)sizeof(integer));
	}
	e_rsle();
	s_rsle(&io___99);
	do_lio(&c__5, &c__1, (char *)&eswitch_1.aye, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitch_1.aze, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitch_1.ate, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitch_1.ase, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitch_1.are, (ftnlen)sizeof(
		doublereal));
	e_rsle();
	s_rsle(&io___100);
	do_lio(&c__5, &c__1, (char *)&eswitc1_1.abe, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc1_1.aae, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc1_1.age, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae3, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae4, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae5, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae6, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae7, (ftnlen)sizeof(
		doublereal));
	e_rsle();
	s_rsle(&io___101);
	do_lio(&c__5, &c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc2_1.abt, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax2, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax3, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax4, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax5, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax6, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax7, (ftnlen)sizeof(
		doublereal));
	e_rsle();
	ij = 6;
	for (i__ = 1; i__ <= 2; ++i__) {
	    io___103.ciunit = ij;
	    s_wsfe(&io___103);
	    do_fio(&c__1, (char *)&np, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&fms_1.ny, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ner, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nv, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&spectrb_1.bryd, (ftnlen)sizeof(doublereal))
		    ;
	    do_fio(&c__1, (char *)&hs1, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&hs2, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    i__1 = nv;
	    for (k = 1; k <= i__1; ++k) {
		io___104.ciunit = ij;
		s_wsfe(&io___104);
		i__2 = k - 1;
		do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&nj[k - 1], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	    io___105.ciunit = ij;
	    s_wsfe(&io___105);
	    do_fio(&c__1, (char *)&eswitch_1.aye, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitch_1.aze, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitch_1.ate, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitch_1.ase, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitch_1.are, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc1_1.abe, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc1_1.aae, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc1_1.age, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc1_1.ae3, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc1_1.ae4, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc1_1.ae5, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc1_1.ae6, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc1_1.ae7, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc2_1.abt, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc2_1.ax2, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc2_1.ax3, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc2_1.ax4, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc2_1.ax5, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc2_1.ax6, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eswitc2_1.ax7, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    if (i__ == 1) {
		ij = 35;
	    }
	}

	i__1 = fms_1.ms + 1;
	for (k = 2; k <= i__1; ++k) {
	    s_rsle(&io___106);
	    do_lio(&c__5, &c__1, (char *)&ff[k - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
/* L111: */
	}
	ff[1] = spectra_1.amu * spectra_1.we * spectra_1.we;
	if (fms_1.ms == 4 || fms_1.ms == 5) {

/* -----   Calculate (f2,...,f5) for ms=4 or (f2,...,f6) for ms=5 : */

	    if (ini == 1) {

/* --- Sove NONlinear equations using standard method */

		calaaf_();
		nff = fms_1.ms - 1;
		calff_(ff, &nff, &c__40, &method);
	    } else {

/* --- Sove NONlinear equations using approximate method */

		getff_(ff, &c__40, &iab, &kab);
	    }

/* --------------------------------------------------- */
/* ---  Rydberg nth force constants */
/*           b = dsqrt(ff(2)/De) */
/*    	      do 112 i = 3, ms+1 */
/* 	        ff(i) = (-1)**i*(i-1)*ff(2)*b**(i-2) */
/* 112 		continue */
/* ---  Morse nth force constants */
/* c          a = dsqrt(0.5d0*ff(2)/De) */
/*           a = Re * dsqrt(0.5d0*ff(2)/De) */
/*           do 113 i = 3, ms+1 */
/* 	        ff(i) = ff(2)*(-1)**i*(2**(i-1)-1)*a**(i-2) */
/* 113 		continue */
/* --------------------------------------------------- */

	} else if (fms_1.ms == 3 && ini == 1) {
/* -----        Calculate f2, f3, and f4 ONLY */

	    a = spectra_1.amu * spectra_1.we;
	    fa = 1675. / (spectra_1.amu * 48. * spectra_1.we * spectra_1.we);
/* Computing 2nd power */
	    d__1 = spectra_1.amu * spectra_1.we * spectra_1.re;
	    fb = spectra_1.re * -12. * (d__1 * d__1);
/* Computing 3rd power */
	    d__1 = spectra_1.amu;
/* Computing 4th power */
	    d__2 = spectra_1.we, d__2 *= d__2;
/* Computing 2nd power */
	    d__3 = spectra_1.re;
	    fc = d__1 * (d__1 * d__1) * -36. * (d__2 * d__2) * (d__3 * d__3);
/* Computing 5th power */
	    d__1 = spectra_1.amu * spectra_1.we * spectra_1.re, d__2 = d__1, 
		    d__1 *= d__1;
	    fc -= spectra_1.re * 24. * (d__2 * (d__1 * d__1)) * 
		    spectrb_1.alphae;
/* Computing 2nd power */
	    d__1 = spectra_1.amu;
/* Computing 2nd power */
	    d__2 = spectra_1.we;
	    fc -= d__1 * d__1 * 335. * (d__2 * d__2) * spectra_1.wexe;
/* --- */
	    quadratic_(&fa, &fb, &fc, &x1, &x2);
	    s_wsfe(&io___114);
	    do_fio(&c__1, (char *)&fa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&fb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&fc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&iab, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&x1, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&x2, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    if (iab == 1) {
		ff[2] = x1;
	    } else {
		ff[2] = x2;
	    }
/* --- */
	    ff[3] = ff[2] * 5. * ff[2] / (spectra_1.amu * 3. * spectra_1.we * 
		    spectra_1.we);
/* Computing 2nd power */
	    d__1 = spectra_1.amu * spectra_1.we;
	    ff[3] -= d__1 * d__1 * 16. * spectra_1.wexe;
/* -------- */
/*              ff(5) = De*a1**5-10.0d0*ff(2)*a1**3-10.0d0*ff(3)*a1**2 */
/*    #  -  5.0d0*ff(4)*a1 */
/*              ff(6) = De*a1**6-15.0d0*ff(2)*a1**4-20.0d0*ff(3)*a1**3 */
/*    #  - 15.0d0*ff(4)*a1**2-6.0d0*ff(5)*a1 */
/* -------- */
	}
/* -------------------------------------------------------- */
/*   Generate the data needed by V(Pseudo-Gaussian; R) */
/* -------------------------------------------------------- */
	vpotdata_();
/* -------------------------------------------------------- */
/*  Calculate force constants ff == gg  using */
/*  the definitions :   f_n = [ d~nV(R) ]/[dR~n] */
/*  and  the function code "dfridr" . */

/*    hs -- an estimated initial stepsize; it needs */
/* not be small, but rather should be an increment */
/* in R' over which function fpot changes substantially. */
/*   err -- An estimate of the error in the derivative. */
/* ----- */
/*  Calculate numerical "force constants" for : */
/*    V_morse(R),  as  ks=1;     V_rydberg,  as  ks=2; */
/*    V_pg(R),     as  ks=3. */
/* -------------------------------------------------------- */
	ms0 = fms_1.ms;
	if (ner > 0 && fms_1.ms < ner + 1) {
	    ms0 = ner;
	}

	for (ks = 1; ks <= 3; ++ks) {
	    i__1 = nryd + 1;
	    for (k = 2; k <= i__1; ++k) {
		if (ks == 1) {
		    gg1[k - 1] = fmorse_(&k);
		    ge1[k - 1] = 0.;
		    if (ks == fms_1.ny) {
			gg[k - 1] = gg1[k - 1];
			ge[k - 1] = ge1[k - 1];
		    }
		} else if (ks == 2) {
		    gg2[k - 1] = dfridr_(&k, &ks, &spectra_1.re, &hs1, &err);
		    ge2[k - 1] = err;
		    ggf[k - 1] = fryd_(&k, &spectra_1.re);
		    if (ks == fms_1.ny) {
			gg[k - 1] = gg2[k - 1];
			ge[k - 1] = ge2[k - 1];
		    }
		} else if (ks == 3) {
		    gg3[k - 1] = dfridr_(&k, &ks, &spectra_1.re, &hs2, &err);
		    ge3[k - 1] = err;
		    if (ks == fms_1.ny) {
			gg[k - 1] = gg3[k - 1];
			ge[k - 1] = ge3[k - 1];
		    }
		}
	    }

	}
/* --------------------------------------------------- */
/*  Calculate coefficients an(i) using the above f's */
/*  OR the above g's. */
/* --------------------------------------------------- */
	s_wsfe(&io___127);
	do_fio(&c__1, (char *)&alamta, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___128);
	do_fio(&c__1, (char *)&alamta, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = ms0 + 1;
	for (k = 2; k <= i__1; ++k) {
	    s_wsfe(&io___129);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ff[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&gg[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ge[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___130);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ff[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&gg[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ge[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
/* - */
	    if (np == 0) {
		ff[k - 1] = gg[k - 1];
	    }
	    s_wsfe(&io___131);
	    do_fio(&c__1, (char *)&ff[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsle(&io___132);
	    do_lio(&c__5, &c__1, (char *)&ff[k - 1], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
	}
/* --- */
	s_wsfe(&io___133);
	e_wsfe();
	i__1 = ms0 + 1;
	for (k = 2; k <= i__1; ++k) {
	    ffk = ff[k - 1] / (ajtoau * pow_di(&aotoa0, &k));
	    ggk = gg[k - 1] / (ajtoau * pow_di(&aotoa0, &k));
	    ery = ggf[k - 1] / (ajtoau * pow_di(&aotoa0, &k));
	    s_wsfe(&io___137);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ffk, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ggk, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ery, (ftnlen)sizeof(doublereal));
	    e_wsfe();
/*              write(6,550) k, ffk, ggk, ge(k) */
	}
/* --------------------------------------------------- */
/* Print numerical "force constants" */
/* --------------------------------------------------- */
	s_wsfe(&io___138);
	e_wsfe();
	s_wsfe(&io___139);
	e_wsfe();
	i__1 = nryd;
	for (k = 2; k <= i__1; ++k) {
	    s_wsfe(&io___140);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&gg1[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&gg2[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&gg3[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___141);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&gg1[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&gg2[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&gg3[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    if (k == ms0 + 1) {
		s_wsle(&io___142);
		e_wsle();
		s_wsle(&io___143);
		e_wsle();
	    }
	}
/* --- */
	s_wsfe(&io___144);
	e_wsfe();
	s_wsfe(&io___145);
	e_wsfe();
	i__1 = ms0 + 1;
	for (k = 2; k <= i__1; ++k) {
	    s_wsfe(&io___146);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ge1[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ge2[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ge3[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___147);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ge1[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ge2[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ge3[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* --------------------------------------------------- */
/*  V(R->inf) = De = Sum_n  f_n*(R - Re)**n/n! */
/* --------------------------------------------------- */
	kk = 0;
	kj = 0;
	bk = 1.;
	vmsg = 0.;
	vmsl = 0.;
	vdeg = 0.;
	vdel = 0.;
	rg = rmax + rless;
	rl = rmax - rless;
	s_wsfe(&io___157);
	do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rmax, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rless, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dconv, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&nryd, (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___158);
	do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rmax, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rless, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dconv, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&nryd, (ftnlen)sizeof(integer));
	e_wsfe();
	i__1 = nryd;
	for (k = 2; k <= i__1; ++k) {
	    bk *= k;
	    d__1 = rg - spectra_1.re;
	    vmsg += gg1[k - 1] * pow_di(&d__1, &k) / bk;
	    d__1 = rl - spectra_1.re;
	    vmsl += gg1[k - 1] * pow_di(&d__1, &k) / bk;

	    d__1 = rg - spectra_1.re;
	    vdeg += ggf[k - 1] * pow_di(&d__1, &k) / bk;
	    d__1 = rl - spectra_1.re;
	    vdel += ggf[k - 1] * pow_di(&d__1, &k) / bk;
	    s_wsfe(&io___159);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&vmsg, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vmsl, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vdeg, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vdel, (ftnlen)sizeof(doublereal));
	    d__1 = vdeg - vdel;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___160);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&vmsg, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vmsl, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vdeg, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vdel, (ftnlen)sizeof(doublereal));
	    d__1 = vdeg - vdel;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
/*            if ( abs(vdeg - De) .lt. Dconv .and. kk .eq. 0 ) then */
	    if ((d__1 = vdeg - vdel, abs(d__1)) < dconv && kk == 0) {
		s_wsle(&io___161);
		e_wsle();
		s_wsle(&io___162);
		e_wsle();
		kk = 1;
		kc = k;
		vde = vdeg;
	    }
	    if ((d__1 = vmsg - vmsl, abs(d__1)) < dconv && kj == 0) {
		kj = 1;
		km = k;
		vdm = vmsg;
	    }
	}
	s_wsfe(&io___167);
	do_fio(&c__1, (char *)&kc, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vde, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&km, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vdm, (ftnlen)sizeof(doublereal));
	e_wsfe();
/* ================================================= */
/*  Calculate ro-vibrational energies E_vj for AB. */
/* ------------------------------------------------- */
	if (ner > 0) {
	    calevj_(&nv, nj, &c__200, &c__100, gg);
	}
/* ================================================= */

	if (mall > 1) {
	    goto L900;
	}
	an[1] = (a1 * a1 - ff[1] / spectra_1.de) * .5;
/* Computing 3rd power */
	d__1 = a1;
	an[2] = a1 * an[1] - d__1 * (d__1 * d__1) / 3. - ff[2] / (
		spectra_1.de * 6.);
	if (fms_1.ms >= 4) {
/* Computing 4th power */
	    d__1 = a1, d__1 *= d__1;
	    an[3] = (d__1 * d__1 - ff[1] * 6. * a1 * a1 / spectra_1.de - ff[2]
		     * 4. * a1 / spectra_1.de - ff[3] / spectra_1.de) / 24.;
	}
	if (fms_1.ms >= 5) {
/* Computing 5th power */
	    d__1 = a1, d__2 = d__1, d__1 *= d__1;
/* Computing 3rd power */
	    d__3 = a1;
	    an[4] = (d__2 * (d__1 * d__1) - ff[1] * 10. * (d__3 * (d__3 * 
		    d__3)) / spectra_1.de - ff[2] * 10. * a1 * a1 / 
		    spectra_1.de - ff[3] * 5. * a1 / spectra_1.de - ff[4] / 
		    spectra_1.de) / 120.;
	}
	s_wsfe(&io___168);
	e_wsfe();
	i__1 = fms_1.ms;
	for (k = 1; k <= i__1; ++k) {
	    s_wsfe(&io___169);
	    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&an[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___170);
	    do_fio(&c__1, (char *)&an[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

    }

/* ===  End Feng's formulae. */

/* ------------------------------------------------------------------ */
/* === Huxley & Murrell: J. Chem. Soc. Faraday Trans 2, 79, 323(1983) */
/*      For Huxley-Murrell-Sorbie's formulae : */

    if (iforce == 4) {
	s_rsle(&io___171);
	do_lio(&c__5, &c__1, (char *)&spectra_1.wexe, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&spectrb_1.be, (ftnlen)sizeof(doublereal)
		);
	do_lio(&c__5, &c__1, (char *)&ae, (ftnlen)sizeof(doublereal));
	e_rsle();
	if (iu == 1) {
	    spectra_1.wexe /= autocm;
	    spectrb_1.be /= autocm;
	    ae /= autocm;
	}
	s_wsfe(&io___173);
	e_wsfe();
/* Computing 2nd power */
	d__1 = spectrb_1.be;
	f0 = ae * spectra_1.we / (d__1 * d__1 * 6.) + 1.;
/* ----- For non-harmonic model */
/*          f2=1.000004496*amu*we*we */
/* ----- For harmonic model */
/* Computing 2nd power */
	d__1 = spectra_1.we;
	f2 = spectra_1.amu * (d__1 * d__1);
	f3 = f2 * -3. * f0 / spectra_1.re;
	f4 = f2 * (f0 * 15. * f0 - spectra_1.wexe * 8. / spectrb_1.be) / (
		spectra_1.re * spectra_1.re);
	ff[1] = f2;
	ff[2] = f3;
	ff[3] = f4;
	s_wsfe(&io___177);
	do_fio(&c__1, (char *)&f2, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&f3, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&f4, (ftnlen)sizeof(doublereal));
	e_wsfe();
	for (i__ = 2; i__ <= 6; ++i__) {
	    s_wsfe(&io___178);
	    do_fio(&c__1, (char *)&ff[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	if (a1 != 0.) {
/* Computing 2nd power */
	    d__1 = a1;
	    an[1] = (d__1 * d__1 - f2 / spectra_1.de) * .5;
/* Computing 3rd power */
	    d__1 = a1;
	    an[2] = a1 * an[1] - d__1 * (d__1 * d__1) / 3. - f3 / (
		    spectra_1.de * 6.);
	    s_wsfe(&io___179);
	    do_fio(&c__1, (char *)&an[0], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&an[1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&an[2], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/* --- Huxley-Murrell coefficients are ready for V_hms(R) */

    }
/* ================================================= */
/*  Calculate diatomic potentials if an are ready */
/* ------------------------------------------------- */
L75:
    rdel = (r2 - r1) / n;
    r__ = r1 - rdel;
    if (m == 3) {
	s_wsfe(&io___180);
	e_wsfe();
	s_wsfe(&io___181);
	do_fio(&c__1, (char *)&fms_1.ms, (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___182);
	e_wsfe();
	s_wsfe(&io___183);
	e_wsfe();
	s_wsfe(&io___184);
	e_wsfe();
	s_wsfe(&io___185);
	e_wsfe();
	s_wsfe(&io___186);
	e_wsfe();
	s_wsfe(&io___187);
	e_wsfe();
    } else if (m == 4) {
	s_wsfe(&io___188);
	e_wsfe();
	s_wsfe(&io___189);
	e_wsfe();
    }
    i1 = 0;
/* ------------------------- */
/* - Prepair  3-term Huxley-Murrell-Sorbie potential V_hms(R)=u(i) ; */
/*      OR : */
/* - Prepair  3-term Murrell-Sorbie potential V_ms(R)=u(i) ; */
/* - Prepair ms-term Murrell-Sorbie potential V_ms(R)=v(i) . */
/*    These potentials used Sun-Feng formulae for coefficients a(n). */
/* ========================= */
    rtemp = 0.;
    vtemp = 0.;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	r__ += rdel;
	if (r__ - spectra_1.re >= .5 && i1 == 0) {
	    i1 = i__;
	}
	x = r__ - spectra_1.re;
	temp = 1. / beta;
	i__2 = fms_1.ms;
	for (k = 1; k <= i__2; ++k) {
	    temp += an[k - 1] * pow_di(&x, &k);
	    if (k == 3) {
		temp3 = temp;
	    }
	}
	rr[i__ - 1] = r__;
/* --- */
/* -   v/u might be the V_hms(R) OR the V_ms(R; SF coefficients) */
/* --- */
	v[i__ - 1] = -spectra_1.de * beta * temp * exp(-a1 * beta * x);
	u[i__ - 1] = -spectra_1.de * beta * temp3 * exp(-a1 * beta * x);

/* === Prepair Sun-Feng (ECM) potential for M = 3 : */

	if (m == 3) {

/* - For Morse potential */
	    f2 = spectra_1.amu * spectra_1.we * spectra_1.we;
	    alph = sqrt(f2 * .5 / spectra_1.de);
	    vmorse = spectra_1.de * (exp(alph * -2. * x) - exp(-alph * x) * 
		    2.);
	    vmos[i__ - 1] = vmorse;

/* - For Rydberg potential */
	    ralph = sqrt(f2 / spectra_1.de);
	    vrydberg = -spectra_1.de * (ralph * x + 1.) * exp(-ralph * x);
	    vryd[i__ - 1] = vrydberg;

/* - For Pseudo-Gaussion (PG) potential */
	    pbeta = sqrt(f2 * spectra_1.re * spectra_1.re / spectra_1.de + 4.)
		     * .5 - 1.;
	    pf1 = 1. - spectra_1.re / r__ * (spectra_1.re / r__);
	    pf2 = 1. - r__ / spectra_1.re * (r__ / spectra_1.re);
	    vpg = -spectra_1.de * (pbeta * pf1 + 1.) * exp(pbeta * pf2);
	    vpg0[i__ - 1] = vpg;

/* - For Sun-Feng (ECM) potential : */

/* - fy is the force-field variational function  LAMDA(R) */

/*           fy = (x/R) */
/*           if (ms .eq. 3) then */
/*      	   fy = fy */
/*     	else if (ms .eq. 4) then */
/*      	   fy = (dabs(fy))**(1.0001d0)*x/dabs(x) */
/*     	else if (ms .eq. 5) then */
/*     	   fy = (dabs(fy))**(2.0001d0)*x/dabs(x) */
/*     	endif */
	    d__1 = x / r__;
	    i__2 = fms_1.ms - 2;
	    fy = pow_di(&d__1, &i__2);
	    fy *= 1. - exp(-pow_dd(&alamta, &c_b488) * (x / spectra_1.re));
	    fy *= alamta;
	    vms0[i__ - 1] = v[i__ - 1];
	    ums0[i__ - 1] = u[i__ - 1];
/* --- */
	    vsf1 = v[i__ - 1];
	    if (nturn < 2) {
		vsf2 = vmorse;
	    } else if (nturn == 2) {
		vsf2 = vrydberg;
	    } else if (nturn == 3) {
		vsf2 = vpg;
	    }
/* --- */
/*  Next v(i) is the ms-term ECM potential */
/* --- */
	    v[i__ - 1] = (fy + 1.) * vsf1 - fy * vsf2;
/* --- */
/*  Next u(i) is the 3-term ECM potential */
/* --- */
	    vsf1 = u[i__ - 1];
	    u[i__ - 1] = (fy + 1.) * vsf1 - fy * vsf2;
/* --- */
/* === Finish Sun-Feng (ECM) potential */

	}

/* L80: */
    }
/* ======================================== */
/* Find the switch value, Rtemp, of R */
/* ---------------------------------------- */
    r__ += rdel;
    rtemp = 0.;
    for (i__ = n; i__ >= 2; --i__) {
	r__ -= rdel;
	if (r__ < spectra_1.re && rtemp == 0.) {
	    rtemp = r__;
	    vecm0 = v[i__ - 1];
	    if (nturn == 0) {
		vdif = (d__1 = v[i__ - 1] - vms0[i__ - 1], abs(d__1));
		vmsp = vms0[i__ - 1];
	    } else if (nturn == 1) {
		vdif = (d__1 = v[i__ - 1] - vmos[i__ - 1], abs(d__1));
		vmsp = vmos[i__ - 1];
	    } else if (nturn == 2) {
		vdif = (d__1 = v[i__ - 1] - vryd[i__ - 1], abs(d__1));
		vmsp = vryd[i__ - 1];
	    } else if (nturn == 3) {
		vdif = (d__1 = v[i__ - 1] - vpg0[i__ - 1], abs(d__1));
		vmsp = vpg0[i__ - 1];
	    }
	    vtemp = vdif;
	}
/* L100: */
    }
/* --------------------------------------------- */
/* Set v(i) = Vecm = vmos(i)  for R =< Rtemp */
/* --------------------------------------------- */
    r__ = r1 - rdel;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	r__ += rdel;
	if (m == 3 && r__ < spectra_1.re) {
	    if (nturn == 0) {
		v[i__ - 1] = vms0[i__ - 1];
		u[i__ - 1] = ums0[i__ - 1];
	    } else if (nturn == 1) {
		v[i__ - 1] = vmos[i__ - 1];
		u[i__ - 1] = vmos[i__ - 1];
	    } else if (nturn == 2) {
		v[i__ - 1] = vryd[i__ - 1];
		u[i__ - 1] = vryd[i__ - 1];
	    } else if (nturn == 3) {
		v[i__ - 1] = vpg0[i__ - 1];
		u[i__ - 1] = vpg0[i__ - 1];
	    }
	}
	s_wsfe(&io___214);
	do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&u[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___215);
	do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&v[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___216);
	do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	d__1 = v[i__ - 1] - u[i__ - 1];
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* --- */
    if (m == 3) {
	if (rtemp > r1) {
	    s_wsfe(&io___217);
	    do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&rtemp, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vecm0, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vmsp, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vtemp, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	} else {
	    s_wsfe(&io___218);
	    do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&r1, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&rtemp, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vtemp, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
/* - */
    b0 = 0.;
    if (ishift == 0) {
	goto L150;
    }
    i__1 = n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (i__ <= i1 && v[i__ - 1] < v[i__ - 2]) {
	    b = v[i__ - 1];
	}
    }

    if (b < 0.) {
	b = -b;
    }

    if (nad == 0) {
	b0 = b;
    }
    if (nad > 0) {
	b0 = add;
    }

/* --- Shift potential v(i) such that the minimum is zero. */

    if (m == 4) {
	s_wsfe(&io___221);
	do_fio(&c__1, (char *)&b0, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___222);
	do_fio(&c__1, (char *)&b0, (ftnlen)sizeof(doublereal));
	e_wsfe();
    } else if (m == 3) {
	s_wsfe(&io___223);
	do_fio(&c__1, (char *)&b0, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___224);
	do_fio(&c__1, (char *)&b0, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

L150:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vshi = v[i__ - 1] + b0;
	s_wsfe(&io___226);
	do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vshi, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___227);
	do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	d__1 = vshi * autocm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	if (m == 3) {
	    s_wsfe(&io___228);
	    do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = u[i__ - 1] + b0;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___229);
	    do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vshi, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___230);
	    do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = vmos[i__ - 1] + b0;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___231);
	    do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = vryd[i__ - 1] + b0;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___232);
	    do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = vpg0[i__ - 1] + b0;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___233);
	    do_fio(&c__1, (char *)&rr[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vshi, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* L200: */
    }

/* ------ Print the cofficients an of HMS or SF (ECM) potential . */
/*         write(4,720) */
    i__1 = fms_1.ms;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*         write(4,550) i, an(i) */
	s_wsfe(&io___234);
	do_fio(&c__1, (char *)&an[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L222: */
    }
/* --------------  End print */

/* ==================================================================== */
/* L521: */
/* 525 format(//3x,'The largest R (< R_e) of |V_ecm - V_ms| > ', */
/*    # 1pe12.6,' is ',1pe9.3,' a_0') */
/*    #10x,'V_dif(R)',/2x,f10.6,1x,3(1pe16.8,2x) ) */
/* 527 format(//3x,'The value of |V_EMS - V_MS| between R = ', */
/*    # f10.6,' and ',f10.6,//3x,'are all < ',1pe12.6) */
/*    #//5x,"  n    f(n; aJ/A**n)    g_(n; aJ/A**n)      err(g) "/) */
/* 546 format(//5x,'Equilibrium internuclear distance',4x, */
/*    #'Re =',1PE16.8,/39x,'Rstep =',1PE16.8, */
/*    #//7x,'n',8x,'R(ao)',7x,'g_n(2;Har/ao**2)     err(g) '/) */
/* 600 format(//'  ***  3-term Sun-Murrell-Sorbie potentials ***',/, */
/* 610 format(//'  ***  ms-term Sun-Murrell-Sorbie potentials ***',/, */
/* 615 format(//'  ***  Sun-Murrell-Sorbie potentials ***',/, */
/* L720: */

L900:
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */


doublereal facx_(integer *i__)
{
    /* Initialized data */

    static doublereal table[15] = { 1.,2.,6.,24.,120.,720.,5040.,40320.,
	    362880.,3628800.,39916800.,479001600.,6227020800.,87178291200.,
	    1.307674368e12 };

    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer j;
    static doublereal fj;

/* ----------------------------------------------------------------- */
/*     THIS IS A FACTORIAL FUNCTION  I! */
/* ----------------------------------------------------------------- */
/* --- */
    ret_val = 1.;
    if (*i__ < 0) {
	goto L95;
    } else if (*i__ == 0) {
	goto L100;
    } else {
	goto L10;
    }
L10:
    if (*i__ - 15 <= 0) {
	goto L20;
    } else {
	goto L30;
    }
L20:
    ret_val = table[*i__ - 1];
    goto L200;
L30:
    fj = 16.;
    ret_val = table[14];
    i__1 = *i__;
    for (j = 16; j <= i__1; ++j) {
	ret_val *= fj;
/* L40: */
	fj += 1.;
    }
    goto L200;
L95:
    ret_val = 0.;
L100:
L200:
    return ret_val;
} /* facx_ */

/* === */

/* Subroutine */ int getff_(doublereal *ff, integer *n1, integer *iab, 
	integer *kab)
{
    /* Format strings */
    static char fmt_100[] = "(//5x,\002{ iab = 1, use x1 of (a*x*x + b*x +c)"
	    " as f(3);\002,/5x,\002      = 2, use x2 of (a*x*x + b*x +c) as f"
	    "(3)  }\002,//20x,\002 a =\002,1pe16.8,/20x,\002 b =\002,1pe16.8,"
	    "/20x,\002 c =\002,1pe16.8,//5x,\002  iab =\002,i2,\002;     x1 "
	    "=\002,1pe16.8,/20x,\002x2 =\002,1pe16.8,/)";
    static char fmt_110[] = "(//5x,\002{ iab = 1, use x1 of (A*x*x + B*x +C)"
	    " as f(5);\002,/5x,\002      = 2, use x2 of (A*x*x + B*x +C) as f"
	    "(5)  }\002,//20x,\002 A =\002,1pe16.8,/20x,\002 B =\002,1pe16.8,"
	    "/20x,\002 C =\002,1pe16.8,//5x,\002  iab =\002,i2,\002;     x1 "
	    "=\002,1pe16.8,/20x,\002x2 =\002,1pe16.8,/)";

    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double sqrt(doublereal);

    /* Local variables */
    static doublereal a, f0, f5;
    extern /* Subroutine */ int quadratic_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal x1, x2, aa, ab, ac, fa, fb, fc, f5a, f6a, f5b, f6b, 
	    f56a, f56b;

    /* Fortran I/O blocks */
    static cilist io___244 = { 0, 6, 0, fmt_100, 0 };
    static cilist io___256 = { 0, 6, 0, fmt_110, 0 };



/* -- Use the Eqs. of ms=3 to get the approx. (F3, F4) for later using */

    /* Parameter adjustments */
    --ff;

    /* Function Body */
    a = spectra_1.amu * spectra_1.we;
    fa = 1675. / (spectra_1.amu * 48. * spectra_1.we * spectra_1.we);
/* Computing 2nd power */
    d__1 = spectra_1.amu * spectra_1.we * spectra_1.re;
    fb = spectra_1.re * -12. * (d__1 * d__1);
/* Computing 3rd power */
    d__1 = spectra_1.amu;
/* Computing 4th power */
    d__2 = spectra_1.we, d__2 *= d__2;
/* Computing 2nd power */
    d__3 = spectra_1.re;
    fc = d__1 * (d__1 * d__1) * -36. * (d__2 * d__2) * (d__3 * d__3);
/* Computing 5th power */
    d__1 = spectra_1.amu * spectra_1.we * spectra_1.re, d__2 = d__1, d__1 *= 
	    d__1;
    fc -= spectra_1.re * 24. * (d__2 * (d__1 * d__1)) * spectrb_1.alphae;
/* Computing 2nd power */
    d__1 = spectra_1.amu;
/* Computing 2nd power */
    d__2 = spectra_1.we;
    fc -= d__1 * d__1 * 335. * (d__2 * d__2) * spectra_1.wexe;
    quadratic_(&fa, &fb, &fc, &x1, &x2);
    s_wsfe(&io___244);
    do_fio(&c__1, (char *)&fa, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&fb, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&fc, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&(*iab), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&x1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&x2, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (*iab == 1) {
	ff[3] = x1;
    } else {
	ff[3] = x2;
    }
    ff[4] = ff[3] * 5. * ff[3] / (spectra_1.amu * 3. * spectra_1.we * 
	    spectra_1.we);
/* Computing 2nd power */
    d__1 = spectra_1.amu * spectra_1.we;
    ff[4] -= d__1 * d__1 * 16. * spectra_1.wexe;

/* -- Solve F5 for ms=4 OR (F5, F6) for ms=5 : */

    if (fms_1.ms == 4) {
	if (*kab == 1) {
/* Computing 4th power */
	    d__1 = spectra_1.re, d__1 *= d__1;
/* Computing 5th power */
	    d__2 = a, d__3 = d__2, d__2 *= d__2;
/* Computing 6th power */
	    d__4 = spectra_1.re, d__4 *= d__4;
	    f5 = -3. / (spectra_1.amu * 2. * a * (d__1 * d__1)) + ff[4] * 
		    335. / (d__3 * (d__2 * d__2) * 384. * (d__4 * (d__4 * 
		    d__4)));
/* Computing 2nd power */
	    d__1 = a;
/* Computing 4th power */
	    d__2 = spectra_1.re, d__2 *= d__2;
/* Computing 3rd power */
	    d__3 = a;
/* Computing 3rd power */
	    d__4 = spectra_1.re;
	    f0 = (95. / (d__1 * d__1 * 8. * (d__2 * d__2)) + 1.) * ff[3] / (
		    d__3 * (d__3 * d__3) * 2. * (d__4 * (d__4 * d__4)));
/* Computing 5th power */
	    d__1 = a, d__2 = d__1, d__1 *= d__1;
/* Computing 5th power */
	    d__3 = spectra_1.re, d__4 = d__3, d__3 *= d__3;
	    ff[5] = d__2 * (d__1 * d__1) * 96. * (d__4 * (d__3 * d__3)) / 19. 
		    * (f5 - f0 - spectrb_1.alphae);
	} else {
/* Computing 2nd power */
	    d__1 = ff[3];
/* Computing 3rd power */
	    d__2 = a;
	    f5 = spectra_1.wexe - d__1 * d__1 * 5. / (spectra_1.we * 48. * (
		    d__2 * (d__2 * d__2))) + ff[4] / (a * 16. * a);
/* Computing 5th power */
	    d__1 = a, d__2 = d__1, d__1 *= d__1;
	    ff[5] = sqrt(spectra_1.we * 92160. * (d__2 * (d__1 * d__1)) * f5 /
		     217.);
	}
    } else if (fms_1.ms == 5) {
/* Computing 5th power */
	d__1 = a, d__2 = d__1, d__1 *= d__1;
/* Computing 5th power */
	d__3 = spectra_1.re, d__4 = d__3, d__3 *= d__3;
	f5a = 19. / (d__2 * (d__1 * d__1) * 96. * (d__4 * (d__3 * d__3)));
/* Computing 4th power */
	d__1 = spectra_1.re, d__1 *= d__1;
/* Computing 5th power */
	d__2 = a, d__3 = d__2, d__2 *= d__2;
/* Computing 4th power */
	d__4 = spectra_1.re, d__4 *= d__4;
	f6a = -(134939. / (a * 720. * a * (d__1 * d__1)) + 5.) / (d__3 * (
		d__2 * d__2) * 256. * (d__4 * d__4));
/* Computing 5th power */
	d__1 = a, d__2 = d__1, d__1 *= d__1;
	f5b = 217. / (spectra_1.we * 92160. * (d__2 * (d__1 * d__1)));
/* Computing 5th power */
	d__1 = a, d__2 = d__1, d__1 *= d__1;
	f6b = ff[4] * 177. / (spectra_1.we * 55296. * (d__2 * (d__1 * d__1)));
/* Computing 4th power */
	d__1 = spectra_1.re, d__1 *= d__1;
/* Computing 4th power */
	d__2 = spectra_1.re, d__2 *= d__2;
	f0 = -(175. / (a * 8. * a * (d__1 * d__1)) + 3.) / (spectra_1.amu * 
		2. * a * (d__2 * d__2));
/* Computing 4th power */
	d__1 = spectra_1.re, d__1 *= d__1;
/* Computing 3rd power */
	d__2 = a;
/* Computing 3rd power */
	d__3 = spectra_1.re;
	f0 -= (95. / (a * 8. * a * (d__1 * d__1)) + 1.) * ff[3] / (d__2 * (
		d__2 * d__2) * 2. * (d__3 * (d__3 * d__3)));
/* Computing 5th power */
	d__1 = a, d__2 = d__1, d__1 *= d__1;
/* Computing 6th power */
	d__3 = spectra_1.re, d__3 *= d__3;
	f56a = f0 + ff[4] * 335. / (d__2 * (d__1 * d__1) * 384. * (d__3 * (
		d__3 * d__3))) - spectrb_1.alphae;
/* Computing 2nd power */
	d__1 = ff[3];
/* Computing 3rd power */
	d__2 = a;
	f56b = spectra_1.wexe - d__1 * d__1 * 5. / (spectra_1.we * 48. * (
		d__2 * (d__2 * d__2))) + ff[4] / (a * 16. * a);
	aa = f5b;
	ab = -f6b * f5a / f6a;
	ac = f6b * f56a / f6a - f56b;
/* - */
	quadratic_(&aa, &ab, &ac, &x1, &x2);
	s_wsfe(&io___256);
	do_fio(&c__1, (char *)&aa, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ab, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ac, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*iab), (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&x1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&x2, (ftnlen)sizeof(doublereal));
	e_wsfe();
	if (*iab == 1) {
	    ff[5] = x1;
	} else {
	    ff[5] = x2;
	}
/* - */
/*        if (iab .eq. 1)then */
/*          ff(5) = 0.5d0*( - Ab + dsqrt( Ab*Ab - 4.0*Aa*Ac ) )/Aa */
/*        else */
/*          ff(5) = 0.5d0*( - Ab - dsqrt( Ab*Ab - 4.0*Aa*Ac ) )/Aa */
/*        endif */
/* - */
	ff[6] = (f56a - f5a * ff[5]) / f6a;
    }
/* - */
/* - */
    return 0;
} /* getff_ */

/* === */
/* Subroutine */ int calaaf_(void)
{
    /* Format strings */
    static char fmt_100[] = "(1x,5(1pe16.7))";

    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4, d__5;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer k;
    static doublereal alp;

    /* Fortran I/O blocks */
    static cilist io___259 = { 0, 24, 0, fmt_100, 0 };


/* --- */
    alp = spectra_1.amu * spectra_1.we;
/* Computing 3rd power */
    d__1 = alp;
    aafcom_1.aaf[0] = 5. / (spectra_1.we * 48. * (d__1 * (d__1 * d__1)));
/* Computing 2nd power */
    d__1 = alp;
    aafcom_1.aaf[4] = -1. / (d__1 * d__1 * 16.);
/* Computing 5th power */
    d__1 = alp, d__2 = d__1, d__1 *= d__1;
    aafcom_1.aaf[8] = 217. / (spectra_1.we * 92160. * (d__2 * (d__1 * d__1)));
/* Computing 5th power */
    d__1 = alp, d__2 = d__1, d__1 *= d__1;
    aafcom_1.aaf[12] = 177. / (spectra_1.we * 55296. * (d__2 * (d__1 * d__1)))
	    ;
    aafcom_1.aaf[16] = -spectra_1.wexe;
/* Computing 4th power */
    d__1 = alp, d__1 *= d__1;
    aafcom_1.aaf[1] = -17. / (spectra_1.we * 2304. * (d__1 * d__1));
/* Computing 4th power */
    d__1 = alp, d__1 *= d__1;
    aafcom_1.aaf[5] = -7. / (spectra_1.we * 288. * (d__1 * d__1));
/* Computing 3rd power */
    d__1 = alp;
    aafcom_1.aaf[9] = 1. / (d__1 * (d__1 * d__1) * 288.);
/* Computing 6th power */
    d__1 = alp, d__1 *= d__1;
    aafcom_1.aaf[13] = -829. / (spectra_1.we * 3317760. * (d__1 * (d__1 * 
	    d__1)));
    aafcom_1.aaf[17] = -spectra_1.weye;
/* Computing 5th power */
    d__1 = alp, d__2 = d__1, d__1 *= d__1;
    aafcom_1.aaf[2] = -7. / (spectra_1.we * 5120. * (d__2 * (d__1 * d__1)));
/* Computing 5th power */
    d__1 = alp, d__2 = d__1, d__1 *= d__1;
    aafcom_1.aaf[6] = -11. / (spectra_1.we * 9216. * (d__2 * (d__1 * d__1)));
    aafcom_1.aaf[10] = -spectra_1.weze;
/* Computing 2nd power */
    d__1 = alp;
/* Computing 4th power */
    d__2 = spectra_1.re, d__2 *= d__2;
/* Computing 3rd power */
    d__3 = alp;
/* Computing 3rd power */
    d__4 = spectra_1.re;
    aafcom_1.aaf[3] = -(95. / (d__1 * d__1 * 8. * (d__2 * d__2)) + 1.) / (
	    d__3 * (d__3 * d__3) * 2. * (d__4 * (d__4 * d__4)));
/* Computing 5th power */
    d__1 = alp, d__2 = d__1, d__1 *= d__1;
/* Computing 6th power */
    d__3 = spectra_1.re, d__3 *= d__3;
    aafcom_1.aaf[7] = 335. / (d__2 * (d__1 * d__1) * 384. * (d__3 * (d__3 * 
	    d__3)));
/* Computing 5th power */
    d__1 = alp, d__2 = d__1, d__1 *= d__1;
/* Computing 5th power */
    d__3 = spectra_1.re, d__4 = d__3, d__3 *= d__3;
    aafcom_1.aaf[11] = -19. / (d__2 * (d__1 * d__1) * 96. * (d__4 * (d__3 * 
	    d__3)));
/* Computing 2nd power */
    d__1 = alp;
/* Computing 4th power */
    d__2 = spectra_1.re, d__2 *= d__2;
/* Computing 5th power */
    d__3 = alp, d__4 = d__3, d__3 *= d__3;
/* Computing 4th power */
    d__5 = spectra_1.re, d__5 *= d__5;
    aafcom_1.aaf[15] = (134939. / (d__1 * d__1 * 720. * (d__2 * d__2)) + 5.) /
	     (d__4 * (d__3 * d__3) * 256. * (d__5 * d__5));
/* Computing 2nd power */
    d__1 = alp;
/* Computing 4th power */
    d__2 = spectra_1.re, d__2 *= d__2;
/* Computing 4th power */
    d__3 = spectra_1.re, d__3 *= d__3;
    aafcom_1.aaf[19] = -(175. / (d__1 * d__1 * 8. * (d__2 * d__2)) + 3.) / (
	    spectra_1.amu * 2. * alp * (d__3 * d__3)) - spectrb_1.alphae;
    if (fms_1.ms == 4) {
/* Computing 4th power */
	d__1 = spectra_1.re, d__1 *= d__1;
	aafcom_1.aaf[19] = -3. / (spectra_1.amu * 2. * alp * (d__1 * d__1)) - 
		spectrb_1.alphae;
    }

    for (k = 1; k <= 4; ++k) {
	s_wsfe(&io___259);
	do_fio(&c__1, (char *)&aafcom_1.aaf[k - 1], (ftnlen)sizeof(doublereal)
		);
	do_fio(&c__1, (char *)&aafcom_1.aaf[k + 3], (ftnlen)sizeof(doublereal)
		);
	do_fio(&c__1, (char *)&aafcom_1.aaf[k + 7], (ftnlen)sizeof(doublereal)
		);
	do_fio(&c__1, (char *)&aafcom_1.aaf[k + 11], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&aafcom_1.aaf[k + 15], (ftnlen)sizeof(
		doublereal));
	e_wsfe();
/* L10: */
    }

    return 0;
} /* calaaf_ */

/* === */
/* Subroutine */ int calff_(doublereal *ff, integer *n, integer *n1, integer *
	method)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer k;
    static doublereal x[40], fvec[40];
    extern /* Subroutine */ int newt_(doublereal *, integer *, integer *, 
	    logical *);
    static logical check;
    extern /* Subroutine */ int funcv_(integer *, integer *, doublereal *, 
	    doublereal *), broydn_(doublereal *, integer *, integer *, 
	    logical *);

    /* Fortran I/O blocks */
    static cilist io___264 = { 0, 25, 0, 0, 0 };


/* ----------------------------------------------------- */
/* N=ms-1; N=3 : f3, f4, f5     => x1, x2, x3 ; */
/*         N=4 : f3, f4, f5, f6 => x1, x2, x3, x4. */
/* ----------------------------------------------------- */
    /* Parameter adjustments */
    --ff;

    /* Function Body */
    check = FALSE_;
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	x[k - 1] = ff[k + 2];
/* L10: */
    }
    if (*method == 1) {
	broydn_(x, n, &c__40, &check);
    } else if (*method == 2) {
	newt_(x, n, &c__40, &check);
    }

    funcv_(&c__40, n, x, fvec);
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	s_wsle(&io___264);
	do_lio(&c__9, &c__1, "fvec(", (ftnlen)5);
	do_lio(&c__3, &c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ") = ", (ftnlen)4);
	do_lio(&c__5, &c__1, (char *)&fvec[k - 1], (ftnlen)sizeof(doublereal))
		;
	e_wsle();
/* L20: */
    }
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	ff[k + 2] = x[k - 1];
/* L30: */
    }
    return 0;
} /* calff_ */

/* === */
/* Subroutine */ int funcv_(integer *n1, integer *n, doublereal *x, 
	doublereal *fvec)
{
    /* System generated locals */
    doublereal d__1, d__2;

/* --- */
    /* Parameter adjustments */
    --fvec;
    --x;

    /* Function Body */
    if (fms_1.ms == 5) {
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[3];
	fvec[1] = aafcom_1.aaf[0] * (d__1 * d__1) + aafcom_1.aaf[4] * x[2] + 
		aafcom_1.aaf[8] * (d__2 * d__2) + aafcom_1.aaf[12] * x[2] * x[
		4] + aafcom_1.aaf[16];
/* Computing 2nd power */
	d__1 = x[2];
/* Computing 2nd power */
	d__2 = x[4];
	fvec[2] = aafcom_1.aaf[1] * (d__1 * d__1) + aafcom_1.aaf[5] * x[1] * 
		x[3] + aafcom_1.aaf[9] * x[4] + aafcom_1.aaf[13] * (d__2 * 
		d__2) + aafcom_1.aaf[17];
/* Computing 2nd power */
	d__1 = x[3];
	fvec[3] = aafcom_1.aaf[2] * (d__1 * d__1) + aafcom_1.aaf[6] * x[2] * 
		x[4] + aafcom_1.aaf[10];
	fvec[4] = aafcom_1.aaf[3] * x[1] + aafcom_1.aaf[7] * x[2] + 
		aafcom_1.aaf[11] * x[3] + aafcom_1.aaf[15] * x[4] + 
		aafcom_1.aaf[19];

    } else if (fms_1.ms == 4) {
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[3];
	fvec[1] = aafcom_1.aaf[0] * (d__1 * d__1) + aafcom_1.aaf[4] * x[2] + 
		aafcom_1.aaf[8] * (d__2 * d__2) + aafcom_1.aaf[16];
/* Computing 2nd power */
	d__1 = x[2];
	fvec[2] = aafcom_1.aaf[1] * (d__1 * d__1) + aafcom_1.aaf[5] * x[1] * 
		x[3] + aafcom_1.aaf[17];
	fvec[3] = aafcom_1.aaf[3] * x[1] + aafcom_1.aaf[7] * x[2] + 
		aafcom_1.aaf[11] * x[3] + aafcom_1.aaf[19];
    }
    return 0;
} /* funcv_ */

/* === */
/* Subroutine */ int broydn_(doublereal *x, integer *n, integer *n1, logical *
	check)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);
    /* Subroutine */ int s_paus(char *, ftnlen);

    /* Local variables */
    static doublereal c__[40], d__[40], f, g[40];
    static integer i__, j, k;
    static doublereal p[40], r__[1600]	/* was [40][40] */, s[40], t[40], w[
	    40], qt[1600]	/* was [40][40] */, den;
    static integer its;
    static doublereal sum, fold;
    extern doublereal fmin_(integer *, doublereal *);
    static logical sing;
    static doublereal temp;
    static logical skip;
    static doublereal xold[40], test;
    extern /* Subroutine */ int fdjac_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *), rsolv_(doublereal *, integer *, integer 
	    *, doublereal *, doublereal *);
    static doublereal fvcold[40];
    extern /* Subroutine */ int qrdcmp_(doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, logical *), lnsrch_(integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, logical *);
    static doublereal stpmax;
    extern /* Subroutine */ int qrupdt_(doublereal *, doublereal *, integer *,
	     integer *, doublereal *, doublereal *);
    static logical restrt;

/*     LOGICAL restrt,sing,skip */
/* --- */
/* U    USES fdjac,fmin,lnsrch,qrdcmp,qrupdt,rsolv */
/* --- */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    newtv_1.nn = *n;

/* - "fmin" produces NEW array  fvec : */

    f = fmin_(n1, &x[1]);
    test = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if ((d__1 = newtv_1.fvec[i__ - 1], abs(d__1)) > test) {
	    test = (d__2 = newtv_1.fvec[i__ - 1], abs(d__2));
	}
/* L11: */
    }
    if (test < 1.0000000000000001e-32) {
	return 0;
    }
    sum = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
	d__1 = x[i__];
	sum += d__1 * d__1;
/* L12: */
    }
/* Computing MAX */
    d__1 = sqrt(sum), d__2 = (doublereal) (*n);
    stpmax = max(d__1,d__2) * 900.;
    restrt = TRUE_;
    for (its = 1; its <= 20000; ++its) {
	if (restrt) {
	    fdjac_(n, &x[1], newtv_1.fvec, &c__40, r__);
	    qrdcmp_(r__, n, &c__40, c__, d__, &sing);
	    if (sing) {
		s_paus("singular Jacobian in broydn", (ftnlen)27);
	    }
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		i__2 = *n;
		for (j = 1; j <= i__2; ++j) {
		    qt[i__ + j * 40 - 41] = 0.;
/* L13: */
		}
		qt[i__ + i__ * 40 - 41] = 1.;
/* L14: */
	    }
	    i__1 = *n - 1;
	    for (k = 1; k <= i__1; ++k) {
		if (c__[k - 1] != 0.) {
		    i__2 = *n;
		    for (j = 1; j <= i__2; ++j) {
			sum = 0.;
			i__3 = *n;
			for (i__ = k; i__ <= i__3; ++i__) {
			    sum += r__[i__ + k * 40 - 41] * qt[i__ + j * 40 - 
				    41];
/* L15: */
			}
			sum /= c__[k - 1];
			i__3 = *n;
			for (i__ = k; i__ <= i__3; ++i__) {
			    qt[i__ + j * 40 - 41] -= sum * r__[i__ + k * 40 - 
				    41];
/* L16: */
			}
/* L17: */
		    }
		}
/* L18: */
	    }
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		r__[i__ + i__ * 40 - 41] = d__[i__ - 1];
		i__2 = i__ - 1;
		for (j = 1; j <= i__2; ++j) {
		    r__[i__ + j * 40 - 41] = 0.;
/* L19: */
		}
/* L21: */
	    }
	} else {
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		s[i__ - 1] = x[i__] - xold[i__ - 1];
/* L22: */
	    }
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		sum = 0.;
		i__2 = *n;
		for (j = i__; j <= i__2; ++j) {
		    sum += r__[i__ + j * 40 - 41] * s[j - 1];
/* L23: */
		}
		t[i__ - 1] = sum;
/* L24: */
	    }
	    skip = TRUE_;
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		sum = 0.;
		i__2 = *n;
		for (j = 1; j <= i__2; ++j) {
		    sum += qt[j + i__ * 40 - 41] * t[j - 1];
/* L25: */
		}
		w[i__ - 1] = newtv_1.fvec[i__ - 1] - fvcold[i__ - 1] - sum;
		if ((d__3 = w[i__ - 1], abs(d__3)) >= ((d__1 = newtv_1.fvec[
			i__ - 1], abs(d__1)) + (d__2 = fvcold[i__ - 1], abs(
			d__2))) * 1e-30) {
		    skip = FALSE_;
		} else {
		    w[i__ - 1] = 0.;
		}
/* L26: */
	    }
	    if (! skip) {
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    sum = 0.;
		    i__2 = *n;
		    for (j = 1; j <= i__2; ++j) {
			sum += qt[i__ + j * 40 - 41] * w[j - 1];
/* L27: */
		    }
		    t[i__ - 1] = sum;
/* L28: */
		}
		den = 0.;
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
		    d__1 = s[i__ - 1];
		    den += d__1 * d__1;
/* L29: */
		}
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    s[i__ - 1] /= den;
/* L31: */
		}
		qrupdt_(r__, qt, n, &c__40, t, s);
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    if (r__[i__ + i__ * 40 - 41] == 0.) {
			s_paus("r singular in broydn", (ftnlen)20);
		    }
		    d__[i__ - 1] = r__[i__ + i__ * 40 - 41];
/* L32: */
		}
	    }
	}
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
	    i__2 = *n;
	    for (j = 1; j <= i__2; ++j) {
		sum += qt[i__ + j * 40 - 41] * newtv_1.fvec[j - 1];
/* L33: */
	    }
	    g[i__ - 1] = sum;
/* L34: */
	}
	for (i__ = *n; i__ >= 1; --i__) {
	    sum = 0.;
	    i__1 = i__;
	    for (j = 1; j <= i__1; ++j) {
		sum += r__[j + i__ * 40 - 41] * g[j - 1];
/* L35: */
	    }
	    g[i__ - 1] = sum;
/* L36: */
	}
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    xold[i__ - 1] = x[i__];
	    fvcold[i__ - 1] = newtv_1.fvec[i__ - 1];
/* L37: */
	}
	fold = f;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
	    i__2 = *n;
	    for (j = 1; j <= i__2; ++j) {
		sum += qt[i__ + j * 40 - 41] * newtv_1.fvec[j - 1];
/* L38: */
	    }
	    p[i__ - 1] = -sum;
/* L39: */
	}
	rsolv_(r__, n, &c__40, d__, p);

/* - "lnsrch" produces NEW array  fvec : */

	lnsrch_(&c__40, n, xold, &fold, g, p, &x[1], &f, &stpmax, check);
/*       call lnsrch(NP,n,xold,fold,g,p,x,f,stpmax,check,fmin) */
	test = 0.;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if ((d__1 = newtv_1.fvec[i__ - 1], abs(d__1)) > test) {
		test = (d__2 = newtv_1.fvec[i__ - 1], abs(d__2));
	    }
/* L41: */
	}
	if (test < 1e-30) {
	    *check = FALSE_;
	    return 0;
	}
	if (*check) {
	    if (restrt) {
		return 0;
	    } else {
		test = 0.;
/* Computing MAX */
		d__1 = f, d__2 = *n * .5;
		den = max(d__1,d__2);
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
		    d__3 = (d__2 = x[i__], abs(d__2));
		    temp = (d__1 = g[i__ - 1], abs(d__1)) * max(d__3,1.) / 
			    den;
		    if (temp > test) {
			test = temp;
		    }
/* L42: */
		}
		if (test < 1e-30) {
		    return 0;
		} else {
		    restrt = TRUE_;
		}
	    }
	} else {
	    restrt = FALSE_;
	    test = 0.;
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
		d__3 = (d__2 = x[i__], abs(d__2));
		temp = (d__1 = x[i__] - xold[i__ - 1], abs(d__1)) / max(d__3,
			1.);
		if (temp > test) {
		    test = temp;
		}
/* L43: */
	    }
	    if (test < 1e-30) {
		return 0;
	    }
	}
/* L44: */
    }
    s_paus("MAXITS exceeded in broydn", (ftnlen)25);
    return 0;
} /* broydn_ */

/* N */
/* Subroutine */ int fdjac_(integer *n, doublereal *x, doublereal *fvec, 
	integer *np, doublereal *df)
{
    /* System generated locals */
    integer df_dim1, df_offset, i__1, i__2;

    /* Local variables */
    static doublereal f[40], h__;
    static integer i__, j;
    static doublereal temp;
    extern /* Subroutine */ int funcv_(integer *, integer *, doublereal *, 
	    doublereal *);

/* --- */
/*     dimension  df(np,np),fvec(n),x(n),f(NP1) */
/* U    USES funcv */
/* --- */
    /* Parameter adjustments */
    df_dim1 = *np;
    df_offset = 1 + df_dim1 * 1;
    df -= df_offset;
    --fvec;
    --x;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	temp = x[j];
	h__ = abs(temp) * 1e-12;
	if (h__ == 0.) {
	    h__ = 1e-12;
	}
	x[j] = temp + h__;
	h__ = x[j] - temp;
	funcv_(np, n, &x[1], f);
	x[j] = temp;
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    df[i__ + j * df_dim1] = (f[i__ - 1] - fvec[i__]) / h__;
/* L11: */
	}
/* L12: */
    }
    return 0;
} /* fdjac_ */

/* N */
/*     SUBROUTINE lnsrch(np,n,xold,fold,g,p,x,f,stpmax,check,func) */
/*     SUBROUTINE lnsrch(np,n,xold,fold,g,p,x,f,stpmax,check,fmin) */
/* =============== */
/* Subroutine */ int lnsrch_(integer *np, integer *n, doublereal *xold, 
	doublereal *fold, doublereal *g, doublereal *p, doublereal *x, 
	doublereal *f, doublereal *stpmax, logical *check)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal a, b;
    static integer i__;
    static doublereal f2, sum, rhs1, rhs2, alam, disc;
    extern doublereal fmin_(integer *, doublereal *);
    static doublereal temp, test, alam2, fold2, slope, alamin, tmplam;

/* --- */
/*     EXTERNAL func */
/* U    USES func */
/* -  There is NO an external function called "func" in this code ! */
/* U    USES fmin */
/* --- */
    /* Parameter adjustments */
    --x;
    --p;
    --g;
    --xold;

    /* Function Body */
    *check = FALSE_;
    sum = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum += p[i__] * p[i__];
/* L11: */
    }
    sum = sqrt(sum);
    if (sum > *stpmax) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    p[i__] = p[i__] * *stpmax / sum;
/* L12: */
	}
    }
    slope = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	slope += g[i__] * p[i__];
/* L13: */
    }
    test = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	d__3 = (d__2 = xold[i__], abs(d__2));
	temp = (d__1 = p[i__], abs(d__1)) / max(d__3,1.);
	if (temp > test) {
	    test = temp;
	}
/* L14: */
    }
    alamin = 1e-12 / test;
    alam = 1.;
L1:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__] = xold[i__] + alam * p[i__];
/* L15: */
    }

/* - "fmin" produces NEW array  fvec : */

    *f = fmin_(np, &x[1]);
/*       f=func(x) */
    if (alam < alamin) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    x[i__] = xold[i__];
/* L16: */
	}
	*check = TRUE_;
	return 0;
    } else if (*f <= *fold + alam * 1e-12 * slope) {
	return 0;
    } else {
	if (alam == 1.) {
	    tmplam = -slope / ((*f - *fold - slope) * 2.);
	} else {
	    rhs1 = *f - *fold - alam * slope;
	    rhs2 = f2 - fold2 - alam2 * slope;
/* Computing 2nd power */
	    d__1 = alam;
/* Computing 2nd power */
	    d__2 = alam2;
	    a = (rhs1 / (d__1 * d__1) - rhs2 / (d__2 * d__2)) / (alam - alam2)
		    ;
/* Computing 2nd power */
	    d__1 = alam;
/* Computing 2nd power */
	    d__2 = alam2;
	    b = (-alam2 * rhs1 / (d__1 * d__1) + alam * rhs2 / (d__2 * d__2)) 
		    / (alam - alam2);
	    if (a == 0.) {
		tmplam = -slope / (b * 2.);
	    } else {
		disc = b * b - a * 3. * slope;
		tmplam = (-b + sqrt(disc)) / (a * 3.);
	    }
	    if (tmplam > alam * .5) {
		tmplam = alam * .5;
	    }
	}
    }
    alam2 = alam;
    f2 = *f;
    fold2 = *fold;
/* Computing MAX */
    d__1 = tmplam, d__2 = alam * .1;
    alam = max(d__1,d__2);
    goto L1;
} /* lnsrch_ */

/* N */
/* Subroutine */ int qrupdt_(doublereal *r__, doublereal *qt, integer *n, 
	integer *np, doublereal *u, doublereal *v)
{
    /* System generated locals */
    integer r_dim1, r_offset, qt_dim1, qt_offset, i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, k;
    extern /* Subroutine */ int rotate_(doublereal *, doublereal *, integer *,
	     integer *, integer *, doublereal *, doublereal *);

/* --- */
/* U    USES rotate */
/* --- */
    /* Parameter adjustments */
    --v;
    --u;
    qt_dim1 = *np;
    qt_offset = 1 + qt_dim1 * 1;
    qt -= qt_offset;
    r_dim1 = *np;
    r_offset = 1 + r_dim1 * 1;
    r__ -= r_offset;

    /* Function Body */
    for (k = *n; k >= 1; --k) {
	if (u[k] != 0.) {
	    goto L1;
	}
/* L11: */
    }
    k = 1;
L1:
    for (i__ = k - 1; i__ >= 1; --i__) {
	d__1 = -u[i__ + 1];
	rotate_(&r__[r_offset], &qt[qt_offset], n, np, &i__, &u[i__], &d__1);
	if (u[i__] == 0.) {
	    u[i__] = (d__1 = u[i__ + 1], abs(d__1));
	} else if ((d__1 = u[i__], abs(d__1)) > (d__2 = u[i__ + 1], abs(d__2))
		) {
/* Computing 2nd power */
	    d__2 = u[i__ + 1] / u[i__];
	    u[i__] = (d__1 = u[i__], abs(d__1)) * sqrt(d__2 * d__2 + 1.);
	} else {
/* Computing 2nd power */
	    d__2 = u[i__] / u[i__ + 1];
	    u[i__] = (d__1 = u[i__ + 1], abs(d__1)) * sqrt(d__2 * d__2 + 1.);
	}
/* L12: */
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	r__[j * r_dim1 + 1] += u[1] * v[j];
/* L13: */
    }
    i__1 = k - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d__1 = -r__[i__ + 1 + i__ * r_dim1];
	rotate_(&r__[r_offset], &qt[qt_offset], n, np, &i__, &r__[i__ + i__ * 
		r_dim1], &d__1);
/* L14: */
    }
    return 0;
} /* qrupdt_ */

/* N */
doublereal fmin_(integer *n1, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Local variables */
    static integer i__;
    static doublereal sum;
    extern /* Subroutine */ int funcv_(integer *, integer *, doublereal *, 
	    doublereal *);

/* --- */
/* U    USES funcv */
/* --- */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    funcv_(&c__40, &newtv_2.n, &x[1], newtv_2.fvec);
    sum = 0.;
    i__1 = newtv_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
	d__1 = newtv_2.fvec[i__ - 1];
	sum += d__1 * d__1;
/* L11: */
    }
    ret_val = sum * .5;
    return ret_val;
} /* fmin_ */

/* N */
/* Subroutine */ int qrdcmp_(doublereal *a, integer *n, integer *np, 
	doublereal *c__, doublereal *d__, logical *sing)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static integer i__, j, k;
    static doublereal tau, sum, scale, sigma;

/* --- */
    /* Parameter adjustments */
    --d__;
    --c__;
    a_dim1 = *np;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    *sing = FALSE_;
    scale = 0.;
    i__1 = *n - 1;
    for (k = 1; k <= i__1; ++k) {
	i__2 = *n;
	for (i__ = k; i__ <= i__2; ++i__) {
/* Computing MAX */
	    d__2 = scale, d__3 = (d__1 = a[i__ + k * a_dim1], abs(d__1));
	    scale = max(d__2,d__3);
/* L11: */
	}
	if (scale == 0.) {
	    *sing = TRUE_;
	    c__[k] = 0.;
	    d__[k] = 0.;
	} else {
	    i__2 = *n;
	    for (i__ = k; i__ <= i__2; ++i__) {
		a[i__ + k * a_dim1] /= scale;
/* L12: */
	    }
	    sum = 0.;
	    i__2 = *n;
	    for (i__ = k; i__ <= i__2; ++i__) {
/* Computing 2nd power */
		d__1 = a[i__ + k * a_dim1];
		sum += d__1 * d__1;
/* L13: */
	    }
	    d__1 = sqrt(sum);
	    sigma = d_sign(&d__1, &a[k + k * a_dim1]);
	    a[k + k * a_dim1] += sigma;
	    c__[k] = sigma * a[k + k * a_dim1];
	    d__[k] = -scale * sigma;
	    i__2 = *n;
	    for (j = k + 1; j <= i__2; ++j) {
		sum = 0.;
		i__3 = *n;
		for (i__ = k; i__ <= i__3; ++i__) {
		    sum += a[i__ + k * a_dim1] * a[i__ + j * a_dim1];
/* L14: */
		}
		tau = sum / c__[k];
		i__3 = *n;
		for (i__ = k; i__ <= i__3; ++i__) {
		    a[i__ + j * a_dim1] -= tau * a[i__ + k * a_dim1];
/* L15: */
		}
/* L16: */
	    }
	}
/* L17: */
    }
    d__[*n] = a[*n + *n * a_dim1];
    if (d__[*n] == 0.) {
	*sing = TRUE_;
    }
    return 0;
} /* qrdcmp_ */

/* N */
/* Subroutine */ int rsolv_(doublereal *a, integer *n, integer *np, 
	doublereal *d__, doublereal *b)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;

    /* Local variables */
    static integer i__, j;
    static doublereal sum;

/* --- */
    /* Parameter adjustments */
    --b;
    --d__;
    a_dim1 = *np;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    b[*n] /= d__[*n];
    for (i__ = *n - 1; i__ >= 1; --i__) {
	sum = 0.;
	i__1 = *n;
	for (j = i__ + 1; j <= i__1; ++j) {
	    sum += a[i__ + j * a_dim1] * b[j];
/* L11: */
	}
	b[i__] = (b[i__] - sum) / d__[i__];
/* L12: */
    }
    return 0;
} /* rsolv_ */

/* N */
/* Subroutine */ int rotate_(doublereal *r__, doublereal *qt, integer *n, 
	integer *np, integer *i__, doublereal *a, doublereal *b)
{
    /* System generated locals */
    integer r_dim1, r_offset, qt_dim1, qt_offset, i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static doublereal c__;
    static integer j;
    static doublereal s, w, y, fact;

/* --- */
    /* Parameter adjustments */
    qt_dim1 = *np;
    qt_offset = 1 + qt_dim1 * 1;
    qt -= qt_offset;
    r_dim1 = *np;
    r_offset = 1 + r_dim1 * 1;
    r__ -= r_offset;

    /* Function Body */
    if (*a == 0.) {
	c__ = 0.;
	s = d_sign(&c_b672, b);
    } else if (abs(*a) > abs(*b)) {
	fact = *b / *a;
/* Computing 2nd power */
	d__2 = fact;
	d__1 = 1. / sqrt(d__2 * d__2 + 1.);
	c__ = d_sign(&d__1, a);
	s = fact * c__;
    } else {
	fact = *a / *b;
/* Computing 2nd power */
	d__2 = fact;
	d__1 = 1. / sqrt(d__2 * d__2 + 1.);
	s = d_sign(&d__1, b);
	c__ = fact * s;
    }
    i__1 = *n;
    for (j = *i__; j <= i__1; ++j) {
	y = r__[*i__ + j * r_dim1];
	w = r__[*i__ + 1 + j * r_dim1];
	r__[*i__ + j * r_dim1] = c__ * y - s * w;
	r__[*i__ + 1 + j * r_dim1] = s * y + c__ * w;
/* L11: */
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	y = qt[*i__ + j * qt_dim1];
	w = qt[*i__ + 1 + j * qt_dim1];
	qt[*i__ + j * qt_dim1] = c__ * y - s * w;
	qt[*i__ + 1 + j * qt_dim1] = s * y + c__ * w;
/* L12: */
    }
    return 0;
} /* rotate_ */

/* N */
/* Subroutine */ int newt_(doublereal *x, integer *n, integer *n1, logical *
	check)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);
    /* Subroutine */ int s_paus(char *, ftnlen);

    /* Local variables */
    static doublereal d__, f, g[40];
    static integer i__, j;
    static doublereal p[40], den;
    static integer its;
    static doublereal sum, fjac[1600]	/* was [40][40] */, fold;
    extern doublereal fmin_(integer *, doublereal *);
    static integer indx[40];
    static doublereal temp, xold[40], test;
    extern /* Subroutine */ int fdjac_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *), lubksb_(doublereal *, integer *, 
	    integer *, integer *, doublereal *), ludcmp_(doublereal *, 
	    integer *, integer *, integer *, doublereal *), lnsrch_(integer *,
	     integer *, doublereal *, doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *, logical *);
    static doublereal stpmax;

/* --- */
/* U    USES fdjac,fmin,lnsrch,lubksb,ludcmp */
/* --- */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    newtv_1.nn = *n;

/* - "fmin" produces NEW array  fvec : */

    f = fmin_(n1, &x[1]);
    test = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if ((d__1 = newtv_1.fvec[i__ - 1], abs(d__1)) > test) {
	    test = (d__2 = newtv_1.fvec[i__ - 1], abs(d__2));
	}
/* L11: */
    }
    if (test < 1.0000000000000001e-32) {
	return 0;
    }
    sum = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
	d__1 = x[i__];
	sum += d__1 * d__1;
/* L12: */
    }
/* Computing MAX */
    d__1 = sqrt(sum), d__2 = (doublereal) (*n);
    stpmax = max(d__1,d__2) * 900.;

/* - "fdjac" produces ANOTHER array  fvec : */

    for (its = 1; its <= 90000; ++its) {
	fdjac_(n, &x[1], newtv_1.fvec, &c__40, fjac);
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
	    i__2 = *n;
	    for (j = 1; j <= i__2; ++j) {
		sum += fjac[j + i__ * 40 - 41] * newtv_1.fvec[j - 1];
/* L13: */
	    }
	    g[i__ - 1] = sum;
/* L14: */
	}
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    xold[i__ - 1] = x[i__];
/* L15: */
	}
	fold = f;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    p[i__ - 1] = -newtv_1.fvec[i__ - 1];
/* L16: */
	}
	ludcmp_(fjac, n, &c__40, indx, &d__);
	lubksb_(fjac, n, &c__40, indx, p);

/* - "lnsrch" produces ANOTHER array  fvec : */

	lnsrch_(&c__40, n, xold, &fold, g, p, &x[1], &f, &stpmax, check);
/*       call lnsrch(NP,n,xold,fold,g,p,x,f,stpmax,check,fmin) */
	test = 0.;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if ((d__1 = newtv_1.fvec[i__ - 1], abs(d__1)) > test) {
		test = (d__2 = newtv_1.fvec[i__ - 1], abs(d__2));
	    }
/* L17: */
	}
	if (test < 1e-30) {
	    *check = FALSE_;
	    return 0;
	}
	if (*check) {
	    test = 0.;
/* Computing MAX */
	    d__1 = f, d__2 = *n * .5;
	    den = max(d__1,d__2);
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
		d__3 = (d__2 = x[i__], abs(d__2));
		temp = (d__1 = g[i__ - 1], abs(d__1)) * max(d__3,1.) / den;
		if (temp > test) {
		    test = temp;
		}
/* L18: */
	    }
	    if (test < 1e-30) {
		*check = TRUE_;
	    } else {
		*check = FALSE_;
	    }
	    return 0;
	}
	test = 0.;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	    d__3 = (d__2 = x[i__], abs(d__2));
	    temp = (d__1 = x[i__] - xold[i__ - 1], abs(d__1)) / max(d__3,1.);
	    if (temp > test) {
		test = temp;
	    }
/* L19: */
	}
	if (test < 1e-30) {
	    return 0;
	}
/* L21: */
    }
    s_paus("MAXITS exceeded in newt", (ftnlen)23);
    return 0;
} /* newt_ */

/* === */
/* Subroutine */ int lubksb_(doublereal *a, integer *n, integer *np, integer *
	indx, doublereal *b)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j, ii, ll;
    static doublereal sum;

/* --- */
/*     dimension  a(np,np),b(n),indx(n) */
/* --- */
    /* Parameter adjustments */
    --b;
    --indx;
    a_dim1 = *np;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    ii = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ll = indx[i__];
	sum = b[ll];
	b[ll] = b[i__];
	if (ii != 0) {
	    i__2 = i__ - 1;
	    for (j = ii; j <= i__2; ++j) {
		sum -= a[i__ + j * a_dim1] * b[j];
/* L11: */
	    }
	} else if (sum != 0.) {
	    ii = i__;
	}
	b[i__] = sum;
/* L12: */
    }
    for (i__ = *n; i__ >= 1; --i__) {
	sum = b[i__];
	i__1 = *n;
	for (j = i__ + 1; j <= i__1; ++j) {
	    sum -= a[i__ + j * a_dim1] * b[j];
/* L13: */
	}
	b[i__] = sum / a[i__ + i__ * a_dim1];
/* L14: */
    }
    return 0;
} /* lubksb_ */

/* N */
/* Subroutine */ int ludcmp_(doublereal *a, integer *n, integer *np, integer *
	indx, doublereal *d__)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ int s_paus(char *, ftnlen);

    /* Local variables */
    static integer i__, j, k;
    static doublereal vv[500], dum, sum;
    static integer imax;
    static doublereal aamax;

/* --- */
/*     dimension  indx(n),a(np,np),vv(NMAX) */
/* --- */
    /* Parameter adjustments */
    --indx;
    a_dim1 = *np;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    *d__ = 1.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	aamax = 0.;
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    if ((d__1 = a[i__ + j * a_dim1], abs(d__1)) > aamax) {
		aamax = (d__2 = a[i__ + j * a_dim1], abs(d__2));
	    }
/* L11: */
	}
	if (aamax == 0.) {
	    s_paus("singular matrix in ludcmp", (ftnlen)25);
	}
	vv[i__ - 1] = 1. / aamax;
/* L12: */
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j - 1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    sum = a[i__ + j * a_dim1];
	    i__3 = i__ - 1;
	    for (k = 1; k <= i__3; ++k) {
		sum -= a[i__ + k * a_dim1] * a[k + j * a_dim1];
/* L13: */
	    }
	    a[i__ + j * a_dim1] = sum;
/* L14: */
	}
	aamax = 0.;
	i__2 = *n;
	for (i__ = j; i__ <= i__2; ++i__) {
	    sum = a[i__ + j * a_dim1];
	    i__3 = j - 1;
	    for (k = 1; k <= i__3; ++k) {
		sum -= a[i__ + k * a_dim1] * a[k + j * a_dim1];
/* L15: */
	    }
	    a[i__ + j * a_dim1] = sum;
	    dum = vv[i__ - 1] * abs(sum);
	    if (dum >= aamax) {
		imax = i__;
		aamax = dum;
	    }
/* L16: */
	}
	if (j != imax) {
	    i__2 = *n;
	    for (k = 1; k <= i__2; ++k) {
		dum = a[imax + k * a_dim1];
		a[imax + k * a_dim1] = a[j + k * a_dim1];
		a[j + k * a_dim1] = dum;
/* L17: */
	    }
	    *d__ = -(*d__);
	    vv[imax - 1] = vv[j - 1];
	}
	indx[j] = imax;
	if (a[j + j * a_dim1] == 0.) {
	    a[j + j * a_dim1] = 1e-20;
	}
	if (j != *n) {
	    dum = 1. / a[j + j * a_dim1];
	    i__2 = *n;
	    for (i__ = j + 1; i__ <= i__2; ++i__) {
		a[i__ + j * a_dim1] *= dum;
/* L18: */
	    }
	}
/* L19: */
    }
    return 0;
} /* ludcmp_ */

/* === */

/* Subroutine */ int quadratic_(doublereal *a, doublereal *b, doublereal *c__,
	 doublereal *x1, doublereal *x2)
{
    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static doublereal q;

/* ---------------------------------------------------------- */
/*   Find the roots, x1 & x2, of a quadratic equation */
/*               a*x*x + b*x + c = 0 */
/* using the skills on P. 178 of "Numerical Reccipes". */
/*       write(6,*) '  sign(1.0, b) = ', sign(1.0,b) */
/* ---------------------------------------------------------- */
    q = (*b + d_sign(&c_b672, b) * sqrt(*b * *b - *a * 4. * *c__)) * -.5;
    *x1 = q / *a;
    *x2 = *c__ / q;
/* --- */
    return 0;
} /* quadratic_ */

/* === */

doublereal dfridr_(integer *n, integer *ns, doublereal *x, doublereal *h__, 
	doublereal *err)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1, d__2, d__3, d__4;

    /* Builtin functions */
    /* Subroutine */ int s_paus(char *, ftnlen);

    /* Local variables */
    static doublereal a[10000]	/* was [100][100] */;
    static integer i__, j;
    static doublereal hh, fac, big, con, con2, safe;
    extern doublereal fpot_(doublereal *, integer *, integer *);
    static doublereal errt;

/* ---------------------------------------------------------- */
/*   Returns the derivative of a function func at a point */
/* x by Ridders' method of polynomial extrapolation. The */
/* value h is input as an estimated initial stepsize; it */
/* needs not be small, but rather should be an increment */
/* in x over which func changes substantially. An estimate */
/* of the error in the derivative is returned as err. */
/*   Parameters:  Stepsize is decreased by CON at each */
/* iteration. Max size of tableau is set by NTAB. Return */
/* when error is SAFE worse than the best so far. */
/*   n -- The order of derivative. */

/*             Experiments on parameters : */
/*          -------------------------------- */
/*           SAFE               Results   err          h */
/* ---------------------------  -------  ------  ----------- */
/* 1.001, 1.01, 1.5, 2.0, 5.0    GOOD    1*E-16  0.001 - 4.0 */

/*   Results are NOT sensitive to  CON, NTAB. */
/*     EXTERNAL func */
/* - */
/*     PARAMETER (CON=1.4,CON2=CON*CON,BIG=1.E30,NTAB=10,SAFE=2.) */
/* ---------------------------------------------------------- */
/* --------------------------------- */
/*  ns=2 for V_rydberg(R) */
/* --------------------------------- */
    if (*ns == 2) {
	con = 1.4;
	big = 1e30;
	safe = 2.;
/* --------------------------------- */
/*  ns=3 for V_pseudo-gaussian(R) */
/* --------------------------------- */
    } else if (*ns == 3) {
	con = 5.;
	big = 1e30;
	safe = 3.;
    }
    con2 = con * con;
/* ---------------------------------------------------------- */
    if (*h__ == 0.) {
	s_paus("h must be nonzero in dfridr", (ftnlen)27);
    }
    hh = *h__;
/*     a(1,1)=(func(x+hh)-func(x-hh))/(2.0*hh) */
    d__1 = *x + hh;
    d__2 = *x - hh;
    a[0] = (fpot_(&d__1, n, ns) - fpot_(&d__2, n, ns)) / (hh * 2.);
    *err = big;
/* ---------------------------------------------------------- */
/*   Successive columns in the Neville tableau will go to */
/* smaller stepsizes and higher orders of extrapolation. */
/* ---------------------------------------------------------- */
    for (i__ = 2; i__ <= 100; ++i__) {
/* --------------------------------- */
/* Try new, smaller stepsize. */
/* --------------------------------- */
	hh /= con;
/*         a(1,i)=(func(x+hh)-func(x-hh))/(2.0*hh) */
	d__1 = *x + hh;
	d__2 = *x - hh;
	a[i__ * 100 - 100] = (fpot_(&d__1, n, ns) - fpot_(&d__2, n, ns)) / (
		hh * 2.);
	fac = con2;
/* ------------------------------------------- */
/* Compute extrapolations of various orders, */
/* requiring no new function evaluations. */
/* ------------------------------------------- */
	i__1 = i__;
	for (j = 2; j <= i__1; ++j) {
	    a[j + i__ * 100 - 101] = (a[j - 1 + i__ * 100 - 101] * fac - a[j 
		    - 1 + (i__ - 1) * 100 - 101]) / (fac - 1.);
	    fac = con2 * fac;
/* Computing MAX */
	    d__3 = (d__1 = a[j + i__ * 100 - 101] - a[j - 1 + i__ * 100 - 101]
		    , abs(d__1)), d__4 = (d__2 = a[j + i__ * 100 - 101] - a[j 
		    - 1 + (i__ - 1) * 100 - 101], abs(d__2));
	    errt = max(d__3,d__4);
	    if (errt <= *err) {
		*err = errt;
		ret_val = a[j + i__ * 100 - 101];
	    }
/* L11: */
	}
	if ((d__1 = a[i__ + i__ * 100 - 101] - a[i__ - 1 + (i__ - 1) * 100 - 
		101], abs(d__1)) >= safe * *err) {
	    return ret_val;
	}
/* L12: */
    }
    return ret_val;
} /* dfridr_ */


/* Subroutine */ int vpotdata_(void)
{
    /* Builtin functions */
    double sqrt(doublereal);

/* ---------------------------------------------------------- */
    vnumpot_1.ff2 = spectra_1.amu * spectra_1.we * spectra_1.we;
    vnumpot_1.betam = sqrt(vnumpot_1.ff2 / (spectra_1.de * 2.));
    pgdata1_1.bt = sqrt(vnumpot_1.ff2 * spectra_1.re * spectra_1.re / 
	    spectra_1.de + 4.) * .5 - 1.;
    pgdata1_1.c0 = pgdata1_1.bt * -2.;

    pgdata2_1.re2 = spectra_1.re * spectra_1.re;
    pgdata2_1.re4 = pgdata2_1.re2 * pgdata2_1.re2;
    pgdata2_1.re6 = pgdata2_1.re4 * pgdata2_1.re2;
    pgdata2_1.re8 = pgdata2_1.re6 * pgdata2_1.re2;
    pgdata2_1.re10 = pgdata2_1.re8 * pgdata2_1.re2;
    pgdata2_1.re12 = pgdata2_1.re10 * pgdata2_1.re2;
    pgdata2_1.re14 = pgdata2_1.re12 * pgdata2_1.re2;
    pgdata2_1.re16 = pgdata2_1.re14 * pgdata2_1.re2;
    pgdata2_1.re18 = pgdata2_1.re16 * pgdata2_1.re2;
    pgdata2_1.re20 = pgdata2_1.re18 * pgdata2_1.re2;

    pgdata1_1.c02 = pgdata1_1.c0 * pgdata1_1.c0;
    pgdata1_1.c03 = pgdata1_1.c02 * pgdata1_1.c0;
    pgdata1_1.c04 = pgdata1_1.c03 * pgdata1_1.c0;
    pgdata1_1.c05 = pgdata1_1.c04 * pgdata1_1.c0;
    pgdata1_1.c06 = pgdata1_1.c05 * pgdata1_1.c0;
    pgdata1_1.c07 = pgdata1_1.c06 * pgdata1_1.c0;
    pgdata1_1.c08 = pgdata1_1.c07 * pgdata1_1.c0;
    pgdata1_1.c09 = pgdata1_1.c08 * pgdata1_1.c0;
    pgdata1_1.c010 = pgdata1_1.c09 * pgdata1_1.c0;

    return 0;
} /* vpotdata_ */


doublereal fmorse_(integer *n)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Builtin functions */
    integer pow_ii(integer *, integer *);
    double pow_di(doublereal *, integer *);

/* ---------------------------------------------------------- */
    i__1 = *n - 1;
    i__2 = *n - 2;
    ret_val = pow_ii(&c_n1, n) * (pow_di(&c_b488, &i__1) - 1.) * pow_di(&
	    vnumpot_1.betam, &i__2) * vnumpot_1.ff2;
/* ---------------------------------------------------------- */
    return ret_val;
} /* fmorse_ */


doublereal fryd_(integer *n, doublereal *r__)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sqrt(doublereal), exp(doublereal), pow_di(doublereal *, integer *);

    /* Local variables */
    static doublereal a, ep, vr;

/* ---------------------------------------------------------- */
/*   The nth derivative of Rydberg potentials;  n >= 2. */
/* ---------------------------------------------------------- */
    if (fms_1.ny == 2) {
	a = (spectra_1.we + spectrb_1.bryd) * sqrt(spectra_1.amu / 
		spectra_1.de);
    }
    ep = exp(-a * (*r__ - spectra_1.re));
    vr = -spectra_1.de * (a * (*r__ - spectra_1.re) + 1.) * ep;
    ret_val = pow_di(&c_b218, n) * pow_di(&a, n) * (*n * spectra_1.de * ep + 
	    vr);
/* ---------------------------------------------------------- */
    return ret_val;
} /* fryd_ */


doublereal fpot_(doublereal *r__, integer *n, integer *ns)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sqrt(doublereal), exp(doublereal), pow_di(doublereal *, integer *);

    /* Local variables */
    static doublereal a;
    static integer m;
    static doublereal b2, g1, g2, h1, h2, r2, r3, r4, r5, r6, r7, r8, r9, r10,
	     r11, ep, r12, vr;

/* ---------------------------------------------------------- */
    if (*ns == 2) {
	goto L100;
    }
    if (*ns == 3) {
	goto L200;
    }
/* ---------------------------------------------------------- */
/*   The nth derivative of Rydberg potentials;  n >= 2. */
/* 	  f(2) = amu*We*We ; */
/*       d/(dR){ f[ V_rydberg(R) ] } = {fpot}' */

/* 100    a = dsqrt(ff2/De) */
/* 100    a = We * dsqrt(amu/De) */
/* ---------------------------------------------------------- */
L100:
    if (fms_1.ny == 2) {
	a = (spectra_1.we + spectrb_1.bryd) * sqrt(spectra_1.amu / 
		spectra_1.de);
    }
    m = *n - 1;
    ep = exp(-a * (*r__ - spectra_1.re));
    vr = -spectra_1.de * (a * (*r__ - spectra_1.re) + 1.) * ep;
    ret_val = pow_di(&c_b218, &m) * pow_di(&a, &m) * (m * spectra_1.de * ep + 
	    vr);
    goto L800;
/* ---------------------------------------------------------- */
/*   The nth derivative of V(Pseudo-Gaussian; R) ; */
/*           2 =< n <= 11  ONLY . */
/* 	V(n)[p-g; R] = d/(dR){ V(n-1)[p-g; R] } = {fpot}' */
/* ---------------------------------------------------------- */
L200:
    b2 = 1. - *r__ * *r__ / (spectra_1.re * spectra_1.re);
    g1 = spectra_1.de * exp(pgdata1_1.bt * b2);
    g2 = pgdata1_1.bt * g1;

    r2 = *r__ * *r__;
    r3 = r2 * *r__;
    r4 = r3 * *r__;
    r5 = r4 * *r__;
    r6 = r5 * *r__;
    r7 = r6 * *r__;
    r8 = r7 * *r__;
    r9 = r8 * *r__;
    r10 = r9 * *r__;
    r11 = r10 * *r__;
    r12 = r11 * *r__;

    h1 = 0.;
    h2 = 0.;
    if (*n == 2) {
	h1 = pgdata1_1.c0 * pgdata2_1.re2 / r3 - pgdata1_1.c0 * *r__ / 
		pgdata2_1.re2;
	h2 = pgdata1_1.c0 / *r__ - pgdata1_1.c0 * *r__ / pgdata2_1.re2;
	goto L300;
    } else if (*n == 3) {
	h1 = pgdata1_1.c02 / r2 - pgdata1_1.c0 * 3. * pgdata2_1.re2 / r4 - 
		pgdata1_1.c0 / pgdata2_1.re2 - pgdata1_1.c02 * r2 / 
		pgdata2_1.re4;
	h2 = -pgdata1_1.c0 / r2 - pgdata1_1.c0 / pgdata2_1.re2 + 
		pgdata1_1.c02 / pgdata2_1.re2 - pgdata1_1.c02 * r2 / 
		pgdata2_1.re4;
	goto L300;
    } else if (*n == 4) {
	h1 = pgdata1_1.c02 * -3. * *r__ / pgdata2_1.re4 - pgdata1_1.c03 * r3 /
		 pgdata2_1.re6 + pgdata1_1.c03 / (*r__ * pgdata2_1.re2);
	h1 = h1 - pgdata1_1.c02 * 5. / r3 + pgdata1_1.c0 * 12. * 
		pgdata2_1.re2 / r5;
	h2 = pgdata1_1.c02 * -3. * *r__ / pgdata2_1.re4 + pgdata1_1.c03 * *
		r__ / pgdata2_1.re4 - pgdata1_1.c03 * r3 / pgdata2_1.re6;
	h2 = h2 - pgdata1_1.c02 / (*r__ * pgdata2_1.re2) + pgdata1_1.c0 * 2. /
		 r3;
	goto L300;
    } else if (*n == 5) {
	h1 = pgdata1_1.c02 * -3. / pgdata2_1.re4 + pgdata1_1.c04 / 
		pgdata2_1.re4 - pgdata1_1.c03 * 6. / (r2 * pgdata2_1.re2) - 
		pgdata1_1.c02 * 27. / r4;
	h1 = h1 - pgdata1_1.c0 * 60. * pgdata2_1.re2 / r6 - pgdata1_1.c03 * 
		6. * r2 / pgdata2_1.re6 - pgdata1_1.c04 * r4 / pgdata2_1.re8;
	h2 = pgdata1_1.c02 * -3. / pgdata2_1.re4 + pgdata1_1.c02 * 3. / (r2 * 
		pgdata2_1.re2) - pgdata1_1.c0 * 6. / r4;
	h2 = h2 - pgdata1_1.c03 * 6. * r2 / pgdata2_1.re6 + pgdata1_1.c04 * 
		r2 / pgdata2_1.re6 - pgdata1_1.c04 * r4 / pgdata2_1.re8;
	goto L300;
    } else if (*n == 6) {
	h1 = pgdata1_1.c03 * 39. / (r3 * pgdata2_1.re2) - pgdata1_1.c02 * 
		168. / r5 + pgdata1_1.c0 * 360. * pgdata2_1.re2 / r7;
	h1 = h1 - pgdata1_1.c03 * 15. * *r__ / pgdata2_1.re6 - pgdata1_1.c04 *
		 10. * r3 / pgdata2_1.re8 + pgdata1_1.c05 * *r__ / 
		pgdata2_1.re6;
	h1 = h1 - pgdata1_1.c04 * 6. / (*r__ * pgdata2_1.re4) - pgdata1_1.c05 
		* r5 / pgdata2_1.re10;
	h2 = pgdata1_1.c02 * -12. / (r3 * pgdata2_1.re2) + pgdata1_1.c0 * 24. 
		/ r5 - pgdata1_1.c03 * 15. * *r__ / pgdata2_1.re6;
	h2 = h2 + pgdata1_1.c04 * 2. * *r__ / pgdata2_1.re6 - pgdata1_1.c04 * 
		10. * r3 / pgdata2_1.re8 + pgdata1_1.c03 * 3. / (*r__ * 
		pgdata2_1.re4);
	h2 = h2 + pgdata1_1.c05 * r3 / pgdata2_1.re8 - pgdata1_1.c05 * r5 / 
		pgdata2_1.re10;
	goto L300;
    } else if (*n == 7) {
	h1 = pgdata1_1.c03 * -285. / (r4 * pgdata2_1.re2) + pgdata1_1.c02 * 
		1200. / r6 - pgdata1_1.c0 * 2520. * pgdata2_1.re2 / r8;
	h1 = h1 - pgdata1_1.c03 * 15. / pgdata2_1.re6 - pgdata1_1.c04 * 45. * 
		r2 / pgdata2_1.re8 - pgdata1_1.c05 * 5. / pgdata2_1.re6;
	h1 = h1 + pgdata1_1.c04 * 45. / (r2 * pgdata2_1.re4) - pgdata1_1.c05 *
		 15. * r4 / pgdata2_1.re10;
	h1 = h1 + pgdata1_1.c06 * r2 / pgdata2_1.re8 - pgdata1_1.c06 * r6 / 
		pgdata2_1.re12;
	h2 = pgdata1_1.c02 * 60. / (r4 * pgdata2_1.re2) - pgdata1_1.c0 * 120. 
		/ r6 - pgdata1_1.c03 * 15. / pgdata2_1.re6;
	h2 = h2 + pgdata1_1.c04 * 5. / pgdata2_1.re6 - pgdata1_1.c04 * 45. * 
		r2 / pgdata2_1.re8 - pgdata1_1.c03 * 15. / (r2 * 
		pgdata2_1.re4);
	h2 = h2 + pgdata1_1.c05 * 5. * r2 / pgdata2_1.re8 - pgdata1_1.c05 * 
		15. * r4 / pgdata2_1.re10 + pgdata1_1.c06 * r4 / 
		pgdata2_1.re10;
	h2 -= pgdata1_1.c06 * r6 / pgdata2_1.re12;
	goto L300;
    } else if (*n == 8) {
	h1 = pgdata1_1.c03 * 2340. / (r5 * pgdata2_1.re2) - pgdata1_1.c02 * 
		9720. / r7 + pgdata1_1.c0 * 20160. * pgdata2_1.re2 / r9;
	h1 = h1 - pgdata1_1.c04 * 105. * *r__ / pgdata2_1.re8 - pgdata1_1.c04 
		* 375. / (r3 * pgdata2_1.re4);
	h1 = h1 - pgdata1_1.c05 * 105. * r3 / pgdata2_1.re10 - pgdata1_1.c06 *
		 21. * r5 / pgdata2_1.re12;
	h1 = h1 - pgdata1_1.c06 * 3. * *r__ / pgdata2_1.re8 + pgdata1_1.c05 * 
		45. / (*r__ * pgdata2_1.re6) + pgdata1_1.c07 * r3 / 
		pgdata2_1.re10;
	h1 -= pgdata1_1.c07 * r7 / pgdata2_1.re14;
	h2 = pgdata1_1.c02 * -360. / (r5 * pgdata2_1.re2) + pgdata1_1.c0 * 
		720. / r7 - pgdata1_1.c04 * 105. * *r__ / pgdata2_1.re8;
	h2 = h2 + pgdata1_1.c03 * 90. / (r3 * pgdata2_1.re4) + pgdata1_1.c05 *
		 15. * *r__ / pgdata2_1.re8;
	h2 = h2 - pgdata1_1.c05 * 105. * r3 / pgdata2_1.re10 + pgdata1_1.c06 *
		 9. * r3 / pgdata2_1.re10;
	h2 = h2 - pgdata1_1.c06 * 21. * r5 / pgdata2_1.re12 - pgdata1_1.c04 * 
		15. / (*r__ * pgdata2_1.re6) + pgdata1_1.c07 * r5 / 
		pgdata2_1.re12;
	h2 -= pgdata1_1.c07 * r7 / pgdata2_1.re14;
	goto L300;
    } else if (*n == 9) {
	h1 = pgdata1_1.c03 * -21420. / (r6 * pgdata2_1.re2) + pgdata1_1.c02 * 
		88200. / r8;
	h1 = h1 - pgdata1_1.c0 * 181440. * pgdata2_1.re2 / r10 - 
		pgdata1_1.c04 * 105. / pgdata2_1.re8;
	h1 = h1 + pgdata1_1.c04 * 3465. / (r4 * pgdata2_1.re4) - 
		pgdata1_1.c05 * 420. * r2 / pgdata2_1.re10;
	h1 = h1 + pgdata1_1.c06 * 42. / pgdata2_1.re8 - pgdata1_1.c06 * 210. *
		 r4 / pgdata2_1.re12;
	h1 = h1 - pgdata1_1.c05 * 420. / (r2 * pgdata2_1.re6) - pgdata1_1.c07 
		* 28. * r6 / pgdata2_1.re14;
	h1 = h1 + pgdata1_1.c08 * r4 / pgdata2_1.re12 - pgdata1_1.c08 * r8 / 
		pgdata2_1.re16;
	h2 = pgdata1_1.c02 * 2520. / (r6 * pgdata2_1.re2) - pgdata1_1.c0 * 
		5040. / r8 - pgdata1_1.c04 * 105. / pgdata2_1.re8;
	h2 = h2 - pgdata1_1.c03 * 630. / (r4 * pgdata2_1.re4) - pgdata1_1.c05 
		* 420. * r2 / pgdata2_1.re10;
	h2 = h2 + pgdata1_1.c06 * 42. * r2 / pgdata2_1.re10 - pgdata1_1.c06 * 
		210. * r4 / pgdata2_1.re12;
	h2 = h2 + pgdata1_1.c04 * 105. / (r2 * pgdata2_1.re6) + pgdata1_1.c07 
		* 14. * r4 / pgdata2_1.re12;
	h2 = h2 - pgdata1_1.c07 * 28. * r6 / pgdata2_1.re14 + pgdata1_1.c08 * 
		r6 / pgdata2_1.re14 - pgdata1_1.c08 * r8 / pgdata2_1.re16;
	goto L300;
    } else if (*n == 10) {
	h1 = pgdata1_1.c03 * 216720. / (r7 * pgdata2_1.re2) - pgdata1_1.c02 * 
		887040. / r9;
	h1 += pgdata1_1.c0 * 1814400 * pgdata2_1.re2 / r11;
	h1 = h1 - pgdata1_1.c04 * 35280. / (r5 * pgdata2_1.re4) - 
		pgdata1_1.c05 * 945. * *r__ / pgdata2_1.re10;
	h1 = h1 - pgdata1_1.c06 * 1260. * r3 / pgdata2_1.re12 + pgdata1_1.c05 
		* 4305. / (r3 * pgdata2_1.re6);
	h1 = h1 - pgdata1_1.c07 * 378. * r5 / pgdata2_1.re14 + pgdata1_1.c08 *
		 4. * r3 / pgdata2_1.re12;
	h1 = h1 - pgdata1_1.c08 * 36. * r7 / pgdata2_1.re16 - pgdata1_1.c06 * 
		420. / (*r__ * pgdata2_1.re8);
	h1 = h1 + pgdata1_1.c07 * 42. * *r__ / pgdata2_1.re10 + pgdata1_1.c09 
		* r5 / pgdata2_1.re14 - pgdata1_1.c09 * r9 / pgdata2_1.re18;
	h2 = pgdata1_1.c02 * -20160. / (r7 * pgdata2_1.re2) + pgdata1_1.c0 * 
		40320. / r9;
	h2 = h2 + pgdata1_1.c03 * 5040. / (r5 * pgdata2_1.re4) - 
		pgdata1_1.c05 * 945. * *r__ / pgdata2_1.re10;
	h2 = h2 + pgdata1_1.c06 * 84. * *r__ / pgdata2_1.re10 - pgdata1_1.c06 
		* 1260. * r3 / pgdata2_1.re12;
	h2 = h2 - pgdata1_1.c04 * 840. / (r3 * pgdata2_1.re6) + pgdata1_1.c07 
		* 98. * r3 / pgdata2_1.re12;
	h2 = h2 - pgdata1_1.c07 * 378. * r5 / pgdata2_1.re14 + pgdata1_1.c08 *
		 20. * r5 / pgdata2_1.re14;
	h2 = h2 - pgdata1_1.c08 * 36. * r7 / pgdata2_1.re16 + pgdata1_1.c05 * 
		105. / (*r__ * pgdata2_1.re8);
	h2 = h2 + pgdata1_1.c09 * r7 / pgdata2_1.re16 - pgdata1_1.c09 * r9 / 
		pgdata2_1.re18;
	goto L300;
    } else if (*n == 11) {
	h1 = pgdata1_1.c03 * -2404080. / (r8 * pgdata2_1.re2) + pgdata1_1.c02 
		* 9797760. / r10;
	h1 = h1 - pgdata1_1.c0 * 19958400. * pgdata2_1.re2 / r12 + 
		pgdata1_1.c04 * 393120. / (r6 * pgdata2_1.re4);
	h1 = h1 - pgdata1_1.c05 * 945. / pgdata2_1.re10 - pgdata1_1.c06 * 
		4725. * r2 / pgdata2_1.re12;
	h1 = h1 - pgdata1_1.c05 * 48195. / (r4 * pgdata2_1.re6) - 
		pgdata1_1.c07 * 3150. * r4 / pgdata2_1.re14;
	h1 = h1 + pgdata1_1.c08 * 54. * r2 / pgdata2_1.re12 - pgdata1_1.c08 * 
		630. * r6 / pgdata2_1.re16;
	h1 = h1 - pgdata1_1.c07 * 378. / pgdata2_1.re10 + pgdata1_1.c06 * 
		4725. / (r2 * pgdata2_1.re8);
	h1 = h1 + pgdata1_1.c09 * 9. * r4 / pgdata2_1.re14 - pgdata1_1.c09 * 
		45. * r8 / pgdata2_1.re18;
	h1 = h1 + pgdata1_1.c010 * r6 / pgdata2_1.re16 - pgdata1_1.c010 * r10 
		/ pgdata2_1.re20;
	h2 = pgdata1_1.c02 * 181440. / (r8 * pgdata2_1.re2) - pgdata1_1.c0 * 
		362880. / r10;
	h2 = h2 - pgdata1_1.c03 * 45360. / (r6 * pgdata2_1.re4) - 
		pgdata1_1.c05 * 945. / pgdata2_1.re10;
	h2 = h2 + pgdata1_1.c06 * 189. / pgdata2_1.re10 - pgdata1_1.c06 * 
		4725. * r2 / pgdata2_1.re12;
	h2 = h2 + pgdata1_1.c04 * 7560. / (r4 * pgdata2_1.re6) + 
		pgdata1_1.c07 * 378. * r2 / pgdata2_1.re12;
	h2 = h2 - pgdata1_1.c07 * 3150. * r4 / pgdata2_1.re14 + pgdata1_1.c08 
		* 198. * r4 / pgdata2_1.re14;
	h2 = h2 - pgdata1_1.c08 * 630. * r6 / pgdata2_1.re16 - pgdata1_1.c05 *
		 945. / (r2 * pgdata2_1.re8);
	h2 = h2 + pgdata1_1.c09 * 27. * r6 / pgdata2_1.re16 - pgdata1_1.c09 * 
		45. * r8 / pgdata2_1.re18;
	h2 = h2 + pgdata1_1.c010 * r8 / pgdata2_1.re18 - pgdata1_1.c010 * r10 
		/ pgdata2_1.re20;
    }
/* --- */
L300:
    ret_val = g1 * h1 + g2 * h2;
/* --- */
/*       goto  800 */
/* ---------------------------------------------------------- */
L800:
    return ret_val;
} /* fpot_ */


/* Subroutine */ int calevj_(integer *nv, integer *nj, integer *lj, integer *
	nd, doublereal *gg)
{
    /* Format strings */
    static char fmt_500[] = "(///10x,\002--- Ro-vibrational constants & ener"
	    "gies --- \002,//6x,\002All calculated constants are the function"
	    "s of the input \002,/6x,\002\"We\", \"Re\", & \"mu\",  and are b"
	    "ased on the perturbation \002,/6x,\002theory,  except that \"W"
	    "e\" is the input value.\002,///8x,\002** The vibrational constan"
	    "ts (in a.u.) are : ** \002,/6x,\002evaluated using force constan"
	    "ts from NUMerical deriv.\002,//10x,\002[ Error% = 100.0*(|Calc.|"
	    " - |Input|)/Calc. ] \002)";
    static char fmt_510[] = "(/5x,\002          Calculated              Inpu"
	    "t \002,13x,\002Error% \002,//3x,\002 W0  = \002,1pe20.12,/3x,"
	    "\002 We0 = \002,1pe20.12,/3x,\002 We  = \002,1pe20.12,2x,1pe20.1"
	    "2,/3x,\002WeXe = \002,1pe20.12,2x,1pe20.12,2x,1pe13.5,/3x,\002We"
	    "Ye = \002,1pe20.12,2x,1pe20.12,2x,1pe13.5,/3x,\002WeZe = \002,1p"
	    "e20.12,2x,1pe20.12,2x,1pe13.5,/3x,\002WeTe = \002,1pe20.12,2x,1p"
	    "e20.12,2x,1pe13.5,/3x,\002WeSe = \002,1pe20.12,/3x,\002WeRe ="
	    " \002,1pe20.12)";
    static char fmt_520[] = "(//13x,\002The vibrational constants (in cm-1) "
	    "are : \002)";
    static char fmt_530[] = "(///12x,\002## The rotational constants (in a.u"
	    ".) are : ## \002)";
    static char fmt_540[] = "(/5x,\002             Calculated               "
	    "Input \002,13x,\002Error% \002,//3x,\002     Be = \002,1pe20.12,"
	    "2x,1pe20.12,2x,1pe13.5,/3x,\002Alpha_e = \002,1pe20.12,2x,1pe20."
	    "12,2x,1pe13.5,/3x,\002Gamma_e = \002,1pe20.12,2x,1pe20.12,2x,1pe"
	    "13.5,/3x,\002 Eta_e3 = \002,1pe20.12,/3x,\002 Eta_e4 = \002,1pe2"
	    "0.12,/3x,\002 Eta_e5 = \002,1pe20.12,/3x,\002 Eta_e6 = \002,1pe2"
	    "0.12,/3x,\002 Eta_e7 = \002,1pe20.12,//3x,\002    D_e = \002,1pe"
	    "20.12,2x,1pe20.12,2x,1pe13.5,/3x,\002 Beta_e = \002,1pe20.12,2x,"
	    "1pe20.12,2x,1pe13.5,/3x,\002 Xsi_e2 = \002,1pe20.12,/3x,\002 Xsi"
	    "_e3 = \002,1pe20.12,/3x,\002 Xsi_e4 = \002,1pe20.12,/3x,\002 Xsi"
	    "_e5 = \002,1pe20.12,/3x,\002 Xsi_e6 = \002,1pe20.12,/3x,\002 Xsi"
	    "_e7 = \002,1pe20.12)";
    static char fmt_550[] = "(//15x,\002The rotational constants (in cm-1) a"
	    "re : \002)";
    static char fmt_552[] = "(///15x,\002    Check VIBRATIONAL constants :"
	    " \002,//8x,\002[ As V(R)=De, De can be calculated by Rees's Eq."
	    " \002,/8x,\002            from  VIBrational constants.        "
	    " \002,//8x,\002    Quadratic potential form :   \002,/8x,\002  D"
	    "e_2c -- De from calculated vibrational constants; \002,/8x,\002 "
	    " De_2i -- De from  inputted  vibrational constants. \002,//8x"
	    ",\002      CUBIC   potential form (much accurate) :   \002,/8x"
	    ",\002  De_3c -- De from calculated vibrational constants; \002,/"
	    "8x,\002  De_3i -- De from  inputted  vibrational constants. \002"
	    ",//8x,\002  Higher_power potential form is much more accurate."
	    " \002,/8x,\002    Set De_3i & D3i% = 0.0 if inputted WeYe is 0.0"
	    " . \002,//8x,\002       Dei = Input 'TRUE' dissociation energy D"
	    "e :   \002,//8x,\002           D2c% = 100 * | De_2c - Dei |/Dei "
	    ";   \002,/8x,\002           D2i% = 100 * | De_2i - Dei |/Dei ;   "
	    "\002,/8x,\002           D3c% = 100 * | De_3c - Dei |/Dei ;   "
	    "\002,/8x,\002           D3i% = 100 * | De_3i - Dei |/Dei .      "
	    "   ]\002,//18x,\002    Dei == De = \002,f12.8,\002  a.u.\002,//4"
	    "x,\002 De_2c(a.u)      De_2i(a.u)      De_3c(a.u)     \002,\002 "
	    "De_3i(a.u) \002,/2x,4(1f12.8,4x),//8x,\002          Percent erro"
	    "rs of generated De : \002,/4x,\002   D2c%            D2i%       "
	    "      D3c%           D3i% \002,/2x,4(1f12.6,4x))";
    static char fmt_560[] = "(///17x,\002=== The VIBrational energies are : "
	    "=== \002,//19x,\002    [  Ev_dif = E(v) - E(v-1)  ] \002,//5x"
	    ",\002v      E(v; a.u.)        E(v; cm-1)     Ev_dif(a.u)\002,"
	    "\002   Ev_dif(cm-1)\002,/)";
    static char fmt_570[] = "(3x,i3,1pe18.9,1pe18.9,x,1pe13.5,x,1pe13.5)";
    static char fmt_575[] = "(/12x,\002The SUM of  Ev_dif(a.u) = \002,1pe16."
	    "8,/12x,\002Maximum energy  Ev(max) = \002,1pe16.8,/)";
    static char fmt_580[] = "(//5x,\002v      E(v; a.u.)      Enew(v; a.u."
	    ")\002,\002    Ev_dif(a.u.)\002,/)";
    static char fmt_585[] = "(//16x,\002Comparing vibrational energies (I)"
	    " : \002,//12x,\002[ Enew(v) = w0 + we0*(v + 0.5) ; \002,/12x,"
	    "\002    Ea(v) = We*(v + 0.5) - wexe*(v + 0.5)**2 ; \002,/12x,"
	    "\002    Eb(v) = We*(v + 0.5) - WeXe*(v + 0.5)**2 ; \002,/12x,"
	    "\002     E(v) = Enew(v) + Ea(v) - ...            ; \002,//12x"
	    ",\002          wexe is calculated by code; \002,/12x,\002       "
	    "   WeXe is  the  INPUT  value;          \002,/12x,\002       w0 "
	    "& we0 are calculated NEW terms.        \002,//12x,\002     If mt"
	    "yp = 1, w0 & we0 are NOT defined.     ] \002,//5x,\002v      E(v"
	    "; a.u.)     Enew(v; a.u.)     Ea(v; a.u.)\002,\002   Eb(v; a.u."
	    ")\002,/)";
    static char fmt_590[] = "(//17x,\002Comparing vibrational energies (II) "
	    ": \002,//12x,\002 [  Eb(v) = We*(v + 0.5) - WeXe *(v + 0.5)**2 ; "
	    "\002,/12x,\002    Em(v) = We*(v + 0.5) - wexem*(v + 0.5)**2 ;"
	    " \002,/12x,\002    Eh(v) = We*(v + 0.5)                      ."
	    " \002,//12x,\002          WeXe   is  the   INPUT  value;       "
	    " \002,/12x,\002          wexem  is  from  Morse formulae.      "
	    " \002,//12x,\002    Eb is the approximate exp't vib. energies;"
	    " \002,/12x,\002    Em is the  Morse   vibrational   energies;"
	    " \002,/12x,\002    Eh is the   SHO    vibrational   energies.   "
	    "] \002,//10x,\002        Calculated        Input         Morse   "
	    "\002,/10x,\002           wex            WeXe          wexem  "
	    " \002,/9x,\002WeXe =\002,3(1pe14.6,x),\002  a.u. \002,/15x,3(1pe"
	    "14.6,x),\002  cm-1 \002,//5x,\002v      E(v; a.u.)      Eb(v; a."
	    "u.)      Em(v; a.u.)\002,\002   Eh(v; a.u.)\002,/)";
    static char fmt_710[] = "(///19x,\002===  Rotational energies :  === "
	    "\002,//14x,\002[ EJ(J) = Be*J(J+1) + D_e*J*J*(J+1)**2        "
	    "\002,/14x,\002  Er(J) = Brigid * J(J+1)                     \002"
	    ",/14x,\002     JJ = dsqrt( J(J+1) )                     \002,/14"
	    "x,\002   w(J) = dsqrt( J(J+1) )/Inertia             \002,//14x"
	    ",\002  EJ(J) = Non-rigid rotor  rot.  energies ;   \002,/14x,"
	    "\002  Er(J) = Rigid rotor rotational energies ;   \002,/14x,\002"
	    "     JJ = Classicle rot. angular momentum ;   \002,/14x,\002   w"
	    "(J) = Rotational angular velocity.      ] \002,//14x,\002  Inert"
	    "ia =   u*Re*Re   = \002,1pe16.8,/14x,\002   Brigid = 1/2*u*Re*Re"
	    " = \002,1pe16.8,//5x,\002J      EJ(J; a.u)        Er(J; a.u)    "
	    "     JJ  \002,\002         w(J)   \002,/)";
    static char fmt_600[] = "(///21x,\002The VIB-ROTational energies are :"
	    " \002,//15x,\002[ A. L. G. Rees, Proc. Phys. Soc.(London) \002,/"
	    "15x,\002             59, 998(1947) :              \002,/15x,\002"
	    "  E(v,j) = ..., + D_e*J*J*(J+1)**2 + ... \002,//15x,\002       G"
	    ". Herzberg, (1953) :              \002,/15x,\002  E(v,j) = ..., "
	    "- D_e*J*J*(J+1)**2 + ... \002,//16x,\002       Evj_dif = E(v,j) "
	    "- E(v,j-1)         ] \002,//15x,\002{ ade =  1.0, use   Rees's  "
	    " definition; \002,/15x,\002      = -1.0, use Herzberg's definiti"
	    "on.    } \002,//31x,\002  ade =\002,f5.1,//5x,\002v  j    E(v,j;"
	    " a.u.)      E(v,j; cm-1)    Evj_dif(a.u)\002,\002  Evj_dif(cm-1"
	    ")\002,/)";
    static char fmt_610[] = "(3x,2i3,1pe18.9,1pe18.9,x,1pe13.5,x,1pe13.5)";
    static char fmt_602[] = "(16x,\002-----\002,13x,\002-----\002,12x,\002--"
	    "---\002,9x,\002-----\002,/)";
    static char fmt_605[] = "(/12x,\002The SUM of Evj_dif(a.u) = \002,1pe16."
	    "8,/12x,\002Maximum energy Evj(max) = \002,1pe16.8,/)";
    static char fmt_630[] = "(///13x,\002Contributions to E_vj are (Part I) "
	    ": \002,//7x,\002[   E(v,j) = E(v) + Ej ;     Ej = Ej1 + Ej2  ;"
	    " \002,/7x,\002  Ej1 = E{v;j*(j*1)} ;  Ej2 = E{v;j*j*(j*1)**2}  ] "
	    "\002,//5x,\002v  j    E(v,j; a.u.)        E(v;a.u.)       Ej(a.u"
	    ".) \002,/)";
    static char fmt_635[] = "(///18x,\002Separate contributions to E_vj  :"
	    " \002,//12x,\002[   E(v,j) = E(v-j_coupling) + E(v) + E(j) ; "
	    "\002,//12x,\002    E(v-j_coup) = Vib-Rot_coupling energies; \002"
	    ",/12x,\002           E(v) =    Vibrational   energies; \002,/12x,"
	    "\002           E(j) =    Rotational    energies.  ] \002,//5x"
	    ",\002v  j    E(v,j; a.u.)       E(v-j_coup)     E(v; a.u.) \002"
	    ",\002    E(j; a.u.) \002,/)";
    static char fmt_640[] = "(///17x,\002Contributions to E_vj are (Part II)"
	    " : \002,//19x,\002[  E(v,j) = E(v) + Ej1 + Ej2  ] \002,//5x,\002"
	    "v  j    E(v,j; a.u.)        E(v;a.u.)       Ej1(a.u.) \002,\002 "
	    "   Ej2(a.u.) \002,/)";
    static char fmt_650[] = "(///12x,\002Contributions to E_vj are (Part III"
	    ") : \002,//16x,\002[   E'(v,j) = E(v) + Ej1  ] \002,//5x,\002v  "
	    "j    E(v,j; a.u.)      E'(v,j; a.u.)     Ej1(a.u.) \002,/)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8, d__9, d__10, 
	    d__11, d__12, d__13, d__14, d__15, d__16, d__17, d__18, d__19, 
	    d__20, d__21;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void), do_fio(integer *, char *, ftnlen);
    double sqrt(doublereal);
    integer s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer i__, j;
    static doublereal bj, eh[200], ej[200], em[200];
    static integer ij;
    static doublereal rb, er[200], eu[200], ev[200], ew[200], ex[200], ra, rg,
	     sd, sb, bv, pt;
    static integer kk, kv;
    static doublereal px, py, pz;
    static integer jj, kj;
    static doublereal aj1[40000]	/* was [200][200] */, aj2[40000]	
	    /* was [200][200] */, al0, bj0, bj2, ej1, ej2, em0, eh0, bv0;
    static integer nj0;
    static doublereal ev0, ex0;
    static integer nv1, nv2;
    static doublereal ejb, ejc, bv02, bv03, bv04, bv05, bv06, euj[40000]	
	    /* was [200][200] */, evj[40000]	/* was [200][200] */, ewj[
	    40000]	/* was [200][200] */, bv07, evb, bjj, bvj, ej1a, ej2a,
	     e2cp, e3cp, e2ip, e3ip, aucm, difv, emax, auev, difj0, difvj;
    extern /* Subroutine */ int convj_(doublereal *, integer *);
    static doublereal wexem, rydev, dise2c, dise3c, dise2i, dise3i, brigid;
    extern /* Subroutine */ int vjcoef_(void);
    static doublereal sumdif, evjmax, rinert;

    /* Fortran I/O blocks */
    static cilist io___402 = { 0, 0, 0, fmt_500, 0 };
    static cilist io___407 = { 0, 0, 0, fmt_510, 0 };
    static cilist io___408 = { 0, 0, 0, fmt_520, 0 };
    static cilist io___409 = { 0, 0, 0, fmt_510, 0 };
    static cilist io___410 = { 0, 0, 0, fmt_530, 0 };
    static cilist io___416 = { 0, 0, 0, fmt_540, 0 };
    static cilist io___417 = { 0, 0, 0, fmt_550, 0 };
    static cilist io___418 = { 0, 0, 0, fmt_540, 0 };
    static cilist io___429 = { 0, 6, 0, fmt_552, 0 };
    static cilist io___465 = { 0, 6, 0, fmt_560, 0 };
    static cilist io___466 = { 0, 35, 0, fmt_560, 0 };
    static cilist io___467 = { 0, 36, 0, fmt_560, 0 };
    static cilist io___473 = { 0, 6, 0, 0, 0 };
    static cilist io___474 = { 0, 35, 0, 0, 0 };
    static cilist io___475 = { 0, 36, 0, 0, 0 };
    static cilist io___476 = { 0, 35, 0, fmt_570, 0 };
    static cilist io___477 = { 0, 6, 0, fmt_570, 0 };
    static cilist io___478 = { 0, 36, 0, fmt_570, 0 };
    static cilist io___479 = { 0, 6, 0, fmt_575, 0 };
    static cilist io___480 = { 0, 35, 0, fmt_575, 0 };
    static cilist io___481 = { 0, 6, 0, fmt_580, 0 };
    static cilist io___482 = { 0, 35, 0, fmt_580, 0 };
    static cilist io___483 = { 0, 6, 0, 0, 0 };
    static cilist io___484 = { 0, 35, 0, 0, 0 };
    static cilist io___485 = { 0, 6, 0, fmt_570, 0 };
    static cilist io___486 = { 0, 35, 0, fmt_570, 0 };
    static cilist io___487 = { 0, 6, 0, fmt_585, 0 };
    static cilist io___488 = { 0, 35, 0, fmt_585, 0 };
    static cilist io___489 = { 0, 6, 0, 0, 0 };
    static cilist io___490 = { 0, 35, 0, 0, 0 };
    static cilist io___491 = { 0, 35, 0, fmt_570, 0 };
    static cilist io___492 = { 0, 6, 0, fmt_570, 0 };
    static cilist io___493 = { 0, 6, 0, fmt_590, 0 };
    static cilist io___494 = { 0, 35, 0, fmt_590, 0 };
    static cilist io___499 = { 0, 6, 0, 0, 0 };
    static cilist io___500 = { 0, 35, 0, 0, 0 };
    static cilist io___501 = { 0, 35, 0, fmt_570, 0 };
    static cilist io___502 = { 0, 6, 0, fmt_570, 0 };
    static cilist io___503 = { 0, 6, 0, fmt_710, 0 };
    static cilist io___506 = { 0, 6, 0, fmt_570, 0 };
    static cilist io___507 = { 0, 35, 0, fmt_570, 0 };
    static cilist io___508 = { 0, 36, 0, fmt_570, 0 };
    static cilist io___509 = { 0, 6, 0, fmt_600, 0 };
    static cilist io___510 = { 0, 35, 0, fmt_600, 0 };
    static cilist io___511 = { 0, 36, 0, fmt_600, 0 };
    static cilist io___517 = { 0, 6, 0, 0, 0 };
    static cilist io___518 = { 0, 35, 0, 0, 0 };
    static cilist io___519 = { 0, 36, 0, 0, 0 };
    static cilist io___520 = { 0, 35, 0, fmt_610, 0 };
    static cilist io___521 = { 0, 6, 0, fmt_610, 0 };
    static cilist io___522 = { 0, 36, 0, fmt_610, 0 };
    static cilist io___523 = { 0, 6, 0, 0, 0 };
    static cilist io___524 = { 0, 35, 0, 0, 0 };
    static cilist io___525 = { 0, 36, 0, 0, 0 };
    static cilist io___526 = { 0, 6, 0, fmt_602, 0 };
    static cilist io___527 = { 0, 35, 0, fmt_602, 0 };
    static cilist io___528 = { 0, 36, 0, fmt_602, 0 };
    static cilist io___529 = { 0, 35, 0, fmt_605, 0 };
    static cilist io___530 = { 0, 6, 0, fmt_605, 0 };
    static cilist io___531 = { 0, 35, 0, fmt_630, 0 };
    static cilist io___532 = { 0, 6, 0, fmt_635, 0 };
    static cilist io___533 = { 0, 35, 0, 0, 0 };
    static cilist io___534 = { 0, 6, 0, 0, 0 };
    static cilist io___535 = { 0, 35, 0, fmt_610, 0 };
    static cilist io___536 = { 0, 6, 0, fmt_610, 0 };
    static cilist io___537 = { 0, 6, 0, 0, 0 };
    static cilist io___538 = { 0, 35, 0, 0, 0 };
    static cilist io___539 = { 0, 6, 0, fmt_602, 0 };
    static cilist io___540 = { 0, 35, 0, fmt_602, 0 };
    static cilist io___541 = { 0, 35, 0, fmt_640, 0 };
    static cilist io___542 = { 0, 35, 0, fmt_610, 0 };
    static cilist io___543 = { 0, 35, 0, 0, 0 };
    static cilist io___544 = { 0, 35, 0, fmt_650, 0 };
    static cilist io___545 = { 0, 35, 0, fmt_610, 0 };
    static cilist io___546 = { 0, 35, 0, 0, 0 };


/* ---------------------------------------------------------- */
/*  (nv-1) - The HIGHEST vibrational state. */
/*         nv = the number of vibrational states used. */
/*  nj - Number of rotational states in each v state. */
/*  nd - The size of the force constants array "gg". */
/*  gg - The force constants array. */
/* ========================================================== */
    /* Parameter adjustments */
    --nj;
    --gg;

    /* Function Body */
    rydev = 13.60569809;
    auev = 27.21139618;
    aucm = 219474.6306;
    rinert = spectra_1.amu * spectra_1.re * spectra_1.re;
    brigid = 1. / (rinert * 2.);
/* ---------------------------------------------------------- */
/*  Calculate coefficients for ro-vib constants */
/* ---------------------------------------------------------- */
    vjcoef_();
/* ---------------------------------------------------------- */
/*  Calculate vibrational-rotational constants for E_vj */
/* ---------------------------------------------------------- */
    convj_(&gg[1], nd);
/* ---------------------------------------------------------- */
/*  Write out vib-rot constants */
/* ---------------------------------------------------------- */
    ij = 6;
    for (i__ = 1; i__ <= 2; ++i__) {
	io___402.ciunit = ij;
	s_wsfe(&io___402);
	e_wsfe();
	if (spectra_1.wexe != 0.) {
	    px = (abs(spectr0_1.wex) - abs(spectra_1.wexe)) * 100. / 
		    spectr0_1.wex;
	}
	if (spectra_1.weye != 0.) {
	    py = (abs(spectr0_1.wey) - abs(spectra_1.weye)) * 100. / 
		    spectr0_1.wey;
	}
	if (spectra_1.weze != 0.) {
	    pz = (abs(spectr0_1.wez) - abs(spectra_1.weze)) * 100. / 
		    spectr0_1.wez;
	}
	if (spectra_1.wete != 0.) {
	    pt = (abs(spectr0_1.wet) - abs(spectra_1.wete)) * 100. / 
		    spectr0_1.wet;
	}
	io___407.ciunit = ij;
	s_wsfe(&io___407);
	do_fio(&c__1, (char *)&spectr0_1.w0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr0_1.we0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr0_1.wex, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.wexe, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&px, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr0_1.wey, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.weye, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&py, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr0_1.wez, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.weze, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&pz, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr0_1.wet, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.wete, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&pt, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr0_1.wes, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr0_1.wer, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___408.ciunit = ij;
	s_wsfe(&io___408);
	e_wsfe();
	io___409.ciunit = ij;
	s_wsfe(&io___409);
	d__1 = spectr0_1.w0 * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__2 = spectr0_1.we0 * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	d__3 = spectra_1.we * aucm;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	d__4 = spectra_1.we * aucm;
	do_fio(&c__1, (char *)&d__4, (ftnlen)sizeof(doublereal));
	d__5 = spectr0_1.wex * aucm;
	do_fio(&c__1, (char *)&d__5, (ftnlen)sizeof(doublereal));
	d__6 = spectra_1.wexe * aucm;
	do_fio(&c__1, (char *)&d__6, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&px, (ftnlen)sizeof(doublereal));
	d__7 = spectr0_1.wey * aucm;
	do_fio(&c__1, (char *)&d__7, (ftnlen)sizeof(doublereal));
	d__8 = spectra_1.weye * aucm;
	do_fio(&c__1, (char *)&d__8, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&py, (ftnlen)sizeof(doublereal));
	d__9 = spectr0_1.wez * aucm;
	do_fio(&c__1, (char *)&d__9, (ftnlen)sizeof(doublereal));
	d__10 = spectra_1.weze * aucm;
	do_fio(&c__1, (char *)&d__10, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&pz, (ftnlen)sizeof(doublereal));
	d__11 = spectr0_1.wet * aucm;
	do_fio(&c__1, (char *)&d__11, (ftnlen)sizeof(doublereal));
	d__12 = spectra_1.wete * aucm;
	do_fio(&c__1, (char *)&d__12, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&pt, (ftnlen)sizeof(doublereal));
	d__13 = spectr0_1.wes * aucm;
	do_fio(&c__1, (char *)&d__13, (ftnlen)sizeof(doublereal));
	d__14 = spectr0_1.wer * aucm;
	do_fio(&c__1, (char *)&d__14, (ftnlen)sizeof(doublereal));
	e_wsfe();

	io___410.ciunit = ij;
	s_wsfe(&io___410);
	e_wsfe();
	if (spectrb_1.be != 0.) {
	    rb = (abs(spectr1_1.bee) - abs(spectrb_1.be)) * 100. / 
		    spectr1_1.bee;
	}
	if (spectrb_1.alphae != 0.) {
	    ra = (abs(spectr1_1.ale) - abs(spectrb_1.alphae)) * 100. / 
		    spectr1_1.ale;
	}
	if (spectrb_1.gamae != 0.) {
	    rg = (abs(spectr1_1.gae) - abs(spectrb_1.gamae)) * 100. / 
		    spectr1_1.gae;
	}
	if (spectrb_1.der != 0.) {
	    sd = (abs(spectr2_1.dee) - abs(spectrb_1.der)) * 100. / 
		    spectr2_1.dee;
	}
	if (spectrb_1.betae != 0.) {
	    sb = (abs(spectr2_1.bete) - abs(spectrb_1.betae)) * 100. / 
		    spectr2_1.bete;
	}
	io___416.ciunit = ij;
	s_wsfe(&io___416);
	do_fio(&c__1, (char *)&spectr1_1.bee, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectrb_1.be, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr1_1.ale, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectrb_1.alphae, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ra, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr1_1.gae, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectrb_1.gamae, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rg, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr1_1.eta3, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr1_1.eta4, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr1_1.eta5, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr1_1.eta6, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr1_1.eta7, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr2_1.dee, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectrb_1.der, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sd, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr2_1.bete, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectrb_1.betae, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr2_1.xsi2, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr2_1.xsi3, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr2_1.xsi4, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr2_1.xsi5, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr2_1.xsi6, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr2_1.xsi7, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___417.ciunit = ij;
	s_wsfe(&io___417);
	e_wsfe();
	io___418.ciunit = ij;
	s_wsfe(&io___418);
	d__1 = spectr1_1.bee * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__2 = spectrb_1.be * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rb, (ftnlen)sizeof(doublereal));
	d__3 = spectr1_1.ale * aucm;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	d__4 = spectrb_1.alphae * aucm;
	do_fio(&c__1, (char *)&d__4, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ra, (ftnlen)sizeof(doublereal));
	d__5 = spectr1_1.gae * aucm;
	do_fio(&c__1, (char *)&d__5, (ftnlen)sizeof(doublereal));
	d__6 = spectrb_1.gamae * aucm;
	do_fio(&c__1, (char *)&d__6, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rg, (ftnlen)sizeof(doublereal));
	d__7 = spectr1_1.eta3 * aucm;
	do_fio(&c__1, (char *)&d__7, (ftnlen)sizeof(doublereal));
	d__8 = spectr1_1.eta4 * aucm;
	do_fio(&c__1, (char *)&d__8, (ftnlen)sizeof(doublereal));
	d__9 = spectr1_1.eta5 * aucm;
	do_fio(&c__1, (char *)&d__9, (ftnlen)sizeof(doublereal));
	d__10 = spectr1_1.eta6 * aucm;
	do_fio(&c__1, (char *)&d__10, (ftnlen)sizeof(doublereal));
	d__11 = spectr1_1.eta7 * aucm;
	do_fio(&c__1, (char *)&d__11, (ftnlen)sizeof(doublereal));
	d__12 = spectr2_1.dee * aucm;
	do_fio(&c__1, (char *)&d__12, (ftnlen)sizeof(doublereal));
	d__13 = spectrb_1.der * aucm;
	do_fio(&c__1, (char *)&d__13, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sd, (ftnlen)sizeof(doublereal));
	d__14 = spectr2_1.bete * aucm;
	do_fio(&c__1, (char *)&d__14, (ftnlen)sizeof(doublereal));
	d__15 = spectrb_1.betae * aucm;
	do_fio(&c__1, (char *)&d__15, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sb, (ftnlen)sizeof(doublereal));
	d__16 = spectr2_1.xsi2 * aucm;
	do_fio(&c__1, (char *)&d__16, (ftnlen)sizeof(doublereal));
	d__17 = spectr2_1.xsi3 * aucm;
	do_fio(&c__1, (char *)&d__17, (ftnlen)sizeof(doublereal));
	d__18 = spectr2_1.xsi4 * aucm;
	do_fio(&c__1, (char *)&d__18, (ftnlen)sizeof(doublereal));
	d__19 = spectr2_1.xsi5 * aucm;
	do_fio(&c__1, (char *)&d__19, (ftnlen)sizeof(doublereal));
	d__20 = spectr2_1.xsi6 * aucm;
	do_fio(&c__1, (char *)&d__20, (ftnlen)sizeof(doublereal));
	d__21 = spectr2_1.xsi7 * aucm;
	do_fio(&c__1, (char *)&d__21, (ftnlen)sizeof(doublereal));
	e_wsfe();
	if (i__ == 1) {
	    ij = 35;
	}
    }
/* ---------------------------------------------------------- */
/*  Calculate Morse parameters : */
/* ---------------------------------------------------------- */
    al0 = spectra_1.re * sqrt(spectra_1.amu * .5 * spectra_1.we * 
	    spectra_1.we / spectra_1.de);
    wexem = al0 * al0 / (spectra_1.amu * 2. * spectra_1.re * spectra_1.re);
/* ---------------------------------------------------------- */
/*  Check if the vibrational constants reproduce De */
/*      c : calculated data;  i : input data. */
/* ---------------------------------------------------------- */
/* --- From quadratic form : */
    dise2c = spectra_1.we * spectra_1.we / (spectr0_1.wex * 4.);
    dise2i = spectra_1.we * spectra_1.we / (spectra_1.wexe * 4.);
/* --- From CUBIC form : */
/* Computing 2nd power */
    d__2 = spectr0_1.wex;
/* Computing 3rd power */
    d__1 = sqrt(d__2 * d__2 - spectra_1.we * 3. * spectr0_1.wey);
    dise3c = d__1 * (d__1 * d__1) * 2.;
/* Computing 2nd power */
    d__1 = spectr0_1.wex;
    dise3c -= spectr0_1.wex * (d__1 * d__1 * 2. - spectra_1.we * 9. * 
	    spectr0_1.wey);
    dise3c /= spectr0_1.wey * 27. * spectr0_1.wey;

/* Computing 2nd power */
    d__2 = spectra_1.wexe;
/* Computing 3rd power */
    d__1 = sqrt(d__2 * d__2 - spectra_1.we * 3. * spectra_1.weye);
    dise3i = d__1 * (d__1 * d__1) * 2.;
/* Computing 2nd power */
    d__1 = spectra_1.wexe;
    dise3i -= spectra_1.wexe * (d__1 * d__1 * 2. - spectra_1.we * 9. * 
	    spectra_1.weye);
    if (abs(spectra_1.weye) > 0.) {
	dise3i /= spectra_1.weye * 27. * spectra_1.weye;
    } else {
	dise3i = 0.;
    }
    e2cp = (d__1 = dise2c - spectra_1.de, abs(d__1)) * 100. / spectra_1.de;
    e2ip = (d__1 = dise2i - spectra_1.de, abs(d__1)) * 100. / spectra_1.de;
    e3cp = (d__1 = dise3c - spectra_1.de, abs(d__1)) * 100. / spectra_1.de;
    e3ip = (d__1 = dise3i - spectra_1.de, abs(d__1)) * 100. / spectra_1.de;
    if (dise3i == 0.) {
	e3ip = 0.;
    }
/* ---------------------------------------------------------- */
/*   Print and compare the calculated De to see the quality */
/* of the constants (We, wex, wey, ...) */
/* ---------------------------------------------------------- */
    s_wsfe(&io___429);
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dise2c, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dise2i, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dise3c, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dise3i, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e2cp, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e2ip, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e3cp, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e3ip, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* ---------------------------------------------------------- */
/*  Calculate vib-rot energies using vib-rot constants */
/* ---------------------------------------------------------- */
    nv1 = *nv + 1;
    nv2 = *nv + 20;
    i__1 = nv2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	bv = (i__ - 1) * 1.;
	bv0 = bv + .5;
	bv02 = bv0 * bv0;
	bv03 = bv02 * bv0;
	bv04 = bv03 * bv0;
	bv05 = bv04 * bv0;
	bv06 = bv05 * bv0;
	bv07 = bv06 * bv0;
	evb = spectr0_1.w0 + (spectra_1.we + spectr0_1.we0) * bv0 - 
		spectr0_1.wex * bv02 + eswitch_1.aye * spectr0_1.wey * bv03;
	evb = evb + eswitch_1.aze * spectr0_1.wez * bv04 + eswitch_1.ate * 
		spectr0_1.wet * bv05 + eswitch_1.ase * spectr0_1.wes * bv06;

/* --- VIBrational energies : */
	ev[i__ - 1] = evb + eswitch_1.are * spectr0_1.wer * bv07;

/* --- Approximate VIBrational energies : */
	eu[i__ - 1] = spectra_1.we * bv0 - spectr0_1.wex * bv02;
	ex[i__ - 1] = spectra_1.we * bv0 - spectra_1.wexe * bv02;
	em[i__ - 1] = spectra_1.we * bv0 - wexem * bv02;
	eh[i__ - 1] = spectra_1.we * bv0;
/* --- New terms of VIBrational energies : */
	ew[i__ - 1] = spectr0_1.w0 + spectr0_1.we0 * bv0;

	nj0 = nj[i__];
	if (nj[1] > 0 && i__ <= nv1) {
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		bj = (j - 1) * 1.;
		bj0 = bj * (bj + 1.);
		bj2 = bj0 * bj0;
		ejb = eswitc1_1.abe * spectr1_1.bee * bj0 - eswitc1_1.aae * 
			spectr1_1.ale * bj0 * bv0;
		ejb += eswitc1_1.age * spectr1_1.gae * bj0 * bv02;
		ejb = ejb - eswitc1_1.ae3 * spectr1_1.eta3 * bj0 * bv03 - 
			eswitc1_1.ae4 * spectr1_1.eta4 * bj0 * bv04;
		ejb = ejb - eswitc1_1.ae5 * spectr1_1.eta5 * bj0 * bv05 - 
			eswitc1_1.ae6 * spectr1_1.eta6 * bj0 * bv06;
		ej1 = ejb - eswitc1_1.ae7 * spectr1_1.eta7 * bj0 * bv07;
		ej1a = ej1 - eswitc1_1.abe * spectr1_1.bee * bj0;
/* --- */
/*           ejb = ejb + ae3*eta3*bj0*bv03 + ae4*eta4*bj0*bv04 */
/*           ejb = ejb + ae5*eta5*bj0*bv05 + ae6*eta6*bj0*bv06 */
/*           ej1 = ejb + ae7*eta7*bj0*bv07 */

/* --- Herzberg (1953) : */
/*           ejc = - ade*Dee*bj2 - abt*bete*bj2*bv0 */
/* --- A. L. G. Rees [ Proc. Phys. Soc. (London)59,998(1947) ] : */
		ejc = eswitc2_1.ade * spectr2_1.dee * bj2 - eswitc2_1.abt * 
			spectr2_1.bete * bj2 * bv0;

		ejc += eswitc2_1.ax2 * spectr2_1.xsi2 * bj2 * bv02;
		ejc = ejc + eswitc2_1.ax3 * spectr2_1.xsi3 * bj2 * bv03 + 
			eswitc2_1.ax4 * spectr2_1.xsi4 * bj2 * bv04;
		ejc = ejc + eswitc2_1.ax5 * spectr2_1.xsi5 * bj2 * bv05 + 
			eswitc2_1.ax6 * spectr2_1.xsi6 * bj2 * bv06;
		ej2 = ejc + eswitc2_1.ax7 * spectr2_1.xsi7 * bj2 * bv07;
		ej2a = ej2 - eswitc2_1.ade * spectr2_1.dee * bj2;

/* --- VIBrational-ROTational energies : */
		evj[i__ + j * 200 - 201] = ev[i__ - 1] + ej1 + ej2;
/* --- ROTational energies : */
		if (i__ == 1) {
		    ej[j - 1] = eswitc1_1.abe * spectr1_1.bee * bj0 + 
			    eswitc2_1.ade * spectr2_1.dee * bj2;
		}
/* --- VIBrational-ROTational COUpling energies : */
		ewj[i__ + j * 200 - 201] = ej1a + ej2a;
/* --- Other energy forms : */
		euj[i__ + j * 200 - 201] = ev[i__ - 1] + ej1;
		aj1[i__ + j * 200 - 201] = ej1;
		aj2[i__ + j * 200 - 201] = ej2;
/* --- Rigid rotor energies at R=Re : */
		if (i__ == 1) {
		    er[j - 1] = brigid * bj * (bj + 1.);
		}
	    }
	}

/* L20: */
    }
/* ---------------------------------------------------------- */
/*  Write out vib-rot energies */
/* ---------------------------------------------------------- */
    s_wsfe(&io___465);
    e_wsfe();
    s_wsfe(&io___466);
    e_wsfe();
    s_wsfe(&io___467);
    e_wsfe();
    emax = 0.;
    sumdif = 0.;
    kk = 0;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	difv = ev[i__ - 1] - ev[i__ - 2];
	if (i__ == 1) {
	    difv = ev[i__ - 1];
	}
	if (ev[i__ - 1] < ev[i__ - 2] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___473);
	    e_wsle();
	    s_wsle(&io___474);
	    e_wsle();
	    s_wsle(&io___475);
	    e_wsle();
	}
	s_wsfe(&io___476);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal));
	d__1 = ev[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	d__2 = difv * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___477);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal));
	d__1 = ev[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	d__2 = difv * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___478);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	if (ev[i__ - 1] > ev[i__ - 2]) {
	    emax = ev[i__ - 1];
	}
	if (difv > 0.) {
	    sumdif += difv;
	}
/* L30: */
    }
    s_wsfe(&io___479);
    do_fio(&c__1, (char *)&sumdif, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&emax, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___480);
    do_fio(&c__1, (char *)&sumdif, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&emax, (ftnlen)sizeof(doublereal));
    e_wsfe();

    s_wsfe(&io___481);
    e_wsfe();
    s_wsfe(&io___482);
    e_wsfe();
    kk = 0;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	difv = ev[i__ - 1] - ev[i__ - 2];
	if (i__ == 1) {
	    difv = ev[0];
	}
	if (ev[i__ - 1] < ev[i__ - 2] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___483);
	    e_wsle();
	    s_wsle(&io___484);
	    e_wsle();
	}
	s_wsfe(&io___485);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___486);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    s_wsfe(&io___487);
    e_wsfe();
    s_wsfe(&io___488);
    e_wsfe();
    kk = 0;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	if (ev[i__ - 1] < ev[i__ - 2] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___489);
	    e_wsle();
	    s_wsle(&io___490);
	    e_wsle();
	}
	s_wsfe(&io___491);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eu[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ex[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___492);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eu[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ex[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    s_wsfe(&io___493);
    do_fio(&c__1, (char *)&spectr0_1.wex, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.wexe, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&wexem, (ftnlen)sizeof(doublereal));
    d__1 = spectr0_1.wex * aucm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = spectra_1.wexe * aucm;
    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = wexem * aucm;
    do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___494);
    do_fio(&c__1, (char *)&spectr0_1.wex, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.wexe, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&wexem, (ftnlen)sizeof(doublereal));
    d__1 = spectr0_1.wex * aucm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = spectra_1.wexe * aucm;
    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = wexem * aucm;
    do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsfe();
    kk = 0;
    i__1 = nv2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	ev0 = ev[i__ - 1];
	ex0 = ex[i__ - 1];
	em0 = em[i__ - 1];
	eh0 = eh[i__ - 1];
	if (ev[i__ - 1] < ev[i__ - 2]) {
	    ev0 = 0.;
	}
	if (ex[i__ - 1] < ex[i__ - 2]) {
	    ex0 = 0.;
	}
	if (em[i__ - 1] < em[i__ - 2]) {
	    em0 = 0.;
	}
/*         if ( Eh(i) .lt. Eh(i-1) ) eh0 = 0.0 */
	if (ev[i__ - 1] < ev[i__ - 2] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___499);
	    e_wsle();
	    s_wsle(&io___500);
	    e_wsle();
	}
	s_wsfe(&io___501);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ex0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&em0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eh0, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___502);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ex0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&em0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eh0, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

/* -- bjj is classicle rotational ANGular momentum */
/* -- bvj is rotational ANGular velocity */

    s_wsfe(&io___503);
    do_fio(&c__1, (char *)&rinert, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&brigid, (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = nj[1];
    for (j = 1; j <= i__1; ++j) {
	bj = j * 1. - 1.;
	bjj = sqrt(bj * (bj + 1.));
	bvj = bjj / rinert;
	s_wsfe(&io___506);
	i__2 = j - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ej[j - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&er[j - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bjj, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bvj, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___507);
	i__2 = j - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ej[j - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&er[j - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bjj, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bvj, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___508);
	i__2 = j - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ej[j - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    if (nj[1] > 0) {
	s_wsfe(&io___509);
	do_fio(&c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___510);
	do_fio(&c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___511);
	do_fio(&c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(doublereal));
	e_wsfe();
	sumdif = 0.;
	difj0 = evj[0];
	evjmax = 0.;
	jj = 0;
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kv = i__ - 1;
	    nj0 = nj[i__];
	    kk = 0;
	    if (evj[i__ - 1] <= emax) {
		if (evj[i__ - 1] > evjmax) {
		    evjmax = evj[i__ - 1];
		}
	    }
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		kj = j - 1;
		if (j >= 2) {
		    difvj = evj[i__ + j * 200 - 201] - evj[i__ + (j - 1) * 
			    200 - 201];
		}
		if (j == 1) {
		    difvj = difj0;
		}
		if (evj[i__ + j * 200 - 201] < evj[i__]) {
		    sumdif += difvj;
		}
		if (evj[i__ + j * 200 - 201] > evj[i__] && kk == 0) {
		    difj0 = evj[i__] - evj[i__ + (j - 1) * 200 - 201];
		    if (evj[i__ + (j - 1) * 200 - 201] > evjmax) {
			if (evj[i__ + (j - 1) * 200 - 201] <= emax) {
			    evjmax = evj[i__ + (j - 1) * 200 - 201];
			}
		    }
		    kk = 1;
		    s_wsle(&io___517);
		    e_wsle();
		    s_wsle(&io___518);
		    e_wsle();
		    s_wsle(&io___519);
		    e_wsle();
		}
		s_wsfe(&io___520);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		d__1 = evj[i__ + j * 200 - 201] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&difvj, (ftnlen)sizeof(doublereal));
		d__2 = difvj * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___521);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		d__1 = evj[i__ + j * 200 - 201] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&difvj, (ftnlen)sizeof(doublereal));
		d__2 = difvj * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___522);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		e_wsfe();
		if (evj[i__ + j * 200 - 201] == emax) {
		    jj = 1;
		}
		if (evj[i__ + j * 200 - 201] == emax) {
		    sumdif += difvj;
		}
	    }
	    if (jj == 1) {
		s_wsle(&io___523);
		e_wsle();
		s_wsle(&io___524);
		e_wsle();
		s_wsle(&io___525);
		e_wsle();
	    } else {
		s_wsfe(&io___526);
		e_wsfe();
		s_wsfe(&io___527);
		e_wsfe();
		s_wsfe(&io___528);
		e_wsfe();
	    }
	}
	s_wsfe(&io___529);
	do_fio(&c__1, (char *)&sumdif, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&evjmax, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___530);
	do_fio(&c__1, (char *)&sumdif, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&evjmax, (ftnlen)sizeof(doublereal));
	e_wsfe();

	s_wsfe(&io___531);
	e_wsfe();
	s_wsfe(&io___532);
	e_wsfe();
	jj = 0;
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kv = i__ - 1;
	    nj0 = nj[i__];
	    kk = 0;
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		kj = j - 1;
		if (evj[i__ + j * 200 - 201] > evj[i__] && kk == 0) {
		    kk = 1;
		    s_wsle(&io___533);
		    e_wsle();
		    s_wsle(&io___534);
		    e_wsle();
		}
		s_wsfe(&io___535);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		d__1 = aj1[i__ + j * 200 - 201] + aj2[i__ + j * 200 - 201];
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___536);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&ewj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&ej[j - 1], (ftnlen)sizeof(doublereal));
		e_wsfe();
		if (evj[i__ + j * 200 - 201] == emax) {
		    jj = 1;
		}
	    }
	    if (jj == 1) {
		s_wsle(&io___537);
		e_wsle();
		s_wsle(&io___538);
		e_wsle();
	    } else {
		s_wsfe(&io___539);
		e_wsfe();
		s_wsfe(&io___540);
		e_wsfe();
	    }
	}

	s_wsfe(&io___541);
	e_wsfe();
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kv = i__ - 1;
	    nj0 = nj[i__];
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		kj = j - 1;
		s_wsfe(&io___542);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&aj1[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&aj2[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		e_wsfe();
	    }
	    s_wsle(&io___543);
	    e_wsle();
	}

	s_wsfe(&io___544);
	e_wsfe();
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kv = i__ - 1;
	    nj0 = nj[i__];
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		kj = j - 1;
		s_wsfe(&io___545);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&euj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&aj1[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		e_wsfe();
	    }
	    s_wsle(&io___546);
	    e_wsle();
	}

    }

/* ---------------------------------------------------------- */
/* ---------------------------------------------------------- */
/* L800: */
    return 0;
} /* calevj_ */


/* Subroutine */ int vjcoef_(void)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal a, ar;

/* ---------------------------------------------------------- */
/*   Calculate the coefficients used to determine the */
/* vibrational-rotational constants WeXe,..., Alfa_e,... */
/* ---------------------------------------------------------- */
    a = spectra_1.amu * spectra_1.we;
    ar = sqrt(a);
    vjcoef0_1.a0 = 1. / (spectra_1.re * spectra_1.re);
    vjcoef0_1.a1 = -2. / (spectra_1.re * spectra_1.re * spectra_1.re * ar);
    vjcoef0_1.a2 = 3. / (spectra_1.re * spectra_1.re * spectra_1.re * 
	    spectra_1.re * a);
/* Computing 5th power */
    d__1 = spectra_1.re, d__2 = d__1, d__1 *= d__1;
/* Computing 3rd power */
    d__3 = ar;
    vjcoef0_1.a3 = -4. / (d__2 * (d__1 * d__1) * (d__3 * (d__3 * d__3)));
/* Computing 6th power */
    d__1 = spectra_1.re, d__1 *= d__1;
/* Computing 2nd power */
    d__2 = a;
    vjcoef0_1.a4 = 5. / (d__1 * (d__1 * d__1) * (d__2 * d__2));
/* Computing 7th power */
    d__1 = spectra_1.re, d__2 = d__1, d__1 *= d__1, d__2 *= d__1;
/* Computing 5th power */
    d__3 = ar, d__4 = d__3, d__3 *= d__3;
    vjcoef0_1.a5 = -6. / (d__2 * (d__1 * d__1) * (d__4 * (d__3 * d__3)));
/* Computing 8th power */
    d__1 = spectra_1.re, d__1 *= d__1, d__1 *= d__1;
/* Computing 3rd power */
    d__2 = a;
    vjcoef0_1.a6 = 7. / (d__1 * d__1 * (d__2 * (d__2 * d__2)));
/* Computing 9th power */
    d__1 = spectra_1.re, d__2 = d__1, d__1 *= d__1, d__1 *= d__1;
/* Computing 7th power */
    d__3 = ar, d__4 = d__3, d__3 *= d__3, d__4 *= d__3;
    vjcoef0_1.a7 = -8. / (d__2 * (d__1 * d__1) * (d__4 * (d__3 * d__3)));
/* Computing 10th power */
    d__1 = spectra_1.re, d__1 *= d__1, d__2 = d__1, d__1 *= d__1;
/* Computing 4th power */
    d__3 = a, d__3 *= d__3;
    vjcoef0_1.a8 = 9. / (d__2 * (d__1 * d__1) * (d__3 * d__3));

/* Computing 3rd power */
    d__1 = ar;
    vjcoef1_1.b3 = 1. / (d__1 * (d__1 * d__1) * 6.);
/* Computing 2nd power */
    d__1 = a;
    vjcoef1_1.b4 = 1. / (d__1 * d__1 * 24.);
/* Computing 5th power */
    d__1 = ar, d__2 = d__1, d__1 *= d__1;
    vjcoef1_1.b5 = 1. / (d__2 * (d__1 * d__1) * 120.);
/* Computing 3rd power */
    d__1 = a;
    vjcoef1_1.b6 = 1. / (d__1 * (d__1 * d__1) * 720.);
/* Computing 7th power */
    d__1 = ar, d__2 = d__1, d__1 *= d__1, d__2 *= d__1;
    vjcoef1_1.b7 = 1. / (d__2 * (d__1 * d__1) * 5040.);
/* Computing 4th power */
    d__1 = a, d__1 *= d__1;
    vjcoef1_1.b8 = 1. / (d__1 * d__1 * 40320.);
/* ---------------------------------------------------------- */
    return 0;
} /* vjcoef_ */


/* Subroutine */ int convj_(doublereal *gg, integer *nd)
{
    static doublereal f2, f3, f4, f5, f6, f7, f8, a12, a22, a32, a42, a52, 
	    f32, f42, f52, f62, f72, f82, a62, a72, a82, b32, b42, b52, b62, 
	    b72, b82, et3, et4, et5, et6, xs2, xs3, xs4, xs5, xs6, bet;

/* ---------------------------------------------------------- */
    /* Parameter adjustments */
    --gg;

    /* Function Body */
    f2 = gg[2];
    f3 = gg[3];
    f4 = gg[4];
    f5 = gg[5];
    f6 = gg[6];
    f7 = gg[7];
    f8 = gg[8];

    f32 = f3 * f3;
    f42 = f4 * f4;
    f52 = f5 * f5;
    f62 = f6 * f6;
    f72 = f7 * f7;
    f82 = f8 * f8;

    a12 = vjcoef0_1.a1 * vjcoef0_1.a1;
    a22 = vjcoef0_1.a2 * vjcoef0_1.a2;
    a32 = vjcoef0_1.a3 * vjcoef0_1.a3;
    a42 = vjcoef0_1.a4 * vjcoef0_1.a4;
    a52 = vjcoef0_1.a5 * vjcoef0_1.a5;
    a62 = vjcoef0_1.a6 * vjcoef0_1.a6;
    a72 = vjcoef0_1.a7 * vjcoef0_1.a7;
    a82 = vjcoef0_1.a8 * vjcoef0_1.a8;

    b32 = vjcoef1_1.b3 * vjcoef1_1.b3;
    b42 = vjcoef1_1.b4 * vjcoef1_1.b4;
    b52 = vjcoef1_1.b5 * vjcoef1_1.b5;
    b62 = vjcoef1_1.b6 * vjcoef1_1.b6;
    b72 = vjcoef1_1.b7 * vjcoef1_1.b7;
    b82 = vjcoef1_1.b8 * vjcoef1_1.b8;

    spectr0_1.w0 = vjcoef1_1.b4 * 3. * f4 / 8. + vjcoef1_1.b8 * 315. * f8 / 
	    128. - b32 * 7. * f32 / (spectra_1.we * 16.);
    spectr0_1.w0 = spectr0_1.w0 - b52 * 1107. * f52 / (spectra_1.we * 256.) - 
	    vjcoef1_1.b4 * 945. * vjcoef1_1.b6 * f4 * f6 / (spectra_1.we * 
	    128.);
    spectr0_1.w0 -= vjcoef1_1.b3 * 1155. * vjcoef1_1.b7 * f3 * f7 / (
	    spectra_1.we * 128.);
    spectr0_1.w0 -= b72 * 180675. * f72 / (spectra_1.we * 2048.);
    spectr0_1.w0 -= vjcoef1_1.b6 * 89775. * vjcoef1_1.b8 * f6 * f8 / (
	    spectra_1.we * 512.);

    spectr0_1.we0 = vjcoef1_1.b6 * 25. * f6 / 8. - b42 * 67. * f42 / (
	    spectra_1.we * 16.);
    spectr0_1.we0 = spectr0_1.we0 - vjcoef1_1.b3 * 95. * vjcoef1_1.b5 * f3 * 
	    f5 / (spectra_1.we * 8.) - b62 * 19277. * f62 / (spectra_1.we * 
	    256.);
    spectr0_1.we0 -= vjcoef1_1.b5 * 22029. * vjcoef1_1.b7 * f5 * f7 / (
	    spectra_1.we * 128.);
    spectr0_1.we0 -= vjcoef1_1.b4 * 10521. * vjcoef1_1.b8 * f4 * f8 / (
	    spectra_1.we * 64.);
    spectr0_1.we0 -= b82 * 5450499. * f82 / (spectra_1.we * 2048.);

    spectr0_1.wex = vjcoef1_1.b4 * 3. * f4 / 2. + vjcoef1_1.b8 * 245. * f8 / 
	    16. - b32 * 15. * f32 / (spectra_1.we * 4.);
    spectr0_1.wex = spectr0_1.wex - b52 * 1085. * f52 / (spectra_1.we * 32.) 
	    - vjcoef1_1.b4 * 885. * vjcoef1_1.b6 * f4 * f6 / (spectra_1.we * 
	    16.);
    spectr0_1.wex -= vjcoef1_1.b3 * 1365. * vjcoef1_1.b7 * f3 * f7 / (
	    spectra_1.we * 16.);
    spectr0_1.wex -= b72 * 444381. * f72 / (spectra_1.we * 512.);
    spectr0_1.wex -= vjcoef1_1.b6 * 204771. * vjcoef1_1.b8 * f6 * f8 / (
	    spectra_1.we * 128.);
    spectr0_1.wex = -spectr0_1.wex;

    spectr0_1.wey = vjcoef1_1.b6 * 5. * f6 / 2. - b42 * 17. * f42 / (
	    spectra_1.we * 4.);
    spectr0_1.wey = spectr0_1.wey - vjcoef1_1.b3 * 35. * vjcoef1_1.b5 * f3 * 
	    f5 / (spectra_1.we * 2.) - b62 * 4145. * f62 / (spectra_1.we * 
	    32.);
    spectr0_1.wey -= vjcoef1_1.b5 * 5355. * vjcoef1_1.b7 * f5 * f7 / (
	    spectra_1.we * 16.);
    spectr0_1.wey -= vjcoef1_1.b4 * 2205. * vjcoef1_1.b8 * f4 * f8 / (
	    spectra_1.we * 8.);
    spectr0_1.wey -= b82 * 2947595. * f82 / (spectra_1.we * 512.);

    spectr0_1.wez = vjcoef1_1.b8 * 35. * f8 / 8. - b52 * 315. * f52 / (
	    spectra_1.we * 16.);
    spectr0_1.wez = spectr0_1.wez - vjcoef1_1.b4 * 165. * vjcoef1_1.b6 * f4 * 
	    f6 / (spectra_1.we * 8.) - vjcoef1_1.b3 * 315. * vjcoef1_1.b7 * 
	    f3 * f7 / (spectra_1.we * 8.);
    spectr0_1.wez = spectr0_1.wez - b72 * 82005. * f72 / (spectra_1.we * 128.)
	     - vjcoef1_1.b6 * 33845. * vjcoef1_1.b8 * f6 * f8 / (spectra_1.we 
	    * 32.);

    spectr0_1.wet = b62 * -393. * f62 / (spectra_1.we * 16.) - vjcoef1_1.b5 * 
	    693. * vjcoef1_1.b7 * f5 * f7 / (spectra_1.we * 8.);
    spectr0_1.wet = spectr0_1.wet - vjcoef1_1.b4 * 189. * vjcoef1_1.b8 * f4 * 
	    f8 / (spectra_1.we * 4.) - b82 * 239841. * f82 / (spectra_1.we * 
	    128.);

    spectr0_1.wes = b72 * -3003. * f72 / (spectra_1.we * 32.) - vjcoef1_1.b6 *
	     889. * vjcoef1_1.b8 * f6 * f8 / (spectra_1.we * 8.);

    spectr0_1.wer = b82 * -3985. * f82 / (spectra_1.we * 32.);
/* --- */
    spectr1_1.bee = vjcoef0_1.a0 + vjcoef0_1.a4 * 3. / 8. + vjcoef0_1.a8 * 
	    315. / 128. - vjcoef0_1.a3 * 7. * vjcoef1_1.b3 * f3 / (
	    spectra_1.we * 8.);
    spectr1_1.bee = spectr1_1.bee - vjcoef0_1.a7 * 1155. * vjcoef1_1.b3 * f3 /
	     (spectra_1.we * 128.) - vjcoef0_1.a2 * 3. * vjcoef1_1.b4 * f4 / (
	    spectra_1.we * 4.);
    spectr1_1.bee = spectr1_1.bee - vjcoef0_1.a6 * 945. * vjcoef1_1.b4 * f4 / 
	    (spectra_1.we * 128.) - vjcoef0_1.a1 * 15. * vjcoef1_1.b5 * f5 / (
	    spectra_1.we * 8.);
    spectr1_1.bee = spectr1_1.bee - vjcoef0_1.a5 * 1107. * vjcoef1_1.b5 * f5 /
	     (spectra_1.we * 128.) - vjcoef0_1.a4 * 945. * vjcoef1_1.b6 * f6 /
	     (spectra_1.we * 128.);
    spectr1_1.bee = spectr1_1.bee - vjcoef0_1.a8 * 89775. * vjcoef1_1.b6 * f6 
	    / (spectra_1.we * 512.) - vjcoef0_1.a3 * 1155. * vjcoef1_1.b7 * 
	    f7 / (spectra_1.we * 128.);
    spectr1_1.bee = spectr1_1.bee - vjcoef0_1.a7 * 180675. * vjcoef1_1.b7 * 
	    f7 / (spectra_1.we * 1024.) - vjcoef0_1.a2 * 315. * vjcoef1_1.b8 *
	     f8 / (spectra_1.we * 32.);
    spectr1_1.bee -= vjcoef0_1.a6 * 89775. * vjcoef1_1.b8 * f8 / (
	    spectra_1.we * 512.);
    spectr1_1.bee /= spectra_1.amu * 2.;

    spectr1_1.ale = vjcoef0_1.a2 + vjcoef0_1.a6 * 25. / 8. - vjcoef0_1.a1 * 
	    3. * vjcoef1_1.b3 * f3 / spectra_1.we - vjcoef0_1.a5 * 95. * 
	    vjcoef1_1.b3 * f3 / (spectra_1.we * 8.);
    spectr1_1.ale = spectr1_1.ale - vjcoef0_1.a4 * 67. * vjcoef1_1.b4 * f4 / (
	    spectra_1.we * 8.) - vjcoef0_1.a8 * 10521. * vjcoef1_1.b4 * f4 / (
	    spectra_1.we * 64.);
    spectr1_1.ale = spectr1_1.ale - vjcoef0_1.a3 * 95. * vjcoef1_1.b5 * f5 / (
	    spectra_1.we * 8.) - vjcoef0_1.a7 * 22029. * vjcoef1_1.b5 * f5 / (
	    spectra_1.we * 128.);
    spectr1_1.ale = spectr1_1.ale - vjcoef0_1.a2 * 75. * vjcoef1_1.b6 * f6 / (
	    spectra_1.we * 8.) - vjcoef0_1.a6 * 19277. * vjcoef1_1.b6 * f6 / (
	    spectra_1.we * 128.);
    spectr1_1.ale = spectr1_1.ale - vjcoef0_1.a1 * 175. * vjcoef1_1.b7 * f7 / 
	    (spectra_1.we * 8.) - vjcoef0_1.a5 * 22029. * vjcoef1_1.b7 * f7 / 
	    (spectra_1.we * 128.);
    spectr1_1.ale -= vjcoef0_1.a4 * 10521. * vjcoef1_1.b8 * f8 / (
	    spectra_1.we * 64.);
    spectr1_1.ale -= vjcoef0_1.a8 * 5450499. * vjcoef1_1.b8 * f8 / (
	    spectra_1.we * 1024.);
    spectr1_1.ale = -spectr1_1.ale / (spectra_1.amu * 2.);

    spectr1_1.gae = vjcoef0_1.a4 * 3. / 2. + vjcoef0_1.a8 * 245. / 16. - 
	    vjcoef0_1.a3 * 15. * vjcoef1_1.b3 * f3 / (spectra_1.we * 2.);
    spectr1_1.gae = spectr1_1.gae - vjcoef0_1.a7 * 1365. * vjcoef1_1.b3 * f3 /
	     (spectra_1.we * 16.) - vjcoef0_1.a2 * 3. * vjcoef1_1.b4 * f4 / 
	    spectra_1.we;
    spectr1_1.gae = spectr1_1.gae - vjcoef0_1.a6 * 885. * vjcoef1_1.b4 * f4 / 
	    (spectra_1.we * 16.) - vjcoef0_1.a1 * 15. * vjcoef1_1.b5 * f5 / (
	    spectra_1.we * 2.);
    spectr1_1.gae = spectr1_1.gae - vjcoef0_1.a5 * 1085. * vjcoef1_1.b5 * f5 /
	     (spectra_1.we * 16.) - vjcoef0_1.a4 * 885. * vjcoef1_1.b6 * f6 / 
	    (spectra_1.we * 16.);
    spectr1_1.gae -= vjcoef0_1.a8 * 204771. * vjcoef1_1.b6 * f6 / (
	    spectra_1.we * 128.);
    spectr1_1.gae = spectr1_1.gae - vjcoef0_1.a3 * 1365. * vjcoef1_1.b7 * f7 /
	     (spectra_1.we * 16.) - vjcoef0_1.a7 * 444381. * vjcoef1_1.b7 * 
	    f7 / (spectra_1.we * 256.);
    spectr1_1.gae = spectr1_1.gae - vjcoef0_1.a2 * 245. * vjcoef1_1.b8 * f8 / 
	    (spectra_1.we * 4.) - vjcoef0_1.a6 * 204771. * vjcoef1_1.b8 * f8 /
	     (spectra_1.we * 128.);
    spectr1_1.gae /= spectra_1.amu * 2.;

/* --- Bee, ale, gae are calculated by DEFINITION ! */

    et3 = vjcoef0_1.a6 * 5. / 2. - vjcoef0_1.a5 * 35. * vjcoef1_1.b3 * f3 / (
	    spectra_1.we * 2.) - vjcoef0_1.a4 * 17. * vjcoef1_1.b4 * f4 / (
	    spectra_1.we * 2.);
    et3 = et3 - vjcoef0_1.a8 * 2205. * vjcoef1_1.b4 * f4 / (spectra_1.we * 8.)
	     - vjcoef0_1.a3 * 35. * vjcoef1_1.b5 * f5 / (spectra_1.we * 2.);
    et3 = et3 - vjcoef0_1.a7 * 5355. * vjcoef1_1.b5 * f5 / (spectra_1.we * 
	    16.) - vjcoef0_1.a2 * 15. * vjcoef1_1.b6 * f6 / (spectra_1.we * 
	    2.);
    et3 = et3 - vjcoef0_1.a6 * 4145. * vjcoef1_1.b6 * f6 / (spectra_1.we * 
	    16.) - vjcoef0_1.a1 * 35. * vjcoef1_1.b7 * f7 / (spectra_1.we * 
	    2.);
    et3 = et3 - vjcoef0_1.a5 * 5355. * vjcoef1_1.b7 * f7 / (spectra_1.we * 
	    16.) - vjcoef0_1.a4 * 2205. * vjcoef1_1.b8 * f8 / (spectra_1.we * 
	    8.);
    et3 -= vjcoef0_1.a8 * 2947595. * vjcoef1_1.b8 * f8 / (spectra_1.we * 256.)
	    ;
    spectr1_1.eta3 = et3 / (spectra_1.amu * 2.);

    et4 = vjcoef0_1.a8 * 35. / 8. - vjcoef0_1.a7 * 315. * vjcoef1_1.b3 * f3 / 
	    (spectra_1.we * 8.) - vjcoef0_1.a6 * 165. * vjcoef1_1.b4 * f4 / (
	    spectra_1.we * 8.);
    et4 = et4 - vjcoef0_1.a5 * 315. * vjcoef1_1.b5 * f5 / (spectra_1.we * 8.) 
	    - vjcoef0_1.a4 * 165. * vjcoef1_1.b6 * f6 / (spectra_1.we * 8.);
    et4 = et4 - vjcoef0_1.a8 * 33845. * vjcoef1_1.b6 * f6 / (spectra_1.we * 
	    32.) - vjcoef0_1.a3 * 315. * vjcoef1_1.b7 * f7 / (spectra_1.we * 
	    8.);
    et4 = et4 - vjcoef0_1.a7 * 82005. * vjcoef1_1.b7 * f7 / (spectra_1.we * 
	    64.) - vjcoef0_1.a2 * 35. * vjcoef1_1.b8 * f8 / (spectra_1.we * 
	    2.);
    et4 -= vjcoef0_1.a6 * 33845. * vjcoef1_1.b8 * f8 / (spectra_1.we * 32.);
    spectr1_1.eta4 = et4 / (spectra_1.amu * 2.);

    et5 = vjcoef0_1.a8 * -189. * vjcoef1_1.b4 * f4 / (spectra_1.we * 4.) - 
	    vjcoef0_1.a7 * 693. * vjcoef1_1.b5 * f5 / (spectra_1.we * 8.);
    et5 = et5 - vjcoef0_1.a6 * 393. * vjcoef1_1.b6 * f6 / (spectra_1.we * 8.) 
	    - vjcoef0_1.a5 * 693. * vjcoef1_1.b7 * f7 / (spectra_1.we * 8.);
    et5 = et5 - vjcoef0_1.a4 * 189. * vjcoef1_1.b8 * f8 / (spectra_1.we * 4.) 
	    - vjcoef0_1.a8 * 239841. * vjcoef1_1.b8 * f8 / (spectra_1.we * 
	    64.);
    spectr1_1.eta5 = et5 / (spectra_1.amu * 2.);

    et6 = vjcoef0_1.a8 * -889. * vjcoef1_1.b6 * f6 / (spectra_1.we * 8.) - 
	    vjcoef0_1.a7 * 3003. * vjcoef1_1.b7 * f7 / (spectra_1.we * 16.);
    et6 -= vjcoef0_1.a6 * 889. * vjcoef1_1.b8 * f8 / (spectra_1.we * 8.);
    spectr1_1.eta6 = et6 / (spectra_1.amu * 2.);

    spectr1_1.eta7 = vjcoef0_1.a8 * -3985. * vjcoef1_1.b8 * f8 / (
	    spectra_1.amu * 32. * spectra_1.we);
/* --- */
    spectr2_1.dee = a12 / (spectra_1.we * 2.) + a32 * 7. / (spectra_1.we * 
	    16.) + vjcoef0_1.a2 * 3. * vjcoef0_1.a4 / (spectra_1.we * 4.);
    spectr2_1.dee = spectr2_1.dee + vjcoef0_1.a1 * 15. * vjcoef0_1.a5 / (
	    spectra_1.we * 8.) + a52 * 1107. / (spectra_1.we * 256.);
    spectr2_1.dee = spectr2_1.dee + vjcoef0_1.a4 * 945. * vjcoef0_1.a6 / (
	    spectra_1.we * 128.) + vjcoef0_1.a3 * 1155. * vjcoef0_1.a7 / (
	    spectra_1.we * 128.);
    spectr2_1.dee = spectr2_1.dee + a72 * 180675. / (spectra_1.we * 2048.) + 
	    vjcoef0_1.a2 * 315. * vjcoef0_1.a8 / (spectra_1.we * 32.);
    spectr2_1.dee += vjcoef0_1.a6 * 89775. * vjcoef0_1.a8 / (spectra_1.we * 
	    512.);
    spectr2_1.dee /= spectra_1.amu * 4. * spectra_1.amu;

    bet = a22 / (spectra_1.we * 2.) + vjcoef0_1.a1 * 3. * vjcoef0_1.a3 / 
	    spectra_1.we + a42 * 67. / (spectra_1.we * 16.);
    bet = bet + vjcoef0_1.a3 * 95. * vjcoef0_1.a5 / (spectra_1.we * 8.) + 
	    vjcoef0_1.a2 * 75. * vjcoef0_1.a6 / (spectra_1.we * 8.);
    bet = bet + a62 * 19277. / (spectra_1.we * 256.) + vjcoef0_1.a1 * 175. * 
	    vjcoef0_1.a7 / (spectra_1.we * 8.);
    bet = bet + vjcoef0_1.a5 * 22029. * vjcoef0_1.a7 / (spectra_1.we * 128.) 
	    + vjcoef0_1.a4 * 10521. * vjcoef0_1.a8 / (spectra_1.we * 64.);
    bet += a82 * 5450499. / (spectra_1.we * 2048.);
    spectr2_1.bete = bet / (spectra_1.amu * 4. * spectra_1.amu);

    xs2 = a32 * 15. / (spectra_1.we * 4.) + vjcoef0_1.a2 * 3. * vjcoef0_1.a4 /
	     spectra_1.we + vjcoef0_1.a1 * 15. * vjcoef0_1.a5 / (spectra_1.we 
	    * 2.);
    xs2 = xs2 + a52 * 1085. / (spectra_1.we * 32.) + vjcoef0_1.a4 * 885. * 
	    vjcoef0_1.a6 / (spectra_1.we * 16.);
    xs2 = xs2 + vjcoef0_1.a3 * 1365. * vjcoef0_1.a7 / (spectra_1.we * 16.) + 
	    a72 * 444381. / (spectra_1.we * 512.);
    xs2 = xs2 + vjcoef0_1.a2 * 245. * vjcoef0_1.a8 / (spectra_1.we * 4.) + 
	    vjcoef0_1.a6 * 204771. * vjcoef0_1.a8 / (spectra_1.we * 128.);
    spectr2_1.xsi2 = xs2 / (spectra_1.amu * 4. * spectra_1.amu);

    xs3 = a42 * 17. / (spectra_1.we * 4.) + vjcoef0_1.a3 * 35. * vjcoef0_1.a5 
	    / (spectra_1.we * 2.) + vjcoef0_1.a2 * 15. * vjcoef0_1.a6 / (
	    spectra_1.we * 2.);
    xs3 = xs3 + a62 * 4145. / (spectra_1.we * 32.) + vjcoef0_1.a1 * 35. * 
	    vjcoef0_1.a7 / (spectra_1.we * 2.);
    xs3 = xs3 + vjcoef0_1.a5 * 5355. * vjcoef0_1.a7 / (spectra_1.we * 16.) + 
	    vjcoef0_1.a4 * 2205. * vjcoef0_1.a8 / (spectra_1.we * 8.);
    xs3 += a82 * 2947595. / (spectra_1.we * 512.);
    spectr2_1.xsi3 = xs3 / (spectra_1.amu * 4. * spectra_1.amu);

    xs4 = a52 * 315. / (spectra_1.we * 16.) + vjcoef0_1.a4 * 165. * 
	    vjcoef0_1.a6 / (spectra_1.we * 8.);
    xs4 = xs4 + vjcoef0_1.a3 * 315. * vjcoef0_1.a7 / (spectra_1.we * 8.) + 
	    a72 * 82005. / (spectra_1.we * 128.);
    xs4 = xs4 + vjcoef0_1.a2 * 35. * vjcoef0_1.a8 / (spectra_1.we * 2.) + 
	    vjcoef0_1.a6 * 33845. * vjcoef0_1.a8 / (spectra_1.we * 32.);
    spectr2_1.xsi4 = xs4 / (spectra_1.amu * 4. * spectra_1.amu);

    xs5 = a62 * 393. / (spectra_1.we * 16.) + vjcoef0_1.a5 * 693. * 
	    vjcoef0_1.a7 / (spectra_1.we * 8.);
    xs5 = xs5 + vjcoef0_1.a4 * 189. * vjcoef0_1.a8 / (spectra_1.we * 4.) + 
	    a82 * 239841. / (spectra_1.we * 128.);
    spectr2_1.xsi5 = xs5 / (spectra_1.amu * 4. * spectra_1.amu);

    xs6 = a72 * 3003. / (spectra_1.we * 32.) + vjcoef0_1.a6 * 889. * 
	    vjcoef0_1.a8 / (spectra_1.we * 8.);
    spectr2_1.xsi6 = xs6 / (spectra_1.amu * 4. * spectra_1.amu);

    spectr2_1.xsi7 = a82 * 3985. / (spectra_1.amu * 128. * spectra_1.amu * 
	    spectra_1.we);

/* ---------------------------------------------------------- */
    return 0;
} /* convj_ */

