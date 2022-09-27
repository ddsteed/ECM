/* ../src/secentv.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal vc[10], xl[10];
    integer nv[10];
    doublereal add;
    integer nvp, iel, ktyp, ms;
} pdata_;

#define pdata_1 pdata_

struct {
    integer kfre, nci, ncf;
} fdata_;

#define fdata_1 fdata_

struct {
    doublereal rmin, rmax, rmatch, h__, ul0[240], ur0[240];
} rdata_;

#define rdata_1 rdata_

struct {
    doublereal evib[240], evib1[240], eori[240], expr[240];
} vibe_;

#define vibe_1 vibe_

struct {
    doublereal qeng[240], vtol[240], dev[240];
    integer kcon[240];
} vcon_;

#define vcon_1 vcon_

struct {
    integer kstd[240];
} kmon_;

#define kmon_1 kmon_

struct {
    doublereal de, alpha, re, rmu, we;
} shomos_;

#define shomos_1 shomos_

struct {
    doublereal de0, a1, a2, a3, a4, a5, a6, a7, shift, beta;
} murrell_;

#define murrell_1 murrell_

struct {
    doublereal alamta;
    integer isf;
    doublereal amu;
    integer nturn;
    doublereal an[20];
} sf_;

#define sf_1 sf_

struct {
    doublereal rv[20000], vnum[20000];
} vmon_;

#define vmon_1 vmon_

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__9 = 9;
static integer c__5 = 5;
static integer c__0 = 0;
static doublereal c_b380 = -1.;

/*     program  secen */
/* Main program */ int MAIN__(void)
{
    /* Format strings */
    static char fmt_320[] = "(//19x,\002Information returned from SOLVESE "
	    ":\002,/,/9x\002[      v -- the (v+1) state with quantum number v"
	    ";     \002,/9x\002  wronsk -- an wronskian of the  left and righ"
	    "t waves; \002,/9x\002   Ecorr -- estimated correction to the inp"
	    "ut energy E  \002,/9x\002      kv -- number of iterations used f"
	    "or this v       ]\002,//11x,\002   (   wronsk = 0  means the der"
	    "ivatives of LEFT     \002,/11x,\002     & RIGHT wavefunctions ar"
	    "e equal and therefore   \002,/11x,\002     the whole EIGENfuncti"
	    "on is SMOOTH.              \002,/11x,\002        When wronsk > 1"
	    ".0, CHECK wavefunctions !   )\002,/,/11x,\002   { abs(kstd) : ab"
	    "s(0) == abs(Ecorr/E)         \002,/11x,\002                 abs("
	    "1) == abs(qeng*Ecorr/E)    \002,/11x,\002                 abs(2)"
	    " == abs(E - E0 )       } \002,//2x\002v\002,4x\002wronsk\002,7x"
	    ",\002Ecorr\002,10x,\002E\002,11x,\002E+Ecorr\002,4x,\002abs(kstd"
	    ") kstd kv \002,/)";
    static char fmt_350[] = "(/14x,\002{ abs(kstd) : abs(0) == abs(Ecorr/E) "
	    "   \002,/18x,\002          abs(1) == abs(qeng*Ecorr/E)    \002,/"
	    "18x,\002          abs(2) == abs(E - E0 )       } \002,//3x,\002"
	    "v\002,9x,\002E\002,13x,\002Ecorr\002,8x,\002 abs(kstd)\002,4x"
	    ",\002kstd\002,6x,\002vtol\002,)";
    static char fmt_435[] = "(/4x,23(\002*\002),\002 NOTATION \002,23(\002"
	    "*\002),//8x,\002Since Linux system admits 99 units for output ON"
	    "LY,\002,/6x,\002and we keep units 1--29 for other purpose,     a"
	    "ll\002,/6x,\002states with quantum number v_initial >= 60 will b"
	    "e \002,/6x,\002written (starting) from unit 30 & up.       So yo"
	    "u \002,/6x,\002might perform your calculations in TWO stages :  "
	    "  \002,/11x,\002  First,   run from v =  0 --> v = 59 ;   \002,/"
	    "11x,\002  Second,  run from v = 60 --> v =120 .\002,//4x,56(\002*"
	    "\002),/)";
    static char fmt_420[] = "(/x,\002For v = \002,i3,2x,\002 &  eigenvalue E"
	    " =\002,1pe18.9)";
    static char fmt_422[] = "(x,\002After adding constant Add,  E =\002,1pe1"
	    "8.9,/)";
    static char fmt_430[] = "(6x,f10.5,3x,1pe16.8)";
    static char fmt_440[] = "(3x,1pe16.8)";
    static char fmt_650[] = "(//9x,\002The energies & expectation values are"
	    " :\002,/,/5x,\002v\002,6x,\002  r  \002,7x,\002E_v(a.u.)\002,6x"
	    ",\002< v | r | v > \002,/)";
    static char fmt_660[] = "(3x,i3,3x,f9.4,2x,2(1pe15.7,2x,))";
    static char fmt_680[] = "(//11x,\002( Edif% = | Emos_v - Ecal_v |/Emos_v"
	    " )\002,/12x,\002The Morse & the calc. energies are :\002,/,/5x"
	    ",\002v\002,5x,\002\"Morse_v\"\002,6x,\002Ecal_v(a.u.)\002,6x,"
	    "\002Em - Ec\002,10x,\002Edif% \002,/)";
    static char fmt_670[] = "(//11x,\002( Edif% = | Expt_v - Ecal_v |/Expt_v"
	    " )\002,/12x,\002The \"expt\" & the calc. energies are :\002,/,/5"
	    "x,\002v\002,5x,\002 \"Expt_v\"\002,6x,\002Ecal_v(a.u.)\002,6x"
	    ",\002Ex - Ec\002,10x,\002Edif% \002,/)";
    static char fmt_710[] = "(3x,i3,x,2(f15.11,x),2(1pe15.7,x,))";
    static char fmt_720[] = "(//2x,\002The \"expt\" & the calc. energies are"
	    "  (cm-1) :\002,/,/5x,\002v\002,5x,\002 \"Expt_v\"\002,6x,\002Eca"
	    "l_v(cm-1)\002,/)";
    static char fmt_722[] = "(//2x,\002The MORSE & the calc. energies are  ("
	    "cm-1) :\002,/,/5x,\002v\002,5x,\002 \"Morse_v\"\002,5x,\002Ecal_"
	    "v(cm-1)\002,/)";
    static char fmt_715[] = "(3x,i3,x,2(f15.6,x),2(1pe15.7,x,))";
    static char fmt_820[] = "(//2x,\002The \"expt\" & the calc. energies are"
	    "  (cm-1) :\002,/,/2x,\002After adding constant Add,  Add = \002,"
	    "f16.5,//5x,\002v\002,5x,\002 \"Expt_v\"\002,6x,\002Ecal_v(cm-1"
	    ")\002,/)";
    static char fmt_725[] = "(//4x,\002The calculated energies are  (cm-1) "
	    ":\002,/,/5x,\002      { edif = E(v) - E(v-1) } \002,/5x,\002v"
	    "\002,5x,\002Ecal_v(cm-1)\002,4x,\002    edif     \002,2x,\002 De"
	    "lta_edif \002,/)";
    static char fmt_718[] = "(3x,i3,x,2(f15.6,x),2x,f12.6)";
    static char fmt_830[] = "(//3x,\002After adding constant Add,  Add = "
	    "\002,f16.5,//5x,\002v\002,5x,\002Ecal_v(cm-1)\002,4x,\002    edi"
	    "f     \002,2x,\002 Delta_edif \002,/)";
    static char fmt_730[] = "(/3x,\002 Free wave  cos(k*r) with  k = sqrt(2E"
	    ") = \002,1pe15.7,/41x,\002E = \002,1pe15.7,//3x,\002  r(ao)     "
	    "     cos(k*r) \002,/)";
    static char fmt_740[] = "(/3x,\002 Free wave  sin(k*r) with  k = sqrt(2E"
	    ") = \002,1pe15.7,/41x,\002E = \002,1pe15.7,//3x,\002  r(ao)     "
	    "     sin(k*r) \002,/)";
    static char fmt_750[] = "(2(1pe15.7,x))";
    static char fmt_760[] = "(//12x,\002Calculated sprctroscopic constants f"
	    "rom known\002,/13x,\002        vibrational energies  E_v's : "
	    "\002,/13x,\002 ( we & wexe > 0; weye & weze MAY be < 0 ) \002,//"
	    "3x,\002  v\002,7x,\002we\002,12x,\002wexe\002,11x,\002weye\002,1"
	    "1x,\002weze\002,/)";
    static char fmt_765[] = "(3x,i3,4(1pe15.7))";
    static char fmt_770[] = "(///7x,\002Sprctroscopic constants AVERAGED ove"
	    "r \002,i3,\002  states : \002,//7x,\002 we  = \002,1pe15.7,\002;"
	    "     wexe = \002,1pe15.7,\002  a.u.\002,/7x,\002weye = \002,1pe1"
	    "5.7,\002;     weze = \002,1pe15.7,\002  a.u.\002,/)";
    static char fmt_790[] = "(7x,\002 we  = \002,1pe15.7,\002;     wexe ="
	    " \002,1pe15.7,\002  cm-1 \002,/7x,\002weye = \002,1pe15.7,\002; "
	    "    weze = \002,1pe15.7,\002  cm-1\002,/)";
    static char fmt_780[] = "(/7x,\002Sprctroscopic constants AVERAGED from"
	    " \002,\002state v = \002,i3,/42x,\002to state v'= \002,i3,\002  :"
	    "\002,//7x,\002 we  = \002,1pe15.7,\002;     wexe = \002,1pe15.7"
	    ",\002  a.u.\002,/7x,\002weye = \002,1pe15.7,\002;     weze = "
	    "\002,1pe15.7,\002  a.u.\002,/)";
    static char fmt_990[] = "(/22x,\002*****  You are done  *****\002,/)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void), s_wsle(cilist *), do_lio(integer *
	    , integer *, char *, ftnlen), e_wsle(void), do_fio(integer *, 
	    char *, ftnlen);
    double sqrt(doublereal), cos(doublereal), sin(doublereal);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int expectwf_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    static integer i__, j;
    static doublereal u[20000], v, z__;
    static integer k1;
    static doublereal ek;
    static integer ng;
    static doublereal ri, we, wf[120], rk;
    static integer nr, ku, kw, nu;
    static doublereal ev1, ev2, ev4, ev5;
    static integer nw1, nw2;
    static doublereal acm;
    static integer nbi;
    static doublereal awe, bwe, dve, evf, dvi, awx, awy, awz, bwx, bwy, bwz;
    static integer nwv;
    static doublereal edif, eigf[2400000]	/* was [20000][120] */, aucm, 
	    eigv, wexe, weye, weze, edif1, edif0, temp1, temp2;
    static integer nbmax;
    static doublereal rmesh[120];
    extern /* Subroutine */ int readin_(doublereal *, integer *, integer *, 
	    integer *, integer *, integer *), boundv_(integer *, integer *, 
	    integer *, doublereal *, doublereal *);

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 6, 0, fmt_320, 0 };
    static cilist io___9 = { 0, 21, 0, fmt_350, 0 };
    static cilist io___14 = { 0, 0, 0, fmt_350, 0 };
    static cilist io___19 = { 0, 6, 0, fmt_435, 0 };
    static cilist io___20 = { 0, 0, 0, fmt_435, 0 };
    static cilist io___22 = { 0, 3, 0, 0, 0 };
    static cilist io___23 = { 0, 7, 0, 0, 0 };
    static cilist io___24 = { 0, 7, 0, fmt_420, 0 };
    static cilist io___25 = { 0, 7, 0, fmt_422, 0 };
    static cilist io___27 = { 0, 3, 0, fmt_430, 0 };
    static cilist io___28 = { 0, 7, 0, fmt_430, 0 };
    static cilist io___29 = { 0, 3, 0, fmt_440, 0 };
    static cilist io___30 = { 0, 6, 0, fmt_650, 0 };
    static cilist io___31 = { 0, 6, 0, fmt_660, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_680, 0 };
    static cilist io___33 = { 0, 6, 0, fmt_670, 0 };
    static cilist io___36 = { 0, 6, 0, fmt_710, 0 };
    static cilist io___37 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___38 = { 0, 6, 0, fmt_722, 0 };
    static cilist io___39 = { 0, 6, 0, fmt_715, 0 };
    static cilist io___40 = { 0, 6, 0, fmt_820, 0 };
    static cilist io___43 = { 0, 6, 0, fmt_715, 0 };
    static cilist io___44 = { 0, 6, 0, fmt_725, 0 };
    static cilist io___45 = { 0, 6, 0, fmt_715, 0 };
    static cilist io___48 = { 0, 6, 0, fmt_718, 0 };
    static cilist io___49 = { 0, 6, 0, fmt_715, 0 };
    static cilist io___50 = { 0, 6, 0, fmt_830, 0 };
    static cilist io___51 = { 0, 6, 0, fmt_715, 0 };
    static cilist io___52 = { 0, 6, 0, fmt_718, 0 };
    static cilist io___53 = { 0, 6, 0, fmt_715, 0 };
    static cilist io___55 = { 0, 98, 0, fmt_730, 0 };
    static cilist io___56 = { 0, 99, 0, fmt_740, 0 };
    static cilist io___59 = { 0, 98, 0, fmt_750, 0 };
    static cilist io___60 = { 0, 99, 0, fmt_750, 0 };
    static cilist io___61 = { 0, 6, 0, 0, 0 };
    static cilist io___73 = { 0, 6, 0, fmt_760, 0 };
    static cilist io___86 = { 0, 6, 0, fmt_765, 0 };
    static cilist io___87 = { 0, 6, 0, fmt_770, 0 };
    static cilist io___88 = { 0, 6, 0, fmt_790, 0 };
    static cilist io___89 = { 0, 6, 0, fmt_780, 0 };
    static cilist io___90 = { 0, 6, 0, fmt_790, 0 };
    static cilist io___91 = { 0, 6, 0, fmt_990, 0 };


/* =================================================================== */
/*   This code uses the Numerov method (subroutine solvese) to solve */
/* the 2nd order differential Schrodinger equation (se) for a CENTRAL */
/* (SPHERICAL) potential (centv) for a given BOUND-STATE */
/* (NOT scattering) energy E_v : */
/*           u''(r) - f(r)u(r) = 0 .     Here it becomes : */
/*  { - d^2/(dr^2) + l*(l+1)/r**2 + 2*mu*[V(r) - E_v] } u(r) = 0 */

/*   The code will give : */
/* 1.)  The potentials V(r) of a specific central potential in unit 10. */
/*                                      numerical potential in unit 20. */
/* 2.)  a set of EIGENfunctions for the given (inputted/calculated) */
/*    energies E_v in unit 7. */

/*  Modified & enlarged  by       Weiguo Sun         11/19/1993 */
/*  Modified to include Sun-Murrell-Sorbie (SMS) potential */
/*           by Weiguo Sun        05/26/1995 */
/*  Modified to converge SMS wavefunctions by Weiguo Sun  11/20/1996 */

/*  Modified to include ECM and rewrite the codes by Hao Feng 11/14/2000 */
/*     Now, it can cope with the following potential: */
/*         ============================ */
/*            mtyp     ktyp    N */
/*                     1       1 */
/*                     2       2 */
/*            3        3       3 */
/*            4        4       4 */
/*                     5       5 */
/*            5        6,7,8   6,7,8 */
/*            9        9       9 */
/*         ============================ */
/*   1.) Gaussion type or of Slater type ; */

/*   2.) Spherical central potential V0*exp(-alpha*R) ; */

/*   3.) Simple harmonic potential   V(x)=0.5d0*ak*x**2 ; */
/*              where  x=R-Re */

/*   4.) Morse potential  V(x)=de[exp(-2*a*x) - 2*exp(-a*x)] + de ; */
/*              where  x=(R-Re)/Re */

/*   5.) Screened central potential  V(x)=-Z*dexp(-x/D)/x */

/*   6.) Sun-Murrell--Sorbie potential  : */
/*              temp = 1/beta + a1*x + a2*x**2 + a3*x**3 + */
/*                     a4*x**4 + a5*x**5 */
/*              V_MS(x) = - De*beta*temp*exp(-a1*beta*x) */
/*              where x = R - Re */

/*   7.) Huxley-Murrell-Sorbie : */
/*              V_MS(x)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x) */
/*              where x = R - Re */

/*   8.) SF (ECM) : */
/*              V_ecm(R) = V_MS(R) + Lamta(R)*delta_V(R) */

/*              V_MS:  Murrell & Sorbie potential in which the */
/*                 (a1,a2,a3, ...) are calculated using SF formulae. */
/*              Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)] */
/*              delta_V(R) = V_MS - V_0 */
/*                For  R < Re : */
/*              Nturn = 0, V_ecm(R) = V_MS   for ms = 3 ONLY ; */
/*                For  ms = 4, 5 : */
/*                = 1, V_ecm(R) = V_0,   V_0 = V_Morse   ; */
/*                = 2, V_ecm(R) = V_0,   V_0 = V_Rydberg ; */
/*                = 3, V_ecm(R) = V_0,   V_0 = V_PG      . */

/*   9.) Numerical potential:    read it from fort.20 */

/* =================================================================== */
/*  nd -- the dimension of r-mesh & wavefunction arrays;  nd >= ng. */
/* ---------------------------------------------------------------- */
/*       parameter(nd=120, nd1=2*nd, na=6000) */
/*       parameter(nd=120, nd1=2*nd, na=10000) */
/* ---------------------------------------------------------------- */
/* ---------------------------------------------------------------- */
    nbmax = 20;
    readin_(rmesh, &nbmax, &ng, &nr, &nbi, &nwv);
/* ---------------------------------------------------------------- */
/*  These rmesh(j) aren't of course gaussian mesh points, but the */
/*  magnitudes are about what we want for our problem. */
/* ---------------------------------------------------------------- */
/*     do j = 1, ng */
/*       rmesh(j) = 1.5d0 + 1.5011274d0 * j/ng */
/*     enddo */
/* ---------------------------------------------------------------- */
    aucm = 219474.63067;
    s_wsfe(&io___8);
    e_wsfe();
    s_wsfe(&io___9);
    e_wsfe();
/* --- */
    k1 = 5;
    ku = 21;
    kw = nbi + 28;
/* ---------------------------------------------------------------- */
/*  Redefine unit switch for writting purpose */
/* ---------------------------------------------------------------- */
    if (nwv >= 61) {
	kw = 29;
    }
    if (nwv >= 61) {
	k1 = nbi + 5;
    }
/* ---------------------------------------------------------------- */
/*  Loop over bound states */
/* ---------------------------------------------------------------- */
    i__1 = nbmax;
    for (nu = nbi; nu <= i__1; ++nu) {

	if (nu > k1 && nu - k1 > 10) {
	    k1 += 10;
	    ++ku;
	    io___14.ciunit = ku;
	    s_wsfe(&io___14);
	    e_wsfe();
	}

/* ---------------------------------------------------------------- */
/*   boundv (call solvese -->) solves Schrodinger Equation & get */
/* the numerical wavefunction u. */
/* ---------------------------------------------------------------- */
	boundv_(&nbi, &ku, &nu, &eigv, u);
/* ---------------------------------------------------------------- */
	i__2 = nr;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    eigf[i__ + nu * 20000 - 20001] = u[i__ - 1];
	}
    }
/* ---------------------------------------------------------------- */
/* Loop bound states again */
/*      vibrational quantum number :   v = nu -1 */
/* ---------------------------------------------------------------- */
    i__1 = nbmax;
    for (nu = nbi; nu <= i__1; ++nu) {

	eigv = vibe_1.evib[nu - 1];
	if (nu == 61) {
	    s_wsfe(&io___19);
	    e_wsfe();
	    s_wsfe(&io___20);
	    e_wsfe();
	}
	++kw;
/* ---------------------------------------------------------------- */
	i__2 = nr;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    u[i__ - 1] = eigf[i__ + nu * 20000 - 20001];
	}
/* ---------------------------------------------------------------- */
/*   Evaluate expectation value of r :  < u | r | u > = expr */
/* and wavefunctions (wf) at rmesh points. */
/* ---------------------------------------------------------------- */
	expectwf_(&kw, &nr, &nu, &ng, &nbi, &nwv, &rdata_1.h__, &rdata_1.rmin,
		 &eigv, u, rmesh, wf, vibe_1.expr);
/* ---------------------------------------------------------------- */
	if (nu == nbi) {
	    s_wsle(&io___22);
	    do_lio(&c__3, &c__1, (char *)&nwv, (ftnlen)sizeof(integer));
	    do_lio(&c__3, &c__1, (char *)&ng, (ftnlen)sizeof(integer));
	    e_wsle();
	}
	if (nu == nbi) {
	    s_wsle(&io___23);
	    do_lio(&c__3, &c__1, (char *)&nwv, (ftnlen)sizeof(integer));
	    do_lio(&c__3, &c__1, (char *)&ng, (ftnlen)sizeof(integer));
	    e_wsle();
	}
	if (nu <= nwv + 1) {
	    s_wsfe(&io___24);
	    i__2 = nu - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&vibe_1.evib[nu - 1], (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	    if (pdata_1.add > 0.) {
		s_wsfe(&io___25);
		d__1 = vibe_1.evib[nu - 1] + pdata_1.add;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    }
	    if (pdata_1.ktyp == 9) {
		goto L100;
	    }
	    i__2 = ng;
	    for (j = 1; j <= i__2; ++j) {
		s_wsfe(&io___27);
		do_fio(&c__1, (char *)&rmesh[j - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&wf[j - 1], (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___28);
		do_fio(&c__1, (char *)&rmesh[j - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&wf[j - 1], (ftnlen)sizeof(doublereal));
		e_wsfe();
	    }
	}
L100:
	;
    }
/* ---------------------------------------------------------------- */
    i__1 = nbmax;
    for (nu = nbi; nu <= i__1; ++nu) {
	if (nu <= nwv + 1) {
	    s_wsfe(&io___29);
	    do_fio(&c__1, (char *)&vibe_1.evib[nu - 1], (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	}
    }
/* ---------------------------------------------------------------- */
/* Print eigenvalues E  &  < u | r | u > */
/* ---------------------------------------------------------------- */
    s_wsfe(&io___30);
    e_wsfe();
    i__1 = nbmax;
    for (i__ = nbi; i__ <= i__1; ++i__) {
	s_wsfe(&io___31);
	i__2 = i__ - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&rdata_1.rmax, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vibe_1.evib[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&vibe_1.expr[i__ - 1], (ftnlen)sizeof(
		doublereal));
	e_wsfe();
    }
/* ---------------------------------------------------------------- */
/* Print calculated eigenvalues E  &  "experiment" E's */
/* ---------------------------------------------------------------- */
    if (pdata_1.ktyp == 9) {
	goto L110;
    }
/* --- */
    if (pdata_1.ktyp == 4) {
	s_wsfe(&io___32);
	e_wsfe();
    } else {
	s_wsfe(&io___33);
	e_wsfe();
    }
/* - */
    i__1 = nbmax;
    for (i__ = nbi; i__ <= i__1; ++i__) {
	dvi = vibe_1.eori[i__ - 1] - vibe_1.evib[i__ - 1];
	edif = abs(dvi) / vibe_1.eori[i__ - 1];
	s_wsfe(&io___36);
	i__2 = i__ - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vibe_1.eori[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&vibe_1.evib[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&dvi, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&edif, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* ---------------------------------------------------------------- */
/* Print eigenvalues &  "experiment" E's in  cm-1 . */
/* ---------------------------------------------------------------- */
    if (pdata_1.ktyp >= 5) {
	s_wsfe(&io___37);
	e_wsfe();
    } else if (pdata_1.ktyp == 4) {
	s_wsfe(&io___38);
	e_wsfe();
    }
    i__1 = nbmax;
    for (i__ = nbi; i__ <= i__1; ++i__) {
	s_wsfe(&io___39);
	i__2 = i__ - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	d__1 = vibe_1.eori[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__2 = vibe_1.evib[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* - */
    if (pdata_1.ktyp == 5 && pdata_1.add > 0.) {
	s_wsfe(&io___40);
	d__1 = pdata_1.add * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = nbmax;
	for (i__ = nbi; i__ <= i__1; ++i__) {
	    temp1 = vibe_1.eori[i__ - 1] + pdata_1.add;
	    temp2 = vibe_1.evib[i__ - 1] + pdata_1.add;
	    s_wsfe(&io___43);
	    i__2 = i__ - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    d__1 = temp1 * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    d__2 = temp2 * aucm;
	    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
/* --- */
L110:
    s_wsfe(&io___44);
    e_wsfe();
    s_wsfe(&io___45);
    i__1 = nbi - 1;
    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
    d__1 = vibe_1.evib[nbi - 1] * aucm;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = nbmax;
    for (i__ = nbi + 1; i__ <= i__1; ++i__) {
	edif = vibe_1.evib[i__ - 1] - vibe_1.evib[i__ - 2];
	if (i__ > 2) {
	    edif1 = edif0 - edif;
	    s_wsfe(&io___48);
	    i__2 = i__ - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    d__1 = vibe_1.evib[i__ - 1] * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    d__2 = edif * aucm;
	    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	    d__3 = edif1 * aucm;
	    do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	} else {
	    s_wsfe(&io___49);
	    i__2 = i__ - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    d__1 = vibe_1.evib[i__ - 1] * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    d__2 = edif * aucm;
	    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	edif0 = edif;
    }
/* - */
    if (pdata_1.ktyp == 5 && pdata_1.add > 0.) {
	s_wsfe(&io___50);
	d__1 = pdata_1.add * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___51);
	i__1 = nbi - 1;
	do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	d__1 = (vibe_1.evib[nbi - 1] + pdata_1.add) * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = nbmax;
	for (i__ = nbi + 1; i__ <= i__1; ++i__) {
	    edif = vibe_1.evib[i__ - 1] - vibe_1.evib[i__ - 2];
	    temp2 = vibe_1.evib[i__ - 1] + pdata_1.add;
	    if (i__ > 2) {
		edif1 = edif0 - edif;
		s_wsfe(&io___52);
		i__2 = i__ - 1;
		do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
		d__1 = temp2 * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = edif * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		d__3 = edif1 * aucm;
		do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    } else {
		s_wsfe(&io___53);
		i__2 = i__ - 1;
		do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
		d__1 = temp2 * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = edif * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    }
	    edif0 = edif;
	}
    }
/* ---------------------------------------------------------------- */
/* Calculate free wave if neccessary */
/* ---------------------------------------------------------------- */
    if (fdata_1.kfre == 1) {
	ek = sqrt(vibe_1.evib[nwv - 1] * 2.);
	s_wsfe(&io___55);
	do_fio(&c__1, (char *)&ek, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vibe_1.evib[nwv - 1], (ftnlen)sizeof(
		doublereal));
	e_wsfe();
	s_wsfe(&io___56);
	do_fio(&c__1, (char *)&ek, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vibe_1.evib[nwv - 1], (ftnlen)sizeof(
		doublereal));
	e_wsfe();
	i__1 = nr;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ri = rdata_1.rmin + (i__ - 1) * rdata_1.h__;
	    rk = ri * ek;
	    s_wsfe(&io___59);
	    do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
	    d__1 = cos(rk);
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___60);
	    do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
	    d__1 = sin(rk);
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	s_wsle(&io___61);
	do_lio(&c__9, &c__1, " Free waves are in fort.98 & fort.99 ", (ftnlen)
		37);
	e_wsle();
    }
/* ---------------------------------------------------------------- */
/* Calculate we, wexe, weye from KNOWN E_v's if neccessary */
/*          v - u = i - (i-1) = 1; */
/*   wexe is evaluated from E(i-1) & E(i); */
/*    we  is evaluated from E(i-1) & wexe; */
/*   weye is evaluated from Delta_E = E(i) - E(i-1); */
/*   weze is evaluated from E(i), we, wexe, & weye. */
/* ---------------------------------------------------------------- */
    if (fdata_1.kfre == 2) {
	acm = 219474.63067;
	awe = 0.;
	awx = 0.;
	awy = 0.;
	awz = 0.;
	bwe = 0.;
	bwx = 0.;
	bwy = 0.;
	bwz = 0.;
	nw1 = nwv - (nbi + 2) + 1;
	nw2 = fdata_1.ncf - fdata_1.nci + 1;
	s_wsfe(&io___73);
	e_wsfe();

	i__1 = nwv;
	for (i__ = nbi + 2; i__ <= i__1; ++i__) {
	    v = (doublereal) (i__ - 1);
	    z__ = v - 1.;
	    dve = vibe_1.evib[i__ - 1] - vibe_1.evib[i__ - 2];
	    evf = (v * 2 + 1) / (z__ * 2 + 1);
/* Computing 2nd power */
	    d__1 = z__ * 2 + 1;
	    ev1 = d__1 * d__1 * .25;
	    ev2 = v + .5;
	    wexe = (evf * vibe_1.evib[i__ - 2] - vibe_1.evib[i__ - 1]) * 2. / 
		    (v * 2 + 1);
	    we = (vibe_1.evib[i__ - 2] + ev1 * wexe) * 2. / (z__ * 2 + 1);
/* -------------------------------------------------------------- */
/*  Next line gives POOR result : */
/*          weye = (evib(i) - we*ev2 + wexe*ev2**2)/(ev2**3) */
/* --- */
/*  Next THREE lines give POOR result : */
/*           ev3 = z + 0.5d0 */
/*           ev4 = 2.0d0*(z + 1.0) */
/*           ev5 = 1.0 + 3.0d0*(z + 0.5d0)*(z + 1.5d0) */
/* --- */
/*  Next THREE lines give POOR result : */
/*           ev6 = 1.0 + 4.0d0*ev3**3 + 6.0d0*ev3**2 */
/*           ev6 = ev6 + 4.0d0*ev3 */
/*          weze = (dve - we + ev4*wexe - ev5*weye)/ev6 */
/* -------------------------------------------------------------- */

/* -- Use energy difference to find weye : */
	    ev4 = (v + 1.) * 2.;
	    ev5 = (v + .5) * 3. * (v + 1.5) + 1.;
	    weye = (dve - we + ev4 * wexe) / ev5;

/* -- Use energy formula to find weze : */
/* Computing 2nd power */
	    d__1 = ev2;
/* Computing 3rd power */
	    d__2 = ev2;
	    weze = vibe_1.evib[i__ - 1] - we * ev2 + wexe * (d__1 * d__1) - 
		    weye * (d__2 * (d__2 * d__2));
/* Computing 4th power */
	    d__1 = ev2, d__1 *= d__1;
	    weze /= d__1 * d__1;

	    awe += we;
	    awx += wexe;
	    awy += weye;
	    awz += weze;
	    s_wsfe(&io___86);
	    i__2 = i__ - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&we, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&weye, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&weze, (ftnlen)sizeof(doublereal));
	    e_wsfe();

	    if (i__ >= fdata_1.nci && i__ <= fdata_1.ncf) {
		bwe += we;
		bwx += wexe;
		bwy += weye;
		bwz += weze;
	    }
	}

	awe /= nw1;
	awx /= nw1;
	awy /= nw1;
	awz /= nw1;

	bwe /= nw2;
	bwx /= nw2;
	bwy /= nw2;
	bwz /= nw2;
	s_wsfe(&io___87);
	do_fio(&c__1, (char *)&nw1, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&awe, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&awx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&awy, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&awz, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___88);
	d__1 = awe * acm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__2 = awx * acm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	d__3 = awy * acm;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	d__4 = awz * acm;
	do_fio(&c__1, (char *)&d__4, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___89);
	i__1 = fdata_1.nci - 1;
	do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	i__2 = fdata_1.ncf - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&bwe, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bwx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bwy, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bwz, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___90);
	d__1 = bwe * acm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__2 = bwx * acm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	d__3 = bwy * acm;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	d__4 = bwz * acm;
	do_fio(&c__1, (char *)&d__4, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* ---------------------------------------------------------------- */
    s_wsfe(&io___91);
    e_wsfe();
/* ---------------------------------------------------------------- */
/*    #/2x'v',4x'wronsk',7x,'Ecorr',10x,'E',11x,'  Q_v  ', */
/* L630: */
/* 710  format(3x,i3,2x,4(1pe15.7,x,)) */
/*    #/5x,'v',5x,'Ecal_v(cm-1)',4x,'E(v) - E(v-1)',/) */
/* ---------------------------------------------------------------- */
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */


/* = */
/* Subroutine */ int readin_(doublereal *rmesh, integer *nbmax, integer *ng, 
	integer *nr, integer *nbi, integer *nwv)
{
    /* Format strings */
    static char fmt_320[] = "(/10x,\002*** Using Sun's energy-consistent met"
	    "hod ***\002,/8x,\002  ( Chengdu Sichuan 610065  P.R.China;  1996"
	    " )\002,//6x,\002ALWAYS CHECK the wavefunction of the HIGH  v sta"
	    "tes;\002,/6x,\002If E is NOT an EIGENvalue, the WAVE has WRONG n"
	    "odes.\002,//6x,\002When the  HIGHLY  excited states are  NOT con"
	    "verged,\002,/6x,\002try to  INCREASE  rmax,     OR change the va"
	    "lues of \002,/6x,\002parameters :         kstd,  kcon,  vtol,  &"
	    "   dev \002,//6x,\002When get 'Floating exception',  CHANGE rmin"
	    " or rmax.\002,/6x,\002   SMALL rmin may cause 'floating exceptio"
	    "n' ; \002,/6x,\002 For LOW  v's, LARGE rmax may cause 'floating "
	    "...'. \002,//2x,\002If E_try > De, one gets 'Floating exception' "
	    "\002,\002too (CONTINUUM)\002,//7x,\002Chose 'ntyp=3' & modify E'"
	    "s in 'Evib' for HIGH v's\002/)";
    static char fmt_350[] = "(///10x,\002*** Solving Schrodinger equation fo"
	    "r ***\002,/,13x,\002   bound SPHERICAL potentials \002,/,/17x"
	    ",\002{ Using Numerov method }\002,/,/3x,\002The minimum radius o"
	    "f the r-mesh,      rmin = \002,f9.4,/3x,\002The maximum radius o"
	    "f the r-mesh,      rmax = \002,f9.4,/3x,\002The step lenth of ra"
	    "dius r,           rstep = \002,f9.4,/3x,\002The matching boundar"
	    "y radius (C.A.), rmatch = \002,f9.4,/3x,\002The angular momentum"
	    " quantum number L,  iel = \002,i4,//3x,\002[ ktyp=1, Gaussion or"
	    " Slater; =2, Exponential; \002,/3x,\002  ktyp=3, Simple Harmonic"
	    "   ; =4, Morse        \002,/3x,\002  ktyp=5, Screened central po"
	    "tential             \002,/3x,\002  ktyp=6, SMS, Sun-Murrell-Sorb"
	    "ie potential     \002,/3x,\002  ktyp=7,  MS, Huxley-Murrell-Sorb"
	    "ie potential  \002,/3x,\002  ktyp=8,  SF, ECM potential         "
	    "           \002,/3x,\002  ktyp=9, Use NUMERICAL potentials      "
	    "      ] \002,/3x,\002Type of central  SPHERICAL potentials, ktyp"
	    " = \002,i3,//3x,\002The # of terms in potential expansion,  nvp "
	    "= \002,i3,/3x,\002    ( nvp is NOT used for ktyp = 5,6,7,8,9 )  "
	    "   \002,//3x,\002The # of r-mesh used for interpolation,  ng ="
	    " \002,i3,/3x,\002(Read-in) maximum # of bound states,  nbmax ="
	    " \002,i3,/3x,\002The # of INITITAL bound state used,     nbi ="
	    " \002,i3,/3x,\002The # of  FINAL   bound state used,     nwv ="
	    " \002,i3,/3x,\002  [ If nbmax > nwv,  code sets nbmax=nwv ]   "
	    " \002,//3x,\002The INITITAL state to AVERAGE we, ... , v_i = "
	    "\002,i3,/3x,\002The  FINAL   state to AVERAGE we, ... , v_f ="
	    " \002,i3,)";
    static char fmt_360[] = "(/3x,\002The equilibrium internuclear distance,"
	    " re =\002,f10.6,\002  ao = \002,f10.6,\002 A\002,/3x,\002The red"
	    "uced mass of linear molecule,  rmu =\002,f14.5,//3x,\002 ( kfre "
	    "= 0, do NOT cal. FREE wave;\002,/3x,\002        = 1, calculate F"
	    "REE wave  ;\002,/3x,\002        = 2, cal. we ... from E_v's )  k"
	    "fre =\002,i3,/)";
    static char fmt_390[] = "(/14x,\002Parameters for eigenvalues & wavefunc"
	    "tions :\002,//11x,\002      [ Switch for energy convergence :"
	    "\002,/11x,\002          kstd = 0, eq1=abs(Ecorr/E)     ; \002,/1"
	    "1x,\002               = 1, eq1=abs(qeng*Ecorr/E); \002,/11x,\002"
	    "               > 1, eq1=abs(E - E0 )     . ] \002,//10x,\002    "
	    "  { kcon -- iteration numbers for E_v  ; \002,/10x,\002        q"
	    "eng is in  E_v = E_v + qeng*Ecorr ; \002,/10x,\002        vtol -"
	    "- tolerances for E_v         ; \002,/10x,\002         dev -- in "
	    "  E_v = E_v + Ecor*dev  ; \002,/10x,\002         ul0 -- values o"
	    "f LEFT  functions  ; \002,/10x,\002         ur0 -- values of RIG"
	    "HT functions  ; \002,/10x,\002           ( qeng is used for kstd"
	    " >= 1 )    \002,/10x,\002           (  dev is used for ALL kstd "
	    " )    \002,/10x,\002        ( both qeng & dev may be negative ) }"
	    "\002,//\002        Iteration Ecorrection  Tolerance\002,15x,\002"
	    "Left\002,7x,\002Right\002,/\002  v kstd  kcon(v)   qeng(v)     v"
	    "tol(v)     dev(v)\002,5x,\002ul0(v)\002,5x,\002ur0(v) \002,/)";
    static char fmt_410[] = "(i3,2x,i2,2x,i5,2x,1pe13.5,1pe12.4,3(1pe11.3))";
    static char fmt_490[] = "(/3x,\002The INPUT parameters in potentials o"
	    "f :\002,//5x,\002ktyp=1; V(r) => vc(i)*r**nv(i)*exp(-xl(i)*r**2)"
	    " ; OR\002,/5x,\002        V(r) => vc(i)*r**nv(i)*exp(-xl(i)*r)"
	    " \002,/5x,\002    =2; V(r) => vc(i)*exp(-xl(i)*r) \002,/5x,\002 "
	    "   =3; V(r) => 0.5*[rmu*vc(i)**2]*r**2; we=vc. \002,/5x,\002    "
	    "=4; V(r) => De*[exp(-2*a*r) - 2*exp(-a*r)] + De\002,/5x,\002    "
	    "        De = vc(1),   a  = xl(1) for ktyp = 4 ,\002,/5x,\002    "
	    "        we = vc(2), wexe = xl(2) for ktyp = 4 .\002,//3x,\002  i"
	    "  nv(i)\002,7x,\002vc(i)\002,11x,\002xl(i)\002,/)";
    static char fmt_550[] = "(3x,i3,i5,3x,2(e15.8,2x))";
    static char fmt_414[] = "(///3x,\002The vibrational constants USED are"
	    " : \002,/6x,\002            Hartree          cm-1  \002,/6x,\002"
	    "    we =\002,2(1pe15.8,x),\002  for both a's & E(v) \002,/6x,"
	    "\002  wexe =\002,2(1pe15.8,x),\002  for E(v) ONLY \002,/6x,\002 "
	    " weye =\002,2(1pe15.8,x),\002  for E(v) ONLY \002,/6x,\002  weze"
	    " =\002,2(1pe15.8,x),\002  for E(v) ONLY \002,/6x,\002  wete ="
	    "\002,2(1pe15.8,x),\002  for E(v) ONLY \002,/6x,\002  wese =\002,"
	    "2(1pe15.8,x),\002  for E(v) ONLY \002,//3x,\002[           For  "
	    "R < Re :               \002,/3x,\002  Nturn = 0, V_ecm(R) = V_MS"
	    "  for ms = 3 ONLY ; \002,/3x,\002            For  ms = 4, 5 :   "
	    "                 \002,/3x,\002        = 1, V_ecm(R) = V_0,  V_0 "
	    "= V_Morse   ; \002,/3x,\002        = 2, V_ecm(R) = V_0,  V_0 = V"
	    "_Rydberg ; \002,/3x,\002        = 3, V_ecm(R) = V_0,  V_0 = V_PG"
	    "      . ] \002,/3x,\002                Nturn = \002,i2,/)";
    static char fmt_416[] = "(///3x,\002The energy shifting constant for ele"
	    "ctronic\002,\002 EXCITED state is : \002,/3x,\002      Add = "
	    "\002,f16.9,\002 a.u.\002,/3x,\002      Add = \002,f16.5,\002 cm-1"
	    "\002,/)";
    static char fmt_420[] = "(//5x,\002{ Switches for INITIAL energy guesses"
	    " : \002,/5x,\002   ntyp = 1, use Morse eigenvalues  ; \002,/5x"
	    ",\002        = 2, use exp't vib. energies; \002,/5x,\002        "
	    "= 3, use SMS or modified exp't E; \002,/5x,\002        = 4, use "
	    "we, ... GIVEN by code.   }  ntyp =\002,i3,///5x,\002The 'experim"
	    "ent' energies of the molecule : \002,/5x,\002( They are also wri"
	    "tten into fort.19 )\002,//7x,\002v\002,8x,\002E(v)\002,9x,\002E("
	    "v)-E(v-1)\002,/)";
    static char fmt_440[] = "(5x,i3,2x,4(1pe15.7,x))";
    static char fmt_445[] = "(1pe20.12)";
    static char fmt_450[] = "(/x,\002The ABOVE are the INITIAL (experiment) "
	    "energies\002,///x,\002Below are the suggested (trial) energies ("
	    "Es) :\002,/\002( You may shift Es around the suggested value a l"
	    "ittle )\002,/)";
    static char fmt_470[] = "(//5x,\002The inputted energies for ABOVE poten"
	    "tial :\002,/11x,\002( Edif% = | E(v) - E(v-1) |/E(v) )\002,//7x"
	    ",\002v\002,8x,\002E(v)\002,9x,\002E(v)-E(v-1)\002,7x,\002Edif"
	    "%\002,7x,\002Edif_v/Edif_v-1\002,/)";
    static char fmt_475[] = "(//20x,\002The vibrational energies :\002,//1"
	    "2x,\002\"Experiment\"\002,6x,\002Inputted\002,/7x,\002v\002,8x"
	    ",\002Ee(v)\002,11x,\002Ei(v)\002,8x,\002Ee(v)-Ei(v)\002,/)";
    static char fmt_480[] = "(//10x,\002The suggested (trial) energies :\002"
	    ",/3x,\002( You may shift Es around the suggested value a little )"
	    "\002,//12x,\002\"Experiment\"\002,6x,\002Suggested\002,/7x,\002"
	    "v\002,8x,\002Ee(v)\002,11x,\002Es(v)\002,/)";
    static char fmt_452[] = "(//11x,\002The 'experiment' vibrational constan"
	    "ts : \002,/11x,\002    we =\002,1pe14.6,\002  for exp't E(v) "
	    "\002,/11x,\002  wexe =\002,1pe14.6,\002  for exp't E(v) \002,/11"
	    "x,\002  weye =\002,1pe14.6,\002  for exp't E(v) \002,/11x,\002  "
	    "weze =\002,1pe14.6,\002  for exp't E(v) \002,/)";
    static char fmt_455[] = "(/11x,\002The code-determined constants are "
	    ":\002,/11x,\002    we =\002,1pe14.6,\002  for both a's & E(v)"
	    " \002,/11x,\002  wexe =\002,1pe14.6,\002  for E(v) ONLY \002,/11"
	    "x,\002  weye =\002,1pe14.6,\002  for E(v) ONLY \002,/11x,\002  w"
	    "eze =\002,1pe14.6,\002  for E(v) ONLY \002,//7x,\002which genera"
	    "tes following energies : \002,//7x,\002v\002,8x,\002E(v)\002,9x"
	    ",\002E(v)-E(v-1)\002,/5x,i3,2x,4(1pe15.7,x))";
    static char fmt_460[] = "(//3x,\002The parameters in Sun-Murrell-Sorbie "
	    "potential :\002,/5x,\002                ( ktyp=5 ) \002,//5x,"
	    "\002V(r) = - De*beta*(1/beta + a1*r + a2*r**2 + a3*r**3 \002,/5x,"
	    "\002           + a4*r**4 + a5*r**5 + ... )*exp(-a1*beta*r)\002,/"
	    "/5x,\002      De =\002,f11.8,\002   Hartree    = \002,f12.4,\002"
	    " cm-1 \002,//5x,\002    Beta =\002,f9.5,//5x,\002 De*Beta =\002,"
	    "f11.8,\002   Hartree    = \002,f12.4,\002 cm-1 \002,/)";
    static char fmt_462[] = "(9x,\002a(\002,i1,\002) =\002,1pe14.6,\002 1/(a"
	    "o**\002,i1,\002) =\002,1pe14.6,\002 1/(A**\002,i1,\002)\002,)";
    static char fmt_464[] = "(//2x,\002The f coefficients used to solve for "
	    "a(k) are :\002/)";
    static char fmt_466[] = "(9x,\002f(\002,i1,\002) =\002,1pe14.6,\002  Har"
	    "/(ao**\002,i1,\002)\002,)";
    static char fmt_560[] = "(//5x,\002===  Using Simple Harmonic Potentials"
	    " ===\002,/,/3x,\002The vibrational constant  we = \002,d16.8,//3"
	    "x,\002The (starting energies) eigenvalues of the \002,\002above "
	    "potential :\002,//7x,\002v\002,8x,\002E(v)\002,9x,\002E(v)-E(v-1)"
	    "\002,/)";
    static char fmt_570[] = "(//8x,\002 { nv(1) = 0, code calculates  we,  w"
	    "exe ; \002,/8x,\002         = 1, code calculates  De,  alpha;"
	    " \002,/8x,\002         = 2, code calculates wexe, alpha; \002,/8"
	    "x,\002         = 3, code calculates alpha  ONLY. }  nv(1) =\002,"
	    "i3,//3x,\002The parameters USED for MORSE potential : \002,/11x"
	    ",\002De, Alpha = \002,2(f9.6,2x),/,/3x,\002The vibrational const"
	    "ants USED for Morse energies :\002,/,8x,\002   we,  wexe =\002,2"
	    "(1pe16.8,x),/,/3x,\002The (starting energies) eigenvalues of the "
	    "\002,\002above potential :\002,//7x,\002v\002,8x,\002E(v)\002,9x,"
	    "\002E(v)-E(v-1)\002,/)";
    static char fmt_520[] = "(//x,\002The 'relative' dissociation energy D_o"
	    " : \002,/7x,\002D_o = D_e - we/2 + wexe/4 - weye/8 = \002,f10.6"
	    ",\002 a.u. \002,/42x,\002= \002,f10.6,\002 eV\002,/)";
    static char fmt_572[] = "(//3x,\002The MS type (SMS,HMS,ECM) potentia"
	    "l\002,\002 before shifting :\002,/x,\002( To find   shift = - V_"
	    "minimum; here the potential \002,/x,\002    has NO relations wit"
	    "h inputted  r_min  & r_max  )\002,//4x,\002    R            V(R)"
	    "  \002,/)";
    static char fmt_574[] = "(//3x,\002The Morse potential AFTER shifting "
	    ":\002,/x,\002( shift = - De;  here the potential has NO\002,/x"
	    ",\002    relations with inputted  r_min  & r_max  )\002,//4x,"
	    "\002    R            V(R)  \002,/)";
    static char fmt_580[] = "(/\002 Calculated shifting constant of the SMS "
	    "potential\002,\002 is, \002,/34x,\002shift =\002,1pe14.6)";
    static char fmt_576[] = "(2x,f10.5,2x,1pe16.8)";
    static char fmt_600[] = "(\002 r-mesh points used to interpolate wavefun"
	    "ctions :\002,/,\002     i           r-mesh(i) \002,/)";
    static char fmt_610[] = "(4x,i2,6x,f15.9)";
    static char fmt_590[] = "(//3x,\002--- Using inputed NUMERICAL potential"
	    " ---\002,//3x,\002The minimum radius of V_num.(r),   rmin  = "
	    "\002,f9.5,/3x,\002The r_step length  of V_num.(r),  r_step = "
	    "\002,f9.5,)";
    static char fmt_595[] = "(/3x,\002The total number of r-mesh points,  nr"
	    " =\002,i6,//3x,\002The LAST radial point is at,    r_last =\002,"
	    "f9.5,//)";
    static char fmt_800[] = "(///23x,\002 *****  THE OUTPUT  ***** \002)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_wsfe(cilist *), e_wsfe(void), do_fio(integer *, 
	    char *, ftnlen);
    double pow_di(doublereal *, integer *), sqrt(doublereal), pow_dd(
	    doublereal *, doublereal *);

    /* Local variables */
    static integer i__, k, m;
    static doublereal r__, u, f2, d00, ff[20];
    static integer kf, mr;
    static doublereal de1;
    static integer ng1;
    static doublereal re1, ri0, ev1[240], ev2[240], ss0, dee, amv, bmv, fms[
	    3500], dvi;
    static integer mpu, msv;
    static doublereal aoa1, aoa2, aoa3, aoa4, aoa5, cmao;
    extern doublereal facx_(integer *);
    static doublereal aucm;
    static integer mall;
    static doublereal perc, auev, wese, wete, wexe, weye, weze, temp;
    extern doublereal potf_(doublereal *);
    static doublereal utoh;
    static integer ntyp;
    static doublereal rlas0, rste1;
    static integer naucm;
    static doublereal rlast, rstep, xstep;

    /* Fortran I/O blocks */
    static cilist io___92 = { 0, 5, 0, 0, 0 };
    static cilist io___94 = { 0, 5, 0, 0, 0 };
    static cilist io___96 = { 0, 5, 0, 0, 0 };
    static cilist io___97 = { 0, 5, 0, 0, 0 };
    static cilist io___98 = { 0, 0, 0, fmt_320, 0 };
    static cilist io___99 = { 0, 6, 0, fmt_350, 0 };
    static cilist io___100 = { 0, 6, 0, fmt_360, 0 };
    static cilist io___101 = { 0, 6, 0, fmt_390, 0 };
    static cilist io___103 = { 0, 5, 0, 0, 0 };
    static cilist io___104 = { 0, 6, 0, fmt_410, 0 };
    static cilist io___107 = { 0, 6, 0, fmt_490, 0 };
    static cilist io___108 = { 0, 5, 0, 0, 0 };
    static cilist io___109 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___110 = { 0, 5, 0, 0, 0 };
    static cilist io___115 = { 0, 5, 0, 0, 0 };
    static cilist io___116 = { 0, 5, 0, 0, 0 };
    static cilist io___120 = { 0, 5, 0, 0, 0 };
    static cilist io___123 = { 0, 5, 0, 0, 0 };
    static cilist io___126 = { 0, 6, 0, fmt_414, 0 };
    static cilist io___127 = { 0, 6, 0, fmt_416, 0 };
    static cilist io___130 = { 0, 6, 0, fmt_420, 0 };
    static cilist io___131 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___132 = { 0, 19, 0, fmt_445, 0 };
    static cilist io___134 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___135 = { 0, 19, 0, fmt_445, 0 };
    static cilist io___136 = { 0, 19, 0, fmt_450, 0 };
    static cilist io___137 = { 0, 5, 0, 0, 0 };
    static cilist io___138 = { 0, 6, 0, fmt_470, 0 };
    static cilist io___139 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___144 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___145 = { 0, 6, 0, fmt_475, 0 };
    static cilist io___146 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___147 = { 0, 6, 0, fmt_480, 0 };
    static cilist io___148 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___149 = { 0, 19, 0, fmt_445, 0 };
    static cilist io___150 = { 0, 6, 0, fmt_452, 0 };
    static cilist io___151 = { 0, 5, 0, 0, 0 };
    static cilist io___152 = { 0, 5, 0, 0, 0 };
    static cilist io___153 = { 0, 6, 0, fmt_455, 0 };
    static cilist io___154 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___155 = { 0, 19, 0, fmt_445, 0 };
    static cilist io___164 = { 0, 4, 0, 0, 0 };
    static cilist io___168 = { 0, 13, 0, 0, 0 };
    static cilist io___169 = { 0, 6, 0, fmt_460, 0 };
    static cilist io___170 = { 0, 6, 0, fmt_462, 0 };
    static cilist io___171 = { 0, 6, 0, fmt_464, 0 };
    static cilist io___172 = { 0, 6, 0, fmt_466, 0 };
    static cilist io___173 = { 0, 6, 0, fmt_560, 0 };
    static cilist io___175 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___176 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___177 = { 0, 6, 0, fmt_570, 0 };
    static cilist io___178 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___179 = { 0, 6, 0, fmt_440, 0 };
    static cilist io___181 = { 0, 6, 0, fmt_520, 0 };
    static cilist io___182 = { 0, 8, 0, fmt_572, 0 };
    static cilist io___183 = { 0, 8, 0, fmt_574, 0 };
    static cilist io___188 = { 0, 6, 0, fmt_580, 0 };
    static cilist io___189 = { 0, 8, 0, fmt_576, 0 };
    static cilist io___192 = { 0, 6, 0, fmt_600, 0 };
    static cilist io___194 = { 0, 22, 0, 0, 0 };
    static cilist io___195 = { 0, 6, 0, fmt_610, 0 };
    static cilist io___197 = { 0, 5, 0, 0, 0 };
    static cilist io___202 = { 0, 5, 0, 0, 0 };
    static cilist io___203 = { 0, 20, 0, 0, 0 };
    static cilist io___204 = { 0, 6, 0, fmt_590, 0 };
    static cilist io___205 = { 0, 6, 0, fmt_595, 0 };
    static cilist io___206 = { 0, 6, 0, fmt_800, 0 };


/* ---------------------------------------------------------------- */
/* ----------------------------------------------------------------- */
/* --- read in radius information */
/*  CAUTION about rmatch : */
/*    rmatch is the matching boundary radius (e.g. rmatch~re). This */
/*  value should be in the classically ALLOWED region. For wave */
/*  functions which are ZERO at re, rmatch must be slightly off */
/*  re to get the converged wavefunctions ! */
/* ================================================================= */
    /* Parameter adjustments */
    --rmesh;

    /* Function Body */
    s_rsle(&io___92);
    do_lio(&c__3, &c__1, (char *)&kf, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___94);
    do_lio(&c__5, &c__1, (char *)&rdata_1.rmin, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&rdata_1.rmax, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&rstep, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&rdata_1.rmatch, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___96);
    do_lio(&c__5, &c__1, (char *)&shomos_1.re, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&shomos_1.rmu, (ftnlen)sizeof(doublereal));
    do_lio(&c__3, &c__1, (char *)&pdata_1.iel, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&pdata_1.ktyp, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&sf_1.isf, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&fdata_1.kfre, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___97);
    do_lio(&c__3, &c__1, (char *)&pdata_1.nvp, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&(*ng), (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&(*nbmax), (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&(*nbi), (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&(*nwv), (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&fdata_1.nci, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&fdata_1.ncf, (ftnlen)sizeof(integer));
    e_rsle();
/* --- */
    s_wsfe(&io___98);
    e_wsfe();
    s_wsfe(&io___99);
    do_fio(&c__1, (char *)&rdata_1.rmin, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rdata_1.rmax, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rstep, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rdata_1.rmatch, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&pdata_1.iel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&pdata_1.ktyp, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&pdata_1.nvp, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*ng), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nbmax), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nbi), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nwv), (ftnlen)sizeof(integer));
    i__1 = fdata_1.nci - 1;
    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
    i__2 = fdata_1.ncf - 1;
    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
    e_wsfe();
    s_wsfe(&io___100);
    do_fio(&c__1, (char *)&shomos_1.re, (ftnlen)sizeof(doublereal));
    d__1 = shomos_1.re * .529177249;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&shomos_1.rmu, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&fdata_1.kfre, (ftnlen)sizeof(integer));
    e_wsfe();
/* --- */
    s_wsfe(&io___101);
    e_wsfe();
/* --- Readin iteration #, E correction factor, & tolerences, etc. */
/* ---   for eigenvalue convergence */
    i__1 = *nbmax;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_rsle(&io___103);
	do_lio(&c__3, &c__1, (char *)&kmon_1.kstd[i__ - 1], (ftnlen)sizeof(
		integer));
	do_lio(&c__3, &c__1, (char *)&vcon_1.kcon[i__ - 1], (ftnlen)sizeof(
		integer));
	do_lio(&c__5, &c__1, (char *)&vcon_1.qeng[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&vcon_1.vtol[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&vcon_1.dev[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&rdata_1.ul0[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&rdata_1.ur0[i__ - 1], (ftnlen)sizeof(
		doublereal));
	e_rsle();
	s_wsfe(&io___104);
	i__2 = i__ - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&kmon_1.kstd[i__ - 1], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vcon_1.kcon[i__ - 1], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vcon_1.qeng[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&vcon_1.vtol[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&vcon_1.dev[i__ - 1], (ftnlen)sizeof(doublereal)
		);
	do_fio(&c__1, (char *)&rdata_1.ul0[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&u, (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L10: */
    }
/* --- */
    rdata_1.h__ = rstep;
    *nr = (integer) ((integer) (rdata_1.rmax - rdata_1.rmin) / rdata_1.h__);
/* ------------------------------------------------------------ */
/* --- read in SPHERICAL potential information */
/* ------------------------------------------------------------ */
    aucm = 219474.63067;
/* - */
    if (pdata_1.ktyp < 5) {
/* -----  For other spherical potential  ---- */
/* -----  For SHO, nvp=1; Morse, nvp=2.  ---- */
	s_wsfe(&io___107);
	e_wsfe();
	i__1 = pdata_1.nvp;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___108);
	    do_lio(&c__3, &c__1, (char *)&pdata_1.nv[i__ - 1], (ftnlen)sizeof(
		    integer));
	    do_lio(&c__5, &c__1, (char *)&pdata_1.vc[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&pdata_1.xl[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	    s_wsfe(&io___109);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&pdata_1.nv[i__ - 1], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&pdata_1.vc[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&pdata_1.xl[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	}
    } else if (pdata_1.ktyp >= 6 && pdata_1.ktyp <= 8) {
/* ------------------------------------------------------------ */
/* -----  For SMS, HMS, SF potential  ---- */
/*   The (corrected) eigenvalues of Morse potential */
/* are input as the first GUESS for MS potential; */
/* or use the experimental vibrational energies as */
/* the first guess. */

/*  naucm = 0, read we, wexe,... in Hartree; = 1, in cm-1. */
/*     ms = 3, 4, 5 : The highest power in the expansion of MS potential. */
/*   ntyp = 1, use Morse eigenvalues as 1st guess */
/*        = 2, use exp't vibrational energies as 1st guess */
/*        = 3, use Sun-Murrell-Sorbie energies as 1st guess */
/*   mall = 1, use the a1 from fort.5 (intermediate case); */
/*        > 1, use the a1 from fort.4 (continuous running case). */
/*    msv = v, the code'll generate approximate energies starting from v. */

/* 1.) Sun's Modified Murrell-Sorbie (x=R-Re) : */
/*           V_SMMS(R)=-De*beta*(1/beta + a1*x + a2*x**2 */
/*                                 + a3*x**3 + ...)*exp(-a1*beta*x) */

/* 2.) Huxley-Murrell-Sorbie (x=R-Re) : */
/*           V_MS(R)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x) */

/* 3.) SF (ECM) : */
/*           V_ecm(R) = V_MS + Lamta(R)*delta_V(R) */

/*           V_MS:  Murrell & Sorbie potential in which the */
/*                 (a1,a2,a3, ...) are calculated using SF formulae. */

/*          Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)] */
/*          delta_V(R) = V_MS - V_0 */
/*                For  R < Re : */
/*      Nturn = 0, V_ecm(R) = V_MS  For ms = 3 ONLY ; */
/*                For  ms = 4, 5 : */
/*            = 1, V_ecm(R) = V_0,  V_0 = V_Morse   ; */
/*            = 2, V_ecm(R) = V_0,  V_0 = V_Rydberg ; */
/*            = 3, V_ecm(R) = V_0,  V_0 = V_PG      . */
/* --------------------------------------------------------------- */
/*  De0, we, wexe, weye, weze, wete, wese  are ALL in  Hartree. */
/* --------------------------------------------------------------- */

	s_rsle(&io___110);
	do_lio(&c__3, &c__1, (char *)&naucm, (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&murrell_1.de0, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&murrell_1.beta, (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&sf_1.alamta, (ftnlen)sizeof(doublereal))
		;
	do_lio(&c__3, &c__1, (char *)&pdata_1.ms, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&ntyp, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&mall, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&msv, (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&murrell_1.a1, (ftnlen)sizeof(doublereal)
		);
	e_rsle();
	s_rsle(&io___115);
	do_lio(&c__5, &c__1, (char *)&sf_1.amu, (ftnlen)sizeof(doublereal));
	do_lio(&c__3, &c__1, (char *)&sf_1.nturn, (ftnlen)sizeof(integer));
	e_rsle();
	s_rsle(&io___116);
	do_lio(&c__5, &c__1, (char *)&shomos_1.we, (ftnlen)sizeof(doublereal))
		;
	do_lio(&c__5, &c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&weye, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&weze, (ftnlen)sizeof(doublereal));
	e_rsle();
	s_rsle(&io___120);
	do_lio(&c__5, &c__1, (char *)&wete, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&wese, (ftnlen)sizeof(doublereal));
	e_rsle();
	s_rsle(&io___123);
	do_lio(&c__5, &c__1, (char *)&pdata_1.add, (ftnlen)sizeof(doublereal))
		;
	e_rsle();

	if (naucm > 0) {
	    shomos_1.we /= aucm;
	    wexe /= aucm;
	    weye /= aucm;
	    weze /= aucm;
	    wete /= aucm;
	    wese /= aucm;
	    pdata_1.add /= aucm;
	}
/* --- */
	dee = murrell_1.de0;
	de1 = murrell_1.de0 * murrell_1.beta;
/* --- */
	s_wsfe(&io___126);
	do_fio(&c__1, (char *)&shomos_1.we, (ftnlen)sizeof(doublereal));
	d__1 = shomos_1.we * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
	d__2 = wexe * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&weye, (ftnlen)sizeof(doublereal));
	d__3 = weye * aucm;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&weze, (ftnlen)sizeof(doublereal));
	d__4 = weze * aucm;
	do_fio(&c__1, (char *)&d__4, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&wete, (ftnlen)sizeof(doublereal));
	d__5 = wete * aucm;
	do_fio(&c__1, (char *)&d__5, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&wese, (ftnlen)sizeof(doublereal));
	d__6 = wese * aucm;
	do_fio(&c__1, (char *)&d__6, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sf_1.nturn, (ftnlen)sizeof(integer));
	e_wsfe();
/* --- */
	if (pdata_1.add > 0.) {
	    s_wsfe(&io___127);
	    do_fio(&c__1, (char *)&pdata_1.add, (ftnlen)sizeof(doublereal));
	    d__1 = pdata_1.add * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* --- */
/*         if (mall .gt. 1) read(4,*) a1 */
/*         an(1) = a1 */
/* --- */
	i__1 = *nbmax;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    amv = (doublereal) (i__ - 1);
	    bmv = amv + .5;
/* Computing 2nd power */
	    d__1 = bmv;
/* Computing 3rd power */
	    d__2 = bmv;
/* Computing 4th power */
	    d__3 = bmv, d__3 *= d__3;
	    vibe_1.evib[i__ - 1] = shomos_1.we * bmv - wexe * (d__1 * d__1) + 
		    weye * (d__2 * (d__2 * d__2)) + weze * (d__3 * d__3);
/* Computing 5th power */
	    d__1 = bmv, d__2 = d__1, d__1 *= d__1;
/* Computing 6th power */
	    d__3 = bmv, d__3 *= d__3;
	    vibe_1.evib[i__ - 1] = vibe_1.evib[i__ - 1] + wete * (d__2 * (
		    d__1 * d__1)) + wese * (d__3 * (d__3 * d__3));
/* --- Herzberg, P. 69 : */
/*             temp =we*(amv+0.5d0) - wexe*(amv+0.5d0)**2 */
/*           evib(i)=temp + weye*(amv+0.5d0)**3 + weze*(amv+0.5d0)**4 */

	    vibe_1.eori[i__ - 1] = vibe_1.evib[i__ - 1];
	}
/* --- */
	s_wsfe(&io___130);
	do_fio(&c__1, (char *)&ntyp, (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___131);
	do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vibe_1.eori[0], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___132);
	do_fio(&c__1, (char *)&vibe_1.eori[0], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = *nbmax;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    dvi = vibe_1.eori[i__ - 1] - vibe_1.eori[i__ - 2];
	    s_wsfe(&io___134);
	    i__2 = i__ - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&vibe_1.eori[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&dvi, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___135);
	    do_fio(&c__1, (char *)&vibe_1.eori[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	}
	s_wsfe(&io___136);
	e_wsfe();
/* === */
	if (ntyp == 3) {
/* --- Readin initial (numerical) guesses for energies */
	    i__1 = *nbmax;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		s_rsle(&io___137);
		do_lio(&c__5, &c__1, (char *)&vibe_1.evib1[i__ - 1], (ftnlen)
			sizeof(doublereal));
		e_rsle();
	    }
	    s_wsfe(&io___138);
	    e_wsfe();
	    s_wsfe(&io___139);
	    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&vibe_1.evib1[0], (ftnlen)sizeof(doublereal)
		    );
	    e_wsfe();
	    vibe_1.expr[0] = 1.;
/* --- */
	    i__1 = *nbmax;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		ev1[i__ - 1] = vibe_1.evib1[i__ - 1] - vibe_1.evib1[i__ - 2];
		vibe_1.expr[i__ - 1] = (d__1 = ev1[i__ - 1], abs(d__1)) / 
			vibe_1.evib1[i__ - 1];
		perc = vibe_1.expr[i__ - 1] / vibe_1.expr[i__ - 2];
/* - */
/*  If wanted, generate suggested (trial) energies ev2(i) : */
		if (msv > 0 && i__ >= msv) {
		    temp = ev1[i__ - 2] * (1. - vibe_1.expr[i__ - 2]);
		    ev2[i__ - 1] = vibe_1.evib1[i__ - 2] + temp;
		}
/* - */
		s_wsfe(&io___144);
		i__2 = i__ - 1;
		do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&vibe_1.evib1[i__ - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&ev1[i__ - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&vibe_1.expr[i__ - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&perc, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    }
/* --- */
	    s_wsfe(&io___145);
	    e_wsfe();
	    i__1 = *nbmax;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		dvi = vibe_1.evib[i__ - 1] - vibe_1.evib1[i__ - 1];
		s_wsfe(&io___146);
		i__2 = i__ - 1;
		do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&vibe_1.evib[i__ - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&vibe_1.evib1[i__ - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&dvi, (ftnlen)sizeof(doublereal));
		e_wsfe();
		vibe_1.evib[i__ - 1] = vibe_1.evib1[i__ - 1];
	    }

	    if (msv > 0) {
		s_wsfe(&io___147);
		e_wsfe();
		i__1 = *nbmax;
		for (i__ = msv; i__ <= i__1; ++i__) {
		    s_wsfe(&io___148);
		    i__2 = i__ - 1;
		    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&vibe_1.eori[i__ - 1], (ftnlen)
			    sizeof(doublereal));
		    do_fio(&c__1, (char *)&ev2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
		    s_wsfe(&io___149);
		    do_fio(&c__1, (char *)&ev2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
		}
	    }
/* --- */
	}
/* === */
	if (ntyp == 4) {
/*  Write out "experimental" constants : */
	    s_wsfe(&io___150);
	    do_fio(&c__1, (char *)&shomos_1.we, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&weye, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&weze, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&wete, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&wese, (ftnlen)sizeof(doublereal));
	    e_wsfe();
/* --- */
/* Read we, ... DETERMINED by code from previous job using */
/* calculated vibrational energies E_v's. */
/* --- */
	    s_rsle(&io___151);
	    do_lio(&c__5, &c__1, (char *)&shomos_1.we, (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
	    do_lio(&c__5, &c__1, (char *)&weye, (ftnlen)sizeof(doublereal));
	    do_lio(&c__5, &c__1, (char *)&weze, (ftnlen)sizeof(doublereal));
	    e_rsle();
	    s_rsle(&io___152);
	    do_lio(&c__5, &c__1, (char *)&wete, (ftnlen)sizeof(doublereal));
	    do_lio(&c__5, &c__1, (char *)&wese, (ftnlen)sizeof(doublereal));
	    e_rsle();
	    if (naucm > 0) {
		shomos_1.we /= aucm;
		wexe /= aucm;
		weye /= aucm;
		weze /= aucm;
		wete /= aucm;
		wese /= aucm;
	    }

	    i__1 = *nbmax;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		amv = (doublereal) (i__ - 1);
		bmv = amv + .5;
/* Computing 2nd power */
		d__1 = bmv;
/* Computing 3rd power */
		d__2 = bmv;
/* Computing 4th power */
		d__3 = bmv, d__3 *= d__3;
		vibe_1.evib[i__ - 1] = shomos_1.we * bmv - wexe * (d__1 * 
			d__1) + weye * (d__2 * (d__2 * d__2)) + weze * (d__3 *
			 d__3);
/*           evib(i)=evib(i) + wete*bmv**5 + wese*bmv**6 */
	    }
	    s_wsfe(&io___153);
	    do_fio(&c__1, (char *)&shomos_1.we, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&weye, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&weze, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&vibe_1.evib[0], (ftnlen)sizeof(doublereal))
		    ;
	    e_wsfe();
	    i__1 = *nbmax;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		dvi = vibe_1.evib[i__ - 1] - vibe_1.evib[i__ - 2];
		s_wsfe(&io___154);
		i__2 = i__ - 1;
		do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&vibe_1.evib[i__ - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&dvi, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___155);
		do_fio(&c__1, (char *)&vibe_1.evib[i__ - 1], (ftnlen)sizeof(
			doublereal));
		e_wsfe();
	    }
	}
/* === */
/* ------------------------------------------------------------ */
/*   Prepare CONVERSION factors : */
/* ------------------------------------------------------------ */
	utoh = 34231772.5;
	cmao = 5.29177249e-9;
	aoa1 = 1.889725989;
	aoa2 = aoa1 * aoa1;
	aoa3 = aoa2 * aoa1;
	aoa4 = aoa3 * aoa1;
	aoa5 = aoa4 * aoa1;
/* --- Prepare Prefactors : */
	ss0 = .021657164;
/* ------------------------------------------------------------ */
/* Evaluate Sun's modified MS expansion coefficients : */
/* ------------------------------------------------------------ */
/* - */
	if (sf_1.isf == 1) {
/* ---For ECM V_ecm(R) : */
	    i__1 = pdata_1.ms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		s_rsle(&io___164);
		do_lio(&c__5, &c__1, (char *)&sf_1.an[i__ - 1], (ftnlen)
			sizeof(doublereal));
		e_rsle();
	    }
	} else if (sf_1.isf == 0) {
/* --- For SMS */
/* Computing 2nd power */
	    d__1 = aucm * shomos_1.we * cmao;
	    f2 = ss0 * 1.000004496 * (utoh * shomos_1.rmu) * (d__1 * d__1);
	    ff[1] = f2;
/* Computing 2nd power */
	    d__1 = murrell_1.a1;
	    sf_1.an[1] = (d__1 * d__1 - f2 / de1) * .5;
/* --- Sun's formulae : */
	    i__1 = pdata_1.ms + 1;
	    for (k = 3; k <= i__1; ++k) {
		temp = pow_di(&c_b380, &k) * 2. * (k - 1);
		temp = temp * sqrt(pow_di(&f2, &k)) / facx_(&k);
		d__1 = k / 2. - 1.;
		ff[k - 1] = temp / pow_dd(&de1, &d__1);
	    }

	    if (kf == 1) {
		i__1 = pdata_1.ms + 1;
		for (k = 2; k <= i__1; ++k) {
		    s_rsle(&io___168);
		    do_lio(&c__5, &c__1, (char *)&ff[k - 1], (ftnlen)sizeof(
			    doublereal));
		    e_rsle();
		}
		ff[1] = f2;
/* Computing 2nd power */
		d__1 = murrell_1.a1;
		sf_1.an[1] = (d__1 * d__1 - f2 / de1) * .5;
	    }
/* --- */
	    i__1 = pdata_1.ms;
	    for (k = 3; k <= i__1; ++k) {
		d__1 = -murrell_1.a1;
		temp = ff[k - 1] * -.5 / de1 + pow_di(&d__1, &k) * (k - 1) / 
			facx_(&k);
		i__2 = k - 1;
		for (i__ = 2; i__ <= i__2; ++i__) {
		    d__1 = -murrell_1.a1;
		    i__3 = k - i__;
		    i__4 = k - i__;
		    temp -= pow_di(&d__1, &i__3) * sf_1.an[i__ - 1] / facx_(&
			    i__4);
		}
		sf_1.an[k - 1] = temp;
	    }
/* --- */
	    s_wsfe(&io___169);
	    do_fio(&c__1, (char *)&murrell_1.de0, (ftnlen)sizeof(doublereal));
	    d__1 = murrell_1.de0 * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&murrell_1.beta, (ftnlen)sizeof(doublereal))
		    ;
	    do_fio(&c__1, (char *)&de1, (ftnlen)sizeof(doublereal));
	    d__2 = de1 * aucm;
	    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    i__1 = pdata_1.ms;
	    for (k = 1; k <= i__1; ++k) {
		s_wsfe(&io___170);
		do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&sf_1.an[k - 1], (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
		d__1 = sf_1.an[k - 1] * pow_di(&aoa1, &k);
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* - */
	    s_wsfe(&io___171);
	    e_wsfe();
	    i__1 = pdata_1.ms + 1;
	    for (k = 2; k <= i__1; ++k) {
		s_wsfe(&io___172);
		do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ff[k - 1], (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* --------------------------------------------------------------- */
/*           f3=-2.0d0*dsqrt(f2**3/De1)/3.0d0 */
/*           f4=0.25d0*f2**2/De1 */
/*           f5=-dsqrt(f2**5)/(15.0d0*dsqrt(De1**3) ) */
/*         a2=0.5d0*(a1**2 - f2/De1) */
/*         a3=a1*a2 - a1**3/3.0d0 - 0.5d0*f3/De1 */
/*         a4=0.0 */
/*         a5=0.0 */
/*         a6=0.0 */
/*         a7=0.0 */
/*           if (ms .gt. 3) then */
/*             a4=a1*a3 - 0.5d0*a1**2*a2 + a1**4/8.0d0 */
/*             a4=a4 - 0.5d0*f4/De1 */
/*             a5=a1*a4 - 0.5d0*a1**2*a3 + a1**3*a2/6.0d0 */
/*             a5=a5 - a1**5/30.0d0 - 0.5d0*f5/De1 */
/*           endif */
/* --------------------------------------------------------------- */
/*         write(6,465) De0,De0*aucm,beta,De1,De1*aucm,a1,a1*aoA1, */
/*    # a2,a2*aoA2,a3,a3*aoA3,a4,a4*aoA4,a5,a5*aoA5,we,we*aucm, */
/*    # wexe,wexe*aucm,weye,weye*aucm,weze,weze*aucm, */
/*    # wete,wete*aucm,wese,wese*aucm, */
/*    # f2,f3,f4,f5 */
/* --------------------------------------------------------------- */
	}
    }
/* ------------------------------------------------------------ */
/*  Calculate SHO energies if neccessary. */
/* ------------------------------------------------------------ */
    if (pdata_1.ktyp == 3) {
	shomos_1.we = pdata_1.vc[0];
	wexe = pdata_1.xl[0];
	s_wsfe(&io___173);
	do_fio(&c__1, (char *)&shomos_1.we, (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = *nbmax;
	for (m = 1; m <= i__1; ++m) {
	    amv = (doublereal) (m - 1);
	    vibe_1.evib[m - 1] = shomos_1.we * (amv + .5);
	    vibe_1.eori[m - 1] = vibe_1.evib[m - 1];
	}
	s_wsfe(&io___175);
	do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vibe_1.evib[0], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = *nbmax;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    dvi = vibe_1.evib[i__ - 1] - vibe_1.evib[i__ - 2];
	    s_wsfe(&io___176);
	    i__2 = i__ - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&vibe_1.evib[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&dvi, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
/* ------------------------------------------------------------ */
/*    Calculate Morse energies if neccessary. */
/*  When nv(1)=0, code calculates we, wexe for given de, alpha, rmu, & re; */
/*            =1, code calculates de, alpha for given we, wexe, rmu, & re; */
/*            =2, code calculates wexe, alpha for given de, we, rmu, & re; */
/*            =3, code calculates alpha (ONLY) for given wexe, rmu,  & re. */
/*  nv(1) = 3 may be UNphysical since  wexe is DETERMINED by De & we, */
/*  then wexe DETERMINES alpha.  wexe MUST be consistent with De, OR */
/*  you may get UNphysical (& unconverged) results. */

/* ------------------------------------------------------------ */
    if (pdata_1.ktyp == 4) {
	shomos_1.de = pdata_1.vc[0];
	shomos_1.alpha = pdata_1.xl[0];
	shomos_1.we = pdata_1.vc[1];
	wexe = pdata_1.xl[1];
	dee = shomos_1.de;

	if (pdata_1.nv[0] == 0) {
	    shomos_1.we = shomos_1.alpha / shomos_1.re * sqrt(shomos_1.de * 
		    2. / shomos_1.rmu);
/* Computing 2nd power */
	    d__1 = shomos_1.alpha / shomos_1.re;
	    wexe = d__1 * d__1 / (shomos_1.rmu * 2.);
	} else if (pdata_1.nv[0] == 1) {
/* Computing 2nd power */
	    d__1 = shomos_1.we;
	    shomos_1.de = d__1 * d__1 / (wexe * 4.);
	    shomos_1.alpha = shomos_1.re * sqrt(shomos_1.rmu * 2. * wexe);
	} else if (pdata_1.nv[0] == 2) {
/* Computing 2nd power */
	    d__1 = shomos_1.we;
	    wexe = d__1 * d__1 / (shomos_1.de * 4.);
	    shomos_1.alpha = shomos_1.re * sqrt(shomos_1.rmu * 2. * wexe);
	} else if (pdata_1.nv[0] == 3) {
	    shomos_1.alpha = shomos_1.re * sqrt(shomos_1.rmu * 2. * wexe);
	}

	s_wsfe(&io___177);
	do_fio(&c__1, (char *)&pdata_1.nv[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&shomos_1.de, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&shomos_1.alpha, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&shomos_1.we, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = *nbmax;
	for (m = 1; m <= i__1; ++m) {
	    amv = (doublereal) (m - 1);
/* Computing 2nd power */
	    d__1 = amv + .5;
	    vibe_1.evib[m - 1] = shomos_1.we * (amv + .5) - wexe * (d__1 * 
		    d__1);
	    vibe_1.eori[m - 1] = vibe_1.evib[m - 1];
	}
	s_wsfe(&io___178);
	do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vibe_1.evib[0], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = *nbmax;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    dvi = vibe_1.evib[i__ - 1] - vibe_1.evib[i__ - 2];
	    s_wsfe(&io___179);
	    i__2 = i__ - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&vibe_1.evib[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&dvi, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
/* ----------------------------------------- */
    if (*nwv < *nbmax) {
	*nbmax = *nwv;
    }
/* --------------------------------------------------------------- */
/* Calculate the "relative" dissociation energy  D_o */
/* --------------------------------------------------------------- */
    if (pdata_1.ktyp >= 4 && pdata_1.ktyp < 9) {
	d00 = dee - shomos_1.we * .5 + wexe * .25 - weye / 8.;
	s_wsfe(&io___181);
	do_fio(&c__1, (char *)&d00, (ftnlen)sizeof(doublereal));
	d__1 = d00 * 27.21139618;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* --------------------------------------------------------------- */
/*   Find shift constant of the Sun-Murrell-Sorbie/Morse */
/* potential such that   shift = - V_minimum  . */
/* --------------------------------------------------------------- */
    if (pdata_1.ktyp < 9) {

	if (pdata_1.ktyp >= 6 && pdata_1.ktyp <= 8) {
	    s_wsfe(&io___182);
	    e_wsfe();
	} else if (pdata_1.ktyp == 4) {
	    s_wsfe(&io___183);
	    e_wsfe();
	}
/* --- */
	murrell_1.shift = 0.;
	re1 = shomos_1.re / 2.;
	xstep = shomos_1.re / 400;
	r__ = re1 - xstep;
	for (i__ = 1; i__ <= 2000; ++i__) {
	    r__ += xstep;
	    fms[i__ - 1] = potf_(&r__);
/* L20: */
	}
	for (i__ = 2; i__ <= 3000; ++i__) {
	    if (fms[i__ - 1] < fms[i__ - 2]) {
		murrell_1.shift = fms[i__ - 1];
	    }
	}
	if (murrell_1.shift < 0.) {
	    murrell_1.shift = -murrell_1.shift;
	}
	s_wsfe(&io___188);
	do_fio(&c__1, (char *)&murrell_1.shift, (ftnlen)sizeof(doublereal));
	e_wsfe();

	r__ = re1 - xstep;
	for (i__ = 1; i__ <= 3000; ++i__) {
	    r__ += xstep;
	    if (pdata_1.ktyp == 4) {
		fms[i__ - 1] -= shomos_1.de;
	    }
	    s_wsfe(&io___189);
	    do_fio(&c__1, (char *)&r__, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&fms[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

    }
    rlast = rdata_1.rmin + (*nr - 1) * rstep;
    rlas0 = rlast;
/* -------------------------------------------------------------- */
    s_wsfe(&io___192);
    e_wsfe();
    if (pdata_1.ktyp >= 4) {
	ng1 = *ng;
    } else if (pdata_1.ktyp < 4) {
	ng1 = 3;
    }
    i__1 = ng1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_rsle(&io___194);
	do_lio(&c__5, &c__1, (char *)&rmesh[i__], (ftnlen)sizeof(doublereal));
	e_rsle();
	s_wsfe(&io___195);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&rmesh[i__], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L100: */
    }
/* --------------------------------------------------------------- */
/* If wanted, read in NUMERICAL potentials : */

/*    mpu -- 0, read POT in Hartree; = 1, in eV; = 2, in cm-1. */
/*     nr -- The # of potential points to be read */
/*    ri0 -- The first radial value of the numerical potential */
/*  rste1 -- The step length of the numerical potential */
/* --------------------------------------------------------------- */
    if (pdata_1.ktyp == 9) {
	auev = 27.21139618;
	s_rsle(&io___197);
	do_lio(&c__3, &c__1, (char *)&mpu, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&mr, (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&ri0, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&rste1, (ftnlen)sizeof(doublereal));
	e_rsle();
	i__1 = *nbmax;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___202);
	    do_lio(&c__5, &c__1, (char *)&vibe_1.evib[i__ - 1], (ftnlen)
		    sizeof(doublereal));
	    e_rsle();
	}
	i__1 = mr;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___203);
	    do_lio(&c__5, &c__1, (char *)&vmon_1.rv[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&vmon_1.vnum[i__ - 1], (ftnlen)
		    sizeof(doublereal));
	    e_rsle();
	    if (mpu == 1) {
		vmon_1.vnum[i__ - 1] /= auev;
	    } else if (mpu == 2) {
		vmon_1.vnum[i__ - 1] /= aucm;
	    }
	}
	rdata_1.h__ = rste1;
	rdata_1.rmin = ri0;
	if (vmon_1.rv[mr - 1] < rdata_1.rmax) {
	    rdata_1.rmax = vmon_1.rv[mr - 1];
	    rlast = rdata_1.rmax;
	}
	if (mr < *nr) {
	    *nr = mr;
	}
	s_wsfe(&io___204);
	do_fio(&c__1, (char *)&ri0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rste1, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* --------------------------------------------------------------- */
/* Wavefunctions will be interpolated on the following r-mesh */
/* --------------------------------------------------------------- */
    s_wsfe(&io___205);
    do_fio(&c__1, (char *)&(*nr), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&rlast, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* - */
    s_wsfe(&io___206);
    e_wsfe();
/* ============================================================ */
/* L200: */
/* L210: */
/*    #/6x,"  For HIGH v's, SMALL rmax may cause WRONG nodes .  ", */
/*    #/7x,'v',8x,'E(v)',9x,'E(v)-E(v-1)',/) */
    return 0;
} /* readin_ */


/* = */
/* Subroutine */ int boundv_(integer *nbi, integer *ku, integer *nu, 
	doublereal *q, doublereal *u)
{
    /* Format strings */
    static char fmt_310[] = "(////5x,55(\002*\002),//10x,\002     I STEAL Dr"
	    ". Weiguo Sun's program \002,//10x,\002        I realized this is"
	    " ILLEGAL \002,//10x,\002I am willing to get any PENALTIES I dese"
	    "rve ! \002,//5x,55(\002*\002),/)";
    static char fmt_25[] = "(x,i3,x,3(1pe16.8),2x,i2,x,1pe14.6)";
    static char fmt_20[] = "(i3,1pe12.4,3(1pe14.6),1pe10.3,2x,i2,i4)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void);
    /* Subroutine */ int s_stop(char *, ftnlen);
    double pow_di(doublereal *, integer *), sqrt(doublereal), exp(doublereal);
    integer do_fio(integer *, char *, ftnlen);

    /* Local variables */
    static doublereal q0;
    static integer kp, kv;
    static doublereal eq1, ul1, ul2, ur1, ur2, qdif, ecor;
    extern doublereal potf_(doublereal *);
    static doublereal ecorr, wronsk;
    extern /* Subroutine */ int solvese_(D_fp, doublereal *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);

    /* Fortran I/O blocks */
    static cilist io___210 = { 0, 6, 0, fmt_310, 0 };
    static cilist io___219 = { 0, 0, 0, fmt_25, 0 };
    static cilist io___221 = { 0, 6, 0, fmt_20, 0 };


/* ---------------------------------------------------------------- */
/*   boundv (call solvese -->) solves Schrodinger Equation & get */
/* the numerical wavefunction u. */

/* Input: */
/*   ku -- the unit to write convergence information. */
/*   nu -- is the nu_th (vibrational) state with quantum number (nu-1). */

/* Output: */
/*    Q -- the eigenvalue of the nu_th vibrational state. */
/*    u -- the nu_th eigenvector. */
/* ---------------------------------------------------------------- */
/* ---------------------------------------------------------------- */
/*  Get the bound state energies for a given state nu */
/*    Q = evib(nu) = E_v */
/* ---------------------------------------------------------------- */
    /* Parameter adjustments */
    --u;

    /* Function Body */
    kv = 0;
    kp = 0;
    q0 = 0.;
/* ---------------------------------------------------------------- */
    if (*nu == *nbi && pdata_1.ktyp >= 98) {
	s_wsfe(&io___210);
	e_wsfe();
	s_stop("", (ftnlen)0);
    }
/* ---------------------------------------------------------------- */
/* L5: */
    *q = vibe_1.evib[*nu - 1];
/* ---------------------------------------------------------------- */
/* parameters for the wavefunction solution */
/* ---------------------------------------------------------------- */
/* L8: */
    ul1 = rdata_1.ul0[*nu - 1];
    ur1 = rdata_1.ur0[*nu - 1];
/* --- */
    d__1 = (rdata_1.rmin + rdata_1.h__) / rdata_1.rmin;
    i__1 = pdata_1.iel + 1;
    ul2 = ul1 * pow_di(&d__1, &i__1);

    if (pdata_1.ktyp < 9) {
	d__1 = rdata_1.rmax - rdata_1.h__;
	ur2 = ur1 * exp(sqrt(rdata_1.h__ * rdata_1.h__ / 2. * (potf_(&
		rdata_1.rmax) - *q)) + sqrt(rdata_1.h__ * rdata_1.h__ / 2. * (
		potf_(&d__1) - *q)));
    } else if (pdata_1.ktyp == 9) {
	ur2 = ur1 * exp(sqrt(rdata_1.h__ * rdata_1.h__ / 2. * (vmon_1.vnum[(
		integer) rdata_1.rmax - 1] - *q)) + sqrt(rdata_1.h__ * 
		rdata_1.h__ / 2. * (vmon_1.vnum[(integer) (rdata_1.rmax - 
		rdata_1.h__) - 1] - *q)));
    }
/* ---------------------------------------------------------------- */
/* qeng -- the input correction factor for energy convergence; */
/* kcon -- the input iteration  number for energy convergence. */
/*   now solve SE & get the numerical wavefunction */
/* ---------------------------------------------------------------- */
L10:
    solvese_((D_fp)potf_, q, &pdata_1.ktyp, &pdata_1.iel, &shomos_1.rmu, &
	    rdata_1.rmin, &rdata_1.rmatch, &rdata_1.rmax, &rdata_1.h__, &ul1, 
	    &ul2, &ur1, &ur2, &wronsk, &ecorr, &u[1]);
/* --- */
/* === Sun's modification === */
    if (kmon_1.kstd[*nu - 1] == 0) {
/* --- IMPORTANT :  Next line is ORIGINAL --- */
	eq1 = (d__1 = ecorr / *q, abs(d__1));
/* --- */
    } else if (kmon_1.kstd[*nu - 1] == 1) {
	eq1 = (d__1 = vcon_1.qeng[*nu - 1] * ecorr / *q, abs(d__1));
    } else if (kmon_1.kstd[*nu - 1] > 1) {
	eq1 = (d__1 = *q - q0, abs(d__1));
    }
    q0 = *q;
/* ---------------------------------------------------------------- */
    ++kv;
    ecor = ecorr;
/* --- */
    io___219.ciunit = *ku;
    s_wsfe(&io___219);
    i__1 = *nu - 1;
    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*q), (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&ecorr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&eq1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&kmon_1.kstd[*nu - 1], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&vcon_1.vtol[*nu - 1], (ftnlen)sizeof(doublereal));
    e_wsfe();
/*     IF ( eq1 .gt.  tol ) THEN */
    if (eq1 > vcon_1.vtol[*nu - 1] && kv <= vcon_1.kcon[*nu - 1]) {
/* --- */
	if (kmon_1.kstd[*nu - 1] >= 1) {
	    ecor = vcon_1.qeng[*nu - 1] * ecorr;
	}
	*q += ecor;
	qdif = *q - q0;
/* --- */
/*             qdif = Q - evib(nu) */
/*               Q1 = Q */
/* --- */
	if (qdif > 0. && ecor > 0.) {
	    *q = *q - ecor + ecor * vcon_1.dev[*nu - 1];
	} else if (qdif < 0. && ecor < 0.) {
	    *q = *q - ecor - ecor * vcon_1.dev[*nu - 1];
/* --- Next one produces degenerate states : */
/*               Q = Q - Ecor + Ecor*dev(nu) */

/* --- Next condition is unlikely TRUE : */
	} else if (qdif > 0. && ecor < 0.) {
	    *q = *q - ecor - ecor * vcon_1.dev[*nu - 1];
/* --- Next condition is unlikely TRUE : */
	} else if (qdif < 0. && ecor > 0.) {
	    *q = *q - ecor + ecor * vcon_1.dev[*nu - 1];
/*       write(28,*) "  v; Q1; Q; Ecor*dev = ",nu-1,Q1,Q,Ecor*dev(nu) */
/* --- */
	}
/* --- */
	goto L10;
/* --- */
    }
/* --- */
    s_wsfe(&io___221);
    i__1 = *nu - 1;
    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&wronsk, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&ecor, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&vibe_1.evib[*nu - 1], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&(*q), (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&eq1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&kmon_1.kstd[*nu - 1], (ftnlen)sizeof(integer));
    i__2 = kv - 1;
    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
    e_wsfe();
/* === End modification === */
/* --- */
    vibe_1.evib[*nu - 1] = *q;
/* ---------------------------------------------------------------- */
/* L90: */
/* --- */
/* ---------------------------------------------------------------- */
    return 0;
} /* boundv_ */



/* Subroutine */ int expectwf_(integer *kw, integer *nr, integer *nu, integer 
	*ng, integer *nbi, integer *nwv, doublereal *h__, doublereal *rmin, 
	doublereal *q, doublereal *u, doublereal *rmesh, doublereal *wf, 
	doublereal *expr)
{
    /* Format strings */
    static char fmt_60[] = "(x,\002From v = \002,i3,\002  to v' = \002,i3"
	    ",\002  with \002,i6,\002  points for each state \002,)";
    static char fmt_40[] = "(/x,\002The central SPHERICAL potentials are "
	    ":\002,//2x,\002The power of the SMS potential,  ms =\002,i3,//4x,"
	    "\002r(a0)  \002,4x,\002 Pot(r; a.u.)\002,/)";
    static char fmt_42[] = "(/x,\002The central SPHERICAL potentials are "
	    ":\002,//2x,\002The power of the SMS potential,  ms =\002,i3,//4x,"
	    "\002r(a0)  \002,4x,\002 Pot(r; cm-1)\002,/)";
    static char fmt_30[] = "(/x,\002For v = \002,i3,2x,\002 &  eigenvalue "
	    "E =\002,1d18.9,\002 a.u.\002,//4x,\002r(a0)  \002,3x,\002Wavefun"
	    "ction(r)\002,/)";
    static char fmt_32[] = "(/x,\002For v = \002,i3,2x,\002 &  eigenvalue "
	    "E =\002,1d18.9,\002 a.u.\002,/x,\002After adding constant,      "
	    "E =\002,1d18.9,\002 a.u.\002,/x,\002                            "
	    "E =\002,1f18.9,\002 cm-1\002,/x,\002Adding do NOT change wavefun"
	    "ctions \002,/,/4x,\002r(a0)  \002,3x,\002Wavefunction(r)\002,/)";
    static char fmt_50[] = "(f10.5,2x,1pe16.8)";
    static char fmt_52[] = "(f10.5,2x,f16.5)";
    static char fmt_44[] = "(/x,\002The DIFFERENCE of SPHERICAL potentials a"
	    "re :\002,//2x,\002The powers for TWO SMS potentials,  ms, ms-2 "
	    "=\002,2i3,//4x,\002r(a0)  \002,4x,\002 Pot(r; a.u.)\002,/)";
    static char fmt_70[] = "(/x,\002The central SPHERICAL potentials are add"
	    "ed \002,/2x,\002by the constant  Add = \002,f12.9,\002 a.u.\002,"
	    "/6x,\002[ Pot = Pot + Add ] \002,//4x,\002r(a0)  \002,4x,\002 Po"
	    "t(r; a.u.)\002,/)";
    static char fmt_80[] = "(/x,\002The central SPHERICAL potentials are add"
	    "ed \002,/2x,\002by the constant  Add = \002,f16.5,\002 cm-1\002,"
	    "/6x,\002[ Pot = Pot + Add ] \002,//4x,\002r(a0)  \002,4x,\002 Po"
	    "t(r; cm-1)\002,/)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer i__, j, k;
    static doublereal r__, q2;
    static integer nb, nj, nk;
    static doublereal ri;
    static integer nbb[20], nbf;
    static doublereal aucm;
    extern doublereal potf_(doublereal *);
    static doublereal potv[10000], temp0, tempv, expect;

    /* Fortran I/O blocks */
    static cilist io___228 = { 0, 9, 0, fmt_60, 0 };
    static cilist io___229 = { 0, 17, 0, fmt_40, 0 };
    static cilist io___230 = { 0, 18, 0, fmt_42, 0 };
    static cilist io___233 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___234 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___236 = { 0, 0, 0, fmt_32, 0 };
    static cilist io___238 = { 0, 0, 0, fmt_50, 0 };
    static cilist io___240 = { 0, 0, 0, fmt_50, 0 };
    static cilist io___241 = { 0, 17, 0, fmt_50, 0 };
    static cilist io___242 = { 0, 18, 0, fmt_52, 0 };
    static cilist io___246 = { 0, 15, 0, fmt_44, 0 };
    static cilist io___247 = { 0, 16, 0, fmt_40, 0 };
    static cilist io___248 = { 0, 17, 0, fmt_70, 0 };
    static cilist io___249 = { 0, 18, 0, fmt_80, 0 };
    static cilist io___251 = { 0, 15, 0, fmt_50, 0 };
    static cilist io___252 = { 0, 16, 0, fmt_50, 0 };
    static cilist io___254 = { 0, 17, 0, fmt_50, 0 };
    static cilist io___255 = { 0, 18, 0, fmt_52, 0 };


/* ---------------------------------------------------------------- */
/*   expectwf returns the vibrational wavefunction on a given */
/* r-mesh and the expectation value of r. */
/* Atomic units throughout (energies in hartrees, not in rydbergs!) */

/* Input: */
/*   kw -- specifies the unit to write wavefunctions. */
/*   nr -- is the total number of radius r points. */
/*   nu -- is the nu_th (vibrational) state with quantum number (nu-1). */
/*   ng -- is the number of r points on which the wavefunction is desired. */
/*  nwv -- write wavefunctions to unit 9  for v =< nwv. */
/*    Q -- the eigenvalue of the nu_th vibrational state. */
/*    h -- the step length of the radius. */
/* rmin -- the (input) minimum of the radius. */
/*    u -- the nu_th vibrational eigenvector. */
/* rmesh -- is the r (vector) points. */

/* Output: */
/*    wf -- is the vector of wavefunction on the rmesh */
/*  expr -- is the expectation value of r :  < u | r | u > */
/* ---------------------------------------------------------------- */
/* ---------------------------------------------------------------- */
/* Find the expectation value of r :  < v | r | v > */
/*           ri = (i-1)*h + rmin */
/* ---------------------------------------------------------------- */
    /* Parameter adjustments */
    --expr;
    --wf;
    --rmesh;
    --u;

    /* Function Body */
    aucm = 219474.63067;
    expect = 0.;
    nb = (*nwv - *nbi + 1) / 10 + 2;
    nbb[0] = *nbi;
    nbb[1] = *nbi + 9;
    if (nb >= 3) {
	i__1 = nb;
	for (i__ = 3; i__ <= i__1; ++i__) {
	    nbb[i__ - 1] = nbb[i__ - 2] + 10;
	}
    }
    i__1 = *nr - 1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	expect += *h__ * u[i__] * u[i__] * ((i__ - 1) * *h__ + *rmin);
    }
    expr[*nu] = expect + *h__ / 2. * (u[1] * u[1] + u[*nr] * u[*nr]);
/* ---------------------------------------------------------------- */
/* If use NUMERICAL potential, then write out E & wavefunctions */
/* ---------------------------------------------------------------- */
    if (pdata_1.ktyp == 9) {
	goto L10;
    }
/* ---------------------------------------------------------------- */
    if (*nu == nbb[0]) {
	nbf = nbb[1] - 2;
	if (nbf > *nwv - 1) {
	    nbf = *nwv - 1;
	}
	s_wsfe(&io___228);
	i__1 = nbb[0] - 1;
	do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&nbf, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&(*nr), (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___229);
	do_fio(&c__1, (char *)&pdata_1.ms, (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___230);
	do_fio(&c__1, (char *)&pdata_1.ms, (ftnlen)sizeof(integer));
	e_wsfe();
    }
/* --- */
    nk = 8;
    i__1 = nb;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (*nu == nbb[i__ - 1]) {
	    nj = nk + i__;
	    nbf = nbb[i__] - 2;
	    if (nbf > *nwv - 1) {
		nbf = *nwv - 1;
	    }
	    io___233.ciunit = nj;
	    s_wsfe(&io___233);
	    i__2 = nbb[i__ - 1] - 1;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbf, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*nr), (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    }
/* --- */
/* Write out eigenvalue : */
/* --- */
L10:
    if (pdata_1.add == 0.) {
	if (*kw <= 90) {
	    io___234.ciunit = *kw;
	    s_wsfe(&io___234);
	    i__1 = *nu - 1;
	    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*q), (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    } else {
	q2 = *q + pdata_1.add;
	if (*kw <= 90) {
	    io___236.ciunit = *kw;
	    s_wsfe(&io___236);
	    i__1 = *nu - 1;
	    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*q), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&q2, (ftnlen)sizeof(doublereal));
	    d__1 = q2 * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
/* - */
/* Write out eigenfunction : */
/* - */
    i__1 = *nr;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ri = *rmin + (i__ - 1) * *h__;
	if (*kw <= 90) {
	    io___238.ciunit = *kw;
	    s_wsfe(&io___238);
	    do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&u[i__], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* --- */
	if (pdata_1.ktyp == 9) {
	    goto L15;
	}
/* --- */
	i__2 = nb - 1;
	for (k = 1; k <= i__2; ++k) {
	    if (*nu >= nbb[k - 1] && *nu < nbb[k]) {
		nj = nk + k;
		if (*nu <= *nwv + 1) {
		    io___240.ciunit = nj;
		    s_wsfe(&io___240);
		    do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&u[i__], (ftnlen)sizeof(doublereal))
			    ;
		    e_wsfe();
		}
	    }
	}
/* - */
	if (*nu == *nbi) {
	    s_wsfe(&io___241);
	    do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
	    d__1 = potf_(&ri);
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___242);
	    do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
	    d__1 = potf_(&ri) * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    potv[i__ - 1] = potf_(&ri);
	}
L15:
	;
    }
    if (pdata_1.ktyp == 9) {
	goto L20;
    }
/* ---------------------------------------------------------------- */
/*   Now using linear interpolation to get wavefunction at rmesh */
/* points. You can get more sophisticated if needed. */
/* ---------------------------------------------------------------- */
    i__1 = *ng;
    for (j = 1; j <= i__1; ++j) {
	r__ = rmesh[j];
	i__ = (integer) ((r__ - *rmin) / *h__ + 1);
	ri = *rmin + (i__ - 1) * *h__;
	wf[j] = u[i__] + (u[i__ + 1] - u[i__]) * (r__ - ri) / *h__;
    }
/* ---------------------------------------------------------------- */
    if (*nu == *nbi) {
	s_wsfe(&io___246);
	do_fio(&c__1, (char *)&pdata_1.ms, (ftnlen)sizeof(integer));
	i__1 = pdata_1.ms - 2;
	do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___247);
	i__1 = pdata_1.ms - 2;
	do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	e_wsfe();
	pdata_1.ms += -2;
	if (pdata_1.add > 0.) {
	    s_wsfe(&io___248);
	    do_fio(&c__1, (char *)&pdata_1.add, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___249);
	    d__1 = pdata_1.add * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* - */
/* Write out potential difference & potential : */
/* - */
	i__1 = *nr;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ri = *rmin + (i__ - 1) * *h__;
	    temp0 = potv[i__ - 1] - potf_(&ri);
	    s_wsfe(&io___251);
	    do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&temp0, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___252);
	    do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
	    d__1 = potf_(&ri);
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    if (pdata_1.add > 0.) {
		tempv = potv[i__ - 1] + pdata_1.add;
		s_wsfe(&io___254);
		do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&tempv, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___255);
		do_fio(&c__1, (char *)&ri, (ftnlen)sizeof(doublereal));
		d__1 = tempv * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    }
	}
	pdata_1.ms += 2;
/*       write(15,*) '  ms, ms-2 =',ms, ms-2 */
    }
/* ---------------------------------------------------------------- */
L20:
/* ---------------------------------------------------------------- */
/* L35: */
/* ---------------------------------------------------------------- */
    return 0;
} /* expectwf_ */


/* = */
/* Subroutine */ int solvese_(D_fp v, doublereal *q, integer *ktyp, integer *
	iel, doublereal *rmu, doublereal *rmin, doublereal *rmatch, 
	doublereal *rmax, doublereal *h__, doublereal *ul1, doublereal *ul2, 
	doublereal *ur1, doublereal *ur2, doublereal *wronsk, doublereal *
	ecorr, doublereal *u)
{
    /* Initialized data */

    static doublereal one = 1.;
    static doublereal two = 2.;
    static doublereal three = 3.;
    static doublereal four = 4.;
    static doublereal ten = 10.;
    static doublereal twelve = 12.;

    /* Format strings */
    static char fmt_20[] = "(\002Need to increase parameter ''nrmax'' i"
	    "n\002,\002 subroutine solvese \002)";

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void);
    /* Subroutine */ int s_stop(char *, ftnlen);
    double sqrt(doublereal);

    /* Local variables */
    static integer i__;
    static doublereal r__, t[20000];
    static integer ml, mr;
    static doublereal ul[20000], ur[20000], a1l, a2l, b1l, b2l, a1r, a2r, b1r,
	     b2r, scale, anorm;
    static integer nrmesh;
    static doublereal ulprime, anormsq, urprime;

    /* Fortran I/O blocks */
    static cilist io___265 = { 0, 6, 0, fmt_20, 0 };


/*    &       rmax, h, ul1, ul2, ur1, ur2, wronsk, Ecorr, u ) */
/* --------------------------------------------------------------------- */
/*  Solves the radial Schrodinger Equation for a GIVEN ENERGY in a */
/*  given potential, returning the wavefunction and an energy */
/*  correction term indicating how close the input energy is to an */
/*  eigenvalue. */
/* --------------------------------------------------------------------- */
/*  Method: */
/*    Uses the Numerov method for propagating the wavefunction out */
/*    from small r and in from large r on a given (equally spaced) */
/*    mesh.  These waefunctions are matched together at some */
/*    intermediate r.  If they match smoothly, i.e if their */
/*    derivatives are the same at the matching radius, then the */
/*    input energy is an eigenvalue and the returned wavefunction */
/*    is the corresponding eigenfunction. */
/* --------------------------------------------------------------------- */
/*  Input: */

/*    V          -  An EXTERNAL DOUBLE PRECISION FUNCTION taking one */
/*                  argument, the radius, which returns the potential, */
/*                  in Hartrees, at that radius.  If other parameters */
/*                  are needed for the potential, they should be */
/*                  provided by a common block.  (double precision) */

/*    Q          -  The INPUT energy, in Hartrees.  (double precision) */

/*    iel        -  The angular momentum quantum number.  (integer) */

/*    rmu        -  The reduced mass, in atomic units.  (double */
/*                  precision) */

/*    rmin       -  The minimum radius for the mesh.  (double precision) */

/*    rmatch     -  The matching radius.  This value should be in the */
/*                  classically allowed region.  (double precision) */

/*    rmax       -  The maximum radius for the mesh.  (double precision) */

/*    h          -  The stepsize for the mesh.  (double precision) */

/*    ul1, ul2   -  The values of the "left" wavefunction (to be */
/*                  propagated outward) at the first and second rmesh */
/*                  points.  (double precision) */

/*    ur1, ur2   -  The values of the "right" wavefunction (to be */
/*                  propagated inward) at the last and next to last */
/*                  rmesh points.  (double precision) */
/* --------------------------------------------------------------------- */
/*  Output: */

/*    wronsk     -  An un-normalized wronskian of the left and right */
/*                  wavefunctions.  (double precision) */
/*                    wronsk = 0  means the derivatives of LEFT & */
/*                  RIGHT wavefunctions are equal and therefore the */
/*                  whole EIGENfunction is SMOOTH. */

/*    Ecorr      -  The estimated correction to the input energy Q. */
/*                  (double precision) */

/*    u          -  The normalized wavefunction on the input rmesh. */
/*                  u(1) holds u at rmin, u(2) holds u at rmin+h, */
/*                  etc.  (double precision) */
/* --------------------------------------------------------------------- */
/*  Notes: */

/*    -  All values are in atomic units:  radii in bohr, energies in */
/*       Hartrees.  also hbar=1. */

/*    -  The values of rmatch and rmax might be changed slightly to */
/*       ensure that they are on the mesh.  rmin will not be changed. */

/*    -  Ecorr is actually the wronskian of the two propagated */
/*       *****                 ********* */
/*       normalized wavefunctions scaled by some constants.  That */
/*       this value may actually be interpreted as an energy correction */
/*       depends on the calculated wavefunction being "close enough" */
/*       to the correct eigenfunction.  This condition is not always */
/*       met in practice, so some care must be used.  It is true, */
/*                           ********************** */
/*       however, that when the wronskian is zero, the input energy */
/*       is an eigenvalue.  Near an eigenvalue, (TRUE) wronsk varies */
/*                          **************************************** */
/*       smoothly with the input energy, so you can bracket the */
/*       ****************************** */
/*       eigenvalue with this information. */

/*    -  Take care not to set rmin exactly equal to zero!  The */
/*                 ************************************* */
/*       centrifugal term WILL blow up.  Use a very small value of */
/*       r instead. */

/*    -  The notation used in the code is taken directly from the */
/*       excellent reference "Practical Points Concerning the Solution */
/*       of the Schrodinger Equation" by John M. Blatt, Journal of */
/*       Computational Physics, v.1  pp 382-396 (1967).  Not all of */
/*       the suggestions in the paper are implemented in this code. */

/*    -  This code was written with clarity more in mind than */
/*       efficiency. */
/* --------------------------------------------------------------------- */
/*  nrmax -- the maximum number of rmesh points allowed. */

    /* Parameter adjustments */
    --u;

    /* Function Body */
/* --------------------------------------------------------------------- */
/*        IMPORTANT variables: */
/*  u() holds the wavefunction on the rmesh */
/*  uL() holds the left wavefunction on a left mesh */
/*  mL is the index of the matching radius for uL */
/*  uR() holds the right wavefunction on a right mesh */
/*  mR is the index of the matching radius for uR */
/*  T() is used in the Numerov propagation; calculated on the rmesh */
/* --------------------------------------------------------------------- */
/*  define the internal Numerov f function  f(r)==Tnumerov(r) */
/* --------------------------------------------------------------------- */
/* --------------------------------------------------------------------- */
/*  make sure match will be clean.  "round up" rmax if necessary, */
/*  put nrmesh to an odd number to make integration convenient */
/* --------------------------------------------------------------------- */
/*       nrmesh = (rmax - rmin)/h  + 1 */

    nrmesh = (integer) ((integer) (*rmax - *rmin) / *h__ + 1);
    if (*rmin + *h__ * (nrmesh + 1) < *rmax) {
	++nrmesh;
    }
    if (nrmesh % 2 == 0) {
	++nrmesh;
    }
/*     mL = (rmatch - rmin)/h + 1.5 */
    ml = (integer) ((*rmatch - *rmin) / *h__ + 1.5);
    mr = nrmesh + 1 - ml;
    *rmatch = *rmin + (ml - 1) * *h__;
/* --------------------------------------------------------------------- */
    if (nrmesh > 20000) {
	s_wsfe(&io___265);
	e_wsfe();
	s_stop("", (ftnlen)0);
    }
/* --- */
/* --------------------------------------------------------------------- */
/*  get T(r) on mesh */
/* --------------------------------------------------------------------- */
    if (*ktyp < 9) {
	i__1 = nrmesh;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    d__1 = *h__ * (i__ - 1) + *rmin;
	    t[i__ - 1] = *h__ * *h__ / twelve * (two * *rmu * ((*v)(&d__1) - *
		    q) + *iel * (*iel + 1) / (d__1 * d__1));
	}
    } else if (*ktyp == 9) {
	i__1 = nrmesh;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    r__ = *h__ * (i__ - 1) + *rmin;
	    t[i__ - 1] = *h__ * *h__ / twelve * (two * *rmu * (vmon_1.vnum[
		    i__ - 1] - *q) + *iel * (*iel + 1) / (r__ * r__));
	}
    }
/* --------------------------------------------------------------------- */
/*  propagate uL  (Numerov method) */
/* --------------------------------------------------------------------- */
/* L50: */
    ul[0] = *ul1;
    ul[1] = *ul2;
    i__1 = ml + 2;
    for (i__ = 3; i__ <= i__1; ++i__) {
	ul[i__ - 1] = ((two + ten * t[i__ - 2]) * ul[i__ - 2] - (one - t[i__ 
		- 3]) * ul[i__ - 3]) / (one - t[i__ - 1]);
    }
/*  same for uR */
    ur[0] = *ur1;
    ur[1] = *ur2;
    i__1 = mr + 2;
    for (i__ = 3; i__ <= i__1; ++i__) {
	ur[i__ - 1] = ((two + ten * t[nrmesh - i__ + 1]) * ur[i__ - 2] - (one 
		- t[nrmesh - i__ + 2]) * ur[i__ - 3]) / (one - t[nrmesh - i__]
		);
    }
/* --------------------------------------------------------------------- */
/*  scale uR */
/* --------------------------------------------------------------------- */
    scale = ul[ml - 1] / ur[mr - 1];
    i__1 = mr + 2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ur[i__ - 1] *= scale;
    }
/*  copy uL and uR onto u */
    i__1 = ml;
    for (i__ = 1; i__ <= i__1; ++i__) {
	u[i__] = ul[i__ - 1];
    }
    i__1 = mr - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	u[nrmesh + 1 - i__] = ur[i__ - 1];
    }
/* --------------------------------------------------------------------- */
/*  normalize, using Simpson's rule for integration */
/* --------------------------------------------------------------------- */
/* Computing 2nd power */
    d__1 = u[1];
/* Computing 2nd power */
    d__2 = u[nrmesh];
    anormsq = d__1 * d__1 + d__2 * d__2;
    i__1 = nrmesh - 1;
    for (i__ = 2; i__ <= i__1; i__ += 2) {
/* Computing 2nd power */
	d__1 = u[i__];
	anormsq += four * (d__1 * d__1);
    }
    i__1 = nrmesh - 2;
    for (i__ = 3; i__ <= i__1; i__ += 2) {
/* Computing 2nd power */
	d__1 = u[i__];
	anormsq += two * (d__1 * d__1);
    }
    anormsq = anormsq * *h__ / three;
    anorm = one / sqrt(anormsq);
    i__1 = nrmesh;
    for (i__ = 1; i__ <= i__1; ++i__) {
	u[i__] *= anorm;
    }
/*  will need these for derivatives */
    for (i__ = -2; i__ <= 2; ++i__) {
	ur[mr + i__ - 1] *= anorm;
	ul[ml + i__ - 1] *= anorm;
    }
/* --------------------------------------------------------------------- */
/*  get derivatives */
/* --------------------------------------------------------------------- */
    a1l = (ul[ml] - ul[ml - 2]) / two;
    a2l = (ul[ml + 1] - ul[ml - 3]) / two;
    a1r = (ur[mr - 2] - ur[mr]) / two;
    a2r = (ur[mr - 3] - ur[mr + 1]) / two;
    b1l = t[ml] * ul[ml] - t[ml - 2] * ul[ml - 2];
    b2l = t[ml + 1] * ul[ml + 1] - t[ml - 3] * ul[ml - 3];
    b1r = t[ml] * ur[mr - 2] - t[ml - 2] * ur[mr];
    b2r = t[ml + 1] * ur[mr - 3] - t[ml - 3] * ur[mr + 1];
    ulprime = 16. / (*h__ * 21.) * (-a1l + a2l * 37. / 32. - b1l * 37. / 5. - 
	    b2l * 17. / 40.);
    urprime = 16. / (*h__ * 21.) * (-a1r + a2r * 37. / 32. - b1r * 37. / 5. - 
	    b2r * 17. / 40.);
    *wronsk = ulprime - urprime;
    *ecorr = ul[ml - 1] * (ulprime - urprime) / (two * *rmu);
/* --------------------------------------------------------------------- */
    return 0;
} /* solvese_ */



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



doublereal potf_(doublereal *r__)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1, d__2;

    /* Builtin functions */
    double exp(doublereal), pow_di(doublereal *, integer *), sqrt(doublereal);

    /* Local variables */
    static doublereal vrydberg, d__;
    static integer n;
    static doublereal z__, f2, ak, v00, fy, xx, yy, pf1, pf2, yy0, vpg, alph, 
	    temp, pbeta, ralph, vmorse;

/* ----------------------------------------------------------------------- */
/*   This function is called to calculate a spherical potential of : */

/*  1.) (if k=1) Gaussion type or of Slater type ; */
/*  2.) (if k=2) Spherical central potential V0*exp(-alpha*x) ; */
/*  3.) (if k=3) Simple harmonic potential   V(r)=0.5d0*ak*r**2 ; */
/*                      where  r=x-re        for ktyp=3 . */
/*  4.) (if k=4) Morse potential  V(r)=de[exp(-2*a*r) - 2*exp(-a*r)] + de ; */
/*                      where  r=(x-re)/re   for ktyp=4 . */
/*  5.) (if k=5) Screened central potential  V(r)=-Z*dexp(-r/D)/r */

/*  6.) (if k=6) Sun-Murrell-Sorbie potential (ktyp=5;  r=x-re) : */
/*               temp = 1/beta + a1*r + a2*r**2 + a3*r**3 + */
/*                               a4*r**4 + a5*r**5 */
/*               V_MS(r) = - De*beta*temp*exp(-a1*beta*r) */

/*  7.) (if k=7) Huxley-Murrell-Sorbie */
/*               V_MS(x)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x) */
/*               where x = R - Re */

/*  8.) (if k=8) (if Isf=1) SF potential . V = (lamta+1)*V_MS - lamta*V_Morse . */
/* ----------------------------------------------------------------------- */
    yy = 0.;
    if (pdata_1.ktyp == 1) {
	i__1 = pdata_1.nvp;
	for (n = 1; n <= i__1; ++n) {
	    yy += pdata_1.vc[n - 1] * pow_di(r__, &pdata_1.nv[n - 1]) * exp(
		    -pdata_1.xl[n - 1] * *r__ * *r__);
/* L15: */
	}
    } else if (pdata_1.ktyp == 2) {
	i__1 = pdata_1.nvp;
	for (n = 1; n <= i__1; ++n) {
	    yy += pdata_1.vc[n - 1] * pow_di(r__, &pdata_1.nv[n - 1]) * exp(
		    -pdata_1.xl[n - 1] * *r__);
/* L25: */
	}
    } else if (pdata_1.ktyp == 3) {
	xx = *r__ - shomos_1.re;
/* Computing 2nd power */
	d__1 = shomos_1.we;
	ak = shomos_1.rmu * (d__1 * d__1);
/* Computing 2nd power */
	d__1 = xx;
	yy += ak * .5 * (d__1 * d__1);
    } else if (pdata_1.ktyp == 4) {
	xx = (*r__ - shomos_1.re) / shomos_1.re;
	temp = exp(shomos_1.alpha * -2. * xx) - exp(-shomos_1.alpha * xx) * 
		2.;
	yy = yy + shomos_1.de * temp + shomos_1.de;
    } else if (pdata_1.ktyp == 5) {
	xx = *r__ - shomos_1.re;
	yy -= z__ * exp(-xx / d__) / xx;
    } else if (pdata_1.ktyp >= 6 && pdata_1.ktyp <= 8) {
	xx = *r__ - shomos_1.re;
	if (pdata_1.ktyp == 6) {
/* Computing 2nd power */
	    d__1 = xx;
/* Computing 3rd power */
	    d__2 = xx;
	    temp = 1. / murrell_1.beta + sf_1.an[0] * xx + sf_1.an[1] * (d__1 
		    * d__1) + sf_1.an[2] * (d__2 * (d__2 * d__2));
	} else {
/* Computing 2nd power */
	    d__1 = xx;
/* Computing 3rd power */
	    d__2 = xx;
	    temp = sf_1.an[0] * xx + 1. + sf_1.an[1] * (d__1 * d__1) + 
		    sf_1.an[2] * (d__2 * (d__2 * d__2));
	}
	if (pdata_1.ms >= 4) {
/* Computing 4th power */
	    d__1 = xx, d__1 *= d__1;
	    temp += sf_1.an[3] * (d__1 * d__1);
	} else if (pdata_1.ms >= 5) {
/* Computing 5th power */
	    d__1 = xx, d__2 = d__1, d__1 *= d__1;
	    temp += sf_1.an[4] * (d__2 * (d__1 * d__1));
	}
/* - */
	if (pdata_1.ktyp == 6) {
	    yy = yy - murrell_1.de0 * murrell_1.beta * temp * exp(-sf_1.an[0] 
		    * murrell_1.beta * xx) + murrell_1.de0;
	    goto L200;
	} else {
	    yy0 = yy - murrell_1.de0 * temp * exp(-sf_1.an[0] * xx);
	    yy = yy0 + murrell_1.de0;
	    if (pdata_1.ktyp == 7) {
		goto L200;
	    }
	}
/* - */
	f2 = sf_1.amu * shomos_1.we * shomos_1.we;
/* --- morse pot */
	if (sf_1.nturn <= 1) {
	    alph = sqrt(f2 * .5 / murrell_1.de0);
	    vmorse = murrell_1.de0 * (exp(alph * -2. * xx) - exp(-alph * xx) *
		     2.);
	    v00 = vmorse;
/* --- rydberg pot */
	} else if (sf_1.nturn == 2) {
	    ralph = sqrt(f2 / murrell_1.de0);
	    vrydberg = -murrell_1.de0 * (ralph * xx + 1.) * exp(-ralph * xx);
	    v00 = vrydberg;
/* --- pseudo-gaussion pot */
	} else if (sf_1.nturn == 3) {
	    pbeta = sqrt(f2 * shomos_1.re * shomos_1.re / murrell_1.de0 + 4.) 
		    * .5 - 1.;
	    pf1 = 1. - shomos_1.re / *r__ * (shomos_1.re / *r__);
	    pf2 = 1. - *r__ / shomos_1.re * (*r__ / shomos_1.re);
	    vpg = -murrell_1.de0 * (pbeta * pf1 + 1.) * exp(pbeta * pf2);
	    v00 = vpg;
	}

/* --- fy is the force-field variational function  lamda(r) */
	fy = xx / *r__;
/* Computing 2nd power */
	d__1 = sf_1.alamta;
	fy *= 1. - exp(-xx / shomos_1.re * (d__1 * d__1));
	fy *= sf_1.alamta;
	yy = (fy + 1.) * yy0 - fy * v00;
/* - */
	if (*r__ < shomos_1.re) {
	    if (sf_1.nturn == 0) {
		yy = yy0;
	    } else {
		yy = v00;
	    }
	}
	yy += murrell_1.de0;
    }
/* ---------------------------------------------------- */
L200:
    ret_val = yy;
    return ret_val;
} /* potf_ */

