/* ../src/vibdm.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal a[10];
} ms1_;

#define ms1_1 ms1_

struct {
    doublereal de, beta, alamta, re;
    integer msf, ims, nturn;
} ms2_;

#define ms2_1 ms2_

struct {
    doublereal amu, rbohr, const__, pi;
} bdt_;

#define bdt_1 bdt_

struct {
    doublereal we, dale, st1i, st1f;
    integer ntot, ncc1;
} mol_;

#define mol_1 mol_

struct {
    integer mset, mcst, mv, nvs[30];
} la1_;

#define la1_1 la1_

/* Table of constant values */

static integer c__1 = 1;
static integer c__96 = 96;
static integer c__30 = 30;
static integer c__0 = 0;
static integer c__9 = 9;
static integer c__20000 = 20000;
static integer c_b56 = 2000000;
static integer c__3 = 3;
static integer c__5 = 5;
static doublereal c_b180 = 1.0001;
static doublereal c_b181 = 2.0001;
static doublereal c_b182 = 2.;
static integer c__2 = 2;
static integer c__4 = 4;

/*    program vibdim */
/* ======================================================================== */
/*                Modified by Feng Hao.        07/10/1998 */
/*                Modified by Weiguo  Sun.     11/08/1998 */
/*    To calculate the bound vibrational energies & wavefunctions of */
/*  diatomic potential : */

/* 1.) Sun's Modified Murrell-Sorbie (x=R-Re) : */
/*           V_SMMS(R)=-De*beta*(1/beta + a1*x + a2*x**2 */
/*                                 + a3*x**3 + ...)*exp(-a1*beta*x) */
/*    which is NOT used here. */

/* 2.) Huxley-Murrell-Sorbie (x=R-Re) : */
/*        V_MS(R)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x) */

/* 3.) SF (ECM) : V_ecm(R) = V_MS + Lamta(R)*delta_V(R) */
/*          V_MS:  Murrell & Sorbie potential in which the */
/*                 (a1,a2,a3) are calculated using SF formulae. */
/*          Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)] */
/*          delta_V(R) = V_MS - V_0 */
/*                                For  R < Re : */
/*   Nturn = 0, V_0 = V_Morse   , V_ecm(R) = V_ecm; */
/*         = 1, V_0 = V_Morse   , V_ecm(R) = V_MS ; */
/*         = 2, V_0 = V_Morse   , V_ecm(R) = V_0  ; */
/*         = 3, V_0 = V_Rydberg , V_ecm(R) = V_0  ; */
/*         = 4, V_0 = V_PG      , V_ecm(R) = V_0  . */

/*    If you want to use the other potential , please modified the */
/*  corresponding cofficients and the FUNCTION POT. */
/* ======================================================================== */
/* Main program */ int MAIN__(void)
{
    /* Format strings */
    static char fmt_300[] = "(//26x,\002=====  Output energies  =====\002,//"
	    "5x,\002i\002,9x,\002Ev(i)\002,9x,\002Ev(i)-Ev(1)\002,7x,\002Ev(i"
	    ")-Ev(i-1)\002,6x,\002Ev(i)+De\002,/)";
    static char fmt_310[] = "(/3x,\002Wavefunctions for v =\002,i3,5x,\002E_"
	    "v = \002,f16.8,//9x,\002R(ao)\002,12x,\002Phi_v(R)\002,/)";
    static char fmt_320[] = "(f15.8,3x,e20.10)";
    static char fmt_330[] = "(///11x,\002***  Statistical calculations on mo"
	    "lecular\002,\002 constants  ***  : \002,//20x,\002 Ave -- mean ("
	    "average) value of data; \002,/20x,\002Adev -- average deviation "
	    "of data; \002,/20x,\002Sdev -- standard deviation of data; \002,"
	    "/20x,\002 Var -- variance of data; \002,/20x,\002Skew -- skewnes"
	    "s of data; \002,/20x,\002Curt -- kurtosis of data; \002,//16x"
	    ",\002{     We, WeXe, WeYe, WeZe, WeSe, WeTe, ...  }\002,/16x,"
	    "\002{ i =  1,   2,    3,    4,    5,    6,  ...  }\002,///2x,"
	    "\002 i\002,8x,\002Ave\002,11x,\002Adev\002,8x,\002Sdev\002,9x"
	    ",\002Var\002,8x,\002Skew\002,8x,\002Curt\002,/)";
    static char fmt_340[] = "(2x,i2,1e17.9,5e12.5)";
    static char fmt_350[] = "(//5x,\002===  Average value of vibrational con"
	    "stants\002,\002 ===\002,//6x,\002i\002,6x,\002Const(a.u.)\002,6x,"
	    "\002Const(cm-1)\002/)";
    static char fmt_380[] = "(5x,i2,2(1pe18.9))";
    static char fmt_370[] = "(//5x,\002***  Vibrational constants from data "
	    "set\002,i3,\002  ***\002,//6x,\002i\002,6x,\002Const(a.u.)\002,6"
	    "x,\002Const(cm-1)\002/)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void), do_fio(integer *, char *, ftnlen);
    double pow_di(doublereal *, integer *);

    /* Local variables */
    static integer i__, j, k;
    static doublereal z__[2000000]	/* was [20000][100] */;
    extern /* Subroutine */ int linersolv_(doublereal *, integer *, 
	    doublereal *, integer *, integer *, doublereal *);
    static doublereal ax[9216]	/* was [96][96] */, ev[100], ex[2880]	/* 
	    was [30][96] */;
    static integer iv;
    static doublereal sv[30], st1[20000], ave, var, adev, aucm, sdev, skew, 
	    curt, xvar0[30];
    static integer ifail;
    static doublereal xadev[30], xmean[30], xsdev[30], xskew[30], xcurt[30];
    extern /* Subroutine */ int vibdim_(integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *), moment_(doublereal *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *), readata_(void);

    /* Fortran I/O blocks */
    static cilist io___2 = { 0, 10, 0, fmt_300, 0 };
    static cilist io___8 = { 0, 0, 0, fmt_310, 0 };
    static cilist io___10 = { 0, 0, 0, fmt_320, 0 };
    static cilist io___28 = { 0, 10, 0, fmt_330, 0 };
    static cilist io___29 = { 0, 10, 0, fmt_340, 0 };
    static cilist io___30 = { 0, 10, 0, fmt_350, 0 };
    static cilist io___31 = { 0, 10, 0, fmt_380, 0 };
    static cilist io___32 = { 0, 10, 0, fmt_370, 0 };
    static cilist io___33 = { 0, 10, 0, fmt_380, 0 };


/* ------------------------------------------------- */
/* Read data and calculate reduced mass  AMU */
/* ------------------------------------------------- */
    aucm = 219474.63067;
    readata_();
/* ------------------------------------------------- */
/* Prepare head for output energies */
/* ------------------------------------------------- */
    s_wsfe(&io___2);
    e_wsfe();
/* ------------------------------------------------------------- */
/*   VIBDIM calls RNORM which uses renormalized Numerov method */
/* to solve a 2nd order radial differential (eigenvalue) */
/* equation for a given potential array V(R). */
/*   st1 -- 1-D array containning radial points R's. */
/*    Z  -- 2-D array containning vibrational eigenfunctions. */
/*    EV -- 1-D array containning vibrational eigenvalues. */
/* ------------------------------------------------------------- */
    vibdim_(&mol_1.ntot, &mol_1.dale, &mol_1.ncc1, &mol_1.st1i, &mol_1.st1f, &
	    ifail, ev, z__, st1, &ms2_1.de);
/* ------------------------------------------------------------- */
/*     These are the wavefunctions. v = 0   ---> fort.30 */
/*                                  v = 1   ---> fort.31 */
/*                                  ...     ...  ... */
/* ------------------------------------------------------------- */
    i__1 = mol_1.ntot;
    for (i__ = 1; i__ <= i__1; ++i__) {
	io___8.ciunit = i__ + 29;
	s_wsfe(&io___8);
	i__2 = i__ - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__2 = mol_1.ncc1;
	for (j = 1; j <= i__2; ++j) {
	    io___10.ciunit = i__ + 29;
	    s_wsfe(&io___10);
	    do_fio(&c__1, (char *)&st1[j - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&z__[j + i__ * 20000 - 20001], (ftnlen)
		    sizeof(doublereal));
	    e_wsfe();
/* L40: */
	}
/* L50: */
    }
/* ------------------------------------------------------------- */
/* Sun's modification */

/*   Generate the coefficients matrix ax(nv,nv) according to */
/* Ev = We(v+1/2) + WeXe(v+1/2)**2 + WeYe(v+1/2)**3 + ... */
/* for (nv = nve+1) states : */
/*         v = 0, 1, 2, 3, 4, 5, ..., 10, ..., mv */
/*  mv =<  96 ;  ==> v_max = mv - 1 = 95. */
/* ------------------------------------------------------------- */
    i__1 = la1_1.mv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iv = i__ - 1;
	i__2 = la1_1.mv;
	for (k = 1; k <= i__2; ++k) {
	    d__1 = iv + .5;
	    ax[i__ + k * 96 - 97] = pow_di(&d__1, &k);
	}
/* L60: */
    }
/* ------------------------------------------------------------- */
/*   Solve linear equation    ax * X = bx   to find the vector */
/* X = (We, WeXe, WeYe, WeZe, WeSe, WeTe, ...) */
/*   When returns,  ex contains mset bx (solution vector X). */
/* ------------------------------------------------------------- */
    linersolv_(ax, &c__96, ex, &c__30, &c__96, ev);
/* ------------------------------------------------------------- */
/* Cal. statistical data for every element of vector X : */
/*    X = ( We, WeXe, WeYe, WeZe, WeSe, WeTe, ... ) */
/*    i =    1,   2,    3,    4,    5,    6,  ... */
/* ex(k,i) -- the ith element generated from the kth data set. */

/*   Given an array of sv(n), moment returns its mean ave, */
/* average deviation adev, standard deviation sdev, variance */
/* var, skewness skew, and kurtosis curt. */
/* ------------------------------------------------------------- */
    i__1 = la1_1.mcst;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = la1_1.mset;
	for (k = 1; k <= i__2; ++k) {
	    sv[k - 1] = ex[k + i__ * 30 - 31];
	}
	moment_(sv, &la1_1.mset, &c__30, &ave, &adev, &sdev, &var, &skew, &
		curt);
	xmean[i__ - 1] = ave;
	xadev[i__ - 1] = adev;
	xsdev[i__ - 1] = sdev;
	xvar0[i__ - 1] = var;
	xskew[i__ - 1] = skew;
	xcurt[i__ - 1] = curt;
/* L70: */
    }
/* --- */
    s_wsfe(&io___28);
    e_wsfe();
    i__1 = la1_1.mcst;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_wsfe(&io___29);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&xmean[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xadev[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xsdev[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xvar0[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xskew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xcurt[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L80: */
    }
/* --- */
    s_wsfe(&io___30);
    e_wsfe();
    i__1 = la1_1.mcst;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_wsfe(&io___31);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&xmean[i__ - 1], (ftnlen)sizeof(doublereal));
	d__1 = xmean[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L90: */
    }
/* --- */
    i__1 = la1_1.mset;
    for (k = 1; k <= i__1; ++k) {
	s_wsfe(&io___32);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	e_wsfe();
	i__2 = la1_1.mcst;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    s_wsfe(&io___33);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ex[k + i__ * 30 - 31], (ftnlen)sizeof(
		    doublereal));
	    d__1 = ex[k + i__ * 30 - 31] * aucm;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* L100: */
    }

/* End modification */
/* ------------------------------------------------------------- */
/* ------------------------------------------------------------- */
    return 0;
} /* MAIN__ */

/* Subroutine */ int vibdim_(integer *ntot, doublereal *dale, integer *ncc1, 
	doublereal *st1i, doublereal *st1f, integer *ifail, doublereal *ev, 
	doublereal *z__, doublereal *st1, doublereal *de)
{
    /* Format strings */
    static char fmt_8[] = "(1x,f16.10)";
    static char fmt_7[] = "(1x,i5,4e18.10)";

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void), s_wsue(cilist *), do_uio(integer *, char *, ftnlen),
	     e_wsue(void), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen)
	    , e_wsfe(void);

    /* Local variables */
    static doublereal a, d__[100];
    static integer i__;
    static doublereal x, v1[20000], al1, hh1, eps;
    extern doublereal pot_(doublereal *);
    extern /* Subroutine */ int rnorm_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *);

    /* Fortran I/O blocks */
    static cilist io___42 = { 0, 10, 0, 0, 0 };
    static cilist io___43 = { 0, 8, 0, 0, 0 };
    static cilist io___44 = { 0, 7, 0, fmt_8, 0 };
    static cilist io___45 = { 0, 23, 0, fmt_8, 0 };
    static cilist io___46 = { 0, 10, 0, fmt_7, 0 };


/* --- */
/*     DIMENSION EV(MHEGR),D(MHEGR),E(MHEGR),Z(MRN,MHEGR) */
/* ------------------------------------------------------------- */
    /* Parameter adjustments */
    --st1;
    z__ -= 20001;
    --ev;

    /* Function Body */
    if (*ncc1 % 2 == 0) {
	++(*ncc1);
    }
    *ifail = 0;
    al1 = 1 / bdt_1.amu;
    hh1 = (*st1f - *st1i) / (*ncc1 - 1);
    st1[1] = *st1i;
    i__1 = *ncc1;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L1: */
	st1[i__] = st1[i__ - 1] + hh1;
    }
    i__1 = *ncc1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x = st1[i__];
/* L3: */
	v1[i__ - 1] = pot_(&x);
    }
/* ---------------------------------------- */
/* -- CONST =1 Hartree = 219474.63067 cm-1 */
/* --   Change DALE into Hartree : */
/* ---------------------------------------- */
    *dale /= bdt_1.const__;
/* ------------------------------------------------------------- */
/*  RNORM - Using renormalized Numerov method to solve a 2nd */
/*          order radial differential (eigenvalue) equation */
/*          for a given potential array V1. */
/* ------------------------------------------------------------- */
    rnorm_(ncc1, ntot, &hh1, &ev[1], v1, &eps, dale, &c__0, &st1[1], &al1, &a,
	     d__, &z__[20001], ifail);
/* ---------------------------------------- */
    if (*ifail == -1) {
	s_wsle(&io___42);
	do_lio(&c__9, &c__1, "   rnorm  failed !", (ftnlen)18);
	e_wsle();
	return 0;
    }
/*     write(10,*)'   ev',' NTOT=',NTOT */
    s_wsue(&io___43);
    do_uio(&c__1, (char *)&(*ncc1), (ftnlen)sizeof(integer));
    do_uio(&c__1, (char *)&(*ntot), (ftnlen)sizeof(integer));
    do_uio(&c__20000, (char *)&st1[1], (ftnlen)sizeof(doublereal));
    do_uio(&c_b56, (char *)&z__[20001], (ftnlen)sizeof(doublereal));
    e_wsue();

    i__1 = *ntot;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_wsfe(&io___44);
	d__1 = ev[i__] + *de;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___45);
	d__1 = ev[i__] + *de;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___46);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	d__1 = ev[i__] - ev[1];
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__2 = ev[i__] - ev[i__ - 1];
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	d__3 = ev[i__] + *de;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L5: */
    }
/* ---------------------------------------- */
/* L6: */
/* 7     FORMAT(1X,I5,4F16.10) */
/* ---------------------------------------- */
    return 0;
} /* vibdim_ */

/* Subroutine */ int rnorm_(integer *n, integer *mroot, doublereal *hh, 
	doublereal *ev, doublereal *v, doublereal *eps, doublereal *dale, 
	integer *imark, doublereal *st, doublereal *dtamu, doublereal *a, 
	doublereal *dd, doublereal *z__, integer *ifail)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static doublereal e, f[20000];
    static integer i__;
    static doublereal t[20000], u[20000], cc, ee, eh;
    static integer ie;
    static doublereal el;
    static integer ir, mp;
    static doublereal rl[20000], rr[20000], dae, emin;
    extern /* Subroutine */ int norm_(integer *, doublereal *, doublereal *, 
	    integer *);

    /* Fortran I/O blocks */
    static cilist io___60 = { 0, 0, 0, 0, 0 };
    static cilist io___61 = { 0, 0, 0, 0, 0 };


/* ================================ */
/*     IMARK=0     NO SYMMETRY */
/*     IMARK=1     SYMMETRY */
/* -------------------------------- */
    /* Parameter adjustments */
    z__ -= 20001;
    --dd;
    a -= 101;
    --st;
    --v;
    --ev;

    /* Function Body */
    *eps = 1e-10;
    emin = 1e6;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*      WRITE(10,*)I,V(I) */
	if (v[i__] < emin) {
	    emin = v[i__];
	}
/* L90: */
    }
/* Computing 2nd power */
    d__1 = *hh;
    cc = -(d__1 * d__1) / (*dtamu * 6);
    el = emin;
    ee = el;
    i__1 = *mroot - 1;
    for (ir = 0; ir <= i__1; ++ir) {
L100:
	ee += *dale;
/* ---    DETERMINING EH,EL */
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t[i__ - 1] = cc * (ee - v[i__]);
/* L1: */
	    u[i__ - 1] = (t[i__ - 1] * 10 + 2) / (1 - t[i__ - 1]);
	}
	rr[*n - 1] = u[*n - 1];
	rl[0] = u[0];
	mp = 0;
	for (i__ = *n - 1; i__ >= 1; --i__) {
	    rr[i__ - 1] = u[i__ - 1] - 1 / rr[i__];
	    if (rr[i__ - 1] < 1.) {
		mp = i__;
		goto L2;
	    }
/* L3: */
	}
L2:
	ie = 0;
	i__2 = mp;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    rl[i__ - 1] = u[i__ - 1] - 1 / rl[i__ - 2];
	    if (rl[i__ - 1] < 0.) {
		++ie;
	    }
/* L6: */
	}
	if (ie == ir + 1) {
	    eh = ee;
/*      WRITE(0,*)'  EH=',EH, '  EL=',EL */
	    goto L25;
	}
	if (ie > ir + 1) {
	    s_wsle(&io___60);
	    do_lio(&c__9, &c__1, "  PLEASE GIVE A SMALL VALUE OF dale (= del"
		    "ta_E) ", (ftnlen)48);
	    e_wsle();
	    s_wsle(&io___61);
	    do_lio(&c__9, &c__1, " IE = ", (ftnlen)6);
	    do_lio(&c__3, &c__1, (char *)&ie, (ftnlen)sizeof(integer));
	    do_lio(&c__9, &c__1, " IR = ", (ftnlen)6);
	    do_lio(&c__3, &c__1, (char *)&ir, (ftnlen)sizeof(integer));
	    e_wsle();
	    s_stop("", (ftnlen)0);
	}
	goto L100;
L25:
	e = (eh + el) / 2;
	if (eh - el < *eps) {
	    goto L30;
	}
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t[i__ - 1] = cc * (e - v[i__]);
/* L28: */
	    u[i__ - 1] = (t[i__ - 1] * 10 + 2) / (1 - t[i__ - 1]);
	}
	rr[*n - 1] = u[*n - 1];
	rl[0] = u[0];
	i__2 = mp + 1;
	for (i__ = *n - 1; i__ >= i__2; --i__) {
/* L26: */
	    rr[i__ - 1] = u[i__ - 1] - 1 / rr[i__];
	}
	i__2 = mp;
	for (i__ = 2; i__ <= i__2; ++i__) {
/* L27: */
	    rl[i__ - 1] = u[i__ - 1] - 1 / rl[i__ - 2];
	}
	dae = 1 / rr[mp] - rl[mp - 1];
	if (dae < 0.) {
	    el = e;
	} else {
	    eh = e;
	}
	goto L25;
L30:
/*      WRITE(10,*)IR+1,E,'  ROOT' */
	el = e;
	ev[ir + 1] = e;
	f[mp - 1] = 1.;
	for (i__ = mp - 1; i__ >= 1; --i__) {
/* L31: */
	    f[i__ - 1] = f[i__] / rl[i__ - 1];
	}
	i__2 = *n;
	for (i__ = mp + 1; i__ <= i__2; ++i__) {
/* L32: */
	    f[i__ - 1] = f[i__ - 2] / rr[i__ - 1];
	}
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L35: */
	    f[i__ - 1] /= 1 - t[i__ - 1];
	}
/* -------------------------------------------- */
/* Normalize wavefunctions in array F. */
/* -------------------------------------------- */
	norm_(n, hh, f, imark);
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L37: */
	    z__[i__ + (ir + 1) * 20000] = f[i__ - 1];
	}
	ee = e;
/* L50: */
    }
/* -------------------------------------------- */
    return 0;
} /* rnorm_ */


/* Subroutine */ int norm_(integer *n, doublereal *hh, doublereal *f, integer 
	*imark)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer k, k1, k2, nm;
    static doublereal sum, sum1, sum2;

/* -------------------------------------------- */
    /* Parameter adjustments */
    --f;

    /* Function Body */
    nm = *n / 2;
    sum1 = 0.;
    sum2 = 0.;
    i__1 = nm - 1;
    for (k = 1; k <= i__1; ++k) {
	k1 = (k << 1) + 1;
/* L5: */
/* Computing 2nd power */
	d__1 = f[k1];
	sum1 += d__1 * d__1;
    }
    i__1 = nm;
    for (k = 1; k <= i__1; ++k) {
	k2 = k << 1;
/* L6: */
/* Computing 2nd power */
	d__1 = f[k2];
	sum2 += d__1 * d__1;
    }
/* Computing 2nd power */
    d__1 = f[1];
/* Computing 2nd power */
    d__2 = f[*n];
    sum = (sum1 * 2 + sum2 * 4 + d__1 * d__1 + d__2 * d__2) * *hh / 3;
    if (*imark == 1) {
	sum *= 2;
    }
    sum = 1 / sqrt(sum);
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
/* L10: */
	f[k] *= sum;
    }
/* -------------------------------------------- */
    return 0;
} /* norm_ */

/* Subroutine */ int readata_(void)
{
    /* Format strings */
    static char fmt_350[] = "(a)";
    static char fmt_370[] = "(//5x,\002THE ATOMIC MASSES ARE (IN ATOMIC MASS"
	    "):\002,/14x,\002A_mass\002,9x,\002B_mass\002,/10x,2f14.10)";
    static char fmt_380[] = "(//5x,\002The number of states used, NTOT =\002"
	    ",i5)";
    static char fmt_390[] = "(/10x,\002R-pts\002,5x,\002R_ini\002,5x,\002R_e"
	    "nd\002,/9x,\002(NCC1)\002,4x,\002(ST1I)\002,4x,\002(ST1F)\002,/8"
	    "x,i6,3x,f8.3,3x,f8.3,/)";
    static char fmt_375[] = "(/5x,\002The reduced molecular mass is : \002/4"
	    "x,f14.10,\002 (IN ATOMIC MASS)\002,/5x,\002or\002,/4x,f18.10,1x"
	    ",\002(IN ATOMIC MASS UNIT)\002)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double acos(doublereal);
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_rsfe(cilist *), do_fio(integer *, char *, ftnlen),
	     e_rsfe(void), s_wsle(cilist *), e_wsle(void), s_wsfe(cilist *), 
	    e_wsfe(void);

    /* Local variables */
    static integer i__;
    static doublereal ama, amb;
    static integer imu;
    static doublereal amu1, amae;
    static integer imal;
    static char title[80];

    /* Fortran I/O blocks */
    static cilist io___73 = { 0, 5, 0, 0, 0 };
    static cilist io___75 = { 0, 5, 0, 0, 0 };
    static cilist io___76 = { 0, 5, 0, fmt_350, 0 };
    static cilist io___78 = { 0, 5, 0, 0, 0 };
    static cilist io___80 = { 0, 5, 0, 0, 0 };
    static cilist io___83 = { 0, 5, 0, 0, 0 };
    static cilist io___85 = { 0, 10, 0, 0, 0 };
    static cilist io___86 = { 0, 10, 0, fmt_350, 0 };
    static cilist io___87 = { 0, 10, 0, fmt_370, 0 };
    static cilist io___88 = { 0, 5, 0, 0, 0 };
    static cilist io___89 = { 0, 5, 0, 0, 0 };
    static cilist io___90 = { 0, 10, 0, fmt_380, 0 };
    static cilist io___91 = { 0, 10, 0, fmt_390, 0 };
    static cilist io___92 = { 0, 5, 0, 0, 0 };
    static cilist io___93 = { 0, 5, 0, 0, 0 };
    static cilist io___94 = { 0, 5, 0, 0, 0 };
    static cilist io___95 = { 0, 5, 0, 0, 0 };
    static cilist io___97 = { 0, 4, 0, 0, 0 };
    static cilist io___98 = { 0, 5, 0, 0, 0 };
    static cilist io___99 = { 0, 10, 0, fmt_375, 0 };


/* ------------------------------------------------- */
    bdt_1.rbohr = .529177249;
    amae = 1822.9163036026441;
/* ------------------------------------------------- */
/*     AMAE=mass_unit/mass_e */
/*     AMAE=1.6605655D-27/9.1093897D-31 */
/*     AMAE=1822.9163 */
/*     CONST=2.0D0*109737.32D0 */
/*     1 Rydberg = 109737.32D0 */
/*     CONST=27.211396181D0*8065.541D0 */
/*     CONST=1 Hartree = 219474.63067 cm-1 */
/* ------------------------------------------------- */
    bdt_1.const__ = 219474.63067;
    bdt_1.pi = acos(-1.);
/* ------------------------------------------------- */
/* Imal == 1, read coeff. a(i) from fort.5; */
/*       = 2, read a(i) from fort.4. */
/*  ims -- number of terms in V(R) expansion. */
/* ------------------------------------------------- */
    s_rsle(&io___73);
    do_lio(&c__3, &c__1, (char *)&imal, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___75);
    do_lio(&c__3, &c__1, (char *)&ms2_1.ims, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ms2_1.nturn, (ftnlen)sizeof(integer));
    e_rsle();
/* ------------------------------------------------- */
/* Read title & atomic mass; cal. reduced mass */
/* ------------------------------------------------- */
    s_rsfe(&io___76);
    do_fio(&c__1, title, (ftnlen)80);
    e_rsfe();
    s_rsle(&io___78);
    do_lio(&c__3, &c__1, (char *)&imu, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___80);
    do_lio(&c__5, &c__1, (char *)&ama, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&amb, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___83);
    do_lio(&c__5, &c__1, (char *)&amu1, (ftnlen)sizeof(doublereal));
    e_rsle();
/* --- */
    s_wsle(&io___85);
    e_wsle();
    s_wsfe(&io___86);
    do_fio(&c__1, title, (ftnlen)80);
    e_wsfe();
    s_wsfe(&io___87);
    do_fio(&c__1, (char *)&ama, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&amb, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* ------------------------------------------------- */
/* NTOT -- number of roots (energies) wanted; */
/* DALE -- E_step (cm-1) used in renormalization. */
/* ------------------------------------------------- */
    s_rsle(&io___88);
    do_lio(&c__3, &c__1, (char *)&mol_1.ntot, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&mol_1.dale, (ftnlen)sizeof(doublereal));
    e_rsle();
/* ------------------------------------------------- */
/* NCC1 -- number of potential points; */
/* ST1I -- R_min for V(R); */
/* ST1F -- R_max used to cal. R_step : */
/*     R_step = (ST1F - ST1I)/(NCC1-1) = HH1 */
/* ------------------------------------------------- */
    s_rsle(&io___89);
    do_lio(&c__3, &c__1, (char *)&mol_1.ncc1, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&mol_1.st1i, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&mol_1.st1f, (ftnlen)sizeof(doublereal));
    e_rsle();
/* --- */
    s_wsfe(&io___90);
    do_fio(&c__1, (char *)&mol_1.ntot, (ftnlen)sizeof(integer));
    e_wsfe();
    s_wsfe(&io___91);
    do_fio(&c__1, (char *)&mol_1.ncc1, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&mol_1.st1i, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&mol_1.st1f, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* ------------------------------------------------- */
/*      De -- molecular dissociation energy. */
/*    beta  = 1.0, for SF potential function. */
/*  alamta  = variational parameter in SF V(R). */
/*     MSF  = 1, use SF V(R); = 0, use MMS. */
/*      Re -- equilibrium internuclear distance. */
/* ------------------------------------------------- */
    s_rsle(&io___92);
    do_lio(&c__5, &c__1, (char *)&ms2_1.de, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&ms2_1.beta, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&ms2_1.alamta, (ftnlen)sizeof(doublereal));
    do_lio(&c__3, &c__1, (char *)&ms2_1.msf, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&ms2_1.re, (ftnlen)sizeof(doublereal));
    e_rsle();
/* ------------------------------------------------- */
/* We -- vibrational constant */
/* ------------------------------------------------- */
    s_rsle(&io___93);
    do_lio(&c__5, &c__1, (char *)&mol_1.we, (ftnlen)sizeof(doublereal));
    e_rsle();
/* ------------------------------------------------- */
/* mset -- number of set of solution vectors  X; */
/*           usually, set  mset =< 20. */
/* mcst -- number of vib. constants in a vector X; */
/*           usually, set  mcst =< 6. */
/*             A(mcst, mcst);  b(mcst, 1) */
/*   mv -- number of vib. states in A used to */
/*           solve linear equation    A * X = b */
/*             set  mv =< 95. */
/* nvs(i) -- value of initial vib. quantum state */
/*           v+1 in each set of mset vectors, e.g. */
/*               i  = 1, 2, 3, 4, 5   (mset=5) */
/*           nvs(i) = 1, 3, 6, 8, 9 */
/*            v_ini = 0, 2, 5, 7, 8 */

/*    When mcst=6, the states used in a set are : */
/*  nvs(1)=1 :  v = 0, 1, 2, 3, 4, 5. */
/*  nvs(3)=6 :  v = 5, 6, 7, 8, 9,10. */

/*    X = ( We, WeXe, WeYe, WeZe, WeSe, WeTe, ...) */
/* ------------------------------------------------- */
    s_rsle(&io___94);
    do_lio(&c__3, &c__1, (char *)&la1_1.mset, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&la1_1.mcst, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&la1_1.mv, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___95);
    i__1 = la1_1.mset;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_lio(&c__3, &c__1, (char *)&la1_1.nvs[i__ - 1], (ftnlen)sizeof(
		integer));
    }
    e_rsle();
/* ------------------------------------------------- */
/* Read potential expansion coefficients  a(i) */
/* ------------------------------------------------- */
    if (imal == 2) {
	i__1 = ms2_1.ims;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___97);
	    do_lio(&c__5, &c__1, (char *)&ms1_1.a[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
/* L10: */
	}
    } else if (imal == 1) {
	i__1 = ms2_1.ims;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___98);
	    do_lio(&c__5, &c__1, (char *)&ms1_1.a[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
/* L20: */
	}
    }
/* ------------------------------------------------- */
    if (imu == 1) {
	bdt_1.amu = amb * ama / (ama + amb) * amae;
    } else if (imu == 2) {
	bdt_1.amu = amu1 * amae;
    }

    s_wsfe(&io___99);
    do_fio(&c__1, (char *)&amu1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&bdt_1.amu, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* ------------------------------------------------- */
/* ------------------------------------------------- */
    return 0;
} /* readata_ */

doublereal pot_(doublereal *r__)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double pow_di(doublereal *, integer *), exp(doublereal), sqrt(doublereal),
	     pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static doublereal vrydberg, d__;
    static integer i__;
    static doublereal x, f2, v00, fp, fy, rr, pf1, pf2, vpg, alph, func, 
	    pbeta, ralph, vmorse;

/* ------------------------------------------------- */
    rr = *r__ - ms2_1.re;
    x = rr;
    d__ = ms2_1.de * ms2_1.beta;
    func = 0.;
    i__1 = ms2_1.ims;
    for (i__ = 1; i__ <= i__1; ++i__) {
	func += ms1_1.a[i__ - 1] * pow_di(&rr, &i__);
/* L10: */
    }

/* --- Prepare Murrell-Sorbie (MS) POT */
    func = -d__ * (func + 1. / ms2_1.beta) * exp(-ms1_1.a[0] * ms2_1.beta * 
	    rr);
/* === */
    if (ms2_1.msf == 1) {
	f2 = bdt_1.amu * mol_1.we * mol_1.we;
/* --- Morse POT */
	if (ms2_1.nturn <= 2) {
	    alph = sqrt(f2 * .5 / ms2_1.de);
	    vmorse = ms2_1.de * (exp(alph * -2. * rr) - exp(-alph * rr) * 2.);
	    v00 = vmorse;
/* --- Rydberg POT */
	} else if (ms2_1.nturn == 3) {
	    ralph = sqrt(f2 / ms2_1.de);
	    vrydberg = -ms2_1.de * (ralph * x + 1.) * exp(-ralph * x);
	    v00 = vrydberg;
/* --- Pseudo-Gaussion POT */
	} else if (ms2_1.nturn == 4) {
	    pbeta = sqrt(f2 * ms2_1.re * ms2_1.re / ms2_1.de + 4.) * .5 - 1.;
	    pf1 = 1. - ms2_1.re / *r__ * (ms2_1.re / *r__);
	    pf2 = 1. - *r__ / ms2_1.re * (*r__ / ms2_1.re);
	    vpg = -ms2_1.de * (pbeta * pf1 + 1.) * exp(pbeta * pf2);
	    v00 = vpg;
	}

/* --- fy is the force-field variational function  LAMDA(R) */
	fy = x / *r__;
	if (ms2_1.ims == 3) {
	    fy = fy;
	} else if (ms2_1.ims == 4) {
	    d__1 = abs(fy);
	    fy = pow_dd(&d__1, &c_b180) * x / abs(x);
	} else if (ms2_1.ims == 5) {
	    d__1 = abs(fy);
	    fy = pow_dd(&d__1, &c_b181) * x / abs(x);
	}
	fy *= 1. - exp(-x / ms2_1.re * pow_dd(&ms2_1.alamta, &c_b182));
	fy *= ms2_1.alamta;
	fp = (fy + 1.) * func - fy * v00;
    }
/* === */
    if (*r__ < ms2_1.re) {
	if (ms2_1.nturn == 0) {
	    ret_val = fp;
	} else if (ms2_1.nturn == 1) {
	    ret_val = func;
	} else {
	    ret_val = v00;
	}
    } else {
	ret_val = fp;
    }
/* ------------------------------------------------- */
    return ret_val;
} /* pot_ */

/* --- */
/* Subroutine */ int linersolv_(doublereal *ax, integer *ka, doublereal *ex, 
	integer *ke, integer *kf, doublereal *bb)
{
    /* Format strings */
    static char fmt_301[] = "(//13x,\002*** Solved A*X=b for data set i ="
	    "\002,i3,\002  ***\002)";

    /* System generated locals */
    integer ax_dim1, ax_offset, ex_dim1, ex_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer i__, j, k, i1, m1, m2;
    static doublereal aa[9216]	/* was [96][96] */, ab[9216]	/* was [96][
	    96] */, ac[9216]	/* was [96][96] */, bx[96]	/* was [96][1]
	     */;
    static integer kt;
    extern /* Subroutine */ int multp_(integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *), gaussj_(doublereal *, integer *, integer *, 
	    doublereal *, integer *, integer *), mprint_(doublereal *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);

    /* Fortran I/O blocks */
    static cilist io___127 = { 0, 0, 0, fmt_301, 0 };


/*     dimension ax(96,96),ex(30,96),bb(mhegr) */
/* ------------------------------------------------- */
/* Loop over (solve) the mset sets linear equa. */
/*       mset =< 30 */
/* ------------------------------------------------- */
    /* Parameter adjustments */
    ax_dim1 = *ka;
    ax_offset = 1 + ax_dim1 * 1;
    ax -= ax_offset;
    ex_dim1 = *ke;
    ex_offset = 1 + ex_dim1 * 1;
    ex -= ex_offset;
    --bb;

    /* Function Body */
    i__1 = la1_1.mset;
    for (j = 1; j <= i__1; ++j) {
	m1 = la1_1.nvs[j - 1];
	m2 = m1 + la1_1.mcst - 1;
/* ------------------------------------------------- */
/* For i=m2 :  m2-m1+1 = m1+mcst-1 - m1+1 = mcst */
/*   The physical size of arrays : */
/*        aa(mcst,mcst),   bx(mcst,1) */
/* Define aa & bx arrays in the jth set */

/*   The shifted energy  E(v) = Ev + De */
/* ------------------------------------------------- */
	i__2 = m2;
	for (i__ = m1; i__ <= i__2; ++i__) {
	    i1 = i__ - m1 + 1;
	    bx[i1 - 1] = bb[i__] + ms2_1.de;
/* ------------------------------------------------- */
/*  Next line generates wrong  We,WeXe,WeYe, ... : */
/*           bx(i1,1) = bb(i) */
/* ------------------------------------------------- */
	    i__3 = la1_1.mcst;
	    for (k = 1; k <= i__3; ++k) {
		aa[i1 + k * 96 - 97] = ax[i__ + k * ax_dim1];
		ab[i1 + k * 96 - 97] = ax[i__ + k * ax_dim1];
	    }
/* L20: */
	}
/* ------------------------------------------------- */
/* Solve   aa*X=bx    for the jth set */
/* ------------------------------------------------- */
	gaussj_(aa, &la1_1.mcst, &c__96, bx, &c__1, &c__1);
/* ------------------------------------------------- */
/*   When gaussj returns, aa = 1/ab , bx == X . */
/* Save the jth solution vector bx onto ex . */
/* ------------------------------------------------- */
	i__2 = la1_1.mcst;
	for (k = 1; k <= i__2; ++k) {
	    ex[j + k * ex_dim1] = bx[k - 1];
	}
/* ------------------------------------------------- */
/* Print ab and its inverse matrix aa == 1/ab */
/* ------------------------------------------------- */
	kt = 18;
	io___127.ciunit = kt;
	s_wsfe(&io___127);
	do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
	e_wsfe();
	mprint_(ab, &la1_1.mcst, &la1_1.mcst, &c__96, &c__96, &kt, &c__1);
	mprint_(aa, &la1_1.mcst, &la1_1.mcst, &c__96, &c__96, &kt, &c__2);
/* ------------------------------------------------- */
/* Check & print  if  ab *aa = ab * 1/ab = 1 */
/* ------------------------------------------------- */
	multp_(&la1_1.mcst, &la1_1.mcst, &la1_1.mcst, aa, ab, ac, &c__96, &
		c__96, &c__96);
	mprint_(ac, &la1_1.mcst, &la1_1.mcst, &c__96, &c__96, &kt, &c__3);
/* L30: */
    }
/* ------------------------------------------------- */
/* Print the solution vector array  ex */
/* ------------------------------------------------- */
    mprint_(&ex[ex_offset], &la1_1.mset, &la1_1.mcst, &c__30, &c__96, &kt, &
	    c__4);
/* ------------------------------------------------- */
/* ------------------------------------------------- */
/*       return */
    return 0;
} /* linersolv_ */


/* ============================================================================== */
/*  GAUSSJ solves for the linear equations */
/*                          A*X=b. */
/* using the Gauss-Jordan elimination. */
/*   Input:  a(n,n) is the coefficient matrix A.  b(n,m) is an inputting */
/*           matrix containning the m right-hand side vectors. */
/*   Output: a(n,n) is the inverse matrix A-1 of A.  b(n,m) is the solution */
/*           vectors X; and b == a = A-1 if b is inputted as an unit matrix. */
/* ============================================================================== */
/* Subroutine */ int gaussj_(doublereal *a, integer *n, integer *np, 
	doublereal *b, integer *m, integer *mp)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_paus(char *, ftnlen);

    /* Local variables */
    static integer i__, j, k, l, ll;
    static doublereal big, dum;
    static integer icol, ipiv[50], irow, indxc[50], indxr[50];
    static doublereal pivinv;

/* ------------------------------------------------------------------------------ */
/*     INTEGER m,mp,n,np,NMAX */
/*     INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX) */
/*     REAL a(np,np),b(np,mp) */
/*     REAL big,dum,pivinv */
/* ------------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------------ */

    /* Parameter adjustments */
    a_dim1 = *np;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *np;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	ipiv[j - 1] = 0;
/* L11: */
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	big = 0.;
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    if (ipiv[j - 1] != 1) {
		i__3 = *n;
		for (k = 1; k <= i__3; ++k) {
		    if (ipiv[k - 1] == 0) {
			if ((d__1 = a[j + k * a_dim1], abs(d__1)) >= big) {
			    big = (d__1 = a[j + k * a_dim1], abs(d__1));
			    irow = j;
			    icol = k;
			}
		    } else if (ipiv[k - 1] > 1) {
			s_paus("singular matrix in gaussj", (ftnlen)25);
		    }
/* L12: */
		}
	    }
/* L13: */
	}
	++ipiv[icol - 1];
	if (irow != icol) {
	    i__2 = *n;
	    for (l = 1; l <= i__2; ++l) {
		dum = a[irow + l * a_dim1];
		a[irow + l * a_dim1] = a[icol + l * a_dim1];
		a[icol + l * a_dim1] = dum;
/* L14: */
	    }
	    i__2 = *m;
	    for (l = 1; l <= i__2; ++l) {
		dum = b[irow + l * b_dim1];
		b[irow + l * b_dim1] = b[icol + l * b_dim1];
		b[icol + l * b_dim1] = dum;
/* L15: */
	    }
	}
	indxr[i__ - 1] = irow;
	indxc[i__ - 1] = icol;
	if (a[icol + icol * a_dim1] == 0.) {
	    s_paus("singular matrix in gaussj", (ftnlen)25);
	}
	pivinv = 1. / a[icol + icol * a_dim1];
	a[icol + icol * a_dim1] = 1.;
	i__2 = *n;
	for (l = 1; l <= i__2; ++l) {
	    a[icol + l * a_dim1] *= pivinv;
/* L16: */
	}
	i__2 = *m;
	for (l = 1; l <= i__2; ++l) {
	    b[icol + l * b_dim1] *= pivinv;
/* L17: */
	}
	i__2 = *n;
	for (ll = 1; ll <= i__2; ++ll) {
	    if (ll != icol) {
		dum = a[ll + icol * a_dim1];
		a[ll + icol * a_dim1] = 0.;
		i__3 = *n;
		for (l = 1; l <= i__3; ++l) {
		    a[ll + l * a_dim1] -= a[icol + l * a_dim1] * dum;
/* L18: */
		}
		i__3 = *m;
		for (l = 1; l <= i__3; ++l) {
		    b[ll + l * b_dim1] -= b[icol + l * b_dim1] * dum;
/* L19: */
		}
	    }
/* L21: */
	}
/* L22: */
    }
    for (l = *n; l >= 1; --l) {
	if (indxr[l - 1] != indxc[l - 1]) {
	    i__1 = *n;
	    for (k = 1; k <= i__1; ++k) {
		dum = a[k + indxr[l - 1] * a_dim1];
		a[k + indxr[l - 1] * a_dim1] = a[k + indxc[l - 1] * a_dim1];
		a[k + indxc[l - 1] * a_dim1] = dum;
/* L23: */
	    }
	}
/* L24: */
    }
/* -------------------------------------------- */
    return 0;
} /* gaussj_ */


/* Subroutine */ int multp_(integer *n, integer *l, integer *m, doublereal *a,
	 doublereal *b, doublereal *c__, integer *n1, integer *l1, integer *
	m1)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2, 
	    i__3;

    /* Local variables */
    static integer i__, j, k;

/* MATRIX MULTIPLICATION     C(N-M) = A(N-L) * B(L-M) */

    /* Parameter adjustments */
    a_dim1 = *n1;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    c_dim1 = *n1;
    c_offset = 1 + c_dim1 * 1;
    c__ -= c_offset;
    b_dim1 = *l1;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    c__[i__ + j * c_dim1] = 0.;
	    i__3 = *l;
	    for (k = 1; k <= i__3; ++k) {
		c__[i__ + j * c_dim1] += a[i__ + k * a_dim1] * b[k + j * 
			b_dim1];
/* L20: */
	    }
	}
    }
/* ------------------------------------------ */
    return 0;
} /* multp_ */


/* Subroutine */ int mprint_(doublereal *g, integer *m, integer *n, integer *
	ni, integer *nj, integer *kt, integer *kg)
{
    /* Format strings */
    static char fmt_100[] = "(//23x,\002The input matrix A :\002/)";
    static char fmt_110[] = "(//23x,\002The inverse of A (=> 1/A) :\002/)";
    static char fmt_120[] = "(//23x,\002The multiplication of A * (1/A) :"
	    "\002/)";
    static char fmt_130[] = "(///23x,\002The solution vector array  X : \002"
	    "/)";
    static char fmt_170[] = "(4x,\002j\002,8x,i2,7(12x,i2))";
    static char fmt_180[] = "(3x,\002i\002)";
    static char fmt_190[] = "(2x,i2,2x,6(1pe14.6))";
    static char fmt_200[] = "(\002*\002,33x,\002Continue.\002/)";
    static char fmt_220[] = "(\002!\002,35x,\002END.\002)";

    /* System generated locals */
    integer g_dim1, g_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    static integer i__, j, k, n1, n2, iout;

    /* Fortran I/O blocks */
    static cilist io___146 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___147 = { 0, 0, 0, fmt_110, 0 };
    static cilist io___148 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___149 = { 0, 0, 0, fmt_130, 0 };
    static cilist io___153 = { 0, 0, 0, fmt_170, 0 };
    static cilist io___155 = { 0, 0, 0, fmt_180, 0 };
    static cilist io___157 = { 0, 0, 0, fmt_190, 0 };
    static cilist io___158 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___159 = { 0, 0, 0, fmt_170, 0 };
    static cilist io___160 = { 0, 0, 0, fmt_180, 0 };
    static cilist io___161 = { 0, 0, 0, fmt_190, 0 };
    static cilist io___162 = { 0, 0, 0, fmt_220, 0 };


/* ------------------------------------------------------------------------- */
/*  This subroutine is written to print a matrix g.  Weiguo Sun  06/25/1993 */
/* On entry : */
/*   g(m,n)  -- The matrix to be printed. */
/*     m,n   -- Integers to specify the actual size of g. */
/*    ni,nj  -- Integers to specify the dimension of g. */
/*       kt  -- Index for printing unit. */
/*       kg  -- Switch to print the tittle (you may change it) of g. */
/*              kg=0, no tittle is printed. */
/* ------------------------------------------------------------------------- */
    /* Parameter adjustments */
    g_dim1 = *ni;
    g_offset = 1 + g_dim1 * 1;
    g -= g_offset;

    /* Function Body */
    iout = *kt;
/*     iout=6 */

    if (*kg == 0) {
	goto L10;
    }
    if (*kg == 1) {
	io___146.ciunit = iout;
	s_wsfe(&io___146);
	e_wsfe();
    } else if (*kg == 2) {
	io___147.ciunit = iout;
	s_wsfe(&io___147);
	e_wsfe();
    } else if (*kg == 3) {
	io___148.ciunit = iout;
	s_wsfe(&io___148);
	e_wsfe();
    } else if (*kg == 4) {
	io___149.ciunit = iout;
	s_wsfe(&io___149);
	e_wsfe();
    }

L10:
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	n1 = k * 5;
	if (n1 >= *n) {
	    goto L75;
	}
	n2 = n1 - 4;
	io___153.ciunit = iout;
	s_wsfe(&io___153);
	i__2 = n1;
	for (j = n2; j <= i__2; ++j) {
	    do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
	}
	e_wsfe();
	io___155.ciunit = iout;
	s_wsfe(&io___155);
	e_wsfe();

	i__2 = *m;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L50: */
	    io___157.ciunit = iout;
	    s_wsfe(&io___157);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    i__3 = n1;
	    for (j = n2; j <= i__3; ++j) {
		do_fio(&c__1, (char *)&g[i__ + j * g_dim1], (ftnlen)sizeof(
			doublereal));
	    }
	    e_wsfe();
	}
	io___158.ciunit = iout;
	s_wsfe(&io___158);
	e_wsfe();
/* L70: */
    }

L75:
    n2 = n1 - 4;
    io___159.ciunit = iout;
    s_wsfe(&io___159);
    i__1 = *n;
    for (j = n2; j <= i__1; ++j) {
	do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
    }
    e_wsfe();
    io___160.ciunit = iout;
    s_wsfe(&io___160);
    e_wsfe();
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L80: */
	io___161.ciunit = iout;
	s_wsfe(&io___161);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	i__3 = *n;
	for (j = n2; j <= i__3; ++j) {
	    do_fio(&c__1, (char *)&g[i__ + j * g_dim1], (ftnlen)sizeof(
		    doublereal));
	}
	e_wsfe();
    }
    io___162.ciunit = iout;
    s_wsfe(&io___162);
    e_wsfe();
/* ------------------------------------------------------- */
/* ------------------------------------------------------- */
    return 0;
} /* mprint_ */


/* Subroutine */ int moment_(doublereal *data, integer *n, integer *nd, 
	doublereal *ave, doublereal *adev, doublereal *sdev, doublereal *var, 
	doublereal *skew, doublereal *curt)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_paus(char *, ftnlen);
    double sqrt(doublereal);

    /* Local variables */
    static integer j;
    static doublereal p, s, ep;

/* --------------------------------------------------------------------- */
/*   Given an array of data(1:n), this routine returns its mean ave, */
/* average deviation adev, standard deviation sdev, variance var, */
/* skewness skew, and kurtosis curt. */
/* --------------------------------------------------------------------- */
/*     REAL adev,ave,curt,sdev,skew,var,data(n) */
/* --------------------------------------------------------------------- */
    /* Parameter adjustments */
    --data;

    /* Function Body */
    if (*n <= 1) {
	s_paus("n must be at least 2 in moment", (ftnlen)30);
    }
    s = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	s += data[j];
/* L11: */
    }
    *ave = s / *n;
    *adev = 0.;
    *var = 0.;
    *skew = 0.;
    *curt = 0.;
    ep = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	s = data[j] - *ave;
	ep += s;
	*adev += abs(s);
	p = s * s;
	*var += p;
	p *= s;
	*skew += p;
	p *= s;
	*curt += p;
/* L12: */
    }
    *adev /= *n;
/* Computing 2nd power */
    d__1 = ep;
    *var = (*var - d__1 * d__1 / *n) / (*n - 1);
    *sdev = sqrt(*var);
    if (*var != 0.) {
/* Computing 3rd power */
	d__1 = *sdev;
	*skew /= *n * (d__1 * (d__1 * d__1));
/* Computing 2nd power */
	d__1 = *var;
	*curt = *curt / (*n * (d__1 * d__1)) - 3.;
    } else {
	s_paus("no skew or kurtosis when zero variance in moment", (ftnlen)48)
		;
    }
    return 0;
} /* moment_ */

