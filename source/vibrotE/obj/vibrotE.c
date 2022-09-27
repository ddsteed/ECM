/* ../src/vibrotE.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal amu, re, de, wee, we, wexe, weye, weze, wete;
} spectra_;

#define spectra_1 spectra_

struct {
    doublereal be, alphae, gamae, der, betae, bryd, dryd;
} spectrb_;

#define spectrb_1 spectrb_

struct {
    doublereal w0, we0, wex, wey, wez, wet, wes, wer, v0, r0;
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
    doublereal beta, br4, br6, br8, br10, br12, br14;
} bdt_;

#define bdt_1 bdt_

struct {
    integer ms, mryd, mwe, ny, nw0, nvd;
} fms_;

#define fms_1 fms_

struct {
    doublereal bt, c0, c02, c03, c04, c05, c06, c07, c08, c09, c010;
} pgdata1_;

#define pgdata1_1 pgdata1_

struct {
    doublereal re2, re4, re6, re8, re10, re12, re14, re16, re18, re20;
} pgdata2_;

#define pgdata2_1 pgdata2_

struct {
    doublereal ff2, betap;
} vnumpot_;

#define vnumpot_1 vnumpot_

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
static integer c__200 = 200;
static integer c__300 = 300;
static integer c_n1 = -1;
static doublereal c_b400 = 2.;
static doublereal c_b402 = -1.;
static integer c__2 = 2;

/*     Program  vibrotE */

/*     This program is to calculate the vibrational/ro-vibrational */
/*   energies, or scattering threshold energies and channel energies. */
/* ---------------------------------------------------------------------- */
/* Main program */ int MAIN__(void)
{
    /* Format strings */
    static char fmt_100[] = "(///16x,\002**=  Output of \"vibrotE.f\"  =**"
	    " \002,//9x,\002Switches to calculate VIB-ROTational energies "
	    ":\002,//6x,\002[ mtyp = 1, use input vib-rot consts to cal. Ev &"
	    " Evj;\002,/6x,\002       = 2, use num. derivatives of V(R) for E"
	    "v & Evj.\002,//6x,\002            ! Run both mtyp=1, 2 to compar"
	    "e Ev(max) ! \002,//6x,\002   Mvr = 1, for vib.; = 2 for rot-vib."
	    "; = 0, for BOTH.\002,/6x,\002    NE = The number of input SCATTE"
	    "RING energies.\002,/6x,\002    Nv = The number of vibrational st"
	    "ates considered.\002,/6x,\002              Nv=6 means that v=0,1"
	    ",2,3,4,5 \002,/6x,\002  iniv = Quantum no. of vibrational refere"
	    "nce state.\002,/6x,\002  jnir = Quantum no. of  rotational refer"
	    "ence state.\002,/6x,\002    Nw = 0, input (WE,...,Be,...) in a.u"
	    ".; \002,/6x,\002       = 1, input in cm-1, code will convert the"
	    "m.     \002,/6x,\002  beta = WIDTH parameter of a Rydberg-like p"
	    "otential. ]\002,//10x,\002mtyp  Mvr  NE  Nv  iniv  jnir  Nw   be"
	    "ta \002,/10x,i3,3x,i2,3x,i2,2x,i2,2x,i3,3x,i3,3x,i2,1x,f10.7///6"
	    "x,\002{   Re = Equilibrium internuclear distance in ao. \002,/6x,"
	    "\002    De = The dissociation energy of AB in a.u.    \002,/6x"
	    ",\002  Amas = The MASS of atom A in amu.               \002,/6x"
	    ",\002  Bmas = The MASS of atom B in amu.               } \002,//"
	    "9x,\002                                    Reduced_mass_of_AB"
	    " \002,/3x,\002Re(ao)\002,3x,\002De(a.u)     Amas       Bmas\002"
	    ",\002     (in amu)    (in a.u) \002,/1f9.5,1f10.6,1x,1f10.6,1x,1"
	    "f10.6,1x,1f10.6,1f14.6,//3x,\002   nj = is no. of rotational sta"
	    "tes in each vib. state :\002,//23x,\002  v      nj  \002,/)";
    static char fmt_105[] = "(23x,i3,4x,i4)";
    static char fmt_108[] = "(//15x,\002The input SCATTERING energies : \002"
	    ",//23x,\002  i      E(i;eV) \002,/)";
    static char fmt_110[] = "(23x,i3,3x,f10.5)";
    static char fmt_115[] = "(//2x,\002   The input ro-vibrational constants"
	    " are : \002//,7x,\002  We \002,11x,\002WeXe\002,11x,\002 WeYe"
	    "\002,/3(1pe16.8),//7x,\002WeZe \002,11x,\002WeTe\002,11x,\002 We"
	    "Se\002,/3(1pe16.8),//7x,\002  Be \002,10x,\002Alphae\002,10x,"
	    "\002Gammae\002,/3(1pe16.8),//7x,\002 Der \002,11x,\002betae\002,"
	    "/2(1pe16.8),/)";
    static char fmt_120[] = "(//15x,\002** Vibrational threshold & channel e"
	    "nergies **\002//,18x,\002The vibrational threshold energies are "
	    ": \002,//,3x,\002v     Ethre(Rydberg)    Ethre(a.u.)      Ethre("
	    "eV) \002,\002   Dif(v-1,v; eV)\002,/)";
    static char fmt_130[] = "(x,i3,4f16.7)";
    static char fmt_140[] = "(///1x,\002For scattering energy =\002,f12.6"
	    ",\002  au  =\002,f12.6,\002 eV\002,//\002 The channel energies ("
	    "Ev = Kv**2 = 2*E) are : \002,//3x,\002v      Echan(a.u.)      Ec"
	    "han(eV)  \002,/)";
    static char fmt_150[] = "(/6x,\002 * Ro-vibrational threshold & channel "
	    "energies *\002//,9x,\002 The ro-vibrational threshold energies a"
	    "re : \002,//,3x,\002      v   j   Ethre(a.u.)   Ethre(eV)   Delt"
	    "a_Evj(eV) \002,/)";
    static char fmt_160[] = "(7x,i3,i4,3f13.7)";
    static char fmt_170[] = "(///2x,\002 For scattering energy =\002,f12.6"
	    ",\002  au  =\002,f12.6,\002 eV\002,//,6x,\002 The channel energi"
	    "es (Evj = Kvj**2 = 2*E) are :\002,//3x,\002      v   j   Echan(a"
	    ".u.)   Echan(eV)  \002,/)";
    static char fmt_200[] = "(//2x,\002* Vibrational energies *\002//,3x,"
	    "\002  v      Evib(a.u.) \002,/)";
    static char fmt_210[] = "(3x,i3,1pe18.8)";
    static char fmt_220[] = "(//2x,\002* Vibrational energies *\002//,3x,"
	    "\002  v       Evib(cm-1) \002,/)";
    static char fmt_230[] = "(3x,i3,f18.6,f16.6)";
    static char fmt_240[] = "(//3x,\002* Vibrational energies *\002//,3x,"
	    "\002  v    Delta_Evib(cm-1) \002,/)";

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen),
	     e_wsfe(void), s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static doublereal e[200];
    static integer i__, j, k, ne, kk;
    static doublereal av[200];
    static integer nj[200], ir, jr, kv, nv, nw;
    static doublereal am0;
    static integer nj1;
    static doublereal dif, eau[200], evb[200], eev[200], ams, bms, avn;
    static integer njr;
    static doublereal evr[40000]	/* was [200][200] */;
    static integer mvr;
    static doublereal evr0, amae, dife, aucm, auev, eryd[200];
    static integer iniv, jnir;
    static doublereal wese, devr;
    static integer mtyp;
    static doublereal echan[200], erefr, ethre[200], erefv, rbohr, evrev, 
	    rydev;
    extern /* Subroutine */ int calevj_(integer *, integer *, integer *, 
	    doublereal *, doublereal *), vrener_(integer *, integer *, 
	    integer *);

    /* Fortran I/O blocks */
    static cilist io___6 = { 0, 5, 0, 0, 0 };
    static cilist io___8 = { 0, 5, 0, 0, 0 };
    static cilist io___15 = { 0, 5, 0, 0, 0 };
    static cilist io___16 = { 0, 5, 0, 0, 0 };
    static cilist io___20 = { 0, 5, 0, 0, 0 };
    static cilist io___23 = { 0, 6, 0, fmt_100, 0 };
    static cilist io___24 = { 0, 35, 0, fmt_100, 0 };
    static cilist io___25 = { 0, 6, 0, fmt_105, 0 };
    static cilist io___26 = { 0, 35, 0, fmt_105, 0 };
    static cilist io___28 = { 0, 5, 0, 0, 0 };
    static cilist io___31 = { 0, 6, 0, fmt_108, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_110, 0 };
    static cilist io___33 = { 0, 5, 0, 0, 0 };
    static cilist io___34 = { 0, 5, 0, 0, 0 };
    static cilist io___36 = { 0, 5, 0, 0, 0 };
    static cilist io___37 = { 0, 5, 0, 0, 0 };
    static cilist io___38 = { 0, 6, 0, fmt_115, 0 };
    static cilist io___39 = { 0, 5, 0, 0, 0 };
    static cilist io___40 = { 0, 5, 0, 0, 0 };
    static cilist io___41 = { 0, 5, 0, 0, 0 };
    static cilist io___47 = { 0, 6, 0, fmt_120, 0 };
    static cilist io___54 = { 0, 6, 0, fmt_130, 0 };
    static cilist io___56 = { 0, 6, 0, fmt_140, 0 };
    static cilist io___58 = { 0, 6, 0, fmt_130, 0 };
    static cilist io___59 = { 0, 6, 0, 0, 0 };
    static cilist io___60 = { 0, 6, 0, fmt_150, 0 };
    static cilist io___66 = { 0, 6, 0, fmt_160, 0 };
    static cilist io___67 = { 0, 6, 0, 0, 0 };
    static cilist io___68 = { 0, 6, 0, fmt_170, 0 };
    static cilist io___71 = { 0, 6, 0, fmt_160, 0 };
    static cilist io___72 = { 0, 6, 0, 0, 0 };
    static cilist io___73 = { 0, 6, 0, fmt_200, 0 };
    static cilist io___75 = { 0, 6, 0, 0, 0 };
    static cilist io___76 = { 0, 6, 0, fmt_210, 0 };
    static cilist io___77 = { 0, 6, 0, fmt_220, 0 };
    static cilist io___78 = { 0, 6, 0, 0, 0 };
    static cilist io___79 = { 0, 6, 0, fmt_230, 0 };
    static cilist io___80 = { 0, 6, 0, fmt_240, 0 };
    static cilist io___82 = { 0, 6, 0, fmt_230, 0 };


/* ---------------------------------------------------------------------- */

/* On input: */

/*       mtyp  = 1, use input vib-rot constants to cal. Ev & Evj; */
/*             = 2, use numerical derivatives of V(R) to cal. Ev & Evj. */
/*        Mvr  = 1, for vibration; = 2 for rot-vibration; =0 for BOTH. */
/*         NE  = The # of input SCATTERING energies */
/*         Nv  = # of vibrational states considered */
/*                Nv=6 means that v=0,1,2,3,4,5 */
/*        iniv = Quantum # of the initial vibrational reference state */
/*        jnir = Quantum # of the initial  rotational reference state */
/*                 (Usually take iniv=v0=0, jnir=j0=0) */
/*         Nw  = 0, input (WE,...,Be,...) in a.u.; */
/*             = 1, input (WE,...,Be,...) in cm-1, code will convert them. */
/*       nj(i) = # of rotational states in the i_th vibrational state. */

/*        beta = WIDTH (adjustable) parameter of the potential. */

/*       E(i)  = The energies of the scattered particle (in eV) */
/*   We, WeXe  = Experimental vibrational energy constants (in Hartree) */
/*  Be,alphae,Der= Experimental  rotational energy constants (in Hartree) */

/*   Using Sun's perturbation formulae to calculate energies Evj. */

/* ---------------------------------------------------------------------- */
    rbohr = .529177249;
    rydev = 13.60569809;
    auev = 27.21139618;
    aucm = 219474.6306;
    amae = 1822.9163036026441;
/* ---------------------------------------------------------------------- */
/*      amae=mass_unit/mass_e */
/*      amae=1.6605655D-27/9.1093897D-31=1822.9163 */
/* ---------------------------------------------------------------------- */
    s_rsle(&io___6);
    do_lio(&c__3, &c__1, (char *)&mtyp, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___8);
    do_lio(&c__3, &c__1, (char *)&mvr, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ne, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&nv, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&iniv, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&jnir, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&nw, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___15);
    do_lio(&c__5, &c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&bdt_1.beta, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___16);
    do_lio(&c__5, &c__1, (char *)&ams, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&bms, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&am0, (ftnlen)sizeof(doublereal));
    e_rsle();
    if (am0 == 0.) {
	am0 = ams * bms / (ams + bms);
    }
    spectra_1.amu = am0 * amae;
    if (nw > 0) {
	spectra_1.re /= rbohr;
	spectra_1.de /= aucm;
    }
    s_rsle(&io___20);
    i__1 = nv;
    for (k = 1; k <= i__1; ++k) {
	do_lio(&c__3, &c__1, (char *)&nj[k - 1], (ftnlen)sizeof(integer));
    }
    e_rsle();
    s_wsfe(&io___23);
    do_fio(&c__1, (char *)&mtyp, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&mvr, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ne, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nv, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iniv, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&jnir, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nw, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bdt_1.beta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&ams, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&bms, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&am0, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.amu, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___24);
    do_fio(&c__1, (char *)&mtyp, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&mvr, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ne, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nv, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iniv, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&jnir, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nw, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bdt_1.beta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&ams, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&bms, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&am0, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.amu, (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = nv;
    for (k = 1; k <= i__1; ++k) {
	s_wsfe(&io___25);
	i__2 = k - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&nj[k - 1], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___26);
	i__2 = k - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&nj[k - 1], (ftnlen)sizeof(integer));
	e_wsfe();
    }
/* ---------------------------------------------------------------------- */
    nj1 = nj[0];
    nj[0] = 1;
/* ---------------------------------------------------------------------- */
    spectr0_1.v0 = (doublereal) iniv;
    spectr0_1.r0 = (doublereal) jnir;
    if (ne > 0) {
	s_rsle(&io___28);
	i__1 = ne;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__5, &c__1, (char *)&e[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	}
	e_rsle();
	s_wsfe(&io___31);
	e_wsfe();
	i__1 = ne;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_wsfe(&io___32);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&e[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
/* ---------------------------------------------------------------------- */
    s_rsle(&io___33);
    do_lio(&c__5, &c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.wexe, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.weye, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___34);
    do_lio(&c__5, &c__1, (char *)&spectra_1.weze, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectra_1.wete, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&wese, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___36);
    do_lio(&c__5, &c__1, (char *)&spectrb_1.be, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectrb_1.alphae, (ftnlen)sizeof(doublereal)
	    );
    do_lio(&c__5, &c__1, (char *)&spectrb_1.gamae, (ftnlen)sizeof(doublereal))
	    ;
    e_rsle();
    s_rsle(&io___37);
    do_lio(&c__5, &c__1, (char *)&spectrb_1.der, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectrb_1.betae, (ftnlen)sizeof(doublereal))
	    ;
    e_rsle();
    s_wsfe(&io___38);
    do_fio(&c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.wexe, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.weye, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.weze, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.wete, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&wese, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectrb_1.be, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectrb_1.alphae, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectrb_1.gamae, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectrb_1.der, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectrb_1.betae, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* - */
    s_rsle(&io___39);
    do_lio(&c__5, &c__1, (char *)&eswitch_1.aye, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitch_1.aze, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitch_1.ate, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitch_1.ase, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitch_1.are, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___40);
    do_lio(&c__5, &c__1, (char *)&eswitc1_1.abe, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc1_1.aae, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc1_1.age, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae3, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae4, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae5, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae6, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc1_1.ae7, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___41);
    do_lio(&c__5, &c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc2_1.abt, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax2, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax3, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax4, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax5, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax6, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&eswitc2_1.ax7, (ftnlen)sizeof(doublereal));
    e_rsle();
/* --- */
    if (nw > 0) {
	spectra_1.we /= aucm;
	spectra_1.wexe /= aucm;
	spectra_1.weye /= aucm;
	spectra_1.weze /= aucm;
	spectra_1.wete /= aucm;
	wese /= aucm;
	spectrb_1.be /= aucm;
	spectrb_1.alphae /= aucm;
	spectrb_1.gamae /= aucm;
	spectrb_1.der /= aucm;
	spectrb_1.betae /= aucm;
    }
    spectra_1.wee = spectra_1.we;
/* ---------------------------------------------------------------------- */
    if (mtyp == 2) {
	goto L10;
    }
/* ---------------------------------------------------------------------- */
    spectr0_1.w0 = 0.;
    spectr0_1.we0 = 0.;
    spectr0_1.wex = spectra_1.wexe;
    spectr0_1.wey = spectra_1.weye;
    spectr0_1.wez = spectra_1.weze;
    spectr0_1.wet = spectra_1.wete;
    spectr0_1.wes = wese;
    spectr0_1.wer = 0.;
    spectr1_1.bee = spectrb_1.be;
    spectr1_1.ale = spectrb_1.alphae;
    spectr1_1.gae = spectrb_1.gamae;
    spectr2_1.dee = spectrb_1.der;
    spectr2_1.bete = spectrb_1.betae;
/* ---------------------------------------------------------------------- */
/*  Cal. vibrational-rotational REFERENCE energies Eref for mtyp=1 */
/* ---------------------------------------------------------------------- */
    calevj_(&c__1, nj, &c__200, ethre, evr);
    erefv = ethre[0];
    erefr = evr[0];
/* ---------------------------------------------------------------------- */
/*  Calculate vib.-rotational energies Ev==Evb; Evj=Evr for mtyp=1 */
/* ---------------------------------------------------------------------- */
    nj[0] = nj1;
    calevj_(&nv, nj, &c__200, evb, evr);
    goto L15;
/* ====================================================================== */
/*  Cal. force constants and vib. constants used in Ev/Evj for mtyp=2 */
/* ---------------------------------------------------------------------- */
L10:
    vrener_(&nv, nj, &c__200);
/* ---------------------------------------------------------------------- */
/*  Cal. vibrational-rotational REFERENCE energies  Eref for mtyp=2 */
/* ---------------------------------------------------------------------- */
    calevj_(&c__1, nj, &c__200, ethre, evr);
    erefv = ethre[0];
    erefr = evr[0];
/* ---------------------------------------------------------------------- */
/*  Calculate vib.-rotational energies Ev==Evb; Evj=Evr  for mtyp=2 */
/* ---------------------------------------------------------------------- */
    nj[0] = nj1;
    calevj_(&nv, nj, &c__200, evb, evr);
/* ====================================================================== */
L15:
    if (mvr == 2) {
	goto L50;
    }
/* ---------------------------------------------------------------------- */
/*  Calculate vibrational THREshold energies  Ethre. */
/* ---------------------------------------------------------------------- */
    s_wsfe(&io___47);
    e_wsfe();
    i__1 = nv;
    for (k = 1; k <= i__1; ++k) {
	kv = k - 1;
	av[k - 1] = (doublereal) kv;
	avn = av[k - 1];
/* --- */
/* Ethre(k) are in Hartree */
/*        Ethre(k)=Wee*(avn-v0) - WeXe*(avn*(avn+1)-v0*(v0+1)) */
/* --- */
	ethre[k - 1] = evb[k - 1] - erefv;
/* Eev(k) are in eV */
	eev[k - 1] = ethre[k - 1] * auev;
/* Convert energy from eV to Rydberg */
	eryd[k - 1] = eev[k - 1] / rydev;
/* --- */
	dife = eev[k - 1] - eev[k - 2];
	s_wsfe(&io___54);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&eryd[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ethre[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eev[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dife, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

/* ---------------------------------------------------------------------- */
/*  Calculate scattering (vibrational) CHANNEL energies  Echan. */
/* ---------------------------------------------------------------------- */
    i__1 = ne;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Convert energy from eV to Hartree (a.u.) */
	eau[i__ - 1] = e[i__ - 1] / auev;
	s_wsfe(&io___56);
	do_fio(&c__1, (char *)&eau[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__2 = nv;
	for (k = 1; k <= i__2; ++k) {
	    echan[k - 1] = eau[i__ - 1] - ethre[k - 1];
	    eev[k - 1] = echan[k - 1] * auev;
	    s_wsfe(&io___58);
	    i__3 = k - 1;
	    do_fio(&c__1, (char *)&i__3, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&echan[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eev[k - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
    s_wsle(&io___59);
    e_wsle();
/* ---------------------------------------------------------------------- */
    if (mvr == 1) {
	goto L900;
    }
/* ====================================================================== */
/*  Calculate vibrational-rotational THREshold energies  Evr. */
/* ---------------------------------------------------------------------- */
L50:
    s_wsfe(&io___60);
    e_wsfe();
/* ====================================================================== */
    evr0 = erefr;
    if (nj[0] > 0) {
	i__1 = nv;
	for (k = 1; k <= i__1; ++k) {
	    kv = k - 1;
	    njr = nj[k - 1];
	    devr = 0.;
	    i__2 = njr;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		ir = i__ - 1;
		evr[k + i__ * 200 - 201] -= evr0;
		if (i__ > 1) {
		    devr = evr[k + i__ * 200 - 201] - evr[k + (i__ - 1) * 200 
			    - 201];
		}
		evrev = evr[k + i__ * 200 - 201] * auev;
		devr *= auev;
		s_wsfe(&io___66);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ir, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evr[k + i__ * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&evrev, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&devr, (ftnlen)sizeof(doublereal));
		e_wsfe();
/* L60: */
	    }
	    s_wsle(&io___67);
	    e_wsle();
/* L65: */
	}
    }
/* ---------------------------------------------------------------------- */
/*  Calculate vibrational-rotational CHANNEL energies  Echan. */
/* ---------------------------------------------------------------------- */
    if (ne > 0) {
	i__1 = ne;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    eau[i__ - 1] = e[i__ - 1] / auev;
	    s_wsfe(&io___68);
	    do_fio(&c__1, (char *)&eau[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&e[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    i__2 = nv;
	    for (k = 1; k <= i__2; ++k) {
		kv = k - 1;
		njr = nj[k - 1];
		i__3 = njr;
		for (j = 1; j <= i__3; ++j) {
		    jr = j - 1;
		    echan[k - 1] = eau[i__ - 1] - evr[k + j * 200 - 201];
		    eev[k - 1] = echan[k - 1] * auev;
		    s_wsfe(&io___71);
		    do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jr, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&echan[k - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&eev[k - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
/* L70: */
		}
		s_wsle(&io___72);
		e_wsle();
/* L75: */
	    }
/* L80: */
	}
    }
/* --- */
    s_wsfe(&io___73);
    e_wsfe();
    kk = 0;
    i__1 = nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (evb[i__ - 1] < evb[i__ - 2] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___75);
	    e_wsle();
	}
	s_wsfe(&io___76);
	i__2 = i__ - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&evb[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* - */
    s_wsfe(&io___77);
    e_wsfe();
    kk = 0;
    i__1 = nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (evb[i__ - 1] < evb[i__ - 2] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___78);
	    e_wsle();
	}
	s_wsfe(&io___79);
	i__2 = i__ - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	d__1 = evb[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* - */
    s_wsfe(&io___80);
    e_wsfe();
    i__1 = nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dif = evb[i__ - 1] - evb[i__ - 2];
	if (i__ == 1) {
	    dif = 0.;
	}
	s_wsfe(&io___82);
	i__2 = i__ - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	d__1 = dif * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* ---------------------------------------------------------------------- */

/* 240 format(//12x,'* Vibrational energies *'//, */
/*    #3x,'  v       Evib(cm-1)    Delta_Evib(cm-1) ',/) */
/* ---------------------------------------------------------------------- */
L900:
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */


/* ========== */
/* Subroutine */ int vrener_(integer *nv, integer *nj, integer *lj)
{
    /* Format strings */
    static char fmt_800[] = "(///13x,\002=#=  OUTPUT from \"VRener.f\"  =#="
	    " \002,//6x,\002The switches for NUMerical derivatives & E_vj :"
	    " \002,//2x,\002[   Mwe = 1, New  We = We + bryd ;  = 0, We = We "
	    ". \002,/6x,\002Ner = No of f_n's used to cal. ro-vib constants."
	    " \002,//12x,\002Use following V(R) to cal. n_th force constants "
	    "f_n's.\002,/8x,\002|\002,/8x,\002| = 1, use Vmorse as V(R).\002,"
	    "/8x,\002|\002,/8x,\002| = 2, use Vryd(R) = -Db *[1/ba + a*x ]*ex"
	    "p(-a1*x)\002,7x,\0021.)\002,/8x,\002|\002,16x,\002ba==beta;  Db="
	    "=De*ba;  a1==a(1)*ba \002,/8x,\002|\002,16x,\002a = b * dsqrt( a"
	    "mu/De ) ;   x = R - Re.\002,/8x,\002|\002,16x,\002b = We + bryd "
	    ";  originally,  b = We.\002,/8x,\002|\002,18(\002-\002),/8x,\002"
	    "| The GENERALIZED Rydberg potentials are defined as : \002,/8x"
	    ",\002|\002,/8x,\002|\002,6x,\002Vryd(R) = -Db *[1/ba + a*x + d*a"
	    "*x*x ] *exp(-a1*x)  2.) \002,/8x,\002|\002,/8x,\002|\002,6x,\002"
	    "Uryd(R)=-Db*[1/ba+a**d * M_SUM_k=1 x**k]*exp(-a1*x) 3.) \002,/8x,"
	    "\002|\002,/3x,\002Ny = |\002,6x,\002Uryd(R)=-Db*[1/ba+M_SUM_k=1 "
	    "a**k * x**k]*exp(-a1*x) 4.) \002,/8x,\002|\002,/8x,\002|\002,7x"
	    ",\002|\002,/8x,\002|\002,7x,\002| M = 2,\002,11x,\002use Eq 2.)"
	    ";\002,/8x,\002| = 3 ->| M > 2, d > 0.0,  use Eq 3.); \002,/8x"
	    ",\002|       | M > 2, d = 0.0,  use Eq 4.). \002,/8x,\002|      "
	    " |\002,/8x,\002|\002,18(\002-\002),/8x,\002| = 4, use\002,/8x"
	    ",\002|\002,6x,\002Vryd(R)=-Db*[1/ba + a* M_SUM_k=1 x**k]*exp(-a1"
	    "*dryd*x) 5.) \002,/8x,\002|       |\002,/8x,\002|\002,18(\002"
	    "-\002),/8x,\002| = 5, use\002,/8x,\002|\002,6x,\002Vryd(R)= Db*["
	    "exp(-a*x)/ba - (a+bryd)*x]*exp(-a*x)      6.) \002,/8x,\002|\002"
	    ",/8x,\002| = 6, use Vpseudo_gaussian  as V(R). \002,//4x,\002 br"
	    "yd - The variational constant used to adjust Vryd(R), \002,/4x"
	    ",\002 dryd = d = integer k OR a positive REAL number , \002,/6x"
	    ",\002        As dryd=0.0,  code uses  a**k  but NOT  a**d \002,/"
	    "4x,\002 Mryd = M = number of terms in summation of Uryd. \002,/6"
	    "x,\002    As Mryd=1, dryd=1 you get ORIginal Vrydberg(R);\002,/6"
	    "x,\002    As Mryd=2, & k=2, coeffi. of x**2 is  dryd*a  .\002,//"
	    "6x,\002    bryd, dryd, Mryd are meaningless for Ny = 1 & 6 .   "
	    "]\002,//10x,\002Mwe   Ner  Ny         bryd         dryd    Mryd"
	    " \002,/10x,i2,4x,i2,3x,i2,2x,f16.12,2x,f8.5,2x,i3///3x,\002[ hs1"
	    ", hs2 --, Estimated initial stepsize used by the \002,/3x,\002  "
	    "               numerical derivative code \"dfirdr\" ; \002,/3x"
	    ",\002                 hs1 for Vrydberg,   hs2 for Vp_g  .   ]"
	    "\002,//16x,\002hs1 =\002,f8.4,4x,\002hs2 =\002,f8.4,//)";
    static char fmt_804[] = "(/3x,\002[ nw0 = 1, E(v)=Enew(v)+We*(v+0.5"
	    ")\002,\002+WeXe*(v+0.5)**2+...\002,//3x,\002      = 0, E(v)=We*("
	    "v+0.5)+WeXe*(v+0.5)**2+...        \002,//3x,\002  nvd = # of vib"
	    ". states used in comparison with the \002,/3x,\002        input "
	    "Ev or energy differencies. nvd =< n_expt \002,/3x,\002        wh"
	    "ich is the # of input (experimental) Ev OR \002,/3x,\002        "
	    "energy differencies for comparison.            ] \002,//20x,\002"
	    "nw0   nvd \002,/20x,i2,5x,i2/)";
    static char fmt_820[] = "(//3x,\002Switches for scale SOME calc. VIB-ROT"
	    " constants :\002,//3x,\002[ aye, ..., are = 0, Zero calc. VIB. c"
	    "onstants \002,/3x,\002                     WeYe, WeZe, WeTe, WeS"
	    "e, WeRe ; \002,/3x,\002                = 1, Do NOT change sign o"
	    "f constants; \002,/3x,\002                =-1, Change the sign o"
	    "f VIB. constants. \002,//3x,\002  abe, aae, age, \002,/3x,\002  "
	    "ae3  ---  ae7 = 0, Zero calc. ROT. constants  \002,/3x,\002     "
	    "                B_e,Alpha_e,Gamma_e,Eta3,...,Eta7 ; \002,/3x,"
	    "\002                = 1, Do NOT change sign of constants; \002,/"
	    "3x,\002                =-1, Change the sign of ROT. constants."
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
	    ".1,1x,f4.1,////5x,\002br4,br6,br8,br10,br12,br14 = Scaling switc"
	    "hes for long \002,/5x,\002range corce const f_n(long;R=Re) = d~n"
	    "/(dr~n)[1/R**k]  \002,/5x,\002which will be added into f_n(R=Re)"
	    " = V_rydberg~n(R=Re) \002,/5x,\002and are important for Wande-Wa"
	    "als molecules and some \002,/5x,\002quasi-stable molecular elect"
	    "ronic states . \002,//5x,\002f_n(long;R=Re) = (-1.0)**n *(k+n-1)"
	    "!/[(k-1)!*R**(k+n)] \002,/8x,\002brn = 1.0, Add f_n(long;R=Re); "
	    "= 0.0, do NOT add. \002,//4x,\002           br4   br6   br8  br1"
	    "0  br12  br14 \002,/14x,f4.1,2x,f4.1,2x,f4.1,2x,f4.1,2x,f4.1,2x,"
	    "f4.1,/)";
    static char fmt_534[] = "(//3x,\002The dissociation energy    De  = \002"
	    ",1pe16.8,//,\002   Equili. intern. distance   Re  = \002,1pe16.8"
	    ",//,\002   Rydberg exponental para. alpha = \002,1pe16.8,/,\002 "
	    "    alpha = We * dsqrt( amu/De ) \002,//,\002    Morse  exponent"
	    "al para. beta0 = \002,1pe16.8,/,\002     beta0 = We * dsqrt( amu"
	    "/(2*De) ) \002,///,\002   f_(n) are from ANAlytical derivative c"
	    "ode. \002,//9x,\002  err are the errors of f_(n).\002,///8x,\002"
	    "   The nth force constants are : \002,//5x,\002  n   f_(n; Har/a"
	    "o**n)     err(f) \002/)";
    static char fmt_550[] = "(5x,i3,2x,3(1pe16.8,x))";
    static char fmt_536[] = "(///4x,\002The nth force constants in other uni"
	    "ts are : \002,//3x,\002    The force constatns are from analytic"
	    "al \002,/3x,\002       derivatives of Rydberg potential. \002,//"
	    "3x,\002      1 aJ = 1 attojoule = 0.229371 Har. \002,/3x,\002   "
	    "          1 ao = 0.529177249 A \002,//5x,\002  n   f_(n; Har/ao*"
	    "*n)  f_(n; aJ/A**n) \002/)";
    static char fmt_750[] = "(///3x,\002Force constants ( h ) are from numer"
	    "ical \002,\002derivative code  \"dfridr\" ;\002,/3x,\002Force co"
	    "nstants ( p & f ) are from ANAlytical formulae :\002,/3x,\002   "
	    "      For  V_p-g(R),  2 =< n =< 11  ONLY  . \002,//5x,\002From  "
	    "  V_morse(R)      V_rydberg(R)       V_p-g(R)  \002,/5x,\002  n "
	    "  p(n; Har/ao**n)  f(n; Har/ao**n)  h(n; Har/ao**n) \002/)";
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
	    "V(Rmax) = De \002,/10x,\002 Nryd - No of f_n's used to cal. V(Rm"
	    "ax) \002,//8x,\002De\002,11x,\002Rmax\002,10x,\002Rless\002,11x"
	    ",\002Dconv\002,8x,\002Nryd\002,/1x,4(1pe14.6,x),3x,i3,///\002  "
	    "n\002,5x,\002De_g(m)\002,7x,\002De_l(m)\002,7x,\002De_g(r)\002,7"
	    "x,\002De_l(r)\002,4x,\002{De_g-De_l}_r \002,/)";
    static char fmt_740[] = "(i3,x,5(1pe14.6))";
    static char fmt_732[] = "(36x,\002V_rydberg(R) converged \002,/)";
    static char fmt_734[] = "(9x,\002V_morse(R) converged \002,/)";
    static char fmt_742[] = "(//17x,\002V(R->inf=Rmax) = De  is reached with"
	    " : \002,//23x,\002  n\002,5x,\002De(Rydbg; a.u)\002,/23x,i3,3x,1"
	    "pe16.8,//23x,\002  n\002,5x,\002De(Morse; a.u)\002,/23x,i3,3x,1p"
	    "e16.8,/)";
    static char fmt_745[] = "(//22x,\002Input We         Corrected We\002,/2"
	    "2x,8(\002-\002),9x,12(\002-\002),/17x,1pe16.8,3x,1pe16.8)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen),
	     e_wsfe(void);
    double sqrt(doublereal), pow_di(doublereal *, integer *);
    integer s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    extern /* Subroutine */ int vpotdata_(void);
    static integer i__, k;
    static doublereal ge[300], bk, gg[300];
    static integer kc, ij, kj, kk;
    static doublereal rg;
    static integer km, ks;
    static doublereal rl, ge1[300], ge2[300], gg1[300], gg2[300], hs1, hs2, 
	    ggf[300], vde;
    static integer ner;
    static doublereal vdm, err, ery, vdg0, vmg0, vdeg, vdel;
    extern doublereal fryd_(integer *, doublereal *);
    static doublereal rmax;
    static integer nryd;
    static doublereal vmsg, vmsl, beta0, dconv;
    extern /* Subroutine */ int convj_(doublereal *, integer *);
    static doublereal rless, alpha0, aotoa0;
    extern doublereal dfridr_(integer *, integer *, doublereal *, doublereal *
	    , doublereal *);
    extern /* Subroutine */ int vjcoef_(void);
    static doublereal ajtoau;
    extern doublereal fmorse_(integer *);

    /* Fortran I/O blocks */
    static cilist io___83 = { 0, 5, 0, 0, 0 };
    static cilist io___87 = { 0, 5, 0, 0, 0 };
    static cilist io___92 = { 0, 5, 0, 0, 0 };
    static cilist io___93 = { 0, 5, 0, 0, 0 };
    static cilist io___96 = { 0, 0, 0, fmt_800, 0 };
    static cilist io___97 = { 0, 0, 0, fmt_804, 0 };
    static cilist io___98 = { 0, 0, 0, fmt_820, 0 };
    static cilist io___113 = { 0, 6, 0, fmt_534, 0 };
    static cilist io___114 = { 0, 35, 0, fmt_534, 0 };
    static cilist io___115 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___116 = { 0, 35, 0, fmt_550, 0 };
    static cilist io___117 = { 0, 6, 0, fmt_536, 0 };
    static cilist io___119 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___120 = { 0, 6, 0, fmt_750, 0 };
    static cilist io___121 = { 0, 35, 0, fmt_750, 0 };
    static cilist io___122 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___123 = { 0, 35, 0, fmt_550, 0 };
    static cilist io___124 = { 0, 6, 0, 0, 0 };
    static cilist io___125 = { 0, 35, 0, 0, 0 };
    static cilist io___126 = { 0, 6, 0, fmt_760, 0 };
    static cilist io___127 = { 0, 35, 0, fmt_760, 0 };
    static cilist io___128 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___129 = { 0, 35, 0, fmt_550, 0 };
    static cilist io___139 = { 0, 6, 0, fmt_730, 0 };
    static cilist io___140 = { 0, 35, 0, fmt_730, 0 };
    static cilist io___143 = { 0, 6, 0, fmt_740, 0 };
    static cilist io___144 = { 0, 35, 0, fmt_740, 0 };
    static cilist io___145 = { 0, 6, 0, fmt_732, 0 };
    static cilist io___146 = { 0, 35, 0, fmt_732, 0 };
    static cilist io___149 = { 0, 6, 0, fmt_732, 0 };
    static cilist io___150 = { 0, 35, 0, fmt_732, 0 };
    static cilist io___151 = { 0, 6, 0, fmt_734, 0 };
    static cilist io___152 = { 0, 35, 0, fmt_734, 0 };
    static cilist io___155 = { 0, 6, 0, fmt_734, 0 };
    static cilist io___156 = { 0, 35, 0, fmt_734, 0 };
    static cilist io___157 = { 0, 6, 0, fmt_742, 0 };
    static cilist io___158 = { 0, 35, 0, fmt_742, 0 };
    static cilist io___159 = { 0, 6, 0, fmt_745, 0 };


/* ----------------------------------------------------------------- */
/*     parameter  (Na=2500, Nd=300) */
/* ----------------------------------------------------------------- */
/*    Read data : */
/*  Ner = number of force constants f's wanted. */

/*     Use following V(R) to calculate n_th force constants f_n's. */
/*      | */
/*      | = 1, use Vmorse as V(R). */
/*      | */
/*      | = 2, use Vryd(R)=-De*ba*[1/ba + a*x ]*exp(-a*ba*x)      1.) */
/*      |               a = b * dsqrt( amu/De ) ;   x = R - Re. */
/*      |             b=We + bryd ;  originally, b=We. */
/*      |             ba == beta is the potential WIDTH parameter. */
/*      |------------------ */
/*      | The GENERALIZED Rydberg potentials are defined as : */
/*      | */
/*      |      Vryd(R)=-De*ba*[1/ba +a*x +d*a*x*x]*exp(-a*ba*x)   2.) */
/*      | */
/*      |      Uryd(R)=-De*ba*[1/ba+a**d*M_SUM_k=1 x**k]*exp(-a*ba*x)  3.) */
/*      | */
/* Ny = |      Uryd(R)=-De*ba*[1/ba+M_SUM_k=1 a**k*x**k]*exp(-a*ba*x)  4.) */
/*      |       | */
/*      |       | M = 2,           use Eq 2.); */
/*      | = 3 ->| M > 2, d > 0.0,  use Eq 3.); */
/*      |       | M > 2, d = 0.0,  use Eq 4.). */
/*      |       | */
/*      |------------------ */
/*      | = 4, use */
/*      |    Vryd(R)=-De*ba*[1/ba+a*M_SUM_k=1 x**k]*exp(-a*ba*dryd*x)  5.) */
/*      | */
/*      |------------------ */
/*      | = 5, use */
/*      |      func(R)=[exp(-a*x)/beta - (a+bryd)*x]*exp(-a*x) */
/*      |      Vryd(R)= De*beta*func(R)                                6.) */
/*      | */
/*      | = 6, use Vpseudo_gaussian  as V(R). */

/* bryd - The variational constant used to adjust Vryd(R). */
/* dryd = d = k or a positive REAL number. */
/* Mryd = M = number of terms in summation. */
/*              As dryd=0.0,  code use  a**k  but NOT a**d; */
/*              As dryd=1, Mryd=1, you get ORIginal Vrydberg(R). */
/*      bryd, dryd, Mryd are meaningless for Vmorse & Vp_g. */

/*  hs -- Estimated initial stepsize used by code "dfridr". */
/*            Good range for  hs :  0.001 --> 4.0  for H_2. */
/*              hs1 for Vrydberg;   hs2 for V_p-g . */

/*   Mwe = 1, New  We = We + bryd ;  = 0, We = We. */
/*  Nryd = Number of f_n's calculated using Vryd(R) and */
/*           is used to cal. V(R->Rmax=inf). */
/*  Rmax = The maximum R value for  V(R->Rmax=inf). */
/* Rless = The R value in    Rbig = Rmax +/- Rless. */
/* Dconv = The tolerance used to check if V(Rmax) = De. */

/*   nw0 = 1, E(v) = w0 + (We + We0)*(v+0.5) + WeXe*(v+0.5)**2 + ... */
/*       = 0, E(v) = We*(v+0.5) + WeXe*(v+0.5)**2 + ... */
/*   nvd = number of vib. states used in comparison with */
/*           the input Ev or energy differencies. nvd =< n_expt. */
/*           nvd is slightly smaller than n_expt which is the */
/*           no. of input Ev OR energy differencies for comparison. */

/* br4,br6,br8,br10,br12,br14 = The scaling switches for the */
/* long range force const f_n(long;R=Re) = d~n/(dr~n)[1/R**k] */
/* which will be added into f_n(R=Re) = V_rydberg~n(R=Re). */
/*   f_n(long;R=Re) = (-1.0)**n *(k+n-1)!/[(k-1)!*R**(k+n)] */
/* which are important for Wande-Waals molecules and */
/* quasi-stable molecules. */
/*    brn = 1.0, Add f_n(long;R=Re); = 1.0, do NOT add. */

/* ========================================================================= */
    /* Parameter adjustments */
    --nj;

    /* Function Body */
    s_rsle(&io___83);
    do_lio(&c__3, &c__1, (char *)&ner, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&fms_1.ny, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&spectrb_1.bryd, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&spectrb_1.dryd, (ftnlen)sizeof(doublereal));
    do_lio(&c__3, &c__1, (char *)&fms_1.mryd, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&hs1, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&hs2, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___87);
    do_lio(&c__3, &c__1, (char *)&fms_1.mwe, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&nryd, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&rmax, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&rless, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&dconv, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_rsle(&io___92);
    do_lio(&c__3, &c__1, (char *)&fms_1.nw0, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&fms_1.nvd, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___93);
    do_lio(&c__5, &c__1, (char *)&bdt_1.br4, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&bdt_1.br6, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&bdt_1.br8, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&bdt_1.br10, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&bdt_1.br12, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&bdt_1.br14, (ftnlen)sizeof(doublereal));
    e_rsle();
    ij = 6;
    for (i__ = 1; i__ <= 2; ++i__) {
	io___96.ciunit = ij;
	s_wsfe(&io___96);
	do_fio(&c__1, (char *)&fms_1.mwe, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ner, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&fms_1.ny, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&spectrb_1.bryd, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectrb_1.dryd, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&fms_1.mryd, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&hs1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&hs2, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___97.ciunit = ij;
	s_wsfe(&io___97);
	do_fio(&c__1, (char *)&fms_1.nw0, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&fms_1.nvd, (ftnlen)sizeof(integer));
	e_wsfe();
	io___98.ciunit = ij;
	s_wsfe(&io___98);
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
	do_fio(&c__1, (char *)&bdt_1.br4, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bdt_1.br6, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bdt_1.br8, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bdt_1.br10, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bdt_1.br12, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bdt_1.br14, (ftnlen)sizeof(doublereal));
	e_wsfe();
	if (i__ == 1) {
	    ij = 35;
	}
    }
/* -------------------------------------------------------- */
/*  Prepare CONVERSION factors : */
/* -------------------------------------------------------- */
    aotoa0 = .529177249;
    ajtoau = .229371;
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
/* in R0 over which function fpot changes substantially. */
/*   err -- An estimate of the error in the derivative. */
/* ----- */
/*  Calculate numerical "force constants" for : */
/*    V_morse(R),  as  ks=1;     V_rydberg,  as  ks=2; */
/*    V_pg(R),     as  ks=3. */
/* -------------------------------------------------------- */
    for (ks = 1; ks <= 3; ++ks) {
	i__1 = nryd + 1;
	for (k = 2; k <= i__1; ++k) {
	    if (ks == 1) {
		gg1[k - 1] = fmorse_(&k);
		ge1[k - 1] = 0.;
		if (fms_1.ny == 1) {
		    gg[k - 1] = gg1[k - 1];
		    ge[k - 1] = ge1[k - 1];
		}
	    } else if (ks == 2) {
		ggf[k - 1] = fryd_(&k, &spectra_1.re);
		if (fms_1.ny >= 2 && fms_1.ny <= 8) {
		    gg[k - 1] = ggf[k - 1];
		    ge[k - 1] = 0.;
		}
	    } else if (ks == 3) {
		gg2[k - 1] = dfridr_(&k, &ks, &spectra_1.re, &hs2, &err);
		ge2[k - 1] = err;
		if (fms_1.ny == 9) {
		    gg[k - 1] = gg2[k - 1];
		    ge[k - 1] = ge2[k - 1];
		}
	    }
	}

    }
/* --------------------------------------------------- */
    alpha0 = spectra_1.we * sqrt(spectra_1.amu / spectra_1.de);
    beta0 = spectra_1.we * sqrt(spectra_1.amu / (spectra_1.de * 2.));
    s_wsfe(&io___113);
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&alpha0, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&beta0, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___114);
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&spectra_1.re, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&alpha0, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&beta0, (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = ner + 1;
    for (k = 2; k <= i__1; ++k) {
	s_wsfe(&io___115);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&gg[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ge[k - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___116);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&gg[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ge[k - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* --- */
    s_wsfe(&io___117);
    e_wsfe();
    i__1 = ner + 1;
    for (k = 2; k <= i__1; ++k) {
/*               ggk =  gg(k)/(aJtoau * aotoA0**k) */
	ery = ggf[k - 1] / (ajtoau * pow_di(&aotoa0, &k));
	s_wsfe(&io___119);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ggf[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ery, (ftnlen)sizeof(doublereal));
	e_wsfe();
/*              write(6,550) k, ggk, ery */
    }
/* --------------------------------------------------- */
/* Print numerical "force constants" */
/* --------------------------------------------------- */
    s_wsfe(&io___120);
    e_wsfe();
    s_wsfe(&io___121);
    e_wsfe();
    i__1 = nryd;
    for (k = 2; k <= i__1; ++k) {
	s_wsfe(&io___122);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&gg1[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&gg[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&gg2[k - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___123);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&gg1[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&gg[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&gg2[k - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	if (k == ner + 1) {
	    s_wsle(&io___124);
	    e_wsle();
	    s_wsle(&io___125);
	    e_wsle();
	}
    }
    s_wsfe(&io___126);
    e_wsfe();
    s_wsfe(&io___127);
    e_wsfe();
    i__1 = ner + 1;
    for (k = 2; k <= i__1; ++k) {
	s_wsfe(&io___128);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ge1[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ge[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ge2[k - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___129);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ge1[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ge[k - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ge2[k - 1], (ftnlen)sizeof(doublereal));
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
    s_wsfe(&io___139);
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rmax, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rless, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dconv, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&nryd, (ftnlen)sizeof(integer));
    e_wsfe();
    s_wsfe(&io___140);
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rmax, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rless, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dconv, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&nryd, (ftnlen)sizeof(integer));
    e_wsfe();
    i__1 = nryd;
    for (k = 2; k <= i__1; ++k) {
	bk *= k;
	vmg0 = vmsg;
	d__1 = rg - spectra_1.re;
	vmsg += gg1[k - 1] * pow_di(&d__1, &k) / bk;
	d__1 = rl - spectra_1.re;
	vmsl += gg1[k - 1] * pow_di(&d__1, &k) / bk;

	vdg0 = vdeg;
	d__1 = rg - spectra_1.re;
	vdeg += ggf[k - 1] * pow_di(&d__1, &k) / bk;
	d__1 = rl - spectra_1.re;
	vdel += ggf[k - 1] * pow_di(&d__1, &k) / bk;
	s_wsfe(&io___143);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vmsg, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vmsl, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vdeg, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vdel, (ftnlen)sizeof(doublereal));
	d__1 = vdeg - vdel;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___144);
	do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&vmsg, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vmsl, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vdeg, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vdel, (ftnlen)sizeof(doublereal));
	d__1 = vdeg - vdel;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
	if ((d__1 = vdeg - vdel, abs(d__1)) < dconv && kk == 0) {
	    s_wsfe(&io___145);
	    e_wsfe();
	    s_wsfe(&io___146);
	    e_wsfe();
	    kk = 1;
	    kc = k;
	    vde = vdeg;
	}
	if ((d__1 = vdeg - vdg0, abs(d__1)) < dconv && kk == 0) {
	    s_wsfe(&io___149);
	    e_wsfe();
	    s_wsfe(&io___150);
	    e_wsfe();
	    kk = 1;
	    kc = k;
	    vde = vdeg;
	}
	if ((d__1 = vmsg - vmsl, abs(d__1)) < dconv && kj == 0) {
	    s_wsfe(&io___151);
	    e_wsfe();
	    s_wsfe(&io___152);
	    e_wsfe();
	    kj = 1;
	    km = k;
	    vdm = vmsg;
	}
	if ((d__1 = vmsg - vmg0, abs(d__1)) < dconv && kj == 0) {
	    s_wsfe(&io___155);
	    e_wsfe();
	    s_wsfe(&io___156);
	    e_wsfe();
	    kj = 1;
	    km = k;
	    vdm = vmsg;
	}
    }
    s_wsfe(&io___157);
    do_fio(&c__1, (char *)&kc, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&vde, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&km, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&vdm, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___158);
    do_fio(&c__1, (char *)&kc, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&vde, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&km, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&vdm, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* ================================================= */
    if (ner > 0) {
/* ------------------------------------------------- */
/*  Define NEW vibrational constant  We if wanted */
/* ------------------------------------------------- */
	if (fms_1.mwe > 0) {
	    spectra_1.we += spectrb_1.bryd;
	    s_wsfe(&io___159);
	    do_fio(&c__1, (char *)&spectra_1.wee, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* ------------------------------------------------- */
/*  Calculate coefficients for ro-vib constants */
/* ------------------------------------------------- */
	vjcoef_();
/* ------------------------------------------------- */
/*  Calculate vib.-rotational constants for E_vj */
/* ------------------------------------------------- */
	convj_(gg, &c__300);
    }
/* ==================================================================== */
/*    #//,'   f_(n) are from numerical derivative code. ', */
/*    #//3x,' e_(n) are the force constatns from analytical ', */
/*    #//5x,"  n    f_(n; aJ/A**n)   e_(n; aJ/A**n) "/) */
/*    #//5x,"  n    f_(n; aJ/A**n)      err(f) "/) */
/* L560: */

/* L900: */
    return 0;
} /* vrener_ */

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
/*     dimension  a(NTAB,NTAB) */
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
    vnumpot_1.betap = sqrt(vnumpot_1.ff2 / (spectra_1.de * 2.));
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
    ret_val = pow_ii(&c_n1, n) * (pow_di(&c_b400, &i__1) - 1.) * pow_di(&
	    vnumpot_1.betap, &i__2) * vnumpot_1.ff2;
/* ---------------------------------------------------------- */
    return ret_val;
} /* fmorse_ */


doublereal fryd_(integer *n, doublereal *r__)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double sqrt(doublereal), exp(doublereal), pow_di(doublereal *, integer *),
	     pow_dd(doublereal *, doublereal *);
    integer pow_ii(integer *, integer *);

    /* Local variables */
    static doublereal a, b;
    static integer i__, k;
    static doublereal x, b0, a1, a2, b1, b2, b3, d1, e4, e6, e8, e10, eb, ai, 
	    gb, dd, bk, di, bm, bn, e12, dn, e14, ep, vm, vr, eb2, fn0, r4n, 
	    r6n, r8n, ane, adr, cni, bkn, bni, bnn, r10n, pki, r12n, tem, 
	    r14n, fnx, vmi, vmn, vri, vrn;
    extern doublereal facx_(integer *);
    static doublereal pref, temp;

/* ---------------------------------------------------------- */
/*   The nth derivative of Rydberg potentials;  n >= 2. */
/* ---------------------------------------------------------- */
    a = (spectra_1.we + spectrb_1.bryd) * sqrt(spectra_1.amu / spectra_1.de);
    ep = exp(-a * bdt_1.beta * (*r__ - spectra_1.re));
    if (fms_1.ny == 2) {
	vr = -spectra_1.de * bdt_1.beta * (1. / bdt_1.beta + a * (*r__ - 
		spectra_1.re)) * ep;
	d__1 = a * bdt_1.beta;
	ret_val = pow_di(&c_b402, n) * pow_di(&d__1, n) * (*n * spectra_1.de *
		 ep + vr);
	goto L50;
/* ---------------------------------------------------------- */
/*   The nth derivative of the GENERALIZED Rydberg */
/* potentials. */
/* ---------------------------------------------------------- */
    } else if (fms_1.ny == 3 || fms_1.ny == 4) {
	fnx = 0.;
	x = *r__ - spectra_1.re;
	bnn = facx_(n);
	if (fms_1.ny == 3) {
	    adr = pow_dd(&a, &spectrb_1.dryd);
	    if (fms_1.mryd == 2) {
		adr = a;
	    }
	} else if (fms_1.ny == 4) {
	    ep = exp(-a * spectrb_1.dryd * bdt_1.beta * x);
	    adr = a;
	}
	d__1 = a * -1. * bdt_1.beta;
	ane = 1. / bdt_1.beta * ep * pow_di(&d__1, n);
	i__1 = fms_1.mryd;
	for (k = 1; k <= i__1; ++k) {
	    if (fms_1.ny == 3 && spectrb_1.dryd == 0.) {
		adr = pow_di(&a, &k);
	    }
	    bk = facx_(&k);
	    fn0 = 0.;
	    i__2 = *n;
	    for (i__ = 0; i__ <= i__2; ++i__) {
		if (i__ >= *n - k) {
		    i__3 = *n - i__;
		    bn = facx_(&i__3);
		    b0 = facx_(&i__);
		    cni = bnn / (bn * b0);
		    if (fms_1.mryd == 2 && k == 2) {
			if (fms_1.ny == 3) {
			    adr = spectrb_1.dryd * a;
			}
		    }
		    i__3 = k - *n + i__;
		    bkn = facx_(&i__3);
		    pki = bk / bkn;
		    d__1 = a * -1. * bdt_1.beta;
		    ai = pow_di(&d__1, &i__);
		    i__3 = k - *n + i__;
		    fn0 += cni * pki * ai * ep * pow_di(&x, &i__3);
		}
/* L30: */
	    }
	    fnx += adr * fn0;
/* L40: */
	}
	ret_val = -spectra_1.de * bdt_1.beta * (ane + fnx);
	goto L50;
/* --- */
    } else if (fms_1.ny >= 5 && fms_1.ny <= 8) {
	a = spectra_1.we * sqrt(spectra_1.amu / spectra_1.de);
	x = *r__ - spectra_1.re;
	ep = exp(-a * x);
	d__1 = -a;
	a1 = pow_di(&d__1, n);
	d__1 = -a;
	i__1 = *n - 1;
	a2 = pow_di(&d__1, &i__1);
	b = spectra_1.we * sqrt(spectra_1.amu / (spectra_1.de * 2.));
	eb = exp(-b * x);
	eb2 = exp(b * -2. * x);
	vr = -spectra_1.de * ((a - spectrb_1.bryd) * x + 1.) * ep;
	temp = a1 * vr - *n * a2 * (a - spectrb_1.bryd) * spectra_1.de * ep;
/* - */
	if (fms_1.ny == 5) {
	    ret_val = temp;
	    goto L50;
	}
/* - */
	if (fms_1.ny == 6) {
	    d__1 = -b;
	    b1 = pow_di(&d__1, n);
	    d__1 = -b;
	    i__1 = *n - 1;
	    b2 = pow_di(&d__1, &i__1);
	    d__1 = b * -2.;
	    b3 = pow_di(&d__1, n);
	    ret_val = b3 * eb2 - *n * spectrb_1.bryd * b2 * eb - b1 * (
		    spectrb_1.bryd * x + 2.) * eb;
	    ret_val = (spectra_1.de * ret_val + temp) / 2.;
	    goto L50;
	}
/* - */
/*         bm = (-bryd)**Mryd */
/*         gb = dexp(-bm*x/Re) */
/* - */
	bm = pow_di(&spectrb_1.bryd, &fms_1.mryd);
	gb = exp(-x / spectra_1.re);
	if (fms_1.ny == 7 || fms_1.ny == 8) {
	    vm = spectra_1.de * (eb2 - eb * 2.);
	    vr = -spectra_1.de * (a * x + 1.) * ep;
	    b0 = bm * (2. - gb);
/*          b0 = bryd*gb */
	    vrn = a1 * (vr + *n * spectra_1.de * ep);
	    i__1 = *n - 1;
	    d__1 = -b;
	    vmn = spectra_1.de * 2. * (pow_ii(&c__2, &i__1) * eb2 - eb) * 
		    pow_di(&d__1, n);
	    dn = facx_(n);
	    tem = 0.;

	    i__1 = *n;
	    for (i__ = 0; i__ <= i__1; ++i__) {
		di = facx_(&i__);
		i__2 = *n - i__;
		d1 = facx_(&i__2);
		dd = dn / (d1 * di);

/*             bni = bryd*gb*( - bm/Re )**(n-i) */
/*             bni = bryd*gb*( -1.0/Re )**(n-i) */

		if (i__ > 0) {
		    d__1 = -a;
		    vri = (vr + i__ * spectra_1.de * ep) * pow_di(&d__1, &i__)
			    ;
		    i__2 = i__ - 1;
		    d__1 = -b;
		    vmi = spectra_1.de * 2. * (pow_ii(&c__2, &i__2) * eb2 - 
			    eb) * pow_di(&d__1, &i__);
		} else {
		    vri = vr;
		    vmi = vm;
		}
		if (i__ < *n) {
/*             bni = bryd*bm*( 1.0/Re )**(n-i) */
		    d__1 = 1. / spectra_1.re;
		    i__2 = *n - i__;
		    bni = spectrb_1.bryd * pow_di(&d__1, &i__2);
		} else {
		    bni = b0;
		}
		if (fms_1.ny == 7) {
		    tem += dd * bni * (vri - vmi);
		}
		if (fms_1.ny == 8) {
		    tem += dd * bni * (vmi - vri);
		}
	    }

	    if (fms_1.ny == 7) {
		ret_val = vrn + tem;
	    }
	    if (fms_1.ny == 8) {
		ret_val = vmn + tem;
	    }
	}
/* --- */
    }
/* -------------------------------------------------------- */
/*   Add  d~n/(dR~n)[1/R**n] terms;  n = 4, 6, ..., 14. */
/* These terms are important for Wande-Waals molecules */
/* and quasi-stable molecules. */
/* f_n(long;R=Re) = (-1.0)**n *(k+n-1)!/[(k-1)!*R**(k+n)] */
/* -------------------------------------------------------- */
L50:
    r4n = 0.;
    r6n = 0.;
    r8n = 0.;
    r10n = 0.;
    r12n = 0.;
    r14n = 0.;
    temp = 0.;
    pref = pow_di(&c_b402, n);
    e4 = 6.;
    e6 = 120.;
    e8 = 5040.;
    e10 = 362880.;
    e12 = 39916800.;
    e14 = 6227020800.;
    if (bdt_1.br4 > 0.) {
	i__1 = *n + 3;
	i__2 = *n + 4;
	r4n = pref * facx_(&i__1) / (e4 * pow_di(r__, &i__2));
    }
    if (bdt_1.br6 > 0.) {
	i__1 = *n + 5;
	i__2 = *n + 6;
	r6n = pref * facx_(&i__1) / (e6 * pow_di(r__, &i__2));
    }
    if (bdt_1.br8 > 0.) {
	i__1 = *n + 7;
	i__2 = *n + 8;
	r8n = pref * facx_(&i__1) / (e8 * pow_di(r__, &i__2));
    }
    if (bdt_1.br10 > 0.) {
	i__1 = *n + 9;
	i__2 = *n + 10;
	r10n = pref * facx_(&i__1) / (e10 * pow_di(r__, &i__2));
    }
    if (bdt_1.br12 > 0.) {
	i__1 = *n + 11;
	i__2 = *n + 12;
	r12n = pref * facx_(&i__1) / (e12 * pow_di(r__, &i__2));
    }
    if (bdt_1.br14 > 0.) {
	i__1 = *n + 13;
	i__2 = *n + 14;
	r14n = pref * facx_(&i__1) / (e14 * pow_di(r__, &i__2));
    }
    temp = bdt_1.br4 * r4n + bdt_1.br6 * r6n + bdt_1.br8 * r8n + bdt_1.br10 * 
	    r10n;
    temp = temp + bdt_1.br12 * r12n + bdt_1.br14 * r14n;
    ret_val += spectrb_1.bryd * temp;
    return ret_val;
} /* fryd_ */


doublereal fpot_(doublereal *r__, integer *n, integer *ns)
{
    /* System generated locals */
    doublereal ret_val, d__1;

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
    ep = exp(-a * bdt_1.beta * (*r__ - spectra_1.re));
    vr = -spectra_1.de * bdt_1.beta * (1. / bdt_1.beta + a * (*r__ - 
	    spectra_1.re)) * ep;
    d__1 = -a * bdt_1.beta;
    ret_val = pow_di(&d__1, &m) * (m * spectra_1.de * ep + vr);
    goto L800;
/* ---------------------------------------------------------- */
/*   The nth derivative of V(Pseudo-Gaussian; R) ; */
/*        2 =< n =< 11  ONLY . */
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


/* Subroutine */ int calevj_(integer *nv, integer *nj, integer *lj, 
	doublereal *ev, doublereal *evj)
{
    /* Format strings */
    static char fmt_400[] = "(///6x,\002--- Ro-vibrational constants & energ"
	    "ies --- \002,//2x,\002All calculated constants are the functions"
	    " of the input \002,/2x,\002\"We\", \"Re\", & \"mu\",  and are ba"
	    "sed on the perturbation \002,/2x,\002theory,  except that \"We\""
	    " is the input value.\002,///8x,\002**  The vibrational constants"
	    " (in a.u.) are  ** \002,/6x,\002evaluated using force constants "
	    "from ANAlytical deriv.\002,//6x,\002  { If mtyp = 1, Consts(calc"
	    ".) == Consts(input) } \002,//10x,\002[ Error% = 100.0*(|Calc.| -"
	    " |Input|)/Calc. ] \002)";
    static char fmt_410[] = "(/5x,\002          Calculated              Inpu"
	    "t \002,13x,\002Error% \002,//3x,\002 W0  = \002,1pe20.12,/3x,"
	    "\002 We0 = \002,1pe20.12,/3x,\002 We  = \002,1pe20.12,2x,1pe20.1"
	    "2,/3x,\002WeXe = \002,1pe20.12,2x,1pe20.12,2x,1pe13.5,/3x,\002We"
	    "Ye = \002,1pe20.12,2x,1pe20.12,2x,1pe13.5,/3x,\002WeZe = \002,1p"
	    "e20.12,2x,1pe20.12,2x,1pe13.5,/3x,\002WeTe = \002,1pe20.12,2x,1p"
	    "e20.12,2x,1pe13.5,/3x,\002WeSe = \002,1pe20.12,/3x,\002WeRe ="
	    " \002,1pe20.12)";
    static char fmt_420[] = "(//13x,\002The vibrational constants (in cm-1) "
	    "are : \002)";
    static char fmt_430[] = "(///12x,\002## The rotational constants (in a.u"
	    ".) are : ## \002)";
    static char fmt_440[] = "(/5x,\002             Calculated               "
	    "Input \002,13x,\002Error% \002,//3x,\002     Be = \002,1pe20.12,"
	    "2x,1pe20.12,2x,1pe13.5,/3x,\002Alpha_e = \002,1pe20.12,2x,1pe20."
	    "12,2x,1pe13.5,/3x,\002Gamma_e = \002,1pe20.12,2x,1pe20.12,2x,1pe"
	    "13.5,/3x,\002 Eta_e3 = \002,1pe20.12,/3x,\002 Eta_e4 = \002,1pe2"
	    "0.12,/3x,\002 Eta_e5 = \002,1pe20.12,/3x,\002 Eta_e6 = \002,1pe2"
	    "0.12,/3x,\002 Eta_e7 = \002,1pe20.12,//3x,\002    Dee = \002,1pe"
	    "20.12,2x,1pe20.12,2x,1pe13.5,/3x,\002 Beta_e = \002,1pe20.12,2x,"
	    "1pe20.12,2x,1pe13.5,/3x,\002 Xsi_e2 = \002,1pe20.12,/3x,\002 Xsi"
	    "_e3 = \002,1pe20.12,/3x,\002 Xsi_e4 = \002,1pe20.12,/3x,\002 Xsi"
	    "_e5 = \002,1pe20.12,/3x,\002 Xsi_e6 = \002,1pe20.12,/3x,\002 Xsi"
	    "_e7 = \002,1pe20.12)";
    static char fmt_450[] = "(//15x,\002The rotational constants (in cm-1) a"
	    "re : \002)";
    static char fmt_460[] = "(///15x,\002    Check VIBRATIONAL constants :"
	    " \002,//8x,\002[ As V(R)=De, De can be calculated by Rees's Eq."
	    " \002,/8x,\002            from  VIBrational constants.        "
	    " \002,//8x,\002    Quadratic potential form :   \002,/8x,\002  D"
	    "e_2c -- De from calculated vibrational constants; \002,/8x,\002 "
	    " De_2i -- De from  inputted  vibrational constants. \002,//8x"
	    ",\002                De_2 = We*We/(4*Wexe)                \002,/"
	    "/8x,\002      CUBIC   potential form (much accurate) :   \002,/8"
	    "x,\002  De_3c -- De from calculated vibrational constants; \002,"
	    "/8x,\002  De_3i -- De from  inputted  vibrational constants. "
	    "\002,//8x,\002     De3(a) = 2*[dsqrt(Wexe**2 - 3*We*Weye)]**3   "
	    "   \002,/8x,\002     De3(b) =  Wexe*(2*Wexe**2 - 9*We*Weye)     "
	    "     \002,/8x,\002       De_3 = [De3(a) - De3(b)]/(27*Weye**2)  "
	    "       \002,//8x,\002     IF  term = Wexe**2 - 3*We*Weye  < 0.0 "
	    "          \002,/8x,\002       dsqrt( term ) == complex number ! "
	    "            \002,//8x,\002       De_2   =       De_2c  OR  De_2i"
	    "               \002,/8x,\002       De_3   =       De_3c  OR  De_"
	    "3i               \002,/8x,\002       De3(a) =       De_3ca OR  D"
	    "e_3ia              \002,/8x,\002       De3(b) =       De_3cb OR "
	    " De_3ib              \002,//8x,\002  Higher_power potential form"
	    " is much more accurate. \002,/8x,\002    Set De_3i & D3i% = 0.0 "
	    "if inputted WeYe is 0.0 . \002,//8x,\002       Dei = Input 'TRUE"
	    "' dissociation energy De :   \002,//8x,\002           D2c% = 100"
	    " * | De_2c - Dei |/Dei ;   \002,/8x,\002           D2i% = 100 * "
	    "| De_2i - Dei |/Dei ;   \002,/8x,\002        D3c% = 100 * | real"
	    "(De_3c - Dei) |/Dei ;   \002,/8x,\002        D3i% = 100 * | real"
	    "(De_3i - Dei) |/Dei .       ]\002,//18x,\002    Dei == De = \002"
	    ",f12.8,\002  a.u.\002,//4x,\002 De_2c(a.u)      De_2i(a.u)      "
	    "De_3c(a.u)     \002,\002 De_3i(a.u) \002,/2x,4(1f12.8,4x),//8x"
	    ",\002          Percent errors of generated De : \002,/4x,\002   "
	    "D2c%             D2i%            D3c%           D3i% \002,/2x,4("
	    "1f12.6,4x))";
    static char fmt_470[] = "(///15x,\002    Check CUBIC form calculations"
	    " : \002,//12x,\002 Using the calculated constants Wexe and Weye"
	    " \002,//11x,\002       Wexe**2(a.u.)     -3*We*Weye(a.u.)   \002"
	    ",/17x,14(\002-\002),5x,16(\002-\002)/15x,2(1pe16.8,4x),///4x,"
	    "\002De_3cb(a.u)\002,9x,\002De_3ca(a.u)\002,16x,\002De_3c(a.u)"
	    "\002,/3x,13(\002-\002),2x,24(\002-\002),2x,25(\002-\002)/7x,\002"
	    "Real\002,10x,\002Real\002,7x,\002Imaginary\002,6x,\002Real\002,8"
	    "x,\002Imaginary\002,/2x,1pe14.6,2(1pe13.5),1x,2(1pe13.5))";
    static char fmt_480[] = "(///15x,\002    Check CUBIC form calculations"
	    " : \002,//15x,\002 Using the INPUT vibrational constants \002,//"
	    "11x,\002       WeXe**2(a.u.)     -3*We*WeYe(a.u.)   \002,/17x,14("
	    "\002-\002),5x,16(\002-\002)/15x,2(1pe16.8,4x),///4x,\002De_3ib(a"
	    ".u)\002,9x,\002De_3ia(a.u)\002,16x,\002De_3i(a.u)\002,/3x,13("
	    "\002-\002),2x,24(\002-\002),2x,25(\002-\002)/7x,\002Real\002,10x,"
	    "\002Real\002,7x,\002Imaginary\002,6x,\002Real\002,8x,\002Imagina"
	    "ry\002,/2x,1pe14.6,2(1pe13.5),1x,2(1pe13.5))";
    static char fmt_500[] = "(///17x,\002=== The VIBrational energies are : "
	    "=== \002,//19x,\002    [  Ev_dif = E(v) - E(v-1)  ] \002,//5x"
	    ",\002v      E(v; a.u.)        E(v; cm-1)     Ev_dif(a.u)\002,"
	    "\002   Ev_dif(cm-1)\002,/)";
    static char fmt_510[] = "(3x,i3,1pe18.9,1pe18.9,x,1pe13.5,x,1pe13.5)";
    static char fmt_530[] = "(/12x,\002The SUM of  Ev_dif(a.u) = \002,1pe16."
	    "8,/12x,\002Maximum energy  Ev(max) = \002,1pe16.8,/)";
    static char fmt_535[] = "(//5x,\002v      E(v; a.u.)      Enew(v; a.u."
	    ")\002,\002    Ev_dif(a.u.)\002,/)";
    static char fmt_560[] = "(///8x,\002[  E'(v) = E(v) - Enew(v);  \002,"
	    "\002E(v) =/= E'(v)  for nw0 = 1 ; \002,/12x,\002E(v) = E(v) - En"
	    "ew(v);  E(v) === E'(v)  for nw0 = 0  ]\002,/31x,\002nw0 =\002,i2"
	    ",//5x,\002v     E(v; a.u.)        E'(v; a.u.)    Enew(v; a.u)"
	    " \002,\002  E'v_dif(au)\002,/)";
    static char fmt_570[] = "(//5x,\002v     E(v; cm-1)        E'(v; cm-1"
	    ")\002,\002    Enew(v;cm-1)  E'v_dif(cm-1)\002,/)";
    static char fmt_580[] = "(//16x,\002Comparing vibrational energies (I)"
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
    static char fmt_540[] = "(/2x,\002SUM of Ea_dif(a.u) =\002,1pe13.6,3x"
	    ",\002SUM of Eb_dif(a.u) =\002,1pe13.6,/2x,\002Maxi. Ea(v)=Ea(max"
	    ")=\002,1pe13.6,3x,\002Maxi. Eb(v)=Eb(max)=\002,1pe13.6,/)";
    static char fmt_544[] = "(//5x,\002v     E(v; cm-1)       E'(v; cm-1)"
	    "\002,\002      Ea(v; cm-1)    Eb(cm-1)\002,/)";
    static char fmt_546[] = "(//5x,\002v     Ea(v; cm-1)       Eb(v; cm-1"
	    ")\002,\002     Ea-Eb(cm-1)   Ea-Eb(a.u.)\002,/)";
    static char fmt_550[] = "(/15x,\002Average of |Ea(cm-1) - Eb(cm-1)| ="
	    "\002,1pe13.6,/15x,\002Average of |Ea(a.u.) - Eb(a.u.)| =\002,1pe"
	    "13.6,/)";
    static char fmt_590[] = "(///18x,\002DEin(v) is the INPUT differencies E"
	    "inp(v) \002,/18x,\002Edifa(v) =  [ Ea(v) - Ea(v-1) ] \002,/18x"
	    ",\002Edela(v) =  [ Ea(v) - Ea(v-1) ] - DEin(v) \002,/18x,\002Ede"
	    "lb(v) =  [ Eb(v) - Eb(v-1) ] - DEin(v) \002,//5x,\002v    Einp(v"
	    "; cm-1)    Edifa(v; cm-1)\002,\002    Edela(cm-1)   Edelb(cm-1"
	    ")\002,/)";
    static char fmt_594[] = "(///17x,\002ERRORa(v)% = 100.0*| Edela(v) |/Ein"
	    "p(v) : \002,//5x,\002v    Einp(v; cm-1)    Edifa(v; cm-1)\002"
	    ",\002    Edela(cm-1)   ERRORa(cm-1)\002,/)";
    static char fmt_596[] = "(/5x,\002Average errors for quadratic Ev,\002"
	    ",\002  ERRORa(v)% = \002,1pe16.8,\002%\002,/)";
    static char fmt_620[] = "(//17x,\002Comparing vibrational energies (II) "
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
    static char fmt_610[] = "(///11x,\002Comparing vibrational energies (III"
	    ") : \002,//12x,\002Edifv(i) =  Ev_(n)[i] - Ev_(n-1)[i] \002,/8x"
	    ",\002Error_v(i)% = 100.0*{ Edifv(i) }/Ev_(n-1)[i] : \002,)";
    static char fmt_642[] = "(//1x,\002Vibrational energies Ev's in various "
	    "form : \002,//2x,\002Ev2(i) => Energy expanded to Wexe term;\002"
	    ",/2x,\002Ev3(i) => Energy expanded to Weye term;\002,/2x,\002Ev4"
	    "(i) => Energy expanded to Weze term;\002,/2x,\002Ev5(i) => Energ"
	    "y expanded to Wete term;\002,/2x,\002Ev6(i) => Energy expanded t"
	    "o Wese term;\002,/2x,\002Ev7(i) => Energy expanded to Were term"
	    ".\002,//5x,\002v      Ev2(a.u.)   \002,/)";
    static char fmt_611[] = "(//11x,\002Ev2(i) => Energy expanded to Wexe te"
	    "rm,\002,/11x,\002Ev3(i) => Energy expanded to Weye term.\002,//5"
	    "x,\002v      Ev2(cm-1)         Ev3(cm-1)       Error_v%\002,/)";
    static char fmt_612[] = "(//11x,\002Ev3(i) => Energy expanded to Weye te"
	    "rm,\002,/11x,\002Ev4(i) => Energy expanded to Weze term.\002,//5"
	    "x,\002v      Ev3(cm-1)         Ev4(cm-1)       Error_v%\002,/)";
    static char fmt_644[] = "(//5x,\002v      Ev3(a.u.)   \002,/)";
    static char fmt_613[] = "(//11x,\002Ev4(i) => Energy expanded to Weze te"
	    "rm,\002,/11x,\002Ev5(i) => Energy expanded to Wete term.\002,//5"
	    "x,\002v      Ev4(cm-1)         Ev5(cm-1)       Error_v%\002,/)";
    static char fmt_645[] = "(//5x,\002v      Ev4(a.u.)   \002,/)";
    static char fmt_614[] = "(//11x,\002Ev5(i) => Energy expanded to Wete te"
	    "rm,\002,/11x,\002Ev6(i) => Energy expanded to Wese term.\002,//5"
	    "x,\002v      Ev5(cm-1)         Ev6(cm-1)       Error_v%\002,/)";
    static char fmt_646[] = "(//5x,\002v      Ev5(a.u.)   \002,/)";
    static char fmt_615[] = "(//11x,\002Ev6(i) => Energy expanded to Wese te"
	    "rm,\002,/11x,\002Ev7(i) => Energy expanded to Were term.\002,//5"
	    "x,\002v      Ev6(cm-1)         Ev7(cm-1)       Error_v%\002,/)";
    static char fmt_647[] = "(//5x,\002v      Ev6(a.u.)   \002,/)";
    static char fmt_648[] = "(//5x,\002v      Ev7(a.u.)   \002,/)";
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
    static char fmt_625[] = "(///21x,\002The VIB-ROTational energies are :"
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
    static char fmt_650[] = "(3x,2i3,1pe18.9,1pe18.9,x,1pe13.5,x,1pe13.5)";
    static char fmt_630[] = "(16x,\002-----\002,13x,\002-----\002,12x,\002--"
	    "---\002,9x,\002-----\002,/)";
    static char fmt_640[] = "(/12x,\002The SUM of Evj_dif(a.u) = \002,1pe16."
	    "8,/12x,\002Maximum energy Evj(max) = \002,1pe16.8,//6x,\002If th"
	    "e above TWO are NOT equal, Check  nj  in each \002,\002v state "
	    "!\002,/11x,\002Set  nj >= 1 + j_max (of vib. state) = 1 + (nj-1) "
	    "\002/)";
    static char fmt_660[] = "(///13x,\002Contributions to E_vj are (Part I) "
	    ": \002,//7x,\002[   E(v,j) = E(v) + Ej ;     Ej = Ej1 + Ej2  ;"
	    " \002,/7x,\002  Ej1 = E{v;j*(j*1)} ;  Ej2 = E{v;j*j*(j*1)**2}  ] "
	    "\002,//5x,\002v  j    E(v,j; a.u.)        E(v;a.u.)       Ej(a.u"
	    ".) \002,/)";
    static char fmt_665[] = "(///18x,\002Separate contributions to E_vj  :"
	    " \002,//12x,\002[   E(v,j) = E(v-j_coupling) + E(v) + E(j) ; "
	    "\002,//12x,\002    E(v-j_coup) = Vib-Rot_coupling energies; \002"
	    ",/12x,\002           E(v) =    Vibrational   energies; \002,/12x,"
	    "\002           E(j) =    Rotational    energies.  ] \002,//5x"
	    ",\002v  j    E(v,j; a.u.)       E(v-j_coup)     E(v; a.u.) \002"
	    ",\002    E(j; a.u.) \002,/)";
    static char fmt_670[] = "(///17x,\002Contributions to E_vj are (Part II)"
	    " : \002,//19x,\002[  E(v,j) = E(v) + Ej1 + Ej2  ] \002,//5x,\002"
	    "v  j    E(v,j; a.u.)        E(v;a.u.)       Ej1(a.u.) \002,\002 "
	    "   Ej2(a.u.) \002,/)";
    static char fmt_680[] = "(///12x,\002Contributions to E_vj are (Part III"
	    ") : \002,//16x,\002[   E'(v,j) = E(v) + Ej1  ] \002,//5x,\002v  "
	    "j    E(v,j; a.u.)      E'(v,j; a.u.)     Ej1(a.u.) \002,/)";

    /* System generated locals */
    integer evj_dim1, evj_offset, i__1, i__2;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8, d__9, d__10, 
	    d__11, d__12, d__13, d__14, d__15, d__16, d__17, d__18, d__19, 
	    d__20, d__21;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void), do_fio(integer *, char *, ftnlen);
    double sqrt(doublereal);
    integer s_wsle(cilist *), e_wsle(void), s_rsle(cilist *), do_lio(integer *
	    , integer *, char *, ftnlen), e_rsle(void);

    /* Local variables */
    static integer i__, j, k;
    static doublereal e1[200], e2[200], e3[200], bj, eh[200], ej[200], em[200]
	    ;
    static integer ij;
    static doublereal rb, ra, er[200], eu[200], ew[200], ex[200], rg, sd, sb, 
	    bv, pt;
    static integer kk;
    static doublereal px, py, pz;
    static integer kv, jj, kj;
    static doublereal aj1[40000]	/* was [200][200] */, aj2[40000]	
	    /* was [200][200] */, al0, bj0, bj2, ej1, ej2, em0, eh0, bv0;
    static integer nj0;
    static doublereal ew0[200], ev2[200], ev3[200], ev4[200], ev5[200], ev6[
	    200];
    static integer nv1, nv2;
    static doublereal ev0, ex0, ejb, ejc, bv02, bv03, bv04, bv05, euj[40000]	
	    /* was [200][200] */, ewj[40000]	/* was [200][200] */, bv06, 
	    bv07, bjj, bvj, ej1a, ej2a, e0cm, e2cp, e3cp, e2ip, e3ip, difa, 
	    difb, aucm, difv, eucm, emax, evcm, ewcm, excm, dxcm, dvcm, auev, 
	    esum, difj0, difcm, difau, emaxa, emaxb, difvj, wexem, rydev, 
	    error, dise2c;
    static doublecomplex dise3c;
    static doublereal dise2i;
    static doublecomplex dise3i;
    static doublereal cubcal, brigid, cubinp, sumdif, evjmax, rinert;
    static doublecomplex dise3ca;
    static doublereal dise3cb;
    static doublecomplex dise3ia;
    static doublereal dise3ib, sumdifa, sumdifb;

    /* Fortran I/O blocks */
    static cilist io___250 = { 0, 0, 0, fmt_400, 0 };
    static cilist io___255 = { 0, 0, 0, fmt_410, 0 };
    static cilist io___256 = { 0, 0, 0, fmt_420, 0 };
    static cilist io___257 = { 0, 0, 0, fmt_410, 0 };
    static cilist io___258 = { 0, 0, 0, fmt_430, 0 };
    static cilist io___264 = { 0, 0, 0, fmt_440, 0 };
    static cilist io___265 = { 0, 0, 0, fmt_450, 0 };
    static cilist io___266 = { 0, 0, 0, fmt_440, 0 };
    static cilist io___281 = { 0, 6, 0, fmt_460, 0 };
    static cilist io___282 = { 0, 35, 0, fmt_460, 0 };
    static cilist io___283 = { 0, 6, 0, fmt_470, 0 };
    static cilist io___284 = { 0, 35, 0, fmt_470, 0 };
    static cilist io___285 = { 0, 6, 0, fmt_480, 0 };
    static cilist io___286 = { 0, 35, 0, fmt_480, 0 };
    static cilist io___327 = { 0, 6, 0, fmt_500, 0 };
    static cilist io___328 = { 0, 35, 0, fmt_500, 0 };
    static cilist io___329 = { 0, 36, 0, fmt_500, 0 };
    static cilist io___335 = { 0, 6, 0, 0, 0 };
    static cilist io___336 = { 0, 35, 0, 0, 0 };
    static cilist io___337 = { 0, 36, 0, 0, 0 };
    static cilist io___338 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___339 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___340 = { 0, 36, 0, fmt_510, 0 };
    static cilist io___341 = { 0, 6, 0, fmt_530, 0 };
    static cilist io___342 = { 0, 35, 0, fmt_530, 0 };
    static cilist io___343 = { 0, 6, 0, fmt_535, 0 };
    static cilist io___344 = { 0, 35, 0, fmt_535, 0 };
    static cilist io___345 = { 0, 6, 0, 0, 0 };
    static cilist io___346 = { 0, 35, 0, 0, 0 };
    static cilist io___347 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___348 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___349 = { 0, 6, 0, fmt_560, 0 };
    static cilist io___350 = { 0, 35, 0, fmt_560, 0 };
    static cilist io___351 = { 0, 6, 0, 0, 0 };
    static cilist io___352 = { 0, 35, 0, 0, 0 };
    static cilist io___353 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___354 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___355 = { 0, 6, 0, fmt_570, 0 };
    static cilist io___356 = { 0, 35, 0, fmt_570, 0 };
    static cilist io___357 = { 0, 6, 0, 0, 0 };
    static cilist io___358 = { 0, 35, 0, 0, 0 };
    static cilist io___362 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___363 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___364 = { 0, 6, 0, fmt_580, 0 };
    static cilist io___365 = { 0, 35, 0, fmt_580, 0 };
    static cilist io___370 = { 0, 6, 0, 0, 0 };
    static cilist io___371 = { 0, 35, 0, 0, 0 };
    static cilist io___374 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___375 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___376 = { 0, 6, 0, fmt_540, 0 };
    static cilist io___377 = { 0, 35, 0, fmt_540, 0 };
    static cilist io___378 = { 0, 6, 0, fmt_544, 0 };
    static cilist io___379 = { 0, 35, 0, fmt_546, 0 };
    static cilist io___380 = { 0, 6, 0, 0, 0 };
    static cilist io___381 = { 0, 35, 0, 0, 0 };
    static cilist io___386 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___387 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___388 = { 0, 6, 0, fmt_550, 0 };
    static cilist io___389 = { 0, 35, 0, fmt_550, 0 };
    static cilist io___390 = { 0, 23, 0, 0, 0 };
    static cilist io___391 = { 0, 6, 0, fmt_590, 0 };
    static cilist io___392 = { 0, 35, 0, fmt_590, 0 };
    static cilist io___394 = { 0, 6, 0, 0, 0 };
    static cilist io___395 = { 0, 35, 0, 0, 0 };
    static cilist io___401 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___402 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___403 = { 0, 6, 0, fmt_594, 0 };
    static cilist io___404 = { 0, 35, 0, fmt_594, 0 };
    static cilist io___405 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___406 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___407 = { 0, 6, 0, fmt_596, 0 };
    static cilist io___408 = { 0, 35, 0, fmt_596, 0 };
    static cilist io___409 = { 0, 6, 0, fmt_620, 0 };
    static cilist io___410 = { 0, 35, 0, fmt_620, 0 };
    static cilist io___415 = { 0, 6, 0, 0, 0 };
    static cilist io___416 = { 0, 35, 0, 0, 0 };
    static cilist io___417 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___418 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___419 = { 0, 6, 0, fmt_610, 0 };
    static cilist io___420 = { 0, 35, 0, fmt_610, 0 };
    static cilist io___421 = { 0, 38, 0, fmt_642, 0 };
    static cilist io___423 = { 0, 6, 0, fmt_611, 0 };
    static cilist io___424 = { 0, 35, 0, fmt_611, 0 };
    static cilist io___425 = { 0, 6, 0, fmt_612, 0 };
    static cilist io___426 = { 0, 35, 0, fmt_612, 0 };
    static cilist io___427 = { 0, 38, 0, fmt_644, 0 };
    static cilist io___428 = { 0, 6, 0, fmt_613, 0 };
    static cilist io___429 = { 0, 35, 0, fmt_613, 0 };
    static cilist io___430 = { 0, 38, 0, fmt_645, 0 };
    static cilist io___431 = { 0, 6, 0, fmt_614, 0 };
    static cilist io___432 = { 0, 35, 0, fmt_614, 0 };
    static cilist io___433 = { 0, 38, 0, fmt_646, 0 };
    static cilist io___434 = { 0, 6, 0, fmt_615, 0 };
    static cilist io___435 = { 0, 35, 0, fmt_615, 0 };
    static cilist io___436 = { 0, 38, 0, fmt_647, 0 };
    static cilist io___437 = { 0, 38, 0, fmt_648, 0 };
    static cilist io___439 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___440 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___441 = { 0, 38, 0, fmt_510, 0 };
    static cilist io___442 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___443 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___444 = { 0, 38, 0, fmt_510, 0 };
    static cilist io___445 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___446 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___447 = { 0, 38, 0, fmt_510, 0 };
    static cilist io___448 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___449 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___450 = { 0, 38, 0, fmt_510, 0 };
    static cilist io___451 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___452 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___453 = { 0, 38, 0, fmt_510, 0 };
    static cilist io___454 = { 0, 38, 0, fmt_510, 0 };
    static cilist io___455 = { 0, 6, 0, fmt_710, 0 };
    static cilist io___456 = { 0, 35, 0, fmt_710, 0 };
    static cilist io___457 = { 0, 36, 0, fmt_710, 0 };
    static cilist io___460 = { 0, 6, 0, fmt_510, 0 };
    static cilist io___461 = { 0, 35, 0, fmt_510, 0 };
    static cilist io___462 = { 0, 36, 0, fmt_510, 0 };
    static cilist io___463 = { 0, 6, 0, fmt_625, 0 };
    static cilist io___464 = { 0, 35, 0, fmt_625, 0 };
    static cilist io___465 = { 0, 36, 0, fmt_625, 0 };
    static cilist io___471 = { 0, 6, 0, 0, 0 };
    static cilist io___472 = { 0, 35, 0, 0, 0 };
    static cilist io___473 = { 0, 36, 0, 0, 0 };
    static cilist io___474 = { 0, 35, 0, fmt_650, 0 };
    static cilist io___475 = { 0, 6, 0, fmt_650, 0 };
    static cilist io___476 = { 0, 36, 0, fmt_650, 0 };
    static cilist io___477 = { 0, 6, 0, 0, 0 };
    static cilist io___478 = { 0, 35, 0, 0, 0 };
    static cilist io___479 = { 0, 36, 0, 0, 0 };
    static cilist io___480 = { 0, 6, 0, fmt_630, 0 };
    static cilist io___481 = { 0, 35, 0, fmt_630, 0 };
    static cilist io___482 = { 0, 36, 0, fmt_630, 0 };
    static cilist io___483 = { 0, 35, 0, fmt_640, 0 };
    static cilist io___484 = { 0, 6, 0, fmt_640, 0 };
    static cilist io___485 = { 0, 35, 0, fmt_660, 0 };
    static cilist io___486 = { 0, 6, 0, fmt_665, 0 };
    static cilist io___487 = { 0, 35, 0, 0, 0 };
    static cilist io___488 = { 0, 6, 0, 0, 0 };
    static cilist io___489 = { 0, 35, 0, fmt_650, 0 };
    static cilist io___490 = { 0, 6, 0, fmt_650, 0 };
    static cilist io___491 = { 0, 6, 0, 0, 0 };
    static cilist io___492 = { 0, 35, 0, 0, 0 };
    static cilist io___493 = { 0, 6, 0, fmt_630, 0 };
    static cilist io___494 = { 0, 35, 0, fmt_630, 0 };
    static cilist io___495 = { 0, 35, 0, fmt_670, 0 };
    static cilist io___496 = { 0, 35, 0, fmt_650, 0 };
    static cilist io___497 = { 0, 35, 0, 0, 0 };
    static cilist io___498 = { 0, 35, 0, fmt_680, 0 };
    static cilist io___499 = { 0, 35, 0, fmt_650, 0 };
    static cilist io___500 = { 0, 35, 0, 0, 0 };


/* ---------------------------------------------------------- */
/*  nv = the number of vibrational states used. */
/*         (nv-1) - The HIGHEST vibrational state. */
/*  nj - Number of rotational states in each v state. */
/* ========================================================== */
    /* Parameter adjustments */
    evj_dim1 = *lj;
    evj_offset = 1 + evj_dim1 * 1;
    evj -= evj_offset;
    --ev;
    --nj;

    /* Function Body */
    rydev = 13.60569809;
    auev = 27.21139618;
    aucm = 219474.6306;
    rinert = spectra_1.amu * spectra_1.re * spectra_1.re;
    brigid = 1. / (rinert * 2.);
/* ---------------------------------------------------------- */
    if (*nv == 1 && nj[1] == 1) {
	goto L10;
    }
/* ---------------------------------------------------------- */
/*  Write out vib-rot constants */
/* ---------------------------------------------------------- */
    ij = 6;
    for (i__ = 1; i__ <= 2; ++i__) {
	io___250.ciunit = ij;
	s_wsfe(&io___250);
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
	io___255.ciunit = ij;
	s_wsfe(&io___255);
	do_fio(&c__1, (char *)&spectr0_1.w0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectr0_1.we0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.we, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&spectra_1.wee, (ftnlen)sizeof(doublereal));
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
	io___256.ciunit = ij;
	s_wsfe(&io___256);
	e_wsfe();
	io___257.ciunit = ij;
	s_wsfe(&io___257);
	d__1 = spectr0_1.w0 * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__2 = spectr0_1.we0 * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	d__3 = spectra_1.we * aucm;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	d__4 = spectra_1.wee * aucm;
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

	io___258.ciunit = ij;
	s_wsfe(&io___258);
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
	io___264.ciunit = ij;
	s_wsfe(&io___264);
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
	io___265.ciunit = ij;
	s_wsfe(&io___265);
	e_wsfe();
	io___266.ciunit = ij;
	s_wsfe(&io___266);
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
/*  Check if the vibrational constants reproduce De */
/*       c : calculated data;  i : input data. */
/* ---------------------------------------------------------- */
/* --- From quadratic form : */
    dise2c = spectra_1.we * spectra_1.we / (spectr0_1.wex * 4.);
    dise2i = spectra_1.wee * spectra_1.wee / (spectra_1.wexe * 4.);
/* --- From CUBIC form : */
/* Computing 2nd power */
    d__1 = spectr0_1.wex;
    cubcal = d__1 * d__1 - spectra_1.we * 3. * spectr0_1.wey;
    if (cubcal >= 0.) {
/* Computing 3rd power */
	d__2 = sqrt(cubcal);
	d__1 = d__2 * (d__2 * d__2);
	z__1.r = d__1 * 2., z__1.i = d__1 * 0.;
	dise3ca.r = z__1.r, dise3ca.i = z__1.i;
    } else {
/* --- */
/* --- As cubcal < 0.0, dsqrt(cubcal) is complex ! */
/* --- ( dsqrt(cubcal) )**3 = ( i * dsqrt(- cubcal) )**3 */
/* --- ( i * dsqrt(cubcal) )**3 = -i * ( dsqrt(- cubcal) )**3 */
/* --- */
/* Computing 3rd power */
	d__2 = sqrt(-cubcal);
	d__1 = d__2 * (d__2 * d__2);
	z__1.r = d__1 * 0., z__1.i = d__1 * -2.;
	dise3ca.r = z__1.r, dise3ca.i = z__1.i;
    }
/* Computing 2nd power */
    d__1 = spectr0_1.wex;
    dise3cb = spectr0_1.wex * (d__1 * d__1 * 2. - spectra_1.we * 9. * 
	    spectr0_1.wey);
    z__2.r = dise3cb * 1., z__2.i = dise3cb * 0.;
    z__1.r = dise3ca.r - z__2.r, z__1.i = dise3ca.i - z__2.i;
    dise3c.r = z__1.r, dise3c.i = z__1.i;
    d__1 = spectr0_1.wey * 27. * spectr0_1.wey;
    z__1.r = dise3c.r / d__1, z__1.i = dise3c.i / d__1;
    dise3c.r = z__1.r, dise3c.i = z__1.i;

/* Computing 2nd power */
    d__1 = spectra_1.wexe;
    cubinp = d__1 * d__1 - spectra_1.wee * 3. * spectra_1.weye;
    if (cubinp >= 0.) {
/* Computing 3rd power */
	d__2 = sqrt(cubinp);
	d__1 = d__2 * (d__2 * d__2);
	z__1.r = d__1 * 2., z__1.i = d__1 * 0.;
	dise3ia.r = z__1.r, dise3ia.i = z__1.i;
    } else {
/* Computing 3rd power */
	d__2 = sqrt(-cubinp);
	d__1 = d__2 * (d__2 * d__2);
	z__1.r = d__1 * 0., z__1.i = d__1 * -2.;
	dise3ia.r = z__1.r, dise3ia.i = z__1.i;
    }
/* Computing 2nd power */
    d__1 = spectra_1.wexe;
    dise3ib = spectra_1.wexe * (d__1 * d__1 * 2. - spectra_1.wee * 9. * 
	    spectra_1.weye);
    z__2.r = dise3ib * 1., z__2.i = dise3ib * 0.;
    z__1.r = dise3ia.r - z__2.r, z__1.i = dise3ia.i - z__2.i;
    dise3i.r = z__1.r, dise3i.i = z__1.i;
    if (abs(spectra_1.weye) > 1e-50) {
	d__1 = spectra_1.weye * 27. * spectra_1.weye;
	z__1.r = dise3i.r / d__1, z__1.i = dise3i.i / d__1;
	dise3i.r = z__1.r, dise3i.i = z__1.i;
    } else {
	dise3i.r = 0., dise3i.i = 0.;
    }

    e2cp = (d__1 = dise2c - spectra_1.de, abs(d__1)) * 100. / spectra_1.de;
    e2ip = (d__1 = dise2i - spectra_1.de, abs(d__1)) * 100. / spectra_1.de;
/* --- */
/* Next 6 line produce Wrong results ! */
/*           dr3c = real(DisE3c) */
/*           di3c = imag(DisE3c) */
/*         E3cp = 100.0*abs( dr3c**2 + di3c**2 - De )/De */
/*           dr3i = real( DisE3i - De ) */
/*           di3i = imag( DisE3i - De ) */
/*         E3ip = 100.0*abs( dr3i**2 + di3i**2 )/De */
/* --- */
    z__1.r = dise3c.r - spectra_1.de, z__1.i = dise3c.i;
    e3cp = (d__1 = z__1.r, abs(d__1)) * 100. / spectra_1.de;
    z__1.r = dise3i.r - spectra_1.de, z__1.i = dise3i.i;
    e3ip = (d__1 = z__1.r, abs(d__1)) * 100. / spectra_1.de;
/* --- */
    if (abs(spectra_1.weye) <= 1e-50) {
	e3ip = 0.;
    }
/* ---------------------------------------------------------- */
/*   Print and compare the calculated De to see the quality */
/* of the constants (We, wex, wey, ...) */
/* ---------------------------------------------------------- */
    s_wsfe(&io___281);
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dise2c, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dise2i, (ftnlen)sizeof(doublereal));
    d__1 = dise3c.r;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = dise3i.r;
    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e2cp, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e2ip, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e3cp, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e3ip, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___282);
    do_fio(&c__1, (char *)&spectra_1.de, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dise2c, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&dise2i, (ftnlen)sizeof(doublereal));
    d__1 = dise3c.r;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = dise3i.r;
    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e2cp, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e2ip, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e3cp, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&e3ip, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* --- */
    if (cubcal < 0.) {
	s_wsfe(&io___283);
/* Computing 2nd power */
	d__2 = spectr0_1.wex;
	d__1 = d__2 * d__2;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__3 = spectra_1.we * -3. * spectr0_1.wey;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dise3cb, (ftnlen)sizeof(doublereal));
	do_fio(&c__2, (char *)&dise3ca, (ftnlen)sizeof(doublereal));
	do_fio(&c__2, (char *)&dise3c, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___284);
/* Computing 2nd power */
	d__2 = spectr0_1.wex;
	d__1 = d__2 * d__2;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__3 = spectra_1.we * -3. * spectr0_1.wey;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dise3cb, (ftnlen)sizeof(doublereal));
	do_fio(&c__2, (char *)&dise3ca, (ftnlen)sizeof(doublereal));
	do_fio(&c__2, (char *)&dise3c, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    if (cubinp < 0.) {
	s_wsfe(&io___285);
/* Computing 2nd power */
	d__2 = spectra_1.wexe;
	d__1 = d__2 * d__2;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__3 = spectra_1.wee * -3. * spectra_1.weye;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dise3ib, (ftnlen)sizeof(doublereal));
	do_fio(&c__2, (char *)&dise3ia, (ftnlen)sizeof(doublereal));
	do_fio(&c__2, (char *)&dise3i, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___286);
/* Computing 2nd power */
	d__2 = spectra_1.wexe;
	d__1 = d__2 * d__2;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	d__3 = spectra_1.wee * -3. * spectra_1.weye;
	do_fio(&c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dise3ib, (ftnlen)sizeof(doublereal));
	do_fio(&c__2, (char *)&dise3ia, (ftnlen)sizeof(doublereal));
	do_fio(&c__2, (char *)&dise3i, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* ---------------------------------------------------------- */
/*  Calculate Morse parameters : */
/* ---------------------------------------------------------- */
L10:
    al0 = spectra_1.re * sqrt(spectra_1.amu * .5 * spectra_1.wee * 
	    spectra_1.wee / spectra_1.de);
    wexem = al0 * al0 / (spectra_1.amu * 2. * spectra_1.re * spectra_1.re);
/* ---------------------------------------------------------- */
/*  Calculate vib-rot energies using vib-rot constants */
/* ---------------------------------------------------------- */
    nv1 = *nv + 1;
    nv2 = *nv + 40;
/* --- */
    if (*nv == 1 && nj[1] == 1) {
	nv1 = 1;
	nv2 = 1;
    }
/* --- */
    i__1 = nv2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ev[i__] = 0.;
	bv = (i__ - 1) * 1.;
/*          bv = dfloat( i - 1 ) */
	if (nv2 == 1) {
	    bv = spectr0_1.v0;
	}
	bv0 = bv + .5;
	bv02 = bv0 * bv0;
	bv03 = bv02 * bv0;
	bv04 = bv03 * bv0;
	bv05 = bv04 * bv0;
	bv06 = bv05 * bv0;
	bv07 = bv06 * bv0;
	ev2[i__ - 1] = spectr0_1.w0 + (spectra_1.we + spectr0_1.we0) * bv0 - 
		spectr0_1.wex * bv02;
	ev3[i__ - 1] = ev2[i__ - 1] + eswitch_1.aye * spectr0_1.wey * bv03;
	ev4[i__ - 1] = ev3[i__ - 1] + eswitch_1.aze * spectr0_1.wez * bv04;
	ev5[i__ - 1] = ev4[i__ - 1] + eswitch_1.ate * spectr0_1.wet * bv05;
	ev6[i__ - 1] = ev5[i__ - 1] + eswitch_1.ase * spectr0_1.wes * bv06;

/* --- Vibrational energies : */

	ev[i__] = ev6[i__ - 1] + eswitch_1.are * spectr0_1.wer * bv07;
/* -------------------------------------- */
/* Wee -- Original input We; */
/*  We -- Original input We if Mwe = 0 ; */
/*  We -- We + bryd  if Mwe > 0 . */
/* --- Approximate VIBrational energies : */
/* -------------------------------------- */
	eu[i__ - 1] = spectra_1.we * bv0 - spectr0_1.wex * bv02;
	ex[i__ - 1] = spectra_1.wee * bv0 - spectra_1.wexe * bv02;
	em[i__ - 1] = spectra_1.wee * bv0 - wexem * bv02;
	eh[i__ - 1] = spectra_1.wee * bv0;
/* --- New terms of VIBrational energies : */
	ew[i__ - 1] = spectr0_1.w0 + spectr0_1.we0 * bv0;
/* --- Ev's without NEW terms : */
	ew0[i__ - 1] = ev[i__] - ew[i__ - 1];

	if (fms_1.nw0 == 0) {
	    ev[i__] = ew0[i__ - 1];
	}

	nj0 = nj[i__];
	if (nj[1] > 0 && i__ <= nv1) {
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		evj[i__ + j * evj_dim1] = 0.;
		bj = (j - 1.) * 1.;
		if (nv2 == 1) {
		    bj = spectr0_1.r0;
		}
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
		evj[i__ + j * evj_dim1] = ev[i__] + ej1 + ej2;
/* --- ROTational energies : */
		if (i__ == 1) {
		    ej[j - 1] = eswitc1_1.abe * spectr1_1.bee * bj0 + 
			    eswitc2_1.ade * spectr2_1.dee * bj2;
		}
/* --- VIBrational-ROTational COUpling energies : */
		ewj[i__ + j * 200 - 201] = ej1a + ej2a;
/* --- Other energy forms : */
		euj[i__ + j * 200 - 201] = ev[i__] + ej1;
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
    if (nv2 == 1) {
	goto L800;
    }
/* ---------------------------------------------------------- */
/*  Write out vib-rot energies */
/* ---------------------------------------------------------- */
    s_wsfe(&io___327);
    e_wsfe();
    s_wsfe(&io___328);
    e_wsfe();
    s_wsfe(&io___329);
    e_wsfe();
    emax = 0.;
    sumdif = 0.;
    kk = 0;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	difv = ev[i__] - ev[i__ - 1];
	if (i__ == 1) {
	    difv = ev[i__];
	}
	if (ev[i__] < ev[i__ - 1] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___335);
	    e_wsle();
	    s_wsle(&io___336);
	    e_wsle();
	    s_wsle(&io___337);
	    e_wsle();
	}
	s_wsfe(&io___338);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	d__1 = ev[i__] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	d__2 = difv * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___339);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	d__1 = ev[i__] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	d__2 = difv * aucm;
	do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___340);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	e_wsfe();
	if (ev[i__] > ev[i__ - 1]) {
	    emax = ev[i__];
	}
	if (difv > 0.) {
	    sumdif += difv;
	}
/* L30: */
    }
    s_wsfe(&io___341);
    do_fio(&c__1, (char *)&sumdif, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&emax, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___342);
    do_fio(&c__1, (char *)&sumdif, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&emax, (ftnlen)sizeof(doublereal));
    e_wsfe();
/*       write(35,530) sumdif, Emax-Ev(1) */

    s_wsfe(&io___343);
    e_wsfe();
    s_wsfe(&io___344);
    e_wsfe();
    kk = 0;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	difv = ev[i__] - ev[i__ - 1];
	if (i__ == 1) {
	    difv = ev[1];
	}
	if (ev[i__] < ev[i__ - 1] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___345);
	    e_wsle();
	    s_wsle(&io___346);
	    e_wsle();
	}
	s_wsfe(&io___347);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___348);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    s_wsfe(&io___349);
    do_fio(&c__1, (char *)&fms_1.nw0, (ftnlen)sizeof(integer));
    e_wsfe();
    s_wsfe(&io___350);
    do_fio(&c__1, (char *)&fms_1.nw0, (ftnlen)sizeof(integer));
    e_wsfe();
    kk = 0;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	difv = ew0[i__ - 1] - ew0[i__ - 2];
	if (i__ == 1) {
	    difv = ew0[0];
	}
	if (ev[i__] < ev[i__ - 1] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___351);
	    e_wsle();
	    s_wsle(&io___352);
	    e_wsle();
	}
	s_wsfe(&io___353);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew0[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___354);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew0[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    s_wsfe(&io___355);
    e_wsfe();
    s_wsfe(&io___356);
    e_wsfe();
    kk = 0;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	difv = (ew0[i__ - 1] - ew0[i__ - 2]) * aucm;
	if (i__ == 1) {
	    difv = ew0[0] * aucm;
	}
	if (ev[i__] < ev[i__ - 1] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___357);
	    e_wsle();
	    s_wsle(&io___358);
	    e_wsle();
	}
	evcm = ev[i__] * aucm;
	ewcm = ew[i__ - 1] * aucm;
	e0cm = ew0[i__ - 1] * aucm;
	s_wsfe(&io___362);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&evcm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e0cm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ewcm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___363);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&evcm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e0cm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ewcm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difv, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    s_wsfe(&io___364);
    e_wsfe();
    s_wsfe(&io___365);
    e_wsfe();
    emaxa = 0.;
    emaxb = 0.;
    sumdifa = 0.;
    sumdifb = 0.;
    kk = 0;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	if (ev[i__] < ev[i__ - 1] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___370);
	    e_wsle();
	    s_wsle(&io___371);
	    e_wsle();
	}
	difa = eu[i__ - 1] - eu[i__ - 2];
	difb = ex[i__ - 1] - ex[i__ - 2];
	if (i__ == 1) {
	    difa = eu[i__ - 1];
	}
	if (i__ == 1) {
	    difb = ex[i__ - 1];
	}
	if (difa > 0.) {
	    sumdifa += difa;
	}
	if (difb > 0.) {
	    sumdifb += difb;
	}
	if (eu[i__ - 1] > eu[i__ - 2]) {
	    emaxa = eu[i__ - 1];
	}
	if (ex[i__ - 1] > ex[i__ - 2]) {
	    emaxb = ex[i__ - 1];
	}
	s_wsfe(&io___374);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eu[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ex[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___375);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ew[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eu[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ex[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    s_wsfe(&io___376);
    do_fio(&c__1, (char *)&sumdifa, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&sumdifb, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&emaxa, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&emaxb, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___377);
    do_fio(&c__1, (char *)&sumdifa, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&sumdifb, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&emaxa, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&emaxb, (ftnlen)sizeof(doublereal));
    e_wsfe();

    s_wsfe(&io___378);
    e_wsfe();
    s_wsfe(&io___379);
    e_wsfe();
    kk = 0;
    difa = 0.;
    difb = 0.;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	if (ev[i__] < ev[i__ - 1] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___380);
	    e_wsle();
	    s_wsle(&io___381);
	    e_wsle();
	}
	evcm = ev[i__] * aucm;
	e0cm = ew0[i__ - 1] * aucm;
	eucm = eu[i__ - 1] * aucm;
	excm = ex[i__ - 1] * aucm;
	difau = eu[i__ - 1] - ex[i__ - 1];
	difcm = difau * aucm;
	difa += abs(difau);
	difb += abs(difcm);
	s_wsfe(&io___386);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&evcm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e0cm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eucm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&excm, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___387);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&eucm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&excm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difcm, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&difau, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    s_wsfe(&io___388);
    d__1 = difb / *nv;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = difa / *nv;
    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___389);
    d__1 = difb / *nv;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = difa / *nv;
    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    e_wsfe();

    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* -- Read in the INPUT vibrational energies OR their differencies */
	s_rsle(&io___390);
	do_lio(&c__5, &c__1, (char *)&ew0[i__ - 1], (ftnlen)sizeof(doublereal)
		);
	e_rsle();
    }

/*     if (Ew0(nv) .lt. Ew0(1)) then */
/* -- As Ew0(nv) < Ew0(1), you readed Ev differencies; */
/* --   you need to find Ev themselves. */
/*         Ew(1) = Ew0(1) */
/*         Ew(2) = Ew0(2) + Ew0(1) */
/*       do i=3,nv */
/*         Ew(i) = Ew(i-1) + Ew0(i) */
/*       enddo */

    if (ew0[*nv - 1] > ew0[0]) {
/* -- As Ew0(nv) > Ew0(1), you readed Ev ; */
/* --   you need to find Ev differencies . */
	ew[0] = ew0[0];
	i__1 = *nv;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    ew[i__ - 1] = ew0[i__ - 1] - ew0[i__ - 2];
	}

	i__1 = *nv;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    ew0[i__ - 1] = ew[i__ - 1];
	}
    }

    if (ew0[0] > ev[1] * 1e3) {
/* -- Change the unit of Ew0 from cm-1 to a.u. */
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ew0[i__ - 1] /= aucm;
	}
    }

    s_wsfe(&io___391);
    e_wsfe();
    s_wsfe(&io___392);
    e_wsfe();
    kk = 0;
    esum = 0.;
    i__1 = *nv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	if (ev[i__] < ev[i__ - 1] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___394);
	    e_wsle();
	    s_wsle(&io___395);
	    e_wsle();
	}

/*           dvcm = ( Ev(i) - Ew0(i) )*aucm */
/*           ducm = ( Eu(i) - Ew0(i) )*aucm */
/*           dxcm = ( Ex(i) - Ew0(i) )*aucm */
/*           dvcm = ( ( Ev(i)-Ev(i-1) ) - Ew0(i) )*aucm */

	e1[i__ - 1] = (eu[i__ - 1] - eu[i__ - 2]) * aucm;
	e2[i__ - 1] = (eu[i__ - 1] - eu[i__ - 2] - ew0[i__ - 1]) * aucm;
	e3[i__ - 1] = (d__1 = e2[i__ - 1], abs(d__1)) * 100. / (ew0[i__ - 1] *
		 aucm);
	dxcm = (ex[i__ - 1] - ex[i__ - 2] - ew0[i__ - 1]) * aucm;
	if (i__ == 1) {
	    dvcm = (ev[1] - ew0[0]) * aucm;
	}
	if (i__ == 1) {
	    e2[i__ - 1] = (eu[0] - ew0[0]) * aucm;
	}
	if (i__ == 1) {
	    dxcm = (ex[0] - ew0[0]) * aucm;
	}
	if (i__ <= fms_1.nvd) {
	    esum += e3[i__ - 1];
	}
	s_wsfe(&io___401);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	d__1 = ew0[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e1[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e2[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dxcm, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___402);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	d__1 = ew0[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e1[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e2[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dxcm, (ftnlen)sizeof(doublereal));
	e_wsfe();
/*       write(35,510) kv, Ew0(i)*aucm, dvcm,  e2(i), dxcm */
    }

    s_wsfe(&io___403);
    e_wsfe();
    s_wsfe(&io___404);
    e_wsfe();
    i__1 = fms_1.nvd;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kv = i__ - 1;
	s_wsfe(&io___405);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	d__1 = ew0[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e1[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e2[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e3[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___406);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	d__1 = ew0[i__ - 1] * aucm;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e1[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e2[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&e3[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    s_wsfe(&io___407);
    d__1 = esum / fms_1.nvd;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___408);
    d__1 = esum / fms_1.nvd;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();

    s_wsfe(&io___409);
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
    s_wsfe(&io___410);
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
	ev0 = ev[i__];
	ex0 = ex[i__ - 1];
	em0 = em[i__ - 1];
	eh0 = eh[i__ - 1];
	if (ev[i__] < ev[i__ - 1]) {
	    ev0 = 0.;
	}
	if (ex[i__ - 1] < ex[i__ - 2]) {
	    ex0 = 0.;
	}
	if (em[i__ - 1] < em[i__ - 2]) {
	    em0 = 0.;
	}
/*         if ( Eh(i) .lt. Eh(i-1) ) eh0 = 0.0 */
	if (ev[i__] < ev[i__ - 1] && kk == 0) {
	    kk = 1;
	    s_wsle(&io___415);
	    e_wsle();
	    s_wsle(&io___416);
	    e_wsle();
	}
	s_wsfe(&io___417);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ex0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&em0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eh0, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___418);
	do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ev0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ex0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&em0, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&eh0, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    s_wsfe(&io___419);
    e_wsfe();
    s_wsfe(&io___420);
    e_wsfe();
    s_wsfe(&io___421);
    e_wsfe();
    for (k = 1; k <= 6; ++k) {
	if (k == 1) {
	    s_wsfe(&io___423);
	    e_wsfe();
	}
	if (k == 1) {
	    s_wsfe(&io___424);
	    e_wsfe();
	}
	if (k == 2) {
	    s_wsfe(&io___425);
	    e_wsfe();
	}
	if (k == 2) {
	    s_wsfe(&io___426);
	    e_wsfe();
	}
	if (k == 2) {
	    s_wsfe(&io___427);
	    e_wsfe();
	}
	if (k == 3) {
	    s_wsfe(&io___428);
	    e_wsfe();
	}
	if (k == 3) {
	    s_wsfe(&io___429);
	    e_wsfe();
	}
	if (k == 3) {
	    s_wsfe(&io___430);
	    e_wsfe();
	}
	if (k == 4) {
	    s_wsfe(&io___431);
	    e_wsfe();
	}
	if (k == 4) {
	    s_wsfe(&io___432);
	    e_wsfe();
	}
	if (k == 4) {
	    s_wsfe(&io___433);
	    e_wsfe();
	}
	if (k == 5) {
	    s_wsfe(&io___434);
	    e_wsfe();
	}
	if (k == 5) {
	    s_wsfe(&io___435);
	    e_wsfe();
	}
	if (k == 5) {
	    s_wsfe(&io___436);
	    e_wsfe();
	}
	if (k == 6) {
	    s_wsfe(&io___437);
	    e_wsfe();
	}
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kv = i__ - 1;
	    if (k == 1) {
		error = (ev3[i__ - 1] - ev2[i__ - 1]) * 100. / ev2[i__ - 1];
		s_wsfe(&io___439);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev2[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev3[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___440);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev2[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev3[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___441);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ev2[i__ - 1], (ftnlen)sizeof(
			doublereal));
		e_wsfe();
	    } else if (k == 2) {
		error = (ev4[i__ - 1] - ev3[i__ - 1]) * 100. / ev3[i__ - 1];
		s_wsfe(&io___442);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev3[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev4[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___443);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev3[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev4[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___444);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ev3[i__ - 1], (ftnlen)sizeof(
			doublereal));
		e_wsfe();
	    } else if (k == 3) {
		error = (ev5[i__ - 1] - ev4[i__ - 1]) * 100. / ev4[i__ - 1];
		s_wsfe(&io___445);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev4[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev5[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___446);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev4[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev5[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___447);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ev4[i__ - 1], (ftnlen)sizeof(
			doublereal));
		e_wsfe();
	    } else if (k == 4) {
		error = (ev6[i__ - 1] - ev5[i__ - 1]) * 100. / ev5[i__ - 1];
		s_wsfe(&io___448);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev5[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev6[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___449);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev5[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev6[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___450);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ev5[i__ - 1], (ftnlen)sizeof(
			doublereal));
		e_wsfe();
	    } else if (k == 5) {
		error = (ev[i__] - ev6[i__ - 1]) * 100. / ev6[i__ - 1];
		s_wsfe(&io___451);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev6[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev[i__] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___452);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		d__1 = ev6[i__ - 1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		d__2 = ev[i__] * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&error, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___453);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ev6[i__ - 1], (ftnlen)sizeof(
			doublereal));
		e_wsfe();
	    } else if (k == 6) {
		s_wsfe(&io___454);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
		e_wsfe();
	    }
	}
    }

/* ==================================================== */
/* -- bjj is classicle rotational ANGular momentum */
/* -- bvj is rotational ANGular velocity */

    s_wsfe(&io___455);
    do_fio(&c__1, (char *)&rinert, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&brigid, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___456);
    do_fio(&c__1, (char *)&rinert, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&brigid, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___457);
    do_fio(&c__1, (char *)&rinert, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&brigid, (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = nj[1];
    for (j = 1; j <= i__1; ++j) {
	bj = j * 1. - 1.;
	bjj = sqrt(bj * (bj + 1.));
	bvj = bjj / rinert;
	s_wsfe(&io___460);
	i__2 = j - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ej[j - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&er[j - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bjj, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bvj, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___461);
	i__2 = j - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ej[j - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&er[j - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bjj, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&bvj, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___462);
	i__2 = j - 1;
	do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ej[j - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    if (nj[1] > 0) {
	s_wsfe(&io___463);
	do_fio(&c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___464);
	do_fio(&c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___465);
	do_fio(&c__1, (char *)&eswitc2_1.ade, (ftnlen)sizeof(doublereal));
	e_wsfe();
	sumdif = 0.;
	difj0 = evj[evj_dim1 + 1];
	evjmax = 0.;
	jj = 0;
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kv = i__ - 1;
	    nj0 = nj[i__];
	    kk = 0;
	    if (evj[i__ + evj_dim1] <= emax) {
		if (evj[i__ + evj_dim1] > evjmax) {
		    evjmax = evj[i__ + evj_dim1];
		}
	    }
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		kj = j - 1;
		if (j >= 2) {
		    difvj = evj[i__ + j * evj_dim1] - evj[i__ + (j - 1) * 
			    evj_dim1];
		}
		if (j == 1) {
		    difvj = difj0;
		}
		if (evj[i__ + j * evj_dim1] < evj[i__ + 1 + evj_dim1]) {
		    sumdif += difvj;
		}
		if (evj[i__ + j * evj_dim1] > evj[i__ + 1 + evj_dim1] && kk ==
			 0) {
		    difj0 = evj[i__ + 1 + evj_dim1] - evj[i__ + (j - 1) * 
			    evj_dim1];
		    if (evj[i__ + (j - 1) * evj_dim1] > evjmax) {
			if (evj[i__ + (j - 1) * evj_dim1] <= emax) {
			    evjmax = evj[i__ + (j - 1) * evj_dim1];
			}
		    }
		    kk = 1;
		    s_wsle(&io___471);
		    e_wsle();
		    s_wsle(&io___472);
		    e_wsle();
		    s_wsle(&io___473);
		    e_wsle();
		}
		s_wsfe(&io___474);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * evj_dim1], (ftnlen)
			sizeof(doublereal));
		d__1 = evj[i__ + j * evj_dim1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&difvj, (ftnlen)sizeof(doublereal));
		d__2 = difvj * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___475);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * evj_dim1], (ftnlen)
			sizeof(doublereal));
		d__1 = evj[i__ + j * evj_dim1] * aucm;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&difvj, (ftnlen)sizeof(doublereal));
		d__2 = difvj * aucm;
		do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___476);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * evj_dim1], (ftnlen)
			sizeof(doublereal));
		e_wsfe();
		if (evj[i__ + j * evj_dim1] == emax) {
		    jj = 1;
		}
		if (evj[i__ + j * evj_dim1] == emax) {
		    sumdif += difvj;
		}
	    }
	    if (jj == 1) {
		s_wsle(&io___477);
		e_wsle();
		s_wsle(&io___478);
		e_wsle();
		s_wsle(&io___479);
		e_wsle();
	    } else {
		s_wsfe(&io___480);
		e_wsfe();
		s_wsfe(&io___481);
		e_wsfe();
		s_wsfe(&io___482);
		e_wsfe();
	    }
	}
	s_wsfe(&io___483);
	do_fio(&c__1, (char *)&sumdif, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&evjmax, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___484);
	do_fio(&c__1, (char *)&sumdif, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&evjmax, (ftnlen)sizeof(doublereal));
	e_wsfe();

	s_wsfe(&io___485);
	e_wsfe();
	s_wsfe(&io___486);
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
		if (evj[i__ + j * evj_dim1] > evj[i__ + 1 + evj_dim1] && kk ==
			 0) {
		    kk = 1;
		    s_wsle(&io___487);
		    e_wsle();
		    s_wsle(&io___488);
		    e_wsle();
		}
		s_wsfe(&io___489);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * evj_dim1], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
		d__1 = aj1[i__ + j * 200 - 201] + aj2[i__ + j * 200 - 201];
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		e_wsfe();
		s_wsfe(&io___490);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * evj_dim1], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&ewj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&ej[j - 1], (ftnlen)sizeof(doublereal));
		e_wsfe();
		if (evj[i__ + j * evj_dim1] == emax) {
		    jj = 1;
		}
	    }
	    if (jj == 1) {
		s_wsle(&io___491);
		e_wsle();
		s_wsle(&io___492);
		e_wsle();
	    } else {
		s_wsfe(&io___493);
		e_wsfe();
		s_wsfe(&io___494);
		e_wsfe();
	    }
	}

	s_wsfe(&io___495);
	e_wsfe();
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kv = i__ - 1;
	    nj0 = nj[i__];
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		kj = j - 1;
		s_wsfe(&io___496);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * evj_dim1], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&ev[i__], (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&aj1[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&aj2[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		e_wsfe();
	    }
	    s_wsle(&io___497);
	    e_wsle();
	}

	s_wsfe(&io___498);
	e_wsfe();
	i__1 = *nv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kv = i__ - 1;
	    nj0 = nj[i__];
	    i__2 = nj0;
	    for (j = 1; j <= i__2; ++j) {
		kj = j - 1;
		s_wsfe(&io___499);
		do_fio(&c__1, (char *)&kv, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&kj, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&evj[i__ + j * evj_dim1], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&euj[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&aj1[i__ + j * 200 - 201], (ftnlen)
			sizeof(doublereal));
		e_wsfe();
	    }
	    s_wsle(&io___500);
	    e_wsle();
	}

    }

/* ---------------------------------------------------------- */
/*    #/6x,'evaluated using force constants from NUMerical deriv.', */
/* L520: */
/*    #/12x,'Ev(maximum)  -  Ev(v=0) = ',1PE16.8,/) */
/* 590  format(///25x,"  Einp(v) is the INPUT Ev's ", */
/*    #/27x,"Edifv(v) =  E(v) - Einp(v) ", */
/*    #/27x,"Edifa(v) = Ea(v) - Einp(v) ", */
/*    #/27x,"Edifb(v) = Eb(v) - Einp(v) ",/ */
/*    #/5x,"v    Einp(v; cm-1)    Edifv(v; cm-1)", */
/*    #"    Edifa(cm-1)   Edifb(cm-1)",/) */
/*    #/18x,"Edelv(v) =  [  E(v) -  E(v-1) ] - DEin(v) ", */
/*    #/5x,"v    Einp(v; cm-1)    Edelv(v; cm-1)", */
/* ---------------------------------------------------------- */
L800:
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

/* --------------------------------------------------------------------- */
/*     THIS IS A FACTORIAL FUNCTION  I! */
/* --------------------------------------------------------------------- */
/* --------------------------------------------------------------------- */
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
/* ----------------------- */
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
/* ----------------------- */
L200:
    return ret_val;
} /* facx_ */

