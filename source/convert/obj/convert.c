/* ../src/convert.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__5 = 5;

/*     program convert */

/* 1.)    This program is to convert cross sections from square */
/*      Angstrom (A**2) to square Bohr (a0**2); or to convert ENERGIES. */
/* 2.)    To convert y's from Hartree to cm-1. */

/* ---------------------------------------------------------------------- */
/* Main program */ int MAIN__(void)
{
    /* Format strings */
    static char fmt_720[] = "(f10.5,3(2x,1pe18.10))";
    static char fmt_730[] = "(f10.5,4(2x,1pe14.6))";
    static char fmt_740[] = "(f10.5,1x,f19.8)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen),
	     e_wsfe(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static doublereal a;
    static integer i__, m, n;
    static doublereal x[7000], y[7000], y1[1500], y2[1500], y3[1500], y4[1500]
	    ;
    static integer ky, ny;
    static doublereal aucm, conv;

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 5, 0, 0, 0 };
    static cilist io___10 = { 0, 5, 0, 0, 0 };
    static cilist io___12 = { 0, 5, 0, 0, 0 };
    static cilist io___14 = { 0, 5, 0, 0, 0 };
    static cilist io___16 = { 0, 5, 0, 0, 0 };
    static cilist io___18 = { 0, 5, 0, 0, 0 };
    static cilist io___21 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___22 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___23 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___24 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___25 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___26 = { 0, 5, 0, 0, 0 };
    static cilist io___27 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___28 = { 0, 5, 0, 0, 0 };
    static cilist io___29 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___30 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___31 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___33 = { 0, 6, 0, fmt_730, 0 };
    static cilist io___34 = { 0, 5, 0, 0, 0 };
    static cilist io___35 = { 0, 6, 0, fmt_740, 0 };


/* ---------------------------------------------------------------------- */

/*     Read the data in one symmetry. */
/*     M  --> the type of conversion : */
/*            =1, convert XSCs from Angstrom**2 to Bohr**2; */
/*            =2, convert XSC & its error from Angstrom**2 to Bohr**2; */
/*            =3, convert energy from Hartree to eV; */
/*            =4, convert energy from Rydberg to eV; */
/*            =5, convert energy from Rydberg to eV, & */
/*                XSCs from Angstrom**2 to Bohr**2; */
/*            =6, convert energy from cm-1 to Hartree; */
/*            =7, convert energy from Hartree to cm-1 ; */
/*            =8, convert XSCs from Bohr**2 to Angstrom**2 (A**2). */

/*            =11, convert y(i) from Hartree to cm-1. */

/*     N  --> the total number of lines of the data */

/*     ny --> the number of sets of y arrays */

/*     ky --> the kth set to be used in the ny y arrays */

/*     a  --> scaling constant for the XSCs. a=1.0 for unit converting. */

/*                    Angstrom**2 = 1D-16 (cm)**2 */
/*              Convert  1D-16 cm2/sr  to  (ao)**2 ,  a = 1.0      ; */
/*              Convert  1D-18 cm2/sr  to  (ao)**2 ,  a = 0.01     ; */
/*              Convert  1D-20 cm2/sr  to  (ao)**2 ,  a = 0.0001   ; */
/*              Convert  1D-22 cm2/sr  to  (ao)**2 ,  a = 0.000001 . */

/*              Convert  (a0)**2 to  1D-16 cm2/sr ,   a = 1.0      ; */
/*              Convert  (a0)**2 to  1D-18 cm2/sr ,   a = 100      ; */
/*              Convert  (a0)**2 to  1D-20 cm2/sr ,   a = 10000    ; */
/*              Convert  (a0)**2 to  1D-22 cm2/sr ,   a = 1000000  . */

/* --------------------------------------------------------------------------- */
    conv = .280028561;
    aucm = 219474.63067;

/*     read (5,700) M,N */
    s_rsle(&io___3);
    do_lio(&c__3, &c__1, (char *)&m, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ny, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ky, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&a, (ftnlen)sizeof(doublereal));
    e_rsle();
/* ---------------------------------- */
    if (m == 6) {
	goto L80;
    }
    if (m == 7) {
	goto L90;
    }
/* ---------------------------------- */
    if (m == 11) {
	goto L200;
    }
/* ---------------------------------- */

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (ny == 0) {
	    s_rsle(&io___10);
	    do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	} else if (ny == 1) {
	    s_rsle(&io___12);
	    do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	} else if (ny == 2) {
	    s_rsle(&io___14);
	    do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	} else if (ny == 3) {
	    s_rsle(&io___16);
	    do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	} else if (ny == 4) {
	    s_rsle(&io___18);
	    do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y4[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	}
	if (ky == 1) {
	    y[i__ - 1] = y1[i__ - 1];
	}
	if (ky == 2) {
	    y[i__ - 1] = y2[i__ - 1];
	}
	if (ky == 3) {
	    y[i__ - 1] = y3[i__ - 1];
	}
	if (ky == 4) {
	    y[i__ - 1] = y4[i__ - 1];
	}
/* L5: */
    }
/* ---------------------------------- */
    if (m == 2) {
	goto L20;
    }
    if (m == 3) {
	goto L30;
    }
    if (m == 4) {
	goto L50;
    }
    if (m == 5) {
	goto L70;
    }
    if (m == 8) {
	goto L150;
    }
/* ---------------------------------- */
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	y[i__ - 1] = a * y[i__ - 1] / conv;
	s_wsfe(&io___21);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&y[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L10: */
    }
    goto L990;
/* ---------------------------------- */

L20:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*       read (5,*) x(i),y(i),y2(i) */
	y[i__ - 1] = a * y[i__ - 1] / conv;
	y2[i__ - 1] = a * y2[i__ - 1] / conv;
	s_wsfe(&io___22);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&y[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L25: */
    }
    goto L990;

/* ---------------------------------- */
L30:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__ - 1] *= 27.21139618;
	s_wsfe(&io___23);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&y[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L40: */
    }
    goto L990;
/* ---------------------------------- */

L50:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__ - 1] = x[i__ - 1] * 27.21139618 / 2.;
	s_wsfe(&io___24);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&y[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L60: */
    }
    goto L990;
/* ---------------------------------- */

L70:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__ - 1] = x[i__ - 1] * 27.21139618 / 2.;
	y[i__ - 1] = a * y[i__ - 1] / conv;
	s_wsfe(&io___25);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&y[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L75: */
    }
    goto L990;
/* ---------------------------------- */

L80:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_rsle(&io___26);
	do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	e_rsle();
	x[i__ - 1] /= 219474.63067;
	s_wsfe(&io___27);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    goto L990;
/* ---------------------------------------------------- */

L90:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_rsle(&io___28);
	do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	e_rsle();
	x[i__ - 1] *= 219474.63067;
	s_wsfe(&io___29);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    goto L990;
/* ---------------------------------------------------- */
L150:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (ny == 1) {
	    y1[i__ - 1] = a * conv * y1[i__ - 1];
	    s_wsfe(&io___30);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	} else if (ny == 2) {
	    y1[i__ - 1] = a * conv * y1[i__ - 1];
	    y2[i__ - 1] = a * conv * y2[i__ - 1];
	    s_wsfe(&io___31);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	} else if (ny == 3) {
	    y1[i__ - 1] = a * conv * y1[i__ - 1];
	    y2[i__ - 1] = a * conv * y2[i__ - 1];
	    y3[i__ - 1] = a * conv * y3[i__ - 1];
	    s_wsfe(&io___32);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	} else if (ny == 4) {
	    y1[i__ - 1] = a * conv * y1[i__ - 1];
	    y2[i__ - 1] = a * conv * y2[i__ - 1];
	    y3[i__ - 1] = a * conv * y3[i__ - 1];
	    y4[i__ - 1] = a * conv * y4[i__ - 1];
	    s_wsfe(&io___33);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y4[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* L155: */
    }
    goto L990;
/* ---------------------------------------------------- */

L200:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_rsle(&io___34);
	do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&y[i__ - 1], (ftnlen)sizeof(doublereal));
	e_rsle();
	y[i__ - 1] *= aucm;
	s_wsfe(&io___35);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&y[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

/* ---------------------------------------------------- */
/* L700: */
/* 720 format(f10.5,2x,f15.10,2x,f15.10) */
/* 720 format(F10.5,2x,1PE18.10,2x,1PE18.10) */
/* ---------------------------------------------------- */
L990:
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */

