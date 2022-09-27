/* ../src/calVIB.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__9 = 9;

/*             calVIB.f */
/*  The program is to calculate the vibrational spectrum . */

/*  number ---  the number of the bound state used; */

/* Main program */ int MAIN__(void)
{
    /* Format strings */
    static char fmt_100[] = "(1x,a,i2,a,1pe15.6)";
    static char fmt_200[] = "(1x,a,1pe15.6,a,1pe15.6,a,1pe15.6)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_wsle(cilist *), e_wsle(void), s_wsfe(cilist *), 
	    do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer i__;
    static doublereal fg[100], we, d1fg[100], d2fg[100], d3fg[100], evib[
	    10000]	/* was [100][100] */, wexe, weye;
    static integer number;
    static doublereal vibspec[100];

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 5, 0, 0, 0 };
    static cilist io___3 = { 0, 5, 0, 0, 0 };
    static cilist io___8 = { 0, 23, 0, 0, 0 };
    static cilist io___12 = { 0, 21, 0, 0, 0 };
    static cilist io___16 = { 0, 14, 0, 0, 0 };
    static cilist io___17 = { 0, 14, 0, 0, 0 };
    static cilist io___18 = { 0, 14, 0, 0, 0 };
    static cilist io___19 = { 0, 14, 0, fmt_100, 0 };
    static cilist io___20 = { 0, 14, 0, 0, 0 };
    static cilist io___21 = { 0, 14, 0, fmt_200, 0 };
    static cilist io___22 = { 0, 14, 0, fmt_200, 0 };
    static cilist io___23 = { 0, 14, 0, fmt_200, 0 };


    s_rsle(&io___1);
    do_lio(&c__3, &c__1, (char *)&number, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___3);
    do_lio(&c__5, &c__1, (char *)&we, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&weye, (ftnlen)sizeof(doublereal));
    e_rsle();
    i__1 = number;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_rsle(&io___8);
	do_lio(&c__5, &c__1, (char *)&evib[i__ - 1], (ftnlen)sizeof(
		doublereal));
	e_rsle();
	fg[i__ - 1] = evib[i__ - 1];
/* L10: */
    }
    i__1 = number - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d1fg[i__ - 1] = fg[i__] - fg[i__ - 1];
	s_wsle(&io___12);
	do_lio(&c__9, &c__1, "d1FG(", (ftnlen)5);
	do_lio(&c__3, &c__1, (char *)&i__, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ") = ", (ftnlen)4);
	do_lio(&c__5, &c__1, (char *)&d1fg[i__ - 1], (ftnlen)sizeof(
		doublereal));
	e_wsle();
/* L20: */
    }
    i__1 = number - 2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d2fg[i__ - 1] = d1fg[i__] - d1fg[i__ - 1];
/* L30: */
    }
    i__1 = number - 3;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d3fg[i__ - 1] = d2fg[i__] - d2fg[i__ - 1];
/* L40: */
    }
    vibspec[2] = 0.;
    i__1 = number - 3;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vibspec[2] += d3fg[i__ - 1];
/* L50: */
    }
    vibspec[2] /= (number - 3) * 6.;
    vibspec[1] = 0.;
    i__1 = number - 2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vibspec[1] = vibspec[1] + vibspec[2] * 6. * (i__ + .5) - d2fg[i__ - 1]
		;
/* L60: */
    }
    vibspec[1] /= (number - 2) * 2.;
    vibspec[0] = 0.;
    i__1 = number - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vibspec[0] = vibspec[0] + d1fg[i__ - 1] + vibspec[1] * 2. * (i__ - .5)
		;
/* Computing 2nd power */
	d__1 = i__ - .5;
	vibspec[0] -= vibspec[2] * (d__1 * d__1 * 3. + .083333333333333329);
/* L70: */
    }
    vibspec[0] /= number - 1;
    s_wsle(&io___16);
    e_wsle();
    s_wsle(&io___17);
    do_lio(&c__9, &c__1, " The value of spectra as follows : ", (ftnlen)35);
    e_wsle();
    s_wsle(&io___18);
    e_wsle();
    i__1 = number;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_wsfe(&io___19);
	do_fio(&c__1, "spec(", (ftnlen)5);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	do_fio(&c__1, ") = ", (ftnlen)4);
	do_fio(&c__1, (char *)&vibspec[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L80: */
    }
    s_wsle(&io___20);
    e_wsle();
    s_wsfe(&io___21);
    do_fio(&c__1, "We  =", (ftnlen)5);
    do_fio(&c__1, (char *)&we, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, "  We_cal  =", (ftnlen)11);
    do_fio(&c__1, (char *)&vibspec[0], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, "  Edif%=", (ftnlen)8);
    d__1 = (we - vibspec[0]) / we;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___22);
    do_fio(&c__1, "Wexe=", (ftnlen)5);
    do_fio(&c__1, (char *)&wexe, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, "  Wexe_cal=", (ftnlen)11);
    do_fio(&c__1, (char *)&vibspec[1], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, "  Edif%=", (ftnlen)8);
    d__1 = (wexe - vibspec[1]) / wexe;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___23);
    do_fio(&c__1, "Weye=", (ftnlen)5);
    do_fio(&c__1, (char *)&weye, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, "  Weye_cal=", (ftnlen)11);
    do_fio(&c__1, (char *)&vibspec[2], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, "  Edif%=", (ftnlen)8);
    d__1 = (weye - vibspec[2]) / weye;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();
    return 0;
} /* MAIN__ */

/* Main program alias */ int main_ () { MAIN__ (); return 0; }
