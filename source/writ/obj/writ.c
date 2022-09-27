/* ../src/writ.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__9 = 9;
static integer c__72 = 72;

/*     program writ.f */

/*     This program is to do one of the following : */
/* 1.)   read n line m column data and then write them separately to n */
/*     sets (fort.11, ..., fort.1n). */
/* 2.)   read in MN=N1*N2 data pairs, and then write them to N1 files */
/* 3.)   read in MN=N1*N2 data pairs, sum y(i) of the N1 sets, then */
/*     write the x(i) and the summed y(i) out. */
/* 4.)   read in MN=N1*N2 data pairs, write x and N1 sets of y's : */
/*           x(i)   y1(i)   y2(i)   y3(i)   y4(i) */
/* 5.)   read in MN=N1*N2 data pairs, write x and N1 sets of y's & SUMMED y's: */
/*           x(i)   y1(i)   y2(i)   y3(i)   y4(i)    Y-sum */
/*  ....................... */
/* 6.)   read in x & y, then write out the pair which satisfies x=a . */
/* 7.)   read in x(i) & y(i) for every z(k), then write out z(k) & y(k) */
/*     pairs for every x(i). */
/* 8.)   read in  x(i), y1(i), y2(i), then write out y1(i) & y2(i) pairs. */

/* 9.) For N3 < 10 : */
/*       read in from fort.5 :  nx(i),y1(i),y2(i)              if N4=2 */
/*       read in from fort.5 :  nx(i),y1(i),y2(i),y3(i)        if N4=3 */
/*       read in from fort.5 :  nx(i),y1(i),y2(i),y3(i),y4(i)  if N4=4 */
/*             & read in nx(i), y5(i)  from fort.3 (i=1,N2;  N2 >= N1), */
/*       then write out : */
/*                   nx(i), y1(i), y2(i), y5(i), dy5,  if N3 = 1; */
/*                   nx(i), y1(i), y5(i), y2(i), dy2,  if N3 = 2; */
/*                   nx(i), y5(i), y1(i), y2(i), dy2,  if N3 = 3; */

/*       nx(i), y15(i), y1(i), y2(i), y5(i), Dy5(i), y52(i),  if N3 = 4; */
/*       nx(i), y12(i), y1(i), y5(i), y2(i), Dy2(i), y25(i),  if N3 = 5; */
/*       nx(i), y52(i), y5(i), y1(i), y2(i), Dy2(i), y21(i),  if N3 = 6. */

/*             [ dy# = y#(i) - y#(i-1) ;  y52(i) = y5(i) - y2(i) ] */

/*     For N3 > 10 : */
/*       read in  nx(i), y1(i), y2(i), y3(i), then write out : */
/*                   nx(i), y1(i), dy1(i)  if N3 = 11; */
/*                   nx(i), y2(i), dy2(i)  if N3 = 12; */
/*                   nx(i), y3(i), dy3(i)  if N3 = 13; */

/*     For N3 > 20 : */
/*       read in  nx(i), y1(i), y2(i), y3(i), then write out : */
/*                   nx(i), dy1(i)  if N3 = 21; */
/*                   nx(i), dy2(i)  if N3 = 22; */
/*                   nx(i), dy3(i)  if N3 = 23. */


/* 10.)  M=10:  (Fill ZEROs to some array elements) */
/*               N1 -- the number of lines of the data. */
/*              For N2=0,  read in x(i) & y2(i), then ZERO y2(i) & */
/*                                                    ---- */
/*                         write x(i) & y2(i) in fort.8 ; */
/*              For N2=1,  read in y1(i)[==x(i)] & y2(i) from fort.8 & */
/*                         read in  xx(i) &  yy(i) from fort.5 ; */
/*                         if y1(i) .lt. xx(i)  {fill ZEROs}, */
/*                                               ---------- */
/*                             put [ y1(i),y2(i) ] into [ x(i), y3(i) ]; */
/*                         then */
/*                             paste [ xx(i),yy(i) ] to [ x, y3 ] */
/*                         and  write out (x, y3). */

/* 11.)  M=11:   To add up static & MAP polarization potentials : */
/*                   V_stmap = V_static + V_map */

/* 12.)  M=12:   read in x(i), y1(i);   i=1,N1.   Then write out : */
/*               x(i), y1(i)-a1     for i =< N2 */
/*               x(i), y1(i)-a2     for i  > N2 */

/* 13.)  M=13:   read in nx(i), y1(i), y2(i), y3(i); then write out : */
/*                 y1(i), y3(i)    i=1,N1 */
/*                 y2(i), y3(i)    i=N1,1,-1 */
/*               Change y1 & y2 from Anstrom to ao  if N2 = 1. */

/*          read in  y1(i), y3(i) &  Change  y3 = y3 - a  if N2 = 2. */

/*          read in  y1(i), y3(i) &  Change y1 from Anstrom to ao, */
/*              and     Change  y3 = y3 - a  if N2 = 3. */

/* ========================================================================= */
/* Main program */ int MAIN__(void)
{
    /* Format strings */
    static char fmt_620[] = "(f10.5,2x,f16.10)";
    static char fmt_630[] = "(f10.5,2x,e14.6)";
    static char fmt_640[] = "(f10.5,2x,1pe20.13)";
    static char fmt_660[] = "(/3x,\002  x(i)        y1(i)         y2(i)\002,"
	    "/)";
    static char fmt_665[] = "(/3x,\002  x(i)        y1(i)         y2(i)     "
	    "     y3(i)\002,/)";
    static char fmt_670[] = "(/3x,\002  x(i)        y1(i)         y2(i)     "
	    "     y3(i)               y4(i)\002,/)";
    static char fmt_675[] = "(/3x,\002 x(i)       y1(i)        y2(i)        "
	    "Y-SUM\002,/)";
    static char fmt_680[] = "(/3x,\002 x(i)       y1(i)        y2(i)        "
	    " y3(i)\002,\002        Y-SUM\002,/)";
    static char fmt_685[] = "(/3x,\002 x(i)       y1(i)        y2(i)        "
	    " y3(i)\002,\002        y4(i)        Y-SUM\002,/)";
    static char fmt_690[] = "(f10.5,4(1pe15.6))";
    static char fmt_700[] = "(f8.4,5(1pe14.6))";
    static char fmt_650[] = "(a72)";
    static char fmt_625[] = "(2x,f10.5,1x,e16.8)";
    static char fmt_710[] = "(//3x,\002 The rearranged data are : \002)";
    static char fmt_720[] = "(//3x,\002    For radius  r = \002,f10.6/)";
    static char fmt_610[] = "(f10.5,1pe23.15)";
    static char fmt_725[] = "(/3x,\002  x(k)          y(k)\002,15x,\002dif[y"
	    "(k)-y(k-1)]\002,/)";
    static char fmt_730[] = "(f10.5,2(1pe23.15))";
    static char fmt_740[] = "(/3x,\002  The average difference of Y is,  AVE"
	    "dy =\002,1pe23.15)";
    static char fmt_750[] = "(2(1pe18.10,2x))";
    static char fmt_810[] = "(//4x,\002Read in N1(=\002,i3,\002) rows of dat"
	    "a from unit 5; \002,/4x,\002Read in N2(=\002,i3,\002) rows of da"
	    "ta from unit 3; \002,/4x,\002           [ N2 >= N1 ]     \002,//"
	    "4x,\002( Print  nx(i) y1(i) y2(i) y5(i) if N3 = 1; \002,/4x,\002"
	    "         nx(i) y1(i) y5(i) y2(i) if N3 = 2; \002,/4x,\002       "
	    "  nx(i) y5(i) y1(i) y2(i) if N3 = 3; \002,//4x,\002  nx(i) y51 y"
	    "1(i) y2(i) y5(i) Dy5 y52 if N3 = 4; \002,/4x,\002  nx(i) y21 y1("
	    "i) y5(i) y2(i) Dy2 y25 if N3 = 5; \002,/4x,\002  nx(i) y25 y5(i)"
	    " y1(i) y2(i) Dy2 y21 if N3 = 6; \002,//4x,\002         nx(i) y1("
	    "i) dy1(i)      if N3 = 11; \002,/4x,\002         nx(i) y2(i) dy2"
	    "(i)      if N3 = 12; \002,/4x,\002         nx(i) y3(i) dy3(i)   "
	    "   if N3 = 13; \002,//4x,\002         nx(i) dy1(i)            if"
	    " N3 = 21; \002,/4x,\002         nx(i) dy2(i)            if N3 = "
	    "22; \002,/4x,\002         nx(i) dy3(i)            if N3 = 23 ); "
	    "   N3 =\002,i3,//4x,\002[  y21 = y2(i) - y1(i) ;  Dy2 = y2(i) - "
	    "y2(i-1)  ]\002,/)";
    static char fmt_712[] = "(//10x,\002 The reprinted data are : \002)";
    static char fmt_812[] = "(/2x,\002nx(i)   y1(i)\002,6x,\002y2(i)\002,6x"
	    ",\002y5(i)    Dy5(i)\002,//)";
    static char fmt_814[] = "(/2x,\002nx(i)   y1(i)\002,6x,\002y5(i)\002,6x"
	    ",\002y2(i)    Dy2(i)\002,//)";
    static char fmt_816[] = "(/2x,\002nx(i)   y5(i)\002,6x,\002y1(i)\002,6x"
	    ",\002y2(i)    Dy2(i)\002,//)";
    static char fmt_820[] = "(/\002nx(i)\002,3x,\002y51(i)\002,7x,\002y1(i"
	    ")\002,4x,\002y2(i)\002,4x,\002y5(i)   Dy5(i)   y52(i)\002,//)";
    static char fmt_822[] = "(/\002nx(i)\002,3x,\002y21(i)\002,7x,\002y1(i"
	    ")\002,4x,\002y5(i)\002,4x,\002y2(i)   Dy2(i)   y25(i)\002,//)";
    static char fmt_824[] = "(/\002nx(i)\002,3x,\002y25(i)\002,7x,\002y5(i"
	    ")\002,4x,\002y1(i)\002,4x,\002y2(i)   Dy2(i)   y21(i)\002,//)";
    static char fmt_830[] = "(2x,i3,1x,f10.1,1x,f10.1,1x,f10.1,2x,f7.1)";
    static char fmt_832[] = "(2x,i3,12x,f10.1)";
    static char fmt_834[] = "(i3,f9.1,6x,f9.1,f9.1,f9.1,f8.1,f8.1)";
    static char fmt_836[] = "(i3,23x,f10.1)";
    static char fmt_835[] = "(i3,f9.4,6x,f9.1,f9.1,f9.1,f8.1,f8.4)";
    static char fmt_632[] = "(/\002nx(i)  y1(i)\002,3x,\002y(i)-y(i-1)\002,/"
	    "/)";
    static char fmt_634[] = "(/\002nx(i) y(i)-y(i-1)\002,//)";
    static char fmt_600[] = "(2i4)";
    static char fmt_770[] = "(72a1)";
    static char fmt_775[] = "(2i5)";
    static char fmt_780[] = "(3(f10.5))";
    static char fmt_790[] = "(2(e15.6))";
    static char fmt_800[] = "(f10.5,e23.16)";
    static char fmt_850[] = "(//4x,\002Shifting y's by a1 if i =< N2 (=\002,"
	    "i3,\002 )\002,/4x,\002Shifting y's by a2 if i  > N2 (=\002,i3"
	    ",\002 )\002,//4x,\002   a1 = \002,f12.3,3x,\002   a2 = \002,f12."
	    "3,//4x,\002nx(i)     y(i) \002,/)";
    static char fmt_860[] = "(3x,i3,3x,f11.2)";
    static char fmt_865[] = "(f10.5,1x,f16.5)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen),
	     e_wsfe(void), s_wsle(cilist *), e_wsle(void), s_rsfe(cilist *), 
	    e_rsfe(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static doublereal a;
    static integer i__, j, k, m, n;
    static doublereal x[800], a1, a2, c1[10], c2[10];
    static integer i1, k1, n1, n2, n3, m2, m3, m4, n4;
    static doublereal y1[800], y2[800], y3[800], y4[800], y5[800], y6[800], 
	    y7[800], y8[800], ee, en, rf[20], ay, ri[20];
    static integer mn;
    static doublereal dy;
    static integer nn;
    static doublereal rs[20];
    static integer nx[800];
    static doublereal xx[2000], yy[2000];
    static integer lab[30], nrg;
    static doublereal dyl, dyr, titl0[72];
    static integer labda, ndata, labst, natom;
    static doublereal title[72];
    static integer labmap;
    static doublereal anstoao;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 5, 0, 0, 0 };
    static cilist io___4 = { 0, 5, 0, 0, 0 };
    static cilist io___7 = { 0, 5, 0, 0, 0 };
    static cilist io___18 = { 0, 11, 0, fmt_620, 0 };
    static cilist io___19 = { 0, 12, 0, fmt_620, 0 };
    static cilist io___20 = { 0, 13, 0, fmt_620, 0 };
    static cilist io___21 = { 0, 14, 0, fmt_620, 0 };
    static cilist io___22 = { 0, 15, 0, fmt_620, 0 };
    static cilist io___23 = { 0, 16, 0, fmt_620, 0 };
    static cilist io___24 = { 0, 17, 0, fmt_620, 0 };
    static cilist io___25 = { 0, 18, 0, fmt_620, 0 };
    static cilist io___26 = { 0, 5, 0, 0, 0 };
    static cilist io___30 = { 0, 6, 0, 0, 0 };
    static cilist io___31 = { 0, 5, 0, 0, 0 };
    static cilist io___37 = { 0, 0, 0, fmt_630, 0 };
    static cilist io___38 = { 0, 5, 0, 0, 0 };
    static cilist io___40 = { 0, 5, 0, 0, 0 };
    static cilist io___41 = { 0, 6, 0, fmt_640, 0 };
    static cilist io___42 = { 0, 5, 0, 0, 0 };
    static cilist io___46 = { 0, 6, 0, fmt_660, 0 };
    static cilist io___47 = { 0, 6, 0, fmt_665, 0 };
    static cilist io___48 = { 0, 6, 0, fmt_670, 0 };
    static cilist io___49 = { 0, 6, 0, fmt_675, 0 };
    static cilist io___50 = { 0, 6, 0, fmt_680, 0 };
    static cilist io___51 = { 0, 6, 0, fmt_685, 0 };
    static cilist io___52 = { 0, 5, 0, 0, 0 };
    static cilist io___53 = { 0, 6, 0, fmt_690, 0 };
    static cilist io___54 = { 0, 6, 0, fmt_690, 0 };
    static cilist io___55 = { 0, 6, 0, fmt_690, 0 };
    static cilist io___56 = { 0, 6, 0, fmt_700, 0 };
    static cilist io___57 = { 0, 6, 0, fmt_700, 0 };
    static cilist io___58 = { 0, 6, 0, fmt_700, 0 };
    static cilist io___59 = { 0, 5, 0, 0, 0 };
    static cilist io___62 = { 0, 5, 0, 0, 0 };
    static cilist io___63 = { 0, 5, 0, fmt_650, 0 };
    static cilist io___65 = { 0, 5, 0, fmt_650, 0 };
    static cilist io___66 = { 0, 5, 0, 0, 0 };
    static cilist io___67 = { 0, 5, 0, fmt_650, 0 };
    static cilist io___68 = { 0, 5, 0, 0, 0 };
    static cilist io___69 = { 0, 5, 0, fmt_650, 0 };
    static cilist io___70 = { 0, 5, 0, 0, 0 };
    static cilist io___71 = { 0, 5, 0, 0, 0 };
    static cilist io___72 = { 0, 6, 0, fmt_625, 0 };
    static cilist io___73 = { 0, 5, 0, 0, 0 };
    static cilist io___74 = { 0, 5, 0, 0, 0 };
    static cilist io___75 = { 0, 5, 0, 0, 0 };
    static cilist io___76 = { 0, 6, 0, fmt_710, 0 };
    static cilist io___77 = { 0, 6, 0, fmt_720, 0 };
    static cilist io___78 = { 0, 6, 0, fmt_610, 0 };
    static cilist io___79 = { 0, 6, 0, fmt_725, 0 };
    static cilist io___82 = { 0, 6, 0, fmt_730, 0 };
    static cilist io___83 = { 0, 6, 0, fmt_740, 0 };
    static cilist io___84 = { 0, 5, 0, 0, 0 };
    static cilist io___85 = { 0, 5, 0, 0, 0 };
    static cilist io___86 = { 0, 6, 0, fmt_750, 0 };
    static cilist io___87 = { 0, 5, 0, 0, 0 };
    static cilist io___89 = { 0, 6, 0, fmt_810, 0 };
    static cilist io___90 = { 0, 5, 0, 0, 0 };
    static cilist io___92 = { 0, 5, 0, 0, 0 };
    static cilist io___93 = { 0, 5, 0, 0, 0 };
    static cilist io___94 = { 0, 6, 0, fmt_712, 0 };
    static cilist io___95 = { 0, 6, 0, fmt_812, 0 };
    static cilist io___96 = { 0, 6, 0, fmt_814, 0 };
    static cilist io___97 = { 0, 6, 0, fmt_816, 0 };
    static cilist io___98 = { 0, 6, 0, fmt_820, 0 };
    static cilist io___99 = { 0, 6, 0, fmt_822, 0 };
    static cilist io___100 = { 0, 6, 0, fmt_824, 0 };
    static cilist io___101 = { 0, 3, 0, 0, 0 };
    static cilist io___102 = { 0, 6, 0, fmt_830, 0 };
    static cilist io___103 = { 0, 6, 0, fmt_830, 0 };
    static cilist io___104 = { 0, 6, 0, fmt_832, 0 };
    static cilist io___105 = { 0, 6, 0, fmt_830, 0 };
    static cilist io___106 = { 0, 6, 0, fmt_830, 0 };
    static cilist io___107 = { 0, 6, 0, fmt_832, 0 };
    static cilist io___108 = { 0, 6, 0, fmt_830, 0 };
    static cilist io___109 = { 0, 6, 0, fmt_830, 0 };
    static cilist io___110 = { 0, 6, 0, fmt_832, 0 };
    static cilist io___113 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___114 = { 0, 6, 0, fmt_836, 0 };
    static cilist io___115 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___116 = { 0, 6, 0, fmt_835, 0 };
    static cilist io___117 = { 0, 6, 0, fmt_836, 0 };
    static cilist io___118 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___119 = { 0, 6, 0, fmt_835, 0 };
    static cilist io___120 = { 0, 6, 0, fmt_836, 0 };
    static cilist io___121 = { 0, 6, 0, fmt_632, 0 };
    static cilist io___122 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___123 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___124 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___125 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___126 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___127 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___128 = { 0, 6, 0, fmt_634, 0 };
    static cilist io___129 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___130 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___131 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___132 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___133 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___134 = { 0, 6, 0, fmt_834, 0 };
    static cilist io___135 = { 0, 5, 0, 0, 0 };
    static cilist io___136 = { 0, 8, 0, fmt_600, 0 };
    static cilist io___137 = { 0, 5, 0, 0, 0 };
    static cilist io___138 = { 0, 8, 0, fmt_620, 0 };
    static cilist io___139 = { 0, 5, 0, 0, 0 };
    static cilist io___140 = { 0, 8, 0, 0, 0 };
    static cilist io___142 = { 0, 8, 0, 0, 0 };
    static cilist io___144 = { 0, 6, 0, fmt_600, 0 };
    static cilist io___145 = { 0, 6, 0, fmt_620, 0 };
    static cilist io___146 = { 0, 3, 0, fmt_770, 0 };
    static cilist io___147 = { 0, 4, 0, fmt_770, 0 };
    static cilist io___148 = { 0, 3, 0, 0, 0 };
    static cilist io___150 = { 0, 4, 0, 0, 0 };
    static cilist io___151 = { 0, 5, 0, fmt_770, 0 };
    static cilist io___153 = { 0, 5, 0, 0, 0 };
    static cilist io___157 = { 0, 8, 0, fmt_770, 0 };
    static cilist io___158 = { 0, 8, 0, fmt_775, 0 };
    static cilist io___159 = { 0, 3, 0, fmt_780, 0 };
    static cilist io___163 = { 0, 4, 0, fmt_780, 0 };
    static cilist io___164 = { 0, 8, 0, fmt_780, 0 };
    static cilist io___165 = { 0, 3, 0, 0, 0 };
    static cilist io___167 = { 0, 4, 0, 0, 0 };
    static cilist io___168 = { 0, 8, 0, fmt_775, 0 };
    static cilist io___169 = { 0, 3, 0, 0, 0 };
    static cilist io___172 = { 0, 4, 0, 0, 0 };
    static cilist io___173 = { 0, 8, 0, fmt_790, 0 };
    static cilist io___174 = { 0, 3, 0, 0, 0 };
    static cilist io___177 = { 0, 4, 0, 0, 0 };
    static cilist io___178 = { 0, 8, 0, fmt_775, 0 };
    static cilist io___179 = { 0, 3, 0, fmt_800, 0 };
    static cilist io___180 = { 0, 4, 0, fmt_800, 0 };
    static cilist io___181 = { 0, 8, 0, fmt_800, 0 };
    static cilist io___182 = { 0, 3, 0, 0, 0 };
    static cilist io___183 = { 0, 8, 0, fmt_775, 0 };
    static cilist io___184 = { 0, 3, 0, fmt_800, 0 };
    static cilist io___185 = { 0, 8, 0, fmt_800, 0 };
    static cilist io___186 = { 0, 5, 0, 0, 0 };
    static cilist io___189 = { 0, 6, 0, fmt_850, 0 };
    static cilist io___190 = { 0, 5, 0, 0, 0 };
    static cilist io___191 = { 0, 6, 0, fmt_860, 0 };
    static cilist io___192 = { 0, 6, 0, fmt_860, 0 };
    static cilist io___193 = { 0, 5, 0, 0, 0 };
    static cilist io___194 = { 0, 5, 0, 0, 0 };
    static cilist io___195 = { 0, 5, 0, 0, 0 };
    static cilist io___196 = { 0, 6, 0, fmt_865, 0 };
    static cilist io___197 = { 0, 6, 0, fmt_865, 0 };


/* The next line is for purpose 1.) */
/*     dimension y5(nmax),y6(nmax),y7(nmax),y8(nmax) */
/* The next line is for purpose 2.) */
/* The next two lines are for purpose 11.) */
/* ------------------------------------------------------------------------- */
/*     Read the data in one symmetry. */
/*     M = 1, for purpose 1.); =2 for 2.); =3 for 3.); =4 for 4.); */
/*       = 6, for purpose 6.); =7 for 7.); =8 for 8.), ... */

/*  For 1.), 8.) : */
/*      read (5,*) N */
/*     N  --> the total number of lines of the data */

/*  For 2.), 3.), 4.), 5.), 7.) : */
/*     N1 --> The # of data sets in the input data */
/*     N2 --> The # of data pairs in each data set */

/*  For 6.) : */
/*      read (5,*) N1,N2,en,a */
/*     en --> Energy in Rydberg;   a --> Angle in degree. */

/*  For 9.) : */
/*      read (5,*) N1,N2,N3 */
/*     N1 --> The # of data pairs in fort.5; */
/*     N2 --> The # of data pairs in fort.3; */
/*     N3 --> A switch number for printting (see above). */


/* ------------------------------------------------------------------------- */
    s_rsle(&io___1);
    do_lio(&c__3, &c__1, (char *)&m, (ftnlen)sizeof(integer));
    e_rsle();

/* -------------------------------- */
    anstoao = .529177249;
/* -------------------------------- */
    if (m == 6) {
	goto L150;
    }
    if (m == 7) {
	goto L170;
    }
    if (m == 8) {
	goto L200;
    }
    if (m == 9) {
	goto L220;
    }
    if (m == 10) {
	goto L240;
    }
    if (m == 11) {
	goto L260;
    }
    if (m == 12) {
	goto L280;
    }
    if (m == 13) {
	goto L300;
    }
/* -------------------------------- */
    if (m == 1) {
/* For purpose 1.) */
/*       read (5,600) N */
	s_rsle(&io___4);
	do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
	e_rsle();
	i__1 = n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___7);
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
	    do_lio(&c__5, &c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y6[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y7[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y8[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
/*         read (5,*) x(i),y1(i),y2(i),y3(i),y4(i),y5(i) */
/*         read (5,*) x(i),y1(i),y2(i),y3(i),y4(i) */
/* L10: */
	}
/* ------------------------------------------------------------------------- */
/*         a=1.0d0 */
	a = 27.21139618;
	i__1 = n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_wsfe(&io___18);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = y1[i__ - 1] * a;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___19);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = y2[i__ - 1] * a;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___20);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = y3[i__ - 1] * a;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___21);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = y4[i__ - 1] * a;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();

	    s_wsfe(&io___22);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = y5[i__ - 1] * a;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___23);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = y6[i__ - 1] * a;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___24);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = y7[i__ - 1] * a;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    s_wsfe(&io___25);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    d__1 = y8[i__ - 1] * a;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
/* L20: */
	}
	goto L990;

    }
/* ------------------------------------------------------------------------- */
/* For purpose 2.) */
    if (m == 2) {
/*       read (5,600) N1,N2 */
	s_rsle(&io___26);
	do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
	e_rsle();
	n3 = n1 * n2;
	s_wsle(&io___30);
	do_lio(&c__9, &c__1, "  M,N1,N2,N3= ", (ftnlen)14);
	do_lio(&c__3, &c__1, (char *)&m, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&n3, (ftnlen)sizeof(integer));
	e_wsle();
	i__1 = n3;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___31);
	    do_lio(&c__5, &c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&yy[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
/*         read (5,610) xx(i),yy(i) */
/* L40: */
	}

	i1 = 18;
	i__1 = n1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i1 += 2;
	    i__2 = n2;
	    for (k = 1; k <= i__2; ++k) {
		k1 = (i__ - 1) * n2 + k;
		io___37.ciunit = i1;
		s_wsfe(&io___37);
		do_fio(&c__1, (char *)&xx[k1 - 1], (ftnlen)sizeof(doublereal))
			;
		do_fio(&c__1, (char *)&yy[k1 - 1], (ftnlen)sizeof(doublereal))
			;
		e_wsfe();
/* L50: */
	    }
/* L60: */
	}
	goto L990;
    }

/* ------------------------------------------------------------------------- */
/* For purpose 3.) */
    if (m == 3) {
/*       read (5,600) N1,N2 */
	s_rsle(&io___38);
	do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
	e_rsle();
	mn = n1 * n2;
	i__1 = mn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___40);
	    do_lio(&c__5, &c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&yy[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
/*         read (5,610) xx(i),yy(i) */
/* L65: */
	}

	i__1 = n2;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    y1[i__ - 1] = 0.;
	    i__2 = n1;
	    for (k = 1; k <= i__2; ++k) {
		k1 = (k - 1) * n2 + i__;
		y1[i__ - 1] += yy[k1 - 1];
/* L75: */
	    }
	    s_wsfe(&io___41);
	    do_fio(&c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
/* L80: */
	}
	goto L990;
    }

/* ------------------------------------------------------------------------- */
/* For purpose 4.) & 5.) */
    if (m == 4 || m == 5) {
	s_rsle(&io___42);
	do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
	e_rsle();
	mn = n1 * n2;
	m2 = n2 << 1;
	m3 = n2 * 3;
	m4 = n2 << 2;
	if (m == 4) {
	    if (n1 == 2) {
		s_wsfe(&io___46);
		e_wsfe();
	    }
	    if (n1 == 3) {
		s_wsfe(&io___47);
		e_wsfe();
	    }
	    if (n1 == 4) {
		s_wsfe(&io___48);
		e_wsfe();
	    }
	} else {
	    if (n1 == 2) {
		s_wsfe(&io___49);
		e_wsfe();
	    }
	    if (n1 == 3) {
		s_wsfe(&io___50);
		e_wsfe();
	    }
	    if (n1 == 4) {
		s_wsfe(&io___51);
		e_wsfe();
	    }
	}

	i__1 = mn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___52);
	    do_lio(&c__5, &c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&yy[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	    if (i__ <= n2) {
		y1[i__ - 1] = yy[i__ - 1];
	    }
	    if (i__ > n2 && i__ <= m2) {
		y2[i__ - n2 - 1] = yy[i__ - 1];
	    }
	    if (i__ > m2 && i__ <= m3) {
		y3[i__ - m2 - 1] = yy[i__ - 1];
	    }
	    if (i__ > m3 && i__ <= m4) {
		y4[i__ - m3 - 1] = yy[i__ - 1];
	    }
/* L90: */
	}

	if (m == 5) {
	    i__1 = n2;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		y8[i__ - 1] = 0.;
		i__2 = n1;
		for (k = 1; k <= i__2; ++k) {
		    k1 = (k - 1) * n2 + i__;
		    y8[i__ - 1] += yy[k1 - 1];
/* L95: */
		}
	    }
	}

	if (m == 4) {
	    i__2 = n2;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		if (n1 == 2) {
		    s_wsfe(&io___53);
		    do_fio(&c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
		}
		if (n1 == 3) {
		    s_wsfe(&io___54);
		    do_fio(&c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
		}
		if (n1 == 4) {
		    s_wsfe(&io___55);
		    do_fio(&c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y4[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
		}
/* L105: */
	    }
	} else {
	    i__2 = n2;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		if (n1 == 2) {
		    s_wsfe(&io___56);
		    do_fio(&c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y8[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
		}
		if (n1 == 3) {
		    s_wsfe(&io___57);
		    do_fio(&c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y8[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
		}
		if (n1 == 4) {
		    s_wsfe(&io___58);
		    do_fio(&c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y4[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y8[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    e_wsfe();
		}
/* L110: */
	    }
	}
	goto L990;
    }
/* ------------------------------------------------------------------------- */
/* For purpose 6.) */
L150:
    s_rsle(&io___59);
    do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&en, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&a, (ftnlen)sizeof(doublereal));
    e_rsle();
    n3 = 1;
    ee = en * 13.60569809;
/*     Next 8 reads corresponds to the format of my dcs files ONLY !! */
L155:
    s_rsle(&io___62);
    e_rsle();
    s_rsfe(&io___63);
    do_fio(&c__72, (char *)&title[0], (ftnlen)sizeof(doublereal));
    e_rsfe();
    s_rsfe(&io___65);
    do_fio(&c__72, (char *)&title[0], (ftnlen)sizeof(doublereal));
    e_rsfe();
    s_rsle(&io___66);
    e_rsle();
    s_rsfe(&io___67);
    do_fio(&c__72, (char *)&title[0], (ftnlen)sizeof(doublereal));
    e_rsfe();
    s_rsle(&io___68);
    e_rsle();
    s_rsfe(&io___69);
    do_fio(&c__72, (char *)&title[0], (ftnlen)sizeof(doublereal));
    e_rsfe();
    s_rsle(&io___70);
    e_rsle();
    i__2 = n2;
    for (i__ = 1; i__ <= i__2; ++i__) {
	s_rsle(&io___71);
	do_lio(&c__5, &c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	do_lio(&c__5, &c__1, (char *)&yy[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	e_rsle();
	if (n3 == n1 && xx[i__ - 1] == a) {
	    s_wsfe(&io___72);
	    do_fio(&c__1, (char *)&ee, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&yy[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* L160: */
    }
    if (n3 < n1) {
	++n3;
	goto L155;
    }
    goto L990;
/* ------------------------------------------------------------------------- */
/* For purpose 7.) */
L170:
    s_rsle(&io___73);
    do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
    e_rsle();
/* ---- y1(k) -> the internuclear distance; x(i) -> the radius r; */
/* ---- yy(j) -> the potential or polarizability or any data you input. */
    i__2 = n1;
    for (k = 1; k <= i__2; ++k) {
	s_rsle(&io___74);
	do_lio(&c__5, &c__1, (char *)&y1[k - 1], (ftnlen)sizeof(doublereal));
	e_rsle();
	i__1 = n2;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsle(&io___75);
	    do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&yy[i__ + n2 * (k - 1) - 1], (ftnlen)
		    sizeof(doublereal));
	    e_rsle();
	}
/* L175: */
    }
    s_wsfe(&io___76);
    e_wsfe();
    i__2 = n2;
    for (i__ = 1; i__ <= i__2; ++i__) {
	s_wsfe(&io___77);
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__1 = n1;
	for (k = 1; k <= i__1; ++k) {
	    s_wsfe(&io___78);
	    do_fio(&c__1, (char *)&y1[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&yy[i__ + n2 * (k - 1) - 1], (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	}
	s_wsfe(&io___79);
	e_wsfe();
	ay = 0.;
	i__1 = n1;
	for (k = 1; k <= i__1; ++k) {
	    dy = 0.;
	    if (k > 1) {
		dy = yy[i__ + n2 * (k - 1) - 1] - yy[i__ + n2 * (k - 2) - 1];
	    }
	    s_wsfe(&io___82);
	    do_fio(&c__1, (char *)&y1[k - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&yy[i__ + n2 * (k - 1) - 1], (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    ay += dy;
	}
	s_wsfe(&io___83);
	d__1 = ay / (n1 - 1);
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L180: */
    }
    goto L990;
/* ------------------------------------------------------------------------- */
/* For purpose 8.) */
L200:
    s_rsle(&io___84);
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_rsle();

    i__2 = n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	s_rsle(&io___85);
	do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	e_rsle();
/*          read (5,*)   nx(i), y1(i), y2(i) */
	s_wsfe(&io___86);
	do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    goto L990;
/* ------------------------------------------------------------------------- */
/* For purpose 9.) */
L220:
    s_rsle(&io___87);
    do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n3, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n4, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsfe(&io___89);
    do_fio(&c__1, (char *)&n1, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&n2, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&n3, (ftnlen)sizeof(integer));
    e_wsfe();
    i__2 = n1;
    for (i__ = 1; i__ <= i__2; ++i__) {
	if (n4 <= 2) {
	    s_rsle(&io___90);
	    do_lio(&c__3, &c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer)
		    );
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	} else if (n4 == 3) {
	    s_rsle(&io___92);
	    do_lio(&c__3, &c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer)
		    );
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	} else if (n4 == 4) {
	    s_rsle(&io___93);
	    do_lio(&c__3, &c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer)
		    );
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
    }
    s_wsfe(&io___94);
    e_wsfe();
    if (n3 == 1) {
	s_wsfe(&io___95);
	e_wsfe();
    }
    if (n3 == 2) {
	s_wsfe(&io___96);
	e_wsfe();
    }
    if (n3 == 3) {
	s_wsfe(&io___97);
	e_wsfe();
    }
    if (n3 == 4) {
	s_wsfe(&io___98);
	e_wsfe();
    }
    if (n3 == 5) {
	s_wsfe(&io___99);
	e_wsfe();
    }
    if (n3 == 6) {
	s_wsfe(&io___100);
	e_wsfe();
    }

    if (n3 > 10 && n3 < 20) {
	goto L225;
    }
    if (n3 > 20) {
	goto L230;
    }
/* --- */
    i__2 = n2;
    for (i__ = 1; i__ <= i__2; ++i__) {
	s_rsle(&io___101);
	do_lio(&c__3, &c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	e_rsle();
	if (n3 == 1) {
	    if (i__ == 1) {
		s_wsfe(&io___102);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    } else if (i__ <= n1) {
		dy = y5[i__ - 1] - y5[i__ - 2];
		s_wsfe(&io___103);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    } else if (i__ > n1) {
		s_wsfe(&io___104);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    }
	}
/* - */
	if (n3 == 2) {
	    if (i__ == 1) {
		s_wsfe(&io___105);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    } else if (i__ <= n1) {
		dy = y2[i__ - 1] - y2[i__ - 2];
		s_wsfe(&io___106);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    } else if (i__ > n1) {
		s_wsfe(&io___107);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    }
	}
/* - */
	if (n3 == 3) {
	    if (i__ == 1) {
		s_wsfe(&io___108);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    } else if (i__ <= n1) {
		dy = y2[i__ - 1] - y2[i__ - 2];
		s_wsfe(&io___109);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    } else if (i__ > n1) {
		s_wsfe(&io___110);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    }
	}
/* -- */
	if (n3 == 4) {
	    if (i__ <= n1) {
		dy = y5[i__ - 1] - y5[i__ - 2];
		dyl = y5[i__ - 1] - y1[i__ - 1];
		dyr = y5[i__ - 1] - y2[i__ - 1];
		if (n4 == 1) {
		    s_wsfe(&io___113);
		    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&dyl, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&dyr, (ftnlen)sizeof(doublereal));
		    e_wsfe();
		} else if (n4 == 2) {
/*  Use format 835 for PERCENT error */
/*                dyL = 100.0*abs(dyL)/y5(i) */
/*                dyR = 100.0*abs(dyR)/y5(i) */
/*                write(6,835) nx(i),dyL,y1(i),y2(i),y5(i),dy,dyR */
		}
	    } else if (i__ > n1) {
		s_wsfe(&io___114);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    }
	}
/* - */
	if (n3 == 5) {
	    if (i__ <= n1) {
		dy = y2[i__ - 1] - y2[i__ - 2];
		dyl = y2[i__ - 1] - y1[i__ - 1];
		dyr = y2[i__ - 1] - y5[i__ - 1];
		if (n4 == 1) {
		    s_wsfe(&io___115);
		    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&dyl, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&dyr, (ftnlen)sizeof(doublereal));
		    e_wsfe();
		} else if (n4 == 2) {
/*  Use format 835 for PERCENT error */
		    dyl = abs(dyl) * 100. / y2[i__ - 1];
		    dyr = abs(dyr) * 100. / y2[i__ - 1];
		    s_wsfe(&io___116);
		    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&dyl, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&dyr, (ftnlen)sizeof(doublereal));
		    e_wsfe();
		}
	    } else if (i__ > n1) {
		s_wsfe(&io___117);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    }
	}
/* - */
	if (n3 == 6) {
	    if (i__ <= n1) {
		dy = y2[i__ - 1] - y2[i__ - 2];
		dyl = y2[i__ - 1] - y5[i__ - 1];
		dyr = y2[i__ - 1] - y1[i__ - 1];
		if (n4 == 1) {
		    s_wsfe(&io___118);
		    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&dyl, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&dyr, (ftnlen)sizeof(doublereal));
		    e_wsfe();
		} else if (n4 == 2) {
/*  Use format 835 for PERCENT error */
		    dyl = abs(dyl) * 100. / y2[i__ - 1];
		    dyr = abs(dyr) * 100. / y2[i__ - 1];
		    s_wsfe(&io___119);
		    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&dyl, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&y5[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
			    doublereal));
		    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&dyr, (ftnlen)sizeof(doublereal));
		    e_wsfe();
		}
	    } else if (i__ > n1) {
		s_wsfe(&io___120);
		do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    }
	}

    }
    goto L990;
/* ------------------------------------------- */
L225:
    s_wsfe(&io___121);
    e_wsfe();
    if (n3 == 11) {
	s_wsfe(&io___122);
	do_fio(&c__1, (char *)&nx[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&y1[0], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__2 = n1;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    dy = y1[i__ - 1] - y1[i__ - 2];
	    s_wsfe(&io___123);
	    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    } else if (n3 == 12) {
	s_wsfe(&io___124);
	do_fio(&c__1, (char *)&nx[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&y2[0], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__2 = n1;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    dy = y2[i__ - 1] - y2[i__ - 2];
	    s_wsfe(&io___125);
	    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    } else if (n3 == 13) {
	s_wsfe(&io___126);
	do_fio(&c__1, (char *)&nx[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&y3[0], (ftnlen)sizeof(doublereal));
	e_wsfe();
	i__2 = n1;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    dy = y3[i__ - 1] - y3[i__ - 2];
	    s_wsfe(&io___127);
	    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }

    goto L990;
/* ---- */
L230:
    s_wsfe(&io___128);
    e_wsfe();
    if (n3 == 21) {
	s_wsfe(&io___129);
	do_fio(&c__1, (char *)&nx[0], (ftnlen)sizeof(integer));
	e_wsfe();
	i__2 = n1;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    dy = y1[i__ - 1] - y1[i__ - 2];
	    s_wsfe(&io___130);
	    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    } else if (n3 == 22) {
	s_wsfe(&io___131);
	do_fio(&c__1, (char *)&nx[0], (ftnlen)sizeof(integer));
	e_wsfe();
	i__2 = n1;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    dy = y2[i__ - 1] - y2[i__ - 2];
	    s_wsfe(&io___132);
	    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    } else if (n3 == 23) {
	s_wsfe(&io___133);
	do_fio(&c__1, (char *)&nx[0], (ftnlen)sizeof(integer));
	e_wsfe();
	i__2 = n1;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    dy = y3[i__ - 1] - y3[i__ - 2];
	    s_wsfe(&io___134);
	    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&dy, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }

    goto L990;
/* ------------------------------------------------------------------------- */
/* For purpose 10.) */
L240:
    s_rsle(&io___135);
    do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
    e_rsle();
    if (n2 == 0) {
	s_wsfe(&io___136);
	do_fio(&c__1, (char *)&n1, (ftnlen)sizeof(integer));
	e_wsfe();
	i__2 = n1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    s_rsle(&io___137);
	    do_lio(&c__5, &c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	    y2[i__ - 1] = 0.;
	    s_wsfe(&io___138);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    } else {
	i__2 = n1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    s_rsle(&io___139);
	    do_lio(&c__5, &c__1, (char *)&xx[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&yy[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	}
	s_rsle(&io___140);
	do_lio(&c__3, &c__1, (char *)&nn, (ftnlen)sizeof(integer));
	e_rsle();
	i__2 = nn;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    s_rsle(&io___142);
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	}
	n3 = nn - n1;
	i__2 = n3;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    x[i__ - 1] = y1[i__ - 1];
	    y3[i__ - 1] = y2[i__ - 1];
	}
	i__2 = n1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    j = i__ + n3;
	    x[j - 1] = xx[i__ - 1];
	    y3[j - 1] = yy[i__ - 1];
	}
	s_wsfe(&io___144);
	do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
	e_wsfe();
	i__2 = nn;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    s_wsfe(&io___145);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
    goto L990;
/* ------------------------------------------------------------------------- */
/* For purpose 11.) */

L260:
    s_rsfe(&io___146);
    do_fio(&c__72, (char *)&title[0], (ftnlen)sizeof(doublereal));
    e_rsfe();
    s_rsfe(&io___147);
    do_fio(&c__72, (char *)&title[0], (ftnlen)sizeof(doublereal));
    e_rsfe();
    s_rsle(&io___148);
    do_lio(&c__3, &c__1, (char *)&nrg, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___150);
    do_lio(&c__3, &c__1, (char *)&nrg, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsfe(&io___151);
    do_fio(&c__72, (char *)&titl0[0], (ftnlen)sizeof(doublereal));
    e_rsfe();
    s_rsle(&io___153);
    do_lio(&c__3, &c__1, (char *)&labst, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&labmap, (ftnlen)sizeof(integer));
    e_rsle();
    labda = labst - labmap;
    s_wsfe(&io___157);
    do_fio(&c__72, (char *)&titl0[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___158);
    do_fio(&c__1, (char *)&nrg, (ftnlen)sizeof(integer));
    e_wsfe();
    i__2 = nrg;
    for (i__ = 1; i__ <= i__2; ++i__) {
	s_rsfe(&io___159);
	do_fio(&c__1, (char *)&ri[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rf[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rs[i__ - 1], (ftnlen)sizeof(doublereal));
	e_rsfe();
	s_rsfe(&io___163);
	do_fio(&c__1, (char *)&ri[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rf[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rs[i__ - 1], (ftnlen)sizeof(doublereal));
	e_rsfe();
	s_wsfe(&io___164);
	do_fio(&c__1, (char *)&ri[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rf[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rs[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    s_rsle(&io___165);
    do_lio(&c__3, &c__1, (char *)&natom, (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___167);
    do_lio(&c__3, &c__1, (char *)&natom, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsfe(&io___168);
    do_fio(&c__1, (char *)&natom, (ftnlen)sizeof(integer));
    e_wsfe();
    i__2 = natom;
    for (i__ = 1; i__ <= i__2; ++i__) {
	s_rsle(&io___169);
	do_lio(&c__5, &c__1, (char *)&c1[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	do_lio(&c__5, &c__1, (char *)&c2[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	e_rsle();
	s_rsle(&io___172);
	do_lio(&c__5, &c__1, (char *)&c1[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	do_lio(&c__5, &c__1, (char *)&c2[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	e_rsle();
	s_wsfe(&io___173);
	do_fio(&c__1, (char *)&c1[i__ - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&c2[i__ - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

    i__2 = labmap;
    for (k = 1; k <= i__2; ++k) {
	s_rsle(&io___174);
	do_lio(&c__3, &c__1, (char *)&lab[k - 1], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&ndata, (ftnlen)sizeof(integer));
	e_rsle();
	s_rsle(&io___177);
	do_lio(&c__3, &c__1, (char *)&lab[k - 1], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&ndata, (ftnlen)sizeof(integer));
	e_rsle();
	s_wsfe(&io___178);
	do_fio(&c__1, (char *)&lab[k - 1], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ndata, (ftnlen)sizeof(integer));
	e_wsfe();
	i__1 = ndata;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsfe(&io___179);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_rsfe();
	    s_rsfe(&io___180);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_rsfe();
	    y3[i__ - 1] = y1[i__ - 1] + y2[i__ - 1];
	    s_wsfe(&io___181);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
/* --- */
    i__2 = labda;
    for (k = 1; k <= i__2; ++k) {
	s_rsle(&io___182);
	do_lio(&c__3, &c__1, (char *)&lab[k + labmap - 1], (ftnlen)sizeof(
		integer));
	do_lio(&c__3, &c__1, (char *)&ndata, (ftnlen)sizeof(integer));
	e_rsle();
	s_wsfe(&io___183);
	do_fio(&c__1, (char *)&lab[k + labmap - 1], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ndata, (ftnlen)sizeof(integer));
	e_wsfe();
	i__1 = ndata;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_rsfe(&io___184);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_rsfe();
	    s_wsfe(&io___185);
	    do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }

    goto L990;
/* ------------------------------------------------------------------------- */
/* For purpose 12.) */
L280:
    s_rsle(&io___186);
    do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&a1, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&a2, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_wsfe(&io___189);
    do_fio(&c__1, (char *)&n2, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&n2, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&a1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&a2, (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__2 = n1;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*           read(5,*)   x(i), y1(i) */
	s_rsle(&io___190);
	do_lio(&c__3, &c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	e_rsle();
	if (nx[i__ - 1] <= n2) {
	    s_wsfe(&io___191);
	    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	    d__1 = y1[i__ - 1] - a1;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
/*           write(6,860)  x(i), y1(i)-a1 */
	} else {
	    s_wsfe(&io___192);
	    do_fio(&c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer));
	    d__1 = y1[i__ - 1] - a2;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
/*           write(6,860)  x(i), y1(i)-a2 */
	}
    }

    goto L990;
/* ------------------------------------------------------------------------- */
/* For purpose 13.) */
L300:
    s_rsle(&io___193);
    do_lio(&c__3, &c__1, (char *)&n1, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&n2, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&a, (ftnlen)sizeof(doublereal));
    e_rsle();
    i__2 = n1;
    for (i__ = 1; i__ <= i__2; ++i__) {
	if (n2 < 2) {
	    s_rsle(&io___194);
	    do_lio(&c__3, &c__1, (char *)&nx[i__ - 1], (ftnlen)sizeof(integer)
		    );
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	} else if (n2 > 1 && n2 <= 3) {
	    s_rsle(&io___195);
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
	    y3[i__ - 1] -= a;
	    if (n2 == 3) {
		y1[i__ - 1] /= anstoao;
	    }
	}
	if (n2 == 1) {
	    y1[i__ - 1] /= anstoao;
	    y2[i__ - 1] /= anstoao;
	}
	if (y1[i__ - 1] != y1[i__ - 2]) {
	    s_wsfe(&io___196);
	    do_fio(&c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
    if (n2 < 2) {
	for (i__ = n1; i__ >= 1; --i__) {
	    if (y2[i__ - 1] != y2[i__ - 2]) {
		s_wsfe(&io___197);
		do_fio(&c__1, (char *)&y2[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		do_fio(&c__1, (char *)&y3[i__ - 1], (ftnlen)sizeof(doublereal)
			);
		e_wsfe();
	    }
	}
    }
/* ------------------------------------------------------------------------- */
/* 630 format(f10.5,2x,f22.16) */


/*    #4x,' x(i)       y(i) ',/) */
/* 860 format(3x,f8.3,3x,f11.2) */
/* ------------------------------------------------------------------------- */
L990:
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */

