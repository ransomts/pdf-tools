#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.67])
AC_INIT([epdfinfo], 1.0, [politza@fh-trier.de])
AM_INIT_AUTOMAKE([-Wall -Wno-override foreign silent-rules])
AC_CONFIG_SRCDIR([epdfinfo.h])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_RANLIB
AM_PROG_AR

# Checks for libraries.
HAVE_POPPLER_FIND_OPTS="no   (requires poppler-glib >= 0.22)"
HAVE_POPPLER_ANNOT_WRITE="no   (requires poppler-glib >= 0.19.4)"
HAVE_POPPLER_ANNOT_MARKUP="no (requires poppler-glib >= 0.26)"
HAVE_POPPLER_ANNOT_SHAPES="no (requires poppler-glib >= 0.26)"
HAVE_POPPLER_ANNOT_STAMP="no (requires poppler-glib >= 22.07)"
HAVE_POPPLER_ANNOT_FREE_TEXT="no (requires poppler-glib >= 24.12)"
HAVE_POPPLER_ANNOT_INK="no (requires poppler-glib >= 25.06)"

PKG_CHECK_MODULES([png], [libpng])
PKG_CHECK_MODULES([glib], [glib-2.0])
PKG_CHECK_MODULES([poppler], [poppler])
PKG_CHECK_MODULES([poppler_glib], [poppler-glib >= 0.16.0])
PKG_CHECK_EXISTS([poppler-glib >= 0.19.4], [HAVE_POPPLER_ANNOT_WRITE=yes])
PKG_CHECK_EXISTS([poppler-glib >= 0.22], [HAVE_POPPLER_FIND_OPTS=yes])
PKG_CHECK_EXISTS([poppler-glib >= 0.26], [HAVE_POPPLER_ANNOT_MARKUP=yes])
PKG_CHECK_EXISTS([poppler-glib >= 0.26], [HAVE_POPPLER_ANNOT_SHAPES=yes])
PKG_CHECK_EXISTS([poppler-glib >= 22.07], [HAVE_POPPLER_ANNOT_STAMP=yes])
PKG_CHECK_EXISTS([poppler-glib >= 24.12], [HAVE_POPPLER_ANNOT_FREE_TEXT=yes])
PKG_CHECK_EXISTS([poppler-glib >= 25.06], [HAVE_POPPLER_ANNOT_INK=yes])
PKG_CHECK_MODULES([zlib], [zlib])

AC_COMPILE_IFELSE(
   [AC_LANG_PROGRAM([[
     #ifndef _WIN32
       error
     #endif
   ]])], [have_w32=true], [have_w32=false])
AM_CONDITIONAL(HAVE_W32, [test "$have_w32" = true])

if test "$have_w32" = true; then
  if test "$MSYSTEM" = MINGW32 -o "$MSYSTEM" = MINGW64 -o "$MSYSTEM" = UCRT64; then
  # glib won't work properly on msys2 without it.
    CFLAGS="-D__USE_MINGW_ANSI_STDIO=1 $CFLAGS"
  fi
fi

# Setup compile time features.
if test "$HAVE_POPPLER_FIND_OPTS" = yes; then
  AC_DEFINE([HAVE_POPPLER_FIND_OPTS],1,
    [Define to 1 to enable case sensitive searching (requires poppler-glib >= 0.22).])
fi

if test "$HAVE_POPPLER_ANNOT_WRITE" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_WRITE],1,
    [Define to 1 to enable writing of annotations (requires poppler-glib >= 0.19.4).])
fi

if test "$HAVE_POPPLER_ANNOT_MARKUP" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_MARKUP],1,
    [Define to 1 to enable adding of markup annotations (requires poppler-glib >= 0.26).])
fi

if test "$HAVE_POPPLER_ANNOT_SHAPES" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_SHAPES],1,
    [Define to 1 to enable adding of square, circle and line annotations (requires poppler-glib >= 0.26).])
fi

if test "$HAVE_POPPLER_ANNOT_STAMP" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_STAMP],1,
    [Define to 1 to enable adding of stamp annotations (requires poppler-glib >= 22.07).])
fi

if test "$HAVE_POPPLER_ANNOT_FREE_TEXT" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_FREE_TEXT],1,
    [Define to 1 to enable adding of free text annotations (requires poppler-glib >= 24.12).])
fi

if test "$HAVE_POPPLER_ANNOT_INK" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_INK],1,
    [Define to 1 to enable adding of ink annotations (requires poppler-glib >= 25.06).])
fi

AC_CANONICAL_HOST
# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h strings.h err.h])

AC_MSG_CHECKING([for error.h])
SAVED_CFLAGS=$CFLAGS
CFLAGS="$poppler_CFLAGS $poppler_glib_CFLAGS"
AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
        #include <error.h>
        ],[error (0, 0, "");])],
        [AC_DEFINE([HAVE_ERROR_H],1, [Define to 1 if error.h is usable.])
        AC_MSG_RESULT([yes])],
        AC_MSG_RESULT([no]))
AC_LANG_POP([C])
CFLAGS=$SAVED_CFLAGS

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_CHECK_TYPES([ptrdiff_t])
AC_C_BIGENDIAN

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_STRTOD
AC_CHECK_FUNCS([strcspn strtol getline _tempnam])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

echo
echo "Is case-sensitive searching enabled ?     ${HAVE_POPPLER_FIND_OPTS}"
echo "Is modifying text annotations enabled ?   ${HAVE_POPPLER_ANNOT_WRITE}"
echo "Is modifying markup annotations enabled ? ${HAVE_POPPLER_ANNOT_MARKUP}"
echo "Is adding shape annotations enabled ?     ${HAVE_POPPLER_ANNOT_SHAPES}"
echo "Is adding stamp annotations enabled ?     ${HAVE_POPPLER_ANNOT_STAMP}"
echo "Is adding free text annotations enabled ? ${HAVE_POPPLER_ANNOT_FREE_TEXT}"
echo "Is adding ink annotations enabled ?       ${HAVE_POPPLER_ANNOT_INK}"
echo
