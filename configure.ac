# https://www.unwoundstack.com/blog/guile-and-autoconf.html
# https://erikedrosa.com/2017/10/29/guile-projects-with-autotools.html
# -*- Autoconf -*-
#
# guile-sdl2 --- FFI bindings for SDL2
# Copyright © 2015, 2020 David Thompson <davet@gnu.org>
#
# This file is part of guile-sdl2.
#
# Guile-sdl2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# Guile-sdl2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with guile-sdl2.  If not, see
# <http://www.gnu.org/licenses/>.

AC_INIT(guile-libnotify, 0.1.0)
AC_CONFIG_SRCDIR(notify)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_PATH_PROG([GUILE], [guile])
AC_CONFIG_FILES([Makefile notify/config.scm])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

# libnotify
PKG_CHECK_MODULES([LIBNOTIFY], [libnotify])
PKG_CHECK_VAR([LIBNOTIFY_LIBDIR], [libnotify], [libdir])
AC_MSG_CHECKING([LIBNOTIFY library path])
AS_IF([test "x$LIBNOTIFY_LIBDIR" = "x"], [
  AC_MSG_FAILURE([unable to find LIBNOTIFY library directory])
], [
  AC_MSG_RESULT([$LIBNOTIFY_LIBDIR])
])
AC_SUBST([LIBNOTIFY_LIBDIR])

# glib
PKG_CHECK_MODULES([GLIB], [glib-2.0])
PKG_CHECK_VAR([GLIB_LIBDIR], [glib-2.0], [libdir])
AC_MSG_CHECKING([GLIB library path])
AS_IF([test "x$GLIB_LIBDIR" = "x"], [
  AC_MSG_FAILURE([unable to find GLIB library directory])
], [
  AC_MSG_RESULT([$GLIB_LIBDIR])
])
AC_SUBST([GLIB_LIBDIR])

AC_OUTPUT

