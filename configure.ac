AC_INIT([greed], [1.4.1], [richard+greed@sfere.greenend.org.uk])
AC_CONFIG_SRCDIR([greed.el])

AC_ARG_WITH([emacs],
            [AS_HELP_STRING([--with-emacs=EMACS-PROGRAM],
                            [specify Emacs implementation to use])],
            [EMACS="$withval"])

AC_MSG_CHECKING([for an Emacs implementation])
if test "x$EMACS" != x; then
  :
else
  EMACS="none found"
  for e in emacs xemacs; do 
    if type $e >/dev/null 2>&1; then
      EMACS="$e"
      break
    fi
  done
fi
AC_MSG_RESULT([$EMACS])
AC_SUBST([EMACS])

AC_MSG_CHECKING([where to put lisp files])
AC_ARG_WITH([lispdir],
            [AS_HELP_STRING([--with-lispdir=LISP-DIRECTORY],
                            [where to install lisp files])],
            [lispdir="$withval"],
            [lispdir=${lispdir:-/usr/local/share/$EMACS/site-lisp}])
AC_MSG_RESULT([$lispdir])
AC_SUBST([lispdir])

AC_MSG_CHECKING([where to put info files])
AC_ARG_WITH([infodir],
            [AS_HELP_STRING([--with-infodir=INFO-DIRECTORY],
                            [where to install info files])],
            [infodir="$withval"],
            [infodir=/usr/local/share/info])
AC_MSG_RESULT([$infodir])
AC_SUBST([infodir])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
