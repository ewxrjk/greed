AC_INIT([greed], [1.4.0], [richard+greed@sfere.greenend.org.uk])
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

AC_CONFIG_FILES([Makefile])
AC_OUTPUT