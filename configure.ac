AC_INIT([greed], [1.4.DEV], [richard+greed@sfere.greenend.org.uk])
AC_CONFIG_SRCDIR([greed.el])

# Let the user choose their preferred version of Emacs via a command-line
# option.
AC_ARG_WITH([emacs],
            [AS_HELP_STRING([--with-emacs=EMACS-PROGRAM],
                            [specify Emacs implementation to use])],
            [EMACS="$withval"])

# Resolve to an absolute path so it's absolutely clear (from configure output)
# which version is used.
case "$EMACS" in
"" )
  # No emacs specified
  AC_PATH_PROG([EMACS],[emacs xemacs])
  ;;
/* )
  # Got an absolute path already
  ;;
* )
  # Got an emacs but need to canonicalize
  AC_PATH_PROG([EMACS],[$EMACS])
  ;;
esac

if test "x$EMACS" = x; then
  AC_MSG_ERROR([no Emacs found])
fi

AC_SUBST([EMACS])

AC_CACHE_CHECK([whether $EMACS has multilingual support],
               [greed_cv_emacs_mule],[
  # XEmacs fails an assert if this test fails and the shell blurts
  # an abort message all over the configure output.  If this bothers
  # you, get Emacs fixed.
  if "$EMACS" --batch -f list-coding-systems </dev/null >/dev/null 2>&1; then
    greed_cv_emacs_mule=yes
  else
    greed_cv_emacs_mule=no
  fi
])

if test $greed_cv_emacs_mule != yes; then
  AC_MSG_ERROR([Greed requires multilingual support])
fi

# Figure out Emacs type
AC_CACHE_CHECK([what kind of Emacs is $EMACS],
               [greed_cv_emacstype],[
  case "$EMACS" in
  *xemacs* )
    greed_cv_emacstype="XEmacs"
    ;;
  */emacs* )
    greed_cv_emacstype="GNU emacs"
    ;;
  * )
    greed_cv_emacstype="unknown"
    ;;
  esac
])
if test "$greed_cv_emacstype" = XEmacs; then
  emacstype=xemacs
else
  emacstype=emacs
fi

# Figure out Emacs root directory
AC_CACHE_CHECK([inferred Emacs install root],[greed_cv_emacsroot],[
  greed_cv_emacsroot=`echo "$EMACS" | sed 's/\/bin\/.*//g'`
  case $greed_cv_emacsroot in
  *bin* )
    greed_cv_emacsroot=unknown
    ;;
  esac
])

# /usr is not suitable for installing extension packages!
case $greed_cv_emacsroot in
/usr | /sw | unknown )
  root_suitable=false
  ;;
* )
  root_suitable=true
  ;;
esac

AC_MSG_CHECKING([where to put lisp files])
AC_ARG_WITH([lispdir],
            [AS_HELP_STRING([--with-lispdir=LISP-DIRECTORY],
                            [where to install lisp files])],
            [lispdir="$withval"],[
  if $root_suitable \
     && test -d "$greed_cv_emacsroot/share/$emacstype/site-lisp"; then
    # Should work in upstream GNU Emacs
    lispdir="$greed_cv_emacsroot/share/$emacstype/site-lisp"
  elif $root_suitable \
     && test -d "$greed_cv_emacsroot/lib/$emacstype/site-lisp"; then
    lispdir="$greed_cv_emacsroot/lib/$emacstype/site-lisp"
  elif $root_suitable \
     && test -d "$greed_cv_emacsroot/share/emacs/site-lisp"; then
    lispdir="$greed_cv_emacsroot/share/emacs/site-lisp"
  elif $root_suitable  \
     && test -d "$greed_cv_emacsroot/lib/emacs/site-lisp"; then
    lispdir="$greed_cv_emacsroot/lib/emacs/site-lisp"
  elif test -d "`eval echo "$datarootdir"`/$emacstype/site-lisp"; then
    lispdir="\${datarootdir}/$emacstype/site-lisp"
  else
    # Should work in Debian's GNU Emacs and XEmacs
    lispdir="\${datarootdir}/emacs/site-lisp"
  fi
])
AC_MSG_RESULT([$lispdir])
AC_SUBST([lispdir])

AC_MSG_CHECKING([where to put info files])
AC_ARG_WITH([infodir],
            [AS_HELP_STRING([--with-infodir=INFO-DIRECTORY],
                            [where to install info files])],
            [infodir="$withval"], [
  if $root_suitable \
     && test -d "$greed_cv_emacsroot/share/info"; then
    # Should work in upstream GNU Emacs
    infodir="$greed_cv_emacsroot/share/info"
  elif $root_suitable \
     && test -d "$greed_cv_emacsroot/info"; then
    # ...in case there's any /usr/share-is-evil holdouts
    infodir="$greed_cv_emacsroot/info"
  else
    # Should work in Debian's GNU Emacs and XEmacs
    infodir="\${datarootdir}/info"
  fi
])
AC_MSG_RESULT([$infodir])
AC_SUBST([infodir])

AC_ARG_WITH([dot-emacs],
            [AS_HELP_STRING([--with-dot-emacs],
                            [Attempt to update your ~/.emacs on install])],
            [UPDATE_DOT_EMACS=true],
            [UPDATE_DOT_EMACS=false])
AC_SUBST([UPDATE_DOT_EMACS])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
