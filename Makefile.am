## @start 1
## Makefile.am -- Process this file with automake to produce Makefile.in
## @end 1
## Copyright (C) 2004 Ning Wang
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

AUTOMAKE_OPTIONS        = foreign

AUX_DIST                = $(ac_aux_dir)/config.guess \
                        $(ac_aux_dir)/install-sh \
                        $(ac_aux_dir)/missing

EXTRA_DIST              = bootstrap include

SUBDIRS                 = . src/ml4 src/utils src/frontc src/const_folding\
	 		src/cmm_utils src/cmm src/c2cmm src/bgrab

MAINTAINERCLEANFILES    = Makefile.in Makefile.conf aclocal.m4 configure config-h.in
			$(AUX_DIST)
CLEANFILES              = -r build
ACLOCAL                 = aclocal -I $(ac_aux_dir)

docdir                  = $(top_builddir)/doc

#(cd $(distdir) && mkdir $(ac_aux_dir))
dist-hook:
	for file in $(AUX_DIST) $(AUX_DIST_EXTRA); do \
		cp $$file $(distdir)/$$file; \
	done
