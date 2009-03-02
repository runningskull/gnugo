/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3 or          *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


#ifndef GNU_GO_WINSOCKET_H
#define GNU_GO_WINSOCKET_H


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#define USE_WINDOWS_SOCKET_CLUDGE		\
  ((defined(_WIN32) || defined(_WIN32_WCE))	\
   && defined(ENABLE_SOCKET_SUPPORT))


#if USE_WINDOWS_SOCKET_CLUDGE


#include <stdarg.h>
#include <stdio.h>


#ifndef WINSOCKET_H_INTERNAL_INCLUSION

/* (sic.) Teh cludge. */

/* At least in some versions of `stdio.h' on Windows, feof() is a
 * macro, not a function.
 */
#ifdef feof
#undef feof
#endif

#define setbuf		winsocket_setbuf
#define fflush		winsocket_fflush
#define feof		winsocket_feof
#define fclose		winsocket_fclose

#define fread		winsocket_fread
#define fgets		winsocket_fgets

#define fwrite		winsocket_fwrite
#define fputc		winsocket_fputc
#define fputs		winsocket_fputs
#define fprintf		winsocket_fprintf
#define vfprintf	winsocket_vfprintf

#endif	/* WINSOCKET_H_INTERNAL_INCLUSION */


void		winsocket_activate(int _socket_handle);

void		winsocket_setbuf(FILE *file, char *buffer);
int		winsocket_fflush(FILE *file);
int		winsocket_feof(FILE *file);
int		winsocket_fclose(FILE *file);

size_t		winsocket_fread(void *buffer,
				size_t size, size_t num_items, FILE *file);
char *		winsocket_fgets(char *buffer, int size, FILE *file);

size_t		winsocket_fwrite(const void *buffer,
				 size_t size, size_t num_items, FILE *file);
int		winsocket_fputc(int character, FILE *file);
int		winsocket_fputs(const char *string, FILE *file);
int		winsocket_fprintf(FILE *file, const char *format_string, ...);
int		winsocket_vfprintf(FILE *file, const char *format_string,
				   va_list arguments);


#endif	/* USE_WINDOWS_SOCKET_CLUDGE */


#endif	/* GNU_GO_WINSOCKET_H */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
