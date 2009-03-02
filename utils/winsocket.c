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

/* Workaround M$ Windows C library deficiency: it cannot handle
 * sockets as stdio streams.  So we do this ourselves, more or less.
 *
 * Call winsocket_activate(...) with socket handle to use.  After this
 * fake stream NULL will work over the socket, while other streams
 * will (hopefully) keep working as usual.
 */

#define WINSOCKET_H_INTERNAL_INCLUSION

#include "winsocket.h"


#if USE_WINDOWS_SOCKET_CLUDGE


#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include <winsock.h>


static int socket_handle      = 0;
static int socket_end_of_file = 0;


void
winsocket_activate(int _socket_handle)
{
  assert(socket_handle == 0);
  socket_handle = _socket_handle;
}


/* Miscellaneous functions. */

void
winsocket_setbuf(FILE *file, char *buffer)
{
  if (file != NULL)
    setbuf(file, buffer);
  else
    assert(socket_handle != 0);
}


int
winsocket_fflush(FILE *file)
{
  if (file != NULL)
    return fflush(file);
  else {
    assert(socket_handle != 0);
    return 0;
  }
}


int
winsocket_feof(FILE *file)
{
  if (file != NULL)
    return feof(file);
  else {
    assert(socket_handle != 0);
    return socket_end_of_file;
  }
}


int
winsocket_fclose(FILE *file)
{
  if (file != NULL)
    return fclose(file);
  else {
    assert(socket_handle != 0);
    return 0;
  }
}


/* Input functions. */

size_t
winsocket_fread(void *buffer, size_t size, size_t num_items, FILE *file)
{
  if (file != NULL)
    return fread(buffer, size, num_items, file);
  else {
    assert(socket_handle != 0);
    if (recv(socket_handle, (char *) buffer, size * num_items, 0)
	== size * num_items)
      return num_items;
    else {
      socket_end_of_file = 1;
      return EOF;
    }
  }
}


char *
winsocket_fgets(char *buffer, int size, FILE *file)
{
  if (file != NULL)
    return fgets(buffer, size, file);
  else {
    /* FIXME: Optimize if reading char-by-char is too slow. */
    int stored_length;

    for (stored_length = 0; stored_length < size - 1; stored_length) {
      if (recv(socket_handle, buffer + stored_length, 1, 0) != 1) {
	socket_end_of_file = 1;
	break;
      }

      if (buffer[stored_length++] == '\n')
	break;
    }

    if (stored_length == 0)
      return NULL;

    buffer[stored_length + 1] = 0;
    return buffer;
  }
}


/* Output functions. */

size_t
winsocket_fwrite(const void *buffer, size_t size, size_t num_items,
		 FILE *file)
{
  if (file != NULL)
    return fwrite(buffer, size, num_items, file);
  else {
    assert(socket_handle != 0);
    return ((send(socket_handle, (const char *) buffer, size * num_items, 0)
	     == size * num_items)
	    ? num_items : EOF);
  }
}


int
winsocket_fputc(int character, FILE *file)
{
  if (file != NULL)
    return fputc(character, file);
  else {
    assert(socket_handle != 0);
    return (send(socket_handle, (const char *) &character, 1, 0) == 1
	    ? character : EOF);
  }
}


int
winsocket_fputs(const char *string, FILE *file)
{
  if (file != NULL)
    return fputs(string, file);
  else {
    int length = strlen(string);

    assert(socket_handle != 0);
    return send(socket_handle, string, length, 0) == length ? length : EOF;
  }
}


int
winsocket_fprintf(FILE *file, const char *format_string, ...)
{
  va_list arguments;
  int result;

  va_start(arguments, format_string);
  result = winsocket_vfprintf(file, format_string, arguments);
  va_end(arguments);

  return result;
}


int
winsocket_vfprintf(FILE *file, const char *format_string, va_list arguments)
{
  if (file != NULL)
    return vfprintf(file, format_string, arguments);
  else {
    char buffer[0x1000];
    int length = _vsnprintf(buffer, sizeof buffer, format_string, arguments);

    assert(socket_handle != 0);
    return send(socket_handle, buffer, length, 0) == length ? length : -1;
  }
}


#endif	/* USE_WINDOWS_SOCKET_CLUDGE */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
