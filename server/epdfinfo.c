/* Copyright (C) 2013, 2014  Andreas Politz
 *
 * Author: Andreas Politz <politza@fh-trier.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include <config.h>

#include <assert.h>
#ifdef HAVE_ERR_H
#  include <err.h>
#endif
#ifdef HAVE_ERROR_H
#  include <error.h>
#endif
#include <glib.h>
#include <poppler.h>
#include <cairo.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <png.h>
#include <math.h>
#include <regex.h>
#include "synctex_parser.h"
#include "epdfinfo.h"


/* ================================================================== *
 * Helper Functions
 * ================================================================== */

#ifndef HAVE_ERR_H
/**
 * Print error message and quit.
 *
 * @param eval Return code
 * @param fmt Formatting string
 */
static void
err(int eval, const char *fmt, ...)
{
  va_list args;

  fprintf (stderr, "epdfinfo: ");
  if (fmt != NULL)
    {
      va_start (args, fmt);
      vfprintf (stderr, fmt, args);
      va_end (args);
      fprintf (stderr, ": %s\n", strerror(errno));
    }
  else
    {
      fprintf (stderr, "\n");
    }

  fflush (stderr);
  exit (eval);
}
#endif

#ifndef HAVE_GETLINE
/**
 * Read one line from a file.
 *
 * @param lineptr Pointer to malloc() allocated buffer
 * @param n Pointer to size of buffer
 * @param stream File pointer to read from
 */
static ssize_t
getline(char **lineptr, size_t *n, FILE *stream)
{
  size_t len = 0;
  int ch;

  if ((lineptr == NULL) || (n == NULL))
    {
      errno = EINVAL;
      return -1;
    }

  if (*lineptr == NULL)
    {
      *lineptr = malloc (128);
      *n = 128;
    }

  while ((ch = fgetc (stream)) != EOF)
    {
      (*lineptr)[len] = ch;

      if (++len >= *n)
        {
          *n += 128;
          *lineptr = realloc (*lineptr, *n);
        }

      if (ch == '\n')
        break;
    }
  (*lineptr)[len] = '\0';

  if (!len)
    {
      len = -1;
    }

  return len;
}
#endif

/**
 * Free a list of command arguments.
 *
 * @param args An array of command arguments.
 * @param n The length of the array.
 */
static void
free_command_args (command_arg_t *args, size_t n)
{
  if (! args)
    return;

  g_free (args);
}

/**
 * Free resources held by document.
 *
 * @param doc The document to be freed.
 */
static void
free_document (document_t *doc)
{
  if (! doc)
    return;

  g_free (doc->filename);
  g_free (doc->passwd);
  if (doc->annotations.pages)
    {
      int npages = poppler_document_get_n_pages (doc->pdf);
      int i;
      for (i = 0; i < npages; ++i)
        {
          GList *item;
          GList *annots  = doc->annotations.pages[i];
          for (item = annots; item; item = item->next)
            {
              annotation_t *a = (annotation_t*) item->data;
              poppler_annot_mapping_free(a->amap);
              g_free (a->key);
              g_free (a->image_file);
              g_free (a);
            }
          g_list_free (annots);
        }
      g_hash_table_destroy (doc->annotations.keys);
      g_free (doc->annotations.pages);
    }
  g_object_unref (doc->pdf);
  g_free (doc);
}

/**
 * Parse a list of whitespace separated double values.
 *
 * @param str The input string.
 * @param values[out] Values are put here.
 * @param nvalues How many values to parse.
 *
 * @return TRUE, if str contained exactly nvalues, else FALSE.
 */
static gboolean
parse_double_list (const char *str, gdouble *values, size_t nvalues)
{
  char *end;
  int i;

  if (! str)
    return FALSE;

  errno = 0;
  for (i = 0; i < nvalues; ++i)
    {
      gdouble n = g_ascii_strtod (str, &end);

      if (str == end || errno)
        return FALSE;

      values[i] = n;
      str = end;
    }

  if (*end)
    return FALSE;

  return TRUE;
}

static gboolean
parse_rectangle (const char *str, PopplerRectangle *r)
{
  gdouble values[4];

  if (! r)
    return FALSE;

  if (! parse_double_list (str, values, 4))
    return FALSE;

  r->x1 = values[0];
  r->y1 = values[1];
  r->x2 = values[2];
  r->y2 = values[3];

  return TRUE;
}

static gboolean
parse_edges_or_position (const char *str, PopplerRectangle *r)
{
  return (parse_rectangle (str, r)
          && r->x1 >= 0 && r->x1 <= 1
          && r->x2 <= 1
          && r->y1 >= 0 && r->y1 <= 1
          && r->y2 <= 1);
}

static gboolean
parse_edges (const char *str, PopplerRectangle *r)
{
  return (parse_rectangle (str, r)
          && r->x1 >= 0 && r->x1 <= 1
          && r->x2 >= 0 && r->x2 <= 1
          && r->y1 >= 0 && r->y1 <= 1
          && r->y2 >= 0 && r->y2 <= 1);
}

/**
 * Print a string properly escaped for a response.
 *
 * @param str The string to be printed.
 * @param suffix_char Append a newline if NEWLINE, a colon if COLON.
 */
static void
print_response_string (const char *str, enum suffix_char suffix)
{
  if (str)
    {
      while (*str)
        {
          switch (*str)
            {
            case '\n':
              printf ("\\n");
              break;
            case '\\':
              printf ("\\\\");
              break;
            case ':':
              printf ("\\:");
              break;
            default:
              putchar (*str);
            }
          ++str;
        }
    }

  switch (suffix)
    {
    case NEWLINE:
      putchar ('\n');
      break;
    case COLON:
      putchar (':');
      break;
    default: ;
    }
}


/**
 * Print a formatted error response.
 *
 * @param fmt The printf-like format string.
 */
static void
printf_error_response (const char *fmt, ...)
{
  va_list va;
  puts ("ERR");
  va_start (va, fmt);
  vprintf (fmt, va);
  va_end (va);
  puts ("\n.");
  fflush (stdout);
}

/**
 * Remove one trailing newline character.  Does nothing, if str does
 * not end with a newline.
 *
 * @param str The string.
 *
 * @return str with trailing newline removed.
 */
static char*
strchomp (char *str)
{
  size_t length;

  if (! str)
    return str;

  length = strlen (str);
  if (str[length - 1] == '\n')
    str[length - 1] = '\0';

  return str;
}

/**
 * Create a new, temporary file and returns its name.
 *
 * @return The filename.
 */
static char*
mktempfile()
{
#if defined (HAVE__TEMPNAM)
  char *filename = NULL;
  int tries = 3;
  while (! filename && tries-- > 0)
    {

      filename =  _tempnam(NULL, "epdfinfo");
      if (filename)
        {
          int fd = open(filename, O_CREAT | O_EXCL | O_RDONLY, S_IRUSR | S_IWUSR);
          if (fd > 0)
            close (fd);
          else
            {
              free (filename);
              filename = NULL;
            }
        }
    }
  if (! filename)
    fprintf (stderr, "Unable to create tempfile");
#else
  char template[] = P_tmpdir "/epdfinfoXXXXXX";
  char *filename = malloc(sizeof(template));
  memcpy(filename, template, sizeof(template));
  int fd = mkstemp(filename);
  if (fd == -1)
    {
      fprintf (stderr, "Unable to create tempfile");
      free(filename);
      filename = NULL;
    }
  else
    close(fd);
#endif
  return filename;
}

/* Holds RGB, HSL, HSV, Lab, or Lch... but note that the order in memory for HSL
 * and HSV are actually VSH and LSH. */
struct color
{
  union
  {
    double r, v, l;
  };
  union
  {
    double g, s, a;
  };
  union
  {
    double b, h;
  };
};

#define struct_color(x) (*((struct color *) x))
#define vec_color(x) ((double *) &x)

// Using values reported at https://bottosson.github.io/posts/oklab/#converting-from-linear-srgb-to-oklab
// instead of going through xyz. This ensures any whitepoint is ignored
static struct color
rgb2oklab(struct color rgb)
{
  struct color srgb;

  for (int i = 0; i < 3; i++)
    {
      double val = vec_color(rgb)[i];
      vec_color(srgb)[i] = ((val > 0.04045)
                            ? pow((val + 0.055) / 1.055, 2.4)
                            : (val / 12.92));
    }

  double l = 0.4121656120 * srgb.r + 0.5362752080 * srgb.g + 0.0514575653 * srgb.b;
  double m = 0.2118591070 * srgb.r + 0.6807189584 * srgb.g + 0.1074065790 * srgb.b;
  double s = 0.0883097947 * srgb.r + 0.2818474174 * srgb.g + 0.6302613616 * srgb.b;

  l = cbrt(l);
  m = cbrt(m);
  s = cbrt(s);

  return (struct color) {
    .l = 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s,
    .a = 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
    .b = 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
  };
}

static double clamp(double x, double low, double high)
{
  return ((x < low)
          ? low
          : ((x > high) ? high : x));
}

static struct color
oklab2rgb(struct color lab)
{
  double l = lab.l + 0.3963377774 * lab.a + 0.2158037573 * lab.b;
  double m = lab.l - 0.1055613458 * lab.a - 0.0638541728 * lab.b;
  double s = lab.l - 0.0894841775 * lab.a - 1.2914855480 * lab.b;

  l = l * l * l;
  m = m * m * m;
  s = s * s * s;

  struct color srgb = {
    .r =  4.0767245293 * l - 3.3072168827 * m + 0.2307590544 * s,
    .g = -1.2681437731 * l + 2.6093323231 * m - 0.3411344290 * s,
    .b = -0.0041119885 * l - 0.7034763098 * m + 1.7068625689 * s
  };

  struct color rgb;
  for (int i = 0; i < 3; i++)
    {
      double val = vec_color(srgb)[i];
      val = ((val > 0.0031308)
             ? (1.055 * pow(val, 1 / 2.4) - 0.055)
             : (12.92 * val));

      vec_color(rgb)[i] = clamp(val, 0.0, 1.0);
    }

  return rgb;
}

#undef struct_color
#undef vec_color

static inline gboolean color_equal(struct color a, struct color b)
{
  return (a.r == b.r && a.g == b.g && a.b == b.b);
}

static void
image_recolor (cairo_surface_t * surface, const PopplerColor * fg,
               const PopplerColor * bg, int usecolors,
               double gamma, int gammabeforeinvert)
{
  /* Performs one of two kinds of image recoloring depending on the value of usecolors:

     1 -> Bg-Fg Interpolation: maps source document colors to colors
          interpolated between the background and foreground values in
          pdf-view-midnight-colors via the lightness of the source color.  This
          discards hue information but allows you to fit your color theme
          perfectly.

     2 -> Hue-Preserving interpolation: same as above, similar to above, but
          attempts to preserve hue while still respecting the background and
          foreground colors.  This is done by matching source document white and
          black to the specified background and foreground as above, but mixes
          hue/saturation with the background color.  This preserves hue but is
          more expensive.
  */

  const unsigned int page_width = cairo_image_surface_get_width (surface);
  const unsigned int page_height = cairo_image_surface_get_height (surface);
  const int rowstride = cairo_image_surface_get_stride (surface);
  unsigned char *image = cairo_image_surface_get_data (surface);

  /* RGB weights for computing lightness. Must sum to one */
  static const double a[] = { 0.30, 0.59, 0.11 };

  const double f = 65535.;
  const struct color rgb_fg = {
    .r = fg->red / f,
    .g = fg->green / f,
    .b = fg->blue / f
  };
  const struct color rgb_bg = {
    .r = bg->red / f,
    .g = bg->green / f,
    .b = bg->blue / f
  };

  const struct color rgb_diff = {
    .r = rgb_bg.r - rgb_fg.r,
    .g = rgb_bg.g - rgb_fg.g,
    .b = rgb_bg.b - rgb_fg.b
  };

  switch (usecolors)
    {
    case 0:
      break;
    case 1:
      {
        unsigned int y;
        for (y = 0; y < page_height * rowstride; y += rowstride)
          {
            unsigned char *data = image + y;

            unsigned int x;
            for (x = 0; x < page_width; x++, data += 4)
              {
                /* Careful. data color components blue, green, red. */
                struct color rgb = {
                  .r = (double) data[2] / 255.,
                  .g = (double) data[1] / 255.,
                  .b = (double) data[0] / 255.
                };

                /* Linear interpolation between bg and fg based on the
                   perceptual lightness measure l */
                /* compute h, s, l data   */
                double l = a[0] * rgb.r + a[1] * rgb.g + a[2] * rgb.b;

                /* linear interpolation between dark and light with color
                   lightness as a parameter */
                data[2] =
                  (unsigned char) round (255. * (l * rgb_diff.r + rgb_fg.r));
                data[1] =
                  (unsigned char) round (255. * (l * rgb_diff.g + rgb_fg.g));
                data[0] =
                  (unsigned char) round (255. * (l * rgb_diff.b + rgb_fg.b));
              }
          }
      }
      break;
    case 2:
      {
        /* If using the Oklab transform, it is relatively expensive.  Precompute
           white->background and black->foreground and have a single entry cache to
           speed up computation */
        const struct color white = {.r = 1.0, .g = 1.0, .b = 1.0};
        const struct color black = {.r = 0.0, .g = 0.0, .b = 0.0};
        struct color precomputed_rgb = white;
        struct color precomputed_inv_rgb = rgb_bg;

        /* Must match the transformation of colors below. */
        struct color oklab_fg = rgb2oklab(rgb_fg);
        struct color oklab_bg = rgb2oklab(rgb_bg);

        unsigned int y;
        for (y = 0; y < page_height * rowstride; y += rowstride)
          {
            unsigned char *data = image + y;

            unsigned int x;
            for (x = 0; x < page_width; x++, data += 4)
              {
                /* Careful. data color components blue, green, red. */
                struct color rgb = {
                  .r = (double) data[2] / 255.,
                  .g = (double) data[1] / 255.,
                  .b = (double) data[0] / 255.
                };

                /* Convert to Oklab coordinates, invert perceived lightness,
                   convert back to RGB. */
                if (color_equal(white, rgb))
                  {
                    rgb = rgb_bg;
                  }
                else if (color_equal(black, rgb))
                  {
                    rgb = rgb_fg;
                  }
                else if (color_equal(precomputed_rgb, rgb))
                  {
                    rgb = precomputed_inv_rgb;
                  }
                else
                  {
                    struct color oklab = rgb2oklab(rgb);
                    precomputed_rgb = rgb;

                    /* Invert the perceived lightness, and scales it */
                    double l = oklab.l;
                    double inv_l = 1.0 - l;

                    /* Nonlinearly scale lightness */
                    if (gammabeforeinvert)
                      {
                        l = pow(l, gamma);
                        inv_l = 1.0 - l;
                      }
                    else
                      {
                        inv_l = pow(inv_l, gamma);
                        l = 1.0 - inv_l;
                      }

                    oklab.l = oklab_bg.l * l + oklab_fg.l * inv_l;

                    /* Have a and b parameters (which encode hue and saturation)
                       start at the background value and interpolate up to
                       foreground */
                    oklab.a = (oklab.a + oklab_bg.a * l + oklab_fg.a * inv_l);
                    oklab.b = (oklab.b + oklab_bg.b * l + oklab_fg.b * inv_l);

                    rgb = oklab2rgb(oklab);

                    precomputed_inv_rgb = rgb;
                  }

                data[2] = (unsigned char) round(255. * rgb.r);
                data[1] = (unsigned char) round(255. * rgb.g);
                data[0] = (unsigned char) round(255. * rgb.b);
              }
          }
      }
      break;
    default:
      internal_error ("image_recolor switch fell through");
    }
}

/**
 * Render a PDF page.
 *
 * @param pdf The PDF document.
 * @param page The page to be rendered.
 * @param width The desired width of the image.
 *
 * @return A cairo_t context encapsulating the rendered image, or
 *         NULL, if rendering failed for some reason.
 */
static cairo_surface_t*
image_render_page(PopplerDocument *pdf, PopplerPage *page,
                  int width, gboolean do_render_annotaions,
                  const render_options_t *options)
{
  cairo_t *cr = NULL;
  cairo_surface_t *surface = NULL;
  double pt_width, pt_height;
  int height;
  double scale = 1;

  if (! page || ! pdf)
    return NULL;

  if (width < 1)
    width = 1;

  poppler_page_get_size (page, &pt_width, &pt_height);
  scale = width / pt_width;
  height = (int) ((scale * pt_height) + 0.5);

  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                        width, height);

  if (cairo_surface_status (surface) != CAIRO_STATUS_SUCCESS)
    {
      fprintf (stderr, "Failed to create cairo surface\n");
      goto error;
    }

  cr = cairo_create (surface);
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS)
    {
      fprintf (stderr, "Failed to create cairo handle\n");
      goto error;
    }

  cairo_translate (cr, 0, 0);
  cairo_scale (cr, scale, scale);
  /* Render w/o annotations. */
  if (! do_render_annotaions || (options && options->printed))
    poppler_page_render_for_printing_with_options
      (page, cr, POPPLER_PRINT_DOCUMENT);
  else
    poppler_page_render (page, cr) ;
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS)
    {
      fprintf (stderr, "Failed to render page\n");
      goto error;
    }

  /* This makes the colors look right. */
  cairo_set_operator (cr, CAIRO_OPERATOR_DEST_OVER);
  cairo_set_source_rgb (cr, 1., 1., 1.);

  cairo_paint (cr);

  if (options && (options->usecolors))
    image_recolor (surface, &options->fg, &options->bg, options->usecolors,
                   options->gamma, options->gammabeforeinvert);

  cairo_destroy (cr);

  return surface;

 error:
  if (surface != NULL)
    cairo_surface_destroy (surface);
  if (cr != NULL)
    cairo_destroy (cr);
  return NULL;
}

/**
 * Write an image to a filename.
 *
 * @param cr The cairo context encapsulating the image.
 * @param filename The filename to be written to.
 * @param type The desired image type.
 *
 * @return 1 if the image was written successfully, else 0.
 */
static gboolean
image_write (cairo_surface_t *surface, const char *filename, enum image_type type)
{

  int i, j;
  unsigned char *data;
  int width, height;
  FILE *file = NULL;
  gboolean success = 0;

  if (! surface ||
      cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS)
    {
      fprintf (stderr, "Invalid cairo surface\n");
      return 0;
    }

  if (! (file = fopen (filename, "wb")))
    {
      fprintf (stderr, "Can not open file: %s\n", filename);
      return 0;
    }

  cairo_surface_flush (surface);
  width = cairo_image_surface_get_width (surface);
  height = cairo_image_surface_get_height (surface);
  data = cairo_image_surface_get_data (surface);

  switch (type)
    {
    case PPM:
      {
        unsigned char *buffer = g_malloc (width * height * 3);
        unsigned char *buffer_p = buffer;

        fprintf (file, "P6\n%d %d\n255\n", width, height);
        for (i = 0; i < width * height; ++i, data += 4, buffer_p += 3)
          ARGB_TO_RGB (buffer_p, data);
        fwrite (buffer, 1, width * height * 3, file);
        g_free (buffer);
        success = 1;
      }
      break;
    case PNG:
      {
        png_infop info_ptr = NULL;
        png_structp png_ptr = NULL;
        unsigned char *row = NULL;

        png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
        if (!png_ptr)
          goto finalize;

        info_ptr = png_create_info_struct(png_ptr);
        if (!info_ptr)
          goto finalize;

        if (setjmp(png_jmpbuf(png_ptr)))
          goto finalize;

        png_init_io (png_ptr, file);
        png_set_compression_level (png_ptr, 1);
        png_set_IHDR (png_ptr, info_ptr, width, height,
                      8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
                      PNG_COMPRESSION_TYPE_BASE,
                      PNG_FILTER_TYPE_DEFAULT);

        png_set_filter (png_ptr, PNG_FILTER_TYPE_BASE,
                        PNG_FILTER_NONE);
        png_write_info (png_ptr, info_ptr);
        row = g_malloc (3 * width);
        for (i = 0; i < height; ++i)
          {
            unsigned char *row_p = row;
            for (j = 0; j < width; ++j, data += 4, row_p += 3)
              {
                ARGB_TO_RGB (row_p, data);
              }
            png_write_row (png_ptr, row);
          }
        png_write_end (png_ptr, NULL);
        success = 1;
      finalize:
        if (png_ptr)
          png_destroy_write_struct (&png_ptr, &info_ptr);
        if (row)
          g_free (row);
        if (! success)
          fprintf (stderr, "Error writing png data\n");
      }
      break;
    default:
      internal_error ("switch fell through");
    }

  fclose (file);
  return success;
}

static void
image_write_print_response(cairo_surface_t *surface, enum image_type type)
{
  char *filename = mktempfile ();

  perror_if_not (filename, "Unable to create temporary file");
  if (image_write (surface, filename, type))
    {
      OK_BEGIN ();
      print_response_string (filename, NEWLINE);
      OK_END ();
    }
  else
    {
      printf_error_response ("Unable to write image");
    }
  free (filename);
 error:
  return;
}

static void
region_print (cairo_region_t *region, double width, double height)
{
  int i;

  for (i = 0; i < cairo_region_num_rectangles (region); ++i)
    {
      cairo_rectangle_int_t r;

      cairo_region_get_rectangle (region, i, &r);
      printf ("%f %f %f %f",
              r.x / width,
              r.y / height,
              (r.x + r.width) / width,
              (r.y + r.height) / height);
      if (i < cairo_region_num_rectangles (region) - 1)
        putchar (':');
    }
  if (0 == cairo_region_num_rectangles (region))
    printf ("0.0 0.0 0.0 0.0");
}

/**
 * Return a string representation of a PopplerActionType.
 *
 * @param type The PopplerActionType.
 *
 * @return Its string representation.
 */
static const char *
xpoppler_action_type_string(PopplerActionType type)
{
  switch (type)
    {
    case POPPLER_ACTION_UNKNOWN: return "unknown";
    case POPPLER_ACTION_NONE: return "none";
    case POPPLER_ACTION_GOTO_DEST: return "goto-dest";
    case POPPLER_ACTION_GOTO_REMOTE: return "goto-remote";
    case POPPLER_ACTION_LAUNCH: return "launch";
    case POPPLER_ACTION_URI: return "uri";
    case POPPLER_ACTION_NAMED: return "goto-dest"; /* actually "named" */
    case POPPLER_ACTION_MOVIE: return "movie";
    case POPPLER_ACTION_RENDITION: return "rendition";
    case POPPLER_ACTION_OCG_STATE: return "ocg-state";
    case POPPLER_ACTION_JAVASCRIPT: return "javascript";
    default: return "invalid";
    }
}

/**
 * Return a string representation of a PopplerAnnotType.
 *
 * @param type The PopplerAnnotType.
 *
 * @return Its string representation.
 */
static const char *
xpoppler_annot_type_string (PopplerAnnotType type)
{
  switch (type)
    {
    case POPPLER_ANNOT_UNKNOWN: return "unknown";
    case POPPLER_ANNOT_TEXT: return "text";
    case POPPLER_ANNOT_LINK: return "link";
    case POPPLER_ANNOT_FREE_TEXT: return "free-text";
    case POPPLER_ANNOT_LINE: return "line";
    case POPPLER_ANNOT_SQUARE: return "square";
    case POPPLER_ANNOT_CIRCLE: return "circle";
    case POPPLER_ANNOT_POLYGON: return "polygon";
    case POPPLER_ANNOT_POLY_LINE: return "poly-line";
    case POPPLER_ANNOT_HIGHLIGHT: return "highlight";
    case POPPLER_ANNOT_UNDERLINE: return "underline";
    case POPPLER_ANNOT_SQUIGGLY: return "squiggly";
    case POPPLER_ANNOT_STRIKE_OUT: return "strike-out";
    case POPPLER_ANNOT_STAMP: return "stamp";
    case POPPLER_ANNOT_CARET: return "caret";
    case POPPLER_ANNOT_INK: return "ink";
    case POPPLER_ANNOT_POPUP: return "popup";
    case POPPLER_ANNOT_FILE_ATTACHMENT: return "file";
    case POPPLER_ANNOT_SOUND: return "sound";
    case POPPLER_ANNOT_MOVIE: return "movie";
    case POPPLER_ANNOT_WIDGET: return "widget";
    case POPPLER_ANNOT_SCREEN: return "screen";
    case POPPLER_ANNOT_PRINTER_MARK: return "printer-mark";
    case POPPLER_ANNOT_TRAP_NET: return "trap-net";
    case POPPLER_ANNOT_WATERMARK: return "watermark";
    case POPPLER_ANNOT_3D: return "3d";
    default: return "invalid";
    }
}

/**
 * Return a string representation of a PopplerAnnotTextState.
 *
 * @param type The PopplerAnnotTextState.
 *
 * @return Its string representation.
 */
static const char *
xpoppler_annot_text_state_string (PopplerAnnotTextState state)
{
  switch (state)
    {
    case POPPLER_ANNOT_TEXT_STATE_MARKED: return "marked";
    case POPPLER_ANNOT_TEXT_STATE_UNMARKED: return "unmarked";
    case POPPLER_ANNOT_TEXT_STATE_ACCEPTED: return "accepted";
    case POPPLER_ANNOT_TEXT_STATE_REJECTED: return "rejected";
    case POPPLER_ANNOT_TEXT_STATE_CANCELLED: return "cancelled";
    case POPPLER_ANNOT_TEXT_STATE_COMPLETED: return "completed";
    case POPPLER_ANNOT_TEXT_STATE_NONE: return "none";
    case POPPLER_ANNOT_TEXT_STATE_UNKNOWN:
    default: return "unknown";
    }
};

#ifdef HAVE_POPPLER_ANNOT_STAMP
/* The standard stamp icons of the PDF specification, which poppler
   is able to set (and draw).  Draft is the default icon of a newly
   created stamp, but poppler-glib can neither set nor report it. */
static const struct
{
  PopplerAnnotStampIcon icon;
  const char *name;
} xpoppler_annot_stamp_icons[] =
  {
    {POPPLER_ANNOT_STAMP_ICON_APPROVED, "Approved"},
    {POPPLER_ANNOT_STAMP_ICON_AS_IS, "AsIs"},
    {POPPLER_ANNOT_STAMP_ICON_CONFIDENTIAL, "Confidential"},
    {POPPLER_ANNOT_STAMP_ICON_DEPARTMENTAL, "Departmental"},
    {POPPLER_ANNOT_STAMP_ICON_EXPERIMENTAL, "Experimental"},
    {POPPLER_ANNOT_STAMP_ICON_EXPIRED, "Expired"},
    {POPPLER_ANNOT_STAMP_ICON_FINAL, "Final"},
    {POPPLER_ANNOT_STAMP_ICON_FOR_COMMENT, "ForComment"},
    {POPPLER_ANNOT_STAMP_ICON_FOR_PUBLIC_RELEASE, "ForPublicRelease"},
    {POPPLER_ANNOT_STAMP_ICON_NOT_APPROVED, "NotApproved"},
    {POPPLER_ANNOT_STAMP_ICON_NOT_FOR_PUBLIC_RELEASE, "NotForPublicRelease"},
    {POPPLER_ANNOT_STAMP_ICON_SOLD, "Sold"},
    {POPPLER_ANNOT_STAMP_ICON_TOP_SECRET, "TopSecret"},
  };

/**
 * Return the name of a PopplerAnnotStampIcon.
 *
 * @param icon The PopplerAnnotStampIcon.
 *
 * @return Its standard PDF name, or "" if it has none.
 */
static const char *
xpoppler_annot_stamp_icon_string (PopplerAnnotStampIcon icon)
{
  int i;

  for (i = 0; i < G_N_ELEMENTS (xpoppler_annot_stamp_icons); ++i)
    if (xpoppler_annot_stamp_icons[i].icon == icon)
      return xpoppler_annot_stamp_icons[i].name;
  return "";
}

/**
 * Parse the name of a standard stamp icon.
 *
 * @param name The icon's name, e.g. "Approved".
 * @param icon[out] Where to store the icon.
 *
 * @return TRUE, if name is a known icon.
 */
static gboolean
xpoppler_annot_stamp_icon_parse (const char *name, PopplerAnnotStampIcon *icon)
{
  int i;

  for (i = 0; i < G_N_ELEMENTS (xpoppler_annot_stamp_icons); ++i)
    if (! strcmp (xpoppler_annot_stamp_icons[i].name, name))
      {
        *icon = xpoppler_annot_stamp_icons[i].icon;
        return TRUE;
      }
  return FALSE;
}
#endif

/**
 * Validate a PopplerSelectionStyle by replacing invalid styles
 * with a default of POPPLER_SELECTION_GLYPH.
 *
 * @param selection_style The selection style.
 *
 * @return selection_style for valid styles, otherwise POPPLER_SELECTION_GLYPH.
 */
static PopplerSelectionStyle
xpoppler_validate_selection_style (int selection_style)
{
  switch (selection_style) {
    case POPPLER_SELECTION_GLYPH:
    case POPPLER_SELECTION_WORD:
    case POPPLER_SELECTION_LINE:
      return selection_style;
  }
  return POPPLER_SELECTION_GLYPH;
}

static document_t*
document_open (const epdfinfo_t *ctx, const char *filename,
               const char *passwd, GError **gerror)
{
  char *uri;
  document_t *doc = g_hash_table_lookup (ctx->documents, filename);

  if (NULL != doc)
    return doc;

  doc = g_malloc0(sizeof (document_t));
  uri = g_filename_to_uri (filename, NULL, gerror);
  if (uri != NULL)
    doc->pdf = poppler_document_new_from_file(uri, passwd, gerror);

  if (NULL == doc->pdf)
    {
      g_free (doc);
      doc = NULL;
    }
  else
    {
      doc->filename = g_strdup (filename);
      doc->passwd = g_strdup (passwd);
      g_hash_table_insert (ctx->documents, doc->filename, doc);
    }
  g_free (uri);
  return doc;
}

/**
 * Split command args into a list of strings.
 *
 * @param args The colon separated list of arguments.
 * @param nargs[out] The number of returned arguments.
 *
 * @return The list of arguments, which should be freed by the caller.
 */
static char **
command_arg_split (const char *args, int *nargs)
{
  char **list = g_malloc (sizeof (char*) * 16);
  int i = 0;
  size_t allocated = 16;
  char *buffer = NULL;
  gboolean last = FALSE;

  if (! args)
    goto theend;

  buffer = g_malloc (strlen (args) + 1);

  while (*args || last)
    {
      gboolean esc = FALSE;
      char *buffer_p = buffer;

      while (*args && (*args != ':' || esc))
        {
          if (esc)
            {
              if (*args == 'n')
                {
                  ++args;
                  *buffer_p++ = '\n';
                }
              else
                {
                  *buffer_p++ = *args++;
                }
              esc = FALSE;
            }
          else if (*args == '\\')
            {
              ++args;
              esc = TRUE;
            }
          else
            {
              *buffer_p++ = *args++;
            }
        }

      *buffer_p = '\0';

      if (i >= allocated)
        {
          allocated = 2 * allocated + 1;
          list = g_realloc (list, sizeof (char*) * allocated);
        }
      list[i++] = g_strdup (buffer);

      last = FALSE;
      if (*args)
        {
          ++args;
          if (! *args)
            last = TRUE;
        }
    }

 theend:
  g_free (buffer);
  *nargs = i;

  return list;
}

static gboolean
command_arg_parse_arg (const epdfinfo_t *ctx, const char *arg,
                       command_arg_t *cmd_arg, command_arg_type_t type,
                       gchar **error_msg)
{
  GError *gerror = NULL;

  if (! arg || !cmd_arg)
    return FALSE;

  switch (type)
    {
    case ARG_DOC:
      {
        document_t *doc = document_open (ctx, arg, NULL, &gerror);
        cerror_if_not (doc, error_msg,
                       "Error opening %s:%s", arg,
                       gerror ? gerror->message : "Unknown reason");

        cmd_arg->value.doc = doc;
        break;
      }
    case ARG_BOOL:
      cerror_if_not (! strcmp (arg, "0") || ! strcmp (arg, "1"),
                     error_msg, "Expected 0 or 1:%s", arg);
      cmd_arg->value.flag = *arg == '1';
      break;
    case ARG_DOUBLE:
      {
        char *endptr;
        double n = strtod (arg, &endptr);
        cerror_if_not (! (*endptr),
                       error_msg, "Expected double (floating point): %s", arg);
        cmd_arg->value.scalar = n;
      }
      break;
    case ARG_NONEMPTY_STRING:
      cerror_if_not (*arg, error_msg, "Non-empty string expected");
      /* fall through */
    case ARG_STRING:
      cmd_arg->value.string = arg;
      break;
    case ARG_NATNUM:
      {
        char *endptr;
        long n = strtol (arg, &endptr, 0);
        cerror_if_not (! (*endptr || (n < 0)), error_msg,
                       "Expected natural number:%s", arg);
        cmd_arg->value.natnum = n;
      }
      break;
    case ARG_EDGES_OR_POSITION:
      {
        PopplerRectangle *r = &cmd_arg->value.rectangle;
        cerror_if_not (parse_edges_or_position (arg, r),
                       error_msg,
                       "Expected a relative position or rectangle: %s", arg);
      }
      break;
    case ARG_EDGES:
      {
        PopplerRectangle *r = &cmd_arg->value.rectangle;
        cerror_if_not (parse_edges (arg, r),
                       error_msg,
                       "Expected a relative rectangle: %s", arg);
      }
      break;
    case ARG_EDGE_OR_NEGATIVE:
    case ARG_EDGE:
      {
        char *endptr;
        double n = strtod (arg, &endptr);
        cerror_if_not (! (*endptr || (type != ARG_EDGE_OR_NEGATIVE && n < 0.0) || n > 1.0),
                       error_msg, "Expected a relative edge: %s", arg);
        cmd_arg->value.edge = n;
      }
      break;
    case ARG_COLOR:
      {
        guint r,g,b;
        cerror_if_not ((strlen (arg) == 7
                        && 3 == sscanf (arg, "#%2x%2x%2x", &r, &g, &b)),
                       error_msg, "Invalid color: %s", arg);
        /* Scale 0xff to 0xffff, so that colors survive a round trip
           through a saved document. */
        cmd_arg->value.color.red = r * 257;
        cmd_arg->value.color.green = g * 257;
        cmd_arg->value.color.blue = b * 257;
      }
      break;
    case ARG_INVALID:
    default:
      internal_error ("switch fell through");
    }

  cmd_arg->type = type;

  return TRUE;
 error:
  if (gerror)
    {
      g_error_free (gerror);
      gerror = NULL;
    }
  return FALSE;
}

/**
 * Parse arguments for a command.
 *
 * @param ctx The epdfinfo context.
 * @param args A string holding the arguments.  This is either empty
 *             or the suffix of the command starting at the first
 *             colon after the command name.
 * @param len The length of args.
 * @param cmd The command for which the arguments should be parsed.
 *
 * @return
 */
static command_arg_t*
command_arg_parse(epdfinfo_t *ctx, char **args, int nargs,
                  const command_t *cmd, gchar **error_msg)
{
  command_arg_t *cmd_args = g_malloc0 (cmd->nargs * sizeof (command_arg_t));
  int i;

  if (nargs < cmd->nargs - 1
      || (nargs == cmd->nargs - 1
          &&  cmd->args_spec[cmd->nargs - 1] != ARG_REST)
      || (nargs > cmd->nargs
          && (cmd->nargs == 0
              || cmd->args_spec[cmd->nargs - 1] != ARG_REST)))
    {
      if (error_msg)
        {
          *error_msg =
            g_strdup_printf ("Command `%s' expects %d argument(s), %d given",
                             cmd->name, cmd->nargs, nargs);
        }
      goto failure;
    }

  for (i = 0; i < cmd->nargs; ++i)
    {
      if (i == cmd->nargs - 1 && cmd->args_spec[i] == ARG_REST)
        {
          cmd_args[i].value.rest.args = args + i;
          cmd_args[i].value.rest.nargs = nargs - i;
          cmd_args[i].type = ARG_REST;
        }
      else if (i >= nargs
               || ! command_arg_parse_arg (ctx, args[i], cmd_args + i,
                                           cmd->args_spec[i], error_msg))
        {
          goto failure;
        }
    }

  return cmd_args;

 failure:
  free_command_args (cmd_args, cmd->nargs);
  return NULL;
}

static void
command_arg_print(const command_arg_t *arg)
{
  switch (arg->type)
    {
    case ARG_INVALID:
      printf ("[invalid]");
      break;
    case ARG_DOC:
      print_response_string (arg->value.doc->filename, NONE);
      break;
    case ARG_BOOL:
      printf ("%d", arg->value.flag ? 1 : 0);
      break;
    case ARG_DOUBLE:
      printf ("%f", arg->value.scalar);
      break;
    case ARG_NONEMPTY_STRING:   /* fall */
    case ARG_STRING:
      print_response_string (arg->value.string, NONE);
      break;
    case ARG_NATNUM:
      printf ("%ld", arg->value.natnum);
      break;
    case ARG_EDGE_OR_NEGATIVE:  /* fall */
    case ARG_EDGE:
      printf ("%f", arg->value.edge);
      break;
    case ARG_EDGES_OR_POSITION: /* fall */
    case ARG_EDGES:
      {
        const PopplerRectangle *r = &arg->value.rectangle;
        if (r->x2 < 0 && r->y2 < 0)
          printf ("%f %f", r->x1, r->y1);
        else
          printf ("%f %f %f %f", r->x1, r->y1, r->x2, r->y2);
        break;
      }
    case ARG_COLOR:
      {
        const PopplerColor *c = &arg->value.color;
        printf ("#%.2x%.2x%.2x", c->red >> 8,
                c->green >> 8, c->blue >> 8);
        break;
      }
    case ARG_REST:
      {
        int i;
        for (i = 0; i < arg->value.rest.nargs; ++i)
          print_response_string (arg->value.rest.args[i], COLON);
        if (arg->value.rest.nargs > 0)
          print_response_string (arg->value.rest.args[i], NONE);
        break;
      }
    default:
      internal_error ("switch fell through");
    }
}

static size_t
command_arg_type_size(command_arg_type_t type)
{
  command_arg_t arg;
  switch (type)
    {
    case ARG_INVALID: return 0;
    case ARG_DOC: return sizeof (arg.value.doc);
    case ARG_BOOL: return sizeof (arg.value.flag);
    case ARG_DOUBLE: return sizeof (arg.value.scalar);
    case ARG_NONEMPTY_STRING:   /* fall */
    case ARG_STRING: return sizeof (arg.value.string);
    case ARG_NATNUM: return sizeof (arg.value.natnum);
    case ARG_EDGE_OR_NEGATIVE:  /* fall */
    case ARG_EDGE: return sizeof (arg.value.edge);
    case ARG_EDGES_OR_POSITION: /* fall */
    case ARG_EDGES: return sizeof (arg.value.rectangle);
    case ARG_COLOR: return sizeof (arg.value.color);
    case ARG_REST: return sizeof (arg.value.rest);
    default:
      internal_error ("switch fell through");
      return 0;
    }
}


/* ------------------------------------------------------------------ *
 * PDF Actions
 * ------------------------------------------------------------------ */

static gboolean
action_is_handled (PopplerAction *action)
{
  if (! action)
    return FALSE;

  switch (action->any.type)
    {
    case POPPLER_ACTION_GOTO_REMOTE:
    case POPPLER_ACTION_GOTO_DEST:
    case POPPLER_ACTION_NAMED:
      /* case POPPLER_ACTION_LAUNCH: */
    case POPPLER_ACTION_URI:
      return TRUE;
    default: ;
    }
  return FALSE;
}

static void
action_print_destination (PopplerDocument *doc, PopplerAction *action)
{
  PopplerDest *dest = NULL;
  gboolean free_dest = FALSE;
  double width, height, top;
  PopplerPage *page;
  int saved_stdin;

  if (action->any.type == POPPLER_ACTION_GOTO_DEST
      && action->goto_dest.dest->type == POPPLER_DEST_NAMED)
    {
      DISCARD_STDOUT (saved_stdin);
      /* poppler_document_find_dest reports errors to stdout, so
         discard them. */
      dest = poppler_document_find_dest
        (doc, action->goto_dest.dest->named_dest);
      UNDISCARD_STDOUT (saved_stdin);
      free_dest = TRUE;
    }
  else if (action->any.type == POPPLER_ACTION_NAMED)

    {
      DISCARD_STDOUT (saved_stdin);
      dest = poppler_document_find_dest (doc, action->named.named_dest);
      UNDISCARD_STDOUT (saved_stdin);
      free_dest = TRUE;
    }

  else if (action->any.type == POPPLER_ACTION_GOTO_REMOTE)
    {
      print_response_string (action->goto_remote.file_name, COLON);
      dest = action->goto_remote.dest;
    }
  else if (action->any.type == POPPLER_ACTION_GOTO_DEST)
    dest = action->goto_dest.dest;

  if (!dest
      || dest->type == POPPLER_DEST_UNKNOWN
      || dest->page_num < 1
      || dest->page_num > poppler_document_get_n_pages (doc))
    {
      printf (":");
      goto theend;
    }

  printf ("%d:", dest->page_num);

  if (action->type == POPPLER_ACTION_GOTO_REMOTE
      || NULL == (page = poppler_document_get_page (doc, dest->page_num - 1)))
    {
      goto theend;
    }

  poppler_page_get_size (page, &width, &height);
  g_object_unref (page);
  top = (height - dest->top) / height;

  /* adapted from xpdf */
  switch (dest->type)
    {
    case POPPLER_DEST_XYZ:
      if (dest->change_top)
        printf ("%f", top);
      break;
    case POPPLER_DEST_FIT:
    case POPPLER_DEST_FITB:
    case POPPLER_DEST_FITH:
    case POPPLER_DEST_FITBH:
      putchar ('0');
      break;
    case POPPLER_DEST_FITV:
    case POPPLER_DEST_FITBV:
    case POPPLER_DEST_FITR:
      printf ("%f", top);
      break;
    default: ;
    }

 theend:
  if (free_dest)
    poppler_dest_free (dest);
}

static void
action_print (PopplerDocument *doc, PopplerAction *action)
{
  if (! action_is_handled (action))
    return;

  print_response_string (xpoppler_action_type_string (action->any.type), COLON);
  print_response_string (action->any.title, COLON);
  switch (action->any.type)
    {
    case POPPLER_ACTION_GOTO_REMOTE:
    case POPPLER_ACTION_GOTO_DEST:
    case POPPLER_ACTION_NAMED:
      action_print_destination (doc, action);
      putchar ('\n');
      break;
    case POPPLER_ACTION_LAUNCH:
      print_response_string (action->launch.file_name, COLON);
      print_response_string (action->launch.params, NEWLINE);
      break;
    case POPPLER_ACTION_URI:
      print_response_string (action->uri.uri, NEWLINE);
      break;
    default:
      ;
    }
}


/* ------------------------------------------------------------------ *
 * PDF Annotations and Attachments
 * ------------------------------------------------------------------ */

/* static gint
 * annotation_cmp_edges (const annotation_t *a1, const annotation_t *a2)
 * {
 *   PopplerRectangle *e1 = &a1->amap->area;
 *   PopplerRectangle *e2 = &a2->amap->area;
 *
 *   return (e1->y1 > e2->y1 ? -1
 *           : e1->y1 < e2->y1 ? 1
 *           : e1->x1 < e2->x1 ? -1
 *           : e1->x1 != e2->x1);
 * } */

static GList*
annoation_get_for_page (document_t *doc, gint pn)
{

  GList *annot_list, *item;
  PopplerPage *page;
  gint i = 0;
  gint npages = poppler_document_get_n_pages (doc->pdf);

  if (pn < 1 || pn > npages)
    return NULL;

  if (! doc->annotations.pages)
    doc->annotations.pages = g_malloc0 (npages * sizeof(GList*));

  if (doc->annotations.pages[pn - 1])
    return doc->annotations.pages[pn - 1];

  if (! doc->annotations.keys)
    doc->annotations.keys = g_hash_table_new (g_str_hash, g_str_equal);

  page = poppler_document_get_page (doc->pdf, pn - 1);
  if (NULL == page)
    return NULL;

  annot_list = poppler_page_get_annot_mapping (page);
  for (item = annot_list; item; item = item->next)
    {
      PopplerAnnotMapping *map = (PopplerAnnotMapping *)item->data;
      gchar *key = g_strdup_printf ("annot-%d-%d", pn, i);
      annotation_t *a = g_malloc (sizeof (annotation_t));
      a->amap = map;
      a->key = key;
      a->image_file = NULL;
      doc->annotations.pages[pn - 1] =
        g_list_prepend (doc->annotations.pages[pn - 1], a);
      assert (NULL == g_hash_table_lookup (doc->annotations.keys, key));
      g_hash_table_insert (doc->annotations.keys, key, a);
      ++i;
    }
  g_list_free (annot_list);
  g_object_unref (page);
  return doc->annotations.pages[pn - 1];
}

static annotation_t*
annotation_get_by_key (document_t *doc, const gchar *key)
{
  if (! doc->annotations.keys)
    return NULL;

  return g_hash_table_lookup (doc->annotations.keys, key);
}

#ifdef HAVE_POPPLER_ANNOT_MARKUP
static cairo_region_t*
annotation_markup_get_text_regions (PopplerPage *page, PopplerAnnotTextMarkup *a)
{
  GArray *quads = poppler_annot_text_markup_get_quadrilaterals (a);
  int i;
  cairo_region_t *region = cairo_region_create ();
  gdouble height;

  poppler_page_get_size (page, NULL, &height);

  for (i = 0; i < quads->len; ++i)
    {
      PopplerQuadrilateral *q = &g_array_index (quads, PopplerQuadrilateral, i);
      cairo_rectangle_int_t r;

      q->p1.y = height - q->p1.y;
      q->p2.y = height - q->p2.y;
      q->p3.y = height - q->p3.y;
      q->p4.y = height - q->p4.y;

      r.x = (int) (MIN (q->p1.x, MIN (q->p2.x, MIN (q->p3.x, q->p4.x))) + 0.5);
      r.y = (int) (MIN (q->p1.y, MIN (q->p2.y, MIN (q->p3.y, q->p4.y))) + 0.5);
      r.width = (int) (MAX (q->p1.x, MAX (q->p2.x, MAX (q->p3.x, q->p4.x))) + 0.5)
                - r.x;
      r.height = (int) (MAX (q->p1.y, MAX (q->p2.y, MAX (q->p3.y, q->p4.y))) + 0.5)
                 - r.y;

      cairo_region_union_rectangle (region, &r);
    }
  g_array_unref (quads);
  return region;
}

/**
 * Append quadrilaterals equivalent to region to an array.
 *
 * @param page The page of the annotation.  This is used to get the
 *             text regions and pagesize.
 * @param selection_style The selection style.
 * @param region The region to add.
 * @param garray[in,out] An array of PopplerQuadrilateral, where the
 *              new quadrilaterals will be appended.
 */
static void
annotation_markup_append_text_region (PopplerPage *page,
				      PopplerSelectionStyle selection_style,
				      PopplerRectangle *region,
                                      GArray *garray)
{
  gdouble height;
  /* poppler_page_get_selection_region is deprecated w/o a
     replacement.  (poppler_page_get_selected_region returns a union
     of rectangles.) */
  GList *regions =
    poppler_page_get_selection_region (page, 1.0, selection_style, region);
  GList *item;

  poppler_page_get_size (page, NULL, &height);
  for (item = regions; item; item = item->next)
    {
      PopplerRectangle *r = item->data;
      PopplerQuadrilateral q;

      q.p1.x = r->x1;
      q.p1.y = height - r->y1;
      q.p2.x = r->x2;
      q.p2.y = height - r->y1;
      q.p4.x = r->x2;
      q.p4.y = height - r->y2;
      q.p3.x = r->x1;
      q.p3.y = height - r->y2;

      g_array_append_val (garray, q);
    }
  g_list_free (regions);
}

#endif

#ifdef HAVE_POPPLER_ANNOT_SHAPES
/* Margin around a line annotation's end points, in PDF points. */
#define LINE_ANNOT_MARGIN 4.0

/**
 * Parse the relative end points of a line annotation.
 *
 * @param ctx The epdfinfo context.
 * @param page The page of the annotation.
 * @param arg The points as a relative "X1 Y1 X2 Y2" string.
 * @param start[out] The start point in PDF coordinates.
 * @param end[out] The end point in PDF coordinates.
 * @param rect[out] The bounding box of the points, plus a small
 *             margin and clipped to the page, in PDF coordinates.
 * @param error_msg[out] An error message, if this fails.
 *
 * @return TRUE, if arg is valid.
 */
static gboolean
annotation_line_parse_points (const epdfinfo_t *ctx, PopplerPage *page,
                              const char *arg, PopplerPoint *start,
                              PopplerPoint *end, PopplerRectangle *rect,
                              char **error_msg)
{
  command_arg_t points;
  double width, height;

  if (! command_arg_parse_arg (ctx, arg, &points, ARG_EDGES, error_msg))
    return FALSE;
  poppler_page_get_size (page, &width, &height);
  start->x = points.value.rectangle.x1 * width;
  start->y = height - points.value.rectangle.y1 * height;
  end->x = points.value.rectangle.x2 * width;
  end->y = height - points.value.rectangle.y2 * height;
  rect->x1 = MAX (0, MIN (start->x, end->x) - LINE_ANNOT_MARGIN);
  rect->y1 = MAX (0, MIN (start->y, end->y) - LINE_ANNOT_MARGIN);
  rect->x2 = MIN (width, MAX (start->x, end->x) + LINE_ANNOT_MARGIN);
  rect->y2 = MIN (height, MAX (start->y, end->y) + LINE_ANNOT_MARGIN);
  return TRUE;
}
#endif

#ifdef HAVE_POPPLER_ANNOT_FREE_TEXT
/* The font of a new free text annotation, until it is changed with
   the font-name and font-size properties. */
#define FREE_TEXT_ANNOT_FONT_NAME "Helvetica"
#define FREE_TEXT_ANNOT_FONT_SIZE 12.0

/**
 * Change the font of a free text annotation.
 *
 * This also makes sure, that the font has glyphs for all characters
 * of the annotation's contents, so it should be called again after
 * the contents changed.
 *
 * @param a The free text annotation.
 * @param name The new font family, or NULL to keep the current one.
 * @param size The new size in points, or a value <= 0 to keep the
 *             current one.
 */
static void
annotation_free_text_set_font (PopplerAnnotFreeText *a, const char *name,
                               double size)
{
  PopplerFontDescription *font = poppler_annot_free_text_get_font_desc (a);

  if (! font)
    {
      font = poppler_font_description_new (FREE_TEXT_ANNOT_FONT_NAME);
      font->size_pt = FREE_TEXT_ANNOT_FONT_SIZE;
    }
  if (name)
    {
      g_free (font->font_name);
      font->font_name = g_strdup (name);
    }
  if (size > 0)
    font->size_pt = size;
  poppler_annot_free_text_set_font_desc (a, font);
  poppler_font_description_free (font);
}
#endif

#ifdef HAVE_POPPLER_ANNOT_INK
/**
 * Parse the relative points of an ink annotation's stroke.
 *
 * @param ctx The epdfinfo context.
 * @param page The page of the annotation.
 * @param arg The points as a relative "X1 Y1 X2 Y2 ..." string.  A
 *            single point is drawn as a dot.
 * @param error_msg[out] An error message, if this fails.
 *
 * @return The stroke as a path in PDF coordinates, or NULL if arg
 *         is invalid.
 */
static PopplerPath *
annotation_ink_parse_path (const epdfinfo_t *ctx, PopplerPage *page,
                           const char *arg, char **error_msg)
{
  GArray *points = g_array_new (FALSE, FALSE, sizeof (PopplerPoint));
  double width, height;
  const char *s = arg;
  gsize n;

  poppler_page_get_size (page, &width, &height);
  while (TRUE)
    {
      PopplerPoint p;
      double x, y;
      char *end;

      while (g_ascii_isspace (*s))
        ++s;
      if (! *s)
        break;
      x = g_ascii_strtod (s, &end);
      cerror_if_not (end != s, error_msg, "Invalid ink path: %s", arg);
      s = end;
      y = g_ascii_strtod (s, &end);
      cerror_if_not (end != s, error_msg, "Invalid ink path: %s", arg);
      s = end;
      cerror_if_not (x >= 0 && x <= 1 && y >= 0 && y <= 1, error_msg,
                     "Ink path is not relative: %s", arg);
      p.x = x * width;
      p.y = height - y * height;
      g_array_append_val (points, p);
    }
  cerror_if_not (points->len > 0, error_msg, "%s", "Empty ink path");
  if (points->len == 1)
    {
      /* Draw a dot as a short line, a line of length 0 is not
         drawn at all. */
      PopplerPoint p = g_array_index (points, PopplerPoint, 0);
      p.x = p.x + 1 <= width ? p.x + 1 : p.x - 1;
      g_array_append_val (points, p);
    }
  n = points->len;
  return poppler_path_new_from_array ((PopplerPoint *)
                                      g_array_free (points, FALSE), n);
 error:
  g_array_free (points, TRUE);
  return NULL;
}

/**
 * Return TRUE if arg is a single number, i.e. the line width of an
 * ink annotation rather than a stroke.
 */
static gboolean
annotation_ink_width_arg_p (const char *arg, double *width)
{
  char *end;

  *width = g_ascii_strtod (arg, &end);
  if (end == arg)
    return FALSE;
  while (g_ascii_isspace (*end))
    ++end;
  return ! *end;
}

/**
 * Set the strokes of an ink annotation.
 *
 * The annotation must already be on page, see
 * poppler_annot_ink_set_ink_list.  This also sets its rectangle to
 * the bounding box of the strokes.
 *
 * @param ctx The epdfinfo context.
 * @param page The page of the annotation.
 * @param a The ink annotation.
 * @param args The strokes as relative "X1 Y1 X2 Y2 ..." strings,
 *             optionally preceded by the line width in points.
 * @param nargs The number of args.
 * @param error_msg[out] An error message, if this fails.
 *
 * @return TRUE, if all strokes were valid.  Otherwise the annotation
 *         is not changed.
 */
static gboolean
annotation_ink_set_paths (const epdfinfo_t *ctx, PopplerPage *page,
                          PopplerAnnotInk *a, char * const *args,
                          int nargs, char **error_msg)
{
  GPtrArray *paths = g_ptr_array_new_with_free_func
    ((GDestroyNotify) poppler_path_free);
  gboolean ok = FALSE;
  double width = 0;
  int i = 0;

  if (nargs > 0 && annotation_ink_width_arg_p (args[0], &width))
    {
      cerror_if_not (width > 0, error_msg, "Invalid line width: %s", args[0]);
      ++i;
    }
  for (; i < nargs; ++i)
    {
      PopplerPath *path = annotation_ink_parse_path (ctx, page, args[i],
                                                     error_msg);
      if (! path)
        goto error;
      g_ptr_array_add (paths, path);
    }
  cerror_if_not (paths->len > 0, error_msg, "%s",
                 "Unable to create empty ink annotation");
  /* The ink list's rectangle depends on the width, so set it first. */
  if (width > 0)
    poppler_annot_set_border_width (POPPLER_ANNOT (a), width);
  poppler_annot_ink_set_ink_list (a, (PopplerPath **) paths->pdata,
                                  paths->len);
  ok = TRUE;
 error:
  g_ptr_array_unref (paths);
  return ok;
}

/**
 * Move the strokes of an ink annotation.
 *
 * @param a The ink annotation.
 * @param dx The horizontal offset in PDF points.
 * @param dy The vertical offset in PDF points.
 */
static void
annotation_ink_translate (PopplerAnnotInk *a, double dx, double dy)
{
  gsize npaths, i;
  PopplerPath **paths = poppler_annot_ink_get_ink_list (a, &npaths);

  for (i = 0; i < npaths; ++i)
    {
      gsize npoints, j;
      PopplerPoint *points = poppler_path_get_points (paths[i], &npoints);

      for (j = 0; j < npoints; ++j)
        {
          points[j].x += dx;
          points[j].y += dy;
        }
    }
  poppler_annot_ink_set_ink_list (a, paths, npaths);
  for (i = 0; i < npaths; ++i)
    poppler_path_free (paths[i]);
  g_free (paths);
}
#endif

/**
 * Create a new annotation.
 *
 * @param doc The document for which to create it.
 * @param type The type of the annotation.
 * @param selection_style The selection style.
 * @param r The rectangle where annotation will end up on the page.
 *
 * @return The new annotation, or NULL, if the annotation type is
 *         not available.
 */

#ifdef HAVE_POPPLER_ANNOT_STAMP
#if CAIRO_HAS_PNG_FUNCTIONS

/* Load the PNG image in FILENAME as a cairo surface, or return NULL
   and set ERROR_MSG.

   The pixels of the returned surface are *PIXELS, which must be freed
   with g_free () once poppler has copied them; it is NULL when the
   image was not copied, see below.

   poppler's get_raw_data_from_cairo_image () steps a pointer four
   bytes along before it reads the alpha byte of a pixel, so on the
   last pixel of the last row of an image it reads up to three bytes
   past the end of the pixel data, which the address sanitizer reports
   as a heap-buffer-overflow (seen in poppler 24.02 through at least
   25.03, in glib/poppler-annot.cc).  A surface of our own, with four
   spare bytes after the pixels, keeps that read inside an allocation
   and leaves the image and the way poppler turns it into an XObject
   untouched. */
static cairo_surface_t *
xcairo_image_surface_from_png (const char *filename, unsigned char **pixels,
                               char **error_msg)
{
  cairo_surface_t *png = cairo_image_surface_create_from_png (filename);
  cairo_status_t status = cairo_surface_status (png);
  cairo_surface_t *surface;
  cairo_format_t format;
  cairo_t *cr;
  int width, height, stride;

  *pixels = NULL;
  if (status != CAIRO_STATUS_SUCCESS)
    {
      if (error_msg)
        *error_msg = g_strdup_printf
          ("Cannot read a PNG image from `%s': %s",
           filename, cairo_status_to_string (status));
      cairo_surface_destroy (png);
      return NULL;
    }

  /* Leave any other kind of image to poppler, which has a better
     error message for it than we would have here. */
  format = cairo_image_surface_get_format (png);
  if (format != CAIRO_FORMAT_ARGB32 && format != CAIRO_FORMAT_RGB24)
    return png;

  width = cairo_image_surface_get_width (png);
  height = cairo_image_surface_get_height (png);
  stride = cairo_format_stride_for_width (format, width);
  /* The four spare bytes, see above. */
  *pixels = g_malloc0 ((size_t) stride * (size_t) height + 4);
  surface = cairo_image_surface_create_for_data (*pixels, format,
                                                 width, height, stride);
  cr = cairo_create (surface);
  cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
  cairo_set_source_surface (cr, png, 0, 0);
  cairo_paint (cr);
  cairo_surface_destroy (png);

  status = cairo_status (cr);
  cairo_destroy (cr);
  if (status != CAIRO_STATUS_SUCCESS)
    {
      if (error_msg)
        *error_msg = g_strdup_printf
          ("Cannot copy the PNG image from `%s': %s",
           filename, cairo_status_to_string (status));
      cairo_surface_destroy (surface);
      g_free (*pixels);
      *pixels = NULL;
      return NULL;
    }
  return surface;
}

/* Make the stamp annotation A show the PNG image in FILENAME.
   Return TRUE on success, FALSE (setting ERROR_MSG) otherwise. */
static gboolean
annotation_stamp_set_image (PopplerAnnotStamp *a, const char *filename,
                            char **error_msg)
{
  unsigned char *pixels;
  cairo_surface_t *surface = xcairo_image_surface_from_png (filename,
                                                            &pixels,
                                                            error_msg);
  GError *gerror = NULL;
  gboolean ok;

  if (! surface)
    return FALSE;
  ok = poppler_annot_stamp_set_custom_image (a, surface, &gerror);
  cairo_surface_destroy (surface);
  /* poppler copied the pixels by now. */
  g_free (pixels);
  if (! ok)
    {
      if (error_msg)
        *error_msg = g_strdup_printf
          ("Setting the image of the stamp failed: %s",
           gerror ? gerror->message : "Reason unknown");
      if (gerror)
        g_error_free (gerror);
    }
  return ok;
}

#endif /* CAIRO_HAS_PNG_FUNCTIONS */
#endif /* HAVE_POPPLER_ANNOT_STAMP */

static PopplerAnnot*
annotation_new (const epdfinfo_t *ctx, document_t *doc, PopplerPage *page,
                const char *type, PopplerSelectionStyle selection_style,
                PopplerRectangle *r, const command_arg_t *rest,
                char **error_msg)
{

  PopplerAnnot *a = NULL;
  int nargs = rest->value.rest.nargs;
#ifdef HAVE_POPPLER_ANNOT_MARKUP
  char * const *args = rest->value.rest.args;
  int i;
  GArray *garray = NULL;
  command_arg_t carg;
  double width, height;
  cairo_region_t *region = NULL;
#endif

  if (! strcmp (type, "text"))
    {
      cerror_if_not (nargs == 0, error_msg, "%s", "Too many arguments");
      return poppler_annot_text_new (doc->pdf, r);
    }

#ifdef HAVE_POPPLER_ANNOT_SHAPES
  if (! strcmp (type, "square"))
    {
      cerror_if_not (nargs == 0, error_msg, "%s", "Too many arguments");
      return poppler_annot_square_new (doc->pdf, r);
    }

  if (! strcmp (type, "circle"))
    {
      cerror_if_not (nargs == 0, error_msg, "%s", "Too many arguments");
      return poppler_annot_circle_new (doc->pdf, r);
    }

  if (! strcmp (type, "line"))
    {
      PopplerPoint start, end;

      cerror_if_not (nargs <= 1, error_msg, "%s", "Too many arguments");
      if (nargs == 1)
        {
          /* Start and end point as relative "X1 Y1 X2 Y2"; these
             determine the annotation's rectangle. */
          error_if_not (annotation_line_parse_points
                        (ctx, page, rest->value.rest.args[0],
                         &start, &end, r, error_msg));
        }
      else
        {
          /* From the top-left to the bottom-right corner of r. */
          start.x = r->x1;
          start.y = r->y2;
          end.x = r->x2;
          end.y = r->y1;
        }
      return poppler_annot_line_new (doc->pdf, r, &start, &end);
    }
#endif

#ifdef HAVE_POPPLER_ANNOT_STAMP
  if (! strcmp (type, "stamp"))
    {
      PopplerAnnotStampIcon icon = POPPLER_ANNOT_STAMP_ICON_UNKNOWN;
      const char *name = nargs > 0 ? rest->value.rest.args[0] : "";

      cerror_if_not (nargs <= 1, error_msg, "%s", "Too many arguments");
      if (! strncmp (name, "image:", 6))
        {
          /* A stamp showing the PNG image in the named file.  The
             image is set once the annotation is on the page, see
             cmd_addannot; check now that it can be read. */
#if CAIRO_HAS_PNG_FUNCTIONS
          unsigned char *pixels;
          cairo_surface_t *surface =
            xcairo_image_surface_from_png (name + 6, &pixels, error_msg);
          if (! surface)
            goto error;
          cairo_surface_destroy (surface);
          g_free (pixels);
#else
          cerror_if_not (0, error_msg, "%s",
                         "This epdfinfo cannot read PNG images");
#endif
        }
      else
        {
          /* An empty name or Draft keeps poppler's default icon, Draft. */
          cerror_if_not (! *name || ! strcmp (name, "Draft")
                         || xpoppler_annot_stamp_icon_parse (name, &icon),
                         error_msg, "Unknown stamp icon: %s", name);
        }
      a = poppler_annot_stamp_new (doc->pdf, r);
      if (icon != POPPLER_ANNOT_STAMP_ICON_UNKNOWN)
        poppler_annot_stamp_set_icon (POPPLER_ANNOT_STAMP (a), icon);
      return a;
    }
#endif

#ifdef HAVE_POPPLER_ANNOT_FREE_TEXT
  if (! strcmp (type, "free-text"))
    {
      PopplerColor black = {0, 0, 0};

      cerror_if_not (nargs == 0, error_msg, "%s", "Too many arguments");
      a = poppler_annot_free_text_new (doc->pdf, r);
      /* poppler-glib leaves font and color undefined. */
      annotation_free_text_set_font (POPPLER_ANNOT_FREE_TEXT (a), NULL, 0);
      poppler_annot_free_text_set_font_color (POPPLER_ANNOT_FREE_TEXT (a),
                                              &black);
      /* Just the text, no frame around it. */
      poppler_annot_set_border_width (a, 0);
      return a;
    }
#endif

#ifdef HAVE_POPPLER_ANNOT_INK
  if (! strcmp (type, "ink"))
    {
      double width;
      int i = 0;

      /* The strokes can only be set once the annotation is on the
         page, see cmd_addannot.  Check them now. */
      if (nargs > 0 && annotation_ink_width_arg_p (rest->value.rest.args[0],
                                                   &width))
        {
          cerror_if_not (width > 0, error_msg, "Invalid line width: %s",
                         rest->value.rest.args[0]);
          ++i;
        }
      cerror_if_not (i < nargs, error_msg, "%s",
                     "Unable to create empty ink annotation");
      for (; i < nargs; ++i)
        {
          PopplerPath *path = annotation_ink_parse_path
            (ctx, page, rest->value.rest.args[i], error_msg);
          if (! path)
            goto error;
          poppler_path_free (path);
        }
      return poppler_annot_ink_new (doc->pdf, r);
    }
#endif

#ifdef HAVE_POPPLER_ANNOT_MARKUP
  garray = g_array_new (FALSE, FALSE, sizeof (PopplerQuadrilateral));
  poppler_page_get_size (page, &width, &height);
  if (nargs == 0)
    {
      PopplerQuadrilateral q;

      q.p1.x = r->x1;
      q.p1.y = r->y1;
      q.p2.x = r->x2;
      q.p2.y = r->y1;
      q.p4.x = r->x2;
      q.p4.y = r->y2;
      q.p3.x = r->x1;
      q.p3.y = r->y2;

      g_array_append_val (garray, q);
    }
  for (i = 0; i < nargs; ++i)
    {
      PopplerRectangle *rr = &carg.value.rectangle;

      error_if_not (command_arg_parse_arg (ctx, args[i], &carg,
                                           ARG_EDGES, error_msg));
      rr->x1 *= width; rr->x2 *= width;
      rr->y1 *= height; rr->y2 *= height;
      annotation_markup_append_text_region (page, selection_style, rr, garray);
    }
  cerror_if_not (garray->len > 0, error_msg, "%s",
                 "Unable to create empty markup annotation");

  if (! strcmp (type, "highlight"))
    a = poppler_annot_text_markup_new_highlight (doc->pdf, r, garray);
  else if (! strcmp (type, "squiggly"))
    a = poppler_annot_text_markup_new_squiggly (doc->pdf, r, garray);
  else if (! strcmp (type, "strike-out"))
    a = poppler_annot_text_markup_new_strikeout (doc->pdf, r, garray);
  else if (! strcmp (type, "underline"))
    a = poppler_annot_text_markup_new_underline (doc->pdf, r, garray);
  else
    cerror_if_not (0, error_msg, "Unknown annotation type: %s", type);

#endif
 error:
#ifdef HAVE_POPPLER_ANNOT_MARKUP
  if (garray) g_array_unref (garray);
  if (region) cairo_region_destroy (region);
#endif
  return a;
}

/* Return TRUE if A is a markup annotation as far as epdfinfo is
   concerned.  Newer poppler-glib (26.01) makes stamps markup
   annotations; keep treating them as plain ones with every version. */
static gboolean
annotation_is_markup (PopplerAnnot *a)
{
#ifdef HAVE_POPPLER_ANNOT_STAMP
  if (POPPLER_IS_ANNOT_STAMP (a))
    return FALSE;
#endif
  return POPPLER_IS_ANNOT_MARKUP (a);
}

static gboolean
annotation_edit_validate (const epdfinfo_t *ctx, const command_arg_t *rest,
                          PopplerAnnot *annotation, char **error_msg)
{
  int nargs = rest->value.rest.nargs;
  char * const *args = rest->value.rest.args;
  int i = 0;
  command_arg_t carg;

  const char* error_fmt =
    "Can modify `%s' property only for %s annotations";

  while (i < nargs)
    {
      command_arg_type_t atype = ARG_INVALID;
      const char *key = args[i++];

      cerror_if_not (i < nargs, error_msg, "Missing a value argument");

      if (! strcmp (key, "flags"))
        atype = ARG_NATNUM;
      else if (! strcmp (key, "color"))
        atype = ARG_COLOR;
      else if (! strcmp (key, "contents"))
        atype = ARG_STRING;
      else if (! strcmp (key, "edges"))
        {
#ifdef HAVE_POPPLER_ANNOT_SHAPES
          /* Changing the rectangle would not move the line, which
             poppler-glib can not read back, see `line'. */
          cerror_if_not (! POPPLER_IS_ANNOT_LINE (annotation), error_msg,
                         "%s", "Use the `line' property to move line annotations");
#endif
          atype = ARG_EDGES_OR_POSITION;
        }
      else if (! strcmp (key, "label"))
        {
          cerror_if_not (annotation_is_markup (annotation), error_msg,
                         error_fmt, key, "markup");
          atype = ARG_STRING;
        }
      else if (! strcmp (key, "opacity"))
        {
          cerror_if_not (annotation_is_markup (annotation), error_msg,
                         error_fmt, key, "markup");
          atype = ARG_EDGE;
        }
      else if (! strcmp (key, "popup"))
        {
          cerror_if_not (annotation_is_markup (annotation), error_msg,
                         error_fmt, key, "markup");
          atype = ARG_EDGES;
        }
      else if (! strcmp (key, "popup-is-open"))
        {
          cerror_if_not (annotation_is_markup (annotation), error_msg,
                         error_fmt, key, "markup");
          atype = ARG_BOOL;
        }
      else if (! strcmp (key, "icon"))
        {
#ifdef HAVE_POPPLER_ANNOT_STAMP
          if (POPPLER_IS_ANNOT_STAMP (annotation))
            {
              PopplerAnnotStampIcon icon;
              cerror_if_not (xpoppler_annot_stamp_icon_parse (args[i], &icon),
                             error_msg, "Unknown stamp icon: %s", args[i]);
            }
          else
#endif
            cerror_if_not (POPPLER_IS_ANNOT_TEXT (annotation), error_msg,
                           error_fmt, key, "text or stamp");
          atype = ARG_STRING;
        }
#ifdef HAVE_POPPLER_ANNOT_SHAPES
      else if (! strcmp (key, "interior-color"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_SQUARE (annotation)
                         || POPPLER_IS_ANNOT_CIRCLE (annotation),
                         error_msg, error_fmt, key, "square and circle");
          /* An empty value removes the interior color. */
          atype = *args[i] ? ARG_COLOR : ARG_STRING;
        }
      else if (! strcmp (key, "line"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_LINE (annotation), error_msg,
                         error_fmt, key, "line");
          atype = ARG_EDGES;
        }
#endif
#ifdef HAVE_POPPLER_ANNOT_FREE_TEXT
      else if (! strcmp (key, "font-name"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_FREE_TEXT (annotation), error_msg,
                         error_fmt, key, "free text");
          atype = ARG_NONEMPTY_STRING;
        }
      else if (! strcmp (key, "font-size"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_FREE_TEXT (annotation), error_msg,
                         error_fmt, key, "free text");
          cerror_if_not (strtod (args[i], NULL) > 0, error_msg,
                         "Expected a positive font size: %s", args[i]);
          atype = ARG_DOUBLE;
        }
      else if (! strcmp (key, "font-color"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_FREE_TEXT (annotation), error_msg,
                         error_fmt, key, "free text");
          atype = ARG_COLOR;
        }
#endif
      else if (! strcmp (key, "is-open"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_TEXT (annotation), error_msg,
                         error_fmt, key, "text");
          atype = ARG_BOOL;
        }
      else
        {
          cerror_if_not (0, error_msg,
                         "Unable to modify property `%s'", key);
        }

      if (! command_arg_parse_arg (ctx, args[i++], &carg, atype, error_msg))
        return FALSE;
    }

  return TRUE;

 error:
  return FALSE;
}

static void
annotation_print (const annotation_t *annot, /* const */ PopplerPage *page)
{
  double width, height;
  PopplerAnnotMapping *m;
  const gchar *key;
  PopplerAnnot *a;
  PopplerAnnotMarkup *ma;
  PopplerAnnotText *ta;
  PopplerRectangle r;
  PopplerColor *color;
  gchar *text;
  gdouble opacity;
  cairo_region_t *region = NULL;
  GDate *date;

  if (! annot || ! page)
    return;

  m = annot->amap;
  key = annot->key;
  a = m->annot;
  poppler_page_get_size (page, &width, &height);

  r.x1 = m->area.x1;
  r.x2 = m->area.x2;
  r.y1 = height - m->area.y2;
  r.y2 = height - m->area.y1;

#ifdef HAVE_POPPLER_ANNOT_MARKUP
  if (POPPLER_IS_ANNOT_TEXT_MARKUP (a))
    {
      region = annotation_markup_get_text_regions (page, POPPLER_ANNOT_TEXT_MARKUP (a));
      perror_if_not (region, "%s", "Unable to extract annotation's text regions");
    }
#endif

  /* >>> Any Annotation >>> */
  /* Page */
  printf ("%d:", poppler_page_get_index (page) + 1);
  /* Area */
  printf ("%f %f %f %f:", r.x1 / width, r.y1 / height
          , r.x2 / width, r.y2 / height);

  /* Type */
  printf ("%s:", xpoppler_annot_type_string (poppler_annot_get_annot_type (a)));
  /* Internal Key */
  print_response_string (key, COLON);

  /* Flags */
  printf ("%d:", poppler_annot_get_flags (a));

  /* Color */
  color = poppler_annot_get_color (a);
  if (color)
    {
      /* Reduce 2 Byte to 1 Byte color space  */
      printf ("#%.2x%.2x%.2x", (color->red >> 8)
              , (color->green >> 8)
              , (color->blue >> 8));
      g_free (color);
    }

  putchar (':');

  /* Text Contents */
  text = poppler_annot_get_contents (a);
  print_response_string (text, COLON);
  g_free (text);

  /* Modified Date */
  text = poppler_annot_get_modified (a);
  print_response_string (text, NONE);
  g_free (text);

  /* <<< Any Annotation <<< */

#ifdef HAVE_POPPLER_ANNOT_STAMP
  /* >>> Stamp Annotation >>> */
  /* Older poppler-glib makes stamps plain annotations, newer (26.01)
     markup annotations.  Print the same fields either way. */
  if (POPPLER_IS_ANNOT_STAMP (a))
    {
      /* Stamp Icon */
      putchar (':');
      print_response_string (xpoppler_annot_stamp_icon_string
                             (poppler_annot_stamp_get_icon
                              (POPPLER_ANNOT_STAMP (a))), NEWLINE);
      goto theend;
    }
  /* <<< Stamp Annotation <<< */
#endif

  /* >>> Markup Annotation >>> */
  if (! POPPLER_IS_ANNOT_MARKUP (a))
    {
      putchar ('\n');
      goto theend;
    }

  putchar (':');
  ma = POPPLER_ANNOT_MARKUP (a);
  /* Label */
  text = poppler_annot_markup_get_label (ma);
  print_response_string (text, COLON);
  g_free (text);

  /* Subject */
  text = poppler_annot_markup_get_subject (ma);
  print_response_string (text, COLON);
  g_free (text);

  /* Opacity */
  opacity = poppler_annot_markup_get_opacity (ma);
  printf ("%f:", opacity);

  /* Popup (Area + isOpen) */
  if (poppler_annot_markup_has_popup (ma)
      && poppler_annot_markup_get_popup_rectangle (ma, &r))
    {
      gdouble tmp = r.y1;
      r.y1 = height - r.y2;
      r.y2 = height - tmp;
      printf ("%f %f %f %f:%d:", r.x1 / width, r.y1 / height
              , r.x2 / width, r.y2 / height
              , poppler_annot_markup_get_popup_is_open (ma) ? 1 : 0);

    }
  else
    printf ("::");

  /* Creation Date */
  date = poppler_annot_markup_get_date (ma);
  if (date != NULL && g_date_valid(date))
    {
      gchar datebuf[128];
      g_date_strftime (datebuf, 127, "%x", date);
      print_response_string (datebuf, NONE);
      g_date_free (date);
    }

  /* <<< Markup Annotation <<< */

  /* >>>  Text Annotation >>> */
  if (POPPLER_IS_ANNOT_TEXT (a))
    {
      putchar (':');
      ta = POPPLER_ANNOT_TEXT (a);
      /* Text Icon */
      text = poppler_annot_text_get_icon (ta);
      print_response_string (text, COLON);
      g_free (text);
      /* Text State */
      printf ("%s:%d",
              xpoppler_annot_text_state_string (poppler_annot_text_get_state (ta)),
              poppler_annot_text_get_is_open (ta));
    }
#ifdef HAVE_POPPLER_ANNOT_MARKUP
  /* <<< Text Annotation <<< */
  else if (POPPLER_IS_ANNOT_TEXT_MARKUP (a))
    {
      /* >>> Markup Text Annotation >>> */
      putchar (':');
      region_print (region, width, height);
      /* <<< Markup Text Annotation <<< */
    }
#endif
#ifdef HAVE_POPPLER_ANNOT_SHAPES
  else if (POPPLER_IS_ANNOT_SQUARE (a) || POPPLER_IS_ANNOT_CIRCLE (a))
    {
      /* >>> Square and Circle Annotation >>> */
      /* Interior Color */
      putchar (':');
      if (POPPLER_IS_ANNOT_SQUARE (a))
        color = poppler_annot_square_get_interior_color (POPPLER_ANNOT_SQUARE (a));
      else
        color = poppler_annot_circle_get_interior_color (POPPLER_ANNOT_CIRCLE (a));
      if (color)
        {
          printf ("#%.2x%.2x%.2x", (color->red >> 8)
                  , (color->green >> 8)
                  , (color->blue >> 8));
          g_free (color);
        }
      /* <<< Square and Circle Annotation <<< */
    }
#endif
#ifdef HAVE_POPPLER_ANNOT_FREE_TEXT
  else if (POPPLER_IS_ANNOT_FREE_TEXT (a))
    {
      /* >>> Free Text Annotation >>> */
      PopplerAnnotFreeText *fa = POPPLER_ANNOT_FREE_TEXT (a);
      PopplerFontDescription *font = poppler_annot_free_text_get_font_desc (fa);

      putchar (':');
      /* Font Name and Size */
      if (font)
        {
          print_response_string (font->font_name, COLON);
          printf ("%f:", font->size_pt);
          poppler_font_description_free (font);
        }
      else
        printf ("::");
      /* Font Color */
      color = poppler_annot_free_text_get_font_color (fa);
      if (color)
        {
          printf ("#%.2x%.2x%.2x", (color->red >> 8)
                  , (color->green >> 8)
                  , (color->blue >> 8));
          g_free (color);
        }
      /* <<< Free Text Annotation <<< */
    }
#endif
  putchar ('\n');
 theend:
#ifdef HAVE_POPPLER_ANNOT_MARKUP
 error:
#endif
  if (region) cairo_region_destroy (region);
}

static void
attachment_print (PopplerAttachment *att, const char *id, gboolean do_save)
{
  time_t time;

  print_response_string (id, COLON);
  print_response_string (att->name, COLON);
  print_response_string (att->description, COLON);
  if (att->size + 1 != 0)
    printf ("%" G_GSIZE_FORMAT ":", att->size);
  else
    printf ("-1:");
  time = (time_t) att->mtime;
  print_response_string (time > 0 ? strchomp (ctime (&time)) : "", COLON);
  time = (time_t) att->ctime;
  print_response_string (time > 0 ? strchomp (ctime (&time)) : "", COLON);
  print_response_string (att->checksum ? att->checksum->str : "" , COLON);
  if (do_save)
    {
      char *filename = mktempfile ();
      GError *error = NULL;
      if (filename)
        {
          if (! poppler_attachment_save (att, filename, &error))
            {
              fprintf (stderr, "Writing attachment failed: %s"
                       , error ? error->message : "reason unknown");
              if (error)
                g_free (error);
            }
          else
            {
              print_response_string (filename, NONE);
            }
          free (filename);
        }
    }
  putchar ('\n');
}



/* ================================================================== *
 * Server command implementations
 * ================================================================== */

/* Name: features
   Args: None
   Returns: A list of compile-time features.
   Errors: None
*/

const command_arg_type_t cmd_features_spec[] = {};

static void
cmd_features (const epdfinfo_t *ctx, const command_arg_t *args)
{
  const char *features[] = {
#ifdef HAVE_POPPLER_FIND_OPTS
    "case-sensitive-search",
#else
    "no-case-sensitive-search",
#endif
#ifdef HAVE_POPPLER_ANNOT_WRITE
    "writable-annotations",
#else
    "no-writable-annotations",
#endif
#ifdef HAVE_POPPLER_ANNOT_MARKUP
    "markup-annotations",
#else
    "no-markup-annotations",
#endif
#ifdef HAVE_POPPLER_METADATA_WRITE
    "writable-metadata",
#else
    "no-writable-metadata",
#endif
    "form-fields",
#ifdef HAVE_POPPLER_FORM_RESET
    "form-reset",
#else
    "no-form-reset",
#endif
#ifdef HAVE_POPPLER_SIGNATURES
    "signatures",
#else
    "no-signatures",
#endif
#ifdef HAVE_POPPLER_STRUCTURE
    "structure-tree",
#else
    "no-structure-tree",
#endif
#ifdef HAVE_POPPLER_ANNOT_SHAPES
    "shape-annotations",
#else
    "no-shape-annotations",
#endif
#ifdef HAVE_POPPLER_ANNOT_STAMP
    "stamp-annotations",
#else
    "no-stamp-annotations",
#endif
#if defined (HAVE_POPPLER_ANNOT_STAMP) && CAIRO_HAS_PNG_FUNCTIONS
    "image-stamps",
#else
    "no-image-stamps",
#endif
#ifdef HAVE_POPPLER_ANNOT_FREE_TEXT
    "free-text-annotations",
#else
    "no-free-text-annotations",
#endif
#ifdef HAVE_POPPLER_ANNOT_INK
    "ink-annotations",
#else
    "no-ink-annotations",
#endif
#ifdef HAVE_POPPLER_SIGNING
    "signing",
#else
    "no-signing",
#endif
    /* Optional content groups, available since poppler-glib 0.12. */
    "layers"
  };
  int i;
  OK_BEGIN ();
  for (i = 0; i < G_N_ELEMENTS (features); ++i)
    {
      printf ("%s", features[i]);
      if (i < G_N_ELEMENTS (features) - 1)
        putchar (':');
    }
  putchar ('\n');
  OK_END ();
}


/* Name: open
   Args: filename password
   Returns: Nothing
   Errors: If file can't be opened or is not a PDF document.
*/

const command_arg_type_t cmd_open_spec[] =
  {
    ARG_NONEMPTY_STRING,        /* filename */
    ARG_STRING,                 /* password */
  };

static void
cmd_open (const epdfinfo_t *ctx, const command_arg_t *args)
{
  const char *filename = args[0].value.string;
  const char *passwd = args[1].value.string;
  GError *gerror = NULL;
  document_t *doc;

  if (! *passwd)
    passwd = NULL;

  doc = document_open(ctx, filename, passwd, &gerror);
  perror_if_not (doc, "Error opening %s:%s", filename,
                gerror ? gerror->message : "unknown error");
  OK ();

 error:
  if (gerror)
    {
      g_error_free (gerror);
      gerror = NULL;
    }
}

/* Name: close
   Args: filename
   Returns: 1 if file was open, otherwise 0.
   Errors: None
*/

const command_arg_type_t cmd_close_spec[] =
  {
    ARG_NONEMPTY_STRING         /* filename */
  };

static void
cmd_close (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = g_hash_table_lookup(ctx->documents, args->value.string);

  g_hash_table_remove (ctx->documents, args->value.string);
  free_document (doc);
  OK_BEGIN ();
  puts (doc ? "1" : "0");
  OK_END ();
}

/* Name: closeall
   Args: None
   Returns: Nothing
   Errors: None
*/
static void
cmd_closeall (const epdfinfo_t *ctx, const command_arg_t *args)
{
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init (&iter, ctx->documents);
  while (g_hash_table_iter_next (&iter, &key, &value))
    {
      document_t *doc = (document_t*) value;
      free_document (doc);
      g_hash_table_iter_remove (&iter);
    }
  OK ();
}


const command_arg_type_t cmd_search_regexp_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* first page */
    ARG_NATNUM,                 /* last page */
    ARG_NONEMPTY_STRING,        /* regexp */
    ARG_NATNUM,                 /* compile flags */
    ARG_NATNUM                  /* match flags */
  };

static void
cmd_search_regexp(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int first = args[1].value.natnum;
  int last = args[2].value.natnum;
  const gchar *regexp = args[3].value.string;
  GRegexCompileFlags cflags = args[4].value.natnum;
  GRegexMatchFlags mflags = args[5].value.natnum;
  double width, height;
  int pn;
  GError *gerror = NULL;
  GRegex *re = NULL;

  NORMALIZE_PAGE_ARG (doc, &first, &last);

  re = g_regex_new (regexp, cflags, mflags, &gerror);
  perror_if_not (NULL == gerror, "Invalid regexp: %s", gerror->message);

  OK_BEGIN ();
  for (pn = first; pn <= last; ++pn)
    {
      PopplerPage *page = poppler_document_get_page(doc, pn - 1);
      char *text;
      PopplerRectangle *rectangles = NULL;
      guint nrectangles;
      GMatchInfo *match = NULL;

      if (! page)
        continue;

      text = poppler_page_get_text (page);
      poppler_page_get_text_layout (page, &rectangles, &nrectangles);
      poppler_page_get_size (page, &width, &height);
      g_regex_match (re, text, 0, &match);

      while (g_match_info_matches (match))
        {
          const double scale = 100.0;
          gint start, end, ustart, ulen;
          gchar *string = NULL;
          gchar *line = NULL;
          int i;

          /* Does this ever happen ? */
          if (! g_match_info_fetch_pos (match, 0, &start, &end))
            continue;

          string = g_match_info_fetch (match, 0);
          ustart = g_utf8_strlen (text, start);
          ulen = g_utf8_strlen (string, -1);

          cairo_region_t *region = cairo_region_create ();
          /* Merge matched glyph rectangles. Scale them so we're able
             to use cairo . */
          if (ulen > 0)
            {
              assert (ustart < nrectangles
                      && ustart + ulen <= nrectangles);
              line = poppler_page_get_selected_text
                     (page, POPPLER_SELECTION_LINE, rectangles + ustart);

              for (i = ustart; i < ustart + ulen; ++i)
                {
                  PopplerRectangle *r = rectangles + i;
                  cairo_rectangle_int_t c;

                  c.x = (int) (scale * r->x1 + 0.5);
                  c.y = (int) (scale * r->y1 + 0.5);
                  c.width = (int) (scale * (r->x2 - r->x1) + 0.5);
                  c.height = (int) (scale * (r->y2 - r->y1) + 0.5);

                  cairo_region_union_rectangle (region, &c);
                }

            }

          printf ("%d:", pn);
          print_response_string (string, COLON);
          print_response_string (strchomp (line), COLON);
          region_print (region, width * scale, height * scale);
          putchar ('\n');
          cairo_region_destroy (region);
          g_free (string);
          g_free (line);
          g_match_info_next (match, NULL);
        }
      g_free (rectangles);
      g_object_unref (page);
      g_free (text);
      g_match_info_free (match);
    }
  OK_END ();

 error:
  if (re) g_regex_unref (re);
  if (gerror) g_error_free (gerror);
}

const command_arg_type_t cmd_regexp_flags_spec[] =
  {
  };

static void
cmd_regexp_flags (const epdfinfo_t *ctx, const command_arg_t *args)
{
  OK_BEGIN ();
  printf ("caseless:%d\n", G_REGEX_CASELESS);
  printf ("multiline:%d\n", G_REGEX_MULTILINE);
  printf ("dotall:%d\n", G_REGEX_DOTALL);
  printf ("extended:%d\n", G_REGEX_EXTENDED);
  printf ("anchored:%d\n", G_REGEX_ANCHORED);
  printf ("dollar-endonly:%d\n", G_REGEX_DOLLAR_ENDONLY);
  printf ("ungreedy:%d\n", G_REGEX_UNGREEDY);
  printf ("raw:%d\n", G_REGEX_RAW);
  printf ("no-auto-capture:%d\n", G_REGEX_NO_AUTO_CAPTURE);
  printf ("optimize:%d\n", G_REGEX_OPTIMIZE);
  printf ("dupnames:%d\n", G_REGEX_DUPNAMES);
  printf ("newline-cr:%d\n", G_REGEX_NEWLINE_CR);
  printf ("newline-lf:%d\n", G_REGEX_NEWLINE_LF);
  printf ("newline-crlf:%d\n", G_REGEX_NEWLINE_CRLF);

  printf ("match-anchored:%d\n", G_REGEX_MATCH_ANCHORED);
  printf ("match-notbol:%d\n", G_REGEX_MATCH_NOTBOL);
  printf ("match-noteol:%d\n", G_REGEX_MATCH_NOTEOL);
  printf ("match-notempty:%d\n", G_REGEX_MATCH_NOTEMPTY);
  printf ("match-partial:%d\n", G_REGEX_MATCH_PARTIAL);
  printf ("match-newline-cr:%d\n", G_REGEX_MATCH_NEWLINE_CR);
  printf ("match-newline-lf:%d\n", G_REGEX_MATCH_NEWLINE_LF);
  printf ("match-newline-crlf:%d\n", G_REGEX_MATCH_NEWLINE_CRLF);
  printf ("match-newline-any:%d\n", G_REGEX_MATCH_NEWLINE_ANY);

  OK_END ();
}


const command_arg_type_t cmd_search_string_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* first page */
    ARG_NATNUM,                 /* last page */
    ARG_NONEMPTY_STRING,        /* search string */
    ARG_BOOL,                   /* ignore-case */
  };

static void
cmd_search_string(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int first = args[1].value.natnum;
  int last = args[2].value.natnum;
  const char *string = args[3].value.string;
  gboolean ignore_case = args[4].value.flag;
  GList *list, *item;
  double width, height;
  int pn;
#ifdef HAVE_POPPLER_FIND_OPTS
  PopplerFindFlags flags = ignore_case ? 0 : POPPLER_FIND_CASE_SENSITIVE;
#endif

  NORMALIZE_PAGE_ARG (doc, &first, &last);
  OK_BEGIN ();
  for (pn = first; pn <= last; ++pn)
    {
      PopplerPage *page = poppler_document_get_page(doc, pn - 1);

      if (! page)
        continue;

#ifdef HAVE_POPPLER_FIND_OPTS
      list = poppler_page_find_text_with_options(page, string, flags);
#else
      list = poppler_page_find_text(page, string);
#endif

      poppler_page_get_size (page, &width, &height);

      for (item = list; item; item = item->next)
        {
          gchar *line, *match;
          PopplerRectangle *r = item->data;
          gdouble y1 =  r->y1;

          r->y1 = height - r->y2;
          r->y2 = height - y1;

          printf ("%d:", pn);
          line = strchomp (poppler_page_get_selected_text
                           (page, POPPLER_SELECTION_LINE, r));
          match = strchomp (poppler_page_get_selected_text
                           (page, POPPLER_SELECTION_GLYPH, r));
          print_response_string (match, COLON);
          print_response_string (line, COLON);
          printf ("%f %f %f %f\n",
                  r->x1 / width, r->y1 / height,
                  r->x2 / width, r->y2 / height);
          g_free (line);
          g_free (match);
          poppler_rectangle_free (r);
        }
      g_list_free (list);
      g_object_unref (page);
    }
  OK_END ();
}

/* Name: metadata
   Args: filename
   Returns: PDF's metadata
   Errors: None

   title author subject keywords creator producer pdf-version create-date mod-date

   Dates are in seconds since the epoche.

*/

const command_arg_type_t cmd_metadata_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_metadata (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  time_t date;
  gchar *md[6];
  gchar *title;
  int i;
  char *time_str;

  OK_BEGIN ();

  title = poppler_document_get_title (doc);
  print_response_string (title, COLON);
  g_free (title);

  md[0] = poppler_document_get_author (doc);
  md[1] = poppler_document_get_subject (doc);
  md[2] = poppler_document_get_keywords (doc);
  md[3] = poppler_document_get_creator (doc);
  md[4] = poppler_document_get_producer (doc);
  md[5] = poppler_document_get_pdf_version_string (doc);

  for (i = 0; i < 6; ++i)
    {
      print_response_string (md[i], COLON);
      g_free (md[i]);
    }

  date = poppler_document_get_creation_date (doc);
  time_str = strchomp (ctime (&date));
  print_response_string (time_str ? time_str : "", COLON);
  date = poppler_document_get_modification_date (doc);
  time_str = strchomp (ctime (&date));
  print_response_string (time_str ? time_str : "", NEWLINE);
  OK_END ();
}

#ifdef HAVE_POPPLER_METADATA_WRITE

/* Name: setmetadata
   Args: filename key value [key value ...]
   Returns: The document's metadata, see cmd_metadata.
   Errors: If a key is unknown or a value is not valid UTF-8.

   Valid keys are title, author, subject, keywords and creator.  An
   empty value removes the entry from the document's information
   dictionary.  All pairs are validated before any of them is
   applied.

   This changes the information dictionary only.  Poppler does not
   update the XMP metadata stream, if the document has one (see
   cmd_xmpmetadata).
*/

typedef void (*metadata_setter_t) (PopplerDocument *, const gchar *);

static const struct
{
  const char *name;
  metadata_setter_t setter;
} metadata_setters[] =
  {
    {"title", poppler_document_set_title},
    {"author", poppler_document_set_author},
    {"subject", poppler_document_set_subject},
    {"keywords", poppler_document_set_keywords},
    {"creator", poppler_document_set_creator},
  };

const command_arg_type_t cmd_setmetadata_spec[] =
  {
    ARG_DOC,
    ARG_REST                    /* key value pairs */
  };

static void
cmd_setmetadata (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int nrest = args[1].value.rest.nargs;
  char * const *rest = args[1].value.rest.args;
  const size_t nsetters = G_N_ELEMENTS (metadata_setters);
  metadata_setter_t *setters = NULL;
  int i;

  perror_if_not (nrest > 0, "Expected at least one key/value pair");
  perror_if_not (nrest % 2 == 0, "Even number of key/value pairs expected");

  setters = g_new0 (metadata_setter_t, nrest / 2);
  for (i = 0; i < nrest; i += 2)
    {
      int j;

      for (j = 0; j < nsetters; ++j)
        if (! strcmp (rest[i], metadata_setters[j].name))
          {
            setters[i / 2] = metadata_setters[j].setter;
            break;
          }
      perror_if_not (j < nsetters, "Unknown metadata key: %s", rest[i]);
      perror_if_not (g_utf8_validate (rest[i + 1], -1, NULL),
                     "Value of %s is not valid UTF-8", rest[i]);
    }

  for (i = 0; i < nrest; i += 2)
    setters[i / 2] (doc, rest[i + 1]);

  cmd_metadata (ctx, args);

 error:
  g_free (setters);
}

#endif  /* HAVE_POPPLER_METADATA_WRITE */

/* Name: outline
   Args: filename

   Returns: The documents outline (or index) as a, possibly empty,
   list of records:

   tree-level ACTION

   See cmd_pagelinks for how ACTION is constructed.

   Errors: None
*/

static void
cmd_outline_walk (PopplerDocument *doc, PopplerIndexIter *iter, int depth)
{
  do
    {
      PopplerIndexIter *child;
      PopplerAction *action = poppler_index_iter_get_action (iter);

      if (! action)
        continue;

      if (action_is_handled (action))
        {
          printf ("%d:", depth);
          action_print (doc, action);
        }

      child = poppler_index_iter_get_child (iter);
      if (child)
        {
          cmd_outline_walk (doc, child, depth + 1);
        }
      poppler_action_free (action);
      poppler_index_iter_free (child);
    } while (poppler_index_iter_next (iter));
}

const command_arg_type_t cmd_outline_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_outline (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerIndexIter *iter = poppler_index_iter_new (args->value.doc->pdf);
  OK_BEGIN ();
  if (iter)
    {
      cmd_outline_walk (args->value.doc->pdf, iter, 1);
      poppler_index_iter_free (iter);
    }
  OK_END ();
}

#ifdef HAVE_POPPLER_STRUCTURE
/* Name: structure
   Args: filename [full]
   Returns: The document's logical structure tree (tagged PDF) in
   document (i.e. reading) order, as a, possibly empty, list of
   records:

   depth kind page edges has-alt alt actual-text language text

   or, if FULL is given and not 0,

   depth kind page edges has-alt alt actual-text language text
   list-numbering

   DEPTH is the level in the tree (>= 1).  KIND is the element's
   kind as a lower-case name (e.g. document, heading-1, paragraph,
   figure).  PAGE is the element's page, or 0 if neither it nor any
   of its descendants is associated with a page.  EDGES are the
   relative edges of the element's BBox attribute, or empty if it has
   none.  poppler-glib reads BBox only for block-level elements (e.g.
   tables), not for figures or formulas, so these never have edges.
   HAS-ALT is 1 if the element has an /Alt entry (possibly empty) and
   0 otherwise; ALT is its value.  TEXT is a short excerpt of the
   element's text; it is empty for grouping elements (document, part,
   section, ...).

   With FULL, TEXT is the element's whole text instead, for reading
   it: the text of descendants with an /ActualText is replaced by
   it, and the text of figures, formulas and forms nested in the
   element is left out (a figure's own TEXT is still its text).
   Whitespace is folded in both cases.  LIST-NUMBERING is the
   numbering of the list a list item is in (none, disc, circle,
   square, decimal, upper-roman, lower-roman, upper-alpha or
   lower-alpha) and empty for other elements.  FULL reads each piece of content once, while without it the text
   of nested elements is read again for each of them.

   The list is empty if the document has no structure tree, i.e. it
   is not tagged.

   Errors: None
*/

/* Maximum number of characters of an element's text to return. */
#define STRUCTURE_TEXT_MAX 120

/* Return the 1-based page of the first element in ITER's subtree
   (including ITER's own element) that has one, or 0. */
static int
structure_iter_page (PopplerStructureElementIter *iter)
{
  int page = 0;

  do
    {
      PopplerStructureElement *elt
        = poppler_structure_element_iter_get_element (iter);
      PopplerStructureElementIter *child;

      if (! elt)
        continue;
      page = poppler_structure_element_get_page (elt) + 1;
      g_object_unref (elt);
      if (page > 0)
        break;
      child = poppler_structure_element_iter_get_child (iter);
      if (child)
        {
          page = structure_iter_page (child);
          poppler_structure_element_iter_free (child);
          if (page > 0)
            break;
        }
    } while (poppler_structure_element_iter_next (iter));

  return page > 0 ? page : 0;
}

/* Return the nick of KIND's enum value, e.g. "heading-1". */
static const char *
structure_kind_string (PopplerStructureElementKind kind)
{
  static GEnumClass *class = NULL;
  GEnumValue *value;

  if (! class)
    class = g_type_class_ref (POPPLER_TYPE_STRUCTURE_ELEMENT_KIND);
  value = g_enum_get_value (class, kind);
  return value ? value->value_nick : "unknown";
}

/* Return the structure text of ELT, with its subtree's if RECURSIVE.

   poppler-glib's text of marked content is UTF-8, but it converts it
   to UTF-8 again as if it were PDFDocEncoding, so that e.g. a bullet
   comes out as three characters.  Undo that: map each character back
   to its PDFDocEncoding byte and use the result if it is valid UTF-8.
   Three bytes (0x7F, 0x9F and 0xAD) all map to U+FFFD; it is taken
   as the one that makes a valid sequence, 0x9F when both 0x9F and
   0xAD would.  Return the text unchanged if it cannot be undone,
   e.g. because a future poppler returns it right.

   FIXME: This guess is wrong for every character whose UTF-8 has
   0xAD as a continuation byte, e.g. í (C3 AD) comes out as ß (C3 9F)
   and Э (D0 AD) as П (D0 9F).  Read the text from the page text
   instead, as pdf-reflow already does to find an element's area. */
static gchar *
structure_element_text (PopplerStructureElement *elt, gboolean recursive)
{
  /* The characters of the PDFDocEncoding bytes 0x18 to 0x1F and 0x80
     to 0xA0 that are not the same as in Latin-1. */
  static const gunichar pdfdoc_18[8] =
    { 0x02d8, 0x02c7, 0x02c6, 0x02d9, 0x02dd, 0x02db, 0x02da, 0x02dc };
  static const gunichar pdfdoc_80[33] =
    { 0x2022, 0x2020, 0x2021, 0x2026, 0x2014, 0x2013, 0x0192, 0x2044,
      0x2039, 0x203a, 0x2212, 0x2030, 0x201e, 0x201c, 0x201d, 0x2018,
      0x2019, 0x201a, 0x2122, 0xfb01, 0xfb02, 0x0141, 0x0152, 0x0160,
      0x0178, 0x017d, 0x0131, 0x0142, 0x0153, 0x0161, 0x017e, 0xfffd,
      0x20ac };
  gchar *text = poppler_structure_element_get_text
    (elt, recursive ? POPPLER_STRUCTURE_GET_TEXT_RECURSIVE : 0);
  GString *bytes;
  const gchar *p;
  /* The number of continuation bytes still expected, and the lead
     byte if the next one is the second byte of a sequence. */
  int need = 0;
  guchar lead = 0;

  if (! text || ! g_utf8_validate (text, -1, NULL))
    {
      g_free (text);
      return NULL;
    }

  bytes = g_string_sized_new (strlen (text));
  for (p = text; *p; p = g_utf8_next_char (p))
    {
      gunichar c = g_utf8_get_char (p);
      int b = -1, i;

      if (c == 0xfffd)
        {
          if (need == 0)
            b = 0x7f;
          else if (lead == 0xe0)
            b = 0xad;           /* 0xE0 needs 0xA0 to 0xBF. */
          else
            b = 0x9f;
        }
      else if (c < 0x18 || (c >= 0x20 && c < 0x7f)
               || (c >= 0xa1 && c <= 0xff && c != 0xad))
        b = c;
      else
        {
          for (i = 0; i < 8 && b < 0; ++i)
            if (pdfdoc_18[i] == c)
              b = 0x18 + i;
          for (i = 0; i < 33 && b < 0; ++i)
            if (pdfdoc_80[i] == c)
              b = 0x80 + i;
        }
      if (b < 0)
        {
          g_string_free (bytes, TRUE);
          return text;
        }

      if (need > 0 && (b & 0xc0) == 0x80)
        --need;
      else if ((b & 0xe0) == 0xc0)
        need = 1;
      else if ((b & 0xf0) == 0xe0)
        need = 2;
      else if ((b & 0xf8) == 0xf0)
        need = 3;
      else
        need = 0;
      lead = ((b & 0xc0) == 0xc0) ? b : 0;
      g_string_append_c (bytes, b);
    }

  if (! g_utf8_validate (bytes->str, bytes->len, NULL))
    {
      g_string_free (bytes, TRUE);
      return text;
    }
  g_free (text);
  return g_string_free (bytes, FALSE);
}

/* Truncate STR to at most MAX characters and fold all whitespace
   runs into one space, in place.  STR has to be valid UTF-8. */
static char *
structure_squeeze_text (char *str, glong max)
{
  char *in = str, *out = str;
  glong n = 0;
  gboolean space = FALSE;

  while (*in && n < max)
    {
      gunichar c = g_utf8_get_char (in);
      char *next = g_utf8_next_char (in);

      if (g_unichar_isspace (c))
        {
          space = TRUE;
        }
      else
        {
          if (space && out != str)
            {
              *out++ = ' ';
              ++n;
            }
          space = FALSE;
          memmove (out, in, next - in);
          out += next - in;
          ++n;
        }
      in = next;
    }
  *out = '\0';
  return str;
}

/* Print the fields of structure element ELT, at DEPTH and on page
   PN, up to its edges. */
static void
structure_print_location (PopplerDocument *doc, PopplerStructureElement *elt,
                          int depth, int pn)
{
  PopplerRectangle bbox;

  printf ("%d:", depth);
  print_response_string (structure_kind_string
                         (poppler_structure_element_get_kind (elt)),
                         COLON);
  printf ("%d:", pn > 0 ? pn : 0);

  /* poppler-glib only reads the BBox of block-level elements;
     figures, formulas and other illustrations are not, and so
     carry no edges. */
  if (pn > 0
      && poppler_structure_element_is_block (elt)
      && poppler_structure_element_get_bounding_box (elt, &bbox))
    {
      PopplerPage *page = poppler_document_get_page (doc, pn - 1);
      double width, height;

      if (page)
        {
          poppler_page_get_size (page, &width, &height);
          /* BBox is in default user space, origin at the
             bottom-left. */
          printf ("%f %f %f %f",
                  MIN (bbox.x1, bbox.x2) / width,
                  (height - MAX (bbox.y1, bbox.y2)) / height,
                  MAX (bbox.x1, bbox.x2) / width,
                  (height - MIN (bbox.y1, bbox.y2)) / height);
          g_object_unref (page);
        }
    }
  putchar (':');
}

/* Print ELT's alternate description, replacement text and language,
   each followed by a colon. */
static void
structure_print_alternates (PopplerStructureElement *elt)
{
  gchar *alt, *actual, *lang;

  alt = poppler_structure_element_get_alt_text (elt);
  printf ("%d:", alt != NULL);
  print_response_string (alt, COLON);
  actual = poppler_structure_element_get_actual_text (elt);
  print_response_string (actual, COLON);
  lang = poppler_structure_element_get_language (elt);
  print_response_string (lang, COLON);
  g_free (alt);
  g_free (actual);
  g_free (lang);
}

static void
cmd_structure_walk (PopplerDocument *doc, PopplerStructureElementIter *iter,
                    int depth)
{
  do
    {
      PopplerStructureElement *elt
        = poppler_structure_element_iter_get_element (iter);
      PopplerStructureElementIter *child;
      gchar *text = NULL;
      int pn;

      if (! elt)
        continue;
      /* Marked content and object references are leaves holding the
         actual page content, not structure. */
      if (poppler_structure_element_is_content (elt)
          || poppler_structure_element_get_kind (elt)
          == POPPLER_STRUCTURE_ELEMENT_OBJECT_REFERENCE)
        {
          g_object_unref (elt);
          continue;
        }

      child = poppler_structure_element_iter_get_child (iter);
      pn = poppler_structure_element_get_page (elt) + 1;
      if (pn <= 0 && child)
        {
          PopplerStructureElementIter *copy
            = poppler_structure_element_iter_copy (child);
          pn = structure_iter_page (copy);
          poppler_structure_element_iter_free (copy);
        }

      structure_print_location (doc, elt, depth, pn);
      structure_print_alternates (elt);
      if (! poppler_structure_element_is_grouping (elt))
        text = structure_element_text (elt, TRUE);
      if (text)
        print_response_string (structure_squeeze_text
                               (text, STRUCTURE_TEXT_MAX), NEWLINE);
      else
        putchar ('\n');

      g_free (text);
      g_object_unref (elt);

      if (child)
        {
          cmd_structure_walk (doc, child, depth + 1);
          poppler_structure_element_iter_free (child);
        }
    } while (poppler_structure_element_iter_next (iter));
}

/* A structure element read with its subtree, for the full structure
   command. */
typedef struct structure_node
{
  PopplerStructureElement *elt;
  /* The 1-based page of the element or of its first descendant
     with one, or 0. */
  int page;
  /* The text of the subtree, see structure_read_tree. */
  GString *text;
  GPtrArray *children;
} structure_node_t;

static void
structure_node_free (gpointer data)
{
  structure_node_t *node = data;

  if (node->elt)
    g_object_unref (node->elt);
  g_string_free (node->text, TRUE);
  g_ptr_array_free (node->children, TRUE);
  g_free (node);
}

static structure_node_t *
structure_node_new (PopplerStructureElement *elt)
{
  structure_node_t *node = g_new0 (structure_node_t, 1);

  node->elt = elt;
  node->text = g_string_new (NULL);
  node->children = g_ptr_array_new_with_free_func (structure_node_free);
  return node;
}

static gboolean
structure_is_illustration (PopplerStructureElement *elt)
{
  switch (poppler_structure_element_get_kind (elt))
    {
    case POPPLER_STRUCTURE_ELEMENT_FIGURE:
    case POPPLER_STRUCTURE_ELEMENT_FORMULA:
    case POPPLER_STRUCTURE_ELEMENT_FORM:
      return TRUE;
    default:
      return FALSE;
    }
}

/* Append TEXT to STRING, separated by a space if SEPARATE. */
static void
structure_append_text (GString *string, const char *text, gboolean separate)
{
  if (! text || ! *text)
    return;
  if (separate && string->len > 0
      && ! g_ascii_isspace (string->str[string->len - 1]))
    g_string_append_c (string, ' ');
  g_string_append (string, text);
}

/* Read the elements of ITER and its siblings as the children of
   PARENT.  Each piece of marked content is read once, and its text
   appended to that of its parent; the text of an element is that of
   its subtree, or its /ActualText if it has one.  Figures, formulas
   and forms do not add their text to their parent; their content is
   not read as text.  Block-level elements and table cells are
   separated from what precedes them by a space. */
static void
structure_read_tree (PopplerStructureElementIter *iter,
                     structure_node_t *parent)
{
  do
    {
      PopplerStructureElement *elt
        = poppler_structure_element_iter_get_element (iter);
      PopplerStructureElementIter *child;
      structure_node_t *node;
      gchar *actual;
      int pn;

      if (! elt)
        continue;
      pn = MAX (poppler_structure_element_get_page (elt) + 1, 0);
      if (poppler_structure_element_get_kind (elt)
          == POPPLER_STRUCTURE_ELEMENT_OBJECT_REFERENCE)
        {
          g_object_unref (elt);
          continue;
        }
      if (poppler_structure_element_is_content (elt))
        {
          gchar *text = structure_element_text (elt, FALSE);

          structure_append_text (parent->text, text, FALSE);
          g_free (text);
          g_object_unref (elt);
          if (parent->page == 0)
            parent->page = pn;
          continue;
        }

      node = structure_node_new (elt);
      node->page = pn;
      child = poppler_structure_element_iter_get_child (iter);
      if (child)
        {
          structure_read_tree (child, node);
          poppler_structure_element_iter_free (child);
        }
      actual = poppler_structure_element_get_actual_text (elt);
      if (actual && *actual && g_utf8_validate (actual, -1, NULL))
        g_string_assign (node->text, actual);
      g_free (actual);

      if (! structure_is_illustration (elt))
        {
          PopplerStructureElementKind kind
            = poppler_structure_element_get_kind (elt);

          structure_append_text
            (parent->text, node->text->str,
             ! poppler_structure_element_is_inline (elt)
             || kind == POPPLER_STRUCTURE_ELEMENT_TABLE_HEADING
             || kind == POPPLER_STRUCTURE_ELEMENT_TABLE_DATA);
        }
      if (parent->page == 0)
        parent->page = node->page;
      g_ptr_array_add (parent->children, node);
    } while (poppler_structure_element_iter_next (iter));
}

/* Print the nodes in CHILDREN and their subtrees at DEPTH, as records
   of the full structure command. */
static void
structure_print_tree (PopplerDocument *doc, GPtrArray *children, int depth)
{
  static GEnumClass *numberings = NULL;
  guint i;

  if (! numberings)
    numberings = g_type_class_ref (POPPLER_TYPE_STRUCTURE_LIST_NUMBERING);

  for (i = 0; i < children->len; ++i)
    {
      structure_node_t *node = g_ptr_array_index (children, i);
      PopplerStructureElement *elt = node->elt;
      PopplerStructureElementKind kind
        = poppler_structure_element_get_kind (elt);

      structure_print_location (doc, elt, depth, node->page);
      structure_print_alternates (elt);
      if (! poppler_structure_element_is_grouping (elt))
        print_response_string (structure_squeeze_text
                               (node->text->str, G_MAXLONG), COLON);
      else
        putchar (':');

      /* ListNumbering is an attribute of the list, but poppler-glib
         (up to 26.09 at least) only reads it for a list item, which
         inherits it.  It reads the row and column span of table
         cells only for tables, so these are left out. */
      if (kind == POPPLER_STRUCTURE_ELEMENT_LIST_ITEM)
        {
          GEnumValue *value = g_enum_get_value
            (numberings, poppler_structure_element_get_list_numbering (elt));
          print_response_string (value ? value->value_nick : "", NEWLINE);
        }
      else
        putchar ('\n');

      structure_print_tree (doc, node->children, depth + 1);
    }
}

const command_arg_type_t cmd_structure_spec[] =
  {
    ARG_DOC,
    ARG_REST,                   /* [full] */
  };

static void
cmd_structure (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args->value.doc->pdf;
  const command_arg_t *rest = &args[1];
  PopplerStructureElementIter *iter;
  gboolean full = FALSE;

  perror_if_not (rest->value.rest.nargs <= 1, "%s", "Too many arguments");
  if (rest->value.rest.nargs == 1)
    {
      const char *flag = rest->value.rest.args[0];

      perror_if_not (! strcmp (flag, "0") || ! strcmp (flag, "1"),
                     "Expected 0 or 1:%s", flag);
      full = *flag == '1';
    }

  iter = poppler_structure_element_iter_new (doc);
  OK_BEGIN ();
  if (iter && full)
    {
      structure_node_t *root = structure_node_new (NULL);

      structure_read_tree (iter, root);
      structure_print_tree (doc, root->children, 1);
      structure_node_free (root);
    }
  else if (iter)
    cmd_structure_walk (doc, iter, 1);
  OK_END ();
  if (iter)
    poppler_structure_element_iter_free (iter);

 error:
  return;
}
#endif  /* HAVE_POPPLER_STRUCTURE */

/* Name: fonts
   Args: filename
   Returns: The fonts used in the document as a, possibly empty, list
   of records:

   name type embedded subset substitute

   NAME is the font's name as given in the PDF, TYPE its type as a
   lower-case name (e.g. type1, truetype, cid-type0c).  EMBEDDED and
   SUBSET are 1 or 0.  SUBSTITUTE is the name of the font used in
   place of a font that is not embedded, if any.

   Errors: None
*/

const command_arg_type_t cmd_fonts_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_fonts (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args->value.doc->pdf;
  PopplerFontInfo *info = poppler_font_info_new (doc);
  PopplerFontsIter *iter = NULL;
  GEnumClass *types = g_type_class_ref (POPPLER_TYPE_FONT_TYPE);
  int npages = poppler_document_get_n_pages (doc);

  OK_BEGIN ();
  /* Scanning all pages in one go returns every font once. */
  if (npages > 0 && poppler_font_info_scan (info, npages, &iter) && iter)
    {
      do
        {
          GEnumValue *type = g_enum_get_value
            (types, poppler_fonts_iter_get_font_type (iter));

          print_response_string (poppler_fonts_iter_get_name (iter), COLON);
          print_response_string (type ? type->value_nick : "unknown", COLON);
          printf ("%d:%d:",
                  poppler_fonts_iter_is_embedded (iter) ? 1 : 0,
                  poppler_fonts_iter_is_subset (iter) ? 1 : 0);
          print_response_string (poppler_fonts_iter_get_substitute_name (iter),
                                 NEWLINE);
        } while (poppler_fonts_iter_next (iter));
      poppler_fonts_iter_free (iter);
    }
  OK_END ();

  g_type_class_unref (types);
  poppler_font_info_free (info);
}

/* Name: xmpmetadata
   Args: filename
   Returns: The document's XMP metadata packet, or an empty string if
   it has none.
   Errors: None
*/

const command_arg_type_t cmd_xmpmetadata_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_xmpmetadata (const epdfinfo_t *ctx, const command_arg_t *args)
{
  gchar *xmp = poppler_document_get_metadata (args->value.doc->pdf);

  OK_BEGIN ();
  print_response_string (xmp, NEWLINE);
  OK_END ();
  g_free (xmp);
}

/* Name: layers
   Args: filename
   Returns: The document's optional content groups (layers), in the
   order of its layer panel, as a, possibly empty, list of records:

   depth id visible radio-group title

   DEPTH is the level in the tree (>= 1); a layer's children follow
   it with a larger depth.  ID is the layer's 1-based number among
   all layers in this order, as accepted by setlayer, or 0 if the
   entry is only a label grouping its children.  VISIBLE is 1 or 0,
   and 0 for labels.  RADIO-GROUP is 0, or a positive number shared
   by all layers of the same radio button group (showing one of them
   hides the others).  TITLE is the layer's or label's name.

   The visibility of layers is a property of the open document, not
   of the file: it is lost when the document is closed and it is not
   saved.

   Errors: None
*/

/* Return the 1-based number of the radio button group of LAYER, or
   0 if it belongs to none.  poppler-glib's group ids are truncated
   pointers, so map them to small numbers in order of appearance,
   using GROUPS. */
static int
layer_radio_group (PopplerLayer *layer, GArray *groups)
{
  gint id = poppler_layer_get_radio_button_group_id (layer);
  guint i;

  if (! id)
    return 0;
  for (i = 0; i < groups->len; ++i)
    if (g_array_index (groups, gint, i) == id)
      return i + 1;
  g_array_append_val (groups, id);
  return groups->len;
}

static void
layers_print_walk (PopplerLayersIter *iter, int depth, int *n,
                   GArray *groups)
{
  do
    {
      PopplerLayer *layer = poppler_layers_iter_get_layer (iter);
      PopplerLayersIter *child = poppler_layers_iter_get_child (iter);

      if (layer)
        {
          ++*n;
          printf ("%d:%d:%d:%d:", depth, *n,
                  poppler_layer_is_visible (layer) ? 1 : 0,
                  layer_radio_group (layer, groups));
          print_response_string (poppler_layer_get_title (layer), NEWLINE);
          g_object_unref (layer);
        }
      else
        {
          gchar *title = poppler_layers_iter_get_title (iter);

          printf ("%d:0:0:0:", depth);
          print_response_string (title, NEWLINE);
          g_free (title);
        }

      if (child)
        {
          layers_print_walk (child, depth + 1, n, groups);
          poppler_layers_iter_free (child);
        }
    } while (poppler_layers_iter_next (iter));
}

/* Print the layers of DOC as described for the layers command. */
static void
layers_print (PopplerDocument *doc)
{
  PopplerLayersIter *iter = poppler_layers_iter_new (doc);
  GArray *groups = g_array_new (FALSE, FALSE, sizeof (gint));
  int n = 0;

  if (iter)
    {
      layers_print_walk (iter, 1, &n, groups);
      poppler_layers_iter_free (iter);
    }
  g_array_free (groups, TRUE);
}

/* Return the layer numbered ID (see the layers command) at or below
   ITER, counting from *N, or NULL.  The caller has to unref it. */
static PopplerLayer *
layers_find_walk (PopplerLayersIter *iter, int id, int *n)
{
  do
    {
      PopplerLayer *layer = poppler_layers_iter_get_layer (iter);
      PopplerLayersIter *child;

      if (layer)
        {
          if (++*n == id)
            return layer;
          g_object_unref (layer);
        }
      child = poppler_layers_iter_get_child (iter);
      if (child)
        {
          layer = layers_find_walk (child, id, n);
          poppler_layers_iter_free (child);
          if (layer)
            return layer;
        }
    } while (poppler_layers_iter_next (iter));

  return NULL;
}

static PopplerLayer *
layers_find (PopplerDocument *doc, int id)
{
  PopplerLayersIter *iter = poppler_layers_iter_new (doc);
  PopplerLayer *layer = NULL;
  int n = 0;

  if (iter)
    {
      layer = layers_find_walk (iter, id, &n);
      poppler_layers_iter_free (iter);
    }
  return layer;
}

const command_arg_type_t cmd_layers_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_layers (const epdfinfo_t *ctx, const command_arg_t *args)
{
  OK_BEGIN ();
  layers_print (args->value.doc->pdf);
  OK_END ();
}

/* Name: setlayer
   Args: filename id visible
   Returns: The document's layers, as returned by the layers command,
   after showing (VISIBLE is 1) or hiding (VISIBLE is 0) the layer
   numbered ID.  Showing a layer of a radio button group hides the
   other layers of its group.
   Errors: If there is no layer numbered ID.
*/

const command_arg_type_t cmd_setlayer_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* layer id */
    ARG_BOOL,                   /* visible */
  };

static void
cmd_setlayer (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int id = args[1].value.natnum;
  PopplerLayer *layer = id > 0 ? layers_find (doc, id) : NULL;

  perror_if_not (layer, "No such layer: %d", id);
  /* poppler_layer_show hides the other layers of the radio button
     group by itself. */
  if (args[2].value.flag)
    poppler_layer_show (layer);
  else
    poppler_layer_hide (layer);
  g_object_unref (layer);

  OK_BEGIN ();
  layers_print (doc);
  OK_END ();
 error: ;
}

/* Name: ocgstate
   Args: filename page index
   Returns: The document's layers, as returned by the layers command,
   after performing the set-OCG-state action of the INDEX'th link of
   PAGE, see the ocg-state action of cmd_pagelinks.
   Errors: If page is out of range or the link does not exist or has
   no such action.
*/

const command_arg_type_t cmd_ocgstate_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_NATNUM,                 /* link index */
  };

static void
cmd_ocgstate (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  int index = args[2].value.natnum;
  PopplerPage *page = poppler_document_get_page (doc, pn - 1);
  GList *link_map = NULL, *item;
  PopplerLinkMapping *link;

  perror_if_not (page, "No such page %d", pn);
  link_map = poppler_page_get_link_mapping (page);
  link = index > 0 ? g_list_nth_data (link_map, index - 1) : NULL;
  perror_if_not (link && link->action
                 && link->action->type == POPPLER_ACTION_OCG_STATE,
                 "No layer action at link %d of page %d", index, pn);

  for (item = link->action->ocg_state.state_list; item; item = item->next)
    {
      PopplerActionLayer *state = item->data;
      GList *l;

      for (l = state->layers; l; l = l->next)
        {
          PopplerLayer *layer = l->data;
          gboolean show = (state->action == POPPLER_ACTION_LAYER_ON
                           || (state->action == POPPLER_ACTION_LAYER_TOGGLE
                               && ! poppler_layer_is_visible (layer)));
          if (show)
            poppler_layer_show (layer);
          else
            poppler_layer_hide (layer);
        }
    }

  OK_BEGIN ();
  layers_print (doc);
  OK_END ();
 error:
  if (page) g_object_unref (page);
  if (link_map) poppler_page_free_link_mapping (link_map);
}

/* Name: quit
   Args: None
   Returns: Nothing
   Errors: None

   Close all documents and exit.
*/


const command_arg_type_t cmd_quit_spec[] = {};

static void
cmd_quit (const epdfinfo_t *ctx, const command_arg_t *args)
{
  cmd_closeall (ctx, args);
  exit (EXIT_SUCCESS);
}

/* Name: number-of-pages
   Args: filename
   Returns: The number of pages.
   Errors: None
*/


const command_arg_type_t cmd_number_of_pages_spec[] =
  {
    ARG_DOC
  };

static void
cmd_number_of_pages (const epdfinfo_t *ctx, const command_arg_t *args)
{
  int npages = poppler_document_get_n_pages (args->value.doc->pdf);
  OK_BEGIN ();
  printf ("%d\n", npages);
  OK_END ();
}

/* Name: pagelinks
   Args: filename page
   Returns: A list of linkmaps:

   edges ACTION ,

   where ACTION is one of

   'goto-dest' title page top
   'goto-remote' title filename page top
   'uri' title URI
   'launch' title program arguments
   'ocg-state' title description page index

   top is desired vertical position, filename is the target PDF of the
   `goto-remote' link.  An `ocg-state' link shows, hides or toggles
   layers: description says which (e.g. "Show A, B; Toggle C"), and
   page and index identify the link for the ocgstate command, which
   performs it.  Outlines do not report this kind of action.

   Errors: None
*/

/* Print the description, PN and INDEX of an ocg-state action, see
   cmd_pagelinks. */
static void
action_print_ocg_state (PopplerAction *action, int pn, int index)
{
  GString *desc = g_string_new (NULL);
  GList *item;

  for (item = action->ocg_state.state_list; item; item = item->next)
    {
      PopplerActionLayer *state = item->data;
      GList *l;

      if (! state->layers)
        continue;
      if (desc->len > 0)
        g_string_append (desc, "; ");
      g_string_append (desc,
                       state->action == POPPLER_ACTION_LAYER_ON ? "Show "
                       : state->action == POPPLER_ACTION_LAYER_OFF ? "Hide "
                       : "Toggle ");
      for (l = state->layers; l; l = l->next)
        {
          const gchar *title = poppler_layer_get_title (l->data);

          g_string_append (desc, title ? title : "?");
          if (l->next)
            g_string_append (desc, ", ");
        }
    }
  print_response_string (desc->str, COLON);
  printf ("%d:%d\n", pn, index);
  g_string_free (desc, TRUE);
}


const command_arg_type_t cmd_pagelinks_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* page number */
  };

static void
cmd_pagelinks(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  PopplerPage *page = NULL;
  int pn = args[1].value.natnum;
  double width, height;
  GList *link_map = NULL, *item;

  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);
  link_map = poppler_page_get_link_mapping (page);

  OK_BEGIN ();
  for (item = g_list_last (link_map); item; item = item->prev)
    {

      PopplerLinkMapping *link = item->data;
      PopplerRectangle *r = &link->area;
      gdouble y1 = r->y1;
      /* LinkMappings have a different gravity. */
      r->y1 = height - r->y2;
      r->y2 = height - y1;

      if (link->action
          && link->action->type == POPPLER_ACTION_OCG_STATE)
        {
          printf ("%f %f %f %f:",
                  r->x1 / width, r->y1 / height,
                  r->x2 / width, r->y2 / height);
          print_response_string ("ocg-state", COLON);
          print_response_string (link->action->any.title, COLON);
          action_print_ocg_state (link->action, pn,
                                  g_list_position (link_map, item) + 1);
          continue;
        }

      if (! action_is_handled (link->action))
        continue;

      printf ("%f %f %f %f:",
              r->x1 / width, r->y1 / height,
              r->x2 / width, r->y2 / height);
      action_print (doc, link->action);
    }
  OK_END ();
 error:
  if (page) g_object_unref (page);
  if (link_map) poppler_page_free_link_mapping (link_map);
}

/* Name: gettext
   Args: filename page edges selection-style
   Returns: The selection's text.
   Errors: If page is out of range.

   For the selection-style argument see getselection command.
*/


const command_arg_type_t cmd_gettext_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGES,                  /* selection */
    ARG_NATNUM                  /* selection-style */
  };

static void
cmd_gettext(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerRectangle r = args[2].value.rectangle;
  int selection_style = args[3].value.natnum;
  PopplerPage *page = NULL;
  double width, height;
  gchar *text = NULL;

  selection_style = xpoppler_validate_selection_style (selection_style);
  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);
  r.x1 = r.x1 * width;
  r.x2 = r.x2 * width;
  r.y1 = r.y1 * height;
  r.y2 = r.y2 * height;
  /* printf ("%f %f %f %f , %f %f\n", r.x1, r.y1, r.x2, r.y2, width, height); */
  text = poppler_page_get_selected_text (page, selection_style, &r);

  OK_BEGIN ();
  print_response_string (text, NEWLINE);
  OK_END ();

 error:
  g_free (text);
  if (page) g_object_unref (page);
}

/* Name: getselection
   Args: filename page edges selection-selection_style
   Returns: The selection's text.
   Errors: If page is out of range.

   selection-selection_style should be as follows.

   0 (POPPLER_SELECTION_GLYPH)
	glyph is the minimum unit for selection

   1 (POPPLER_SELECTION_WORD)
	word is the minimum unit for selection

   2 (POPPLER_SELECTION_LINE)
	line is the minimum unit for selection
*/


const command_arg_type_t cmd_getselection_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGES,                  /* selection */
    ARG_NATNUM                  /* selection-style */
  };

static void
cmd_getselection (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerRectangle r = args[2].value.rectangle;
  int selection_style = args[3].value.natnum;
  gdouble width, height;
  cairo_region_t *region = NULL;
  PopplerPage *page = NULL;
  int i;

  selection_style = xpoppler_validate_selection_style (selection_style);
  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);

  r.x1 = r.x1 * width;
  r.x2 = r.x2 * width;
  r.y1 = r.y1 * height;
  r.y2 = r.y2 * height;

  region = poppler_page_get_selected_region (page, 1.0, selection_style, &r);

  OK_BEGIN ();
  for (i = 0; i < cairo_region_num_rectangles (region); ++i)
    {
      cairo_rectangle_int_t r;

      cairo_region_get_rectangle (region, i, &r);
      printf ("%f %f %f %f\n",
              r.x / width,
              r.y / height,
              (r.x + r.width) / width,
              (r.y + r.height) / height);
    }
  OK_END ();

 error:
  if (region) cairo_region_destroy (region);
  if (page) g_object_unref (page);
}

/* Name: pagesize
   Args: filename page
   Returns: width height
   Errors: If page is out of range.
*/


const command_arg_type_t cmd_pagesize_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* page number */
  };

static void
cmd_pagesize(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerPage *page = NULL;
  double width, height;


  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);

  OK_BEGIN ();
  printf ("%f:%f\n", width, height);
  OK_END ();

 error:
  if (page) g_object_unref (page);
}

/* Annotations */

/* Name: getannots
   Args: filename firstpage lastpage
   Returns: The list of annotations of this page.

   For all annotations

   page edges type key flags color contents mod-date

   ,where

   name is a document-unique name,
   flags is PopplerAnnotFlag bitmask,
   color is 3-byte RGB hex number and

   Then

   label subject opacity popup-edges popup-is-open create-date

   if this is a markup annotation and additionally

   text-icon text-state

   for markup text annotations,

   interior-color

   for square and circle annotations,

   stamp-icon

   for stamp annotations and

   font-name font-size font-color

   for free text annotations.

   Errors: If page is out of range.
*/


const command_arg_type_t cmd_getannots_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* first page */
    ARG_NATNUM                  /* last page */
  };

static void
cmd_getannots(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  gint first = args[1].value.natnum;
  gint last = args[2].value.natnum;
  GList *list;
  gint pn;

  first = MAX(1, first);
  if (last <= 0)
    last = poppler_document_get_n_pages (doc);
  else
    last = MIN(last, poppler_document_get_n_pages (doc));

  OK_BEGIN ();
  for (pn = first; pn <= last; ++pn)
    {
      GList *annots = annoation_get_for_page (args->value.doc, pn);
      PopplerPage *page = poppler_document_get_page (doc, pn - 1);

      if (! page)
        continue;

      for (list = annots; list; list = list->next)
        {
          annotation_t *annot = (annotation_t *)list->data;
          annotation_print (annot, page);
        }
      g_object_unref (page);
    }
  OK_END ();
}

/* Name: getannot
   Args: filename name
   Returns: The annotation for name, see cmd_getannots.
   Errors: If no annotation named ,name' exists.
*/


const command_arg_type_t cmd_getannot_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING,        /* annotation's key */
  };

static void
cmd_getannot (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  PopplerPage *page = NULL;
  annotation_t *a = annotation_get_by_key (doc, key);
  gint index;

  perror_if_not (a, "No such annotation: %s", key);
  index = poppler_annot_get_page_index (a->amap->annot);
  if (index >= 0)
    page = poppler_document_get_page (doc->pdf, index);
  perror_if_not (page, "Unable to get page %d", index + 1);

  OK_BEGIN ();
  annotation_print (a, page);
  OK_END ();

 error:
  if (page) g_object_unref (page);
}

/* Name: getannot_attachment
   Args: filename name [output-filename]
   Returns: name description size mtime ctime output-filename
   Errors: If no annotation named ,name' exists or output-filename is
   not writable.
*/


const command_arg_type_t cmd_getattachment_from_annot_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING,        /* annotation's name */
    ARG_BOOL                    /* save attachment */
  };

static void
cmd_getattachment_from_annot (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  gboolean do_save = args[2].value.flag;
  PopplerAttachment *att = NULL;
  annotation_t *a = annotation_get_by_key (doc, key);
  gchar *id = NULL;

  perror_if_not (a, "No such annotation: %s", key);
  perror_if_not (POPPLER_IS_ANNOT_FILE_ATTACHMENT (a->amap->annot),
                "Not a file annotation: %s", key);
  att = poppler_annot_file_attachment_get_attachment
        (POPPLER_ANNOT_FILE_ATTACHMENT (a->amap->annot));
  perror_if_not (att, "Unable to get attachment: %s", key);
  id = g_strdup_printf ("attachment-%s", key);

  OK_BEGIN ();
  attachment_print (att, id, do_save);
  OK_END ();

 error:
  if (att) g_object_unref (att);
  if (id) g_free (id);
}


/* document-level attachments */
const command_arg_type_t cmd_getattachments_spec[] =
  {
    ARG_DOC,
    ARG_BOOL,        /* save attachments */
  };

static void
cmd_getattachments (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  gboolean do_save = args[1].value.flag;
  GList *item;
  GList *attmnts = poppler_document_get_attachments (doc->pdf);
  int i;

  OK_BEGIN ();
  for (item = attmnts, i = 0; item; item = item->next, ++i)
    {
      PopplerAttachment *att = (PopplerAttachment*) item->data;
      gchar *id = g_strdup_printf ("attachment-document-%d", i);

      attachment_print (att, id, do_save);
      g_object_unref (att);
      g_free (id);
    }
  g_list_free (attmnts);

  OK_END ();
}

#ifdef HAVE_POPPLER_ANNOT_WRITE

/* Name: addannot
   Args: filename page type selection-style edges [rest ...]
   Returns: The new annotation, see getannots.

   The meaning of rest depends on type:

   highlight, squiggly, strike-out, underline: the edges of the
   marked up text regions,
   text, square, circle: none,
   line: optionally the relative start and end point "X1 Y1 X2 Y2"
   (default: from the top-left to the bottom-right corner of edges),
   stamp: optionally a standard icon name, e.g. Approved, or
   "image:FILENAME" for a stamp showing the PNG image in FILENAME,
   free-text: none; set its text with editannot's contents and its
   font with font-name, font-size and font-color,
   ink: optionally the line width in points, followed by one or more
   strokes, each a list of relative points "X1 Y1 X2 Y2 ...".  The
   strokes determine the edges.

   Errors: If the type is not supported or the arguments are invalid.
*/

const command_arg_type_t cmd_addannot_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_STRING,                 /* type */
    ARG_NATNUM,                 /* selection-style */
    ARG_EDGES_OR_POSITION,      /* edges or position (uses default size) */
    ARG_REST,                  /* markup regions */
  };

static void
cmd_addannot (const epdfinfo_t *ctx, const command_arg_t *args)
{

  document_t *doc = args->value.doc;
  gint pn = args[1].value.natnum;
  const char *type_string = args[2].value.string;
  int selection_style = args[3].value.natnum;
  PopplerRectangle r = args[4].value.rectangle;
  int i;
  PopplerPage *page = NULL;
  double width, height;
  PopplerAnnot *pa;
  PopplerAnnotMapping *amap;
  annotation_t *a;
  gchar *key;
  GList *annotations;
  gdouble y2;
  char *error_msg = NULL;

  selection_style = xpoppler_validate_selection_style (selection_style);
  page = poppler_document_get_page (doc->pdf, pn - 1);
  perror_if_not (page, "Unable to get page %d", pn);
  poppler_page_get_size (page, &width, &height);
  r.x1 *= width; r.x2 *= width;
  r.y1 *= height; r.y2 *= height;
  if (r.y2 < 0)
    r.y2 = r.y1 + 24;
  if (r.x2 < 0)
    r.x2 = r.x1 + 24;
  y2 = r.y2;
  r.y2 = height - r.y1;
  r.y1 = height - y2;

  pa = annotation_new (ctx, doc, page, type_string, selection_style, &r, &args[5],
                       &error_msg);
  perror_if_not (pa, "Creating annotation failed: %s",
                 error_msg ? error_msg : "Reason unknown");
  amap = poppler_annot_mapping_new ();
  amap->area = r;
  amap->annot = pa;
  annotations = annoation_get_for_page (doc, pn);

  i = g_list_length (annotations);
  key = g_strdup_printf ("annot-%d-%d", pn, i);
  while (g_hash_table_lookup (doc->annotations.keys, key))
    {
      g_free (key);
      key = g_strdup_printf ("annot-%d-%d", pn, ++i);
    }
  a = g_malloc (sizeof (annotation_t));
  a->amap = amap;
  a->key = key;
  a->image_file = NULL;
  doc->annotations.pages[pn - 1] =
    g_list_prepend (annotations, a);
  g_hash_table_insert (doc->annotations.keys, key, a);
  poppler_page_add_annot (page, pa);
#ifdef HAVE_POPPLER_ANNOT_STAMP
  if (POPPLER_IS_ANNOT_STAMP (pa)
      && args[5].value.rest.nargs > 0
      && ! strncmp (args[5].value.rest.args[0], "image:", 6))
    {
      /* The image of the stamp can only be set now that the
         annotation is on the page: adding it moves the annotation,
         and moving drops the appearance it would have been given. */
#if CAIRO_HAS_PNG_FUNCTIONS
      perror_if_not (annotation_stamp_set_image
                     (POPPLER_ANNOT_STAMP (pa),
                      args[5].value.rest.args[0] + 6, &error_msg),
                    "Creating annotation failed: %s",
                    error_msg ? error_msg : "Reason unknown");
      /* Remember it, so that it survives moving the annotation,
         which drops the appearance again. */
      a->image_file = g_strdup (args[5].value.rest.args[0] + 6);
#endif
    }
#endif
#ifdef HAVE_POPPLER_ANNOT_INK
  if (POPPLER_IS_ANNOT_INK (pa))
    {
      /* The strokes need the page and determine the rectangle.  They
         were checked by annotation_new. */
      perror_if_not (annotation_ink_set_paths
                     (ctx, page, POPPLER_ANNOT_INK (pa),
                      args[5].value.rest.args, args[5].value.rest.nargs,
                      &error_msg),
                     "Creating annotation failed: %s",
                     error_msg ? error_msg : "Reason unknown");
      poppler_annot_get_rectangle (pa, &amap->area);
    }
#endif
  OK_BEGIN ();
  annotation_print (a, page);
  OK_END ();

 error:
  if (page) g_object_unref (page);
  if (error_msg) g_free (error_msg);
}


const command_arg_type_t cmd_delannot_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING         /* Annotation's key */
  };

static void
cmd_delannot (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  PopplerPage *page = NULL;
  annotation_t *a = annotation_get_by_key (doc, key);
  gint pn;

  perror_if_not (a, "No such annotation: %s", key);
  pn = poppler_annot_get_page_index (a->amap->annot) + 1;
  if (pn >= 1)
    page = poppler_document_get_page (doc->pdf, pn - 1);
  perror_if_not (page, "Unable to get page %d", pn);
  poppler_page_remove_annot (page, a->amap->annot);
  doc->annotations.pages[pn - 1] =
    g_list_remove (doc->annotations.pages[pn - 1], a);
  g_hash_table_remove (doc->annotations.keys, a->key);
  poppler_annot_mapping_free(a->amap);
  OK ();

 error:
  if (a)
    {
      g_free (a->key);
      g_free (a->image_file);
      g_free (a);
    }
  if (page) g_object_unref (page);
}

const command_arg_type_t cmd_editannot_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING,        /* annotation key */
    ARG_REST                    /* (KEY VALUE ...) */
  };

static void
cmd_editannot (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  const char *key = args[1].value.string;
  int nrest_args = args[2].value.rest.nargs;
  char * const *rest_args = args[2].value.rest.args;
  annotation_t *a = annotation_get_by_key (doc, key);
  PopplerAnnot *pa;
  PopplerPage *page = NULL;
  int i = 0;
  gint index;
  char *error_msg = NULL;
  command_arg_t carg;
  const char *unexpected_parse_error = "Internal error while parsing arg `%s'";

  perror_if_not (a, "No such annotation: %s", key);
  pa = a->amap->annot;
  perror_if_not (annotation_edit_validate (ctx, &args[2], pa, &error_msg),
                 "%s", error_msg);
  index = poppler_annot_get_page_index (pa);
  page = poppler_document_get_page (doc->pdf, index);
  perror_if_not (page, "Unable to get page %d for annotation", index);

  for (i = 0; i < nrest_args; ++i)
    {
      const char *key = rest_args[i++];

      if (! strcmp (key, "flags"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_NATNUM, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_set_flags (pa, carg.value.natnum);
        }
      else if (! strcmp (key, "color"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_COLOR, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_set_color (pa, &carg.value.color);
        }
      else if (! strcmp (key, "contents"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_STRING, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_set_contents (pa, carg.value.string);
#ifdef HAVE_POPPLER_ANNOT_FREE_TEXT
          /* Make sure the font has glyphs for the new text. */
          if (POPPLER_IS_ANNOT_FREE_TEXT (pa))
            annotation_free_text_set_font (POPPLER_ANNOT_FREE_TEXT (pa),
                                           NULL, 0);
#endif
        }
      else if (! strcmp (key, "edges"))
        {
          PopplerRectangle *area = &a->amap->area;
          gdouble width, height;
          PopplerRectangle r;

          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_EDGES_OR_POSITION, NULL),
                         unexpected_parse_error, rest_args[i]);
          r = carg.value.rectangle;
          poppler_page_get_size (page, &width, &height);

          /* Translate Gravity and maybe keep the width and height. */
          if (r.x2 < 0)
            area->x2 +=  (r.x1 * width) - area->x1;
          else
            area->x2 = r.x2 * width;

          if (r.y2 < 0)
            area->y1 -=  (r.y1 * height) - (height - area->y2);
          else
            area->y1 = height - (r.y2 * height);

          area->x1 = r.x1 * width;
          area->y2 = height - (r.y1 * height);

#ifdef HAVE_POPPLER_ANNOT_INK
          if (POPPLER_IS_ANNOT_INK (pa))
            {
              /* Move the strokes, which determine the rectangle;
                 their size stays the same. */
              PopplerRectangle old;

              poppler_annot_get_rectangle (pa, &old);
              annotation_ink_translate (POPPLER_ANNOT_INK (pa),
                                        area->x1 - old.x1,
                                        area->y2 - old.y2);
              poppler_annot_get_rectangle (pa, area);
            }
          else
#endif
            poppler_annot_set_rectangle (pa, area);

#ifdef HAVE_POPPLER_ANNOT_STAMP
          /* Moving drops the appearance of an image stamp; put the
             image back on. */
          if (a->image_file)
            {
#if CAIRO_HAS_PNG_FUNCTIONS
              perror_if_not (annotation_stamp_set_image
                             (POPPLER_ANNOT_STAMP (pa),
                              a->image_file, &error_msg),
                            "Moving the annotation failed: %s",
                            error_msg ? error_msg : "Reason unknown");
#endif
            }
#endif
        }
      else if (! strcmp (key, "label"))
        {
          PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_STRING, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_markup_set_label (ma, carg.value.string);
        }
      else if (! strcmp (key, "opacity"))
        {
          PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_EDGE, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_markup_set_opacity (ma, carg.value.edge);
        }
      else if (! strcmp (key, "popup"))
        {
          PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_EDGES, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_markup_set_popup (ma, &carg.value.rectangle);
        }
      else if (! strcmp (key, "popup-is-open"))
        {
          PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_BOOL, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_markup_set_popup_is_open (ma, carg.value.flag);
        }
      else if (! strcmp (key, "icon"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_STRING, NULL),
                         unexpected_parse_error, rest_args[i]);
#ifdef HAVE_POPPLER_ANNOT_STAMP
          if (POPPLER_IS_ANNOT_STAMP (pa))
            {
              PopplerAnnotStampIcon icon;
              perror_if_not (xpoppler_annot_stamp_icon_parse (carg.value.string,
                                                              &icon),
                             unexpected_parse_error, rest_args[i]);
              poppler_annot_stamp_set_icon (POPPLER_ANNOT_STAMP (pa), icon);
            }
          else
#endif
            poppler_annot_text_set_icon (POPPLER_ANNOT_TEXT (pa),
                                         carg.value.string);
        }
#ifdef HAVE_POPPLER_ANNOT_SHAPES
      else if (! strcmp (key, "interior-color"))
        {
          PopplerColor *color = NULL;

          if (*rest_args[i])
            {
              perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                     ARG_COLOR, NULL),
                             unexpected_parse_error, rest_args[i]);
              color = &carg.value.color;
            }
          if (POPPLER_IS_ANNOT_SQUARE (pa))
            poppler_annot_square_set_interior_color (POPPLER_ANNOT_SQUARE (pa),
                                                     color);
          else
            poppler_annot_circle_set_interior_color (POPPLER_ANNOT_CIRCLE (pa),
                                                     color);
        }
      else if (! strcmp (key, "line"))
        {
          PopplerPoint start, end;

          perror_if_not (annotation_line_parse_points (ctx, page, rest_args[i],
                                                       &start, &end,
                                                       &a->amap->area, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_line_set_vertices (POPPLER_ANNOT_LINE (pa),
                                           &start, &end);
          poppler_annot_set_rectangle (pa, &a->amap->area);
        }
#endif
#ifdef HAVE_POPPLER_ANNOT_FREE_TEXT
      else if (! strcmp (key, "font-name"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_NONEMPTY_STRING, NULL),
                         unexpected_parse_error, rest_args[i]);
          annotation_free_text_set_font (POPPLER_ANNOT_FREE_TEXT (pa),
                                         carg.value.string, 0);
        }
      else if (! strcmp (key, "font-size"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_DOUBLE, NULL),
                         unexpected_parse_error, rest_args[i]);
          annotation_free_text_set_font (POPPLER_ANNOT_FREE_TEXT (pa),
                                         NULL, carg.value.scalar);
        }
      else if (! strcmp (key, "font-color"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_COLOR, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_free_text_set_font_color (POPPLER_ANNOT_FREE_TEXT (pa),
                                                  &carg.value.color);
        }
#endif
      else if (! strcmp (key, "is-open"))
        {
          PopplerAnnotText *ta = POPPLER_ANNOT_TEXT (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_BOOL, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_text_set_is_open (ta, carg.value.flag);
        }
      else
        {
          perror_if_not (0, "internal error: annotation property validation failed");
        }
    }

  OK_BEGIN ();
  annotation_print (a, page);
  OK_END ();

 error:
  if (error_msg) g_free (error_msg);
  if (page) g_object_unref (page);
}

const command_arg_type_t cmd_save_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_save (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  char *filename = mktempfile ();
  GError *gerror = NULL;
  gchar *uri;
  gboolean success = FALSE;

  if (!filename)
    {
      printf_error_response ("Unable to create temporary file");
      return;
    }

  uri = g_filename_to_uri (filename, NULL, &gerror);

  if (uri)
    {
      success = poppler_document_save (doc->pdf, uri, &gerror);
      g_free (uri);
    }
  if (! success)
    {
      printf_error_response ("Error while saving %s:%s"
                    , filename, gerror ? gerror->message : "?");
      if (gerror)
        g_error_free (gerror);
      return;
    }
  OK_BEGIN ();
  print_response_string (filename, NEWLINE);
  OK_END ();
}

#endif  /* HAVE_POPPLER_ANNOT_WRITE */


/* ================================================================== *
 * Form fields
 * ================================================================== */

static const char *
form_field_type_string (PopplerFormField *field)
{
  switch (poppler_form_field_get_field_type (field))
    {
    case POPPLER_FORM_FIELD_TEXT:
      switch (poppler_form_field_text_get_text_type (field))
        {
        case POPPLER_FORM_TEXT_MULTILINE: return "multiline";
        case POPPLER_FORM_TEXT_FILE_SELECT: return "file-select";
        default:
          return poppler_form_field_text_is_password (field)
            ? "password" : "text";
        }
    case POPPLER_FORM_FIELD_BUTTON:
      switch (poppler_form_field_button_get_button_type (field))
        {
        case POPPLER_FORM_BUTTON_CHECK: return "checkbox";
        case POPPLER_FORM_BUTTON_RADIO: return "radio";
        default: return "push";
        }
    case POPPLER_FORM_FIELD_CHOICE:
      return poppler_form_field_choice_get_choice_type (field)
        == POPPLER_FORM_CHOICE_COMBO ? "combo" : "list";
    case POPPLER_FORM_FIELD_SIGNATURE: return "signature";
    default: return "unknown";
    }
}

#ifdef HAVE_POPPLER_FORM_RESET
/* Return the reset-form action of push button FIELD, or NULL. */
static PopplerActionResetForm *
form_field_reset_action (PopplerFormField *field)
{
  PopplerAction *action;

  if (poppler_form_field_get_field_type (field) != POPPLER_FORM_FIELD_BUTTON
      || poppler_form_field_button_get_button_type (field)
      != POPPLER_FORM_BUTTON_PUSH)
    return NULL;
  action = poppler_form_field_get_action (field);
  return action && action->type == POPPLER_ACTION_RESET_FORM
    ? &action->reset_form : NULL;
}
#endif

/* Name: getformfields
   Args: filename page
   Returns: A list of form fields on page, one per line:

   page edges id type name tooltip read-only flags max-length value
   [text item ...]

   where type is one of text, multiline, password, file-select,
   checkbox, radio, push, combo, list, signature or unknown.  The
   tooltip is the field's user-facing description, if it has one.
   Flags is a space separated list of the words multiple, if more
   than one item of a choice field may be selected, editable, if a
   combo field accepts text other than its items, and reset, if a
   push button resets the form.  Max-length is the most characters a
   text field takes, or empty if it has no limit.  The value is the text of
   a text field, 1 or 0 for a checkbox or radio button, empty for
   other buttons, and the space separated indices of the selected
   items of a combo or list field.  The text a user typed into an
   editable combo field and the field's items follow those of choice
   fields.

   Errors: If page is out of range.
*/

const command_arg_type_t cmd_getformfields_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* page number */
  };

static void
cmd_getformfields (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerPage *page = NULL;
  GList *field_map = NULL, *item;
  double width, height;

  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);
  field_map = poppler_page_get_form_field_mapping (page);

  OK_BEGIN ();
  for (item = g_list_last (field_map); item; item = item->prev)
    {
      PopplerFormFieldMapping *m = item->data;
      PopplerFormField *field = m->field;
      PopplerRectangle *r = &m->area;
      gchar *name = poppler_form_field_get_name (field);
      gchar *text;
      int i, n;

      printf ("%d:", pn);
      /* Mappings have a different gravity, see cmd_pagelinks. */
      printf ("%f %f %f %f:",
              r->x1 / width, (height - r->y2) / height,
              r->x2 / width, (height - r->y1) / height);
      printf ("%d:", poppler_form_field_get_id (field));
      printf ("%s:", form_field_type_string (field));
      print_response_string (name, COLON);
      g_free (name);
#ifdef HAVE_POPPLER_FORM_TOOLTIP
      text = poppler_form_field_get_alternate_ui_name (field);
      print_response_string (text, COLON);
      g_free (text);
#else
      putchar (':');
#endif
      printf ("%d:", poppler_form_field_is_read_only (field) ? 1 : 0);

      /* Flags */
      if (poppler_form_field_get_field_type (field) == POPPLER_FORM_FIELD_CHOICE)
        {
          if (poppler_form_field_choice_can_select_multiple (field))
            printf ("multiple ");
          if (poppler_form_field_choice_is_editable (field))
            printf ("editable");
        }
#ifdef HAVE_POPPLER_FORM_RESET
      if (form_field_reset_action (field))
        printf ("reset");
#endif
      putchar (':');

      /* Max-length */
      if (poppler_form_field_get_field_type (field) == POPPLER_FORM_FIELD_TEXT
          && poppler_form_field_text_get_max_len (field) > 0)
        printf ("%d", poppler_form_field_text_get_max_len (field));
      putchar (':');

      switch (poppler_form_field_get_field_type (field))
        {
        case POPPLER_FORM_FIELD_TEXT:
          text = poppler_form_field_text_get_text (field);
          print_response_string (text, NEWLINE);
          g_free (text);
          break;
        case POPPLER_FORM_FIELD_BUTTON:
          if (poppler_form_field_button_get_button_type (field)
              == POPPLER_FORM_BUTTON_PUSH)
            putchar ('\n');
          else
            printf ("%d\n", poppler_form_field_button_get_state (field) ? 1 : 0);
          break;
        case POPPLER_FORM_FIELD_CHOICE:
          n = poppler_form_field_choice_get_n_items (field);
          for (i = 0; i < n; ++i)
            if (poppler_form_field_choice_is_item_selected (field, i))
              printf ("%d ", i);
          putchar (':');
          text = poppler_form_field_choice_is_editable (field)
            ? poppler_form_field_choice_get_text (field) : NULL;
          print_response_string (text, NONE);
          g_free (text);
          for (i = 0; i < n; ++i)
            {
              text = poppler_form_field_choice_get_item (field, i);
              putchar (':');
              print_response_string (text, NONE);
              g_free (text);
            }
          putchar ('\n');
          break;
        default:
          putchar ('\n');
        }
    }
  OK_END ();

 error:
  if (field_map) poppler_page_free_form_field_mapping (field_map);
  if (page) g_object_unref (page);
}

/* Name: setformfield
   Args: filename id value
   Returns: Nothing
   Errors: If there is no such field, it is read-only or can't be set.

   The value is the text of a text field, 1 or 0 for a checkbox or
   radio button and the space separated indices of the items to
   select for a combo or list field.
*/

const command_arg_type_t cmd_setformfield_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* field id */
    ARG_STRING                  /* value */
  };

static void
cmd_setformfield (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int id = args[1].value.natnum;
  const char *value = args[2].value.string;
  PopplerFormField *field = poppler_document_get_form_field (doc, id);
  gchar **indices = NULL, **index;
  int n;

  perror_if_not (field, "No such form field: %d", id);
  perror_if_not (! poppler_form_field_is_read_only (field),
                 "Form field is read-only: %d", id);

  switch (poppler_form_field_get_field_type (field))
    {
    case POPPLER_FORM_FIELD_TEXT:
      poppler_form_field_text_set_text (field, value);
      break;
    case POPPLER_FORM_FIELD_BUTTON:
      perror_if_not (poppler_form_field_button_get_button_type (field)
                     != POPPLER_FORM_BUTTON_PUSH,
                     "Can't set a push button: %d", id);
      poppler_form_field_button_set_state (field, strcmp (value, "1") == 0);
      break;
    case POPPLER_FORM_FIELD_CHOICE:
      n = poppler_form_field_choice_get_n_items (field);
      indices = g_strsplit (value, " ", -1);
      for (index = indices; *index; ++index)
        {
          char *end;
          long i;

          if (! **index)
            continue;
          i = strtol (*index, &end, 10);
          perror_if_not (! *end && i >= 0 && i < n,
                         "Invalid item index: %s", *index);
        }
      poppler_form_field_choice_unselect_all (field);
      for (index = indices; *index; ++index)
        if (**index)
          poppler_form_field_choice_toggle_item (field, atoi (*index));
      break;
    default:
      perror_if_not (0, "Can't set this kind of form field: %d", id);
    }

  OK_BEGIN ();
  OK_END ();

 error:
  if (indices) g_strfreev (indices);
  if (field) g_object_unref (field);
}

#ifdef HAVE_POPPLER_FORM_RESET
/* Name: resetform
   Args: filename id
   Returns: Nothing
   Errors: If ID is not 0 and not a push button that resets the form.

   Resets the fields of the form to their default values: all of
   them if ID is 0, otherwise the ones the reset button ID names.
*/

const command_arg_type_t cmd_resetform_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* field id, or 0 */
  };

static void
cmd_resetform (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int id = args[1].value.natnum;
  PopplerFormField *field = NULL;
  PopplerActionResetForm *reset;

  if (id == 0)
    poppler_document_reset_form (doc, NULL, FALSE);
  else
    {
      field = poppler_document_get_form_field (doc, id);
      perror_if_not (field, "No such form field: %d", id);
      reset = form_field_reset_action (field);
      perror_if_not (reset, "Not a reset button: %d", id);
      poppler_document_reset_form (doc, reset->fields, reset->exclude);
    }

  OK_BEGIN ();
  OK_END ();

 error:
  if (field) g_object_unref (field);
}
#endif  /* HAVE_POPPLER_FORM_RESET */

/* Name: setformfield-text
   Args: filename id text
   Returns: Nothing
   Errors: If there is no such field, it is read-only or not an
   editable combo field.

   Sets the text of an editable combo field to one that need not be
   one of its items.
*/

const command_arg_type_t cmd_setformfield_text_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* field id */
    ARG_STRING                  /* text */
  };

static void
cmd_setformfield_text (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int id = args[1].value.natnum;
  const char *text = args[2].value.string;
  PopplerFormField *field = poppler_document_get_form_field (doc, id);

  perror_if_not (field, "No such form field: %d", id);
  perror_if_not (! poppler_form_field_is_read_only (field),
                 "Form field is read-only: %d", id);
  perror_if_not (poppler_form_field_get_field_type (field)
                 == POPPLER_FORM_FIELD_CHOICE
                 && poppler_form_field_choice_is_editable (field),
                 "Not an editable choice field: %d", id);
  poppler_form_field_choice_set_text (field, text);

  OK_BEGIN ();
  OK_END ();

 error:
  if (field) g_object_unref (field);
}

#ifdef HAVE_POPPLER_SIGNING
/* The NSS database directory epdfinfo opened, "" for poppler's
   default, or NULL if it has not been opened yet.  NSS opens it
   only once per process. */
static gchar *nss_dir;

/* The password for the NSS database, while a command may need it. */
static const char *nss_password;

/* The token NSS last asked a password for during this command. */
static gchar *nss_password_token;

/* Whether NSS asked for a password we did not have, respectively
   asked again after the one we gave it failed. */
static gboolean nss_password_missing, nss_password_wrong;

/* Note that NSS is about to open whatever database it opens. */
static void
nss_opened (void)
{
  if (! nss_dir)
    nss_dir = g_strdup ("");
}

static char *
nss_password_callback (const gchar *token)
{
  if (nss_password_token && ! g_strcmp0 (token, nss_password_token))
    {
      /* NSS asks again, so the password was wrong. */
      nss_password_wrong = TRUE;
      return NULL;
    }
  g_free (nss_password_token);
  nss_password_token = g_strdup (token);
  if (! nss_password || ! *nss_password)
    {
      nss_password_missing = TRUE;
      return NULL;
    }
  /* NSS frees it. */
  return strdup (nss_password);
}

/* Make PASSWORD, or none if it is empty, the NSS database's
   password for the current command. */
static void
nss_begin (const char *password)
{
  nss_opened ();
  nss_password = password;
  g_free (nss_password_token);
  nss_password_token = NULL;
  nss_password_missing = nss_password_wrong = FALSE;
  poppler_set_nss_password_callback (nss_password_callback);
}

static void
nss_end (void)
{
  nss_password = NULL;
}
#endif  /* HAVE_POPPLER_SIGNING */

#ifdef HAVE_POPPLER_SIGNATURES

static const char *
signature_status_string (PopplerSignatureStatus status)
{
  switch (status)
    {
    case POPPLER_SIGNATURE_VALID: return "valid";
    case POPPLER_SIGNATURE_INVALID: return "invalid";
    case POPPLER_SIGNATURE_DIGEST_MISMATCH: return "digest-mismatch";
    case POPPLER_SIGNATURE_DECODING_ERROR: return "decoding-error";
    case POPPLER_SIGNATURE_NOT_FOUND: return "not-found";
    default: return "error";
    }
}

static const char *
certificate_status_string (PopplerCertificateStatus status)
{
  switch (status)
    {
    case POPPLER_CERTIFICATE_TRUSTED: return "trusted";
    case POPPLER_CERTIFICATE_UNTRUSTED_ISSUER: return "untrusted-issuer";
    case POPPLER_CERTIFICATE_UNKNOWN_ISSUER: return "unknown-issuer";
    case POPPLER_CERTIFICATE_REVOKED: return "revoked";
    case POPPLER_CERTIFICATE_EXPIRED: return "expired";
    case POPPLER_CERTIFICATE_NOT_VERIFIED: return "not-verified";
    default: return "error";
    }
}

static void
print_date_time (GDateTime *time, enum suffix_char suffix)
{
  gchar *str = time ? g_date_time_format_iso8601 (time) : NULL;

  print_response_string (str, suffix);
  g_free (str);
}

/* Name: validatesignature
   Args: filename id
   Returns: The signature's status, its certificate's status, the
   signer's name, the signing time, the common name and organization
   of the certificate's subject and issuer, and its expiration time:

   signature-status certificate-status signer signing-time
   subject subject-organization issuer issuer-organization expires

   The statuses are one of valid, invalid, digest-mismatch,
   decoding-error, not-found (the field is not signed) or error,
   respectively trusted, untrusted-issuer, unknown-issuer, revoked,
   expired, not-verified or error.  Times are in ISO 8601 format.  The certificate's
   revocation is not checked online.

   Errors: If there is no such field or it is not a signature field.
*/

const command_arg_type_t cmd_validatesignature_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* field id */
  };

static void
cmd_validatesignature (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int id = args[1].value.natnum;
  PopplerFormField *field = poppler_document_get_form_field (doc, id);
  PopplerSignatureInfo *info = NULL;
  PopplerCertificateInfo *cert;
  PopplerSignatureStatus status;
  const gchar *signer, *subject;
  gboolean unsigned_field;
  GError *gerror = NULL;

  perror_if_not (field, "No such form field: %d", id);
  perror_if_not (poppler_form_field_get_field_type (field)
                 == POPPLER_FORM_FIELD_SIGNATURE,
                 "Not a signature field: %d", id);
#ifdef HAVE_POPPLER_SIGNING
  nss_opened ();
#endif
  info = poppler_form_field_signature_validate_sync
    (field,
     POPPLER_SIGNATURE_VALIDATION_FLAG_VALIDATE_CERTIFICATE
     | POPPLER_SIGNATURE_VALIDATION_FLAG_WITHOUT_OCSP_REVOCATION_CHECK,
     NULL, &gerror);
  perror_if_not (info, "Unable to validate signature: %s",
                 gerror ? gerror->message : "?");
  cert = poppler_signature_info_get_certificate_info (info);
  status = poppler_signature_info_get_signature_status (info);
  /* Poppler reports an error (generic, or not verified, depending on
     its version), and neither signer nor certificate, for a field
     nobody signed yet. */
  signer = poppler_signature_info_get_signer_name (info);
  subject = cert ? poppler_certificate_info_get_subject_common_name (cert) : NULL;
  unsigned_field = (! strcmp (signature_status_string (status), "error")
                    && (! signer || ! *signer) && (! subject || ! *subject));

  OK_BEGIN ();
  printf ("%s:", unsigned_field ? "not-found" : signature_status_string (status));
  printf ("%s:", certificate_status_string
          (poppler_signature_info_get_certificate_status (info)));
  print_response_string (signer, COLON);
  print_date_time (unsigned_field ? NULL
                   : poppler_signature_info_get_local_signing_time (info), COLON);
  print_response_string (cert ? poppler_certificate_info_get_subject_common_name (cert) : NULL, COLON);
  print_response_string (cert ? poppler_certificate_info_get_subject_organization (cert) : NULL, COLON);
  print_response_string (cert ? poppler_certificate_info_get_issuer_common_name (cert) : NULL, COLON);
  print_response_string (cert ? poppler_certificate_info_get_issuer_organization (cert) : NULL, COLON);
  print_date_time (cert ? poppler_certificate_info_get_expiration_time (cert) : NULL, NEWLINE);
  OK_END ();

 error:
  if (gerror) g_error_free (gerror);
  if (info) poppler_signature_info_free (info);
  if (field) g_object_unref (field);
}

#endif  /* HAVE_POPPLER_SIGNATURES */

#ifdef HAVE_POPPLER_SIGNING

/* Name: setnssdir
   Args: directory
   Returns: Nothing
   Errors: If NSS already opened a different database.

   Use the NSS database in directory, or poppler's default one if
   directory is empty, for signing and validating signatures.  NSS
   opens its database once per process, so this has to come before
   any other command that uses it.
*/

const command_arg_type_t cmd_setnssdir_spec[] =
  {
    ARG_STRING                  /* directory */
  };

static void
cmd_setnssdir (const epdfinfo_t *ctx, const command_arg_t *args)
{
  const char *dir = args[0].value.string;

  if (! nss_dir)
    {
      poppler_set_nss_dir (dir);
      nss_dir = g_strdup (dir);
    }
  else
    perror_if_not (! strcmp (nss_dir, dir),
                   "The NSS database %s is already open, restart epdfinfo"
                   " to use another one", *nss_dir ? nss_dir : "(default)");

  OK_BEGIN ();
  OK_END ();

 error:
  return;
}

/* Report a missing or wrong NSS password, if NSS asked for one. */
#define nss_perror_if_password()                                        \
  do {                                                                  \
    perror_if_not (! nss_password_wrong, "%s", "Wrong NSS password");   \
    perror_if_not (! nss_password_missing, "%s", "NSS password required"); \
  } while (0)

/* Name: signingcertificates
   Args: password
   Returns: The certificates available for signing, one per line:

   id subject subject-organization subject-email issuer expires

   where id is the certificate's nickname, subject and issuer are
   common names and expires is an ISO 8601 time.

   Errors: NSS password required, if the NSS database has a password
   and password is empty, Wrong NSS password, if it is the wrong one.
*/

const command_arg_type_t cmd_signingcertificates_spec[] =
  {
    ARG_STRING                  /* password */
  };

static void
cmd_signingcertificates (const epdfinfo_t *ctx, const command_arg_t *args)
{
  GList *certs, *item;

  nss_begin (args[0].value.string);
  certs = poppler_get_available_signing_certificates ();
  nss_perror_if_password ();

  OK_BEGIN ();
  for (item = certs; item; item = item->next)
    {
      PopplerCertificateInfo *cert = item->data;

      print_response_string (poppler_certificate_info_get_id (cert), COLON);
      print_response_string (poppler_certificate_info_get_subject_common_name (cert), COLON);
      print_response_string (poppler_certificate_info_get_subject_organization (cert), COLON);
      print_response_string (poppler_certificate_info_get_subject_email (cert), COLON);
      print_response_string (poppler_certificate_info_get_issuer_common_name (cert), COLON);
      print_date_time (poppler_certificate_info_get_expiration_time (cert), NEWLINE);
    }
  OK_END ();

 error:
  nss_end ();
  g_list_free_full (certs, (GDestroyNotify) poppler_certificate_info_free);
}

typedef struct
{
  gboolean done;
  gboolean ok;
  GError *error;
} sign_result_t;

static void
sign_ready (GObject *source, GAsyncResult *result, gpointer user_data)
{
  sign_result_t *res = user_data;

  res->ok = poppler_document_sign_finish (POPPLER_DOCUMENT (source),
                                          result, &res->error);
  res->done = TRUE;
}

/* Name: sign
   Args: filename page edges certificate field destination reason
         location text text-left password
   Returns: Nothing
   Errors: If the page or certificate does not exist, if the NSS
   password is missing or wrong (see signingcertificates), or if
   signing fails.

   Writes a copy of the document to destination, with a new signature
   field named field in the rectangle edges of page, signed with the
   certificate whose id (nickname) is certificate.  Text is shown in
   the field and text-left on its left half.  Reason, location and
   the texts may be empty.  The document itself is not changed.
*/

const command_arg_type_t cmd_sign_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGES,                  /* rectangle */
    ARG_NONEMPTY_STRING,        /* certificate id */
    ARG_NONEMPTY_STRING,        /* field name */
    ARG_NONEMPTY_STRING,        /* destination filename */
    ARG_STRING,                 /* reason */
    ARG_STRING,                 /* location */
    ARG_STRING,                 /* signature text */
    ARG_STRING,                 /* signature text, left */
    ARG_STRING                  /* NSS password */
  };

static void
cmd_sign (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerRectangle r = args[2].value.rectangle;
  const char *cert_id = args[3].value.string;
  const char *reason = args[6].value.string;
  const char *location = args[7].value.string;
  const char *text = args[8].value.string;
  const char *text_left = args[9].value.string;
  PopplerPage *page = NULL;
  PopplerCertificateInfo *cert = NULL;
  PopplerSigningData *data = NULL;
  PopplerRectangle rect;
  sign_result_t res = {0};
  double width, height;

  nss_begin (args[10].value.string);
  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);
  cert = poppler_get_certificate_info_by_id (cert_id);
  nss_perror_if_password ();
  perror_if_not (cert, "No such signing certificate: %s", cert_id);

  /* PDF coordinates start at the bottom of the page. */
  rect.x1 = MIN (r.x1, r.x2) * width;
  rect.x2 = MAX (r.x1, r.x2) * width;
  rect.y1 = height - MAX (r.y1, r.y2) * height;
  rect.y2 = height - MIN (r.y1, r.y2) * height;

  data = poppler_signing_data_new ();
  poppler_signing_data_set_destination_filename (data, args[5].value.string);
  poppler_signing_data_set_certificate_info (data, cert);
  poppler_signing_data_set_page (data, pn - 1);
  poppler_signing_data_set_signature_rectangle (data, &rect);
  poppler_signing_data_set_field_partial_name (data, args[4].value.string);
  if (*reason)
    poppler_signing_data_set_reason (data, reason);
  if (*location)
    poppler_signing_data_set_location (data, location);
  /* Older poppler versions need both texts, even if empty. */
  poppler_signing_data_set_signature_text (data, text);
  poppler_signing_data_set_signature_text_left (data, text_left);
  poppler_signing_data_set_password (data, args[10].value.string);

  /* Signing runs in a thread; wait for it. */
  poppler_document_sign (doc, data, NULL, sign_ready, &res);
  while (! res.done)
    g_main_context_iteration (NULL, TRUE);
  nss_perror_if_password ();
  perror_if_not (res.ok, "Unable to sign the document: %s",
                 res.error ? res.error->message
                 : "wrong password or unusable certificate?");

  OK_BEGIN ();
  OK_END ();

 error:
  nss_end ();
  if (res.error) g_error_free (res.error);
  if (data) poppler_signing_data_free (data);
  if (cert) poppler_certificate_info_free (cert);
  if (page) g_object_unref (page);
}

#endif  /* HAVE_POPPLER_SIGNING */


const command_arg_type_t cmd_synctex_forward_search_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING,        /* source file */
    ARG_NATNUM,                 /* line number */
    ARG_NATNUM                  /* column number */
  };

static void
cmd_synctex_forward_search (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  const char *source = args[1].value.string;
  int line = args[2].value.natnum;
  int column = args[3].value.natnum;
  synctex_scanner_p scanner = NULL;
  synctex_node_p node;
  float x1, y1, x2, y2;
  PopplerPage *page = NULL;
  double width, height;
  int pn;

  scanner = synctex_scanner_new_with_output_file (doc->filename, NULL, 1);
  perror_if_not (scanner, "Unable to create synctex scanner,\
 did you run latex with `--synctex=1' ?");

  perror_if_not (synctex_display_query (scanner, source, line, column, 0)
                && (node = synctex_scanner_next_result (scanner)),
                "Destination not found");

  pn = synctex_node_page (node);
  page = poppler_document_get_page(doc->pdf, pn - 1);
  perror_if_not (page, "Page not found");
  x1 =  synctex_node_box_visible_h (node);
  y1 =  synctex_node_box_visible_v (node)
        - synctex_node_box_visible_height (node);
  x2 = synctex_node_box_visible_width (node) + x1;
  y2 = synctex_node_box_visible_depth (node)
       + synctex_node_box_visible_height (node) + y1;
  poppler_page_get_size (page, &width, &height);
  x1 /= width;
  y1 /= height;
  x2 /= width;
  y2 /= height;

  OK_BEGIN ();
  printf("%d:%f:%f:%f:%f\n", pn, x1, y1, x2, y2);
  OK_END ();

 error:
  if (page) g_object_unref (page);
  if (scanner) synctex_scanner_free (scanner);
}


const command_arg_type_t cmd_synctex_backward_search_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGE,                   /* x */
    ARG_EDGE                    /* y */
  };

static void
cmd_synctex_backward_search (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  int pn = args[1].value.natnum;
  double x = args[2].value.edge;
  double y = args[3].value.edge;
  synctex_scanner_p scanner = NULL;
  const char *filename;
  PopplerPage *page = NULL;
  synctex_node_p node;
  double width, height;
  int line, column;

  scanner = synctex_scanner_new_with_output_file (doc->filename, NULL, 1);
  perror_if_not (scanner, "Unable to create synctex scanner,\
 did you run latex with `--synctex=1' ?");

  page = poppler_document_get_page(doc->pdf, pn - 1);
  perror_if_not (page, "Page not found");
  poppler_page_get_size (page, &width, &height);
  x = x * width;
  y = y * height;

  if (! synctex_edit_query (scanner, pn, x, y)
      || ! (node = synctex_scanner_next_result (scanner))
      || ! (filename =
            synctex_scanner_get_name (scanner, synctex_node_tag (node))))
    {
      printf_error_response ("Destination not found");
      goto error;
    }

  line = synctex_node_line (node);
  column = synctex_node_column (node);

  OK_BEGIN ();
  print_response_string (filename, COLON);
  printf("%d:%d\n", line, column);
  OK_END ();

 error:
  if (page) g_object_unref (page);
  if (scanner) synctex_scanner_free (scanner);
}


const command_arg_type_t cmd_renderpage_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_NATNUM,                 /* width */
    ARG_REST,                   /* commands */
  };

static void
cmd_renderpage (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  int pn = args[1].value.natnum;
  int width = args[2].value.natnum;
  int nrest_args = args[3].value.rest.nargs;
  char * const *rest_args = args[3].value.rest.args;
  PopplerPage *page = poppler_document_get_page(doc->pdf, pn - 1);
  cairo_surface_t *surface = NULL;
  cairo_t *cr = NULL;
  command_arg_t rest_arg;
  gchar *error_msg = NULL;
  double pt_width, pt_height;
  PopplerColor fg = { 0, 0, 0 };
  PopplerColor bg = { 65535, 0, 0 };
  double alpha = 1.0;
  double line_width = 1.5;
  PopplerSelectionStyle selection_style = POPPLER_SELECTION_GLYPH;
  PopplerRectangle cb = {0.0, 0.0, 1.0, 1.0};
  int i = 0;

  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &pt_width, &pt_height);
  surface = image_render_page (doc->pdf, page, width, 1,
                               &doc->options.render);
  perror_if_not (surface, "Failed to render page %d", pn);

  if (! nrest_args)
    goto theend;

  cr = cairo_create (surface);
  cairo_scale (cr, width / pt_width, width / pt_width);

  while (i < nrest_args)
    {
      const char* keyword;

      perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                            ARG_STRING, &error_msg),
                     "%s", error_msg);
      keyword = rest_arg.value.string;
      ++i;

      perror_if_not (i < nrest_args, "Keyword is `%s' missing an argument",
                     keyword);

      if (! strcmp (keyword, ":foreground")
          || ! strcmp (keyword, ":background"))
        {
          perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                                ARG_COLOR, &error_msg),
                         "%s", error_msg);
          ++i;
          if (! strcmp (keyword, ":foreground"))
            fg = rest_arg.value.color;
          else
            bg = rest_arg.value.color;

        }
      else if (! strcmp (keyword, ":alpha"))
        {
          perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                                ARG_EDGE, &error_msg),
                         "%s", error_msg);
          ++i;
          alpha = rest_arg.value.edge;
        }
      else if (! strcmp (keyword, ":crop-to")
               || ! strcmp (keyword, ":highlight-region")
               || ! strcmp (keyword, ":highlight-text")
               || ! strcmp (keyword, ":highlight-line"))
        {
          PopplerRectangle *r;
          perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                                ARG_EDGES, &error_msg),
                         "%s", error_msg);

          ++i;
          r = &rest_arg.value.rectangle;

          if (! strcmp (keyword, ":crop-to"))
            {
              gdouble w = (cb.x2 - cb.x1);
              gdouble h = (cb.y2 - cb.y1);
              gdouble x1 = cb.x1;
              gdouble y1 = cb.y1;

              cb.x1 = r->x1 * w + x1;
              cb.x2 = r->x2 * w + x1;
              cb.y1 = r->y1 * h + y1;
              cb.y2 = r->y2 * h + y1;

            }
          else
            {
              r->x1 = pt_width * r->x1 * (cb.x2 - cb.x1) + pt_width * cb.x1;
              r->x2 = pt_width * r->x2 * (cb.x2 - cb.x1) + pt_width * cb.x1;
              r->y1 = pt_height * r->y1 * (cb.y2 - cb.y1) + pt_height * cb.y1;
              r->y2 = pt_height * r->y2 * (cb.y2 - cb.y1) + pt_height * cb.y1;

              if (! strcmp (keyword, ":highlight-region"))
                {
                  const double deg = M_PI / 180.0;
                  double rad;

                  r->x1 += line_width / 2;
                  r->x2 -= line_width / 2;
                  r->y1 += line_width / 2;
                  r->y2 -= line_width / 2;

                  rad = MIN (5, MIN (r->x2 - r->x1, r->y2 - r->y1) / 2.0);

                  cairo_move_to (cr, r->x1 , r->y1 + rad);
                  cairo_arc (cr, r->x1 + rad, r->y1 + rad, rad, 180 * deg, 270 * deg);
                  cairo_arc (cr, r->x2 - rad, r->y1 + rad, rad, 270 * deg, 360 * deg);
                  cairo_arc (cr, r->x2 - rad, r->y2 - rad, rad, 0 * deg, 90 * deg);
                  cairo_arc (cr, r->x1 + rad, r->y2 - rad, rad, 90 * deg, 180 * deg);
                  cairo_close_path (cr);

                  cairo_set_source_rgba (cr,
                                         bg.red / 65535.0,
                                         bg.green / 65535.0,
                                         bg.blue / 65535.0, alpha);
                  cairo_fill_preserve (cr);
                  cairo_set_source_rgba (cr,
                                         fg.red / 65535.0,
                                         fg.green / 65535.0,
                                         fg.blue / 65535.0, 1.0);
                  cairo_set_line_width (cr, line_width);
                  cairo_stroke (cr);
                }
              else
                {
                  gboolean is_single_line = ! strcmp (keyword, ":highlight-line");

                  if (is_single_line)
                    {
                      gdouble m = r->y1 + (r->y2 - r->y1) / 2;

                      /* Make the rectangle flat, otherwise poppler frequently
                         renders neighboring lines.*/
                      r->y1 = m;
                      r->y2 = m;
                    }

                  poppler_page_render_selection (page, cr, r, NULL,
                                                 selection_style, &fg, &bg);
                }
            }
        }
      else if (! strcmp (keyword, ":selection-style"))
        {
          perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                                ARG_NATNUM, &error_msg),
                         "%s", error_msg);
          ++i;
	  selection_style = xpoppler_validate_selection_style (rest_arg.value.natnum);
        }
      else
        perror_if_not (0, "Unknown render command: %s", keyword);
    }
  if (cb.x1 != 0 || cb.y1 != 0 || cb.x2 != 1 || cb.y2 != 1)
    {
      int height = cairo_image_surface_get_height (surface);
      cairo_rectangle_int_t r = {(int) (width * cb.x1 + 0.5),
                                 (int) (height * cb.y1 + 0.5),
                                 (int) (width * (cb.x2 - cb.x1) + 0.5),
                                 (int) (height * (cb.y2 - cb.y1) + 0.5)};
      cairo_surface_t *nsurface =
        cairo_image_surface_create (CAIRO_FORMAT_ARGB32, r.width, r.height);
      perror_if_not (cairo_surface_status (surface) == CAIRO_STATUS_SUCCESS,
                     "%s", "Failed to create cairo surface");
      cairo_destroy (cr);
      cr = cairo_create (nsurface);
      perror_if_not (cairo_status (cr) == CAIRO_STATUS_SUCCESS,
                     "%s", "Failed to create cairo context");
      cairo_set_source_surface (cr, surface, -r.x, -r.y);
      cairo_paint (cr);
      cairo_surface_destroy (surface);
      surface = nsurface;
    }

 theend:
  image_write_print_response (surface, PNG);

 error:
  if (error_msg) g_free (error_msg);
  if (cr) cairo_destroy (cr);
  if (surface) cairo_surface_destroy (surface);
  if (page) g_object_unref (page);
}

const command_arg_type_t cmd_boundingbox_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    /* region */
  };

static void
cmd_boundingbox (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  int pn = args[1].value.natnum;
  PopplerPage *page = poppler_document_get_page(doc->pdf, pn - 1);
  cairo_surface_t *surface = NULL;
  int width, height;
  double pt_width, pt_height;
  unsigned char *data, *data_p;
  PopplerRectangle bbox;
  int i, j;

  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &pt_width, &pt_height);
  surface = image_render_page (doc->pdf, page, (int) pt_width, 1,
                               &doc->options.render);

  perror_if_not (cairo_surface_status(surface) == CAIRO_STATUS_SUCCESS,
                "Failed to render page");

  width = cairo_image_surface_get_width (surface);
  height = cairo_image_surface_get_height (surface);
  data = cairo_image_surface_get_data (surface);

  /* Determine the bbox by comparing each pixel in the 4 corner
     stripes with the origin. */
  for (i = 0; i < width; ++i)
    {
      data_p = data + 4 * i;
      for (j = 0; j < height; ++j, data_p += 4 * width)
        {
          if (! ARGB_EQUAL (data, data_p))
            break;
        }
      if (j < height)
        break;
    }
  bbox.x1 = i;

  for (i = width - 1; i > -1; --i)
    {
      data_p = data + 4 * i;
      for (j = 0; j < height; ++j, data_p += 4 * width)
        {
          if (! ARGB_EQUAL (data, data_p))
            break;
        }
      if (j < height)
        break;
    }
  bbox.x2 = i + 1;

  for (i = 0; i < height; ++i)
    {
      data_p = data + 4 * i * width;
      for (j = 0; j < width; ++j, data_p += 4)
        {
          if (! ARGB_EQUAL (data, data_p))
            break;
        }
      if (j < width)
        break;
    }
  bbox.y1 = i;

  for (i = height - 1; i > -1; --i)
    {
      data_p = data + 4 * i * width;
      for (j = 0; j < width; ++j, data_p += 4)
        {
          if (! ARGB_EQUAL (data, data_p))
            break;
        }
      if (j < width)
        break;
    }
  bbox.y2 = i + 1;

  OK_BEGIN ();
  if (bbox.x1 >= bbox.x2 || bbox.y1 >= bbox.y2)
    {
      /* empty page */
      puts ("0:0:1:1");
    }
  else
    {
      printf ("%f:%f:%f:%f\n",
              bbox.x1 / width,
              bbox.y1 / height,
              bbox.x2 / width,
              bbox.y2 / height);
    }
  OK_END ();

 error:
  if (surface) cairo_surface_destroy (surface);
  if (page) g_object_unref (page);
}

const command_arg_type_t cmd_charlayout_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGES_OR_POSITION,      /* region or position */
  };

static void
cmd_charlayout(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerRectangle region = args[2].value.rectangle;
  double width, height;
  PopplerPage *page = poppler_document_get_page(doc, pn - 1);
  char *text = NULL;
  char *text_p;
  PopplerRectangle *rectangles = NULL;
  guint nrectangles;
  int i;
  gboolean have_position = region.y2 < 0;

  perror_if_not (page, "No such page %d", pn);

  text = poppler_page_get_text (page);
  text_p = text;
  poppler_page_get_text_layout (page, &rectangles, &nrectangles);
  poppler_page_get_size (page, &width, &height);
  region.x1 *= width;
  region.x2 *= width;
  region.y1 *= height;
  region.y2 *= height;

  OK_BEGIN ();
  for (i = 0; i < nrectangles && *text_p; ++i)
    {
      PopplerRectangle *r = &rectangles[i];
      char *nextc = g_utf8_offset_to_pointer (text_p, 1);

      if ((have_position
           && region.x1 >= r->x1
           && region.x1 <= r->x2
           && region.y1 >= r->y1
           && region.y1 <= r->y2)
          || (! have_position
              && r->x1 >= region.x1
              && r->y1 >= region.y1
              && r->x2 <= region.x2
              && r->y2 <= region.y2))
        {
          char endc = *nextc;

          printf ("%f %f %f %f:",
                  r->x1 / width, r->y1 / height,
                  r->x2 / width, r->y2 / height);
          *nextc = '\0';
          print_response_string (text_p, NEWLINE);
          *nextc = endc;
        }
      text_p = nextc;
    }
  OK_END ();

  g_free (rectangles);
  g_object_unref (page);
  g_free (text);

 error:
  return;
}

const document_option_t document_options [] =
  {
    DEC_DOPT (":render/usecolors", ARG_NATNUM, render.usecolors),
    DEC_DOPT (":render/printed", ARG_BOOL, render.printed),
    DEC_DOPT (":render/foreground", ARG_COLOR, render.fg),
    DEC_DOPT (":render/background", ARG_COLOR, render.bg),
    DEC_DOPT (":render/gamma", ARG_DOUBLE, render.gamma),
    DEC_DOPT (":render/gammabeforeinvert", ARG_BOOL, render.gammabeforeinvert),
  };

const command_arg_type_t cmd_getoptions_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_getoptions(const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  int i;
  OK_BEGIN ();
  for (i = 0; i < G_N_ELEMENTS (document_options); ++i)
    {
      command_arg_t arg;

      arg.type = document_options[i].type;
      memcpy (&arg.value,
              ((char*) &doc->options) + document_options[i].offset,
              command_arg_type_size (arg.type));
      print_response_string (document_options[i].name, COLON);
      command_arg_print (&arg);
      puts("");
    }
  OK_END ();
}

const command_arg_type_t cmd_setoptions_spec[] =
  {
    ARG_DOC,
    ARG_REST                    /* key value pairs */
  };

static void
cmd_setoptions(const epdfinfo_t *ctx, const command_arg_t *args)
{
  int i = 0;
  document_t *doc = args[0].value.doc;
  int nrest = args[1].value.rest.nargs;
  char * const *rest = args[1].value.rest.args;
  gchar *error_msg = NULL;
  document_options_t opts = doc->options;
  const size_t nopts = G_N_ELEMENTS (document_options);

  perror_if_not (nrest % 2 == 0, "Even number of key/value pairs expected");

  while (i < nrest)
    {
      int j;
      command_arg_t key, value;

      perror_if_not (command_arg_parse_arg
                     (ctx, rest[i], &key, ARG_NONEMPTY_STRING, &error_msg),
                     "%s", error_msg);

      ++i;
      for (j = 0; j < nopts; ++j)
        {
          const document_option_t *dopt = &document_options[j];
          if (! strcmp (key.value.string, dopt->name))
            {
              perror_if_not (command_arg_parse_arg
                             (ctx, rest[i], &value, dopt->type, &error_msg),
                             "%s", error_msg);
              memcpy (((char*) &opts) + dopt->offset,
                      &value.value, command_arg_type_size (value.type));
              break;
            }
        }
      perror_if_not (j < nopts, "Unknown option: %s", key.value.string);
      ++i;
    }
  doc->options = opts;
  cmd_getoptions (ctx, args);

 error:
  if (error_msg) g_free (error_msg);
}

const command_arg_type_t cmd_pagelabels_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_pagelabels(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int i;

  OK_BEGIN ();
  for (i = 0; i < poppler_document_get_n_pages (doc); ++i)
    {
      PopplerPage *page = poppler_document_get_page(doc, i);
      gchar *label = poppler_page_get_label (page);

      print_response_string (label ? label : "", NEWLINE);
      g_object_unref (page);
      g_free (label);
    }
  OK_END ();
}

const command_arg_type_t cmd_ping_spec[] =
  {
    ARG_STRING                  /* any message */
  };

static void
cmd_ping (const epdfinfo_t *ctx, const command_arg_t *args)
{
  const gchar *msg = args[0].value.string;
  OK_BEGIN ();
  print_response_string (msg, NEWLINE);
  OK_END ();
}


/* ================================================================== *
 * Main
 * ================================================================== */

static const command_t commands [] =
  {
    /* Basic */
    DEC_CMD (ping),
    DEC_CMD (features),
    DEC_CMD (open),
    DEC_CMD (close),
    DEC_CMD (quit),
    DEC_CMD (getoptions),
    DEC_CMD (setoptions),

    /* Searching */
    DEC_CMD2 (search_string, "search-string"),
    DEC_CMD2 (search_regexp, "search-regexp"),
    DEC_CMD2 (regexp_flags, "regexp-flags"),

    /* General Information */
    DEC_CMD (metadata),
    DEC_CMD (outline),
    DEC_CMD2 (number_of_pages, "number-of-pages"),
    DEC_CMD (pagelinks),
    DEC_CMD (gettext),
    DEC_CMD (getselection),
    DEC_CMD (pagesize),
    DEC_CMD (boundingbox),
    DEC_CMD (charlayout),

    /* General Information */
    DEC_CMD (metadata),
    DEC_CMD (outline),
    DEC_CMD2 (number_of_pages, "number-of-pages"),
    DEC_CMD (pagelinks),
    DEC_CMD (gettext),
    DEC_CMD (getselection),
    DEC_CMD (pagesize),
    DEC_CMD (boundingbox),
    DEC_CMD (charlayout),
    DEC_CMD (pagelabels),
#ifdef HAVE_POPPLER_METADATA_WRITE
    DEC_CMD (setmetadata),
#endif

    /* Accessibility */
#ifdef HAVE_POPPLER_STRUCTURE
    DEC_CMD (structure),
#endif
    DEC_CMD (fonts),
    DEC_CMD (xmpmetadata),

    /* Layers */
    DEC_CMD (layers),
    DEC_CMD (setlayer),
    DEC_CMD (ocgstate),

    /* Annotations */
    DEC_CMD (getannots),
    DEC_CMD (getannot),
#ifdef HAVE_POPPLER_ANNOT_WRITE
    DEC_CMD (addannot),
    DEC_CMD (delannot),
    DEC_CMD (editannot),
    DEC_CMD (save),
#endif

    /* Attachments */
    DEC_CMD2 (getattachment_from_annot, "getattachment-from-annot"),
    DEC_CMD (getattachments),

    /* Forms */
    DEC_CMD (getformfields),
    DEC_CMD (setformfield),
    DEC_CMD2 (setformfield_text, "setformfield-text"),
#ifdef HAVE_POPPLER_FORM_RESET
    DEC_CMD (resetform),
#endif
#ifdef HAVE_POPPLER_SIGNATURES
    DEC_CMD (validatesignature),
#endif
#ifdef HAVE_POPPLER_SIGNING
    DEC_CMD (setnssdir),
    DEC_CMD (signingcertificates),
    DEC_CMD (sign),
#endif

    /* Synctex */
    DEC_CMD2 (synctex_forward_search, "synctex-forward-search"),
    DEC_CMD2 (synctex_backward_search, "synctex-backward-search"),

    /* Rendering */
    DEC_CMD (renderpage),
  };

int main(int argc, char **argv)
{
  epdfinfo_t ctx = {0};
  char *line = NULL;
  ssize_t read;
  size_t line_size;
  const char *error_log = "/dev/null";

#ifdef __MINGW32__
  error_log = "NUL";
  _setmode(_fileno(stdin), _O_BINARY);
  _setmode(_fileno(stdout), _O_BINARY);
#endif

  if (argc > 2)
    {
      fprintf(stderr, "usage: epdfinfo [ERROR-LOGFILE]\n");
      exit (EXIT_FAILURE);
    }
  if (argc == 2)
    error_log = argv[1];

  if (! freopen (error_log, "a", stderr))
    err (2, "Unable to redirect stderr");

#if ! GLIB_CHECK_VERSION(2,36,0)
  g_type_init ();
#endif

  ctx.documents = g_hash_table_new (g_str_hash, g_str_equal);

  setvbuf (stdout, NULL, _IOFBF, BUFSIZ);

  while ((read = getline (&line, &line_size, stdin)) != -1)
    {
      int nargs = 0;
      command_arg_t *cmd_args = NULL;
      char **args = NULL;
      gchar *error_msg = NULL;
      int i;

      if (read <= 1 || line[read - 1] != '\n')
        {
          fprintf (stderr, "Skipped parts of a line: `%s'\n", line);
          goto next_line;
        }

      line[read - 1] = '\0';
      args = command_arg_split (line, &nargs);
      if (nargs == 0)
        continue;

      for (i = 0; i < G_N_ELEMENTS (commands);  i++)
        {
          if (! strcmp (commands[i].name, args[0]))
            {
              if (commands[i].nargs == 0
                  || (cmd_args = command_arg_parse (&ctx, args + 1, nargs - 1,
                                                    commands + i, &error_msg)))
                {
                  commands[i].execute (&ctx, cmd_args);
                  if (commands[i].nargs > 0)
                    free_command_args (cmd_args, commands[i].nargs);
                }
              else
                {
                  printf_error_response ("%s", error_msg ? error_msg :
                                         "Unknown error (this is a bug)");
                }
              if (error_msg)
                g_free (error_msg);
              break;
            }
        }
      if (G_N_ELEMENTS (commands) == i)
        {
          printf_error_response ("Unknown command: %s", args[0]);
        }
      for (i = 0; i < nargs; ++i)
        g_free (args[i]);
      g_free (args);
    next_line:
      free (line);
      line = NULL;
    }

  if (ferror (stdin))
    err (2, NULL);
  exit (EXIT_SUCCESS);
}
