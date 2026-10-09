// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
#include "unicode.h"
#include <string.h>
#include <utf8proc.h>

uint64_t thc_grapheme_step(int previous, int current, int state) {
    utf8proc_int32_t next=state;
    int boundary=utf8proc_grapheme_break_stateful(previous,current,&next);
    return ((uint64_t)(uint32_t)next<<1) | (boundary!=0);
}

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#ifdef WITH_WINDOW
#include <CoreText/CoreText.h>
#endif
#ifdef WITH_WINDOW
int thc_unicode_bitmap(const char *utf8, int w, int h, uint32_t fg, uint32_t traits, uint32_t *pixels) {
    CFStringRef s = CFStringCreateWithCString(NULL, utf8, kCFStringEncodingUTF8);
    if (!s) return 0;
    CTFontRef font = CTFontCreateWithName(CFSTR("Menlo"), h * 0.85, NULL);
    CTFontSymbolicTraits mask=((traits&1)?kCTFontBoldTrait:0)|((traits&2)?kCTFontItalicTrait:0);
    if (mask) {
        CTFontRef styled=CTFontCreateCopyWithSymbolicTraits(font,0,NULL,mask,mask);
        if (styled) { CFRelease(font); font=styled; }
    }
    CGColorSpaceRef space = CGColorSpaceCreateDeviceRGB();
    CGFloat rgba[] = {((fg>>16)&255)/255.0, ((fg>>8)&255)/255.0, (fg&255)/255.0, 1};
    CGColorRef color = CGColorCreate(space, rgba);
    const void *keys[] = {kCTFontAttributeName, kCTForegroundColorAttributeName};
    const void *values[] = {font, color};
    CFDictionaryRef attrs = CFDictionaryCreate(NULL, keys, values, 2, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    CFAttributedStringRef text = CFAttributedStringCreate(NULL, s, attrs);
    CTLineRef line = CTLineCreateWithAttributedString(text);
    CGContextRef ctx = CGBitmapContextCreate(pixels, w, h, 8, w*4, space, kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host);
    if (ctx) {
        CGRect ink = CTLineGetBoundsWithOptions(line, kCTLineBoundsUseGlyphPathBounds);
        CGFloat logical_w=(traits&4)?w/2.0:w;
        CGFloat sx = ink.size.width > logical_w ? logical_w / ink.size.width : 1;
        CGFloat sy = ink.size.height > h ? h / ink.size.height : 1;
        CGContextScaleCTM(ctx, sx*((traits&4)?2:1), sy);
        CGContextSetTextPosition(ctx, (logical_w/sx-ink.size.width)/2-ink.origin.x, (h/sy-ink.size.height)/2-ink.origin.y);
        CTLineDraw(line, ctx);
        CGContextRelease(ctx);
    }
    CFRelease(line); CFRelease(text); CFRelease(attrs);
    CGColorRelease(color); CGColorSpaceRelease(space); CFRelease(font); CFRelease(s);
    return ctx != NULL;
}
#endif
#else
#ifdef WITH_WINDOW
#include <pango/pango.h>
#include <pango/pangocairo.h>
#endif
#ifdef WITH_WINDOW
int thc_unicode_bitmap(const char *utf8, int w, int h, uint32_t fg, uint32_t traits, uint32_t *pixels) {
    cairo_surface_t *surface = cairo_image_surface_create_for_data((unsigned char *)pixels, CAIRO_FORMAT_ARGB32, w, h, w*4);
    cairo_t *cr = cairo_create(surface);
    PangoLayout *layout = pango_cairo_create_layout(cr);
    PangoFontDescription *font = pango_font_description_from_string("monospace");
    pango_font_description_set_absolute_size(font, h*0.85*PANGO_SCALE);
    pango_font_description_set_weight(font,(traits&1)?PANGO_WEIGHT_BOLD:PANGO_WEIGHT_NORMAL);
    pango_font_description_set_style(font,(traits&2)?PANGO_STYLE_ITALIC:PANGO_STYLE_NORMAL);
    pango_layout_set_font_description(layout, font);
    pango_layout_set_text(layout, utf8, -1);
    PangoRectangle ink;
    pango_layout_get_pixel_extents(layout, &ink, NULL);
    double logical_w=(traits&4)?w/2.0:w;
    double sx = ink.width > logical_w ? logical_w/ink.width : 1;
    double sy = ink.height > h ? (double)h/ink.height : 1;
    cairo_scale(cr, sx*((traits&4)?2:1), sy);
    cairo_move_to(cr, (logical_w/sx-ink.width)/2-ink.x, (h/sy-ink.height)/2-ink.y);
    cairo_set_source_rgb(cr, ((fg>>16)&255)/255.0, ((fg>>8)&255)/255.0, (fg&255)/255.0);
    pango_cairo_show_layout(cr, layout);
    cairo_surface_flush(surface);
    int ok = cairo_status(cr) == CAIRO_STATUS_SUCCESS;
    pango_font_description_free(font); g_object_unref(layout);
    cairo_destroy(cr); cairo_surface_destroy(surface);
    return ok;
}
#endif
#endif

#ifdef WITH_WINDOW
#include <math.h>
#include <stdlib.h>

static double linear_channel(double s) {
    return s<=0.04045 ? s/12.92 : pow((s+0.055)/1.055,2.4);
}
static unsigned display_channel(double linear) {
    linear=fmax(0,fmin(1,linear));
    double s=linear<=0.0031308 ? linear*12.92 : 1.055*pow(linear,1/2.4)-0.055;
    return (unsigned)lround(255*s);
}

int thc_unicode_downsample(const uint32_t *source, int w, int h, uint32_t *pixels) {
    if (w<1 || h<1 || w>1024 || h>1024) return 0;
    size_t stride=(size_t)(w+2)*4;
    double *errors=calloc(2*stride,sizeof(double));
    if (!errors) return 0;
    double *current=errors, *next=errors+stride;
    for (int y=0;y<h;++y) {
        for (int x=0;x<w;++x) {
            double value[4]={0};
            /* Average premultiplied linear-light color and coverage, so
             * transparent edges neither darken colors nor erase thin strokes. */
            for (int dy=0;dy<4;++dy) for (int dx=0;dx<4;++dx) {
                uint32_t p=source[(y*4+dy)*w*4+x*4+dx];
                double a=(p>>24)/255.0;
                value[3]+=a/16;
                if (a>0) for (int c=0;c<3;++c)
                    value[c]+=a*linear_channel(fmin(1,((p>>(16-8*c))&255)/(255*a)))/16;
            }
            for (int c=0;c<4;++c) value[c]+=current[(x+1)*4+c];
            double chosen[4]={0};
            uint32_t p=0;
            /* Four coverage levels keep thin text connected; binary coverage
             * makes small CJK and combining marks unnecessarily speckled. */
            double alpha=fmax(0,fmin(3,round(value[3]*3)))/3;
            if (alpha>0) {
                p=(uint32_t)lround(alpha*255)<<24; chosen[3]=alpha;
                for (int c=0;c<3;++c) {
                    unsigned channel=(unsigned)lround(display_channel(value[c]/value[3])*alpha);
                    p|=channel<<(16-8*c);
                    chosen[c]=alpha*linear_channel(channel/(255*alpha));
                }
            }
            pixels[y*w+x]=p;
            /* Floyd–Steinberg: right, below-left, below, below-right. */
            for (int c=0;c<4;++c) {
                double error=(value[c]-chosen[c])/16;
                current[(x+2)*4+c]+=error*7;
                next[x*4+c]+=error*3;
                next[(x+1)*4+c]+=error*5;
                next[(x+2)*4+c]+=error;
            }
        }
        double *swap=current; current=next; next=swap;
        memset(next,0,stride*sizeof(double));
    }
    free(errors);
    return 1;
}

int thc_unicode_pixelated(const char *text, int w, int h, uint32_t fg, uint32_t traits, uint32_t *pixels) {
    if (w<1 || h<1 || w>1024 || h>1024) return 0;
    uint32_t *source=calloc((size_t)w*h*16,sizeof(uint32_t));
    if (!source) return 0;
    int ok=thc_unicode_bitmap(text,w*4,h*4,fg,traits,source) && thc_unicode_downsample(source,w,h,pixels);
    free(source);
    return ok;
}
#endif
