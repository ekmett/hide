#include "unicode.h"
#include <string.h>
#include <utf8proc.h>

int thc_graphemes(const char *utf8, int bytes, int *boundaries) {
    int count=0, offset=0;
    utf8proc_int32_t previous=-1, state=0, point;
    for (int i=0;i<bytes;) {
        utf8proc_ssize_t n=utf8proc_iterate((const utf8proc_uint8_t *)utf8+i, bytes-i, &point);
        if (n<0) return 0;
        if (previous<0 || utf8proc_grapheme_break_stateful(previous,point,&state)) boundaries[count++]=offset;
        previous=point; i+=(int)n; ++offset;
    }
    boundaries[count++]=offset;
    return count;
}

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#ifdef WITH_WINDOW
#include <CoreText/CoreText.h>
#endif
#ifdef WITH_WINDOW
int thc_unicode_bitmap(const char *utf8, int w, int h, uint32_t fg, uint32_t *pixels) {
    CFStringRef s = CFStringCreateWithCString(NULL, utf8, kCFStringEncodingUTF8);
    if (!s) return 0;
    CTFontRef font = CTFontCreateWithName(CFSTR("Menlo"), h * 0.85, NULL);
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
        CGFloat sx = ink.size.width > w ? w / ink.size.width : 1;
        CGFloat sy = ink.size.height > h ? h / ink.size.height : 1;
        CGContextScaleCTM(ctx, sx, sy);
        CGContextSetTextPosition(ctx, (w/sx-ink.size.width)/2-ink.origin.x, (h/sy-ink.size.height)/2-ink.origin.y);
        CTLineDraw(line, ctx);
        CGContextRelease(ctx);
    }
    CFRelease(line); CFRelease(text); CFRelease(attrs);
    CGColorRelease(color); CGColorSpaceRelease(space); CFRelease(font); CFRelease(s);
    return ctx != NULL;
}
#endif
#else
#include <pango/pango.h>
#ifdef WITH_WINDOW
#include <pango/pangocairo.h>
#endif
#ifdef WITH_WINDOW
int thc_unicode_bitmap(const char *utf8, int w, int h, uint32_t fg, uint32_t *pixels) {
    cairo_surface_t *surface = cairo_image_surface_create_for_data((unsigned char *)pixels, CAIRO_FORMAT_ARGB32, w, h, w*4);
    cairo_t *cr = cairo_create(surface);
    PangoLayout *layout = pango_cairo_create_layout(cr);
    PangoFontDescription *font = pango_font_description_from_string("monospace");
    pango_font_description_set_absolute_size(font, h*0.85*PANGO_SCALE);
    pango_layout_set_font_description(layout, font);
    pango_layout_set_text(layout, utf8, -1);
    PangoRectangle ink;
    pango_layout_get_pixel_extents(layout, &ink, NULL);
    double sx = ink.width > w ? (double)w/ink.width : 1;
    double sy = ink.height > h ? (double)h/ink.height : 1;
    cairo_scale(cr, sx, sy);
    cairo_move_to(cr, (w/sx-ink.width)/2-ink.x, (h/sy-ink.height)/2-ink.y);
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
