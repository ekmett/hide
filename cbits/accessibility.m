/* SPDX-License-Identifier: BSD-3-Clause */
#import <Cocoa/Cocoa.h>
#include "accessibility.h"
#include <math.h>

static BOOL integer(id value, double maximum) {
    if (![value isKindOfClass:NSNumber.class] || CFGetTypeID((__bridge CFTypeRef)value)==CFBooleanGetTypeID()) return NO;
    double n=[value doubleValue]; return isfinite(n) && n>=0 && n<=maximum && floor(n)==n;
}
static BOOL boolean(id value) { return [value isKindOfClass:NSNumber.class] && CFGetTypeID((__bridge CFTypeRef)value)==CFBooleanGetTypeID(); }
static BOOL text(id value, NSUInteger limit, BOOL nonempty) {
    if (![value isKindOfClass:NSString.class]) return NO;
    NSUInteger count=0;
    for (NSUInteger i=0;i<[value length];++i) {
        unichar c=[value characterAtIndex:i];
        if (!c) return NO;
        if (CFStringIsSurrogateHighCharacter(c)) {
            if (++i==[value length] || !CFStringIsSurrogateLowCharacter([value characterAtIndex:i])) return NO;
        } else if (CFStringIsSurrogateLowCharacter(c)) return NO;
        if (++count>limit) return NO;
    }
    return !nonempty || count>0;
}
static BOOL identity(id value) {
    if (![value isKindOfClass:NSArray.class]) return NO;
    NSArray *parts=value;
    if (parts.count==1) return [parts[0] isEqual:@"sidebar"];
    if (parts.count!=3 || ![parts[0] isEqual:@"tree"] || !text(parts[1],48,YES) || [parts[1] length]!=48 || !text(parts[2],128,YES)) return NO;
    return [parts[1] rangeOfCharacterFromSet:[[NSCharacterSet characterSetWithCharactersInString:@"0123456789abcdef"] invertedSet]].location==NSNotFound;
}
static BOOL nullableBoolean(id value) { return value==NSNull.null || boolean(value); }
static NSInteger rowIndex(NSDictionary *node) { return node[@"index"]==NSNull.null ? NSNotFound : [node[@"index"] integerValue]; }
static BOOL bounds(id value, NSInteger cols, NSInteger rows) {
    if (value==NSNull.null) return YES;
    if (![value isKindOfClass:NSArray.class] || [value count]!=4) return NO;
    NSArray *r=value;
    return integer(r[0],cols) && integer(r[1],rows) && integer(r[2],cols) && integer(r[3],rows) &&
           [r[2] integerValue]>0 && [r[3] integerValue]>0 && [r[0] integerValue]+[r[2] integerValue]<=cols && [r[1] integerValue]+[r[3] integerValue]<=rows;
}

@class HideAXHost;
@interface HideAXRow : NSAccessibilityElement <NSAccessibilityRow>
@property(weak) HideAXHost *host;
@property(weak) HideAXRow *discloser;
@property(copy) NSDictionary *record;
@property(copy) NSArray<HideAXRow *> *disclosedRows;
- (void)retire;
@end
@interface HideAXHost : NSView <NSAccessibilityOutline>
@property(copy) NSDictionary *record;
@property(copy) NSArray<HideAXRow *> *rows;
@property(copy) NSDictionary<NSArray *,HideAXRow *> *nodes;
@end
static HideAXHost *host;
static NSRect screenFrame(NSView *view, id value) {
    if (!view.window || value==NSNull.null || !value) return NSZeroRect;
    NSArray *r=value; double rectangle[4];
    if (!thc_accessibility_cell_rect([r[0] intValue],[r[1] intValue],[r[2] intValue],[r[3] intValue],rectangle)) return NSZeroRect;
    NSRect frame=NSMakeRect(rectangle[0],rectangle[1],rectangle[2],rectangle[3]);
    if (!view.isFlipped) frame.origin.y=NSHeight(view.bounds)-NSMaxY(frame);
    return NSAccessibilityFrameInView(view,frame);
}
static BOOL readOnlySelector(SEL selector) {
    NSString *name=NSStringFromSelector(selector);
    return ![name hasPrefix:@"setAccessibility"] && ![name hasPrefix:@"accessibilityPerform"];
}
@implementation HideAXRow
- (BOOL)isAccessibilityElement { return self.record!=nil; }
- (NSAccessibilityRole)accessibilityRole { return NSAccessibilityRowRole; }
- (NSString *)accessibilityLabel { return self.record[@"name"]; }
- (id)accessibilityParent { return self.host; }
- (NSRect)accessibilityFrame { return screenFrame(self.host,self.record[@"bounds"]); }
- (NSInteger)accessibilityIndex { return rowIndex(self.record); }
- (NSInteger)accessibilityDisclosureLevel { return [self.record[@"level"] integerValue]-1; }
- (BOOL)isAccessibilitySelected { return [self.record[@"selected"] boolValue]; }
- (BOOL)isAccessibilityDisclosed { return self.record[@"expanded"]!=NSNull.null && [self.record[@"expanded"] boolValue]; }
- (id)accessibilityDisclosedByRow { return self.discloser; }
- (id)accessibilityDisclosedRows { return self.disclosedRows; }
- (BOOL)isAccessibilityFocused { return NO; }
- (NSString *)accessibilityHelp {
    NSMutableArray *parts=[NSMutableArray new];
    if ([self.record[@"focused"] boolValue]) [parts addObject:@"Focused in editor"];
    if ([self.record[@"loading"] boolValue]) [parts addObject:@"Loading children"];
    if ([self.record[@"moreChildren"] boolValue]) [parts addObject:@"Children are not fully loaded"];
    return [parts componentsJoinedByString:@". "];
}
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector {
    return readOnlySelector(selector) && !(selector==@selector(accessibilityIndex) && self.record[@"index"]==NSNull.null) && [super isAccessibilitySelectorAllowed:selector];
}
- (void)retire { self.record=nil; self.disclosedRows=nil; self.discloser=nil; self.host=nil; }
@end
@implementation HideAXHost
- (BOOL)isFlipped { return YES; }
- (BOOL)acceptsFirstResponder { return NO; }
- (NSView *)hitTest:(NSPoint)point { (void)point; return nil; }
- (BOOL)isAccessibilityElement { return self.rows.count>0; }
- (NSAccessibilityRole)accessibilityRole { return NSAccessibilityOutlineRole; }
- (NSString *)accessibilityLabel { return self.record[@"name"]; }
- (NSString *)accessibilityHelp { return @"Read-only visible sidebar; use editor controls to open or change files."; }
- (NSRect)accessibilityFrame { return screenFrame(self,self.record[@"bounds"]); }
- (NSArray *)accessibilityChildren { return self.rows; }
- (NSArray *)accessibilityRows { return self.rows; }
- (NSArray *)accessibilityVisibleRows {
    NSMutableArray *visible=[NSMutableArray new]; for (HideAXRow *row in self.rows) if (row.record[@"bounds"]!=NSNull.null) [visible addObject:row]; return visible;
}
- (NSArray *)accessibilitySelectedRows {
    NSMutableArray *selected=[NSMutableArray new]; for (HideAXRow *row in self.rows) if (row.isAccessibilitySelected) [selected addObject:row]; return selected;
}
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector { return readOnlySelector(selector) && [super isAccessibilitySelectorAllowed:selector]; }
@end

/* Images share the renderer's clipped cell ownership, with no action or focus
 * selectors. Resource IDs and pixels never enter the accessibility adapter. */
@class HideAXCanvas;
@interface HideAXImage : NSAccessibilityElement
@property(weak) HideAXCanvas *host;
@property(copy) NSDictionary *record;
@end
@interface HideAXCanvas : NSView
@property(copy) NSArray<HideAXImage *> *images;
@property(copy) NSDictionary<NSNumber *,HideAXImage *> *nodes;
@end
static HideAXCanvas *canvasHost;
@implementation HideAXImage
- (BOOL)isAccessibilityElement { return self.record!=nil; }
- (NSAccessibilityRole)accessibilityRole { return NSAccessibilityImageRole; }
- (NSString *)accessibilityLabel { return self.record[@"name"]; }
- (NSString *)accessibilityHelp { return self.record[@"description"]; }
- (id)accessibilityParent { return self.host; }
- (NSRect)accessibilityFrame { return screenFrame(self.host,self.record[@"bounds"]); }
- (BOOL)isAccessibilityFocused { return NO; }
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector { return readOnlySelector(selector) && [super isAccessibilitySelectorAllowed:selector]; }
@end
@implementation HideAXCanvas
- (BOOL)isFlipped { return YES; }
- (BOOL)acceptsFirstResponder { return NO; }
- (NSView *)hitTest:(NSPoint)point { (void)point; return nil; }
- (BOOL)isAccessibilityElement { return self.images.count>0; }
- (NSAccessibilityRole)accessibilityRole { return NSAccessibilityGroupRole; }
- (NSString *)accessibilityLabel { return @"Images"; }
- (NSArray *)accessibilityChildren { return self.images; }
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector { return readOnlySelector(selector) && [super isAccessibilitySelectorAllowed:selector]; }
@end
static void clearCanvasAccessibility(void) {
    NSView *parent=canvasHost.superview;
    for (HideAXImage *image in canvasHost.images) { image.record=nil; image.host=nil; }
    canvasHost.images=nil; canvasHost.nodes=nil; [canvasHost removeFromSuperview]; canvasHost=nil;
    if (parent) NSAccessibilityPostNotification(parent,NSAccessibilityLayoutChangedNotification);
}
static int updateCanvasAccessibility(void *native_window,NSDictionary *snapshot) {
    NSArray *size=snapshot[@"size"],*values=snapshot[@"images"];
    if (![size isKindOfClass:NSArray.class] || size.count!=2 || !integer(size[0],512) || !integer(size[1],256) ||
        [size[0] integerValue]<1 || [size[1] integerValue]<1 || ![values isKindOfClass:NSArray.class] || values.count>64) goto invalid;
    @autoreleasepool {
        NSMutableSet *identities=[NSMutableSet new];
        for (NSDictionary *entry in values) {
            if (![entry isKindOfClass:NSDictionary.class] || !integer(entry[@"id"],2147483647) || ![entry[@"id"] integerValue] ||
                !text(entry[@"name"],256,NO) || !text(entry[@"description"],1024,NO) || entry[@"bounds"]==NSNull.null ||
                !bounds(entry[@"bounds"],[size[0] integerValue],[size[1] integerValue]) || [identities containsObject:entry[@"id"]]) goto invalid;
            [identities addObject:entry[@"id"]];
        }
        if (!values.count) { clearCanvasAccessibility(); return 1; }
        NSWindow *window=(__bridge NSWindow *)native_window;
        if (canvasHost && canvasHost.window!=window) clearCanvasAccessibility();
        if (!canvasHost) {
            canvasHost=[[HideAXCanvas alloc] initWithFrame:window.contentView.bounds];
            canvasHost.autoresizingMask=NSViewWidthSizable|NSViewHeightSizable;
            [window.contentView addSubview:canvasHost];
        }
        NSMutableDictionary *nodes=[NSMutableDictionary new]; NSMutableArray *images=[NSMutableArray new];
        for (NSDictionary *entry in values) {
            HideAXImage *image=canvasHost.nodes[entry[@"id"]] ?: [HideAXImage new];
            image.record=entry;image.host=canvasHost;nodes[entry[@"id"]]=image;[images addObject:image];
        }
        for (NSNumber *ident in canvasHost.nodes) if (!nodes[ident]) { canvasHost.nodes[ident].record=nil; canvasHost.nodes[ident].host=nil; }
        canvasHost.nodes=nodes;canvasHost.images=images;
        NSAccessibilityPostNotification(canvasHost,NSAccessibilityLayoutChangedNotification);
        return 1;
    }
invalid:
    clearCanvasAccessibility(); return 0;
}

/* A copied visible excerpt. Static text supplies no text-range, editing or
 * focus interface, and never reads source buffers or hidden editor state. */
@interface HideAXSource : NSView
@property(copy) NSDictionary *record;
@end
static HideAXSource *sourceHost;
static BOOL sourceIdentity(id value) {
    if (![value isKindOfClass:NSArray.class] || [value count]!=3 || ![value[0] isEqual:@"source"]) return NO;
    for (NSUInteger i=1;i<3;++i) {
        id part=value[i];
        if (!text(part,20,YES) || ([part length]>1 && [part characterAtIndex:0]=='0')) return NO;
        for (NSUInteger j=0;j<[part length];++j) {
            unichar c=[part characterAtIndex:j];if (c<'0' || c>'9') return NO;
        }
    }
    return YES;
}
static BOOL sourceText(id value,NSUInteger limit,BOOL multiline,NSUInteger *lines) {
    if (!text(value,limit,NO)) return NO;
    NSUInteger count=1,column=0;
    for (NSUInteger i=0;i<[value length];++i) {
        unichar c=[value characterAtIndex:i];
        if (multiline && c=='\n') { ++count;column=0; }
        else {
            if (c<32 || (c>=127 && c<=159)) return NO;
            if (multiline && !CFStringIsSurrogateLowCharacter(c) && ++column>2048) return NO;
        }
    }
    if (lines) *lines=count;
    return YES;
}
@implementation HideAXSource
- (BOOL)isFlipped { return YES; }
- (BOOL)acceptsFirstResponder { return NO; }
- (NSView *)hitTest:(NSPoint)point { (void)point;return nil; }
- (BOOL)isAccessibilityElement { return self.record!=nil; }
- (NSAccessibilityRole)accessibilityRole { return NSAccessibilityStaticTextRole; }
- (NSString *)accessibilityLabel { return self.record[@"name"]; }
- (id)accessibilityValue { return self.record[@"value"]; }
- (id)accessibilityParent { return self.record ? self.superview : nil; }
- (NSRect)accessibilityFrame { return screenFrame(self,self.record[@"bounds"]); }
- (NSArray *)accessibilityChildren { return self.record ? @[] : nil; }
- (BOOL)isAccessibilityFocused { return NO; }
- (NSString *)accessibilityHelp {
    if (!self.record) return nil;
    return [NSString stringWithFormat:@"Read-only visible source excerpt, line %@, display column %@.%@",
        self.record[@"firstLine"],self.record[@"firstColumn"],
        [self.record[@"truncated"] boolValue] ? @" Some visible content is omitted." : @""];
}
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector {
    if (!self.record) return NO;
    // A StaticText role must not inherit NSView's optional text-range API.
    return (selector==@selector(isAccessibilityElement) || selector==@selector(accessibilityRole) ||
        selector==@selector(accessibilityRoleDescription) || selector==@selector(accessibilityLabel) ||
        selector==@selector(accessibilityValue) || selector==@selector(accessibilityHelp) ||
        selector==@selector(accessibilityParent) || selector==@selector(accessibilityFrame) ||
        selector==@selector(accessibilityChildren) || selector==@selector(isAccessibilityFocused)) &&
        [super isAccessibilitySelectorAllowed:selector];
}
@end
static void clearSourceAccessibility(void) {
    NSView *parent=sourceHost.superview;
    sourceHost.record=nil;[sourceHost removeFromSuperview];sourceHost=nil;
    if (parent) NSAccessibilityPostNotification(parent,NSAccessibilityLayoutChangedNotification);
}
static int updateSourceAccessibility(void *native_window,NSDictionary *wrapper,size_t length) {
    NSArray *size=wrapper[@"size"];NSDictionary *snapshot=wrapper[@"source"];
    if (length>262144 || ![size isKindOfClass:NSArray.class] || size.count!=2 || !integer(size[0],512) || !integer(size[1],256) ||
        [size[0] integerValue]<1 || [size[1] integerValue]<1 || ![snapshot isKindOfClass:NSDictionary.class] ||
        !boolean(snapshot[@"present"]) || !boolean(snapshot[@"readOnly"]) || ![snapshot[@"readOnly"] boolValue]) goto invalid;
    if (![snapshot[@"present"] boolValue]) { clearSourceAccessibility();return 1; }
    @autoreleasepool {
        NSUInteger lines=0;
        if (!sourceIdentity(snapshot[@"id"]) || !integer(snapshot[@"revision"],9007199254740991) || !sourceText(snapshot[@"name"],256,NO,NULL) ||
            snapshot[@"bounds"]==NSNull.null || !bounds(snapshot[@"bounds"],[size[0] integerValue],[size[1] integerValue]) ||
            !integer(snapshot[@"firstLine"],9007199254740991) || [snapshot[@"firstLine"] integerValue]<1 ||
            !integer(snapshot[@"firstColumn"],9007199254740991) || !integer(snapshot[@"lineCount"],256) || [snapshot[@"lineCount"] integerValue]<1 ||
            [snapshot[@"lineCount"] integerValue]>[snapshot[@"bounds"][3] integerValue] ||
            [snapshot[@"firstLine"] unsignedLongLongValue]>9007199254740991ULL-[snapshot[@"lineCount"] unsignedLongLongValue]+1 ||
            !sourceText(snapshot[@"value"],32768,YES,&lines) || lines!=[snapshot[@"lineCount"] unsignedIntegerValue] || !boolean(snapshot[@"truncated"])) goto invalid;
        NSWindow *window=(__bridge NSWindow *)native_window;
        if (sourceHost && (sourceHost.window!=window || ![sourceHost.record[@"id"] isEqual:snapshot[@"id"]])) clearSourceAccessibility();
        if (!sourceHost) {
            sourceHost=[[HideAXSource alloc] initWithFrame:window.contentView.bounds];
            sourceHost.autoresizingMask=NSViewWidthSizable|NSViewHeightSizable;[window.contentView addSubview:sourceHost];
        }
        sourceHost.record=snapshot;
        NSAccessibilityPostNotification(sourceHost,NSAccessibilityLayoutChangedNotification);return 1;
    }
invalid:
    clearSourceAccessibility();return 0;
}

static void clearSidebarAccessibility(void) {
    NSView *parent=host.superview;
    for (HideAXRow *row in host.rows) [row retire];
    host.rows=nil; host.nodes=nil; host.record=nil;
    [host removeFromSuperview]; host=nil;
    if (parent) NSAccessibilityPostNotification(parent,NSAccessibilityLayoutChangedNotification);
}
/* A complete current-modal snapshot. Structural IDs preserve reading location
 * only; neither values nor visibility introduce actions or OS input focus. */
@class HideAXDialog;
@interface HideAXControl : NSAccessibilityElement
@property(weak) HideAXDialog *host;
@property(weak) id parent;
@property(copy) NSDictionary *record;
@property(copy) NSArray<HideAXControl *> *children;
- (void)retire;
@end
@interface HideAXDialog : NSView
@property(copy) NSDictionary *record;
@property(copy) NSArray<HideAXControl *> *children;
@property(copy) NSDictionary<NSArray *,HideAXControl *> *nodes;
@property BOOL truncated;
@property BOOL hapticFeedback;
- (void)crossedButton:(NSTrackingArea *)area dragging:(BOOL)dragging;
- (void)requestHapticFeedback;
@end
static HideAXDialog *dialogHost;
static BOOL dialogPresent;
static NSArray *dialogRoles(void) { return @[@"dialog",@"text",@"textbox",@"checkbox",@"radiogroup",@"radio",@"listbox",@"option",@"combobox",@"button"]; }
static BOOL dialogIdentity(id value) {
    if (![value isKindOfClass:NSArray.class] || ![value count] || [value count]>5 || ![value[0] isEqual:@"dialog"]) return NO;
    for (NSString *part in value) {
        if (!text(part,24,YES)) return NO;
        if ([@[@"dialog",@"field",@"body",@"button",@"option"] containsObject:part]) continue;
        if ([part rangeOfCharacterFromSet:[[NSCharacterSet characterSetWithCharactersInString:@"0123456789"] invertedSet]].location!=NSNotFound) return NO;
    }
    return YES;
}
static NSUInteger scalarCount(NSString *value) {
    NSUInteger count=value.length;
    for (NSUInteger i=0;i<value.length;++i) if (CFStringIsSurrogateLowCharacter([value characterAtIndex:i])) --count;
    return count;
}
static NSString *dialogHelp(NSDictionary *record) {
    return [record[@"focused"] boolValue] ? @"Read-only snapshot. Focused in editor." : @"Read-only snapshot; use editor controls to change this dialog.";
}
@implementation HideAXControl
- (BOOL)isAccessibilityElement { return self.record!=nil; }
- (NSAccessibilityRole)accessibilityRole {
    NSString *role=self.record[@"role"];
    if ([role isEqual:@"text"]) return NSAccessibilityStaticTextRole;
    if ([role isEqual:@"textbox"]) return [self.record[@"multiline"] boolValue] ? NSAccessibilityTextAreaRole : NSAccessibilityTextFieldRole;
    if ([role isEqual:@"checkbox"]) return NSAccessibilityCheckBoxRole;
    if ([role isEqual:@"radio"]) return NSAccessibilityRadioButtonRole;
    if ([role isEqual:@"radiogroup"]) return NSAccessibilityRadioGroupRole;
    if ([role isEqual:@"listbox"]) return NSAccessibilityListRole;
    if ([role isEqual:@"option"]) return NSAccessibilityRowRole;
    if ([role isEqual:@"combobox"]) return NSAccessibilityComboBoxRole;
    return NSAccessibilityButtonRole;
}
- (NSString *)accessibilityLabel { return self.record[@"name"]; }
- (id)accessibilityValue {
    NSString *role=self.record[@"role"];
    id value=[role isEqual:@"checkbox"] || [role isEqual:@"radio"] ? self.record[@"checked"] : self.record[@"value"];
    return value==NSNull.null ? nil : value;
}
- (id)accessibilityParent { return self.parent; }
- (NSArray *)accessibilityChildren { return self.children; }
- (NSArray *)accessibilitySelectedChildren {
    NSMutableArray *selected=[NSMutableArray new];
    for (HideAXControl *child in self.children) if (child.record[@"selected"]!=NSNull.null && [child.record[@"selected"] boolValue]) [selected addObject:child];
    return selected;
}
- (NSRect)accessibilityFrame { return screenFrame(self.host,self.record[@"bounds"]); }
- (BOOL)isAccessibilitySelected { return self.record[@"selected"]!=NSNull.null && [self.record[@"selected"] boolValue]; }
- (BOOL)isAccessibilityExpanded { return self.record[@"expanded"]!=NSNull.null && [self.record[@"expanded"] boolValue]; }
- (BOOL)isAccessibilityFocused { return NO; }
- (NSString *)accessibilityHelp { return self.record ? dialogHelp(self.record) : nil; }
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector {
    if ((selector==@selector(accessibilityValue) && !self.accessibilityValue) ||
        (selector==@selector(isAccessibilitySelected) && self.record[@"selected"]==NSNull.null) ||
        (selector==@selector(isAccessibilityExpanded) && self.record[@"expanded"]==NSNull.null)) return NO;
    return readOnlySelector(selector) && [super isAccessibilitySelectorAllowed:selector];
}
- (void)retire { self.record=nil;self.children=nil;self.parent=nil;self.host=nil; }
@end
@implementation HideAXDialog
- (void)updateTrackingAreas {
    [super updateTrackingAreas];
    NSArray<NSTrackingArea *> *previous=self.trackingAreas.copy;
    NSMutableArray<NSTrackingArea *> *retained=[NSMutableArray new];
    if (self.hapticFeedback) for (HideAXControl *control in self.nodes.allValues) {
        if (![control.record[@"role"] isEqual:@"button"]) continue;
        NSArray *r=control.record[@"bounds"]; double rect[4];
        if (!thc_accessibility_cell_rect([r[0] intValue],[r[1] intValue],[r[2] intValue],[r[3] intValue],rect)) continue;
        NSRect frame=NSMakeRect(rect[0],rect[1],rect[2],rect[3]);
        NSTrackingArea *area=nil;
        for (NSTrackingArea *candidate in previous)
            if ([candidate.userInfo[@"id"] isEqual:control.record[@"id"]] && NSEqualRects(candidate.rect,frame)) { area=candidate;break; }
        if (!area) {
            area=[[NSTrackingArea alloc] initWithRect:frame options:NSTrackingMouseEnteredAndExited|NSTrackingActiveInKeyWindow
                owner:self userInfo:@{@"id":control.record[@"id"]}];
            [self addTrackingArea:area];
        }
        [retained addObject:area];
    }
    // Editing a field or repainting must not recreate the edge beneath a stationary pointer.
    for (NSTrackingArea *area in previous) if (![retained containsObject:area]) {
        [self removeTrackingArea:area];
    }
}
- (void)requestHapticFeedback {
    [NSHapticFeedbackManager.defaultPerformer performFeedbackPattern:NSHapticFeedbackPatternGeneric performanceTime:NSHapticFeedbackPerformanceTimeNow];
}
- (void)crossedButton:(NSTrackingArea *)area dragging:(BOOL)dragging {
    if (self.hapticFeedback && !dragging && area && [self.trackingAreas containsObject:area]) [self requestHapticFeedback];
}
// AppKit owns edge state, including areas created beneath a stationary pointer.
- (void)mouseEntered:(NSEvent *)event { [self crossedButton:event.trackingArea dragging:NSEvent.pressedMouseButtons!=0]; }
- (void)mouseExited:(NSEvent *)event { [self crossedButton:event.trackingArea dragging:NSEvent.pressedMouseButtons!=0]; }
- (BOOL)isFlipped { return YES; }
- (BOOL)acceptsFirstResponder { return NO; }
- (NSView *)hitTest:(NSPoint)point { (void)point;return nil; }
- (BOOL)isAccessibilityElement { return self.record!=nil; }
- (NSAccessibilityRole)accessibilityRole { return NSAccessibilityGroupRole; }
- (NSAccessibilitySubrole)accessibilitySubrole { return NSAccessibilityDialogSubrole; }
- (NSString *)accessibilityLabel { return self.record[@"name"]; }
- (NSString *)accessibilityHelp { return self.truncated ? @"Read-only dialog snapshot. Some visible content is omitted." : @"Read-only dialog snapshot; use editor controls to change this dialog."; }
- (NSRect)accessibilityFrame { return screenFrame(self,self.record[@"bounds"]); }
- (NSArray *)accessibilityChildren { return self.children; }
- (BOOL)isAccessibilityFocused { return NO; }
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector { return readOnlySelector(selector) && [super isAccessibilitySelectorAllowed:selector]; }
@end
static void clearDialogAccessibility(void) {
    NSView *parent=dialogHost.superview;
    for (HideAXControl *control in dialogHost.nodes.allValues) [control retire];
    dialogHost.hapticFeedback=NO;[dialogHost updateTrackingAreas];
    dialogHost.children=nil;dialogHost.nodes=nil;dialogHost.record=nil;
    [dialogHost removeFromSuperview];dialogHost=nil;
    if (parent) NSAccessibilityPostNotification(parent,NSAccessibilityLayoutChangedNotification);
}
static int updateDialogAccessibility(void *native_window,NSDictionary *wrapper) {
    NSArray *size=wrapper[@"size"];NSDictionary *snapshot=wrapper[@"dialog"];
    id haptics=wrapper[@"hapticFeedback"] ?: @NO;
    if (!boolean(haptics)) goto invalid;
    if (![size isKindOfClass:NSArray.class] || size.count!=2 || !integer(size[0],512) || !integer(size[1],256) ||
        [size[0] integerValue]<1 || [size[1] integerValue]<1 || ![snapshot isKindOfClass:NSDictionary.class] ||
        !boolean(snapshot[@"present"]) || !boolean(snapshot[@"readOnly"]) || ![snapshot[@"readOnly"] boolValue] || !boolean(snapshot[@"truncated"])) goto invalid;
    @autoreleasepool {
        NSArray *values=snapshot[@"nodes"];
        if (![values isKindOfClass:NSArray.class] || values.count>256 || (![snapshot[@"present"] boolValue] && values.count)) goto invalid;
        NSMutableDictionary<NSArray *,NSDictionary *> *records=[NSMutableDictionary new];NSDictionary *root=nil;NSUInteger budget=0;
        for (NSDictionary *node in values) {
            if (![node isKindOfClass:NSDictionary.class] || !dialogIdentity(node[@"id"]) || ![dialogRoles() containsObject:node[@"role"]] ||
                !text(node[@"name"],256,NO) || !(node[@"value"]==NSNull.null || text(node[@"value"],2048,NO)) || node[@"bounds"]==NSNull.null ||
                !bounds(node[@"bounds"],[size[0] integerValue],[size[1] integerValue]) || !boolean(node[@"focused"]) || !boolean(node[@"multiline"]) ||
                !nullableBoolean(node[@"checked"]) || !nullableBoolean(node[@"selected"]) || !nullableBoolean(node[@"expanded"]) || records[node[@"id"]]) goto invalid;
            budget+=scalarCount(node[@"name"])+(node[@"value"]==NSNull.null ? 0 : scalarCount(node[@"value"]));if (budget>32768) goto invalid;
            records[node[@"id"]]=node;
            if ([node[@"id"] isEqual:@[@"dialog"]] && [node[@"role"] isEqual:@"dialog"] && node[@"parent"]==NSNull.null) root=node;
            else if ([node[@"role"] isEqual:@"dialog"] || !dialogIdentity(node[@"parent"])) goto invalid;
        }
        if (values.count && !root) goto invalid;
        for (NSDictionary *node in values) if (node!=root) {
            NSDictionary *parent=records[node[@"parent"]];NSUInteger depth=0;
            while (parent && parent!=root && depth<5) { ++depth;parent=records[parent[@"parent"]]; }
            if (parent!=root) goto invalid;
        }
        dialogPresent=[snapshot[@"present"] boolValue];
        if (dialogPresent) { clearSidebarAccessibility();clearCanvasAccessibility();clearSourceAccessibility(); }
        if (!values.count) { clearDialogAccessibility();return 1; }
        NSWindow *window=(__bridge NSWindow *)native_window;
        if (dialogHost && dialogHost.window!=window) clearDialogAccessibility();
        if (!dialogHost) {
            dialogHost=[[HideAXDialog alloc] initWithFrame:window.contentView.bounds];
            dialogHost.autoresizingMask=NSViewWidthSizable|NSViewHeightSizable;[window.contentView addSubview:dialogHost];
        }
        BOOL changed=dialogHost.hapticFeedback!=[haptics boolValue] || ![root isEqual:dialogHost.record] || dialogHost.truncated!=[snapshot[@"truncated"] boolValue] || dialogHost.nodes.count!=values.count-1;
        for (NSDictionary *node in values) if (node!=root && ![node isEqual:dialogHost.nodes[node[@"id"]].record]) changed=YES;
        if (!changed) return 1;
        NSMutableDictionary *nodes=[NSMutableDictionary new],*children=[NSMutableDictionary new];
        for (NSDictionary *node in values) if (node!=root) {
            NSArray *key=node[@"id"];HideAXControl *control=dialogHost.nodes[key] ?: [HideAXControl new];
            control.record=node;control.host=dialogHost;nodes[key]=control;
            NSArray *parent=node[@"parent"];if (!children[parent]) children[parent]=[NSMutableArray new];[children[parent] addObject:control];
        }
        for (NSArray *key in nodes) {
            HideAXControl *control=nodes[key];control.parent=[control.record[@"parent"] isEqual:root[@"id"]] ? dialogHost : nodes[control.record[@"parent"]];
            control.children=children[key] ?: @[];
        }
        for (NSArray *key in dialogHost.nodes) if (!nodes[key]) [dialogHost.nodes[key] retire];
        dialogHost.record=root;dialogHost.nodes=nodes;dialogHost.children=children[root[@"id"]] ?: @[];dialogHost.truncated=[snapshot[@"truncated"] boolValue];
        dialogHost.hapticFeedback=[haptics boolValue];[dialogHost updateTrackingAreas];
        NSAccessibilityPostNotification(dialogHost,NSAccessibilityLayoutChangedNotification);return 1;
    }
invalid:
    clearDialogAccessibility();clearSidebarAccessibility();clearCanvasAccessibility();clearSourceAccessibility();dialogPresent=YES;return 0;
}
void thc_accessibility_close(void) {
    clearDialogAccessibility();dialogPresent=NO;clearCanvasAccessibility();clearSidebarAccessibility();clearSourceAccessibility();
}
void thc_accessibility_geometry_changed(void) {
    if (host.rows.count) NSAccessibilityPostNotification(host,NSAccessibilityLayoutChangedNotification);
    if (canvasHost.images.count) NSAccessibilityPostNotification(canvasHost,NSAccessibilityLayoutChangedNotification);
    if (sourceHost.record) NSAccessibilityPostNotification(sourceHost,NSAccessibilityLayoutChangedNotification);
    if (dialogHost.record) { [dialogHost updateTrackingAreas];NSAccessibilityPostNotification(dialogHost,NSAccessibilityLayoutChangedNotification); }
}
int thc_accessibility_update(void *native_window, const char *json, size_t length) {
    if (!NSThread.isMainThread) return 0;
    if (!length) { thc_accessibility_close(); return 1; }
    if (!native_window || !json || length>2097152) goto invalid;
    @autoreleasepool {
        NSDictionary *snapshot=[NSJSONSerialization JSONObjectWithData:[NSData dataWithBytes:json length:length] options:0 error:nil];
        if ([snapshot isKindOfClass:NSDictionary.class] && snapshot[@"dialog"]) return updateDialogAccessibility(native_window,snapshot);
        if (dialogPresent) return 1;
        if ([snapshot isKindOfClass:NSDictionary.class] && snapshot[@"source"]) return updateSourceAccessibility(native_window,snapshot,length);
        if ([snapshot isKindOfClass:NSDictionary.class] && snapshot[@"images"]) return updateCanvasAccessibility(native_window,snapshot);
        if (![snapshot isKindOfClass:NSDictionary.class] || !boolean(snapshot[@"readOnly"]) || ![snapshot[@"readOnly"] boolValue] || !integer(snapshot[@"revision"],9007199254740991)) goto invalid;
        NSArray *layout=snapshot[@"layout"],*values=snapshot[@"nodes"];
        if (![layout isKindOfClass:NSArray.class] || layout.count!=7 || ![values isKindOfClass:NSArray.class] || values.count>512) goto invalid;
        for (id n in layout) if (!integer(n,9007199254740991)) goto invalid;
        NSInteger cols=[layout[0] integerValue],rows=[layout[1] integerValue];
        if (cols<1 || cols>512 || rows<1 || rows>256 || !integer(snapshot[@"visibleStart"],65536) || !integer(snapshot[@"visibleCount"],256) || !integer(snapshot[@"logicalRows"],65536)) goto invalid;
        NSMutableDictionary<NSArray *,NSDictionary *> *records=[NSMutableDictionary new];
        NSDictionary *root=nil; NSMutableArray<NSDictionary *> *ordered=[NSMutableArray new];
        for (NSDictionary *node in values) {
            if (![node isKindOfClass:NSDictionary.class] || !identity(node[@"id"]) || !text(node[@"name"],256,NO) || !bounds(node[@"bounds"],cols,rows) ||
                !boolean(node[@"selected"]) || !boolean(node[@"focused"]) || !boolean(node[@"loading"]) || !boolean(node[@"moreChildren"]) || !nullableBoolean(node[@"expanded"]) ||
                !integer(node[@"generation"],9007199254740991) || !integer(node[@"level"],65) || !integer(node[@"posInSet"],32768) || !integer(node[@"childrenKnown"],32768) ||
                !([node[@"setSize"] isEqual:@(-1)] || integer(node[@"setSize"],32768)) ||
                !(node[@"index"]==NSNull.null || (integer(node[@"index"],65535) && [node[@"index"] integerValue]<[snapshot[@"logicalRows"] integerValue]))) goto invalid;
            NSArray *key=node[@"id"]; if (records[key]) goto invalid; records[key]=node;
            if ([node[@"role"] isEqual:@"tree"] && [key isEqual:@[@"sidebar"]] && node[@"parent"]==NSNull.null) root=node;
            else if ([node[@"role"] isEqual:@"treeitem"] && [key[0] isEqual:@"tree"] && identity(node[@"parent"]) && [node[@"level"] integerValue]>0 && [node[@"posInSet"] integerValue]>0) [ordered addObject:node];
            else goto invalid;
        }
        if (!ordered.count || ![snapshot[@"visibleCount"] integerValue]) { clearSidebarAccessibility(); return 1; }
        if (!root || root[@"bounds"]==NSNull.null) goto invalid;
        for (NSDictionary *node in ordered) {
            NSDictionary *parent=records[node[@"parent"]]; NSInteger level=1;
            while (parent && parent!=root && level<=65) { ++level; parent=records[parent[@"parent"]]; }
            if (parent!=root || level!=[node[@"level"] integerValue]) goto invalid;
        }
        [ordered sortUsingComparator:^NSComparisonResult(NSDictionary *a,NSDictionary *b) { NSInteger x=rowIndex(a),y=rowIndex(b); return x<y?NSOrderedAscending:x>y?NSOrderedDescending:NSOrderedSame; }];
        NSWindow *window=(__bridge NSWindow *)native_window;
        if (host && host.window!=window) thc_accessibility_close();
        if (!host) {
            host=[[HideAXHost alloc] initWithFrame:window.contentView.bounds];
            host.autoresizingMask=NSViewWidthSizable|NSViewHeightSizable;
            [window.contentView addSubview:host];
        }
        NSArray *oldSelected=[host.accessibilitySelectedRows copy];
        NSMutableDictionary<NSArray *,HideAXRow *> *nodes=[NSMutableDictionary new]; NSMutableArray<HideAXRow *> *newRows=[NSMutableArray new];
        for (NSDictionary *node in ordered) {
            HideAXRow *row=host.nodes[node[@"id"]] ?: [HideAXRow new];
            row.record=node; row.host=host; row.discloser=nil; row.disclosedRows=@[];
            nodes[node[@"id"]]=row; [newRows addObject:row];
        }
        NSMutableDictionary<NSArray *,NSMutableArray *> *children=[NSMutableDictionary new];
        for (HideAXRow *row in newRows) {
            NSArray *parent=row.record[@"parent"];
            row.discloser=nodes[parent];
            if (!children[parent]) children[parent]=[NSMutableArray new];
            [children[parent] addObject:row];
        }
        for (HideAXRow *row in newRows) row.disclosedRows=children[row.record[@"id"]] ?: @[];
        for (NSArray *key in host.nodes) if (!nodes[key]) [host.nodes[key] retire];
        host.record=root; host.nodes=nodes; host.rows=newRows;
        NSAccessibilityPostNotification(host,NSAccessibilityLayoutChangedNotification);
        if (![oldSelected isEqual:host.accessibilitySelectedRows]) NSAccessibilityPostNotification(host,NSAccessibilitySelectedRowsChangedNotification);
        return 1;
    }
invalid:
    thc_accessibility_close(); return 0;
}
