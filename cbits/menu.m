#import <Cocoa/Cocoa.h>
#include "window.h"
#include <limits.h>
#include <stdlib.h>

@interface THCMenuTarget : NSObject
- (void)invoke:(id)sender;
@end
@implementation THCMenuTarget
- (void)invoke:(id)sender { thc_post_command((int)[sender tag], [[sender representedObject] intValue]); }
@end
static THCMenuTarget *target;
static NSMenu *current;
static NSMutableDictionary<NSNumber *, NSMutableArray<NSMenuItem *> *> *items;
static int generation;
int thc_menu_generation(void) { return generation; }

/* Set before SDL creates NSApplication: unbundled launches otherwise inherit
 * the executable name for the application menu. */
void thc_menu_prepare(void) { [[NSProcessInfo processInfo] setProcessName:@"Haskell"]; }

static NSMenuItem *commandItem(NSString *title, NSString *shortcut, int command, int enabled) {
    BOOL option = [shortcut hasPrefix:@"~"];
    if (option) shortcut = [shortcut substringFromIndex:1];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:title action:@selector(invoke:) keyEquivalent:[shortcut lowercaseString]];
    [item setKeyEquivalentModifierMask:NSEventModifierFlagCommand | (option ? NSEventModifierFlagOption : 0) | (![shortcut isEqualToString:[shortcut lowercaseString]] ? NSEventModifierFlagShift : 0)];
    [item setTarget:target]; [item setTag:command]; [item setEnabled:enabled];
    [item setRepresentedObject:@(generation)];
    if (!items[@(command)]) items[@(command)] = [NSMutableArray new];
    [items[@(command)] addObject:item];
    return item;
}

void thc_menu_clear(int about, int settings, int quit) {
    if (generation == INT_MAX) abort(); /* Never reuse a queued event incarnation. */
    ++generation;
    if (!target) target = [THCMenuTarget new];
    items = [NSMutableDictionary new];
    NSMenu *bar = [NSMenu new];
    NSMenuItem *appItem = [[NSMenuItem alloc] initWithTitle:@"Haskell" action:nil keyEquivalent:@""];
    NSMenu *appMenu = [[NSMenu alloc] initWithTitle:@"Haskell"];
    [appMenu setAutoenablesItems:NO];
    [appMenu addItem:commandItem(@"About Haskell", @"", about, 1)];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItem:commandItem(@"Settings…", @",", settings, 1)];
    [appMenu addItem:[NSMenuItem separatorItem]];
    NSMenu *services = [[NSMenu alloc] initWithTitle:@"Services"];
    NSMenuItem *servicesItem = [appMenu addItemWithTitle:@"Services" action:nil keyEquivalent:@""];
    [servicesItem setSubmenu:services];
    [NSApp setServicesMenu:services];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItemWithTitle:@"Hide Haskell" action:@selector(hide:) keyEquivalent:@"h"];
    NSMenuItem *hideOthers = [appMenu addItemWithTitle:@"Hide Others" action:@selector(hideOtherApplications:) keyEquivalent:@"h"];
    [hideOthers setKeyEquivalentModifierMask:NSEventModifierFlagCommand | NSEventModifierFlagOption];
    [appMenu addItemWithTitle:@"Show All" action:@selector(unhideAllApplications:) keyEquivalent:@""];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItem:commandItem(@"Quit Haskell", @"q", quit, 1)];
    [appItem setSubmenu:appMenu];
    [bar addItem:appItem];
    [NSApp setMainMenu:bar];
}
void thc_menu_add(const char *title) {
    NSString *name = [NSString stringWithUTF8String:title];
    current = [[NSMenu alloc] initWithTitle:name];
    [current setAutoenablesItems:NO];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:name action:nil keyEquivalent:@""];
    [item setSubmenu:current];
    [[NSApp mainMenu] addItem:item];
}
void thc_menu_separator(void) { [current addItem:[NSMenuItem separatorItem]]; }
void thc_menu_item(const char *title, const char *key, int command, int enabled) {
    [current addItem:commandItem([NSString stringWithUTF8String:title], [NSString stringWithUTF8String:key], command, enabled)];
}
void thc_menu_enabled(int command, int enabled) {
    for (NSMenuItem *item in items[@(command)]) [item setEnabled:enabled];
}
