#import <Cocoa/Cocoa.h>
#include "window.h"

@interface THCMenuTarget : NSObject
- (void)invoke:(id)sender;
@end
@implementation THCMenuTarget
- (void)invoke:(id)sender { thc_post_command((int)[sender tag]); }
@end
static THCMenuTarget *target;
static NSMenu *current;
static NSMutableDictionary<NSNumber *, NSMenuItem *> *items;

/* Set before SDL creates NSApplication: unbundled launches otherwise inherit
 * the executable name for the application menu. */
void thc_menu_prepare(void) { [[NSProcessInfo processInfo] setProcessName:@"Turbo Haskell"]; }

static NSMenuItem *commandItem(NSString *title, NSString *shortcut, int command, int enabled) {
    BOOL option = [shortcut hasPrefix:@"~"];
    if (option) shortcut = [shortcut substringFromIndex:1];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:title action:@selector(invoke:) keyEquivalent:[shortcut lowercaseString]];
    [item setKeyEquivalentModifierMask:NSEventModifierFlagCommand | (option ? NSEventModifierFlagOption : 0) | (![shortcut isEqualToString:[shortcut lowercaseString]] ? NSEventModifierFlagShift : 0)];
    [item setTarget:target]; [item setTag:command]; [item setEnabled:enabled];
    items[@(command)] = item;
    return item;
}

void thc_menu_clear(int about, int settings, int quit) {
    if (!target) target = [THCMenuTarget new];
    items = [NSMutableDictionary new];
    NSMenu *bar = [NSMenu new];
    NSMenuItem *appItem = [[NSMenuItem alloc] initWithTitle:@"Turbo Haskell" action:nil keyEquivalent:@""];
    NSMenu *appMenu = [[NSMenu alloc] initWithTitle:@"Turbo Haskell"];
    [appMenu setAutoenablesItems:NO];
    [appMenu addItem:commandItem(@"About Turbo Haskell", @"", about, 1)];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItem:commandItem(@"Settings…", @",", settings, 1)];
    [appMenu addItem:[NSMenuItem separatorItem]];
    NSMenu *services = [[NSMenu alloc] initWithTitle:@"Services"];
    NSMenuItem *servicesItem = [appMenu addItemWithTitle:@"Services" action:nil keyEquivalent:@""];
    [servicesItem setSubmenu:services];
    [NSApp setServicesMenu:services];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItemWithTitle:@"Hide Turbo Haskell" action:@selector(hide:) keyEquivalent:@"h"];
    NSMenuItem *hideOthers = [appMenu addItemWithTitle:@"Hide Others" action:@selector(hideOtherApplications:) keyEquivalent:@"h"];
    [hideOthers setKeyEquivalentModifierMask:NSEventModifierFlagCommand | NSEventModifierFlagOption];
    [appMenu addItemWithTitle:@"Show All" action:@selector(unhideAllApplications:) keyEquivalent:@""];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItem:commandItem(@"Quit Turbo Haskell", @"q", quit, 1)];
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
void thc_menu_item(const char *title, const char *key, int command, int enabled) {
    [current addItem:commandItem([NSString stringWithUTF8String:title], [NSString stringWithUTF8String:key], command, enabled)];
}
void thc_menu_enabled(int command, int enabled) { [items[@(command)] setEnabled:enabled]; }
