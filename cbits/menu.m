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
void thc_menu_clear(void) {
    if (!target) target = [THCMenuTarget new];
    items = [NSMutableDictionary new];
    NSMenu *bar = [NSMenu new];
    NSMenuItem *appItem = [NSMenuItem new];
    NSMenu *appMenu = [[NSMenu alloc] initWithTitle:@"Turbo Haskell"];
    [appMenu addItemWithTitle:@"Hide Turbo Haskell" action:@selector(hide:) keyEquivalent:@"h"];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItemWithTitle:@"Show All" action:@selector(unhideAllApplications:) keyEquivalent:@""];
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
    NSString *shortcut = [NSString stringWithUTF8String:key];
    BOOL option = [shortcut hasPrefix:@"~"];
    if (option) shortcut = [shortcut substringFromIndex:1];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:[NSString stringWithUTF8String:title]
                                                     action:@selector(invoke:) keyEquivalent:[shortcut lowercaseString]];
    [item setKeyEquivalentModifierMask:NSEventModifierFlagCommand | (option ? NSEventModifierFlagOption : 0) | (![shortcut isEqualToString:[shortcut lowercaseString]] ? NSEventModifierFlagShift : 0)];
    [item setTarget:target]; [item setTag:command]; [item setEnabled:enabled];
    [current addItem:item]; items[@(command)] = item;
}
void thc_menu_enabled(int command, int enabled) { [items[@(command)] setEnabled:enabled]; }
