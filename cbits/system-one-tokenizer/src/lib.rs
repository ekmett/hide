// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::{ptr, slice, str};
use tokenizers::Tokenizer;

pub struct HideTokenizer(Tokenizer);

fn guarded(action: impl FnOnce() -> i32) -> i32 {
    match catch_unwind(AssertUnwindSafe(action)) {
        Ok(result) => result,
        Err(_) => 1,
    }
}

/// Borrow checked JSON bytes for construction; the returned owner is independent.
#[no_mangle]
pub unsafe extern "C" fn hide_tokenizer_create(
    json: *const u8, length: usize, out: *mut *mut HideTokenizer,
) -> i32 {
    if out.is_null() { return 2; }
    ptr::write(out, ptr::null_mut());
    if json.is_null() || length == 0 || length > 4 * 1024 * 1024 { return 2; }
    guarded(|| match Tokenizer::from_bytes(slice::from_raw_parts(json, length)) {
        Ok(tokenizer) if tokenizer.get_truncation().is_none() && tokenizer.get_padding().is_none() => {
            ptr::write(out, Box::into_raw(Box::new(HideTokenizer(tokenizer))));
            0
        }
        _ => 1,
    })
}

/// No automatic special tokens or truncation. Output overflow is explicit.
#[no_mangle]
pub unsafe extern "C" fn hide_tokenizer_encode(
    tokenizer: *const HideTokenizer, utf8: *const u8, length: usize,
    tokens: *mut u32, capacity: usize, written: *mut usize,
) -> i32 {
    if written.is_null() { return 2; }
    ptr::write(written, 0);
    if tokenizer.is_null() || utf8.is_null() || tokens.is_null()
        || length > 1024 * 1024 || capacity > 512 { return 2; }
    guarded(|| {
        let Ok(text) = str::from_utf8(slice::from_raw_parts(utf8, length)) else { return 2; };
        match (*tokenizer).0.encode(text, false) {
            Ok(encoded) => {
                let ids = encoded.get_ids();
                if ids.len() > capacity { return 3; }
                ptr::copy_nonoverlapping(ids.as_ptr(), tokens, ids.len());
                ptr::write(written, ids.len());
                0
            }
            Err(_) => 1,
        }
    })
}

/// Free only after all borrowed calls have returned.
#[no_mangle]
pub unsafe extern "C" fn hide_tokenizer_free(tokenizer: *mut HideTokenizer) {
    if !tokenizer.is_null() {
        let _ = catch_unwind(AssertUnwindSafe(|| drop(Box::from_raw(tokenizer))));
    }
}
