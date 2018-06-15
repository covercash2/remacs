#![allow(unused)]
// Temporarily allow ZSTs for display_info.
#![allow(improper_ctypes)]

//! This module contains all FFI declarations.
//!
//! These types and constants are generated at build time to mimic how they are
//! in C:
//!
//! - `EmacsInt`
//! - `EmacsUint`
//! - `EmacsDouble`
//! - `EMACS_INT_MAX`
//! - `EMACS_INT_SIZE`
//! - `EMACS_FLOAT_SIZE`
//! - `GCTYPEBITS`
//! - `USE_LSB_TAG`
//! - `BoolBF`

use libc::{self, c_void, ptrdiff_t};
use std;

use libc::timespec;
use remacs_lib::current_timespec;

use data::{Lisp_Boolfwd, Lisp_Buffer_Objfwd, Lisp_Fwd, Lisp_Intfwd, Lisp_Kboard_Objfwd,
           Lisp_Objfwd};
use lisp::LispObject;

include!(concat!(env!("OUT_DIR"), "/definitions.rs"));

type Lisp_Object = LispObject;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
include!(concat!(env!("OUT_DIR"), "/globals.rs"));

pub const VAL_MAX: EmacsInt = (EMACS_INT_MAX >> (GCTYPEBITS - 1));
pub const VALMASK: EmacsInt = [VAL_MAX, -(1 << GCTYPEBITS)][USE_LSB_TAG as usize];
pub const INTMASK: EmacsInt = (EMACS_INT_MAX >> (Lisp_Bits::INTTYPEBITS - 1));
pub const PSEUDOVECTOR_FLAG: usize = 0x4000000000000000;

// These signal an error, therefore are marked as non-returning.
extern "C" {
    pub fn circular_list(tail: Lisp_Object) -> !;
    pub fn wrong_type_argument(predicate: Lisp_Object, value: Lisp_Object) -> !;
    // defined in eval.c, where it can actually take an arbitrary
    // number of arguments.
    // TODO: define a Rust version of this that uses Rust strings.
    pub fn error(m: *const u8, ...) -> !;
    pub fn nsberror(spec: Lisp_Object) -> !;
    pub fn emacs_abort() -> !;
    pub fn Fsignal(error_symbol: Lisp_Object, data: Lisp_Object) -> !;
    pub fn memory_full(nbytes: libc::size_t) -> !;
    pub fn bitch_at_user() -> !;
    pub fn wrong_choice(choice: LispObject, wrong: LispObject) -> !;
    pub fn wrong_range(min: LispObject, max: LispObject, wrong: LispObject) -> !;
}

/// Type of comparison for `internal_equal()`.
#[repr(C)]
pub enum EqualKind {
    NoQuit,
    Plain,
    IncludingProperties,
}

// bindgen apparently misses these, for various reasons
extern "C" {
    // these weren't declared in a header, for example
    pub static Vprocess_alist: Lisp_Object;
    pub fn hash_clear(h: *mut Lisp_Hash_Table);
#[repr(C)]
pub struct re_registers {
    pub num_regs: libc::c_uint,
    pub start: *mut c_void, // TODO
    pub end: *mut c_void,   // TODO
}

#[repr(C)]
pub struct thread_state {
    pub header: Lisp_Vectorlike_Header,
    /// The buffer in which the last search was performed, or
    /// Qt if the last search was done in a string;
    /// Qnil if no searching has been done yet.
    pub m_last_thing_searched: LispObject,

    pub m_saved_last_thing_searched: LispObject,
    /// The thread's name.
    pub name: LispObject,

    /// The thread's function.
    pub function: LispObject,

    /// If non-nil, this thread has been signaled.
    pub error_symbol: LispObject,
    pub error_data: LispObject,

    /// If we are waiting for some event, this holds the object we are
    /// waiting on.
    pub event_object: LispObject,

    /// m_stack_bottom must be the first non-Lisp field.
    /// An address near the bottom of the stack.
    /// Tells GC how to save a copy of the stack.
    pub m_stack_bottom: *mut c_char,
    /// An address near the top of the stack.
    pub stack_top: *mut c_char,

    pub m_catchlist: *mut c_void, // TODO
    /// Chain of condition handlers currently in effect.
    /// The elements of this chain are contained in the stack frames
    /// of Fcondition_case and internal_condition_case.
    /// When an error is signaled (by calling Fsignal),
    /// this chain is searched for an element that applies.
    pub m_handlerlist: *mut c_void, // TODO
    pub m_handlerlist_list: *mut c_void, // TODO

    /// Current number of specbindings allocated in specpdl.
    pub m_specpdl_size: ptrdiff_t,

    /// Pointer to beginning of specpdl.
    pub m_specpdl: *mut c_void, // TODO
    /// Pointer to first unused element in specpdl.
    pub m_specpdl_ptr: *mut c_void, // TODO
    /// Depth in Lisp evaluations and function calls.
    pub m_lisp_eval_depth: EmacsInt,

    /// This points to the current buffer.
    pub m_current_buffer: *mut c_void,
    /// Every call to re_match, etc., must pass &search_regs as the regs
    /// argument unless you can show it is unnecessary (i.e., if re_match
    /// is certainly going to be called again before region-around-match
    /// can be called).

    /// Since the registers are now dynamically allocated, we need to make
    /// sure not to refer to the Nth register before checking that it has
    /// been allocated by checking search_regs.num_regs.

    /// The regex code keeps track of whether it has allocated the search
    /// buffer using bits in the re_pattern_buffer.  This means that whenever
    /// you compile a new pattern, it completely forgets whether it has
    /// allocated any registers, and will allocate new registers the next
    /// time you call a searching or matching function.  Therefore, we need
    /// to call re_set_registers after compiling a new pattern or after
    /// setting the match registers, so that the regex functions will be
    /// able to free or re-allocate it properly.
    pub m_search_regs: re_registers,
    /// If non-zero the match data have been saved in saved_search_regs
    /// during the execution of a sentinel or filter.
    pub m_search_regs_saved: bool,
    pub m_saved_search_regs: re_registers,
    /// This is the string or buffer in which we
    /// are matching.  It is used for looking up syntax properties.

    /// If the value is a Lisp string object, we are matching text in that
    /// string; if it's nil, we are matching text in the current buffer; if
    /// it's t, we are matching text in a C string.
    pub m_re_match_object: LispObject,
    /// This member is different from waiting_for_input.
    /// It is used to communicate to a lisp process-filter/sentinel (via the
    /// function Fwaiting_for_user_input_p) whether Emacs was waiting
    /// for user-input when that process-filter was called.
    /// waiting_for_input cannot be used as that is by definition 0 when
    /// lisp code is being evalled.
    /// This is also used in record_asynch_buffer_change.
    /// For that purpose, this must be 0
    /// when not inside wait_reading_process_output.
    pub m_waiting_for_user_input_p: c_int,
    /// True while doing kbd input.
    pub m_waiting_for_input: bool,
    // TODO: this struct is incomplete. We're missing thread_id,
    // thread_condvar, wait_condvar, not_holding_lock, and
    // next_thread.
}

extern "C" {
    pub fn SPECPDL_INDEX() -> ptrdiff_t;
}

/// Lisp_Char_Table
#[repr(C)]
pub enum ChartabSize {
    Bits0 = 6,
    Bits1 = 4,
    Bits2 = 5,
    Bits3 = 7,
}

/// Lisp_Char_Table
#[repr(C)]
pub struct Lisp_Char_Table {
    /// HEADER.SIZE is the vector's size field, which also holds the
    /// pseudovector type information.  It holds the size, too.
    /// The size counts the defalt, parent, purpose, ascii,
    /// contents, and extras slots.
    pub header: Lisp_Vectorlike_Header,

    /// This holds a default value,
    /// which is used whenever the value for a specific character is nil.
    pub default: LispObject,

    /// This points to another char table, which we inherit from when the
    /// value for a specific character is nil.  The `defalt' slot takes
    /// precedence over this.
    pub parent: LispObject,

    /// This is a symbol which says what kind of use this char-table is
    /// meant for.
    pub purpose: LispObject,

    /// The bottom sub char-table for characters of the range 0..127.  It
    /// is nil if none of ASCII character has a specific value.
    pub ascii: LispObject,

    pub contents: [LispObject; 1 << ChartabSize::Bits0 as u8],

    /// These hold additional data.  It is a vector.
    // actually any number of items
    pub extras: [LispObject; 1],
}

#[repr(C)]
pub struct Lisp_Sub_Char_Table {
    /// HEADER.SIZE is the vector's size field, which also holds the
    /// pseudovector type information.  It holds the size, too.
    pub header: Lisp_Vectorlike_Header,

    /// Depth of this sub char-table.  It should be 1, 2, or 3.  A sub
    /// char-table of depth 1 contains 16 elements, and each element
    /// covers 4096 (128*32) characters.  A sub char-table of depth 2
    /// contains 32 elements, and each element covers 128 characters.  A
    /// sub char-table of depth 3 contains 128 elements, and each element
    /// is for one character.
    pub depth: libc::c_int,

    /// Minimum character covered by the sub char-table.
    pub min_char: libc::c_int,

    /// Use set_sub_char_table_contents to set this.
    pub contents: [LispObject; 1],
}

extern "C" {
    pub fn uniprop_table_uncompress(table: LispObject, idx: libc::c_int) -> LispObject;
}

#[repr(C)]
pub struct Lisp_Process {
    pub header: Lisp_Vectorlike_Header,

    /// Name of subprocess terminal.
    pub tty_name: LispObject,

    /// Name of this process.
    pub name: LispObject,

    /// List of command arguments that this process was run with.
    /// Is set to t for a stopped network process; nil otherwise.
    pub command: LispObject,

    /// (funcall FILTER PROC STRING)  (if FILTER is non-nil)
    /// to dispose of a bunch of chars from the process all at once.
    pub filter: LispObject,

    /// (funcall SENTINEL PROCESS) when process state changes.
    pub sentinel: LispObject,

    /// (funcall LOG SERVER CLIENT MESSAGE) when a server process
    /// accepts a connection from a client.
    pub log: LispObject,

    /// Buffer that output is going to.
    pub buffer: LispObject,

    /// t if this is a real child process.  For a network or serial
    /// connection, it is a plist based on the arguments to
    /// make-network-process or make-serial-process.
    pub childp: LispObject,

    /// Plist for programs to keep per-process state information, parameters, etc.
    pub plist: LispObject,

    /// Symbol indicating the type of process: real, network, serial.
    pub process_type: LispObject,

    /// Marker set to end of last buffer-inserted output from this process.
    pub mark: LispObject,

    /// Symbol indicating status of process.
    /// This may be a symbol: run, open, closed, listen, or failed.
    /// Or it may be a pair (connect . ADDRINFOS) where ADDRINFOS is
    /// a list of remaining (PROTOCOL . ADDRINFO) pairs to try.
    /// Or it may be (failed ERR) where ERR is an integer, string or symbol.
    /// Or it may be a list, whose car is stop, exit or signal
    /// and whose cdr is a pair (EXIT_CODE . COREDUMP_FLAG)
    /// or (SIGNAL_NUMBER . COREDUMP_FLAG).
    pub status: LispObject,

    /// Coding-system for decoding the input from this process.
    pub decode_coding_system: LispObject,

    /// Working buffer for decoding.
    pub decoding_buf: LispObject,

    /// Coding-system for encoding the output to this process.
    pub encode_coding_system: LispObject,

    /// Working buffer for encoding.
    pub encoding_buf: LispObject,

    /// Queue for storing waiting writes.
    pub write_queue: LispObject,
    // This struct is incomplete.
    // To access remaining fields use access functions written in
    // src/process.c and export them here for use in Rust.
}

/// Functions to access members of `struct Lisp_Process`.
extern "C" {
    pub fn pget_pid(p: *const Lisp_Process) -> pid_t;
    pub fn pget_kill_without_query(p: *const Lisp_Process) -> BoolBF;
    pub fn pget_process_inherit_coding_system_flag(p: *const Lisp_Process) -> BoolBF;
}

/// Functions to set members of `struct Lisp_Process`.
extern "C" {
    pub fn pset_kill_without_query(p: *mut Lisp_Process, b: BoolBF);
}

#[repr(C)]
pub struct Lisp_Frame {
    pub header: Lisp_Vectorlike_Header,

    /// All LispObject components must come first.
    /// That ensures they are all aligned normally.

    /// Name of this frame: a Lisp string.  It is used for looking up resources,
    /// as well as for the title in some cases.
    pub name: LispObject,

    /// The name to use for the icon, the last time
    /// it was refreshed.  nil means not explicitly specified.
    pub icon_name: LispObject,

    /// This is the frame title specified explicitly, if any.
    /// Usually it is nil.
    pub title: LispObject,

    // This struct is incomplete.
    // It is difficult, if not impossible, to import the rest of this struct.
    // 1. #IFDEF logic means the proper number of fields is hard to determine.
    // 2. Bitfields are compiler dependent. How much padding, where?
    //    The current count is roughly 50 bits.
    //
    // Because of this, access functions are written in src/frame.c and
    // exported here for use in Rust. This means that instead of
    // frame.foo the proper method is fget_foo(frame).
    /// This frame's parent frame, if it has one.
    parent_frame: LispObject,

    ///  The frame which should receive keystrokes that occur in this
    /// frame, or nil if they should go to the frame itself.  This is
    /// usually nil, but if the frame is minibufferless, we can use this
    /// to redirect keystrokes to a surrogate minibuffer frame when
    /// needed.
    ///
    /// Note that a value of nil is different than having the field point
    /// to the frame itself.  Whenever the Fselect_frame function is used
    /// to shift from one frame to the other, any redirections to the
    /// original frame are shifted to the newly selected frame; if
    /// focus_frame is nil, Fselect_frame will leave it alone.
    pub focus_frame: LispObject,

    /// This frame's root window.  Every frame has one.
    /// If the frame has only a minibuffer window, this is it.
    /// Otherwise, if the frame has a minibuffer window, this is its sibling.
    root_window: LispObject,

    /// This frame's selected window.
    /// Each frame has its own window hierarchy
    /// and one of the windows in it is selected within the frame.
    /// The selected window of the selected frame is Emacs's selected window.
    selected_window: LispObject,

    /// This frame's minibuffer window.
    /// Most frames have their own minibuffer windows,
    /// but only the selected frame's minibuffer window
    /// can actually appear to exist.
    minibuffer_window: LispObject,

    /// Parameter alist of this frame.
    /// These are the parameters specified when creating the frame
    /// or modified with modify-frame-parameters.
    param_alist: LispObject,

    /// List of scroll bars on this frame.
    /// Actually, we don't specify exactly what is stored here at all; the
    /// scroll bar implementation code can use it to store anything it likes.
    /// This field is marked by the garbage collector.  It is here
    /// instead of in the `device' structure so that the garbage
    /// collector doesn't need to look inside the window-system-dependent
    /// structure.
    scroll_bars: LispObject,
    condemned_scroll_bars: LispObject,

    /// Vector describing the items to display in the menu bar.
    /// Each item has four elements in this vector.
    /// They are KEY, STRING, SUBMAP, and HPOS.
    /// (HPOS is not used in when the X toolkit is in use.)
    /// There are four additional elements of nil at the end, to terminate.
    menu_bar_items: LispObject,

    /// Alist of elements (FACE-NAME . FACE-VECTOR-DATA).
    face_alist: LispObject,

    /// A vector that records the entire structure of this frame's menu bar.
    /// For the format of the data, see extensive comments in xmenu.c.
    /// Only the X toolkit version uses this.
    menu_bar_vector: LispObject,

    /// Predicate for selecting buffers for other-buffer.
    buffer_predicate: LispObject,

    /// List of buffers viewed in this frame, for other-buffer.
    buffer_list: LispObject,

    /// List of buffers that were viewed, then buried in this frame.  The
    /// most recently buried buffer is first.  For last-buffer.
    buried_buffer_list: LispObject,
}

extern "C" {
    pub fn fget_buffer_list(frame: *const Lisp_Frame) -> LispObject;
    pub fn fget_buried_buffer_list(frame: *const Lisp_Frame) -> LispObject;
    pub fn fget_internal_border_width(frame: *const Lisp_Frame) -> c_int;
    pub fn fget_selected_window(frame: *const Lisp_Frame) -> LispObject;
    pub fn fset_selected_window(frame: *mut Lisp_Frame, window: LispObject);
}

#[repr(C)]
pub union display_info {
    pub tty: *const tty_display_info,
    pub x: *const x_display_info,
    pub w32: *const w32_display_info,
    pub ns: *const ns_display_info,
}

#[repr(C)]
pub struct terminal {
    pub header: Lisp_Vectorlike_Header,
    pub kboard: kboard,
    pub display_info: display_info,
}

#[repr(C)]
#[derive(PartialEq)]
pub struct kboard;

// These are representations of the `display_info` union.
// They are ZSTs and not ideal. They need to be filled out
// or ported.
#[repr(C)]
pub struct tty_display_info;

#[repr(C)]
pub struct x_display_info;

#[repr(C)]
pub struct w32_display_info;

#[repr(C)]
pub struct ns_display_info;

/// Functions to access members of `struct frame`.
extern "C" {
    pub fn fget_column_width(f: *const Lisp_Frame) -> c_int;
    pub fn fget_line_height(f: *const Lisp_Frame) -> c_int;
    pub fn fget_minibuffer_window(f: *const Lisp_Frame) -> LispObject;
    pub fn fget_root_window(f: *const Lisp_Frame) -> LispObject;
    pub fn fget_terminal(f: *const Lisp_Frame) -> *mut terminal;
    pub fn fget_output_method(f: *const Lisp_Frame) -> c_int;
    pub fn fget_visible(f: *const Lisp_Frame) -> bool;
    pub fn fget_iconified(f: *const Lisp_Frame) -> BoolBF;
    pub fn fget_pointer_invisible(f: *const Lisp_Frame) -> BoolBF;
    pub fn fget_top_pos(f: *const Lisp_Frame) -> c_int;
    pub fn fget_left_pos(f: *const Lisp_Frame) -> c_int;

    pub fn estimate_mode_line_height(f: *const Lisp_Frame, face_id: face_id) -> c_int;
}

extern "C" {
    pub fn pget_raw_status_new(p: *const Lisp_Process) -> bool;
}

#[repr(C)]
pub struct hash_table_test {
    pub name: LispObject,
    pub user_hash_function: LispObject,
    pub user_cmp_function: LispObject,
    pub cmpfn: extern "C" fn(t: *mut hash_table_test, a: LispObject, b: LispObject) -> bool,
    pub hashfn: extern "C" fn(t: *mut hash_table_test, a: LispObject) -> EmacsUint,
}

#[repr(C)]
pub struct Lisp_Hash_Table {
    pub header: Lisp_Vectorlike_Header,
    pub weak: LispObject,
    pub hash: LispObject,
    pub next: LispObject,
    pub index: LispObject,
    pub count: ptrdiff_t,
    pub next_free: ptrdiff_t,
    pub pure_: bool, // pure is a reserved keyword in Rust
    pub rehash_threshold: c_float,
    pub rehash_size: c_float,
    pub key_and_value: LispObject,
    pub test: hash_table_test,
    pub next_weak: *mut Lisp_Hash_Table,
}

pub type Time = u32;

/// A Lisp time (HI LO US PS), sans the cons cells.
#[repr(C)]
#[derive(Default)]
pub struct lisp_time {
    pub hi: EmacsInt,
    pub lo: c_int,
    pub us: c_int,
    pub ps: c_int,
}

pub type map_keymap_function_t =
    unsafe extern "C" fn(LispObject, LispObject, LispObject, *const c_void);
pub type voidfuncptr = unsafe extern "C" fn();

extern "C" {
    pub static initialized: bool;
    pub static mut buffer_local_flags: Lisp_Buffer;
    pub static mut current_global_map: LispObject;
    pub static current_thread: *mut thread_state;
    pub static empty_unibyte_string: LispObject;
    pub static fatal_error_in_progress: bool;
    pub static mut globals: emacs_globals;
    pub static initial_obarray: LispObject;
    pub static mut last_per_buffer_idx: usize;
    pub static lispsym: Lisp_Symbol;
    pub static minibuf_level: EmacsInt;
    pub static minibuf_selected_window: LispObject;
    pub static mut minibuf_window: LispObject;
    pub static selected_frame: LispObject;
    pub static selected_window: LispObject;

    pub static mut Vautoload_queue: LispObject;
    pub static Vbuffer_alist: LispObject;
    pub static Vframe_list: LispObject;
    pub static Vminibuffer_list: LispObject;
    pub static Vprocess_alist: LispObject;
    pub static Vrun_hooks: LispObject;

    pub fn get_frame_param(frame: *const Lisp_Frame, property: LispObject) -> LispObject;
    pub fn staticpro(varaddress: *const LispObject);

    // Use LispObject::tag_ptr instead of make_lisp_ptr
    pub fn make_lisp_ptr(ptr: *const c_void, ty: Lisp_Type) -> LispObject;
    pub fn Fmake_char_table(purpose: LispObject, init: LispObject) -> LispObject;
    pub fn map_char_table(
        c_function: unsafe extern "C" fn(LispObject, LispObject, LispObject),
        function: LispObject,
        table: LispObject,
        arg: LispObject,
    );
    pub fn CHAR_TABLE_SET(ct: LispObject, idx: c_int, val: LispObject);

    pub fn aset_multibyte_string(array: LispObject, idxval: EmacsInt, c: c_int);
    pub fn Fcons(car: LispObject, cdr: LispObject) -> LispObject;
    pub fn Fsignal(error_symbol: LispObject, data: LispObject) -> !;
    pub fn Fcopy_sequence(seq: LispObject) -> LispObject;
    pub fn Ffind_operation_coding_system(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject;
    pub fn Flocal_variable_p(variable: LispObject, buffer: LispObject) -> LispObject;
    pub fn Ffuncall(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject;
    pub fn Fpurecopy(string: LispObject) -> LispObject;
    pub fn Fmapcar(function: LispObject, sequence: LispObject) -> LispObject;
    pub fn Fset(symbol: LispObject, newval: LispObject) -> LispObject;
    pub fn Fset_default(symbol: LispObject, value: LispObject) -> LispObject;
    pub fn Fconcat(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject;
    pub fn Fnconc(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject;

    pub fn make_float(float_value: c_double) -> LispObject;
    pub fn make_string(s: *const c_char, length: ptrdiff_t) -> LispObject;
    pub fn make_string_from_bytes(
        contents: *const c_char,
        nchars: ptrdiff_t,
        nbytes: ptrdiff_t,
    ) -> LispObject;
    pub fn make_pure_c_string(data: *const c_char, nchars: ptrdiff_t) -> LispObject;

    pub fn make_lisp_symbol(ptr: *mut Lisp_Symbol) -> LispObject;
    pub fn build_string(s: *const c_char) -> LispObject;
    pub fn make_unibyte_string(s: *const c_char, length: ptrdiff_t) -> LispObject;
    pub fn make_uninit_string(length: EmacsInt) -> LispObject;
    pub fn make_uninit_multibyte_string(nchars: EmacsInt, nbytes: EmacsInt) -> LispObject;
    pub fn make_specified_string(
        contents: *const c_char,
        nchars: ptrdiff_t,
        nbytes: ptrdiff_t,
        multibyte: bool,
    ) -> LispObject;
    pub fn string_to_multibyte(string: LispObject) -> LispObject;
    pub fn initial_define_key(keymap: LispObject, key: c_int, defname: *const c_char);

    pub fn eval_sub(form: LispObject) -> LispObject;

    pub fn preferred_coding_system() -> LispObject;
    pub fn Fcoding_system_p(o: LispObject) -> LispObject;
    pub fn code_convert_string(
        string: LispObject,
        coding_system: LispObject,
        dst_object: LispObject,
        encodep: bool,
        nocopy: bool,
        norecord: bool,
    ) -> LispObject;
    pub fn validate_subarray(
        array: LispObject,
        from: LispObject,
        to: LispObject,
        size: libc::ptrdiff_t,
        ifrom: &mut libc::ptrdiff_t,
        ito: &mut libc::ptrdiff_t,
    );
    pub fn string_char_to_byte(string: LispObject, char_index: libc::ptrdiff_t) -> libc::ptrdiff_t;

    pub fn record_unwind_current_buffer();
    pub fn set_buffer_internal(buffer: *mut Lisp_Buffer);
    pub fn make_buffer_string(
        start: libc::ptrdiff_t,
        end: libc::ptrdiff_t,
        props: bool,
    ) -> LispObject;

    pub fn intern_sym(sym: LispObject, obarray: LispObject, index: LispObject) -> LispObject;
    pub fn oblookup(
        obarray: LispObject,
        s: *const c_char,
        size: ptrdiff_t,
        size_bytes: ptrdiff_t,
    ) -> LispObject;

    pub fn CHECK_IMPURE(obj: LispObject, ptr: *const c_void);
    pub fn internal_equal(
        o1: Lisp_Object,
        o2: Lisp_Object,
        kind: EqualKind,
        depth: libc::c_int,
        ht: Lisp_Object,
    ) -> bool;
    pub fn uniprop_table_uncompress(table: Lisp_Object, idx: u32) -> Lisp_Object;
    pub fn update_buffer_defaults(objvar: *mut LispObject, newval: LispObject);
    pub fn find_field(
        pos: LispObject,
        merge_at_boundary: LispObject,
        beg_limit: LispObject,
        beg: *mut ptrdiff_t,
        end_limit: LispObject,
        end: *mut ptrdiff_t,
    );
    pub fn concat(
        nargs: ptrdiff_t,
        args: *mut LispObject,
        target_type: Lisp_Type,
        last_special: bool,
    ) -> LispObject;
    pub fn map_keymap_item(
        fun: map_keymap_function_t,
        args: LispObject,
        key: LispObject,
        val: LispObject,
        data: *const c_void,
    );
    pub fn map_keymap_char_table_item(args: LispObject, key: LispObject, val: LispObject);
    pub fn mset_charpos(m: *const Lisp_Marker, charpos: ptrdiff_t);
    pub fn mset_bytepos(m: *const Lisp_Marker, bytepos: ptrdiff_t);
    pub static initial_obarray: LispObject;
    pub fn scan_lists(
        from: EmacsInt,
        count: EmacsInt,
        depth: EmacsInt,
        sexpflag: bool,
    ) -> LispObject;
    pub fn is_minibuffer(w: *const Lisp_Window) -> bool;
    pub static minibuf_prompt: LispObject;
    pub fn pset_sentinel(p: *mut Lisp_Process, val: LispObject);
}

// Largest and smallest numbers that can be represented as fixnums in
// Emacs lisp.
pub const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> Lisp_Bits::INTTYPEBITS as u32;
pub const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);

// Max value for the first argument of wait_reading_process_output.
pub const WAIT_READING_MAX: i64 = std::i64::MAX;

// In order to use `lazy_static!` with LispSubr, it must be Sync. Raw
// pointers are not Sync, but it isn't a problem to define Sync if we
// never mutate LispSubr values. If we do, we will need to create
// these objects at runtime, perhaps using forget().
//
// Based on http://stackoverflow.com/a/28116557/509706
unsafe impl Sync for Lisp_Subr {}

pub type Lisp_Buffer = buffer;
pub type Lisp_Window = window;
pub type Lisp_Frame = frame;

#[repr(C)]
pub struct Lisp_Vectorlike {
    pub header: vectorlike_header,
    // shouldn't look at the contents without knowing the structure...
}

// No C equivalent.  Generic type for a vectorlike with one or more
// LispObject slots after the header.
#[repr(C)]
pub struct Lisp_Vectorlike_With_Slots {
    pub header: vectorlike_header,
    // actually any number of items... not sure how to express this
    pub contents: __IncompleteArrayField<Lisp_Object>,
}

//// declare this ourselves so that the arg isn't mutable
//extern "C" {
//    pub fn staticpro(arg1: *const Lisp_Object);
//}
