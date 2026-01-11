import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}

/// Opaque type representing an ETS table reference
pub type Table

/// Options for ETS table creation (subset needed for tests)
pub type TableOption {
  Set
  Public
}

/// Create a new ETS table with the given name and options.
/// Returns a table reference that can be used for insert/lookup/delete operations.
///
/// ⚠️ WARNING: Each unique name creates a permanent atom. Do not use
/// dynamically-generated names (timestamps, random values, etc.) as this
/// will exhaust the atom table. Use fixed, hardcoded names only.
pub fn new(name: String) -> Table {
  ffi_new(atom_from_string(name), [Set, Public])
}

@external(erlang, "ets", "new")
fn ffi_new(name: Dynamic, options: List(TableOption)) -> Table

@external(erlang, "erlang", "binary_to_atom")
fn atom_from_string(name: String) -> Dynamic

/// Insert a key-value pair into the table.
/// If the key already exists, the value is replaced.
pub fn insert(table: Table, key: Dynamic, value: Dynamic) -> Nil {
  // ETS insert takes a tuple {key, value}
  ffi_insert(table, #(key, value))
  Nil
}

@external(erlang, "ets", "insert")
fn ffi_insert(table: Table, tuple: #(Dynamic, Dynamic)) -> Dynamic

/// Look up a key in the table.
/// Returns Some(value) if found, None if not found.
pub fn lookup(table: Table, key: Dynamic) -> Option(Dynamic) {
  case ffi_lookup(table, key) {
    [] -> None
    [tuple] -> Some(tuple_element(tuple, 2))
    _ -> None
  }
}

@external(erlang, "ets", "lookup")
fn ffi_lookup(table: Table, key: Dynamic) -> List(Dynamic)

@external(erlang, "erlang", "element")
fn tuple_element(tuple: Dynamic, index: Int) -> Dynamic

/// Delete a key from the table.
pub fn delete(table: Table, key: Dynamic) -> Nil {
  ffi_delete(table, key)
  Nil
}

@external(erlang, "ets", "delete")
fn ffi_delete(table: Table, key: Dynamic) -> Dynamic

/// Create a unique reference.
/// Each call returns a new unique reference that can be used as a key.
@external(erlang, "erlang", "make_ref")
pub fn make_ref() -> Dynamic
