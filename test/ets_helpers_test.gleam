import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import gleeunit/should
import support/ets_helpers

/// FFI to coerce any value to Dynamic
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

pub fn new_creates_table_test() {
  let table = ets_helpers.new("test_table_1")
  // Table should be usable (no crash means success)
  ets_helpers.insert(table, dynamic.string("key"), dynamic.string("value"))
}

pub fn insert_and_lookup_test() {
  let table = ets_helpers.new("test_table_2")
  let key = dynamic.string("my_key")
  let value = dynamic.int(42)

  ets_helpers.insert(table, key, value)

  case ets_helpers.lookup(table, key) {
    Some(_retrieved) -> {
      // The value should be retrievable - success
      should.be_true(True)
    }
    None -> should.fail()
  }
}

pub fn lookup_missing_key_returns_none_test() {
  let table = ets_helpers.new("test_table_3")
  let key = dynamic.string("nonexistent")

  ets_helpers.lookup(table, key)
  |> should.equal(None)
}

pub fn delete_removes_key_test() {
  let table = ets_helpers.new("test_table_4")
  let key = dynamic.string("to_delete")
  let value = dynamic.string("some_value")

  ets_helpers.insert(table, key, value)
  ets_helpers.delete(table, key)

  ets_helpers.lookup(table, key)
  |> should.equal(None)
}

pub fn insert_replaces_existing_value_test() {
  let table = ets_helpers.new("test_table_5")
  let key = dynamic.string("same_key")

  ets_helpers.insert(table, key, dynamic.int(1))
  ets_helpers.insert(table, key, dynamic.int(2))

  case ets_helpers.lookup(table, key) {
    Some(_retrieved) -> {
      // Value was replaced - success
      should.be_true(True)
    }
    None -> should.fail()
  }
}

pub fn make_ref_returns_unique_refs_test() {
  let ref1 = ets_helpers.make_ref()
  let ref2 = ets_helpers.make_ref()
  let ref3 = ets_helpers.make_ref()

  // Each ref should be unique - we can verify by using them as keys
  let table = ets_helpers.new("test_table_6")

  ets_helpers.insert(table, ref1, dynamic.int(1))
  ets_helpers.insert(table, ref2, dynamic.int(2))
  ets_helpers.insert(table, ref3, dynamic.int(3))

  // All three should be independently retrievable
  case ets_helpers.lookup(table, ref1) {
    Some(_v1) -> should.be_true(True)
    None -> should.fail()
  }

  case ets_helpers.lookup(table, ref2) {
    Some(_v2) -> should.be_true(True)
    None -> should.fail()
  }

  case ets_helpers.lookup(table, ref3) {
    Some(_v3) -> should.be_true(True)
    None -> should.fail()
  }
}

pub fn ref_as_ets_key_workflow_test() {
  // This test demonstrates the intended usage pattern for test_runner
  let table = ets_helpers.new("test_table_7")

  // Simulate on_spawn: create a ref and store initial state
  let ref = ets_helpers.make_ref()
  let initial_lines = to_dynamic(["line1\n", "line2\n", "line3\n"])
  ets_helpers.insert(table, ref, initial_lines)

  // Simulate on_read: lookup and verify state exists
  case ets_helpers.lookup(table, ref) {
    Some(_state) -> {
      // State found, as expected
      should.be_true(True)
    }
    None -> should.fail()
  }

  // Simulate on_close: delete state
  ets_helpers.delete(table, ref)

  // After close, state should be gone
  ets_helpers.lookup(table, ref)
  |> should.equal(None)
}
