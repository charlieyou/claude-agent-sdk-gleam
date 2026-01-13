/// Control Operations Test Index
///
/// This module documents the test organization for control operations.
/// The epic specified `test/control_ops_test.gleam` but the implementation
/// organized tests into specialized modules for better maintainability:
///
/// ## Test Files
///
/// - `interrupt_test.gleam` - Tests for interrupt() operation
/// - `set_permission_mode_test.gleam` - Tests for set_permission_mode() operation
/// - `set_model_test.gleam` - Tests for set_model() operation
/// - `rewind_files_test.gleam` - Tests for rewind_files() operation
/// - `control_op_infra_test.gleam` - Infrastructure tests (request/response flow)
/// - `op_timeout_test.gleam` - Timeout and cancellation edge cases
///
/// ## Coverage Summary
///
/// Each operation is tested for:
/// - Correct wire format sent to CLI
/// - Success response handling
/// - Error response handling
/// - Timeout handling
/// - Session stop behavior
///
/// See `plans/2026-01-12-bidir-protocol-plan.md` for the design specification.
import gleeunit/should

/// Verify that the control operations test suite exists.
/// This test confirms the module compiles and is included in the test run.
/// For actual control operation tests, see the specialized test modules.
pub fn control_ops_test_index_exists_test() {
  // This test serves as a marker that control operation tests are organized
  // across specialized modules rather than a single monolithic file.
  should.be_true(True)
}
