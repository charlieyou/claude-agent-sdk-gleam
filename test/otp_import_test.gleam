import gleam/otp/actor
import gleeunit/should

pub fn otp_import_test() {
  // Verify we can reference actor.start - confirms gleam_otp is available
  // We don't need a real actor, just confirm the module imports
  let _start_fn = actor.start
  should.be_true(True)
}
