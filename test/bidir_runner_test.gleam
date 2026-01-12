import claude_agent_sdk/internal/bidir_runner
import gleeunit/should

/// BidirRunner.start() is not yet implemented - returns Error placeholder.
/// This test documents the expected behavior and will pass once T3 implements start().
pub fn bidir_runner_start_not_implemented_test() {
  let result = bidir_runner.start([])
  should.be_error(result)
}
