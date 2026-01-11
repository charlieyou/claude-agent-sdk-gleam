//// Internal constants for timeouts, buffer limits, and decode error thresholds.
//// Single source of truth for all stream/CLI configuration values.

// Version check timeout (ms) - max wait for CLI version detection
pub const version_check_timeout_ms = 5000

// Post-result drain timeout (ms) - brief wait after result for trailing messages
pub const post_result_drain_timeout_ms = 100

// Post-exit drain timeout (ms) - wait per iteration when draining after exit
pub const post_exit_drain_timeout_ms = 50

// Close drain timeout (ms) - wait when draining during close
pub const close_drain_timeout_ms = 50

// Close drain max messages - cap on messages processed during close drain
pub const close_drain_max_messages = 100

// Post-exit drain max iterations - cap on drain loop iterations after exit
pub const post_exit_drain_max_iterations = 10

// Post-exit drain max bytes - byte limit for post-exit drain buffer
pub const post_exit_drain_max_bytes = 1024

// Max line bytes - buffer overflow threshold (10 MB)
pub const max_line_bytes = 10_000_000

// Max consecutive decode errors before stream terminates
pub const max_consecutive_decode_errors = 5
