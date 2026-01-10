import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode

@external(erlang, "os", "getenv")
fn ffi_getenv(name: String) -> Dynamic

pub fn get_env(name: String) -> Result(String, Nil) {
  case decode.run(ffi_getenv(name), decode.string) {
    Ok(value) if value != "" -> Ok(value)
    _ -> Error(Nil)
  }
}
