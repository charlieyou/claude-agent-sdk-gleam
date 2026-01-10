import gleam/dynamic.{type Dynamic}

@external(erlang, "os", "getenv")
fn ffi_getenv(name: String) -> Dynamic

pub fn get_env(name: String) -> Result(String, Nil) {
  case dynamic.string(ffi_getenv(name)) {
    Ok(value) if value != "" -> Ok(value)
    _ -> Error(Nil)
  }
}
