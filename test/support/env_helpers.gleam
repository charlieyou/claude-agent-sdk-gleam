import gleam/dynamic.{type Dynamic}

/// Convert Gleam String to Erlang charlist for os:getenv
@external(erlang, "erlang", "binary_to_list")
fn string_to_charlist(s: String) -> Dynamic

/// Convert Erlang charlist to Gleam String
@external(erlang, "unicode", "characters_to_binary")
fn charlist_to_string(chars: Dynamic) -> String

@external(erlang, "os", "getenv")
fn ffi_getenv(name: Dynamic) -> Dynamic

/// Check if value is the atom 'false' (os:getenv returns false when not set)
@external(erlang, "erlang", "is_atom")
fn is_atom(value: Dynamic) -> Bool

pub fn get_env(name: String) -> Result(String, Nil) {
  let charlist_name = string_to_charlist(name)
  let result = ffi_getenv(charlist_name)
  // os:getenv returns 'false' atom when not set, or a charlist when set
  case is_atom(result) {
    True -> Error(Nil)
    False -> {
      let value = charlist_to_string(result)
      case value {
        "" -> Error(Nil)
        _ -> Ok(value)
      }
    }
  }
}
