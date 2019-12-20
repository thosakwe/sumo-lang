; ModuleID = 'return_var'
source_filename = "return_var"

define void @main() {
entry:
  %code = alloca i64
  store i64* %code, i64 20
  ret void
}
