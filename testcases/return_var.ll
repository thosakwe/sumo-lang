; ModuleID = 'return_var'
source_filename = "return_var"

define i64 @main() {
entry:
  %exitCode = alloca i64
  store i64 21, i64* %exitCode
  %ignored = alloca i64
  store i64 4, i64* %ignored
  %ignored2 = alloca i64
  store i64 4, i64* %ignored2
  %ignored3 = alloca i64
  store i64 4, i64* %ignored3
  %exitCode1 = load i64, i64* %exitCode
  ret i64 %exitCode1
}
