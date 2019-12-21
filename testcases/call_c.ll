; ModuleID = 'call_c'
source_filename = "call_c"

define void @main() {
entry:
  call void @tulip_glue_putc(i64 2)
  ret void
}

declare void @tulip_glue_putc()
