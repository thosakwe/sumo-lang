; ModuleID = 'call_c'
source_filename = "call_c"

define void @main() {
entry:
  call void @tulip_glue_putc(i64 72)
  call void @tulip_glue_putc(i64 101)
  call void @tulip_glue_putc(i64 108)
  call void @tulip_glue_putc(i64 108)
  call void @tulip_glue_putc(i64 111)
  call void @tulip_glue_putc(i64 32)
  call void @tulip_glue_putc(i64 119)
  call void @tulip_glue_putc(i64 111)
  call void @tulip_glue_putc(i64 114)
  call void @tulip_glue_putc(i64 108)
  call void @tulip_glue_putc(i64 100)
  call void @tulip_glue_putc(i64 10)
  ret void
}

declare void @tulip_glue_putc(i64)
